import Lean.Data.Json
import Orchestra.Dirs
import Orchestra.Utils.Files

open Lean (Json)

/-!
# Skills

A *skill* is one Markdown file telling an agent how to do something orchestra-specific — reach
for the MCP pull-request tools instead of `gh`, work a taxis issue the way the claim protocol
expects. They are the third configuration surface, after listeners and roles, and the only one
whose content is prose rather than a record.

On disk they are a Claude plugin directory, because that is what the agent backends consume:

```
<config>/skills/.claude-plugin/plugin.json    -- makes the directory a plugin
<config>/skills/skills/<name>/SKILL.md        -- one skill
```

`Dirs.skillsDir` is the plugin directory; `TaskRunner.pluginDirs` passes it to every agent it
launches. The doubled `skills/` is the plugin layout, not a typo.

Every `SKILL.md` opens with YAML front matter carrying a `name` and a `description`. Those two
fields are not decoration: an agent reads the description to decide whether the skill is relevant
before it reads the body, so a file without one is loaded and then never used. Writing a skill
through the API therefore *requires* both, and requires the name to agree with the directory —
accepting a file the agent would silently ignore is not an improvement on rejecting it.

Only the front matter is parsed. It is read as a flat map of single-line `key: value` pairs
rather than with a YAML parser, which is all the format uses and all that can be validated
without pretending to understand YAML.
-/

namespace Orchestra.Skill

/-- One skill, as the API and the CLI see it. -/
structure Skill where
  /-- Directory name under `skills/`, and the front matter's `name`. -/
  name        : String
  /-- The front matter's `description`. Required on write; `none` only for a file that was put
      there by hand. -/
  description : Option String
  /-- The whole `SKILL.md`, front matter included. This is what a client edits and sends back. -/
  content     : String
  /-- Last modification of `SKILL.md`, as epoch seconds. `none` when it could not be read. -/
  updatedAt   : Option Int
deriving Repr, Inhabited

/-! ## Filesystem layout -/

/-- Optional override for the skills plugin directory (tests redirect this, exactly as
    `Project.globalRolesDirOverride` does for roles). -/
initialize skillsDirOverride : IO.Ref (Option System.FilePath) ← IO.mkRef none

def setSkillsDirOverride (p : Option System.FilePath) : IO Unit :=
  skillsDirOverride.set p

/-- The plugin directory itself: `<config>/skills`. -/
def pluginDir : IO System.FilePath := do
  match ← skillsDirOverride.get with
  | some p => return p
  | none   => Dirs.skillsDir

/-- Where the individual skills live: `<config>/skills/skills`. -/
def skillsRoot : IO System.FilePath := do
  return (← pluginDir) / "skills"

def skillFile (name : String) : IO System.FilePath := do
  return (← skillsRoot) / name / "SKILL.md"

/-- The plugin manifest that makes `pluginDir` loadable at all. Written when a skill is created
    into a directory that has none — a `skills/` tree without it is invisible to every backend,
    so creating the first skill through the API would otherwise appear to do nothing. -/
private def pluginManifest : String :=
  "{\n  \"name\": \"orchestra\",\n  \"version\": \"0.1.0\",\n  \
\"description\": \"Skills for agents running under orchestra.\",\n  \
\"author\": { \"name\": \"orchestra\" }\n}\n"

/-! ## Front matter -/

/-- The front matter's `key: value` pairs, or the reason the block is not one.

    A skill file must open with `---` on its own first line and close with another `---`. Values
    are taken verbatim to the end of the line, which is why a `description` containing a colon
    survives. -/
def parseFrontMatter (content : String) : Except String (List (String × String)) := do
  let lines := content.splitOn "\n"
  match lines with
  | [] => .error "the file is empty"
  | first :: rest =>
    unless first.trimAscii.toString == "---" do
      .error "a skill must open with a '---' front matter block on its first line"
    let body := rest.takeWhile (·.trimAscii.toString != "---")
    if body.length == rest.length then
      .error "the front matter block is never closed by a second '---'"
    let mut out : List (String × String) := []
    for line in body do
      if line.trimAscii.isEmpty then continue
      match line.splitOn ":" with
      | key :: v :: vs =>
        out := out ++ [(key.trimAscii.toString, (":".intercalate (v :: vs)).trimAscii.toString)]
      | _ => .error s!"front matter line '{line}' is not a 'key: value' pair"
    return out

/-- Check a proposed `SKILL.md` against the name it is being stored under, returning the skill
    it describes.

    The name is checked against the front matter rather than rewritten into it: silently editing
    someone's file to make it valid hides the disagreement, and the two names are visible in
    different places (a directory listing, and the agent's skill list) where they would then
    disagree forever. -/
def validate (name : String) (content : String) : Except String Skill := do
  -- First, and for the same reason the other two stores check it first: `name` becomes a
  -- directory under `skillsRoot`, and on a `POST` it arrives in the request *body* rather than
  -- as a path segment, so nothing upstream has decoded and rejected it. The front matter check
  -- below compares two strings the caller controls, so it establishes nothing about either.
  Utils.checkConfigName "skill" name
  if content.trimAscii.isEmpty then
    .error "the skill body is empty"
  let fields ← parseFrontMatter content
  let field (k : String) : Option String := (fields.find? (·.1 == k)).map (·.2)
  let some declared := field "name"
    | .error "the front matter has no 'name'; an agent lists skills by it"
  unless declared == name do
    .error s!"the front matter names this skill '{declared}', but it is being stored as '{name}'"
  let some description := field "description"
    | .error "the front matter has no 'description'; an agent decides whether to read a skill \
from it, so one without it is loaded and never used"
  if description.isEmpty then
    .error "the front matter's 'description' is empty"
  return { name, description := some description, content, updatedAt := none }

/-! ## Reading -/

private def modifiedEpoch (path : System.FilePath) : IO (Option Int) := do
  try return some (← path.metadata).modified.sec catch _ => return none

/-- Read one skill. `none` when no file of that name exists; a file that exists but has unusable
    front matter still comes back, with whatever could be read of it — the API's job there is to
    show the operator the file that needs fixing, not to hide it. -/
def loadSkill (name : String) : IO (Option Skill) := do
  Utils.ensureConfigName "skill" name
  let path ← skillFile name
  if !(← path.pathExists) then return none
  let content ← IO.FS.readFile path
  let description := match parseFrontMatter content with
    | .ok fields => (fields.find? (·.1 == "description")).map (·.2)
    | .error _   => none
  return some { name, description, content, updatedAt := ← modifiedEpoch path }

/-- Every skill installed, ordered by name so a listing is stable across calls. -/
def loadAllSkills : IO (Array Skill) := do
  let root ← skillsRoot
  if !(← root.pathExists) then return #[]
  let mut names : Array String := #[]
  for entry in ← System.FilePath.readDir root do
    -- A directory the API could not have created — a dotfile, say, left there by hand — is
    -- skipped rather than listed. `loadSkill` refuses to build a path from such a name, so
    -- including it here would turn one stray directory into a listing that throws.
    if !Utils.validConfigName entry.fileName then continue
    if ← (entry.path / "SKILL.md").pathExists then names := names.push entry.fileName
  let mut out : Array Skill := #[]
  for name in names.qsort (· < ·) do
    if let some s ← loadSkill name then out := out.push s
  return out

/-! ## Writing -/

/-- Install or replace a skill. The content is written exactly as given; `validate` is the
    caller's business, and is what the API runs before reaching this. -/
def saveSkill (name : String) (content : String) : IO Unit := do
  Utils.ensureConfigName "skill" name
  let manifest := (← pluginDir) / ".claude-plugin" / "plugin.json"
  unless ← manifest.pathExists do
    Utils.writeFileAtomically manifest pluginManifest
  Utils.writeFileAtomically (← skillFile name) content

/-- Remove a skill. `false` when there was none of that name.

    Removes the whole skill directory, since that is the unit the plugin layout defines — a
    skill's supporting files live beside its `SKILL.md`, and leaving them behind would leave an
    empty-looking directory that is not a skill and not nothing. -/
def deleteSkill (name : String) : IO Bool := do
  Utils.ensureConfigName "skill" name
  let dir := (← skillsRoot) / name
  if !(← (dir / "SKILL.md").pathExists) then return false
  IO.FS.removeDirAll dir
  return true

end Orchestra.Skill
