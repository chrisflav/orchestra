import Lean.Data.Json
import Orchestra.Config
import Orchestra.Dirs
import Orchestra.Utils.Files

open Lean (Json FromJson ToJson)

/-!
# Identities

An *identity* is a persistent someone for a task to be. It is the fourth configuration surface,
after listeners, roles and skills, and the only one that outlives the run it is used in: a task
names an identity, and the identity hands it a memory that was there before the task started and
is still there afterwards, plus the credentials to act on the tracker as itself rather than as
the instance.

```
<config>/identities/<name>/identity.json   -- the record
<config>/identities/<name>/AGENTS.md       -- its standing instructions, optional
<data>/identities/<name>/memory/           -- its memory, mounted read-write into the sandbox
```

A directory rather than one file because an identity is a bundle: the record says who it is to
orchestra, and `AGENTS.md` says how it works, in the format that is already the convention for
telling an agent that. Keeping the instructions in their own file is what makes them editable as
prose — a `"prompt"` string inside JSON is a paragraph with `\n` in it that nobody wants to
maintain.

An identity is *not* a role and the two are worth keeping apart. A role says what a task does —
its prompt, its tools, its model. An identity says who does it, and it is the half that
accumulates: two tasks dispatched for the same role a week apart share nothing, while two tasks
run under the same identity share everything the first one wrote down.

## Why the memory is not under `<data>/memory`

The obvious home for it would be beside the global and per-project memories, as
`<data>/memory/identities/<name>`. It cannot go there. Global memory is the memory *root*
itself — `resolveMemoryDirs` hands `<data>/memory` to any task whose `memory` is `global` or
`both` — so every directory below it is readable and writable by every such task. An identity's
memory kept there would be one every ordinary task could read and rewrite, which is the opposite
of dedicated. A separate root under `<data>/identities` is reachable only by naming the identity.

For the same reason `memory: "none"` does not suppress it. That field chooses among the *shared*
memories; the identity's own memory is part of the identity, and a task that asked to run as
somebody and got none of their memory would have got the shell of an identity and not the thing.

## What "acting as" reaches, and what it does not

An identity may carry a `taxis_token`. Every write a task's issue tools make — comments, reviews,
issues created and updated, context notes — is then made with that token, so the tracker records
the identity as the author instead of whatever actor orchestra's own token belongs to. This is
the whole of the first version. GitHub is still reached with orchestra's App installation token
and the configured PAT: a pull request an identity opens is opened by orchestra, and there is no
per-identity credential for it here.

Claims are the deliberate exception on the taxis side. `o-claimed` is orchestra's own bookkeeping,
written and read back by the daemon rather than authored by anyone, and the holder it records is
a task id. It stays on the instance token so that a claim taken under an identity whose token is
later rotated or revoked is still one the daemon can release.

Permissions per identity are not here either. They want an authorization service to hold them
(kleis), and a permission list in this file would be a second, weaker answer to the question that
service exists to answer — one every task-configuration surface could already override, since
`tools` is written on the task. The token is what an identity carries for now, and the tracker is
what enforces what that token may do.

What an identity *does* name is where its access comes from: a `kleis_spec` (or a `kleis_token`)
that tasks run as it derive their kleis tokens from, ahead of the role's and the default
(`Kleis.choose`, `docs/kleis.md`). The access itself is written in kleis, as a spec; the identity
only says which.

## An identity is assigned, never chosen

Nothing an agent says selects an identity: it is written on the task, the role or the listener
action, all of which are configuration. `queue_task` does not offer it as a field either — a task
queued by a task inherits the identity of the task that queued it (`SpawnContext`), so an agent
can hand its own identity onwards but cannot put on another one.
-/

namespace Orchestra.Identity

/-- An identity's `resident` block: what makes it a resident, with one session that never ends
    (`docs/design/resident-sessions.md`). Every field is optional; what is absent falls back to the
    `residents` block of `config.json` (`ResidentSpec.settings`).

    `feeds` is read and kept, not acted on: the feeds themselves — Zulip first — are a later phase,
    and a block written for them should not stop the resident from working without them. -/
structure ResidentSpec where
  budgetPerDay : Option Float := none
  processIdleSeconds : Option Nat := none
  podIdleSeconds : Option Nat := none
  batchWindowSeconds : Option Nat := none
  model : Option String := none
  /-- Optional tools its MCP server grants; `none` is every one a repository-less session can use. -/
  tools : Option (List String) := none
  /-- The agent backend, as a session's `backend`. -/
  agent : Option String := none
  /-- The feeds, as written. Not acted on yet. -/
  feeds : Array Json := #[]
deriving Inhabited

/-- A resident's settings with every default filled in. -/
structure ResidentSettings where
  budgetPerDay : Float
  processIdleSeconds : Nat
  podIdleSeconds : Nat
  batchWindowSeconds : Nat
  model : Option String
  tools : Option (List String)
  agent : String
deriving Inhabited, Repr

/-- The block's own values, and the configuration's where it has none. -/
def ResidentSpec.settings (r : ResidentSpec) (cfg : ResidentsConfig) : ResidentSettings :=
  { budgetPerDay       := r.budgetPerDay.getD cfg.budgetPerDay
    processIdleSeconds := r.processIdleSeconds.getD cfg.processIdleSeconds
    -- Never shorter than the process stage: the pod outliving its process is what warm means.
    podIdleSeconds     := max (r.podIdleSeconds.getD cfg.podIdleSeconds)
                            (r.processIdleSeconds.getD cfg.processIdleSeconds)
    batchWindowSeconds := r.batchWindowSeconds.getD cfg.batchWindowSeconds
    model := r.model, tools := r.tools, agent := r.agent.getD "claude" }

/-- Read a `resident` block. Strict: a block that half-parsed would be a resident running on
    defaults nobody wrote — a hundred dollars a day instead of the five the operator asked for. -/
def parseResidentSpec (who : String) (j : Json) : Except String ResidentSpec := do
  let .obj kvs := j | throw s!"{who}: 'resident' must be an object"
  for key in ["upstream", "fork"] do
    if (j.getObjVal? key).isOk then
      throw s!"{who}: a resident has no repository, so its block may not set '{key}'"
  let known := ["budget_per_day", "process_idle_seconds", "pod_idle_seconds",
    "batch_window_seconds", "model", "tools", "agent", "feeds"]
  for (k, _) in kvs.toArray do
    unless known.contains k do
      throw s!"{who}: unknown field '{k}' in 'resident' (known: {", ".intercalate known})"
  let optNat (k : String) : Except String (Option Nat) := match j.getObjVal? k with
    | .error _ => pure none
    | .ok v => match (FromJson.fromJson? v : Except String Nat) with
      | .ok n => pure (some n)
      | .error _ => throw s!"{who}: resident.{k} must be a whole number of seconds"
  let optStr (k : String) : Except String (Option String) := match j.getObjVal? k with
    | .error _ => pure none
    | .ok v => match (FromJson.fromJson? v : Except String String) with
      | .ok s => pure (if s.trimAscii.isEmpty then none else some s)
      | .error _ => throw s!"{who}: resident.{k} must be a string"
  let budgetPerDay ← match j.getObjVal? "budget_per_day" with
    | .error _ => pure none
    | .ok v => match v.getNum? with
      | .ok n =>
        let f := n.toFloat
        if f < 0.0 then throw s!"{who}: resident.budget_per_day must not be negative"
        pure (some f)
      | .error _ => throw s!"{who}: resident.budget_per_day must be a number of USD"
  let tools ← match j.getObjVal? "tools" with
    | .error _ => pure none
    | .ok v => match (FromJson.fromJson? v : Except String (List String)) with
      | .ok ts => pure (some ts)
      | .error _ => throw s!"{who}: resident.tools must be a list of tool names"
  let feeds ← match j.getObjVal? "feeds" with
    | .error _ => pure #[]
    | .ok (.arr fs) => pure fs
    | .ok _ => throw s!"{who}: resident.feeds must be a list"
  return { budgetPerDay, tools, feeds
           processIdleSeconds := ← optNat "process_idle_seconds"
           podIdleSeconds     := ← optNat "pod_idle_seconds"
           batchWindowSeconds := ← optNat "batch_window_seconds"
           model := ← optStr "model", agent := ← optStr "agent" }

/-- One identity, as configuration writes it and as a task carries it. -/
structure Identity where
  /-- The name a task, role or listener action refers to it by, and the file it is stored in. -/
  name : String
  /-- One or two sentences saying who this is, shown to the agent in its system prompt.

      Worth writing even though nothing reads it mechanically: the agent is told the name
      regardless, and a name with nothing behind it ("maintainer") tells it as little as it tells
      a person reading the tracker afterwards. -/
  description : Option String := none
  /-- The taxis API token this identity's tracker writes are made with. `none` leaves them on
      orchestra's own token, which is a usable identity — one with a dedicated memory and no
      separate presence on the tracker. -/
  taxisToken : Option String := none
  /-- The kleis spec tasks run as this identity derive their tokens from (`Kleis.choose`), when
      the task names no source of its own. Above the role's: a secretary dispatched for a role
      keeps the secretary's access. At most one of this and `kleisToken`. -/
  kleisSpec : Option String := none
  /-- A kleis token tasks run as this identity derive theirs from, instead of a spec: what the
      identity was handed by whoever owns the accounts it reaches. Resolved from `secrets.json`
      when the record is read, like `taxisToken`, and redacted the same way. -/
  kleisToken : Option String := none
  /-- Narrowings every task run as this identity carries, on top of the task's own. -/
  kleisNarrow : List String := []
  /-- The identity's `AGENTS.md`, whole, when it has one.

      Not a field of the JSON record: it is read from the file beside it by `loadIdentity`, the
      way `Skill.content` is. Instructions are prose and belong in a markdown file the operator
      edits, not in a string inside a config document.

      What it is *for* is the standing half of an identity — how this one works, what it always
      checks before it opens a pull request, which conventions it holds itself to — as against
      the per-run half, which is the task's prompt. It reaches the agent appended to its system
      prompt (`TaskRunner.identityInstructions`), not written into the checkout: a repository has
      its own `AGENTS.md` and orchestra has no business overwriting it, or leaving a file in a
      working tree that the agent would then have to remember not to commit. -/
  agents : Option String := none
  /-- The identity's `resident` block, when it is a resident (`ResidentSpec`). -/
  resident : Option ResidentSpec := none
deriving Inhabited

/-- Hand-written so the token cannot be printed. `deriving Repr` would put it in whatever a
    `s!"{repr identity}"` in a debug line goes to, which for the daemon is its log — undoing, in
    one line somebody adds in a hurry, what the `ToJson` instance below is careful about. The
    fields worth seeing in a debug print are the name and whether there is a token at all. -/
instance : Repr Identity where
  reprPrec i _ :=
    let described := if i.description.isSome then "described" else "no description"
    let token     := if i.taxisToken.isSome then "token <redacted>" else "no token"
    let agents    := if i.agents.isSome then "AGENTS.md" else "no AGENTS.md"
    let kleis     := match i.kleisSpec, i.kleisToken with
      | some s, _    => s!"kleis spec {s}"
      | none, some _ => "kleis token <redacted>"
      | none, none   => "no kleis source"
    f!"identity {i.name} ({described}, {token}, {kleis}, {agents})"

instance : ToJson Identity where
  toJson i :=
    let fields : List (String × Json) := [("name", Json.str i.name)]
    let fields := if let some d := i.description then fields ++ [("description", Json.str d)]
                  else fields
    -- The spec is configuration and worth listing; the token, like `taxis_token`, is not.
    let fields := if let some s := i.kleisSpec then fields ++ [("kleis_spec", Json.str s)]
                  else fields
    let fields := if !i.kleisNarrow.isEmpty then fields ++ [("kleis_narrow", Lean.toJson i.kleisNarrow)]
                  else fields
    -- `agents` is left out too, but for a duller reason than the token: it is a file, and a
    -- listing that inlined every identity's instructions would be a listing nobody could read.
    -- The token is deliberately not serialized. This instance is what an API listing and
    -- `orchestra` printing a record would use, and a credential that goes out over either is a
    -- credential in a log. Reading a record back through `FromJson` is therefore not a
    -- round-trip, which is the right asymmetry: the file on disk is the only place the token
    -- lives.
    Json.mkObj fields

instance : FromJson Identity where
  fromJson? j := do
    let name        ← j.getObjValAs? String "name"
    let description := j.getObjValAs? String "description" |>.toOption
    -- Strict where the fields above are lenient, and held to the same three rules
    -- `github.pats` entries are (`Config.GitHubAuth`), because the failure is the same shape: a
    -- token that is present but not a usable one authenticates as nobody, and every write the
    -- identity makes is refused by taxis at the far end of a run that has already been paid for.
    --
    -- The silent cases are the ones worth refusing. A number, or the field misspelled as
    -- `taxisToken` — the name the Lean field carries, so a plausible typo — reads as *no token*,
    -- and an identity with no token quietly authors everything as orchestra: the run succeeds,
    -- and the only way to notice is to look at who signed the comments.
    let taxisToken ← match j.getObjVal? "taxis_token" with
      | .error _ => pure none
      | .ok v    =>
        match (FromJson.fromJson? v : Except String String) with
        | .error _ => throw s!"identity '{name}': 'taxis_token' must be a string"
        | .ok t =>
          if t.trimAscii.toString.isEmpty then
            throw s!"identity '{name}' has an empty 'taxis_token'; remove the field to act as \
orchestra, or fill it in"
          -- An unresolved `{{key}}` is a secret that secrets.json does not define. Left alone it
          -- becomes a bearer token authenticating as nobody, which taxis answers with a 401 —
          -- and `decide_issue` records a failed comment as one stderr line and completes the
          -- issue anyway, so the reviewer's verdict is simply lost.
          else if (t.splitOn "{{").length > 1 then
            throw s!"identity '{name}' still holds an unsubstituted placeholder in \
'taxis_token'; define it in secrets.json"
          else pure (some t)
    -- Held to the same rules, for the same reason: a token that is present but unusable fails
    -- the task at derive time, and one silently read as absent would run it under the role's or
    -- the default spec instead — the wrong access, with nothing to say so.
    let kleisSpec ← parseKleisSpec? j s!"identity '{name}': kleis_spec"
    let kleisToken ← match j.getObjVal? "kleis_token" with
      | .error _ => pure none
      | .ok v    =>
        match (FromJson.fromJson? v : Except String String) with
        | .error _ => throw s!"identity '{name}': 'kleis_token' must be a string"
        | .ok t =>
          if t.trimAscii.toString.isEmpty then
            throw s!"identity '{name}' has an empty 'kleis_token'; remove the field or fill it in"
          else if (t.splitOn "{{").length > 1 then
            throw s!"identity '{name}' still holds an unsubstituted placeholder in \
'kleis_token'; define it in secrets.json"
          else pure (some t.trimAscii.toString)
    -- One source per level: which of the two would win is a question with no right answer.
    if kleisSpec.isSome && kleisToken.isSome then
      throw s!"identity '{name}' names both a 'kleis_spec' and a 'kleis_token'; an identity has \
one source"
    let kleisNarrow ← parseKleisNarrow? j s!"identity '{name}': kleis_narrow"
    let resident ← match j.getObjVal? "resident" with
      | .error _ => pure none
      | .ok v    => some <$> parseResidentSpec s!"identity '{name}'" v
    return { name, description, taxisToken, kleisSpec, kleisToken, kleisNarrow, resident }

/-! ## Filesystem layout -/

/-- Optional override for the identities directory (tests redirect this, exactly as
    `Project.globalRolesDirOverride` does for roles). -/
initialize identitiesDirOverride : IO.Ref (Option System.FilePath) ← IO.mkRef none

def setIdentitiesDirOverride (p : Option System.FilePath) : IO Unit :=
  identitiesDirOverride.set p

/-- Where the identities live: `<config>/identities`. -/
def identitiesDir : IO System.FilePath := do
  match ← identitiesDirOverride.get with
  | some p => return p
  | none   => return (← Dirs.configBase) / "identities"

/-- One identity's directory: `<config>/identities/<name>`. -/
def identityDir (name : String) : IO System.FilePath := do
  Utils.ensureConfigName "identity" name
  return (← identitiesDir) / name

def identityFile (name : String) : IO System.FilePath := do
  return (← identityDir name) / "identity.json"

/-- The identity's standing instructions. Absent is fine — an identity is a memory and a name
    before it is a set of instructions. -/
def agentsFile (name : String) : IO System.FilePath := do
  return (← identityDir name) / "AGENTS.md"

/-- The root of one identity's own data: `<data>/identities/<name>`. -/
def identityDataDir (name : String) : IO System.FilePath := do
  Utils.ensureConfigName "identity" name
  return (← Dirs.dataBase) / "identities" / name

/-- The identity's memory directory. Not created — see `ensureMemoryDir`. -/
def memoryDir (name : String) : IO System.FilePath := do
  return (← identityDataDir name) / "memory"

/-- The identity's memory directory, created if this is the first task to run as it. -/
def ensureMemoryDir (name : String) : IO System.FilePath := do
  let dir ← memoryDir name
  IO.FS.createDirAll dir
  return dir

/-! ## Reading -/

/-- Read one identity. `none` when no record of that name exists.

    A file that exists and cannot be read is an error rather than a `none`, which is where this
    parts company with `Project.loadRole`. A role that fails to parse takes that role out of the
    dispatcher's rotation and nothing else happens; an identity that answered `none` would leave
    the task it belongs to looking exactly like a task that named no identity at all — running on
    orchestra's own taxis token, against no memory, with the run's whole point silently dropped.

    `{{secret}}` substitution is applied first, so the `taxis_token` can be held in
    `secrets.json` with the record naming it rather than holding it. -/
def loadIdentity (name : String) : IO (Option Identity) := do
  let path ← identityFile name
  if !(← path.pathExists) then return none
  let secrets ← loadSecrets
  let identity ← loadJsonFileWithSecrets Identity path secrets
  -- The directory name is what every other surface names it by, so a record whose `name`
  -- disagrees would be reachable under one spelling and print itself under another.
  unless identity.name == name do
    throw (.userError s!"{path}: this record names the identity '{identity.name}', but it is \
stored as '{name}'")
  -- Read as-is, with no substitution and no validation. It is prose for an agent, and the one
  -- thing orchestra could check about it — that it is not empty — is worth saying rather than
  -- refusing over, since a half-written file is still what its author meant to be there.
  let agentsPath ← agentsFile name
  let agents ← if ← agentsPath.pathExists then some <$> IO.FS.readFile agentsPath else pure none
  return some { identity with agents }

/-- Every configured identity, ordered by name so a listing is stable across calls.

    Unreadable records are reported on stderr and skipped rather than failing the listing: this
    is what answers "which identities are there?", and one broken file should not stop it
    answering. The task path uses `requireIdentity`, which does fail. -/
def loadAllIdentities : IO (Array Identity) := do
  let dir ← identitiesDir
  if !(← dir.pathExists) then return #[]
  let mut names : Array String := #[]
  for entry in ← System.FilePath.readDir dir do
    -- A directory holding a record. Anything else under `identities/` — a stray file, a
    -- directory somebody made and did not finish — is not an identity and is passed over in
    -- silence, the way the skill store passes over a directory with no `SKILL.md`.
    if !Utils.validConfigName entry.fileName then continue
    if ← (entry.path / "identity.json").pathExists then names := names.push entry.fileName
  let mut out : Array Identity := #[]
  for name in names.qsort (· < ·) do
    try
      if let some i ← loadIdentity name then out := out.push i
    catch e =>
      IO.eprintln s!"[identity] {name}: not a usable identity record, so it is being skipped: {e}"
  return out

/-- Resolve the identity a task names, or fail saying which ones exist.

    Every path that runs a task as somebody goes through this. A name with no record behind it is
    a configuration mistake with no safe reading: running the task anyway would run it as the
    instance, writing to the tracker under orchestra's own actor and into a memory the identity
    never sees, and it would look like it had worked. -/
def requireIdentity (name : String) : IO Identity := do
  match ← loadIdentity name with
  | some i => return i
  | none   =>
    let known := (← loadAllIdentities).map (·.name)
    let listed := if known.isEmpty then "none are configured"
                  else String.intercalate ", " known.toList
    let dir ← identitiesDir
    throw (.userError s!"no identity named '{name}' is configured in {dir} ({listed})")

end Orchestra.Identity
