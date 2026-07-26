import OrchestraTest.TestM
import Orchestra

open Orchestra
open Orchestra.Dashboard

namespace OrchestraTest.ConfigApi

/-!
# The configuration write paths

`Orchestra.Dashboard`'s write routes are thin: they check a credential, check a content type,
validate a document, and hand it to the store. The credential and the content type are pinned in
`OrchestraTest.Dashboard`, which is where the rest of the HTTP surface lives. What is here is the
other two thirds — the validation each resource applies, and the store round trip underneath it —
because those are what decide whether a bad request can leave the daemon reading a file that is
not a config.

Nothing here opens a socket. The validators are pure or `IO`-over-the-filesystem, and the stores
are redirected to a temporary directory, so the suite has no server, no port and no network.
-/

private def tempRoot (tag : String) : IO System.FilePath := do
  return System.FilePath.mk "/tmp" / s!"orchestra-config-api-{tag}-{← IO.monoNanosNow}"

/-- Run `act` with the listener, role and skill stores redirected into a temporary directory, so
    that a test writing configuration cannot reach the developer's own. -/
private def withTempStores (act : IO α) : IO α := do
  let root ← tempRoot "stores"
  IO.FS.createDirAll (root / "listeners")
  IO.FS.createDirAll (root / "listener-state")
  IO.FS.createDirAll (root / "roles")
  IO.FS.createDirAll (root / "skills")
  Listener.setListenersConfigDirOverride (some (root / "listeners"))
  Listener.setListenerStateDirOverride (some (root / "listener-state"))
  Project.setGlobalRolesDirOverride (some (root / "roles"))
  Skill.setSkillsDirOverride (some (root / "skills"))
  try act
  finally
    Listener.setListenersConfigDirOverride none
    Listener.setListenerStateDirOverride none
    Project.setGlobalRolesDirOverride none
    Skill.setSkillsDirOverride none
    try IO.FS.removeDirAll root catch _ => pure ()

/-! ## Names

Every one of the three resources turns its name into a path component, so all three go through
the same check. It is enforced in the store rather than at the HTTP boundary because the CLI
writes through the same functions. -/

@[test]
def validConfigName_acceptsNamesAndRejectsPaths : Test := do
  TestM.assert (Utils.validConfigName "nightly")
  TestM.assert (Utils.validConfigName "issue-triage")
  TestM.assert (Utils.validConfigName "role_v2")
  TestM.assert (Utils.validConfigName "with space")
    (msg := "a space is awkward but names itself; the CLI percent-encodes it")
  TestM.assert (!Utils.validConfigName "") (msg := "the empty name")
  TestM.assert (!Utils.validConfigName "..") (msg := "the parent directory")
  TestM.assert (!Utils.validConfigName ".") (msg := "the current directory")
  TestM.assert (!Utils.validConfigName ".hidden")
    (msg := "a leading dot hides the file from every listing that skips dotfiles")
  TestM.assert (!Utils.validConfigName "a/b") (msg := "a separator")
  TestM.assert (!Utils.validConfigName "a\\b") (msg := "a backslash separator")
  TestM.assert (!Utils.validConfigName "a\nb") (msg := "a newline, which no table can print")

/-! ## Listener validation -/

private def goodListener : String :=
  "{\"name\": \"nightly\", \"interval_seconds\": 300,
    \"source\": {\"type\": \"shell\", \"command\": \"echo\", \"args\": [\"hi\"]},
    \"action\": {\"mode\": \"pr\", \"upstream\": \"o/r\", \"fork\": \"o/r\",
                 \"prompt_template\": \"do the thing\"}}"

/-- What `validateListenerConfig` said, as `none` for "accepted". -/
private def listenerVerdict (name body : String) : IO (Option String) := do
  match ← Listener.validateListenerConfig name body with
  | .ok _    => return none
  | .error e => return some e

@[test]
def validateListenerConfig_acceptsAWellFormedConfig : Test := do
  TestM.assertEqual (← listenerVerdict "nightly" goodListener) none
    (msg := "a config that names itself and parses is accepted")

@[test]
def validateListenerConfig_rejectsWhatWouldCorruptTheStore : Test := do
  -- Each of these is a distinct mistake, and each has to come back as a sentence rather than as
  -- a file the daemon then fails to parse on every tick for the rest of its life.
  TestM.assert (← listenerVerdict "nightly" "{not json" |>.map Option.isSome)
    (msg := "a body that is not JSON")
  TestM.assert (← listenerVerdict "nightly" "{\"hello\": 1}" |>.map Option.isSome)
    (msg := "valid JSON that is not a listener")
  TestM.assert
    (← listenerVerdict "morning" goodListener |>.map Option.isSome)
    (msg := "a config whose own name disagrees with the one it is stored under")
  TestM.assert (← listenerVerdict "a/b" goodListener |>.map Option.isSome)
    (msg := "a name that is a path")
  let zeroInterval := goodListener.replace "\"interval_seconds\": 300" "\"interval_seconds\": 0"
  TestM.assert (← listenerVerdict "nightly" zeroInterval |>.map Option.isSome)
    (msg := "a zero poll interval would spin against the source as fast as the network allows")

/-! ## Role validation -/

private def goodRole : String :=
  "{\"name\": \"implementor\", \"permissions\": [\"create_pr\", \"work_issues\"],
    \"prompt_template\": \"work {{issue_id}}\", \"priority\": 20}"

private def roleVerdict (name body : String) : Option String :=
  match Project.validateRole name body with
  | .ok _    => none
  | .error e => some e

@[test]
def validateRole_acceptsAWellFormedRole : Test := do
  TestM.assertEqual (roleVerdict "implementor" goodRole) none

@[test]
def validateRole_rejectsWhatWouldDispatchAnAgentWrong : Test := do
  TestM.assert (roleVerdict "implementor" "nope").isSome (msg := "a body that is not JSON")
  TestM.assert (roleVerdict "implementor" "{\"name\": \"implementor\"}").isSome
    (msg := "a role with no permissions and no prompt template")
  TestM.assert (roleVerdict "reviewer" goodRole).isSome
    (msg := "a role whose own name disagrees with the one it is stored under")
  let emptyPrompt := goodRole.replace "work {{issue_id}}" "   "
  TestM.assert (roleVerdict "implementor" emptyPrompt).isSome
    (msg := "an empty prompt template dispatches an agent with nothing to do")
  -- The one worth having: a permission the sandbox does not grant is otherwise invisible until a
  -- dispatched agent finds itself without the tool it was told to use.
  let typo := goodRole.replace "\"work_issues\"" "\"work_issue\""
  TestM.assert (roleVerdict "implementor" typo).isSome
    (msg := "a misspelled permission")

@[test]
def roleKnownPermissions_isTheOptionalToolSet : Test := do
  -- Pinned because the check above is only as good as this list: a tool group added to the MCP
  -- server and not added here is one no role may name.
  for p in ["create_pr", "comment", "label_issue", "manage_issues", "work_issues",
            "review_issues"] do
    TestM.assert (Project.Role.knownPermissions.contains p) (msg := s!"{p} is grantable")
  -- `merge_pr` is not here on purpose: a role dispatched at whatever issue comes along is not
  -- the thing to hand a merge button to. Only a task file may name it.
  TestM.assert (!Project.Role.knownPermissions.contains "merge_pr")
    (msg := "merge_pr is not grantable by role")
  TestM.assertEqual Project.Role.knownPermissions.length 6
    (msg := "no permission has been added without deciding what it means for a role")

/-! ## Skill validation

A skill's front matter is not decoration: an agent reads the `description` to decide whether the
skill is relevant before it reads the body, so a file without one is loaded and never used.
Writing one through the API therefore requires both fields. -/

private def goodSkill : String :=
  "---\nname: pull-requests\ndescription: How to open a PR from inside a task.\n---\n\n\
# PRs\n\nUse the tools."

private def skillVerdict (name body : String) : Option String :=
  match Skill.validate name body with
  | .ok _    => none
  | .error e => some e

@[test]
def validateSkill_acceptsAWellFormedSkill : Test := do
  TestM.assertEqual (skillVerdict "pull-requests" goodSkill) none
  match Skill.validate "pull-requests" goodSkill with
  | .ok s =>
    TestM.assertEqual s.description (some "How to open a PR from inside a task.")
      (msg := "the description is read back off the front matter")
  | .error e => TestM.fail e

@[test]
def validateSkill_rejectsWhatAnAgentWouldSilentlyIgnore : Test := do
  TestM.assert (skillVerdict "x" "").isSome (msg := "an empty file")
  TestM.assert (skillVerdict "x" "# Just a heading").isSome
    (msg := "no front matter block at all")
  TestM.assert (skillVerdict "x" "---\nname: x\ndescription: d\n").isSome
    (msg := "a front matter block that is never closed")
  TestM.assert (skillVerdict "x" "---\ndescription: d\n---\nbody").isSome
    (msg := "no name; an agent lists skills by it")
  TestM.assert (skillVerdict "x" "---\nname: y\ndescription: d\n---\nbody").isSome
    (msg := "a name disagreeing with the one it is stored under")
  TestM.assert (skillVerdict "x" "---\nname: x\n---\nbody").isSome
    (msg := "no description, which is the field that decides whether it is ever read")
  TestM.assert (skillVerdict "x" "---\nname: x\ndescription:\n---\nbody").isSome
    (msg := "an empty description")

@[test]
def parseFrontMatter_keepsAColonInsideAValue : Test := do
  -- Descriptions routinely contain a colon, and splitting on the last one would truncate them.
  match Skill.parseFrontMatter "---\nname: x\ndescription: Use this: always.\n---\nbody" with
  | .ok fields =>
    TestM.assertEqual ((fields.find? (·.1 == "description")).map (·.2))
      (some "Use this: always.")
  | .error e => TestM.fail e

/-! ## Round trips

The store half. Each of these is the shape of one successful write followed by the read the API
answers it with, and then the delete — which is the sequence `PUT`, `GET`, `DELETE` performs. -/

@[test]
def listenerRoundTrip : Test := do
  let outcome ← withTempStores do
    -- Nothing there to begin with.
    let before ← Listener.loadListenerConfigRaw "nightly"
    Listener.saveListenerConfigRaw "nightly" goodListener
    let raw    ← Listener.loadListenerConfigRaw "nightly"
    let parsed ← Listener.loadListenerConfig "nightly"
    -- State is separate from config, which is what makes enable/disable a sub-resource.
    let st ← Listener.loadListenerState "nightly"
    Listener.saveListenerState "nightly" { st with enabled := false }
    let disabled ← Listener.loadListenerState "nightly"
    let removed ← Listener.deleteListenerConfig "nightly"
    let after ← Listener.loadListenerConfigRaw "nightly"
    -- The state file goes with the config: a listener re-created under the same name must not
    -- inherit its predecessor's list of already-handled events.
    let stateAfter ← Listener.loadListenerState "nightly"
    let removedAgain ← Listener.deleteListenerConfig "nightly"
    return (before, raw, parsed.map (·.intervalSeconds), disabled.enabled, removed, after,
            stateAfter.enabled, removedAgain)
  let (before, raw, interval, disabled, removed, after, stateAfter, removedAgain) := outcome
  TestM.assertEqual before none (msg := "nothing is stored before the write")
  TestM.assertEqual raw (some goodListener)
    (msg := "stored verbatim, so placeholders and unknown fields survive an edit")
  TestM.assertEqual interval (some 300) (msg := "and it parses back as the daemon reads it")
  TestM.assertEqual disabled false (msg := "enabled lives in the state file and toggles alone")
  TestM.assertEqual removed true (msg := "the delete reports that it removed something")
  TestM.assertEqual after none (msg := "and it is gone")
  TestM.assertEqual stateAfter true
    (msg := "the state went with it, so a re-created listener starts from a clean slate")
  TestM.assertEqual removedAgain false
    (msg := "deleting what is not there is a 404, not a silent success")

@[test]
def roleRoundTrip : Test := do
  let (before, raw, loaded, removed, after) ← withTempStores do
    let before ← Project.loadGlobalRoleRaw "implementor"
    Project.saveGlobalRoleRaw "implementor" goodRole
    let raw ← Project.loadGlobalRoleRaw "implementor"
    let loaded := (← Project.loadGlobalRoles).find? (·.name == "implementor")
    let removed ← Project.deleteGlobalRole "implementor"
    let after ← Project.loadGlobalRoleRaw "implementor"
    return (before, raw, loaded.map (·.priority), removed, after)
  TestM.assertEqual before none
  TestM.assertEqual raw (some goodRole) (msg := "stored verbatim")
  TestM.assertEqual loaded (some 20)
    (msg := "and the dispatcher's own loader sees it, which is what makes the write take effect")
  TestM.assertEqual removed true
  TestM.assertEqual after none

@[test]
def skillRoundTrip : Test := do
  let (before, loaded, listed, removed, after, removedAgain) ← withTempStores do
    let before ← Skill.loadSkill "pull-requests"
    Skill.saveSkill "pull-requests" goodSkill
    let loaded ← Skill.loadSkill "pull-requests"
    let listed ← Skill.loadAllSkills
    let removed ← Skill.deleteSkill "pull-requests"
    let after ← Skill.loadSkill "pull-requests"
    let removedAgain ← Skill.deleteSkill "pull-requests"
    return (before.isSome, loaded.map (·.content), listed.map (·.name), removed, after.isSome,
            removedAgain)
  TestM.assertEqual before false
  TestM.assertEqual loaded (some goodSkill) (msg := "the Markdown comes back byte for byte")
  TestM.assertEqual listed #["pull-requests"]
  TestM.assertEqual removed true
  TestM.assertEqual after false
  TestM.assertEqual removedAgain false (msg := "removing what is not there reports so")

@[test]
def savingASkillSeedsThePluginManifest : Test := do
  -- A `skills/` tree without `.claude-plugin/plugin.json` is invisible to every agent backend,
  -- so creating the first skill through the API would otherwise appear to do nothing at all.
  let present ← withTempStores do
    Skill.saveSkill "pull-requests" goodSkill
    ((← Skill.pluginDir) / ".claude-plugin" / "plugin.json").pathExists
  TestM.assert present (msg := "the plugin manifest is written alongside the first skill")

@[test]
def aRejectedBodyLeavesTheStoredConfigAlone : Test := do
  -- The property the whole write path exists to hold: validation runs to completion against the
  -- text the client sent, and the store is only reached once it has passed. A `400` must not be
  -- a config file the daemon then fails to parse on every tick.
  let (raw, stillParses) ← withTempStores do
    Listener.saveListenerConfigRaw "nightly" goodListener
    -- Exactly what a `PUT` of a broken body does: validate first, and never reach the store.
    let verdict ← Listener.validateListenerConfig "nightly" "{\"name\": \"nightly\"}"
    if verdict.toOption.isSome then
      Listener.saveListenerConfigRaw "nightly" "{\"name\": \"nightly\"}"
    let raw ← Listener.loadListenerConfigRaw "nightly"
    return (raw, (← Listener.loadListenerConfig "nightly").isSome)
  TestM.assertEqual raw (some goodListener) (msg := "the good config is still there, untouched")
  TestM.assert stillParses (msg := "and the daemon can still read it")

/-! ### Traversal through a body-supplied name

The path-based routes (`PUT`, `DELETE`, `GET /…/{name}`) run their component through
`safeSegment`, which is covered in `OrchestraTest.Dashboard`. `POST` does not: it names the
record from the request **body**, so nothing upstream has decoded or rejected the string, and the
only thing standing between it and a filename is the resource's own validator.

That gap was real. `Skill.validate` shipped without the name check the other two had, and because
the front matter check only compares two strings the caller controls, a `POST` of
`{"name": "../../…", "content": "---\nname: ../../…\n---\n…"}` was accepted and wrote a
`SKILL.md` outside the skills root. Hence: all three validators, and the stores under them,
checked here against the same set of names. -/

/-- Names that must never become a path, whichever resource they are offered to. -/
private def traversalNames : List String :=
  ["..", ".", "../evil", "../../../../tmp/pwned", "a/b", "/etc/passwd", "a\\b", "",
   ".hidden", "with\nnewline"]

/-- Whether a rejection came from the shared name check rather than from something incidental.

    Worth distinguishing: a traversing name also trips other rules by accident — `""` fails the
    front matter comparison, `"a/b"` might fail a parse — so a test that only asserted "rejected"
    would pass against a validator with no name check at all. This asserts *which* rule fired. -/
private def isNameRejection (verdict : Option String) : Bool :=
  match verdict with
  | none   => false
  | some e => (e.splitOn "is not a usable").length > 1

@[test]
def noValidatorAcceptsATraversingName : Test := do
  for bad in traversalNames do
    -- The body is otherwise well-formed and self-consistent — including the front matter naming
    -- itself exactly as the path does, which is what made the skill case slip through.
    let listener := goodListener.replace "\"name\": \"nightly\"" s!"\"name\": \"{bad}\""
    TestM.assert (isNameRejection (← listenerVerdict bad listener))
      (msg := s!"listener named {repr bad} must be rejected as an unusable name")
    let role := goodRole.replace "\"name\": \"implementor\"" s!"\"name\": \"{bad}\""
    TestM.assert (isNameRejection (roleVerdict bad role))
      (msg := s!"role named {repr bad} must be rejected as an unusable name")
    let skill := s!"---\nname: {bad}\ndescription: traversal probe\n---\n\nbody"
    TestM.assert (isNameRejection (skillVerdict bad skill))
      (msg := s!"skill named {repr bad} must be rejected as an unusable name")

@[test]
def noStoreWritesOutsideItsRootEvenIfAskedDirectly : Test := do
  -- The backstop under the validators: the save functions refuse the name themselves, so the
  -- property is one the store holds rather than one every caller is trusted to have established.
  -- A traversing name here is a bug in a caller, so it throws rather than returning.
  let escaped ← withTempStores do
    let root ← Skill.skillsRoot
    let outside := root / ".." / ".." / "escaped"
    for bad in ["../../escaped", "../escaped"] do
      try Skill.saveSkill bad "---\nname: x\ndescription: d\n---\nb" catch _ => pure ()
      try Listener.saveListenerConfigRaw bad goodListener catch _ => pure ()
      try Project.saveGlobalRoleRaw bad goodRole catch _ => pure ()
      try Listener.saveListenerState bad { lastChecked := "", processedIds := #[] }
      catch _ => pure ()
    -- Nothing may have appeared above the store roots.
    (outside / "SKILL.md").pathExists
  TestM.assert (!escaped) (msg := "no store wrote above its own root")

@[test]
def loadersRefuseATraversingNameRatherThanStatIt : Test := do
  -- The existence probe in every writer runs *after* validation, but the loaders refuse the name
  -- as well, so a `409`-versus-`404` oracle cannot be built out of one even if that order were
  -- ever reversed.
  let threw ← withTempStores do
    let mut all := true
    for bad in ["../../../etc/passwd", ".."] do
      let a ← try let _ ← Skill.loadSkill bad; pure false catch _ => pure true
      let b ← try let _ ← Listener.loadListenerConfigRaw bad; pure false catch _ => pure true
      let c ← try let _ ← Project.loadGlobalRoleRaw bad; pure false catch _ => pure true
      all := all && a && b && c
    return all
  TestM.assert threw (msg := "every loader refuses to build a path from a traversing name")

@[test]
def listingsSkipRatherThanThrowOnAnUnusableName : Test := do
  -- The flip side of the loaders throwing: a stray directory left by hand must not take the
  -- whole listing down with it.
  let names ← withTempStores do
    Skill.saveSkill "good" "---\nname: good\ndescription: d\n---\nb"
    let root ← Skill.skillsRoot
    IO.FS.createDirAll (root / ".hidden")
    IO.FS.writeFile (root / ".hidden" / "SKILL.md") "---\nname: .hidden\ndescription: d\n---\nb"
    return (← Skill.loadAllSkills).map (·.name)
  TestM.assertEqual names #["good"]
    (msg := "the unusable directory is skipped and the listing still answers")

/-! ## Authentication

The gate in `Orchestra.Dashboard.route` is one check with `publicPaths` as its exemption list, so
"is this route authenticated?" is answerable without a socket: it is authenticated unless its
path is in that array. What is asserted here is that no write is. -/

@[test]
def everyWriteRouteRequiresACredential : Test := do
  let mut writes := 0
  for (kind, methods) in apiRoutes do
    let path := s!"/api/{apiVersion}/{kind}"
    for m in methods do
      if m != "get" then
        writes := writes + 1
        TestM.assert (!publicPaths.contains path)
          (msg := s!"{m.toUpper} {path} would be reachable without a credential")
  -- A guard on the guard: if `apiRoutes` ever loses its writes, the loop above passes vacuously.
  TestM.assert (writes ≥ 10)
    (msg := s!"the write surface is still present ({writes} non-GET routes)")

end OrchestraTest.ConfigApi
