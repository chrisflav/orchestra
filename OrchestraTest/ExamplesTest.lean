import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra

namespace OrchestraTest.Examples

/-! Parse the configuration files shipped under `examples/`.

    Every other test in this suite builds its JSON inline, so nothing checked that the shipped
    examples still decode. They drifted: the taxis migration turned a listener's `project_id`
    from a string into a taxis issue id (a bare JSON number) and
    `examples/listeners/auto-dispatcher.json` kept its old string, which
    `loadAllListenerConfigs` reports as a warning and then skips — a dispatcher that silently
    never dispatches. These tests exist so a shipped example that no longer parses fails the
    build instead. The listener case also checks that no example carries a `name` field, since
    an example is the likeliest thing to be copied into a listeners directory unrenamed.

    Paths are relative to the package root, which is `lake test`'s working directory. -/

private def examplesDir : System.FilePath := "examples"

/-- Every `*.json` under `dir`, or `none` if `dir` is missing. -/
private def jsonFilesIn (dir : System.FilePath) : IO (Option (Array System.FilePath)) := do
  if !(← dir.pathExists) then return none
  let entries ← System.FilePath.readDir dir
  return some ((entries.filter (·.fileName.endsWith ".json")).map (·.path))

@[test]
def listenerExamplesParse : Test := do
  match ← jsonFilesIn (examplesDir / "listeners") with
  | none => TestM.fail s!"{examplesDir}/listeners not found (wrong working directory?)"
  | some files =>
    TestM.assert (!files.isEmpty) "expected at least one listener example"
    for path in files do
      let raw ← IO.FS.readFile path
      match Json.parse raw with
      | .error e => TestM.fail s!"{path}: invalid JSON: {e}"
      | .ok j =>
        match (FromJson.fromJson? j : Except String Listener.ListenerConfig) with
        | .error e => TestM.fail s!"{path}: does not decode as ListenerConfig: {e}"
        | .ok _ =>
          -- A listener is named by its file. Two of these examples used to carry a `name` that
          -- disagreed with theirs, so copying one into a listeners directory without renaming it
          -- produced a listener the daemon could list and never load.
          TestM.assert (j.getObjVal? "name" |>.toOption |>.isNone)
            (msg := s!"{path}: carries a 'name' field; a listener is named by its file")

@[test]
def roleExamplesParse : Test := do
  match ← jsonFilesIn (examplesDir / "projects" / "roles") with
  | none => TestM.fail s!"{examplesDir}/projects/roles not found (wrong working directory?)"
  | some files =>
    TestM.assert (!files.isEmpty) "expected at least one role example"
    for path in files do
      let raw ← IO.FS.readFile path
      match Json.parse raw with
      | .error e => TestM.fail s!"{path}: invalid JSON: {e}"
      | .ok j =>
        match (FromJson.fromJson? j : Except String Project.Role) with
        | .error e => TestM.fail s!"{path}: does not decode as Role: {e}"
        | .ok _ => TestM.assert true

/-- The example identities, each loaded the way orchestra loads one.

    Through `loadIdentity` per directory rather than `loadAllIdentities`, which catches and skips
    what it cannot read: under that, a shipped example whose record had rotted was skipped in
    silence and every assertion here still held — the test passed while checking nothing about
    the example it was named for.

    A shipped example may legitimately fail to load, and exactly one way: `maintainer` carries
    its token as a `{{secret}}` placeholder, which is how a real one should be written, and
    `secrets.json` does not define it on a machine that has not been set up. That refusal is the
    behaviour we want and is asserted as such. Any other refusal is a broken example. -/
@[test]
def identityExamplesLoad : Test := do
  let dir := examplesDir / "identities"
  if !(← dir.pathExists) then
    TestM.fail s!"{dir} not found (wrong working directory?)"
    return
  let mut names : Array String := #[]
  for entry in ← System.FilePath.readDir dir do
    if ← (entry.path / "identity.json").pathExists then names := names.push entry.fileName
  TestM.assert (names.size ≥ 2) s!"expected the shipped identity examples, found {names.size}"
  let previous ← Identity.identitiesDirOverride.get
  Identity.setIdentitiesDirOverride (some dir)
  for name in names do
    let outcome : Except String Identity.Identity ← try
        match ← Identity.loadIdentity name with
        | none   =>
          pure (Except.error "loadIdentity answered none for a directory it just listed")
        | some i => pure (Except.ok i)
      catch e => pure (Except.error (toString e))
    match outcome with
    | .ok identity =>
      TestM.assertEqual identity.name name (msg := s!"{name}: named by its directory")
      TestM.assert identity.agents.isSome
        (msg := s!"{name}: an example identity should ship the AGENTS.md that shows what one is \
for")
    | .error why =>
      -- The one refusal a shipped example is allowed: an undefined secret. Anything else — a
      -- record that stopped decoding, a name that drifted from its directory — is a real break.
      TestM.assert ((why.splitOn "unsubstituted placeholder").length > 1)
        (msg := s!"{name}: the only reason a shipped example may fail to load is an undefined \
secret, got: {why}")
  Identity.setIdentitiesDirOverride previous

end OrchestraTest.Examples
