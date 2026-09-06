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

/-- The example identities. `maintainer` carries its token as a `{{secret}}` placeholder, which
    is how a real one should be written — so this also pins that a record with an unsubstituted
    placeholder in it still decodes, since `loadIdentity` substitutes before parsing and an
    example is read here without any secrets to substitute.

    Read through `loadIdentity` rather than off the files, because half of what an identity is
    lives beside its record: this is what checks that the shipped `AGENTS.md` files are actually
    picked up, and not just present. -/
@[test]
def identityExamplesLoad : Test := do
  let dir := examplesDir / "identities"
  if !(← dir.pathExists) then
    TestM.fail s!"{dir} not found (wrong working directory?)"
    return
  let previous ← Identity.identitiesDirOverride.get
  let loaded ← try
      Identity.setIdentitiesDirOverride (some dir)
      Identity.loadAllIdentities
    finally
      Identity.setIdentitiesDirOverride previous
  TestM.assert (!loaded.isEmpty) "expected at least one identity example"
  for identity in loaded do
    -- Named by its directory, like a listener is by its file and for the same reason: an example
    -- copied into an identities directory unrenamed would be refused by `loadIdentity`, which is
    -- what `loadAllIdentities` just went through.
    TestM.assert (!identity.name.isEmpty) "an identity example with no name"
    TestM.assert (identity.agents.isSome)
      (msg := s!"{identity.name}: an example identity should ship the AGENTS.md that shows what \
one is for")

end OrchestraTest.Examples
