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
    build instead.

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
        | .ok _ => TestM.assert true

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

end OrchestraTest.Examples
