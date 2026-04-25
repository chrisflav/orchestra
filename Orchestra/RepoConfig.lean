import Lean.Data.Json

open Lean (Json FromJson)

namespace Orchestra.RepoConfig

structure ValidationConfig where
  retryPrompt : String := "Validation failed. Please review the issues and fix them."
  maxRetries  : Nat    := 3
deriving Repr

structure RepoConfig where
  validation : ValidationConfig := {}
deriving Repr

instance : FromJson ValidationConfig where
  fromJson? j := do
    let retryPrompt :=
      j.getObjValAs? String "retry_prompt" |>.toOption
      |>.getD "Validation failed. Please review the issues and fix them."
    let maxRetries := j.getObjValAs? Nat "max_retries" |>.toOption |>.getD 3
    return { retryPrompt, maxRetries }

instance : FromJson RepoConfig where
  fromJson? j := do
    let validation :=
      j.getObjValAs? ValidationConfig "validation" |>.toOption |>.getD {}
    return { validation }

private def agentDir (repoPath : System.FilePath) : System.FilePath :=
  repoPath / ".agent"

/-- Load `.agent/config.json` from the repository. Returns defaults if absent or unparseable. -/
def loadRepoConfig (repoPath : System.FilePath) : IO RepoConfig := do
  let configPath := agentDir repoPath / "config.json"
  if !(← configPath.pathExists) then return {}
  let contents ← IO.FS.readFile configPath
  match Json.parse contents with
  | .error _ => return {}
  | .ok j =>
    match FromJson.fromJson? j with
    | .error _ => return {}
    | .ok cfg => return cfg

/--
Run `.agent/<name>` as a bash script with the repository as the working directory.
Does nothing if the script does not exist. Throws if the script exits non-zero.
-/
def runHook (repoPath : System.FilePath) (name : String) : IO Unit := do
  let hookPath := agentDir repoPath / name
  if !(← hookPath.pathExists) then return
  let child ← IO.Process.spawn {
    cmd  := "bash"
    args := #[hookPath.toString]
    cwd  := repoPath
    stdout := .inherit
    stderr := .inherit
  }
  let code ← child.wait
  if code != 0 then
    throw (.userError s!"hook {name} failed with exit code {code}")

/--
Run `.agent/init.sh` once after the repository is first cloned.
Completion is recorded in `.agent/.initialized`; subsequent calls are no-ops.
Does nothing if `.agent/` does not exist.
-/
def runInitIfNeeded (repoPath : System.FilePath) : IO Unit := do
  let dir := agentDir repoPath
  if !(← dir.pathExists) then return
  let markerPath := dir / ".initialized"
  if ← markerPath.pathExists then return
  runHook repoPath "init.sh"
  IO.FS.writeFile markerPath ""

/--
Run `.agent/validation.sh`.
Returns `(true, "")` if the script passes (exit 0) or does not exist.
Returns `(false, output)` if the script exits non-zero, where `output` is the
combined stdout and stderr of the script. Does not throw.
-/
def runValidation (repoPath : System.FilePath) : IO (Bool × String) := do
  let hookPath := agentDir repoPath / "validation.sh"
  if !(← hookPath.pathExists) then return (true, "")
  let child ← IO.Process.spawn {
    cmd  := "bash"
    args := #[hookPath.toString]
    cwd  := repoPath
    stdout := .piped
    stderr := .piped
  }
  let stdout ← child.stdout.readToEnd
  let stderr ← child.stderr.readToEnd
  let code ← child.wait
  let combined := (stdout ++ stderr).trimAscii.toString
  return (code == 0, combined)

end Orchestra.RepoConfig
