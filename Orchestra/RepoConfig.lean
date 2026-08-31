import Lean.Data.Json
import Orchestra.Exec.Backend

open Lean (Json FromJson)

namespace Orchestra.RepoConfig

structure ValidationConfig where
  retryPrompt : String := "Validation failed. Please review the issues and fix them."
  maxRetries  : Nat    := 3
deriving Repr

structure RepoConfig where
  validation : ValidationConfig := {}
  /-- The image this repository's tasks should run in, when the execution backend runs them in one.

      ```json
      { "execution": { "image": "ghcr.io/acme/widgets-ci:latest" } }
      ```

      The repository is what knows which build and dev dependencies its tasks need — a toolchain, a
      JDK, a browser, a database client — and it already says so somewhere for its own CI. Ignored
      by the backends that run tasks on the daemon's own machine, where there is nothing to choose:
      what is installed there is what is installed there. -/
  image : Option String := none
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
    let image := (j.getObjVal? "execution" |>.toOption).bind fun e =>
      e.getObjValAs? String "image" |>.toOption
    return { validation, image }

/-- Return the per-repo config directory, preferring `.orchestra/` and falling back to `.agent/`. -/
private def orchestraDir (repoPath : System.FilePath) : IO System.FilePath := do
  let newDir := repoPath / ".orchestra"
  if ← newDir.pathExists then return newDir
  let oldDir := repoPath / ".agent"
  if ← oldDir.pathExists then return oldDir
  return newDir

/-- Load `.orchestra/config.json` (or `.agent/config.json`) from the repository. Returns defaults if absent or unparseable. -/
def loadRepoConfig (repoPath : System.FilePath) : IO RepoConfig := do
  let configPath := (← orchestraDir repoPath) / "config.json"
  if !(← configPath.pathExists) then return {}
  let contents ← IO.FS.readFile configPath
  match Json.parse contents with
  | .error _ => return {}
  | .ok j =>
    match FromJson.fromJson? j with
    | .error _ => return {}
    | .ok cfg => return cfg

/-!
## Running the repository's scripts

The scripts belong to the repository and run where the agent works — which is the daemon's own
machine for the landrun and local backends, and inside the task's pod for a backend that runs the
agent elsewhere. They go through `Exec.Session.runScript` for that reason, and nothing else about
them changed: `bash`, the checkout as the working directory, hooks inheriting the daemon's streams
and `validation.sh` having its output captured.

Whether a script *exists* is still read here, from the checkout on this machine. It is the same
checkout the session was opened on, so there is nothing a remote lookup would tell us that this
does not.
-/

/--
Run `.orchestra/<name>` (or `.agent/<name>`) as a bash script with the repository as the working
directory. Does nothing if the script does not exist. Throws if the script exits non-zero.
-/
def runHook (session : Exec.Session) (repoPath : System.FilePath) (name : String) : IO Unit := do
  let hookPath := (← orchestraDir repoPath) / name
  if !(← hookPath.pathExists) then return
  let result ← session.runScript {
    path := hookPath.toString, workdir := repoPath, stdio := .inherit }
  if result.exitCode != 0 then
    throw (.userError s!"hook {name} failed with exit code {result.exitCode}")

/--
Run `.orchestra/init.sh` (or `.agent/init.sh`) once after the repository is first cloned.
Completion is recorded in the same directory as `.initialized`; subsequent calls are no-ops.
Does nothing if neither `.orchestra/` nor `.agent/` exists in the repo.

"Once" means once per *environment*, which is why the marker is not the whole answer. It lives in
the checkout, and the checkout is exactly the thing an execution backend carries into a container
that has nothing installed — so a session that starts fresh every task (`freshEnvironment`) runs the
hook every task, marker or no marker. A repository's `init.sh` should be idempotent, and the ones
that install a toolchain or warm a cache already are: they check before they fetch.
-/
def runInitIfNeeded (session : Exec.Session) (repoPath : System.FilePath) : IO Unit := do
  let dir ← orchestraDir repoPath
  if !(← dir.pathExists) then return
  let markerPath := dir / ".initialized"
  if (← markerPath.pathExists) && !session.freshEnvironment then return
  runHook session repoPath "init.sh"
  IO.FS.writeFile markerPath ""

/-- Returns `true` if `validation.sh` exists in the repo's `.orchestra/` or `.agent/` directory. -/
def hasValidationScript (repoPath : System.FilePath) : IO Bool := do
  return (← (← orchestraDir repoPath) / "validation.sh" |>.pathExists)

/--
Run `.orchestra/validation.sh` (or `.agent/validation.sh`).
Returns `(true, "")` if the script passes (exit 0) or does not exist.
Returns `(false, output)` if the script exits non-zero, where `output` is the
combined stdout and stderr of the script. Does not throw.
-/
def runValidation (session : Exec.Session) (repoPath : System.FilePath) : IO (Bool × String) := do
  if !(← hasValidationScript repoPath) then return (true, "")
  let hookPath := (← orchestraDir repoPath) / "validation.sh"
  let result ← session.runScript {
    path := hookPath.toString, workdir := repoPath, stdio := .piped }
  return (result.exitCode == 0, result.output)

end Orchestra.RepoConfig
