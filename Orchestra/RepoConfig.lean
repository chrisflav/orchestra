import Lean.Data.Json
import Orchestra.Exec.Backend
import Orchestra.Repo

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

/-- The repository's scripts and `config.json`, as the tip of its default branch has them, copied
    into a directory only the daemon can reach (`Repo.snapshotScripts`).

    Everything in this module reads from here and never from the checkout. The checkout is the
    agent's to write, `.orchestra/` included, and these scripts are run by the daemon — on this
    machine under the local backend, and in the agent's sandbox, with the daemon choosing what runs,
    under landrun. Read from the working tree, an agent that
    rewrote `validation.sh` had the daemon run whatever it wrote, and a task checked out at a pull
    request's head ran that pull request's scripts. The copy is made before the agent starts and
    removed when the task ends (`discard`). -/
structure Scripts where
  /-- The copy: `<data>/scripts/<label>/.orchestra` (or `.agent`). -/
  dir : System.FilePath
  /-- What the directory is called in the checkout: `.orchestra`, or the legacy `.agent`. -/
  name : String
deriving Repr

namespace Scripts

/-- Where the copy for `label` (a task's or a session's id) is kept. -/
def root (label : String) : IO System.FilePath :=
  return (← Dirs.dataBase) / "scripts" / label

/-- Copy the scripts of the repository checked out at `repoPath` for `label`, replacing any copy
    left by a run of the same label; `none` when its default branch has no script directory. -/
def prepare (repoPath : System.FilePath) (label : String) : IO (Option Scripts) := do
  let dest ← root label
  if ← dest.pathExists then IO.FS.removeDirAll dest
  match ← Repo.snapshotScripts repoPath dest with
  | some name => return some { dir := dest / name, name }
  | none      => return none

/-- Remove the copy for `label`. Best effort: a copy left behind holds nothing but the
    repository's own published scripts. -/
def discard (label : String) : IO Unit := do
  let dest ← root label
  try if ← dest.pathExists then IO.FS.removeDirAll dest catch _ => pure ()

end Scripts

/-- Load `config.json` from the repository's script directory. Returns defaults if absent or
    unparseable, or when there is no script directory. -/
def loadRepoConfig (scripts : Option Scripts) : IO RepoConfig := do
  let some sc := scripts | return {}
  let configPath := sc.dir / "config.json"
  if !(← configPath.pathExists) then return {}
  let contents ← IO.FS.readFile configPath
  match Json.parse contents with
  | .error _ => return {}
  | .ok j =>
    match FromJson.fromJson? j with
    | .error _ => return {}
    | .ok cfg => return cfg

/-- `config.json` as the default branch of the repository checked out at `repoPath` has it, for a
    caller that needs only the configuration and runs no script: a copy is made, read and removed.
    Defaults when there is none, or when `repoPath` is not a repository at all. -/
def loadDefaultBranchConfig (repoPath : System.FilePath) : IO RepoConfig := do
  let label := s!"config-{← Exec.randomHex 6}"
  try loadRepoConfig (← Scripts.prepare repoPath label)
  catch _ => return {}
  finally Scripts.discard label

/-!
## Running the repository's scripts

The scripts belong to the repository and run where the agent works — which is the daemon's own
machine for the landrun and local backends, and inside the task's pod for a backend that runs the
agent elsewhere. They go through `Exec.Session.runScript` for that reason: `bash`, the checkout as
the working directory, hooks inheriting the daemon's streams and `validation.sh` having its output
captured.

What runs is the copy (`Scripts`), never the working tree. The path a script is given is still the
one it has in the checkout, `<repo>/.orchestra/<name>`, so that a pod, which has the checkout and
not the copy, runs it where it always did; `ScriptSpec.trusted` tells a backend running it on this
machine where the copy is.
-/

/--
Run `<scripts>/<name>` as a bash script with the repository as the working directory. Does nothing
if there are no scripts or the script does not exist. Throws if the script exits non-zero.
-/
def runHook (session : Exec.Session) (repoPath : System.FilePath) (scripts : Option Scripts)
    (name : String) (runId : Option String := none) : IO Unit := do
  let some sc := scripts | return
  if !(← (sc.dir / name).pathExists) then return
  let result ← session.runScript {
    path := (repoPath / sc.name / name).toString, trusted := some sc.dir, workdir := repoPath
    stdio := .inherit, runId }
  if result.exitCode != 0 then
    throw (.userError s!"hook {name} failed with exit code {result.exitCode}")

/--
Run `init.sh` once after the repository is first cloned.
Completion is recorded in the checkout's `.orchestra/.initialized` (or `.agent/`); subsequent calls
are no-ops. Does nothing if the repository has no script directory.

"Once" means once per *environment*, which is why the marker is not the whole answer. It lives in
the checkout, and the checkout is exactly the thing an execution backend carries into a container
that has nothing installed — so a session that starts fresh every task (`freshEnvironment`) runs the
hook every task, marker or no marker. A repository's `init.sh` should be idempotent, and the ones
that install a toolchain or warm a cache already are: they check before they fetch.
-/
def runInitIfNeeded (session : Exec.Session) (repoPath : System.FilePath)
    (scripts : Option Scripts) (runId : Option String := none) : IO Unit := do
  let some sc := scripts | return
  let markerDir := repoPath / sc.name
  let markerPath := markerDir / ".initialized"
  if (← markerPath.pathExists) && !session.freshEnvironment then return
  runHook session repoPath scripts "init.sh" runId
  IO.FS.createDirAll markerDir
  IO.FS.writeFile markerPath ""

/-- Returns `true` if the repository has a `validation.sh`. -/
def hasValidationScript (scripts : Option Scripts) : IO Bool := do
  let some sc := scripts | return false
  return (← (sc.dir / "validation.sh").pathExists)

/--
Run `validation.sh`.
Returns `(true, "")` if the script passes (exit 0) or does not exist.
Returns `(false, output)` if the script exits non-zero, where `output` is the
combined stdout and stderr of the script. Does not throw.
-/
def runValidation (session : Exec.Session) (repoPath : System.FilePath)
    (scripts : Option Scripts) (runId : Option String := none) : IO (Bool × String) := do
  let some sc := scripts | return (true, "")
  if !(← hasValidationScript scripts) then return (true, "")
  let result ← session.runScript {
    path := (repoPath / sc.name / "validation.sh").toString, trusted := some sc.dir
    workdir := repoPath, stdio := .piped, runId }
  return (result.exitCode == 0, result.output)

end Orchestra.RepoConfig
