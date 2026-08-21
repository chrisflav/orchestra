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

private def runCapture (cmd : String) (args : Array String)
    (cwd : Option System.FilePath := none) : IO (Bool × String × String) := do
  let child ← IO.Process.spawn {
    cmd, args, cwd, stdout := .piped, stderr := .piped
  }
  let out ← child.stdout.readToEnd
  let err ← child.stderr.readToEnd
  let code ← child.wait
  return (code == 0, out, err)

/-- Hook directory names, newest first. `.agent/` is the legacy spelling. -/
private def hookDirNames : List String := [".orchestra", ".agent"]

/-- Git's mode for a symlink entry. -/
private def symlinkMode : String := "120000"

/--
The hook scripts and per-repository config of one task, read out of git's object store rather than
out of any directory an agent can write.

Hooks are `bash` scripts from the repository, and they run **outside the landrun sandbox** with the
daemon's own environment and credentials — that is what they are for: `init.sh` installs a
toolchain, `validation.sh` runs the repository's test suite. What they must not be is a channel
whose contents an agent chooses, and every obvious way of finding them turned out to be one:

* **The working tree.** The agent holds `--rwx` on its clone (`Sandbox.launchAgent`) and
  `.orchestra/` is inside it, so reading `validation.sh` at the point it runs read a file the agent
  had just been given the chance to write.
* **The working tree, next task.** `Repo.resetSlot` runs `git clean -fd` *without* `-x`,
  deliberately, so git-ignored files survive a slot reset: an agent that appends `.orchestra/` to
  `.git/info/exclude` and drops a script there has it survive into the *next* task, where a capture
  taken from the tree looks clean by construction.
* **`git archive`.** Archiving applies the repository's own smudge filters, and `.git/config` and
  `.git/info/attributes` are as writable as the rest of the clone and are touched by no reset. A
  `filter.<n>.smudge` plus one line of attributes both substitutes the archived script *and* runs a
  command as the daemon at capture time; `export-ignore` deletes a hook from the archive outright,
  which — since a repository with no `validation.sh` validates as passing — turns a failing test
  suite into a green one.

So the hooks are read with `git ls-tree` and `git cat-file blob`, which return the committed bytes
with no filter, no attribute processing and no working tree involved, and which report the entry's
mode so a symlink is refused structurally rather than deleted afterwards.

And they are read from `ref`, the task's *base* ref, not `HEAD`. On a continuation task
`Repo.ensureSlot` deliberately skips `resetSlot`, so `HEAD` is the tip the previous agent
committed — committing is what a task that opens a pull request does, so `HEAD` is agent-controlled
through an entirely ordinary path.

The scripts still execute *in* the working tree (`cwd := repoPath`) — validation has to see the
agent's work to be worth running.

**What this does not do.** It fixes *which script* runs, not what that script does when it runs. A
real `validation.sh` invokes the repository's build and test suite over the agent-modified tree, so
agent-written code still executes outside the sandbox by design; a hook that sources a sibling by a
cwd-relative path (`source .orchestra/lib.sh` rather than `"$(dirname "$0")/lib.sh"`) reads the live
tree and reopens the hole verbatim; and `.git/hooks/post-checkout` still fires on the daemon's next
`git checkout -B` regardless of anything here. Closing the boundary means running hooks in a sandbox
of their own, which this does not do. This narrows what an agent can choose; it does not make an
untrusted repository safe to run hooks for.
-/
structure Hooks where
  private mk ::
  /-- The private copy hooks are read from. `none` when the base ref carries no hook directory,
      which is the common case and makes every operation below a no-op. -/
  private dir : Option System.FilePath
  /-- The working tree hooks run *in*, which is the live checkout rather than the copy. -/
  private repoPath : System.FilePath
  /-- Which of `hookDirNames` was captured, so the `.initialized` marker lands beside the hooks
      that were actually used rather than beside a directory an agent created. -/
  private dirName : String := ".orchestra"
  /-- Whether a `validation.sh` was present when the capture was taken.

      Held so that a capture which later goes missing — swept, or removed by anything else sharing
      the data root — fails the task instead of reporting that validation passed. "No validation
      script" and "validation succeeded" are the same answer from `runValidation`, so a capture
      that quietly disappears would turn a failing test suite green. -/
  private hadValidation : Bool := false

/-- Sandbox grants that are constants of the build rather than of a deployment's config: `/tmp`,
    which `Sandbox.sandboxArgs` grants `--rw` unconditionally, and the home-relative paths the
    agent definitions in `Orchestra.Agents.*` ask for write or write+execute on. A capture under
    any of these is exactly as writable as the working tree.

    Kept as literals rather than imported from the agent definitions, which would be a dependency
    cycle. It is therefore a floor, not a ceiling: a deployment that hands the sandbox its own data
    root through `additional_sandbox_paths` can still defeat this, and no check here can see that
    config — but that is an operator writing a grant, not an agent taking one. -/
private def agentWritableRoots : IO (List String) := do
  let fixed := ["/tmp"]
  match ← IO.getEnv "HOME" with
  | none => return fixed
  | some h =>
    let rels : List String :=
      [".cache", ".elan", ".claude", ".config/claude", ".config/gh", ".config/git"]
    return fixed ++ rels.map (fun rel => (System.FilePath.mk h / System.FilePath.mk rel).toString)

/-- Checked against a *resolved* path, so `//tmp/x`, `/tmp/../tmp/x` or a symlinked parent cannot
    spell their way around it. -/
private def isAgentWritable (resolved : System.FilePath) : IO Bool := do
  let s := resolved.toString
  return (← agentWritableRoots).any fun root => s == root || s.startsWith (root ++ "/")

/-- How long a capture left behind by a run that threw is kept before a later capture reclaims it.
    Well past any real task, so this does not race a live one. -/
private def staleCaptureSecs : Nat := 24 * 60 * 60

/-- Remove captures abandoned by earlier runs.

    `Hooks.run` throws on a non-zero exit, so a failing `before.sh` — or anything else thrown
    between capture and release — skips `release`, and the key is a task id that never recurs.
    Sweeping on the way in bounds that without making every caller carry a `finally`.

    "Now" is taken from the mtime of the directory `capture` has just created, so both sides of the
    comparison come from the filesystem's clock. Reading it from `IO.monoMsNow` instead compares a
    monotonic counter with an arbitrary epoch against seconds since 1970: on `Nat` the subtraction
    saturates at zero and the sweep silently never fires. -/
private def sweepStaleCaptures (into : System.FilePath) : IO Unit := do
  let some root := into.parent | return
  if !(← root.pathExists) then return
  let nowSecs ← try pure (← into.metadata).modified.sec catch _ => return
  for entry in ← root.readDir do
    if entry.path == into then continue
    try
      let md ← entry.path.metadata
      if md.type != .dir then continue
      if md.modified.sec + staleCaptureSecs < nowSecs then
        IO.FS.removeDirAll entry.path
    catch _ => pure ()

/-- One committed entry: its git mode and the path it should take under the capture. -/
private structure Entry where
  mode : String
  kind : String
  sha  : String
  rel  : String

/-- Parse `git ls-tree -r -z` output: `<mode> SP <type> SP <sha> TAB <path>` per NUL-separated
    record. `prefix_` is stripped from each path so the capture is rooted at the hook directory. -/
private def parseLsTree (out : String) (prefix_ : String) : Array Entry := Id.run do
  let mut es : Array Entry := #[]
  for rec in out.splitOn "\x00" do
    if rec.trimAscii.isEmpty then continue
    let parts := rec.splitOn "\t"
    let some hdr := parts.head? | continue
    let path := String.intercalate "\t" parts.tail!
    match hdr.trimAscii.toString.splitOn " " with
    | [mode, kind, sha] =>
      let p := if path.startsWith (prefix_ ++ "/") then (path.drop (prefix_.length + 1)).toString
               else path
      if p.isEmpty then continue
      es := es.push { mode, kind, sha, rel := p }
    | _ => continue
  return es

/--
Read the hook directory committed at `ref` into `into`, and answer the handle every later hook call
goes through.

`into` must be somewhere the agent sandbox cannot write; such a destination is refused outright
rather than silently producing a copy with no security value. The daemon's data root is the
intended home — of the paths the sandbox grants, the only one that could ever contain it is
`~/.local` on a non-XDG install, and that is granted read+execute, not write.

Fails loudly rather than quietly. A git invocation that errors throws, because `dir := none` means
"this repository ships no hooks", which in turn means `runValidation` reports success — so a
repository that had a `validation.sh` yesterday and cannot produce one today must fail its task,
not pass it.
-/
def capture (repoPath : System.FilePath) (into : System.FilePath) (ref : String) : IO Hooks := do
  let resolved ← try IO.FS.realPath into catch _ => pure into
  if ← isAgentWritable resolved then
    throw (.userError s!"refusing to capture repository hooks into {into}: the agent sandbox is \
granted write access there, so the copy would be no safer than the working tree")
  if ← into.pathExists then IO.FS.removeDirAll into
  IO.FS.createDirAll into
  let _ ← runCapture "chmod" #["700", into.toString]
  sweepStaleCaptures into
  for name in hookDirNames do
    -- `core.fsmonitor` is the one repository-config setting that would run a command here; the
    -- rest cannot change what a tree lookup returns. `ls-tree`/`cat-file` apply no smudge filter
    -- and honour no `export-ignore`, which is the whole reason they are used instead of `archive`.
    let (ok, out, err) ← runCapture "git"
      #["-c", "core.fsmonitor=", "ls-tree", "-r", "-z", ref, "--", name] (some repoPath)
    if !ok then
      IO.FS.removeDirAll into
      throw (.userError s!"could not read {name} from {ref}: {err.trimAscii}")
    let entries := parseLsTree out name
    if entries.isEmpty then continue
    for e in entries do
      if e.mode == symlinkMode then
        IO.eprintln s!"  [hooks] ignoring {name}/{e.rel}: a hook that is a symlink points wherever \
the repository said, and the places it could point are writable by the sandboxed agent."
        continue
      if e.kind != "blob" then continue
      let (bok, blob, berr) ← runCapture "git"
        #["-c", "core.fsmonitor=", "cat-file", "blob", e.sha] (some repoPath)
      if !bok then
        IO.FS.removeDirAll into
        throw (.userError s!"could not read {name}/{e.rel} from {ref}: {berr.trimAscii}")
      let dest := into / e.rel
      if let some parent := dest.parent then IO.FS.createDirAll parent
      IO.FS.writeFile dest blob
    return { dir := some into, repoPath, dirName := name
           , hadValidation := ← (into / "validation.sh").pathExists }
  -- Nothing committed under any hook directory name: the common case, and not an error. The
  -- directory goes with it so a hook-less repository does not leave one behind per task.
  try IO.FS.removeDirAll into catch _ => pure ()
  return { dir := none, repoPath }

/-- Drop the captured copy. Best-effort: a task whose scripts outlive it by a few minutes is a
    tidiness problem, and failing a finished task over one would be worse. -/
def Hooks.release (h : Hooks) : IO Unit := do
  if let some d := h.dir then
    try IO.FS.removeDirAll d catch _ => pure ()

/-- The path of hook `name` within the captured copy, if the repository committed one.

    `lstat`-equivalent by construction: `capture` has already deleted every symlink under the
    capture, so a path that exists here is a regular file or a directory. -/
private def Hooks.script? (h : Hooks) (name : String) : IO (Option System.FilePath) := do
  let some d := h.dir | return none
  let p := d / name
  if ← p.pathExists then return some p else return none

/-- Load `config.json` from the captured copy. Returns defaults if absent or unparseable.

    Captured rather than read live for the same reason as the scripts, and with a sharper edge:
    `validation.retry_prompt` is interpolated into the *next prompt the agent is given*, so a
    live read would let an agent that failed validation write its own retry instructions. -/
def Hooks.config (h : Hooks) : IO RepoConfig := do
  let some configPath ← h.script? "config.json" | return {}
  let contents ← IO.FS.readFile configPath
  match Json.parse contents with
  | .error _ => return {}
  | .ok j =>
    match FromJson.fromJson? j with
    | .error _ => return {}
    | .ok cfg => return cfg

/--
Run captured hook `name` as a bash script, with the repository as the working directory.
Does nothing if the repository shipped no such hook. Throws if the script exits non-zero.
-/
def Hooks.run (h : Hooks) (name : String) : IO Unit := do
  let some hookPath ← h.script? name | return
  let child ← IO.Process.spawn {
    cmd  := "bash"
    args := #[hookPath.toString]
    cwd  := h.repoPath
    stdout := .inherit
    stderr := .inherit
  }
  let code ← child.wait
  if code != 0 then
    throw (.userError s!"hook {name} failed with exit code {code}")

/--
Run the captured `init.sh` once after the repository is first cloned.
Completion is recorded as `.initialized` in the repository's own hook directory; subsequent calls
are no-ops. Does nothing if the repository ships no hook directory.

The marker stays in the working tree rather than in the capture, because what it records is a fact
about *this clone* — that its toolchain is installed — and it has to outlive a single task's copy.
An agent can reach it, and the worst it can do is make `init.sh` run again or not at all; the
script that would run is still the captured one.
-/
def Hooks.runInitIfNeeded (h : Hooks) : IO Unit := do
  if h.dir.isNone then return
  let markerPath := h.repoPath / h.dirName / ".initialized"
  if ← markerPath.pathExists then return
  h.run "init.sh"
  IO.FS.createDirAll (h.repoPath / h.dirName)
  IO.FS.writeFile markerPath ""

/-- Whether the repository shipped a `validation.sh`. -/
def Hooks.hasValidation (h : Hooks) : IO Bool :=
  return (← h.script? "validation.sh").isSome

/--
Run the captured `validation.sh`.
Returns `(true, "")` if the script passes (exit 0) or does not exist.
Returns `(false, output)` if the script exits non-zero, where `output` is the
combined stdout and stderr of the script. Does not throw.
-/
def Hooks.runValidation (h : Hooks) : IO (Bool × String) := do
  let some hookPath ← h.script? "validation.sh"
    | if h.hadValidation then
        throw (.userError "the captured validation.sh has gone missing since it was captured; \
refusing to report validation as passed")
      else return (true, "")
  let child ← IO.Process.spawn {
    cmd  := "bash"
    args := #[hookPath.toString]
    cwd  := h.repoPath
    stdout := .piped
    stderr := .piped
  }
  let stdout ← child.stdout.readToEnd
  let stderr ← child.stderr.readToEnd
  let code ← child.wait
  let combined := (stdout ++ stderr).trimAscii.toString
  return (code == 0, combined)

end Orchestra.RepoConfig
