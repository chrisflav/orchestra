import Orchestra.Config
import Std.Data.HashMap
import Std.Sync

namespace Orchestra.Repo

/-- Environment carrying a GitHub App installation token for one `git`/`gh` invocation.

    Per-process rather than a global `gh auth login`: the queue daemon runs several tasks
    concurrently, each holding its own installation token, and a token written to
    `~/.config/gh/hosts.yml` is shared by all of them — whichever task authenticated last
    would silently supply the credentials for every other task's clone, fetch and push. -/
private def tokenEnv (token : Option String) : Array (String × Option String) :=
  match token with
  | none   => #[]
  | some t => #[("GH_TOKEN", some t), ("GITHUB_TOKEN", some t)]

/-- The credential helper used for HTTPS GitHub remotes.

    Prefers `GH_TOKEN` from the environment of the invocation it runs under, so nothing about
    it is process-global and concurrent tasks cannot see each other's credentials. Falls back
    to `gh auth git-credential` — what `gh auth setup-git` would have installed globally — so
    that interactive CLI commands, which have no installation token of their own, keep working
    off the user's own `gh` login. -/
private def credentialHelperScript : String :=
  "!f() { if [ -n \"$GH_TOKEN\" ]; then echo username=x-access-token; \
echo \"password=$GH_TOKEN\"; else gh auth git-credential \"$@\"; fi; }; f"

/-- Config key the helper is installed under.

    Deliberately scoped to `https://github.com` rather than the bare `credential.helper`. The
    helper hands `$GH_TOKEN` to whoever invokes it, and the slot it is installed in is mounted
    read-write into the agent's sandbox — so under the bare key a `git push
    https://elsewhere.example/x`, whether a typo or a URL an agent was talked into, would ship
    the installation token to that host. Git matches this key against the remote's URL and
    simply does not invoke the helper for anything else, which is a stronger guarantee than
    parsing the `host=` line inside the script. -/
private def credentialConfigKey : String :=
  "credential.https://github.com.helper"

/-- `-c` overrides installing `credentialHelperScript` for a single `git` invocation.
    The empty values clear any helper inherited from `~/.gitconfig` — under either the bare or
    the scoped key — so a stale global helper left by an earlier `gh auth setup-git` cannot
    take precedence and feed git a different task's token. -/
private def credentialArgs (token : Option String) : Array String :=
  if token.isNone then #[]
  else #["-c", "credential.helper=",
         "-c", s!"{credentialConfigKey}=",
         "-c", s!"{credentialConfigKey}={credentialHelperScript}"]

private def runGit (args : Array String) (cwd : Option System.FilePath := none)
    (token : Option String := none) (extraEnv : Array (String × Option String) := #[]) :
    IO String := do
  let child ← IO.Process.spawn {
    cmd := "git"
    args := credentialArgs token ++ args
    cwd
    env := tokenEnv token ++ extraEnv
    stdout := .piped
    stderr := .piped
  }
  -- stderr is drained on its own task while stdout is read here. Reading one to the end before
  -- the other deadlocks as soon as git writes more than a pipe buffer to the second: a fetch that
  -- reports every branch of a busy repository blocks on stderr while this waits on stdout.
  let stderrTask ← IO.asTask (prio := .dedicated) child.stderr.readToEnd
  let stdout ← child.stdout.readToEnd
  let stderr ← match ← IO.wait stderrTask with
    | .ok s => pure s
    | .error e => throw e
  let code ← child.wait
  if code != 0 then
    throw (.userError s!"git {args[0]!} failed (exit {code}):\n{stderr}")
  return stdout.trimAscii.toString

private def runGit' (args : Array String) (cwd : Option System.FilePath := none)
    (token : Option String := none) : IO Unit := do
  let _ ← runGit args cwd token

private def runGh (args : Array String) (cwd : Option System.FilePath := none)
    (token : Option String := none) : IO String := do
  let child ← IO.Process.spawn {
    cmd := "gh"
    args
    cwd
    env := tokenEnv token
    stdout := .piped
    stderr := .piped
  }
  -- As in `runGit`: both pipes drained at once, or a chatty command deadlocks.
  let stderrTask ← IO.asTask (prio := .dedicated) child.stderr.readToEnd
  let stdout ← child.stdout.readToEnd
  let stderr ← match ← IO.wait stderrTask with
    | .ok s => pure s
    | .error e => throw e
  let code ← child.wait
  if code != 0 then
    throw (.userError s!"gh {args[0]!} failed (exit {code}):\n{stderr}")
  return stdout.trimAscii.toString

private def runGh' (args : Array String) (cwd : Option System.FilePath := none)
    (token : Option String := none) : IO Unit := do
  let _ ← runGh args cwd token

/-- Base directory for all agent work. -/
def workDir : IO System.FilePath :=
  return (← Dirs.dataBase) / "repos"

/-- Build a plain GitHub HTTPS URL (no credentials). -/
private def githubUrl (repo : Repository) : String :=
  s!"https://github.com/{repo}.git"

/-- Install `credentialHelperScript` in a repository's *local* config.

    Repository-local rather than global, for two reasons. `gh auth setup-git` writes
    `~/.gitconfig`, which every concurrent task shares. And the sandboxed agent needs a
    helper too — it pushes from inside the sandbox with the `GH_TOKEN` that `Sandbox` injects
    for its own task, and the repository is mounted while `~/.gitconfig` may not be.

    Installed under `credentialConfigKey`, so it only ever fires for github.com remotes. -/
private def configureCredentialHelper (repoPath : System.FilePath) : IO Unit := do
  -- Drop a bare `credential.helper` a previous version of orchestra installed here: it is
  -- unscoped, so leaving it behind would keep offering the token to any host.
  try runGit' #["config", "--unset-all", "credential.helper"] repoPath catch _ => pure ()
  try runGit' #["config", credentialConfigKey, credentialHelperScript] repoPath
  catch e =>
    IO.eprintln s!"  Warning: could not configure the credential helper in {repoPath}: {e}"

/-- Hide orchestra's own bookkeeping files from the repository's git status.

    `RepoConfig.runInitIfNeeded` records that the init hook has run by writing a marker inside
    the working tree. Without this the marker shows up as an untracked file in every `git
    status` the agent runs, in a tree that is otherwise pristine — an invitation to `git add
    .` it into a pull request. Writing `.git/info/exclude` rather than `.gitignore` keeps the
    exclusion out of the repository's own history. -/
private def excludeOrchestraArtifacts (repoPath : System.FilePath) : IO Unit := do
  try
    let infoDir := repoPath / ".git" / "info"
    IO.FS.createDirAll infoDir
    let excludePath := infoDir / "exclude"
    let existing ← try IO.FS.readFile excludePath catch _ => pure ""
    -- Specific enough that a repository mentioning orchestra in its own exclude file for
    -- some other reason does not read as "already done" and suppress the write.
    let marker := "# orchestra: init-hook markers"
    -- `splitOn` yields one piece exactly when the marker is absent; appending is idempotent.
    if (existing.splitOn marker).length == 1 then
      IO.FS.writeFile excludePath
        (existing ++ s!"\n{marker}\n.orchestra/.initialized\n.agent/.initialized\n")
  catch e =>
    IO.eprintln s!"  Warning: could not update .git/info/exclude in {repoPath}: {e}"

/--
Ensure the fork is cloned and the upstream remote is configured.
Returns the path to the local repository.
When `interactive` is false (e.g. queue mode), user prompts are suppressed and
an error is thrown instead.
Cloning is performed via `gh repo clone`; `token`, when given, authenticates it and every
subsequent network operation through this invocation's environment only. Passing `none`
falls back to whatever ambient `gh` credentials exist, which is only appropriate for
single-task CLI commands.
-/
def ensureCloned (fork upstream : Repository) (interactive : Bool := true)
    (token : Option String := none) : IO System.FilePath := do
  let base ← workDir
  let repoPath := base / fork.owner / fork.name
  if ← repoPath.pathExists then
    let entries ← repoPath.readDir
    if entries.isEmpty then
      -- Empty directory left over from a failed clone: remove and retry
      IO.println s!"  Directory '{repoPath}' is empty; removing and re-cloning..."
      IO.FS.removeDirAll repoPath
      IO.FS.createDirAll repoPath
      runGh' #["repo", "clone", fork.toString, repoPath.toString] (token := token)
      let remotes₁ ← runGit #["remote"] repoPath
      if !(remotes₁.splitOn "\n" |>.any (· == "upstream")) then
        runGit' #["remote", "add", "upstream", githubUrl upstream] repoPath
    else
      -- Non-empty: verify it's a valid git repo
      let isGitRepo : Bool ← do
        try
          let _ ← runGit #["rev-parse", "--git-dir"] repoPath
          pure true
        catch _ =>
          pure false
      if !isGitRepo then
        if !interactive then
          throw (IO.userError s!"Directory '{repoPath}' exists but is not a valid git repository. Remove it manually and try again.")
        IO.eprint s!"Directory '{repoPath}' exists but is not a valid git repository.\nDelete it and re-clone? [y/N] "
        let stdin ← IO.getStdin
        let answer := (← stdin.getLine).trimAscii.toString.toLower
        if answer == "y" || answer == "yes" then
          IO.FS.removeDirAll repoPath
          IO.FS.createDirAll repoPath
          runGh' #["repo", "clone", fork.toString, repoPath.toString] (token := token)
          let remotes₂ ← runGit #["remote"] repoPath
          if !(remotes₂.splitOn "\n" |>.any (· == "upstream")) then
            runGit' #["remote", "add", "upstream", githubUrl upstream] repoPath
        else
          throw (IO.userError s!"Directory '{repoPath}' is not a valid git repository. Remove it manually and try again.")
      else
        -- Make sure upstream remote exists
        let remotes ← runGit #["remote"] repoPath
        let hasUpstream := remotes.splitOn "\n" |>.any (· == "upstream")
        if !hasUpstream then
          runGit' #["remote", "add", "upstream", githubUrl upstream] repoPath
        -- Make sure origin points at the fork HTTPS URL
        let originUrl ← runGit #["remote", "get-url", "origin"] repoPath
        let expectedOriginUrl := githubUrl fork
        if originUrl != expectedOriginUrl then
          IO.println s!"  Fixing origin URL: {originUrl} → {expectedOriginUrl}"
          runGit' #["remote", "set-url", "origin", expectedOriginUrl] repoPath
  else
    IO.FS.createDirAll repoPath
    runGh' #["repo", "clone", fork.toString, repoPath.toString] (token := token)
    let remotes₃ ← runGit #["remote"] repoPath
    if !(remotes₃.splitOn "\n" |>.any (· == "upstream")) then
      runGit' #["remote", "add", "upstream", githubUrl upstream] repoPath
  -- Wire credentials into this clone rather than into `~/.gitconfig`: see
  -- `configureCredentialHelper`. Replaces the `gh auth setup-git` this used to run, which
  -- mutated global state shared by every concurrently running task.
  configureCredentialHelper repoPath
  -- Fetch upstream to keep remote tracking branches up to date
  IO.println "  Fetching upstream..."
  runGit' #["fetch", "upstream"] repoPath token
  return repoPath

/-- Path for the main clone of a fork repository. -/
def clonePath (fork : Repository) : IO System.FilePath := do
  return (← workDir) / fork.owner / fork.name

/-- Path of task slot `slot` for a fork repository. -/
def slotPath (fork : Repository) (slot : Nat) : IO System.FilePath := do
  return (← workDir) / fork.owner / s!"{fork.name}-slot-{slot}"

/-- Return true if `path` is a git repository with a real `.git` directory. -/
private def isGitRepo (path : System.FilePath) : IO Bool :=
  (path / ".git" / "config").pathExists

/-- File recording which task's working tree currently sits in a slot.

    Kept under `.git/` rather than in the working tree so that `resetSlot`'s `git clean` does
    not remove it and the agent never sees it in `git status`. -/
private def slotOccupantPath (slot : System.FilePath) : System.FilePath :=
  slot / ".git" / "orchestra-occupant"

/-- Read an occupant marker.

    `none` covers a slot that does not exist yet, one created before this bookkeeping existed,
    and one whose marker could not be read — all of which mean the same thing to a caller: do
    not assume the tree is anybody's in particular. -/
private def readOccupant (path : System.FilePath) : IO (Option String) := do
  try
    let s := (← IO.FS.readFile path).trimAscii.toString
    return if s.isEmpty then none else some s
  catch _ => return none

/-- The task whose working tree currently sits in slot `slot` of `fork`, if it is known. -/
def slotOccupant (fork : Repository) (slot : Nat) : IO (Option String) := do
  readOccupant (slotOccupantPath (← slotPath fork slot))

/-- Record `occupant` as the owner of the tree now sitting in `slot`. -/
private def setSlotOccupant (slot : System.FilePath) (occupant : Option String) : IO Unit := do
  let path := slotOccupantPath slot
  try
    match occupant with
    | some o => IO.FS.writeFile path o
    | none   => IO.FS.removeFile path
  catch e =>
    -- Losing the marker is not fatal on its own: a slot with no recorded occupant reads as
    -- "provenance unknown", which makes a later continuation reset rather than resume — the
    -- safe direction.
    if occupant.isSome then
      IO.eprintln s!"  Warning: could not record the slot occupant in {slot}: {e}"

/-- Which slot a task runs in, and what should become of the tree already sitting there. -/
structure SlotAssignment where
  /-- Index of the per-repo clone slot. -/
  slot : Nat
  /-- Identifier stamped into the slot once it is prepared, naming this task as the owner of
      the working tree it leaves behind. A later continuation checks it before concluding the
      tree is still the one its predecessor left. -/
  occupant : Option String := none
  /-- When set, keep the working tree instead of resetting it — but only if the slot's
      recorded occupant is still this identifier. Slots are reused across tasks, so a
      predecessor's slot being *free* is not evidence that its tree is still there: an
      unrelated task may have taken the slot and reset it in between. Resuming onto that
      tree would hand the agent someone else's branch and working state while its context
      describes edits that are gone. -/
  resumeFrom : Option String := none
  deriving Inhabited

/-- Serialises first-time creation of the shared cache clone: two tasks starting at once
    on a never-cloned repository would otherwise both run `gh repo clone` into the same
    directory. Held only around the existence check and the clone itself, so a repository
    that is already cloned never blocks. -/
private initialize cloneMutex : Std.BaseMutex ← Std.BaseMutex.new

/-- Ensure the shared cache clone for `fork` exists, and return its path.

    Unlike `ensureCloned` this does not fetch when the clone is already present. The queue
    never works in the cache clone — it exists only as an object source for `ensureSlot`'s
    `git clone --local` — and every slot fetches `upstream` itself, so refreshing here
    would only add ref-lock contention between parallel tasks in order to retrieve objects
    the slot's own fetch retrieves anyway. -/
private def ensureCacheClone (fork upstream : Repository) (token : Option String)
    : IO System.FilePath := do
  let mainPath ← clonePath fork
  if ← isGitRepo mainPath then return mainPath
  cloneMutex.lock
  try
    -- Re-check under the lock: another worker may have cloned it while we waited.
    if ← isGitRepo mainPath then return mainPath
    ensureCloned fork upstream (interactive := false) (token := token)
  finally
    cloneMutex.unlock

/-- Return true if `ref` resolves to an object in `repoPath`. -/
private def refExists (repoPath : System.FilePath) (ref : String) : IO Bool := do
  try
    runGit' #["rev-parse", "--verify", "--quiet", ref] repoPath
    return true
  catch _ => return false

/-- Name of the default branch (e.g. `master`) recorded in `refs/remotes/<remote>/HEAD`.
    `git clone` records this at clone time, so for `origin` it is available offline. A
    remote added afterwards has no HEAD until `git remote set-head` asks the server, so
    try that once before giving up. -/
private def defaultBranchName (repoPath : System.FilePath) (remote : String) : IO (Option String) := do
  let readHead : IO (Option String) := do
    try
      -- Yields e.g. `origin/master`; keep the branch part.
      let out ← runGit #["symbolic-ref", "--short", s!"refs/remotes/{remote}/HEAD"] repoPath
      let prefix_ := s!"{remote}/"
      return some (if out.startsWith prefix_ then (out.drop prefix_.length).toString else out)
    catch _ => return none
  if let some name ← readHead then return some name
  try runGit' #["remote", "set-head", remote, "--auto"] repoPath catch _ => pure ()
  readHead

/-- The default branch of a slot, as a `(branchName, startPointRef)` pair.

    The branch *name* comes from `origin` (recorded at clone time, so no network call).
    The commit is taken from `upstream` when available: a slot fetches upstream on every
    reset and never fetches origin, so the fork's remote-tracking refs are stale as of
    clone time and would silently base new work on an old commit. -/
private def slotBaseRef (repoPath : System.FilePath) : IO (Option (String × String)) := do
  let some branch ← defaultBranchName repoPath "origin" | return none
  for remote in ["upstream", "origin"] do
    let ref := s!"{remote}/{branch}"
    if ← refExists repoPath ref then return some (branch, ref)
  return none

/-- Bring a task slot back to a clean state on an up-to-date default branch.

    Deliberately `git clean -fd` *without* `-x`: reusing slots rather than recreating them
    exists precisely so that gitignored build output (`.lake/`, `target/`, `node_modules/`)
    survives between tasks. The previous task's tracked modifications and untracked scratch
    files do not. -/
private def resetSlot (slotPath : System.FilePath) (token : Option String) : IO Unit := do
  -- Discard the previous task's work before switching branches: a leftover untracked file
  -- that also exists on the target branch would otherwise abort the checkout.
  try runGit' #["reset", "--hard"] slotPath catch _ => pure ()
  -- `-e` keeps the init-hook marker: `RepoConfig.runInitIfNeeded` records completion inside
  -- the tree, and wiping it would re-run an expensive one-time setup hook on every task.
  try runGit' #["clean", "-fd", "-e", ".orchestra/.initialized", "-e", ".agent/.initialized"]
        slotPath
  catch _ => pure ()
  -- A failed fetch is fatal rather than a warning: everything below bases the task on
  -- `upstream/<default>`, so continuing would silently start work from whatever commit the
  -- slot last saw — for a task that will open a pull request, branching off a week-old base
  -- is worse than not running at all.
  runGit' #["fetch", "upstream"] slotPath token
  match ← slotBaseRef slotPath with
  | some (branch, ref) =>
    -- `-B` rather than `--detach`: a slot is an independent clone with its own ref
    -- namespace, so the real branch can be checked out without colliding with any other
    -- slot, and the agent sees an ordinary repository on the default branch.
    runGit' #["checkout", "-B", branch, ref] slotPath
  | none =>
    throw (.userError s!"could not determine the default branch in {slotPath}; refusing to \
      start a task on an unknown base commit")

/-- Point a clone made from the cache at the fork and the upstream, with the credential helper and
    the excludes every checkout orchestra prepares carries. Shared by the pooled slots and by the
    per-task checkouts of a backend that keeps workspaces itself. -/
private def configureClone (fork upstream : Repository) (path : System.FilePath) : IO Unit := do
  runGit' #["remote", "set-url", "origin", githubUrl fork] path
  let remotes ← runGit #["remote"] path
  if remotes.splitOn "\n" |>.any (· == "upstream") then
    runGit' #["remote", "set-url", "upstream", githubUrl upstream] path
  else
    runGit' #["remote", "add", "upstream", githubUrl upstream] path
  configureCredentialHelper path
  excludeOrchestraArtifacts path

/-- Ensure task slot `slot` of `fork` exists and is clean, and return its path.

    A slot is an independent clone with its own `.git`, and therefore its own ref
    namespace: two concurrent tasks on one repository can create identically named
    branches without git refusing, which is what a shared worktree layout cannot offer.
    `git clone --local` hardlinks the object store from the cache clone, so a slot costs a
    working tree rather than a second copy of the history.

    Slots are reused across tasks rather than created per task so that gitignored build
    output survives; see `resetSlot`.

    `assign.resumeFrom` keeps the working tree exactly as the previous task left it. The queue
    passes this for a `continuesFrom` entry, whose agent resumes a conversation full of
    references to files it had just edited and would otherwise wake up to a wiped tree. The
    tree is only kept when the slot's recorded occupant still matches; see `SlotAssignment`. -/
def ensureSlot (fork upstream : Repository) (assign : SlotAssignment)
    (token : Option String := none) : IO System.FilePath := do
  let slot := assign.slot
  let mainPath ← ensureCacheClone fork upstream token
  let sPath ← slotPath fork slot
  if ← isGitRepo sPath then
    IO.println s!"  Reusing slot {slot} at {sPath}"
  else
    -- A directory that is not a repository is debris from an interrupted clone; it cannot
    -- be reused and `git clone` refuses to write into it.
    if ← sPath.pathExists then
      IO.println s!"  Removing incomplete slot directory {sPath}..."
      IO.FS.removeDirAll sPath
    IO.println s!"  Creating slot {slot} at {sPath}..."
    -- `--local` hardlinks objects instead of copying them, so a slot costs a working tree
    -- rather than a second copy of the history. Safe with respect to garbage collection,
    -- because git objects are immutable and a later `gc` in either clone only ever drops its
    -- own link; `--shared`, which points at the cache through alternates, would not be.
    --
    -- Note this is a space optimisation, not an isolation boundary: the hardlinked object
    -- files are the same inodes as the cache clone's, and the sandbox grants the slot `--rwx`
    -- by path, so an agent that deliberately wrote into `.git/objects` would be writing into
    -- storage the other slots share. Git itself never rewrites an object in place, so this
    -- does not happen by accident. What the slot layout does isolate — and what the worktree
    -- layout could not — is refs, config, hooks and `HEAD`, which are per-slot.
    runGit' #["clone", "--local", mainPath.toString, sPath.toString]
  -- Remote repair runs for reused slots too, not just freshly created ones. A slot whose
  -- `remote add upstream` failed after the clone succeeded, or one left by an older layout,
  -- would otherwise never grow the remote and fail every fetch from then on.
  configureClone fork upstream sPath
  -- A continuation only inherits the tree if the slot still holds *its predecessor's* tree.
  -- Slots are pooled and reset between tasks, so "the slot is free" is not the same claim.
  let keepTree ← match assign.resumeFrom with
    | none      => pure false
    | some pred =>
      match ← slotOccupant fork slot with
      | some cur =>
        if cur == pred then pure true
        else do
          IO.eprintln s!"  Warning: slot {slot} was last used by {cur}, not {pred}; \
resetting it. The resumed agent will not find the working tree its session refers to."
          pure false
      | none => do
        IO.eprintln s!"  Warning: slot {slot} has no recorded occupant, so the tree {pred} \
left cannot be confirmed; resetting it. The resumed agent will not find the working tree \
its session refers to."
        pure false
  if keepTree then
    let pred := assign.resumeFrom.getD "?"
    IO.println s!"  Keeping slot {slot} as-is (continuation of {pred})"
  else
    resetSlot sPath token
  -- Stamped after preparation, so a task that dies during `resetSlot` does not leave the slot
  -- claiming to hold a tree it never produced.
  setSlotOccupant sPath assign.occupant
  return sPath


/-- Where a single task's checkout is prepared, for a backend that keeps the workspace itself
    (`Exec.Backend.persistentWorkspaces`). One directory per task rather than a pooled slot: the
    tree is only what the task's own workspace is filled from, and is removed when the task ends. -/
def taskCheckoutPath (fork : Repository) (taskId : String) : IO System.FilePath := do
  return (← workDir) / fork.owner / s!"{fork.name}-tasks" / taskId

/-- Where build output carried from one task chain to the next is kept for a repository, for the
    same backends (`TaskVolumes.seedPaths` in the kubernetes backend). -/
def seedPath (fork : Repository) : IO System.FilePath := do
  return (← workDir) / fork.owner / s!"{fork.name}-seed"

/-- A fresh checkout of `fork` for task `taskId`, on an up-to-date default branch: what `ensureSlot`
    produces for a reset slot, without the pool. `git clone --local` from the cache clone, so it
    costs a working tree and not a second copy of the history, and is quick to make and to throw
    away. A leftover directory of the same name — a task that died before it was removed — is
    replaced. -/
def ensureTaskCheckout (fork upstream : Repository) (taskId : String)
    (token : Option String := none) : IO System.FilePath := do
  let mainPath ← ensureCacheClone fork upstream token
  let path ← taskCheckoutPath fork taskId
  if ← path.pathExists then IO.FS.removeDirAll path
  if let some parent := path.parent then IO.FS.createDirAll parent
  IO.println s!"  Preparing a checkout for {taskId} at {path}..."
  runGit' #["clone", "--local", mainPath.toString, path.toString]
  configureClone fork upstream path
  resetSlot path token
  return path

/-! ## A task's checkout at a commit (`Config.CheckoutSpec`)

A task that names a commit is never prepared in a pooled slot. Everything about it is built from a
repository only the daemon ever touches, fetched from the upstream's URL for this task alone:

1. `prepareCheckoutSource` makes that repository (`<fork>-tasks/<task>.src`), fetches the upstream's
   branches and the `fetch_ref` into it, checks that the commit is reachable from what it just
   fetched, and copies the default branch's `.orchestra/` (and `.agent/`) into
   `<fork>-tasks/<task>.config`, which is what the task's configuration and hooks are read from.
2. `buildCheckoutTree` makes the tree the task runs in (`<fork>-tasks/<task>`) from that source: an
   ordinary repository with the upstream's branches on a detached `HEAD` at the commit, or with
   `isolate`, one holding the commit and its history and nothing else (`isolateCheckout`).

Why not the slot. A slot is mounted into the agent's sandbox read-write whenever a task is not
read-only, and survives from one task to the next, so anything in it may have been written by an
earlier agent: a ref under `refs/remotes/upstream/` that makes a fork-only commit look like the
upstream's, a `refs/replace/` or an `info/grafts` that rewrites which commits are a commit's parents
— and with them what a pack built from it contains — a hook that runs on the next checkout, a
`url.*.insteadOf` that sends a fetch elsewhere. A repository made for one task and never granted to
any agent has none of those, and every `git` run against it is also told to ignore replacements,
grafts, hooks and every configuration file but its own (`hardened`).

Why the configuration comes from the default branch. A commit named by an API client or an agent
carries an `.orchestra/` of its author's choosing, and `init.sh`, `before.sh`, `validation.sh` and
`after.sh` run outside the agent's sandbox, and `execution.image` decides where it runs. The task
reviews the commit; it runs under the upstream's own configuration.

Costs: the source is a fresh fetch of the upstream's history over the network for each such task,
which for a large repository is a large pack, and the tree starts without the gitignored build
output a reused slot would have kept. -/

/-- The environment for `git` runs that nothing an agent wrote may steer: no replacement objects,
    no system or global configuration — a global `core.hooksPath` or `insteadOf` would otherwise
    reach in — and no prompt for a password. Inherited by the `upload-pack` a local fetch runs,
    which is what builds the pack an isolated checkout is filled from.

    No `GIT_GRAFT_FILE`: grafts are read from the repository's own `info/grafts`, and every
    repository these runs touch is one this module just made, which has none. -/
private def hardenedEnv : Array (String × Option String) :=
  #[("GIT_NO_REPLACE_OBJECTS", some "1"),
    ("GIT_CONFIG_NOSYSTEM", some "1"), ("GIT_CONFIG_GLOBAL", some "/dev/null"),
    ("GIT_TERMINAL_PROMPT", some "0")]

/-- `git` with `hardenedEnv`, hooks switched off, and replacement refs ignored. -/
private def hardened (args : Array String) (cwd : Option System.FilePath := none)
    (token : Option String := none) : IO String :=
  runGit (#["-c", "core.hooksPath=/dev/null", "--no-replace-objects"] ++ args) cwd token hardenedEnv

private def hardened' (args : Array String) (cwd : Option System.FilePath := none)
    (token : Option String := none) : IO Unit := do
  let _ ← hardened args cwd token

private def hardenedOk (args : Array String) (cwd : System.FilePath) : IO Bool := do
  try hardened' args cwd; pure true catch _ => pure false

/-- The daemon-only source repository of task `taskId`'s checkout. -/
def checkoutSourcePath (fork : Repository) (taskId : String) : IO System.FilePath :=
  taskCheckoutPath fork s!"{taskId}.src"

/-- Where the default branch's `.orchestra/` of task `taskId` is copied (`prepareCheckoutSource`). -/
def checkoutConfigPath (fork : Repository) (taskId : String) : IO System.FilePath :=
  taskCheckoutPath fork s!"{taskId}.config"

/-- The scratch directory an isolated checkout task gets instead of the shared `/tmp`, on a backend
    that runs the agent on this machine. -/
def checkoutTmpPath (fork : Repository) (taskId : String) : IO System.FilePath :=
  taskCheckoutPath fork s!"{taskId}.tmp"

/-- Everything a checkout task leaves: its tree, its source, its configuration copy and its
    scratch directory. -/
def removeCheckoutDirs (fork : Repository) (taskId : String) : IO Unit := do
  for p in [← taskCheckoutPath fork taskId, ← checkoutSourcePath fork taskId,
            ← checkoutConfigPath fork taskId, ← checkoutTmpPath fork taskId] do
    try if ← p.pathExists then IO.FS.removeDirAll p
    catch e => IO.eprintln s!"  Warning: could not remove {p}: {e}"

private def freshDir (p : System.FilePath) : IO Unit := do
  if ← p.pathExists then IO.FS.removeDirAll p
  IO.FS.createDirAll p

/-- Every path under `root`, recursively, without following symlinks into directories. -/
private partial def walk (root : System.FilePath) : IO (Array System.FilePath) := do
  let mut out := #[]
  for e in ← root.readDir do
    out := out.push e.path
    let md ← e.path.symlinkMetadata
    if md.type == .dir then out := out ++ (← walk e.path)
  return out

/-- Refuse a copied `.orchestra/` holding a symlink that leads outside it, or nowhere.

    The default branch's hooks are run from the copy, and a symlink in it would run, or read, a
    file the copy does not hold — the checked-out commit's own `init.sh`, a file elsewhere on this
    machine — under the name of a trusted one. A link within the copy is fine. -/
def refuseEscapingSymlinks (root : System.FilePath) : IO Unit := do
  let base ← IO.FS.realPath root
  for p in ← walk root do
    if (← p.symlinkMetadata).type == .symlink then
      let target ← try IO.FS.realPath p
        catch _ => throw (.userError s!"checkout: the default branch's {p} is a symlink to \
nothing; refusing to run hooks from a configuration that names files it does not hold")
      unless target.toString.startsWith (base.toString ++ "/") do
        throw (.userError s!"checkout: the default branch's {p} is a symlink to {target}, outside \
its .orchestra/; refusing to run hooks from a configuration that names files it does not hold")

/-- Make the daemon-only repository at `source` for a checkout of `spec`, fetched from `url`, and
    copy the upstream default branch's `.orchestra/` and `.agent/` into `config`. Throws, naming the
    commit, when it is not to be had — a task that asked for a commit and ran on another would
    review the wrong code or see the answer it is being scored against.

    The commit has to be **reachable from what this fetch wrote**: the `fetch_ref`, or without one
    one of the upstream's branches. That is what makes a commit named by an API client or an agent
    safe to check out: it can only ever name code the upstream has published. It is never fetched
    by name alone, which git would skip if the object were already local; here nothing is local to
    begin with, but the rule is the one that holds wherever the objects came from. -/
def prepareCheckoutSource (url : String) (spec : CheckoutSpec) (source config : System.FilePath)
    (token : Option String := none) : IO Unit := do
  let sha := spec.commit
  freshDir source
  hardened' #["init", "--quiet", "--bare", source.toString]
  let symref ← try hardened #["ls-remote", "--symref", url, "HEAD"] source token
    catch e => throw (.userError s!"checkout: could not reach the upstream: {e}")
  let some defaultBranch := (symref.splitOn "\n").findSome? fun line =>
      if line.startsWith "ref: refs/heads/" then
        some ((line.drop 16).toString.takeWhile (· != '\t')).toString
      else none
    | throw (.userError "checkout: the upstream names no default branch")
  let heads := "refs/orchestra/fetch/heads"
  let refspecs := #[s!"+refs/heads/*:{heads}/*"] ++
    (spec.fetchRef.map (fun r => #[s!"+{r}:refs/orchestra/fetch/ref"])).getD #[]
  let source' := spec.fetchRef.getD "the upstream's branches"
  try hardened' (#["fetch", "--quiet", "--no-tags", "--no-write-fetch-head", url] ++ refspecs) source token
  catch e => throw (.userError s!"checkout: could not fetch {source'} to find commit {sha}: {e}")
  let present ← hardenedOk #["cat-file", "-e", s!"{sha}^\{commit}"] source
  let reachable ← if !present then pure false else match spec.fetchRef with
    | some _ => hardenedOk #["merge-base", "--is-ancestor", sha, "refs/orchestra/fetch/ref"] source
    | none => do
      let refs ← hardened #["for-each-ref", "--format=%(refname)", s!"--contains={sha}",
        s!"{heads}/"] source
      pure !refs.isEmpty
  unless reachable do
    throw (.userError s!"checkout: commit {sha} is not reachable from {source'} on the upstream; \
the task asked to run on it and will not run on anything else")
  -- The upstream's own configuration, never the commit's.
  freshDir config
  let tip := s!"{heads}/{defaultBranch}"
  for dir in [".orchestra", ".agent"] do
    if ← hardenedOk #["cat-file", "-e", s!"{tip}:{dir}"] source then
      hardened' #["--git-dir", source.toString, "--work-tree", config.toString,
        "checkout", tip, "--", dir] source
  refuseEscapingSymlinks config
  IO.println s!"  Verified {sha} against {source'}; configuration from {defaultBranch}"

/-- Make `dest` a repository of its own holding the commit `commit` of `source` and nothing else:
    every object reachable from it, a detached `HEAD` on it, and no refs, no reflog, no
    `FETCH_HEAD` or `ORIG_HEAD` — so `git log --all`, `git branch -a`, `git tag` and `git reflog`
    reach nothing that came after it, and neither does `git cat-file --batch-all-objects`. What
    came *before* it is still there, through its parents: that is the history the commit has, and
    a task that sees the commit sees how it was reached.

    Filled by a fetch over the local path, which runs `upload-pack` on `source` with the hardened
    environment: a pack of what is reachable from the commit, with no replacement or graft able to
    add a parent it does not have. -/
def isolateCheckout (source dest : System.FilePath) (commit : String) : IO Unit := do
  freshDir dest
  let format ← try hardened #["rev-parse", "--show-object-format"] source catch _ => pure "sha1"
  hardened' #["init", "--quiet", s!"--object-format={format}", dest.toString]
  let tmp := "refs/orchestra/isolate"
  hardened' #["update-ref", tmp, commit] source
  try
    hardened' #["fetch", "--no-tags", "--quiet", source.toString, tmp] dest
  finally
    try hardened' #["update-ref", "-d", tmp] source catch _ => pure ()
  hardened' #["checkout", "--detach", "--quiet", commit] dest
  let refs ← hardened #["for-each-ref", "--format=%(refname)"] dest
  for ref in refs.splitOn "\n" do
    unless ref.isEmpty do hardened' #["update-ref", "-d", ref] dest
  for f in ["FETCH_HEAD", "ORIG_HEAD"] do
    let p := dest / ".git" / f
    if ← p.pathExists then IO.FS.removeFile p
  try hardened' #["reflog", "expire", "--expire=now", "--all"] dest catch _ => pure ()
  let logs := dest / ".git" / "logs"
  if ← logs.pathExists then IO.FS.removeDirAll logs
  let head ← hardened #["rev-parse", "HEAD"] dest
  unless head == commit do
    throw (.userError s!"checkout: the isolated checkout's HEAD is {head}, not {commit}")
  excludeOrchestraArtifacts dest
  IO.println s!"  Isolated {commit} at {dest}: no other refs, no reflog"

/-- Make `dest` the tree task runs in, from the verified `source` (`prepareCheckoutSource`).

    With `isolate`, `isolateCheckout`. Otherwise an ordinary repository: the upstream's branches as
    `upstream/*`, the fork and the upstream as remotes with the credential helper a slot has, and a
    detached `HEAD` at the commit, checked to be it. -/
def buildCheckoutTree (fork upstream : Repository) (source dest : System.FilePath)
    (spec : CheckoutSpec) : IO Unit := do
  if spec.isolate then
    isolateCheckout source dest spec.commit
    return
  freshDir dest
  let format ← try hardened #["rev-parse", "--show-object-format"] source catch _ => pure "sha1"
  hardened' #["init", "--quiet", s!"--object-format={format}", dest.toString]
  let refspecs := #["+refs/orchestra/fetch/heads/*:refs/remotes/upstream/*"] ++
    (if spec.fetchRef.isSome then #["+refs/orchestra/fetch/ref:refs/orchestra/checkout"] else #[])
  hardened' (#["fetch", "--no-tags", "--quiet", source.toString] ++ refspecs) dest
  hardened' #["checkout", "--detach", "--quiet", "--force", spec.commit] dest
  try hardened' #["update-ref", "-d", "refs/orchestra/checkout"] dest catch _ => pure ()
  let head ← hardened #["rev-parse", "HEAD"] dest
  unless head == spec.commit do
    throw (.userError s!"checkout: HEAD is {head} after checking out {spec.commit}")
  runGit' #["remote", "add", "origin", githubUrl fork] dest
  configureClone fork upstream dest
  IO.println s!"  Checked out {spec.commit} at {dest} on a detached HEAD"

/-- Prepare task `taskId`'s checkout of `spec` from scratch (see the section docs) and return the
    tree it runs in and the directory its configuration is read from. `url` is the upstream's,
    unless a test points it at a repository on this disk. -/
def prepareCheckout (fork upstream : Repository) (taskId : String) (spec : CheckoutSpec)
    (token : Option String := none) (url : Option String := none) (buildTree : Bool := true) :
    IO (System.FilePath × System.FilePath) := do
  let source ← checkoutSourcePath fork taskId
  let config ← checkoutConfigPath fork taskId
  let tree ← taskCheckoutPath fork taskId
  try
    prepareCheckoutSource (url.getD (githubUrl upstream)) spec source config token
    -- Without a tree, for a continuation whose backend hands it its predecessor's workspace: an
    -- empty directory stands where the tree would, for what reads the path.
    if buildTree then buildCheckoutTree fork upstream source tree spec else freshDir tree
  catch e =>
    removeCheckoutDirs fork taskId
    throw e
  -- The source has served its purpose; only the tree and the configuration are used from here.
  try IO.FS.removeDirAll source catch _ => pure ()
  return (tree, config)

/-- Remove every `refs/orchestra/*` a crash left in `repo`: the temporary refs a checkout uses
    while it runs, which nothing should find afterwards. Best effort. -/
def clearOrchestraRefs (repo : System.FilePath) : IO Unit := do
  try
    -- The repository at `repo` and no other: named by `--git-dir`, so git's discovery cannot climb
    -- out of a directory that is not one into an enclosing repository — a git-tracked `$HOME`.
    let gitDir ← if ← (repo / ".git").pathExists then pure (repo / ".git")
      else if (← (repo / "HEAD").pathExists) && (← (repo / "objects").pathExists) then pure repo
      else return
    let g := #["--git-dir", gitDir.toString]
    let refs ← hardened (g ++ #["for-each-ref", "--format=%(refname)", "refs/orchestra/"]) repo
    for ref in refs.splitOn "\n" do
      unless ref.isEmpty do hardened' (g ++ #["update-ref", "-d", ref]) repo
  catch _ => pure ()

/-- Base directory for the scratch workspaces repository-independent tasks run in.

    A sibling of `workDir` rather than a directory inside it: everything under `workDir` is a
    clone or a slot of one, and `listClones` walks it expecting exactly that shape. -/
def workspaceBase : IO System.FilePath :=
  return (← Dirs.dataBase) / "workspaces"

/-- Directory name of workspace slot `slot`. -/
private def workspaceSlotName (slot : Nat) : String := s!"slot-{slot}"

/-- Directory name of the workspace a run outside the queue uses.

    Kept apart from the numbered slots the daemon pools, the way a CLI run works in the shared
    cache clone rather than in one of them: preparing a workspace *empties* it, so sharing a name
    with the daemon would let `orchestra run` delete the files out from under a task the daemon is
    in the middle of.

    Two runs outside the queue do still share it, and this is where the analogy with the cache
    clone stops: `ensureCloned` leaves an existing checkout in place, while this deletes one. A
    second `orchestra run` or `orchestra interactive` with no repository, started while the first
    is still going, takes the directory and empties it. Nothing guards that — running several at
    once is what the queue is for, and there each gets a pooled slot of its own. -/
private def adhocWorkspaceName : String := "adhoc"

/-- The workspace base a call works under: an explicit one where the caller supplied it, and the
    installation's own otherwise.

    The override exists for the tests. Preparing a workspace *empties* it, and the directory a test
    would otherwise land on is the very one `orchestra run` and `orchestra interactive` share — so
    a test suite that reached the real base would delete the scratch files of a repository-
    independent run happening to be in progress, and leave its own occupant marker behind for the
    next continuation to trip over. Nothing outside the tests passes it. -/
private def resolveWorkspaceBase : Option System.FilePath → IO System.FilePath
  | some b => pure b
  | none   => workspaceBase

/-- File recording which task's files currently sit in the workspace named `name`.

    Beside the workspace rather than inside it, because the whole directory is handed to the
    agent and wiped between tasks — a marker within it would be both visible to the agent and
    destroyed by the next reset. -/
private def workspaceOccupantPath (base : System.FilePath) (name : String) : System.FilePath :=
  base / s!"{name}.occupant"

/-- The task whose files currently sit in workspace slot `slot`, if it is known. -/
def workspaceOccupant (slot : Nat) (base : Option System.FilePath := none)
    : IO (Option String) := do
  readOccupant (workspaceOccupantPath (← resolveWorkspaceBase base) (workspaceSlotName slot))

/-- Who holds slot `slot` of the pool `fork` names, or of the shared repository-independent
    pool when `fork` is `none`. The one entry point the queue asks, so that a continuation is
    checked the same way whether or not its predecessor had a repository. -/
def poolOccupant (fork : Option Repository) (slot : Nat) : IO (Option String) := do
  match fork with
  | some f => slotOccupant f slot
  | none   => workspaceOccupant slot

/-- Ensure the workspace named `name` exists and is empty, and return its path.

    The counterpart of `ensureSlot` for a repository-independent task: same pooling, same
    occupant bookkeeping, same continuation rule — only with nothing to check out. The agent
    gets an empty directory it can write in, and reaches whatever it needs (other repositories,
    the issue tracker) through the tools it was granted.

    Reset means *emptied*, not cleaned selectively. A clone slot keeps gitignored build output
    between tasks because git can say which files those are; a workspace has nothing that could
    draw that line, so one task's scratch files would otherwise turn up in the next task's
    directory with no way to tell them apart from its own. Anything meant to outlive a run
    belongs in a memory directory, which is mounted separately and persists on purpose. -/
private def prepareWorkspace (base : System.FilePath) (name : String) (assign : SlotAssignment)
    : IO System.FilePath := do
  let path := base / name
  let marker := workspaceOccupantPath base name
  -- Same rule as `ensureSlot`: a continuation only inherits the directory if the predecessor's
  -- files are still the ones sitting in it. Workspaces are pooled, so a free slot is not
  -- evidence of that.
  let keep ← match assign.resumeFrom with
    | none      => pure false
    | some pred =>
      match ← readOccupant marker with
      | some cur =>
        if cur == pred then pure true
        else do
          IO.eprintln s!"  Warning: workspace {name} was last used by {cur}, not {pred}; \
emptying it. The resumed agent will not find the files its session refers to."
          pure false
      | none => do
        IO.eprintln s!"  Warning: workspace {name} has no recorded occupant, so the files {pred} \
left cannot be confirmed; emptying it. The resumed agent will not find the files its session \
refers to."
        pure false
  -- Errors are reported, not raised, on both ends: failing to write the marker costs a
  -- continuation its files, while failing to prepare the directory costs the run itself, and the
  -- second is not worth risking for the first.
  let writeMarker (occupant : Option String) : IO Unit := do
    try
      match occupant with
      | some o => IO.FS.writeFile marker o
      | none   => if ← marker.pathExists then IO.FS.removeFile marker
    catch e =>
      IO.eprintln s!"  Warning: could not record the occupant of workspace {name}: {e}"
  if keep then
    IO.println s!"  Keeping workspace {name} as-is (continuation of {assign.resumeFrom.getD "?"})"
  else
    -- Cleared *before* the directory is emptied, not merely overwritten afterwards. A run that
    -- died between the two would otherwise leave an empty directory under a marker still naming
    -- the previous occupant — and the next continuation of *that* task would match it, take the
    -- branch above, and wake up in an empty directory with nothing on the way to say why.
    -- Cleared first, the same crash leaves the workspace nobody's, which is the answer every
    -- caller already handles.
    writeMarker none
    if ← path.pathExists then
      IO.FS.removeDirAll path
  IO.FS.createDirAll path
  -- Stamped after preparation, for the same reason as `ensureSlot`: a task that dies while the
  -- directory is being emptied must not leave it claiming to hold files it never wrote.
  writeMarker assign.occupant
  return path

/-- Ensure workspace slot `slot` of the repository-independent pool exists and is empty. -/
def ensureWorkspace (assign : SlotAssignment) (base : Option System.FilePath := none)
    : IO System.FilePath := do
  prepareWorkspace (← resolveWorkspaceBase base) (workspaceSlotName assign.slot) assign

/-- Ensure the workspace for a repository-independent run outside the queue exists, and empty it
    unless `resumeFrom` names the task whose files are still in it.

    `resumeFrom` is what makes `orchestra continue` on a repository-independent task work: the
    resumed agent's session describes files it wrote, and without it every continuation would wake
    up to an emptied directory. It is checked against the recorded occupant like the pooled path's
    is, so a continuation whose files were taken by an unrelated run in between is told so rather
    than resumed onto somebody else's.

    See `adhocWorkspaceName` for why this is not one of the pooled slots. -/
def ensureAdhocWorkspace (occupant : Option String := none)
    (resumeFrom : Option String := none) (base : Option System.FilePath := none)
    : IO System.FilePath := do
  prepareWorkspace (← resolveWorkspaceBase base) adhocWorkspaceName
    { slot := 0, occupant, resumeFrom }

/-- An empty scratch directory for one repository-independent task, for the same backends. -/
def ensureTaskWorkspace (taskId : String) (base : Option System.FilePath := none)
    : IO System.FilePath := do
  let path := (← resolveWorkspaceBase base) / "tasks" / taskId
  if ← path.pathExists then IO.FS.removeDirAll path
  IO.FS.createDirAll path
  return path

/-- The scratch workspace of a session without a repository — a resident — on a backend that runs
    the agent on this machine: `<workspaces>/sessions/<id>`. Created if missing and never emptied,
    unlike every other workspace here: a resident's session does not end, and what it left in its
    workspace yesterday is part of the conversation it is still having. -/
def ensureSessionWorkspace (id : String) (base : Option System.FilePath := none)
    : IO System.FilePath := do
  let path := (← resolveWorkspaceBase base) / "sessions" / id
  IO.FS.createDirAll path
  return path

/-- Remove a per-task checkout or workspace. Never throws: what is left behind is replaced the next
    time the same id is prepared, and costs disk, not correctness. -/
def removeTaskDir (path : System.FilePath) : IO Unit := do
  try if ← path.pathExists then IO.FS.removeDirAll path
  catch e => IO.eprintln s!"  Warning: could not remove {path}: {e}"

/-- Remove per-task checkouts and workspaces left behind by tasks that never got to remove their
    own — a daemon that crashed, a task that failed before its environment was opened. Anything
    older than a day: no task holds its checkout that long, since the checkout is only what its
    workspace is filled from at the start. Best effort. -/
def sweepTaskDirs : IO Unit := do
  try
    let work ← workDir
    let ws := (← workspaceBase) / "tasks"
    let q (s : String) : String := "'" ++ s.replace "'" "'\\''" ++ "'"
    let script := s!"for d in {q work.toString}/*/*-tasks {q ws.toString}; do \
[ -d \"$d\" ] && find \"$d\" -mindepth 1 -maxdepth 1 -mmin +1440 -exec rm -rf {"{}"} +; done; true"
    let _ ← IO.Process.output { cmd := "/bin/sh", args := #["-c", script] }
  catch _ => pure ()

/-- Every scratch workspace that currently exists, in name order. -/
def listWorkspaces (base : Option System.FilePath := none) : IO (Array System.FilePath) := do
  let base ← resolveWorkspaceBase base
  if !(← base.pathExists) then return #[]
  let entries ← base.readDir
  let dirs ← entries.filterM (fun e => e.path.isDir)
  return dirs.map (·.path) |>.qsort (fun a b => decide (a.toString < b.toString))

/-- Split a slot directory name into the repository it belongs to, e.g.
    `mathlib4-slot-2` ↦ `mathlib4`. Returns none for a non-slot directory.

    Only the trailing `-slot-<digits>` counts, so a repository genuinely named
    `my-slot-repo` is not mistaken for slot `repo` of some repository `my`. -/
def slotBaseName (name : String) : Option String :=
  let parts := name.splitOn "-slot-"
  if parts.length < 2 then none
  else
    let idx := parts.getLast!
    if !idx.isEmpty && idx.all Char.isDigit then
      some (String.intercalate "-slot-" parts.dropLast)
    else none

/-- List all cache clones under the work directory, together with the task slots belonging
    to each. Returns an array of (cacheClonePath, slotPaths). -/
def listClones : IO (Array (System.FilePath × Array System.FilePath)) := do
  let base ← workDir
  if !(← base.pathExists) then return #[]
  let mut result : Array (System.FilePath × Array System.FilePath) := #[]
  for ownerEntry in ← base.readDir do
    let ownerPath := ownerEntry.path
    -- A stray regular file under the work directory is not an owner directory; skipping it
    -- keeps `cleanup list` from throwing on `readDir`.
    if !(← ownerPath.isDir) then continue
    let entries ← ownerPath.readDir
    -- Group the slots by the repository they belong to in one pass, rather than rescanning
    -- the owner directory once per repository.
    let slotsByRepo := entries.foldl (init := ({} : Std.HashMap String (Array System.FilePath)))
      fun m s =>
        match slotBaseName s.fileName with
        | some base => m.insert base ((m.getD base #[]).push s.path)
        | none      => m
    for e in entries do
      let name := e.fileName
      -- Slots are listed under their repository, never as repositories of their own.
      if (slotBaseName name).isSome then continue
      if !(← isGitRepo e.path) then continue
      let slots := slotsByRepo.getD name #[]
      result := result.push (e.path, slots.qsort (fun a b => decide (a.toString < b.toString)))
  return result

/-- Remove the `refs/orchestra/*` a crash left behind, at startup and whatever their age: in every
    cache clone and slot (an earlier version made checkouts there), and in every per-task
    checkout — the source a checkout was verified in, its tree, an isolated one. A task the next
    daemon picks up again loses nothing by it: those refs only live while a checkout is being
    made. Best effort. -/
def clearCrashLeftovers : IO Unit := do
  try
    for (clone, slots) in ← listClones do
      clearOrchestraRefs clone
      for slot in slots do clearOrchestraRefs slot
    let base ← workDir
    if ← base.pathExists then
      for ownerEntry in ← base.readDir do
        if !(← ownerEntry.path.isDir) then continue
        for e in ← ownerEntry.path.readDir do
          unless e.fileName.endsWith "-tasks" && (← e.path.isDir) do continue
          for t in ← e.path.readDir do
            if ← t.path.isDir then clearOrchestraRefs t.path
  catch _ => pure ()

/-- Remove all cloned repositories, task slots and scratch workspaces. -/
def cleanup : IO Unit := do
  let mut removed := false
  for base in [← workDir, ← workspaceBase] do
    if ← base.pathExists then
      IO.FS.removeDirAll base
      IO.println s!"Removed {base}"
      removed := true
  if !removed then
    IO.println "Nothing to clean up"

end Orchestra.Repo
