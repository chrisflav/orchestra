import Orchestra.Config
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

/-- `-c` overrides installing `credentialHelperScript` for a single `git` invocation.
    The empty first value clears any helper inherited from `~/.gitconfig`, so a stale global
    helper left by an earlier `gh auth setup-git` cannot take precedence and feed git a
    different task's token. -/
private def credentialArgs (token : Option String) : Array String :=
  if token.isNone then #[]
  else #["-c", "credential.helper=", "-c", s!"credential.helper={credentialHelperScript}"]

private def runGit (args : Array String) (cwd : Option System.FilePath := none)
    (token : Option String := none) : IO String := do
  let child ← IO.Process.spawn {
    cmd := "git"
    args := credentialArgs token ++ args
    cwd
    env := tokenEnv token
    stdout := .piped
    stderr := .piped
  }
  let stdout ← child.stdout.readToEnd
  let stderr ← child.stderr.readToEnd
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
  let stdout ← child.stdout.readToEnd
  let stderr ← child.stderr.readToEnd
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
    for its own task, and the repository is mounted while `~/.gitconfig` may not be. -/
private def configureCredentialHelper (repoPath : System.FilePath) : IO Unit := do
  try runGit' #["config", "credential.helper", credentialHelperScript] repoPath
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
    let marker := "# orchestra"
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

/-- Ensure task slot `slot` of `fork` exists and is clean, and return its path.

    A slot is an independent clone with its own `.git`, and therefore its own ref
    namespace: two concurrent tasks on one repository can create identically named
    branches without git refusing, which is what a shared worktree layout cannot offer.
    `git clone --local` hardlinks the object store from the cache clone, so a slot costs a
    working tree rather than a second copy of the history.

    Slots are reused across tasks rather than created per task so that gitignored build
    output survives; see `resetSlot`.

    `reset := false` keeps the working tree exactly as the previous task left it. The queue
    passes this for a `continuesFrom` entry, whose agent resumes a conversation full of
    references to files it had just edited and would otherwise wake up to a wiped tree. -/
def ensureSlot (fork upstream : Repository) (slot : Nat) (token : Option String := none)
    (reset : Bool := true) : IO System.FilePath := do
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
  runGit' #["remote", "set-url", "origin", githubUrl fork] sPath
  let remotes ← runGit #["remote"] sPath
  if remotes.splitOn "\n" |>.any (· == "upstream") then
    runGit' #["remote", "set-url", "upstream", githubUrl upstream] sPath
  else
    runGit' #["remote", "add", "upstream", githubUrl upstream] sPath
  configureCredentialHelper sPath
  excludeOrchestraArtifacts sPath
  if reset then
    resetSlot sPath token
  else
    IO.println s!"  Keeping slot {slot} as-is (continuation task)"
  return sPath

/-- Split a slot directory name into the repository it belongs to, e.g.
    `mathlib4-slot-2` ↦ `mathlib4`. Returns none for a non-slot directory. -/
private def slotBaseName (name : String) : Option String :=
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
    for e in entries do
      let name := e.fileName
      -- Slots are listed under their repository, never as repositories of their own.
      if (slotBaseName name).isSome then continue
      if !(← isGitRepo e.path) then continue
      let slots := entries.filterMap fun s =>
        if slotBaseName s.fileName == some name then some s.path else none
      result := result.push (e.path, slots.qsort (fun a b => decide (a.toString < b.toString)))
  return result

/-- Remove all cloned repositories and task slots. -/
def cleanup : IO Unit := do
  let base ← workDir
  if ← base.pathExists then
    IO.FS.removeDirAll base
    IO.println s!"Removed {base}"
  else
    IO.println "Nothing to clean up"

end Orchestra.Repo
