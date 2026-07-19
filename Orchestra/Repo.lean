import Orchestra.Config

namespace Orchestra.Repo

private def runGit (args : Array String) (cwd : Option System.FilePath := none) : IO String := do
  let child ← IO.Process.spawn {
    cmd := "git"
    args
    cwd
    stdout := .piped
    stderr := .piped
  }
  let stdout ← child.stdout.readToEnd
  let stderr ← child.stderr.readToEnd
  let code ← child.wait
  if code != 0 then
    throw (.userError s!"git {args[0]!} failed (exit {code}):\n{stderr}")
  return stdout.trimAscii.toString

private def runGit' (args : Array String) (cwd : Option System.FilePath := none) : IO Unit := do
  let _ ← runGit args cwd

private def runGh (args : Array String) (cwd : Option System.FilePath := none) : IO String := do
  let child ← IO.Process.spawn {
    cmd := "gh"
    args
    cwd
    stdout := .piped
    stderr := .piped
  }
  let stdout ← child.stdout.readToEnd
  let stderr ← child.stderr.readToEnd
  let code ← child.wait
  if code != 0 then
    throw (.userError s!"gh {args[0]!} failed (exit {code}):\n{stderr}")
  return stdout.trimAscii.toString

private def runGh' (args : Array String) (cwd : Option System.FilePath := none) : IO Unit := do
  let _ ← runGh args cwd

/-- Base directory for all agent work. -/
def workDir : IO System.FilePath :=
  return (← Dirs.dataBase) / "repos"

/-- Build a plain GitHub HTTPS URL (no credentials). -/
private def githubUrl (repo : Repository) : String :=
  s!"https://github.com/{repo}.git"

/--
Ensure the fork is cloned and the upstream remote is configured.
Returns the path to the local repository.
When `interactive` is false (e.g. queue mode), user prompts are suppressed and
an error is thrown instead.
Cloning is performed via `gh repo clone`, which uses the GitHub App authentication
already configured by `GitHub.setupGhAuth`.
-/
def ensureCloned (fork upstream : Repository) (interactive : Bool := true) : IO System.FilePath := do
  let base ← workDir
  let repoPath := base / fork.owner / fork.name
  if ← repoPath.pathExists then
    let entries ← repoPath.readDir
    if entries.isEmpty then
      -- Empty directory left over from a failed clone: remove and retry
      IO.println s!"  Directory '{repoPath}' is empty; removing and re-cloning..."
      IO.FS.removeDirAll repoPath
      IO.FS.createDirAll repoPath
      runGh' #["repo", "clone", fork.toString, repoPath.toString]
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
          runGh' #["repo", "clone", fork.toString, repoPath.toString]
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
    runGh' #["repo", "clone", fork.toString, repoPath.toString]
    let remotes₃ ← runGit #["remote"] repoPath
    if !(remotes₃.splitOn "\n" |>.any (· == "upstream")) then
      runGit' #["remote", "add", "upstream", githubUrl upstream] repoPath
  -- Ensure gh credentials are wired into git for authenticated operations
  runGh' #["auth", "setup-git"] none
  -- Fetch upstream to keep remote tracking branches up to date
  IO.println "  Fetching upstream..."
  runGit' #["fetch", "upstream"] repoPath
  return repoPath

/-- Path for the main clone of a fork repository. -/
def clonePath (fork : Repository) : IO System.FilePath := do
  return (← workDir) / fork.owner / fork.name

/-- Path for a git worktree tied to a specific queue entry. -/
def worktreePath (fork : Repository) (entryId : String) : IO System.FilePath := do
  return (← workDir) / fork.owner / s!"{fork.name}-wt-{entryId}"

/-- Remove a git worktree after the task using it finishes. -/
def removeWorktree (mainPath : System.FilePath) (wtPath : System.FilePath) : IO Unit := do
  try
    runGit' #["worktree", "remove", "--force", wtPath.toString] mainPath
    IO.println s!"  Removed worktree {wtPath}"
  catch e =>
    IO.eprintln s!"  Warning: failed to remove worktree {wtPath}: {e}"
    try IO.FS.removeDirAll wtPath catch _ => pure ()
    -- The directory is gone but git still has it registered; drop the registration
    -- so a later `worktree add` can reuse the path.
    try runGit' #["worktree", "prune"] mainPath catch _ => pure ()

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

/-- The commit a new worktree should be based on.

    New work belongs on the default branch, not on whatever branch the previous task
    happened to leave the main clone sitting on. The branch *name* comes from `origin`
    (recorded at clone time, so no network call), but the commit is taken from
    `upstream` when available: `ensureCloned` fetches upstream and never fetches
    origin, so the fork's remote-tracking refs are stale as of clone time. -/
private def worktreeBaseRef (repoPath : System.FilePath) : IO (Option String) := do
  let some branch ← defaultBranchName repoPath "origin" | return none
  for remote in ["upstream", "origin"] do
    let ref := s!"{remote}/{branch}"
    if ← refExists repoPath ref then return some ref
  return none

/-- Create a git worktree for a parallel agent task on the same repository.
    Ensures the main clone exists and is up to date first, then adds a worktree that
    shares the same git objects, detached at the default branch. Returns the worktree path. -/
def ensureWorktree (fork upstream : Repository) (entryId : String) : IO System.FilePath := do
  let mainPath ← ensureCloned fork upstream (interactive := false)
  let wtPath ← worktreePath fork entryId
  -- Drop registrations whose directories vanished behind git's back (e.g. the daemon
  -- was killed mid-task), otherwise `worktree add` refuses to reuse the path.
  try runGit' #["worktree", "prune"] mainPath catch _ => pure ()
  -- A leftover tree from a previous run would start the task from a dirty state,
  -- so tear it down rather than silently reusing it.
  if ← wtPath.pathExists then
    IO.println s!"  Removing stale worktree at {wtPath}..."
    removeWorktree mainPath wtPath
  let baseRef ← worktreeBaseRef mainPath
  if baseRef.isNone then
    IO.eprintln s!"  Warning: could not determine the default branch of {fork}; basing the \
      worktree on the main clone's current HEAD, which may be a previous task's branch."
  IO.println s!"  Creating worktree at {wtPath} (base: {baseRef.getD "HEAD"})..."
  -- Detached: the default branch is checked out in the main clone, and git refuses to
  -- have the same branch checked out in two worktrees at once.
  runGit' (#["worktree", "add", "--detach", wtPath.toString] ++ baseRef.toArray) mainPath
  return wtPath

/-- Return true if `path` is a main git clone. A worktree's `.git` is a file rather
    than a directory, so it has no `.git/config` and is correctly excluded here. -/
private def isMainClone (path : System.FilePath) : IO Bool :=
  (path / ".git" / "config").pathExists

/-- List all main clones under the work directory, together with any git worktrees
    each clone has registered. Returns an array of (mainClonePath, worktreePaths). -/
def listClones : IO (Array (System.FilePath × Array System.FilePath)) := do
  let base ← workDir
  if !(← base.pathExists) then return #[]
  let mut result : Array (System.FilePath × Array System.FilePath) := #[]
  for ownerEntry in ← base.readDir do
    let ownerPath := ownerEntry.path
    if !(← ownerPath.pathExists) then continue
    for repoEntry in ← ownerPath.readDir do
      let repoPath := repoEntry.path
      if !(← isMainClone repoPath) then continue
      -- `git worktree list` prints fully resolved absolute paths, so compare against
      -- the resolved main clone path rather than the one built from `workDir` — a
      -- symlink anywhere in the data directory would otherwise make the main clone
      -- list itself as one of its own worktrees.
      let mainReal ← try IO.FS.realPath repoPath catch _ => pure repoPath
      let worktrees ← try
        let out ← runGit #["worktree", "list", "--porcelain"] repoPath
        let paths := out.splitOn "\n" |>.filterMap fun l =>
          if l.startsWith "worktree " then
            let p := (l.drop "worktree ".length).toString
            if p != mainReal.toString then some (System.FilePath.mk p)
            else none
          else none
        pure paths.toArray
      catch _ => pure #[]
      result := result.push (repoPath, worktrees)
  return result

/-- Remove all cloned repositories and worktrees. -/
def cleanup : IO Unit := do
  let base ← workDir
  if ← base.pathExists then
    IO.FS.removeDirAll base
    IO.println s!"Removed {base}"
  else
    IO.println "Nothing to clean up"

end Orchestra.Repo
