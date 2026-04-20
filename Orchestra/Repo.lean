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
def workDir : IO System.FilePath := do
  match ← IO.getEnv "HOME" with
  | some h => return System.FilePath.mk h / ".agent" / "repos"
  | none => throw (.userError "HOME not set")

/-- Parse "owner/repo" into (owner, repo). -/
def splitRepo (s : String) : IO (String × String) := do
  match s.splitOn "/" with
  | [owner, repo] => return (owner, repo)
  | _ => throw (.userError s!"invalid repo format '{s}', expected 'owner/repo'")

/-- Build a plain GitHub HTTPS URL (no credentials). -/
private def githubUrl (repo : String) : String :=
  s!"https://github.com/{repo}.git"

/--
Ensure the fork is cloned and the upstream remote is configured.
Returns the path to the local repository.
When `interactive` is false (e.g. queue mode), user prompts are suppressed and
an error is thrown instead.
Cloning is performed via `gh repo clone`, which uses the GitHub App authentication
already configured by `GitHub.setupGhAuth`.
-/
def ensureCloned (fork upstream : String) (interactive : Bool := true) : IO System.FilePath := do
  let (forkOwner, forkRepo) ← splitRepo fork
  let base ← workDir
  let repoPath := base / forkOwner / forkRepo
  if ← repoPath.pathExists then
    let entries ← repoPath.readDir
    if entries.isEmpty then
      -- Empty directory left over from a failed clone: remove and retry
      IO.println s!"  Directory '{repoPath}' is empty; removing and re-cloning..."
      IO.FS.removeDirAll repoPath
      IO.FS.createDirAll repoPath
      runGh' #["repo", "clone", fork, repoPath.toString]
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
          runGh' #["repo", "clone", fork, repoPath.toString]
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
    runGh' #["repo", "clone", fork, repoPath.toString]
    let remotes₃ ← runGit #["remote"] repoPath
    if !(remotes₃.splitOn "\n" |>.any (· == "upstream")) then
      runGit' #["remote", "add", "upstream", githubUrl upstream] repoPath
  -- Ensure gh credentials are wired into git for authenticated pushes
  runGh' #["auth", "setup-git"] none
  return repoPath

/-- Remove all cloned repositories. -/
def cleanup : IO Unit := do
  let base ← workDir
  if ← base.pathExists then
    IO.FS.removeDirAll base
    IO.println s!"Removed {base}"
  else
    IO.println "Nothing to clean up"

end Orchestra.Repo
