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

/--
Ensure the fork is cloned and the upstream remote is configured.
Returns the path to the local repository.
-/
def ensureCloned (fork upstream : String) : IO System.FilePath := do
  let (forkOwner, forkRepo) ← splitRepo fork
  let base ← workDir
  let repoPath := base / forkOwner / forkRepo
  if ← repoPath.pathExists then
    -- Verify it's a git repo
    let _ ← runGit #["rev-parse", "--git-dir"] repoPath
    -- Make sure upstream remote exists
    let remotes ← runGit #["remote"] repoPath
    let hasUpstream := remotes.splitOn "\n" |>.any (· == "upstream")
    if !hasUpstream then
      runGit' #["remote", "add", "upstream", s!"https://github.com/{upstream}.git"] repoPath
    return repoPath
  else
    IO.FS.createDirAll repoPath
    runGit' #["clone", s!"https://github.com/{fork}.git", repoPath.toString]
    runGit' #["remote", "add", "upstream", s!"https://github.com/{upstream}.git"] repoPath
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
