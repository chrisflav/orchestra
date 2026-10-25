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

/-! ## Git the daemon runs in a tree an agent wrote

Every clone orchestra prepares is handed to an agent read-write, `.git` included, and the daemon
runs `git` in it again afterwards, unsandboxed, as the operator: the next task's `resetSlot`, the
merger's `gh pr checkout`, a continuation's fetch. Git executes things its configuration names —
hooks, `core.fsmonitor`, `core.askPass`, filter and diff drivers, `ext::` transports — and the
repository's own `.git/config` is configuration. An agent that planted `core.fsmonitor` in a slot
ran its payload on the host three times over on the next reset (verified on this host). So two
layers, either of which would stop that:

* **Every daemon-side git runs with the dangerous settings pinned** (`hardeningArgs`,
  `hardeningEnv`): no hooks, no fsmonitor, no askpass, no `ext::` transport, no implicit bare
  repository, no replacement objects, no system config, no prompting, and no walking up out of the
  directory it was pointed at. The pins go in as `-c` flags on our own invocations and through
  `GIT_CONFIG_COUNT` in the environment, so the git that `gh` runs on our behalf gets them too.
* **The repository's own configuration is rewritten from scratch** before the daemon works in a
  clone an agent may have touched (`resetLocalConfig`): a handful of harmless keys are carried over
  and everything else — includes, drivers, `core.worktree`, remotes, credential helpers — is
  dropped, then orchestra's own remotes and helper are written back. A `.git` that is not a plain
  directory, or that points somewhere else (`commondir`), is not reused at all (`isGitRepo`). -/

/-- The settings pinned on every git the daemon runs; see the section above. -/
def hardeningConfig : Array (String × String) := #[
  ("core.hooksPath", "/dev/null"),
  ("core.fsmonitor", "false"),
  -- Empty means "do not ask": git skips an askpass program that is set to nothing.
  ("core.askPass", ""),
  ("protocol.ext.allow", "never"),
  ("safe.bareRepository", "explicit")]

/-- `hardeningConfig` as `-c` flags. -/
def hardeningArgs : Array String :=
  hardeningConfig.foldl (init := #[]) fun acc (k, v) => acc.push "-c" |>.push s!"{k}={v}"

/-- The environment every daemon-side git runs with, including the git `gh` runs on our behalf:
    `hardeningConfig` through `GIT_CONFIG_COUNT`, no system configuration, no replacement objects
    (an agent's `refs/replace/<sha>` would otherwise change what a commit the daemon asks for
    contains), no prompting — and, when `cwd` is given, no repository discovery above it: an agent
    that deleted a slot's `.git` would otherwise have the next `git reset --hard` find whatever
    repository encloses the data directory, the operator's home among them. -/
def hardeningEnv (cwd : Option System.FilePath := none) : IO (Array (String × Option String)) := do
  let mut env : Array (String × Option String) := #[
    ("GIT_CONFIG_NOSYSTEM", some "1"), ("GIT_NO_REPLACE_OBJECTS", some "1"),
    ("GIT_TERMINAL_PROMPT", some "0"), ("GIT_CONFIG_COUNT", some (toString hardeningConfig.size))]
  for h : i in [0:hardeningConfig.size] do
    let (k, v) := hardeningConfig[i]
    env := env.push (s!"GIT_CONFIG_KEY_{i}", some k) |>.push (s!"GIT_CONFIG_VALUE_{i}", some v)
  if let some dir := cwd then
    -- Resolved, because git compares the ceiling with the real working directory.
    let real ← try IO.FS.realPath dir catch _ => pure dir
    if let some parent := real.parent then
      env := env.push ("GIT_CEILING_DIRECTORIES", some parent.toString)
  return env

/-- Run `git args` in `cwd` as the daemon, with `hardeningArgs` and `hardeningEnv` and, given a
    token, the credential helper (`credentialArgs`); its trimmed standard output, or a throw
    carrying its standard error. Every git the daemon runs goes through here. -/
def runGit (args : Array String) (cwd : Option System.FilePath := none)
    (token : Option String := none) : IO String := do
  let child ← IO.Process.spawn {
    cmd := "git"
    args := hardeningArgs ++ credentialArgs token ++ args
    cwd
    env := (← hardeningEnv cwd) ++ tokenEnv token
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
    env := (← hardeningEnv cwd) ++ tokenEnv token
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

/-- Keys of a repository's own configuration that `resetLocalConfig` carries over. None of them
    names a program, a path git would read or write, or a place to fetch from; everything else in
    the file can, or could in a future git, and is dropped. -/
private def keptLocalConfig : Array String := #[
  "core.repositoryformatversion", "core.filemode", "core.logallrefupdates", "core.ignorecase",
  "core.precomposeunicode", "core.symlinks", "extensions.objectformat", "extensions.refstorage",
  "user.name", "user.email"]

/-- Rewrite `path`'s own git configuration from scratch: `keptLocalConfig` carried over, `origin`
    the fork and `upstream` the upstream over HTTPS, and nothing else. Then the credential helper
    and the excludes are put back (`configureCredentialHelper`, `excludeOrchestraArtifacts`).

    An agent has had `.git` read-write. What it may have left there for the daemon's next git to
    find — `include.path`, `core.worktree` pointing the next `git clean -fd` at the operator's home,
    a filter driver the next checkout runs, an askpass or credential helper, a remote URL of its
    choosing — is in this file or in one this file names, so the file is replaced rather than
    edited. So are the other places git reads configuration-like state from a repository:
    `config.worktree`, `info/attributes` and `objects/info/alternates`.

    Reading the old file runs nothing: `git config --file` reads one file and follows no includes.
    The new one is written next to it and renamed over it, so a failure half way leaves the old
    one, never a truncated one. -/
def resetLocalConfig (path : System.FilePath) (fork upstream : Repository) : IO Unit := do
  let gitDir := path / ".git"
  let cfg := gitDir / "config"
  let fresh := gitDir / "config.orchestra-new"
  try IO.FS.removeFile fresh catch _ => pure ()
  let set (k v : String) : IO Unit := runGit' #["config", "--file", fresh.toString, k, v]
  for k in keptLocalConfig do
    match ← (try some <$> runGit #["config", "--file", cfg.toString, "--get", k]
             catch _ => pure none) with
    | some v => set k v
    | none   => pure ()
  set "core.bare" "false"
  set "remote.origin.url" (githubUrl fork)
  set "remote.origin.fetch" "+refs/heads/*:refs/remotes/origin/*"
  set "remote.upstream.url" (githubUrl upstream)
  set "remote.upstream.fetch" "+refs/heads/*:refs/remotes/upstream/*"
  IO.FS.rename fresh cfg
  for extra in [gitDir / "config.worktree", gitDir / "info" / "attributes",
                gitDir / "objects" / "info" / "alternates"] do
    try IO.FS.removeFile extra catch _ => pure ()
  configureCredentialHelper path
  excludeOrchestraArtifacts path

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
        -- An existing clone may be one an agent worked in (`orchestra interactive` runs it in
        -- the cache clone itself), so its configuration is rewritten rather than trusted: the
        -- remotes come back as the fork and the upstream over HTTPS, and nothing an agent
        -- planted survives. See `resetLocalConfig`.
        resetLocalConfig repoPath fork upstream
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

/-- Return true if `path` is a git repository orchestra can work in: a real `.git` directory (not
    a symlink, not a `gitdir:` file), with a real `config` file in it, that is not a linked
    worktree of some other repository (`commondir`). Anything else in a slot was put there by an
    agent, and the slot is cloned afresh rather than trusted. -/
def isGitRepo (path : System.FilePath) : IO Bool := do
  let isKind (p : System.FilePath) (k : IO.FS.FileType) : IO Bool := do
    match ← p.symlinkMetadata.toBaseIO with
    | .ok md => return md.type == k
    | .error _ => return false
  return (← isKind (path / ".git") .dir) && (← isKind (path / ".git" / "config") .file)
    && !(← (path / ".git" / "commondir").pathExists)

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

/-- Copy the repository's script directory (`.orchestra/`, or the legacy `.agent/`) as the tip of
    its default branch has it into `dest`, and return which of the two it was; `none` when the
    default branch has neither.

    The scripts — `init.sh`, `before.sh`, `validation.sh`, `after.sh` and `config.json` — are taken
    from here and never from the working tree (`RepoConfig.Scripts`). The working tree is the
    agent's: it can rewrite `validation.sh` to do anything it likes, and then the daemon would run
    it. A task that checks out some other commit — a pull request's head, a past commit to be
    scored against — has the same problem without any agent, which is why the copy is of the
    default branch and not of `HEAD`.

    Read out of the object store by commit, after the daemon's own fetch has just set the upstream
    refs (`resetSlot`, or the fetch a kept tree gets in `ensureSlot`), with replacement objects off
    (`hardeningEnv`): neither an edited file nor a planted `refs/replace/` changes what comes out.
    Called before the agent starts, so there is nothing running that could move the ref between the
    fetch and the read. `dest` should be a directory only the daemon can reach. -/
def snapshotScripts (path dest : System.FilePath) : IO (Option String) := do
  let some (_, ref) ← slotBaseRef path
    | throw (.userError s!"could not determine the default branch in {path}, so the repository's \
scripts cannot be taken from it")
  let commit ← runGit #["rev-parse", "--verify", "--end-of-options", ref ++ "^{commit}"] path
  for name in [".orchestra", ".agent"] do
    let listing ← try runGit #["ls-tree", "-d", "--name-only", commit, "--", name] path
      catch _ => pure ""
    if listing.isEmpty then continue
    IO.FS.createDirAll dest
    let archive ← IO.Process.spawn {
      cmd := "git", cwd := path, env := ← hardeningEnv path
      args := hardeningArgs ++ #["archive", "--format=tar", commit, "--", name]
      stdin := .null, stdout := .piped, stderr := .piped }
    let errTask ← IO.asTask (prio := .dedicated) archive.stderr.readToEnd
    let bytes ← archive.stdout.readBinToEnd
    let archiveErr ← IO.ofExcept (← IO.wait errTask)
    unless (← archive.wait) == 0 do
      throw (.userError s!"git archive of {name} at {commit} failed: {archiveErr}")
    -- Through a file rather than a pipe into tar: the archive is small, and a file has no
    -- question of when the writing end is closed.
    let tarFile := System.FilePath.mk s!"{dest}.tar"
    IO.FS.writeBinFile tarFile bytes
    let out ← try
        IO.Process.output {
          cmd := "tar", args := #["-x", "--no-same-owner", "-C", dest.toString, "-f",
                                  tarFile.toString] }
      finally
        try IO.FS.removeFile tarFile catch _ => pure ()
    unless out.exitCode == 0 do
      throw (.userError s!"unpacking {name} at {commit} failed: {out.stderr}")
    return some name
  return none

/-- Fetch the upstream's branches and tags into `path`, by URL rather than by remote name: what the
    daemon fetches is decided here, not by a `remote.upstream.*` an agent may have edited since
    `resetLocalConfig` last wrote it. -/
private def fetchUpstream (path : System.FilePath) (upstream : Repository)
    (token : Option String) : IO Unit :=
  runGit' #["fetch", githubUrl upstream, "+refs/heads/*:refs/remotes/upstream/*",
            "+refs/tags/*:refs/tags/*"] path token

/-- Bring a task slot back to a clean state on an up-to-date default branch.

    Deliberately `git clean -fd` *without* `-x`: reusing slots rather than recreating them
    exists precisely so that gitignored build output (`.lake/`, `target/`, `node_modules/`)
    survives between tasks. The previous task's tracked modifications and untracked scratch
    files do not. -/
private def resetSlot (slotPath : System.FilePath) (upstream : Repository)
    (token : Option String) : IO Unit := do
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
  fetchUpstream slotPath upstream token
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
private def configureClone (fork upstream : Repository) (path : System.FilePath) : IO Unit :=
  resetLocalConfig path fork upstream

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
    -- The tree stays; the upstream refs do not. They are what the repository's scripts are taken
    -- from (`snapshotScripts`), and the predecessor's agent could have pointed them anywhere. A
    -- failure leaves them as they were, which is no worse than before: the scripts then run
    -- confined like the agent.
    try fetchUpstream sPath upstream token
    catch e => IO.eprintln s!"  Warning: could not refresh the upstream refs in slot {slot}: {e}"
  else
    resetSlot sPath upstream token
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
  resetSlot path upstream token
  return path


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
