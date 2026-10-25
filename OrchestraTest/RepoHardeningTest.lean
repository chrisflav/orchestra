import OrchestraTest.TestM
import Orchestra

open Orchestra

/-!
# Git the daemon runs in a tree an agent wrote, and the scripts it runs from it

Every clone orchestra prepares is the agent's to write, `.git` included, and the daemon runs `git`
and the repository's scripts in it afterwards, as the operator. Pinned here, against local
repositories and with no network:

* every daemon-side git runs with hooks, fsmonitor, askpass and `ext::` off, and does not walk up
  out of the directory it was pointed at (`Repo.hardeningArgs`, `Repo.hardeningEnv`) — shown by
  planting the payloads and checking a plain `git` does run them, so the test is not vacuous;
* a clone's own configuration is rewritten from scratch (`Repo.resetLocalConfig`), and a `.git`
  that is a symlink, a `gitdir:` file or a linked worktree is not a repository orchestra reuses;
* the scripts are taken from the default branch's commit, not from the working tree and not
  through a replacement object (`Repo.snapshotScripts`), and a backend running them here runs that
  copy (`ScriptSpec.trusted`).
-/

namespace OrchestraTest.RepoHardening

/-- A fresh directory under `/tmp`, removed afterwards. -/
private def withScratch (label : String) (act : System.FilePath → IO α) : IO α := do
  let root := System.FilePath.mk "/tmp" / s!"orchestra-{label}-{← IO.monoNanosNow}"
  IO.FS.createDirAll root
  try act root
  finally try IO.FS.removeDirAll root catch _ => pure ()

/-- Plain git, as an agent (or a test setting the scene) would run it: none of the daemon's pins. -/
private def git (cwd : System.FilePath) (args : Array String) : IO String := do
  let out ← IO.Process.output {
    cmd := "git", cwd
    args := #["-c", "user.name=t", "-c", "user.email=t@example.invalid",
              "-c", "init.defaultBranch=main", "-c", "commit.gpgsign=false"] ++ args }
  unless out.exitCode == 0 do
    throw (.userError s!"git {args}: {out.stderr}")
  return out.stdout.trimAscii.toString

/-- A repository at `dir` with one commit on `main` holding `.orchestra/validation.sh` (echoing
    `BASE`) and `.orchestra/lib.sh`, with `origin/HEAD` and `upstream/main` pointing at it, the way
    a slot looks after `resetSlot`. Returns the commit. -/
private def makeSlot (dir : System.FilePath) : IO String := do
  IO.FS.createDirAll (dir / ".orchestra")
  discard <| git dir #["init", "-q"]
  IO.FS.writeFile (dir / ".orchestra" / "validation.sh") "source \"$(dirname \"$0\")/lib.sh\"\n"
  IO.FS.writeFile (dir / ".orchestra" / "lib.sh") "echo BASE\n"
  IO.FS.writeFile (dir / "README") "hello\n"
  discard <| git dir #["add", "-A"]
  discard <| git dir #["commit", "-q", "-m", "base"]
  let commit ← git dir #["rev-parse", "HEAD"]
  discard <| git dir #["update-ref", "refs/remotes/origin/main", commit]
  discard <| git dir #["update-ref", "refs/remotes/upstream/main", commit]
  discard <| git dir #["symbolic-ref", "refs/remotes/origin/HEAD", "refs/remotes/origin/main"]
  return commit

/-- What an agent with `.git` read-write plants for the daemon: an fsmonitor and a hook that each
    leave a file in `marks`. -/
private def plant (dir marks : System.FilePath) : IO Unit := do
  IO.FS.createDirAll marks
  let payload (name : String) := s!"#!/bin/sh\ntouch {marks / name}\n"
  IO.FS.writeFile (dir / "fsmonitor.sh") (payload "fsmonitor")
  IO.FS.createDirAll (dir / "evil-hooks")
  IO.FS.writeFile (dir / "evil-hooks" / "pre-commit") (payload "hook")
  IO.FS.writeFile (dir / "evil-hooks" / "reference-transaction") (payload "hook")
  for f in [dir / "fsmonitor.sh", dir / "evil-hooks" / "pre-commit",
            dir / "evil-hooks" / "reference-transaction"] do
    discard <| IO.Process.output { cmd := "chmod", args := #["+x", f.toString] }
  discard <| git dir #["config", "core.fsmonitor", (dir / "fsmonitor.sh").toString]
  discard <| git dir #["config", "core.hooksPath", (dir / "evil-hooks").toString]

private def marked (marks : System.FilePath) : IO (List String) := do
  if !(← marks.pathExists) then return []
  return ((← marks.readDir).map (·.fileName)).toList

@[test]
def daemonGitRunsNothingAnAgentPlanted : Test := do
  let (plain, daemon) ← withScratch "git-pins" fun root => do
    let dir := root / "slot"
    discard <| makeSlot dir
    -- Control: plain git runs both payloads, so the planting works.
    plant dir (root / "plain")
    IO.FS.writeFile (dir / "README") "changed\n"
    discard <| git dir #["status", "--porcelain"]
    discard <| git dir #["commit", "-q", "-am", "plain"]
    let plain ← marked (root / "plain")
    -- The daemon's git, in the same tree with the same planting.
    plant dir (root / "daemon")
    IO.FS.writeFile (dir / "README") "changed again\n"
    discard <| Repo.runGit #["status", "--porcelain"] (some dir)
    discard <| Repo.runGit #["-c", "user.name=t", "-c", "user.email=t@example.invalid",
      "-c", "commit.gpgsign=false", "commit", "-q", "-am", "daemon"] (some dir)
    discard <| Repo.runGit #["reset", "--hard", "HEAD~1"] (some dir)
    return (plain, ← marked (root / "daemon"))
  TestM.assert (plain.contains "fsmonitor" && plain.contains "hook")
    s!"plain git runs the planted fsmonitor and hooks (got {plain}), so the check is not vacuous"
  TestM.assertEqual daemon [] (msg := "the daemon's git runs neither")

@[test]
def theDaemonsGitCarriesThePins : Test := do
  for (k, v) in [("core.hooksPath", "/dev/null"), ("core.fsmonitor", "false"),
                 ("protocol.ext.allow", "never"), ("core.askPass", "")] do
    TestM.assert (Repo.hardeningArgs.contains s!"{k}={v}") s!"-c {k}={v}"
  let env ← Repo.hardeningEnv (some (System.FilePath.mk "/tmp"))
  for name in ["GIT_CONFIG_NOSYSTEM", "GIT_NO_REPLACE_OBJECTS", "GIT_TERMINAL_PROMPT",
               "GIT_CONFIG_COUNT", "GIT_CEILING_DIRECTORIES"] do
    TestM.assert (env.any (·.1 == name)) s!"{name} is set"
  TestM.assert (env.contains ("GIT_CONFIG_KEY_0", some "core.hooksPath"))
    "the pins reach the git gh runs, through GIT_CONFIG_COUNT"

@[test]
def aSlotWithoutItsGitDoesNotFindTheEnclosingRepository : Test := do
  let r ← withScratch "git-ceiling" fun root => do
    discard <| makeSlot root
    let slot := root / "inner"
    IO.FS.createDirAll slot
    IO.FS.writeFile (slot / "file") "x\n"
    -- Plain git finds the enclosing repository from here...
    let plainFinds ← try discard <| git slot #["rev-parse", "--show-toplevel"]; pure true
      catch _ => pure false
    -- ...the daemon's does not, so its `reset --hard` cannot reach the enclosing tree.
    let daemonFinds ← try
        discard <| Repo.runGit #["rev-parse", "--show-toplevel"] (some slot); pure true
      catch _ => pure false
    return (plainFinds, daemonFinds)
  TestM.assert r.1 "plain git walks up into the enclosing repository"
  TestM.assert (!r.2) "the daemon's git stops at the directory it was pointed at"

@[test]
def aClonesOwnConfigurationIsRewrittenFromScratch : Test := do
  let (cfg, gone) ← withScratch "git-reset-config" fun root => do
    let dir := root / "slot"
    discard <| makeSlot dir
    discard <| git dir #["config", "core.worktree", "/home"]
    discard <| git dir #["config", "include.path", (root / "evil.cfg").toString]
    discard <| git dir #["config", "filter.x.smudge", "touch /tmp/pwned"]
    discard <| git dir #["config", "credential.helper", "!touch /tmp/pwned"]
    discard <| git dir #["config", "remote.upstream.url", "ext::sh -c touch% /tmp/pwned"]
    discard <| git dir #["config", "user.name", "Agent Smith"]
    IO.FS.writeFile (dir / ".git" / "config.worktree") "[core]\n\tfsmonitor = /tmp/x\n"
    IO.FS.writeFile (dir / ".git" / "info" / "attributes") "* filter=x\n"
    Repo.resetLocalConfig dir ⟨"acme", "widgets"⟩ ⟨"upstream-org", "widgets"⟩
    let cfg ← git dir #["config", "--file", (dir / ".git" / "config").toString, "--list"]
    return (cfg, !(← (dir / ".git" / "config.worktree").pathExists)
                 && !(← (dir / ".git" / "info" / "attributes").pathExists))
  for bad in ["core.worktree", "include.path", "filter.x", "credential.helper=!touch", "ext::"] do
    TestM.assert (!cfg.contains bad) s!"{bad} is gone"
  TestM.assert (cfg.contains "remote.upstream.url=https://github.com/upstream-org/widgets.git")
    "upstream is the upstream over HTTPS again"
  TestM.assert (cfg.contains "remote.origin.url=https://github.com/acme/widgets.git")
    "origin is the fork"
  TestM.assert (cfg.contains "credential.https://github.com.helper=") "orchestra's helper is back"
  TestM.assert (cfg.contains "user.name=Agent Smith") "a harmless key is carried over"
  TestM.assert gone "config.worktree and info/attributes are removed"

@[test]
def aGitDirectoryThatPointsElsewhereIsNotReused : Test := do
  let r ← withScratch "git-own" fun root => do
    let real := root / "real"
    discard <| makeSlot real
    let viaLink := root / "link"
    IO.FS.createDirAll viaLink
    discard <| IO.Process.output { cmd := "ln", args := #["-s", (real / ".git").toString,
      (viaLink / ".git").toString] }
    let viaFile := root / "file"
    IO.FS.createDirAll viaFile
    IO.FS.writeFile (viaFile / ".git") s!"gitdir: {real / ".git"}\n"
    let linked := root / "linked"
    discard <| git real #["worktree", "add", "-q", linked.toString]
    let fakeCommon := root / "common"
    discard <| makeSlot fakeCommon
    IO.FS.writeFile (fakeCommon / ".git" / "commondir") (real / ".git").toString
    return (← Repo.isGitRepo real, ← Repo.isGitRepo viaLink, ← Repo.isGitRepo viaFile,
            ← Repo.isGitRepo linked, ← Repo.isGitRepo fakeCommon)
  TestM.assert r.1 "an ordinary clone is reused"
  TestM.assert (!r.2.1) "a .git that is a symlink is not"
  TestM.assert (!r.2.2.1) "nor a gitdir: file"
  TestM.assert (!r.2.2.2.1) "nor a linked worktree"
  TestM.assert (!r.2.2.2.2) "nor a .git with a commondir"

/-! ## The scripts come from the default branch -/

@[test]
def theScriptsAreTheDefaultBranchesNotTheTrees : Test := do
  let (name, content, lib) ← withScratch "git-snapshot" fun root => do
    let dir := root / "slot"
    let commit ← makeSlot dir
    -- The agent rewrites the script in the tree, commits it on a branch, and points a
    -- replacement object at it from the base commit.
    IO.FS.writeFile (dir / ".orchestra" / "lib.sh") "echo AGENT\n"
    discard <| git dir #["checkout", "-q", "-b", "agent"]
    discard <| git dir #["commit", "-q", "-am", "mine"]
    let mine ← git dir #["rev-parse", "HEAD"]
    discard <| git dir #["replace", commit, mine]
    IO.FS.writeFile (dir / ".orchestra" / "validation.sh") "echo AGENT-UNCOMMITTED\n"
    let dest := root / "copy"
    let name ← Repo.snapshotScripts dir dest
    return (name, ← IO.FS.readFile (dest / ".orchestra" / "validation.sh"),
            ← IO.FS.readFile (dest / ".orchestra" / "lib.sh"))
  TestM.assertEqual name (some ".orchestra")
  TestM.assertEqual content "source \"$(dirname \"$0\")/lib.sh\"\n"
    (msg := "the committed validation.sh, not the tree's")
  TestM.assertEqual lib "echo BASE\n" (msg := "and not what a replacement object says")

@[test]
def aHookRunsTheDaemonsCopy : Test := do
  let (out, specs) ← withScratch "git-hook-copy" fun root => do
    let dir := root / "slot"
    discard <| makeSlot dir
    IO.FS.writeFile (dir / ".orchestra" / "lib.sh") "echo AGENT\n"
    let saved ← Orchestra.withTempDataNoDb root do
      let some scripts ← RepoConfig.Scripts.prepare dir "t-hook"
        | throw (.userError "no scripts")
      let seen ← IO.mkRef (#[] : Array Exec.ScriptSpec)
      let session : Exec.Session := {
        id := "test", mcpEndpoint := pure, describe := fun _ => pure ""
        start := fun _ => throw (.userError "no agent here")
        runScript := fun spec => do seen.modify (·.push spec); Exec.hostRunScript spec
        close := pure () }
      let (_, out) ← RepoConfig.runValidation session dir (some scripts)
      RepoConfig.Scripts.discard "t-hook"
      return (out, ← seen.get)
    return saved
  TestM.assertEqual out "BASE" (msg := "validation ran the default branch's scripts, lib.sh too")
  match specs[0]? with
  | some spec =>
    TestM.assert (spec.path.endsWith ".orchestra/validation.sh")
      "the backend is told the path the script has in the checkout"
    TestM.assert spec.trusted.isSome "and where the daemon's copy is"
  | none => TestM.fail "validation reached the session"

end OrchestraTest.RepoHardening
