import OrchestraTest.TestM

/-!
# Captured repository hooks

`RepoConfig.Hooks` exists to stop a script an agent chose from being run outside the sandbox. The
tests here pin that property against the three ways in that were confirmed against earlier versions
of it:

* rewriting the hook in the working tree during the task,
* planting an untracked hook that survives `Repo.resetSlot`'s `git clean -fd` into the *next* task,
* committing a hook that is a symlink into storage the sandbox can write.

They build real git repositories, because the capture reads the committed blobs (`git ls-tree` plus
`git cat-file`, from the base ref) and the difference between "committed", "present in the tree",
and "what the repository's own `.git` config would rewrite it to" is the whole point.

Scratch lives under `/var/tmp` rather than `/tmp`: `capture` refuses a destination under `/tmp`
(the sandbox is granted `--rw` there), and these tests exercise the real path rather than a
weakened one.
-/

namespace Orchestra

open Orchestra.RepoConfig

private def scratch (tag : String) : IO System.FilePath := do
  return System.FilePath.mk "/var/tmp" / s!"orchestra-hooks-{tag}-{← IO.monoNanosNow}"

private def run (cmd : String) (args : Array String) (cwd : System.FilePath) : IO Unit := do
  let child ← IO.Process.spawn {
    cmd, args, cwd, stdout := .null, stderr := .null
  }
  let _ ← child.wait

/-- A git repository with a committed initial file, ready to have hooks added. -/
private def gitRepo (root : System.FilePath) : IO Unit := do
  IO.FS.createDirAll root
  run "git" #["init", "-q"] root
  run "git" #["config", "user.email", "test@example.com"] root
  run "git" #["config", "user.name", "test"] root
  IO.FS.writeFile (root / "README") "x"
  run "git" #["add", "-A"] root
  run "git" #["commit", "-q", "-m", "init"] root

private def commitAll (root : System.FilePath) : IO Unit := do
  run "git" #["add", "-A"] root
  run "git" #["commit", "-q", "-m", "hooks"] root

/-- Write `.orchestra/<name>` so that running it writes `marker` into `out`. -/
private def writeHook (root : System.FilePath) (dir name marker : String)
    (out : System.FilePath) : IO Unit := do
  IO.FS.createDirAll (root / dir)
  IO.FS.writeFile (root / dir / name) s!"#!/usr/bin/env bash\necho -n '{marker}' > {out}\n"

private def readMarker (out : System.FilePath) : IO String := do
  if ← out.pathExists then IO.FS.readFile out else return ""

private def cleanup (root : System.FilePath) : IO Unit := do
  try IO.FS.removeDirAll root catch _ => pure ()

/-- The committed script runs, not the one the working tree holds by the time it is run.

    The same-task rewrite: `validation.sh` runs *after* the agent has had write access to the
    clone, so reading it live let the agent choose what the daemon executed. -/
@[test]
def committedHookWinsOverRewrittenOne : Test := do
  let root ← scratch "rewrite"
  let out := root / "marker"
  let result ← (do
    gitRepo (root / "repo")
    writeHook (root / "repo") ".orchestra" "validation.sh" "committed" out
    commitAll (root / "repo")
    let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "HEAD"
    -- The "agent" rewrites the hook in its clone, which it is granted write access to.
    writeHook (root / "repo") ".orchestra" "validation.sh" "agent-controlled" out
    let (ok, _) ← hooks.runValidation
    let marker ← readMarker out
    hooks.release
    return (ok, marker))
  cleanup root
  TestM.assertEqual result.2 "committed"
    "validation.sh ran the working tree's copy, so an agent still chooses what runs unsandboxed"
  TestM.assert result.1 "the captured validation.sh should have exited 0"

/-- An untracked hook planted in the working tree is not captured, even though it is sitting
    exactly where the old code looked.

    This is the cross-task escape. `Repo.resetSlot` runs `git clean -fd` *without* `-x`, so a file
    an agent has arranged to be ignored survives a slot reset into the next task — where the
    capture happens before that task's agent starts and so looks clean by construction. Reading
    `git archive HEAD` instead of the filesystem is what closes it. -/
@[test]
def untrackedPlantedHookIsNotCaptured : Test := do
  let root ← scratch "planted"
  let out := root / "marker"
  let (hasVal, marker) ← (do
    gitRepo (root / "repo")
    -- The repository commits no hook directory at all.
    -- A previous task's agent plants one and hides it from `git clean -fd`.
    writeHook (root / "repo") ".orchestra" "validation.sh" "ESCAPED" out
    IO.FS.writeFile ((root / "repo") / ".git" / "info" / "exclude") ".orchestra/\n"
    let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "HEAD"
    let h ← hooks.hasValidation
    let (_, _) ← hooks.runValidation
    let m ← readMarker out
    hooks.release
    return (h, m))
  cleanup root
  TestM.assert (!hasVal) "an untracked planted validation.sh was captured"
  TestM.assertEqual marker "" "an untracked planted validation.sh was executed unsandboxed"

/-- A committed hook that is a symlink is dropped rather than followed.

    A captured `validation.sh -> /tmp/x.sh` would point straight back into storage the sandbox is
    granted `--rw`, so the capture would hand the agent the same choice by another route. -/
@[test]
def symlinkedHookIsNotExecuted : Test := do
  let root ← scratch "symlink"
  let out := root / "marker"
  let target := root / "target.sh"
  let (hasVal, marker) ← (do
    gitRepo (root / "repo")
    IO.FS.writeFile target s!"#!/usr/bin/env bash\necho -n 'VIA-SYMLINK' > {out}\n"
    IO.FS.createDirAll ((root / "repo") / ".orchestra")
    run "ln" #["-s", target.toString, "validation.sh"] ((root / "repo") / ".orchestra")
    commitAll (root / "repo")
    let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "HEAD"
    let h ← hooks.hasValidation
    let (_, _) ← hooks.runValidation
    let m ← readMarker out
    hooks.release
    return (h, m))
  cleanup root
  TestM.assert (!hasVal) "a symlinked validation.sh survived the capture"
  TestM.assertEqual marker "" "a symlinked validation.sh was executed"

/-- A `.orchestra` that is itself a symlink does not pull the target's contents into the capture.

    Followed, this is an exfiltration primitive rather than an execution one: pointing it at the
    config directory would copy the App private key and `secrets.json` somewhere the sandbox can
    read on a non-XDG install. -/
@[test]
def symlinkedHookDirDoesNotExfiltrate : Test := do
  let root ← scratch "exfil"
  let secrets := root / "secrets"
  let leaked ← (do
    IO.FS.createDirAll secrets
    IO.FS.writeFile (secrets / "key.pem") "PRIVATE-KEY-MATERIAL"
    gitRepo (root / "repo")
    run "ln" #["-s", secrets.toString, ".orchestra"] (root / "repo")
    commitAll (root / "repo")
    let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "HEAD"
    let l ← ((root / "capture") / "key.pem").pathExists
    hooks.release
    return l)
  cleanup root
  TestM.assert (!leaked) "a symlinked .orchestra copied the link target into the capture"

/-- `capture` refuses a destination the sandbox can write to, rather than silently producing a
    copy with no security value. -/
@[test]
def captureRefusesAgentWritableDestination : Test := do
  let root ← scratch "refuse"
  let refused ← (do
    gitRepo (root / "repo")
    try
      let _ ← RepoConfig.capture (root / "repo") (System.FilePath.mk "/tmp/orchestra-hooks-bad") "HEAD"
      return false
    catch _ => return true)
  cleanup root
  TestM.assert refused "capture accepted a destination under /tmp, which the sandbox can write"

/-- `config.json` is captured too, so `retry_prompt` — which is interpolated into the agent's next
    prompt — cannot be rewritten by the agent whose retry it governs. -/
@[test]
def configIsCapturedNotReadLive : Test := do
  let root ← scratch "config"
  let cfg ← (do
    gitRepo (root / "repo")
    IO.FS.createDirAll ((root / "repo") / ".orchestra")
    IO.FS.writeFile ((root / "repo") / ".orchestra" / "config.json")
      "{\"validation\": {\"retry_prompt\": \"committed\", \"max_retries\": 2}}"
    commitAll (root / "repo")
    let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "HEAD"
    IO.FS.writeFile ((root / "repo") / ".orchestra" / "config.json")
      "{\"validation\": {\"retry_prompt\": \"agent-controlled\", \"max_retries\": 99}}"
    let c ← hooks.config
    hooks.release
    return c)
  cleanup root
  TestM.assertEqual cfg.validation.retryPrompt "committed"
    "retry_prompt was read live, so an agent can write its own retry instructions"
  TestM.assertEqual cfg.validation.maxRetries 2 "max_retries was read live"

/-- A repository with no committed hook directory captures to a `Hooks` that runs nothing — the
    common case, and it must not throw. -/
@[test]
def repoWithoutHooksIsInert : Test := do
  let root ← scratch "empty"
  let (hasVal, ok) ← (do
    gitRepo (root / "repo")
    let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "HEAD"
    hooks.run "before.sh"
    hooks.runInitIfNeeded
    let h ← hooks.hasValidation
    let (ok, _) ← hooks.runValidation
    hooks.release
    return (h, ok))
  cleanup root
  TestM.assert (!hasVal) "a repository with no hook directory reported a validation script"
  TestM.assert ok "a repository with no validation script should validate as passing"

/-- A `.orchestra` committed as a regular file is treated as "no hooks" rather than failing the
    task. It was a silent no-op before the capture existed, and must stay one. -/
@[test]
def hookDirAsRegularFileIsInert : Test := do
  let root ← scratch "regular"
  let ok ← (do
    gitRepo (root / "repo")
    IO.FS.writeFile ((root / "repo") / ".orchestra") "not a directory"
    commitAll (root / "repo")
    try
      let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "HEAD"
      let h ← hooks.hasValidation
      hooks.release
      return !h
    catch _ => return false)
  cleanup root
  TestM.assert ok "a .orchestra committed as a regular file broke the task"

/-- The legacy `.agent/` directory is captured the same way `.orchestra/` is. -/
@[test]
def legacyAgentDirIsCaptured : Test := do
  let root ← scratch "legacy"
  let out := root / "marker"
  let marker ← (do
    gitRepo (root / "repo")
    writeHook (root / "repo") ".agent" "validation.sh" "legacy" out
    commitAll (root / "repo")
    let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "HEAD"
    let _ ← hooks.runValidation
    let m ← readMarker out
    hooks.release
    return m)
  cleanup root
  TestM.assertEqual marker "legacy" "a repository using the legacy .agent/ directory was skipped"

/-- A failing validation script reports its output rather than throwing. -/
@[test]
def failingValidationIsReported : Test := do
  let root ← scratch "failing"
  let (ok, output) ← (do
    gitRepo (root / "repo")
    IO.FS.createDirAll ((root / "repo") / ".orchestra")
    IO.FS.writeFile ((root / "repo") / ".orchestra" / "validation.sh")
      "#!/usr/bin/env bash\necho 'boom'\nexit 3\n"
    commitAll (root / "repo")
    let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "HEAD"
    let r ← hooks.runValidation
    hooks.release
    return r)
  cleanup root
  TestM.assert (!ok) "a validation script exiting 3 was reported as passing"
  TestM.assert (output.trimAscii.toString == "boom") s!"unexpected validation output: {output}"

/-- `release` removes the captured copy, so a long-lived daemon does not accumulate one per task. -/
@[test]
def releaseRemovesTheCapture : Test := do
  let root ← scratch "release"
  let stillThere ← (do
    gitRepo (root / "repo")
    writeHook (root / "repo") ".orchestra" "before.sh" "x" (root / "unused")
    commitAll (root / "repo")
    let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "HEAD"
    hooks.release
    ((root / "capture")).pathExists)
  cleanup root
  TestM.assert (!stillThere) "release left the captured hook directory behind"

/-- A smudge filter configured in the repository's own `.git` does not reach the captured script,
    and is never executed.

    `git archive` applies working-tree conversion, so `filter.<n>.smudge` in `.git/config` plus one
    line in `.git/info/attributes` both substitutes the archived hook and runs a command as the
    daemon while doing it. Neither file is touched by `Repo.resetSlot`, so one write poisons a slot
    permanently. `ls-tree`/`cat-file` apply no filter.

    The filter here *substitutes the script's content* rather than only causing a side effect: a
    filter whose effect the real hook then overwrites is a test that cannot fail. `sentinel`
    separately catches the filter command running at all. -/
@[test]
def smudgeFilterDoesNotReachTheCapture : Test := do
  let root ← scratch "smudge"
  let out := root / "marker"
  let sentinel := root / "smudge-ran"
  let (marker, ran) ← (do
    gitRepo (root / "repo")
    writeHook (root / "repo") ".orchestra" "validation.sh" "committed" out
    commitAll (root / "repo")
    run "git" #["config", "filter.evil.smudge",
      s!"touch {sentinel}; sed s/committed/FILTERED/"] (root / "repo")
    IO.FS.writeFile ((root / "repo") / ".git" / "info" / "attributes")
      ".orchestra/validation.sh filter=evil\n"
    let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "HEAD"
    let (_, _) ← hooks.runValidation
    let m ← readMarker out
    let r ← sentinel.pathExists
    hooks.release
    return (m, r))
  cleanup root
  TestM.assertEqual marker "committed"
    "a smudge filter from .git/config decided what the captured hook contained"
  TestM.assert (!ran) "a smudge filter command from .git/config ran as the daemon at capture time"

/-- `export-ignore` in the repository's own `.git` does not make a committed hook vanish.

    It is the quieter half of the same channel: one line, no config, no execution — and because a
    repository with no `validation.sh` validates as passing, deleting the hook turns a failing test
    suite into a green one. -/
@[test]
def exportIgnoreDoesNotHideAHook : Test := do
  let root ← scratch "exportignore"
  let hasVal ← (do
    gitRepo (root / "repo")
    IO.FS.createDirAll ((root / "repo") / ".orchestra")
    IO.FS.writeFile ((root / "repo") / ".orchestra" / "validation.sh")
      "#!/usr/bin/env bash\nexit 1\n"
    commitAll (root / "repo")
    IO.FS.writeFile ((root / "repo") / ".git" / "info" / "attributes")
      ".orchestra/validation.sh export-ignore\n"
    let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "HEAD"
    let h ← hooks.hasValidation
    hooks.release
    return h)
  cleanup root
  TestM.assert hasVal "export-ignore hid a committed validation.sh, so validation passed vacuously"

/-- Capturing from the base ref ignores what the task's own commits did to the hooks.

    A continuation task keeps its predecessor's tree, so `HEAD` is the previous agent's commit;
    committing a rewritten hook is an entirely ordinary thing for a task to do. -/
@[test]
def baseRefIgnoresCommittedHookRewrite : Test := do
  let root ← scratch "baseref"
  let out := root / "marker"
  let marker ← (do
    gitRepo (root / "repo")
    writeHook (root / "repo") ".orchestra" "validation.sh" "base" out
    commitAll (root / "repo")
    run "git" #["branch", "base"] (root / "repo")
    -- The "agent" commits a rewritten hook on top.
    writeHook (root / "repo") ".orchestra" "validation.sh" "agent-committed" out
    commitAll (root / "repo")
    let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "base"
    let (_, _) ← hooks.runValidation
    let m ← readMarker out
    hooks.release
    return m)
  cleanup root
  TestM.assertEqual marker "base"
    "the capture followed HEAD, so a task that commits a hook rewrite chooses what runs"

/-- A git failure throws rather than reporting "this repository ships no hooks", which would make
    `runValidation` report success. -/
@[test]
def unreadableRefFailsLoudly : Test := do
  let root ← scratch "badref"
  let threw ← (do
    gitRepo (root / "repo")
    try
      let _ ← RepoConfig.capture (root / "repo") (root / "capture") "no-such-ref"
      return false
    catch _ => return true)
  cleanup root
  TestM.assert threw "an unreadable ref was reported as 'no hooks', which validates as passing"

/-- A capture abandoned by a run that threw is reclaimed by a later one; a live one is not. -/
@[test]
def staleCapturesAreSwept : Test := do
  let root ← scratch "sweep"
  let (staleGone, freshKept) ← (do
    gitRepo (root / "repo")
    let hooksRoot := root / "captures"
    IO.FS.createDirAll (hooksRoot / "abandoned")
    IO.FS.createDirAll (hooksRoot / "recent")
    -- Two days old, and current.
    run "touch" #["-d", "2 days ago", (hooksRoot / "abandoned").toString] root
    let hooks ← RepoConfig.capture (root / "repo") (hooksRoot / "live") "HEAD"
    let staleGone := !(← (hooksRoot / "abandoned").pathExists)
    let freshKept ← (hooksRoot / "recent").pathExists
    hooks.release
    return (staleGone, freshKept))
  cleanup root
  TestM.assert staleGone "a capture abandoned two days ago was not reclaimed"
  TestM.assert freshKept "the sweep removed a capture that was not stale"

/-- A repository with no committed hooks leaves no capture directory behind. -/
@[test]
def hooklessRepoLeavesNothingBehind : Test := do
  let root ← scratch "noleak"
  let leftBehind ← (do
    gitRepo (root / "repo")
    let hooks ← RepoConfig.capture (root / "repo") (root / "capture") "HEAD"
    hooks.release
    (root / "capture").pathExists)
  cleanup root
  TestM.assert (!leftBehind) "a hook-less repository left an empty capture directory behind"

end Orchestra
