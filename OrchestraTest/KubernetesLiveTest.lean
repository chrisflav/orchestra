import OrchestraTest.TestM
import Orchestra

open Lean (Json)
open Orchestra
open Orchestra.Exec

/-!
# The Kubernetes backend, against a real cluster

`OrchestraTest/KubernetesTest.lean` checks what a pod spec *says*, which is most of what can be
checked without somewhere to send it. This file checks what a pod *does*: that a session opens,
that the checkout arrives, that work done in the pod comes back, and that the ways a task can end
badly — cancelled, or a pod that disappeared underneath it — leave the daemon's disk in the state
the code claims they do.

None of that can be faked. A double would answer the questions this asks in whatever way the
implementation already believes, which is exactly the belief being tested.

So these opt in the same way the taxis-backed tests do, and skip themselves — not fail — when the
environment is not set, so the suite stays green on a machine with no cluster:

```sh
ORCHESTRA_TEST_K8S_IMAGE=debian:stable-slim lake test
```

The image needs `sh`, `bash` and `tar`; that is all the session lifecycle uses. `git` and `nc` are
the agent's own requirements and no test here launches an agent — that would need the pod to reach
the daemon's MCP server, which is a routing question about a particular cluster rather than
anything this code decides.

`ORCHESTRA_TEST_K8S_NAMESPACE` picks the namespace (default `orchestra-runners`), and
`ORCHESTRA_TEST_K8S_KUBECTL` the binary, for a `kubectl` that is not on `PATH`.

Every test deletes its pod, through `close` where that is what is being tested and directly where
it is not.
-/

namespace OrchestraTest.KubernetesLive

open Orchestra.Exec.Kubernetes

/-- The backend under test, or `none` when this machine was not given a cluster to use. -/
private def liveBackend? (extra : List (String × Json) := []) : IO (Option (Backend × Config)) := do
  let some image ← IO.getEnv "ORCHESTRA_TEST_K8S_IMAGE" | return none
  let ns := (← IO.getEnv "ORCHESTRA_TEST_K8S_NAMESPACE").getD "orchestra-runners"
  let kubectl := (← IO.getEnv "ORCHESTRA_TEST_K8S_KUBECTL").getD "kubectl"
  let options := Json.mkObj ([
    ("image", .str image),
    ("namespace", .str ns),
    ("kubectl", .str kubectl),
    -- Never reached: nothing here launches an agent. Required, so it is supplied.
    ("mcp_host", .str "10.0.100.1"),
    ("startup_timeout_seconds", .num 300)] ++ extra)
  match Config.fromJson options with
  | .error e => throw (IO.userError s!"the live test options were rejected: {e}")
  | .ok cfg  =>
    match Kubernetes.factory.make options with
    | .error e => throw (IO.userError s!"the live backend could not be built: {e}")
    | .ok b    => return some (b, cfg)

/-- A scratch checkout on the daemon's disk, with the files a test wants in it. -/
private def makeCheckout (name : String) (files : List (String × String)) : IO System.FilePath := do
  let root := System.FilePath.mk s!"/tmp/orchestra-k8s-live-{name}-{← Exec.randomHex 6}"
  for (rel, contents) in files do
    let path := root / rel
    if let some parent := path.parent then IO.FS.createDirAll parent
    IO.FS.writeFile path contents
  return root

private def cleanup (root : System.FilePath) : IO Unit := do
  try IO.FS.removeDirAll root catch _ => pure ()

/-- Run one of the repository's scripts, captured.

    The script has to be in the checkout *before* the session is opened, because that is when the
    checkout is copied into the pod — which is also the honest shape of the thing being modelled:
    `init.sh`, `before.sh` and `validation.sh` are files in the repository, present before any of
    this starts. A helper that wrote the script as it went would be testing a staging path that
    does not exist. -/
private def runStep (session : Session) (root : System.FilePath) (script : String)
    : IO ScriptResult :=
  session.runScript { path := script, workdir := root, stdio := .piped }

/-! ## The session lifecycle -/

@[test]
def aTaskGetsAPodAndItsWorkComesBack : Test := do
  let some (backend, _) ← liveBackend? | TestM.skip "no ORCHESTRA_TEST_K8S_IMAGE"; return ()
  match ← backend.preflight with
  | .error e => TestM.fail s!"preflight failed: {e}"
  | .ok () =>
  let root ← makeCheckout "roundtrip"
    [("kept.txt", "from the daemon\n"),
     ("doomed.txt", "the agent deletes this\n"),
     ("check.sh", "cat kept.txt\npwd\n"),
     ("work.sh", "echo 'written in the pod' > made.txt\nrm doomed.txt\n\
echo 'appended' >> kept.txt\n")]
  let session ← backend.openSession {
    workdir := root
    grants  := #[{ path := root.toString, access := .rwx, required := true, from_ := .orchestra }]
    label   := "live-roundtrip" }
  TestM.assert (AgentDef.containsCI session.id "pod") "the session names the pod it opened"
  -- The checkout arrived, and it is the same path it has on the daemon — which is what keeps every
  -- log line and prompt that mentions it true.
  let arrived ← runStep session root "check.sh"
  TestM.assertEqual arrived.exitCode 0 (msg := s!"the script ran: {arrived.output}")
  TestM.assert (AgentDef.containsCI arrived.output "from the daemon")
    "the checkout was carried into the pod"
  TestM.assert (AgentDef.containsCI arrived.output root.toString)
    "and is mounted at the path the daemon has it at"
  -- Work done in the pod, of both kinds a swap has to get right.
  let worked ← runStep session root "work.sh"
  TestM.assertEqual worked.exitCode 0 (msg := s!"the work script ran: {worked.output}")
  session.close
  -- A file created in the pod is here; a file deleted there is gone here. The second is the one a
  -- `tar` extracted over the old tree would get wrong, and the reason the checkout is swapped.
  TestM.assert (← (root / "made.txt").pathExists) "a file the agent created came back"
  TestM.assert (!(← (root / "doomed.txt").pathExists)) "a file the agent deleted is gone"
  let kept ← IO.FS.readFile (root / "kept.txt")
  TestM.assert (AgentDef.containsCI kept "appended") "and an edit to an existing file came back"
  cleanup root

@[test]
def theSessionSaysItIsFreshAndCannotResume : Test := do
  let some (backend, _) ← liveBackend? | TestM.skip "no ORCHESTRA_TEST_K8S_IMAGE"; return ()
  let root ← makeCheckout "flags" [("a.txt", "a\n")]
  let session ← backend.openSession {
    workdir := root
    grants  := #[{ path := root.toString, access := .rwx, required := true, from_ := .orchestra }]
    label   := "live-flags" }
  -- Both are what the task runner reads before it decides to run `init.sh` again, or to refuse a
  -- continuation. Wrong here and the failure is a toolchain that is never installed, or a
  -- follow-up prompt answered by a model that never saw what it follows.
  TestM.assert session.freshEnvironment "a pod per task is a fresh environment every task"
  TestM.assert (!session.carriesAgentState)
    "and with no home_claim it cannot hold a conversation for the next one"
  session.close
  cleanup root

/-! ## What `excludes` means in each direction -/

@[test]
def anExcludedPathIsNotCarriedAndNotDestroyed : Test := do
  let some (backend, _) ← liveBackend? [("excludes", .arr #[.str ".lake"])]
    | TestM.skip "no ORCHESTRA_TEST_K8S_IMAGE"; return ()
  let root ← makeCheckout "excludes"
    [("src.txt", "source\n"),
     (".lake/build-cache", "warmed by orchestra prepare\n"),
     ("probe.sh", "test -e .lake && echo LAKE-PRESENT || echo LAKE-ABSENT\n"),
     ("work.sh", "echo 'agent output' > result.txt\n")]
  let session ← backend.openSession {
    workdir := root
    grants  := #[{ path := root.toString, access := .rwx, required := true, from_ := .orchestra }]
    label   := "live-excludes" }
  -- Not carried in: that is the transfer the setting exists to avoid.
  let inPod ← runStep session root "probe.sh"
  TestM.assert (AgentDef.containsCI inPod.output "LAKE-ABSENT")
    s!"the excluded path was not copied into the pod: {inPod.output}"
  let worked ← runStep session root "work.sh"
  TestM.assertEqual worked.exitCode 0 (msg := s!"the work script ran: {worked.output}")
  session.close
  -- And not destroyed on the way back. The checkout is replaced wholesale, so an excluded path
  -- absent from the incoming tree would be deleted here — which would mean configuring `excludes`
  -- for build output wiped the build output `orchestra prepare` exists to warm.
  TestM.assert (← (root / ".lake" / "build-cache").pathExists)
    "the daemon's copy of the excluded path survived the swap"
  -- Read defensively: when this regresses the file is gone, and the failure should read as the
  -- assertion it is rather than as an uncaught exception three lines later.
  let cache ← try IO.FS.readFile (root / ".lake" / "build-cache") catch _ => pure ""
  TestM.assert (AgentDef.containsCI cache "warmed by orchestra prepare")
    "with its contents intact"
  TestM.assert (← (root / "result.txt").pathExists) "and the rest of the checkout still came back"
  cleanup root

/-! ## Memory directories -/

@[test]
def memoriesAreMergedRatherThanSwapped : Test := do
  let some (backend, _) ← liveBackend? | TestM.skip "no ORCHESTRA_TEST_K8S_IMAGE"; return ()
  let mem ← makeCheckout "mem-dir" [("existing.md", "written by an earlier task\n")]
  let root ← makeCheckout "mem-checkout"
    [("src.txt", "source\n"),
     ("remember.sh", s!"echo 'learned this run' > {mem}/new.md\n")]
  let session ← backend.openSession {
    workdir := root
    grants  := #[{ path := root.toString, access := .rwx, required := true, from_ := .orchestra },
                 { path := mem.toString, access := .rw, from_ := .orchestra }]
    label   := "live-memory" }
  let worked ← runStep session root "remember.sh"
  TestM.assertEqual worked.exitCode 0 (msg := s!"the memory write ran: {worked.output}")
  session.close
  TestM.assert (← (mem / "new.md").pathExists) "a memory written in the pod came back"
  -- Merged, not swapped: another task may hold a copy of this directory at the same time, so what
  -- was already here has to survive a merge from a pod that never saw the newest of it.
  TestM.assert (← (mem / "existing.md").pathExists) "and what was already here was not swapped away"
  cleanup root
  cleanup mem

/-! ## Carrying the agent's own configuration across -/

@[test]
def provideCarriesAFileWrittenAfterTheSessionOpened : Test := do
  let some (backend, _) ← liveBackend? | TestM.skip "no ORCHESTRA_TEST_K8S_IMAGE"; return ()
  -- The shape of the agent's MCP configuration: a single file, in a directory the image had no
  -- reason to create, written to the daemon's disk *after* the session opened — because it holds
  -- an address that needs a server that is started after the session.
  let cfgPath := System.FilePath.mk
    s!"/tmp/orchestra-live-mcp-{← Exec.randomHex 6}/agent-mcp.json"
  let root ← makeCheckout "provide"
    [("src.txt", "source\n"), ("read-config.sh", s!"cat {cfgPath}\n")]
  let session ← backend.openSession {
    workdir := root
    grants  := #[{ path := root.toString, access := .rwx, required := true, from_ := .orchestra }]
    label   := "live-provide" }
  -- Written only now: before this point there is nothing to carry, which is the whole reason
  -- `provide` exists separately from what `openSession` stages.
  if let some parent := cfgPath.parent then IO.FS.createDirAll parent
  IO.FS.writeFile cfgPath "{\"mcpServers\":{\"agent\":{\"command\":\"nc\"}}}"
  session.provide #[{ path := cfgPath.toString, access := .ro, from_ := .orchestra }]
  let seen ← runStep session root "read-config.sh"
  TestM.assertEqual seen.exitCode 0
    (msg := s!"the config the agent is pointed at exists in the pod: {seen.output}")
  TestM.assert (AgentDef.containsCI seen.output "mcpServers")
    "and it is the file that was written here"
  session.close
  try IO.FS.removeFile cfgPath catch _ => pure ()
  cleanup root

/-! ## The ways a task ends badly -/

@[test]
def cancellingTheAgentLeavesThePodUsable : Test := do
  let some (backend, cfg) ← liveBackend? | TestM.skip "no ORCHESTRA_TEST_K8S_IMAGE"; return ()
  let root ← makeCheckout "cancel"
    [("src.txt", "source\n"), ("after.sh", "echo 'after.sh ran' > after.txt\n")]
  let session ← backend.openSession {
    workdir := root
    grants  := #[{ path := root.toString, access := .rwx, required := true, from_ := .orchestra }]
    label   := "live-cancel" }
  -- Stands in for the agent: something long-running that a cancellation interrupts.
  let handle ← session.start {
    command := "sleep", args := #["600"], workdir := root, stdio := .piped
    label := "live-cancel-agent" }
  handle.kill
  let _ ← handle.wait
  -- Cancelling ends the run, not the environment. `after.sh` still has to run in the pod, the
  -- checkout still has to come back, and the task still has to record that it was cancelled —
  -- all of which are more `kubectl exec`s into a pod that has to still be there.
  let after ← runStep session root "after.sh"
  TestM.assertEqual after.exitCode 0
    (msg := s!"a hook still runs after a cancellation: {after.output}")
  session.close
  TestM.assert (← (root / "after.txt").pathExists)
    "and the work done after the cancellation came back"
  -- Closing is what takes the pod down — and only closing. Deletion is asked for rather than
  -- waited on (`--wait=false`), so the pod is either already gone or carries a deletion timestamp;
  -- what would be wrong is a pod still running with nothing having asked it to stop.
  let out ← IO.Process.output {
    cmd := cfg.kubectl,
    args := #["-n", cfg.ns, "get", "pods", "-l", "orchestra.dev/task=live-cancel",
              "--ignore-not-found", "-o",
              "jsonpath={range .items[*]}{.metadata.name}={.metadata.deletionTimestamp}{end}"] }
  let listed := out.stdout.trimAscii.toString
  TestM.assert (listed.isEmpty || !(AgentDef.containsCI listed "=<no value>"))
    s!"close asked the cluster to delete the pod: {listed}"
  cleanup root

@[test]
def closingOnAPodThatIsAlreadyGoneSaysSoRatherThanThrowing : Test := do
  let some (backend, cfg) ← liveBackend? | TestM.skip "no ORCHESTRA_TEST_K8S_IMAGE"; return ()
  let root ← makeCheckout "evicted" [("src.txt", "source\n")]
  let session ← backend.openSession {
    workdir := root
    grants  := #[{ path := root.toString, access := .rwx, required := true, from_ := .orchestra }]
    label   := "live-evicted" }
  -- What an eviction, a node going away, or `deadline_seconds` expiring looks like from here.
  let podName := session.id.splitOn " " |>.getD 1 ""
  let name := (podName.splitOn "/").getD 1 podName
  let _ ← IO.Process.output {
    cmd := cfg.kubectl,
    args := #["-n", cfg.ns, "delete", "pod", name, "--now", "--wait=true"] }
  -- Reported, not thrown: the task runner calls this in a `finally`, and a throw here would
  -- replace the reason the task actually failed with this one.
  session.close
  TestM.assert (← root.pathExists) "the daemon's checkout is left as it was"
  cleanup root

end OrchestraTest.KubernetesLive
