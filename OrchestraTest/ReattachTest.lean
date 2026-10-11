import OrchestraTest.TestM
import Orchestra

open Lean (Json)
open Orchestra
open Orchestra.Exec

/-!
# Tasks that survive a daemon restart

Three halves, tested as far as each can be without a cluster.

**The run helper** (`Exec.Runner`) is a shell script, so it is run: started commands, their exit
statuses, following a stream from an offset, stdin through the FIFO, cancellation, and — the point
of all of it — a run that keeps going when whatever was following it goes away, and is followed
again from where it was. The daemon-side follower is run against a stand-in for `kubectl` that drops
its first connection, and the agent's MCP relay against a real MCP server that is stopped and
started again on the same port.

**What is recorded** (`RunState`, the kubernetes `Snapshot`) round-trips, and anything a build
cannot fully read is refused rather than half-used.

**What is decided** — which running entries a starting daemon carries on
(`Queue.adoptionDecision`), when a stopping one may hand over (`Exec.HandOffState`), where a
picked-up loop resumes (`TaskRunner.resumeAttempt`) — is pure, and pinned case by case.
-/

namespace OrchestraTest.Reattach

open Orchestra.Exec.Runner

private def withTempData (act : IO α) : IO α := Orchestra.withTempData "reattach" act

/-- Whether `x` succeeded with exactly `v`. -/
private def okIs [BEq α] (x : Except String α) (v : α) : Bool := x.toOption == some v

/-! ## The run helper, run -/

/-- The run helper written into a fresh directory, as it is staged into a pod. -/
private def stageRunner : IO String := do
  let dir ← IO.FS.createTempDir
  let path := dir / s!"run-{protocol}"
  IO.FS.writeFile path runnerScript
  return path.toString

private def runner (r : String) (args : Array String) : IO (UInt32 × String × String) := do
  let out ← IO.Process.output { cmd := "/bin/sh", args := #[r] ++ args }
  return (out.exitCode, out.stdout, out.stderr)

@[test]
def theRunHelperStartsARunOnceAndReportsHowItEnded : Test := do
  let r ← stageRunner
  let (code, said, _) ← runner r (startArgs "a" #["sh", "-c",
    "echo one; echo oops >&2; sleep 1; echo two; exit 7"])
  TestM.assertEqual code 0 (msg := "start returns at once")
  TestM.assert (parseStartReply said == some .started) s!"and says it started ({said})"
  let (code, out, _) ← runner r (attachArgs "a" "out")
  TestM.assertEqual code 7 (msg := "following a run ends with the run's own exit status")
  TestM.assertEqual out "one\ntwo\n" (msg := "after all of its stdout")
  let (_, err, _) ← runner r (attachArgs "a" "err")
  TestM.assertEqual err "oops\n" (msg := "stderr is a stream of its own")
  let (_, rest, _) ← runner r (attachArgs "a" "out" 4)
  TestM.assertEqual rest "two\n" (msg := "and a stream can be followed from a byte offset")
  let (_, st, _) ← runner r #["status", "a"]
  TestM.assert (parseStatus st == some (.exited 7 8 5)) s!"status knows how it ended ({st})"
  let (code, again, _) ← runner r (startArgs "a" #["sh", "-c", "echo second"])
  TestM.assertEqual code 0
  TestM.assert (parseStartReply again == some (.existing 8))
    s!"starting the same id again starts nothing, and says how much is there ({again})"
  let (_, unknown, _) ← runner r #["status", "nope"]
  TestM.assert (parseStatus unknown == some .unknown) "an id never started is unknown"
  let (code, _, _) ← runner r (startArgs "../escape" #["true"])
  TestM.assert (code != 0) "and an id that is a path is refused"

@[test]
def aRunOutlivesWhateverWasFollowingIt : Test := do
  -- The daemon's side of a run is a follower; the daemon going away is that follower dying. The
  -- run must not notice, and a new follower must see all of it.
  let r ← stageRunner
  let _ ← runner r (startArgs "slow" #["sh", "-c",
    "for i in 1 2 3 4 5; do echo line$i; sleep 0.3; done; exit 3"])
  let follower ← IO.Process.spawn {
    cmd := "/bin/sh", args := #[r] ++ attachArgs "slow" "out"
    stdin := .null, stdout := .piped, stderr := .null }
  let first ← follower.stdout.getLine
  follower.kill
  discard follower.wait
  TestM.assertEqual first "line1\n" (msg := "the first follower saw the start")
  let (_, st, _) ← runner r #["status", "slow"]
  TestM.assert (match parseStatus st with | some (.running _ _) => true | _ => false)
    s!"the run is still going after its follower died ({st})"
  let (code, all, _) ← runner r (attachArgs "slow" "out")
  TestM.assertEqual code 3 (msg := "a new follower gets the run's exit status")
  TestM.assertEqual all "line1\nline2\nline3\nline4\nline5\n"
    (msg := "and every line, from the beginning")
  let (_, rest, _) ← runner r (attachArgs "slow" "out" first.utf8ByteSize)
  TestM.assertEqual rest "line2\nline3\nline4\nline5\n"
    (msg := "or from where the first one stopped")

@[test]
def aRunsStdinIsHeldOpenUntilItIsClosedOnPurpose : Test := do
  let r ← stageRunner
  let _ ← runner r (startArgs "talk" (stdin := true) #["sh", "-c",
    "while read l; do echo \"got $l\"; done; echo eof"])
  -- Two writers, one after the other: the first one going away is not an EOF, which is what keeps
  -- a daemon restart from ending a conversation.
  for line in ["hello", "world"] do
    let w ← IO.Process.spawn {
      cmd := "/bin/sh", args := #[r, "write", "talk"], stdin := .piped, stdout := .null }
    let (stdin, w) ← w.takeStdin
    stdin.putStrLn line
    stdin.flush
    let _ := stdin
    discard w.wait
  IO.sleep 300
  let (_, st, _) ← runner r #["status", "talk"]
  TestM.assert (match parseStatus st with | some (.running _ _) => true | _ => false)
    s!"a writer leaving does not end the run ({st})"
  let _ ← runner r #["close-stdin", "talk"]
  let (code, out, _) ← runner r (attachArgs "talk" "out")
  TestM.assertEqual code 0
  TestM.assertEqual out "got hello\ngot world\neof\n" (msg := "close-stdin is the EOF")

@[test]
def killingARunStopsItsWholeProcessGroup : Test := do
  let r ← stageRunner
  -- Ignores SIGTERM, and has a child of its own: both have to go.
  let _ ← runner r (startArgs "stubborn" #["sh", "-c", "trap '' TERM; sleep 300 & wait; sleep 300"])
  IO.sleep 300
  let (_, st, _) ← runner r #["status", "stubborn"]
  TestM.assert (match parseStatus st with | some (.running _ _) => true | _ => false) "it runs"
  let started ← IO.monoMsNow
  let _ ← runner r #["kill", "stubborn", "1"]
  let took := (← IO.monoMsNow) - started
  let (_, st, _) ← runner r #["status", "stubborn"]
  TestM.assert (match parseStatus st with | some (.exited _ _ _) => true | _ => false)
    s!"after kill the run has an exit status on disk ({st})"
  TestM.assert (took < 10000) s!"and kill escalated rather than waiting forever ({took}ms)"
  let (code, _, _) ← runner r (attachArgs "stubborn" "out")
  TestM.assert (code != 0) "and following it ends at once, with a failure"

@[test]
def theFollowerPicksADroppedConnectionUpWhereItStopped : Test := do
  let r ← stageRunner
  let dir := (System.FilePath.mk r).parent.getD "."
  -- A `kubectl` that runs the command here, and drops the first `attach` after a moment — an API
  -- server cutting an idle stream, or the daemon's own connection dying.
  let fake := dir / "kubectl"
  IO.FS.writeFile fake s!"#!/bin/sh
while [ \"$1\" != \"--\" ]; do shift; done; shift
if [ \"$3\" = attach ] && [ ! -f {dir}/dropped ]; then
  : > {dir}/dropped
  \"$@\" & p=$!; sleep 0.7; kill $p; wait $p; exit 1
fi
exec \"$@\"
"
  let _ ← IO.Process.output { cmd := "chmod", args := #["+x", fake.toString] }
  let _ ← runner r (startArgs "x" #["sh", "-c",
    "for i in 1 2 3 4 5 6; do echo line$i; sleep 0.3; done; exit 5"])
  let out ← IO.Process.output {
    cmd := "/bin/sh", args := followArgs fake.toString "ns" "pod" r "x" "out" }
  TestM.assert (← (dir / "dropped").pathExists) "the first connection was dropped"
  TestM.assertEqual out.stdout "line1\nline2\nline3\nline4\nline5\nline6\n"
    (msg := "and the stream still arrived whole, nothing twice and nothing missing")
  TestM.assertEqual out.exitCode 5 (msg := "ending with the run's exit status")
  let gone ← IO.Process.output {
    cmd := "/bin/sh", args := followArgs fake.toString "ns" "pod" r "never-started" "out" }
  TestM.assertEqual gone.exitCode 255 (msg := "a run that is not there is given up on")

/-! ## The agent's MCP relay, across a server restart -/

private def testServerState (token : Option String) : Server.State :=
  { repo := some { upstream := { owner := "acme", name := "widgets" }
                 , fork := { owner := "acme", name := "widgets" } }
    allowedTools := []
    appId := 0, privateKeyPath := "", installationId := some 0, pat := ""
    authToken := token }

/-- A line from `h`, or `none` if none came within `ms`. -/
private def lineWithin (h : IO.FS.Handle) (ms : UInt32) : IO (Option String) := do
  let reader ← IO.asTask (prio := .dedicated) h.getLine
  let timer ← IO.asTask (prio := .dedicated) (do IO.sleep ms; return "")
  match ← IO.waitAny [reader, timer] with
  | .ok l => return if l.isEmpty then none else some l
  | .error _ => return none

@[test]
def theRelayDialsTheServerAgainWhenItComesBack : Test := do
  if (← IO.Process.output { cmd := "sh", args := #["-c", "command -v bash"] }).exitCode != 0 then
    TestM.skip "no bash here"; return
  let token := "tok-" ++ (← Exec.randomHex 8)
  let (port, stop) ← Server.start (testServerState (some token))
  let (cmd, args) := McpEndpoint.stdioCommand { host := "127.0.0.1", port, token := some token }
  let relay ← IO.Process.spawn { cmd, args, stdin := .piped, stdout := .piped, stderr := .null }
  let (stdin, relay) ← relay.takeStdin
  let ask (id : Nat) : IO Unit := do
    stdin.putStrLn s!"\{\"jsonrpc\":\"2.0\",\"id\":{id},\"method\":\"tools/list\"}"
    stdin.flush
  ask 1
  let first ← lineWithin relay.stdout 10000
  TestM.assert (first.any (AgentDef.containsCI · "\"id\":1")) "the first server answers"
  -- The daemon goes away. A request made while it is gone waits in the relay, and the next daemon
  -- serves it on the same port with the same token.
  stop
  IO.sleep 300
  ask 2
  IO.sleep 500
  let (port', stop') ← Server.start (testServerState (some token)) (exactPort := some port)
  TestM.assertEqual port' port (msg := "the server comes back on the port the agent knows")
  let second ← lineWithin relay.stdout 20000
  TestM.assert (second.any (AgentDef.containsCI · "\"id\":2"))
    s!"the request made while it was gone is answered by the new one ({second})"
  ask 3
  let third ← lineWithin relay.stdout 10000
  TestM.assert (third.any (AgentDef.containsCI · "\"id\":3")) "and the tools go on working"
  -- The agent closing its side ends the relay.
  stdin.flush
  relay.kill
  discard relay.wait
  stop'

@[test]
def aReservedPortIsLeftForTheTaskThatAsksForIt : Test := do
  let (port, stop) ← Server.start (testServerState (some "t")) (portRange := some (47811, 47813))
  stop
  IO.sleep 100
  Server.reservePorts #[port]
  let (other, stop2) ← Server.start (testServerState (some "t")) (portRange := some (47811, 47813))
  TestM.assert (other != port) "a task taking any port in the range is not handed a reserved one"
  let (exact, stop3) ← Server.start (testServerState (some "t")) (exactPort := some port)
  TestM.assertEqual exact port (msg := "while the task it was kept for gets it by number")
  Server.releasePort port
  stop2; stop3

/-! ## What is recorded -/

private def sampleState : RunState.RunState := {
  phase := .attempt, attempt := 2, backend := "kubernetes"
  session := Json.mkObj [("pod", .str "orchestra-abc")]
  repoPath := "/data/work/acme-widgets-tasks/t-1", mcpPort := 47812
  mcpToken := some "secret", authLabel := some "acct-a"
  kleis := some { token := "k", revocationId := "rev", expires := 1700000000 } }

@[test]
def aRunStateRoundTrips : Test := do
  match RunState.ofJson? (RunState.toJson sampleState) with
  | .ok s => TestM.assert (s == sampleState) "everything written is read back"
  | .error e => TestM.fail s!"it did not read back: {e}"
  let bare : RunState.RunState := { sampleState with mcpToken := none, kleis := none, authLabel := none }
  TestM.assert (okIs (RunState.ofJson? (RunState.toJson bare)) bare) "and so is a record without the optional parts"

@[test]
def aRunStateThisBuildCannotFullyReadIsRefused : Test := do
  let j := RunState.toJson sampleState
  let with_ (k : String) (v : Json) := j.setObjVal! k v
  TestM.assert (RunState.ofJson? (with_ "version" (Json.num 2))).toOption.isNone "a newer version"
  TestM.assert (RunState.ofJson? (with_ "phase" (.str "mystery"))).toOption.isNone "an unknown phase"
  TestM.assert (RunState.ofJson? (with_ "mcp_port" (Json.num 0))).toOption.isNone "a port that is not one"
  TestM.assert (RunState.ofJson? (with_ "mcp_port" (Json.num 70000))).toOption.isNone "nor that"
  TestM.assert (RunState.ofJson? (Json.mkObj [("version", Json.num 1)])).toOption.isNone
    "a record missing what the task needs"

@[test]
def aRunStateIsStoredUntilTheTaskEnds : Test := do
  let (saved, again, gone) ← withTempData do
    RunState.save "t-1" sampleState
    let saved ← RunState.load "t-1"
    RunState.save "t-1" { sampleState with phase := .after }
    let again ← RunState.load "t-1"
    RunState.delete "t-1"
    let gone ← RunState.load "t-1"
    return (saved, again, gone)
  TestM.assert (saved.any (okIs · sampleState)) "what was saved is loaded"
  TestM.assert (again.any fun r => r.toOption.any (·.phase == .after)) "a later save replaces it"
  TestM.assert gone.isNone "and a deleted one is gone"

@[test]
def aPodSnapshotRoundTripsAndNamesItsRunHelper : Test := do
  let snap : Kubernetes.Snapshot := {
    pod := "orchestra-abc", image := "img:1", claim := some ("orchestra-ws-1", true), created := 42
    ns := "orchestra", mcpHost := "orchestra.svc", mcpPorts := some (47000, 47099) }
  TestM.assert (okIs (Kubernetes.Snapshot.fromJson? snap.toJson) snap) "a snapshot reads back"
  let noClaim : Kubernetes.Snapshot := { snap with claim := none }
  TestM.assert (okIs (Kubernetes.Snapshot.fromJson? noClaim.toJson) noClaim) "with or without a claim"
  let other := snap.toJson.setObjVal! "runner" (.str "/orchestra/run-0")
  TestM.assert (Kubernetes.Snapshot.fromJson? other).toOption.isNone
    "a pod set up with another protocol's run helper is not picked up"
  TestM.assertEqual Kubernetes.runnerPath s!"/orchestra/run-{protocol}"
    (msg := "the helper lives in the control directory, under its protocol")

/-! ## The run helper's protocol, as the daemon speaks it -/

@[test]
def theRunHelpersRepliesAreRead : Test := do
  TestM.assert (parseStartReply "started\n" == some .started) "started"
  TestM.assert (parseStartReply "existing 1234\n" == some (.existing 1234)) "existing, with bytes"
  TestM.assert (parseStartReply "orchestra-run: oops" == none) "anything else is not a reply"
  TestM.assert (parseStatus "exited 143 10 3\n" == some (.exited 143 10 3)) "exited"
  TestM.assert (parseStatus "running 5 2" == some (.running 5 2)) "running"
  TestM.assert (parseStatus "starting 0 0" == some .starting) "starting"
  TestM.assert (parseStatus "unknown 0 0" == some .unknown) "unknown"
  TestM.assert (parseStatus "exited 1 10" == none) "and a protocol-1 reply is not read as this one"
  TestM.assert (parseStatus "" == none) "and silence is not a status"
  TestM.assertEqual (startArgs "agent-0" #["claude", "-p", "hi"])
    #["start", "agent-0", "--", "claude", "-p", "hi"]
  TestM.assertEqual (startArgs "s" #["cat"] (stdin := true)) #["start", "s", "--stdin", "--", "cat"]
  TestM.assertEqual (attachArgs "validate-1" "err" 77) #["attach", "validate-1", "err", "77"]
  for good in ["agent-0", "validate-12", "after", "script-ab12", "finish"] do
    TestM.assert (validRunId good) s!"'{good}' names a run"
  for bad in ["", "../x", "a/b", ".hidden", "a b", "$(id)"] do
    TestM.assert (!validRunId bad) s!"'{bad}' does not"

@[test]
def everyStepOfTheTaskLoopHasAnIdOfItsOwn : Test := do
  let ids := ["init", "after"].map (TaskRunner.stepRunId ·)
    ++ (List.range 3).flatMap fun k =>
      ["before", "agent", "validate"].map (TaskRunner.stepRunId · (some k))
  TestM.assertEqual ids.eraseDups.length ids.length (msg := "no two steps share an id")
  TestM.assert (ids.all validRunId) "and every one is a valid run id"
  TestM.assertEqual (TaskRunner.stepRunId "agent" (some 1)) "agent-1"

@[test]
def aPickedUpLoopResumesAtTheAttemptItHadReached : Test := do
  TestM.assertEqual (TaskRunner.resumeAttempt 0 3) 0
  TestM.assertEqual (TaskRunner.resumeAttempt 2 3) 2 (msg := "the attempt it was at")
  TestM.assertEqual (TaskRunner.resumeAttempt 5 3) 2
    (msg := "clamped to the last one the configuration allows now")

/-! ## Who is picked up, and when a stop hands over -/

private def runningEntry : Queue.QueueEntry := {
  id := "q-1", createdAt := "2026-10-10T08:00:00Z", status := .running
  repo := none, prompt := "fix it", taskId := some "t-1" }

private def decide (e : Queue.QueueEntry := runningEntry)
    (record : Option TaskStore.TaskStatus := some .running)
    (state : Option (Except String RunState.RunState) := some (.ok sampleState))
    (backend := "kubernetes") (same := true) : Except String RunState.RunState :=
  Queue.adoptionDecision e record state backend same

private def refused (x : Except String RunState.RunState) : Bool := x.toOption.isNone

@[test]
def aRunningEntryWithItsProgressOnRecordIsPickedUp : Test := do
  TestM.assert (okIs decide sampleState) "picked up, from what it recorded"
  TestM.assert (refused (decide (e := { runningEntry with status := .unfinished })))
    "not an entry that is not running"
  TestM.assert (refused (decide (e := { runningEntry with concertStepKey := some "k" })))
    "not a concert step, whose workflow did not survive"
  TestM.assert (refused (decide (e := { runningEntry with taskId := none }))) "not one with no run"
  TestM.assert (refused (decide (record := none))) "not one whose run has no record"
  TestM.assert (refused (decide (record := some .completed))) "not one whose run has landed"
  TestM.assert (refused (decide (state := none))) "not one that recorded nothing"
  TestM.assert (refused (decide (state := some (.error "bad")))) "nor one whose record is unreadable"
  TestM.assert (refused (decide (backend := "landrun"))) "not one from another backend"
  TestM.assert (refused (decide (same := false))) "nor one with an execution config of its own"

@[test]
def aStopHandsOverOnlyWhenEveryRunningTaskCanBeCarriedOn : Test := do
  let s := (({} : HandOffState).enter "t-1").enter "t-2"
  TestM.assertEqual (s.decide #["t-1", "t-3"]).2 false
    (msg := "not while one running task cannot be carried on")
  TestM.assertEqual (s.decide #[]).2 false (msg := "not with nothing running")
  let (s', decided) := s.decide #["t-1", "t-2"]
  TestM.assertEqual decided true (msg := "but once all of them can")
  TestM.assertEqual (s'.decide #["t-9"]).2 true (msg := "and the decision is not taken back")
  TestM.assertEqual (s'.leave "t-1").2 true
    (msg := "a task leaving after the decision stops where it is")
  TestM.assertEqual (s.leave "t-1").2 false (msg := "one leaving before it goes on")
  TestM.assertEqual ((s.leave "t-1").1.decide #["t-1", "t-2"]).2 false
    (msg := "and is no longer one that can be carried on")

@[test]
def theStartupSweepLeavesPickedUpEntriesRunning : Test := do
  let (kept, swept) ← withTempData do
    Queue.saveEntry runningEntry
    Queue.saveEntry { runningEntry with id := "q-2", taskId := some "t-2" }
    let swept ← Queue.markStaleRunningAsUnfinished (keep := #["q-1"])
    let kept ← Queue.loadEntry "q-1"
    return (kept, swept)
  TestM.assert (kept.any (·.status == .running)) "the entry being picked up is still running"
  TestM.assertEqual (swept.map (·.id)) #["q-2"] (msg := "and only the other one was swept")

/-! ## What the review found -/

@[test]
def aFollowerCutOffBeforeTheEndStillPassesTheLastLineOn : Test := do
  -- The run has already finished — its last line, the result, is on disk — and the follower's
  -- first connection is dropped before it says anything. Exiting on "exited" there loses the line
  -- the whole task is decided by.
  let r ← stageRunner
  let dir := (System.FilePath.mk r).parent.getD "."
  let fake := dir / "kubectl"
  IO.FS.writeFile fake s!"#!/bin/sh
while [ \"$1\" != \"--\" ]; do shift; done; shift
if [ \"$3\" = attach ] && [ ! -f {dir}/dropped ]; then : > {dir}/dropped; exit 1; fi
exec \"$@\"
"
  let _ ← IO.Process.output { cmd := "chmod", args := #["+x", fake.toString] }
  let _ ← runner r (startArgs "done" #["sh", "-c", "echo progress; echo RESULT; exit 4"])
  IO.sleep 500
  let out ← IO.Process.output {
    cmd := "/bin/sh", args := followArgs fake.toString "ns" "pod" r "done" "out" }
  TestM.assert (← (dir / "dropped").pathExists) "the first connection was dropped"
  TestM.assertEqual out.stdout "progress\nRESULT\n" (msg := "and every line still came through")
  TestM.assertEqual out.exitCode 4

@[test]
def aRunWhoseProcessesWereKilledFromOutsideIsSettled : Test := do
  -- `kill -9` of the wrapper and the command: nothing is left to write an exit status, and a
  -- follower would otherwise wait for one forever.
  let r ← stageRunner
  let _ ← runner r (startArgs "victim" #["sh", "-c", "sleep 300"])
  IO.sleep 300
  let dir := ((System.FilePath.mk r).parent.getD ".") / "runs" / "victim"
  let wrapper := (← IO.FS.readFile (dir / "wrapper")).trimAscii.toString
  let pid := (← IO.FS.readFile (dir / "pid")).trimAscii.toString
  let _ ← IO.Process.output { cmd := "kill", args := #["-9", wrapper, pid] }
  IO.sleep 300
  let (_, st, _) ← runner r #["status", "victim"]
  TestM.assert (parseStatus st == some (.exited 255 0 0))
    s!"status records it as ended, with 255 ({st})"
  let started ← IO.monoMsNow
  let (code, _, _) ← runner r (attachArgs "victim" "out")
  TestM.assertEqual code 255 (msg := "and following it ends at once")
  TestM.assert ((← IO.monoMsNow) - started < 5000) "rather than waiting for an exit that cannot come"

@[test]
def aStartThatIsStillGoingIsNotTakenOver : Test := do
  let r ← stageRunner
  let dir := ((System.FilePath.mk r).parent.getD ".") / "runs" / "busy"
  IO.FS.createDirAll dir
  -- A starter that is alive (this test process) and has launched nothing yet.
  IO.FS.writeFile (dir / "starter") s!"{← IO.Process.getPID}\n"
  let (code, _, _) ← runner r (startArgs "busy" #["true"])
  TestM.assert (code != 0) "a second start does not take over one in progress"
  -- One whose starter died before launching anything is done again.
  IO.FS.writeFile (dir / "starter") "999999\n"
  let (code, said, _) ← runner r (startArgs "busy" #["true"])
  TestM.assertEqual code 0
  TestM.assert (parseStartReply said == some .started) s!"a dead start is started again ({said})"

/-- Listen on a free loopback port and serve exactly one connection with `serve`. -/
private def oneShotServer (serve : Std.Internal.UV.TCP.Socket → IO Unit) : IO UInt16 := do
  let sock ← Std.Internal.UV.TCP.Socket.new
  sock.bind (Std.Net.SocketAddress.v4 { addr := Std.Net.IPv4Addr.ofParts 127 0 0 1, port := 0 })
  sock.listen 1
  let port := match ← sock.getSockName with | .v4 a => a.port | .v6 a => a.port
  discard <| IO.asTask (prio := .dedicated) do
    match ← IO.wait (← sock.accept).result? with
    | some (.ok client) => serve client
    | _ => pure ()
  return port

private def awaitP (p : IO.Promise (Except IO.Error α)) : IO α := do
  match ← IO.wait p.result! with
  | .error e => throw e
  | .ok v    => return v

@[test]
def aRequestTheServerNeverAnsweredIsAnsweredByTheRelay : Test := do
  if (← IO.Process.output { cmd := "sh", args := #["-c", "command -v bash"] }).exitCode != 0 then
    TestM.skip "no bash here"; return
  -- A server that reads the token and the request, then dies without answering: the daemon going
  -- away mid-call. The agent must get an answer — an error — and the request must not be resent.
  let received ← IO.mkRef ""
  let port ← oneShotServer fun c => do
    let mut got := ""
    for _ in [0:20] do
      match ← awaitP (← c.recv? 65536) with
      | some bytes => got := got ++ String.fromUTF8! bytes
      | none => break
      if (got.splitOn "\n").length ≥ 3 then break
    received.set got
    try awaitP (← c.shutdown) catch _ => pure ()
  let (cmd, args) := McpEndpoint.stdioCommand { host := "127.0.0.1", port, token := some "tok" }
  let relay ← IO.Process.spawn { cmd, args, stdin := .piped, stdout := .piped, stderr := .null }
  let (stdin, relay) ← relay.takeStdin
  -- An id inside the arguments as well, which must not be mistaken for the request's.
  let long := String.ofList (List.replicate 9000 'x')
  stdin.putStrLn s!"\{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":\{\"arguments\":\{\"id\":99,\"body\":\"{long}\"}},\"id\":7}"
  stdin.flush
  let reply ← lineWithin relay.stdout 20000
  TestM.assert (reply.any (AgentDef.containsCI · "\"id\":7,\"error\""))
    s!"the request gets an error answer, under its own id ({reply.map (·.take 120)})"
  TestM.assert (reply.any (AgentDef.containsCI · "interrupted")) "saying what happened"
  let got ← received.get
  TestM.assert (AgentDef.containsCI got long) "the long request reached the server whole"
  let lines := (got.splitOn "\n").filter (!·.isEmpty)
  TestM.assertEqual lines.length 2 (msg := "the token and the request, once")
  -- Used here, so the pipe stays open until now: dropping the last reference closes it, and a
  -- relay whose agent closed its stdin has nothing left to answer.
  stdin.flush
  relay.kill
  discard relay.wait

@[test]
def aConcertStepIsNeverHandedOver : Test := do
  TestM.assert (Queue.handOverable runningEntry true) "an ordinary entry can be"
  TestM.assert (!Queue.handOverable { runningEntry with concertStepKey := some "k" } true)
    "a concert step cannot: the next daemon would refuse it, and it would be lost"
  TestM.assert (!Queue.handOverable runningEntry false)
    "nor an entry under an execution configuration of its own"

@[test]
def aPickedUpTaskThatFailsBeforeItRunsAgainIsResumedNotFailed : Test := do
  TestM.assert (Daemon.fallsBackToResume (reattachFailed := true) (started := true))
    "it said it could not be picked up"
  TestM.assert (Daemon.fallsBackToResume (reattachFailed := false) (started := false))
    "any error before it was running again — a token, an identity, a backend — falls back too"
  TestM.assert (!Daemon.fallsBackToResume (reattachFailed := false) (started := true))
    "once it is running again, its errors are its own"
  let e := Exec.reattachFailed "pod gone"
  TestM.assert (Exec.isReattachFailed e) "and the error says which it is"

@[test]
def aDaemonConfiguredDifferentlyDoesNotPickThePodUp : Test := do
  let cfg (extra : List (String × Json)) : Kubernetes.Config :=
    match Kubernetes.Config.fromJson (Json.mkObj ([("mcp_host", .str "orchestra.svc"),
        ("namespace", .str "orchestra"), ("mcp_ports", Json.arr #[Json.num 47000, Json.num 47099])]
        ++ extra)) with
    | .ok c => c
    | .error _ => { mcpHost := "unparsed" }
  let snap : Kubernetes.Snapshot := {
    pod := "p"
    image := "i"
    created := 1
    ns := "orchestra"
    mcpHost := "orchestra.svc"
    mcpPorts := some (47000, 47099) }
  TestM.assert (snap.compatible (cfg [])).toOption.isSome "the same configuration picks it up"
  TestM.assert (snap.compatible (cfg [("namespace", .str "elsewhere")])).toOption.isNone
    "another namespace does not"
  TestM.assert (snap.compatible (cfg [("mcp_host", .str "moved.svc")])).toOption.isNone
    "nor another MCP host, which the agent's relay would never reach"
  TestM.assert (snap.compatible
      (cfg [("mcp_ports", Json.arr #[Json.num 48000, Json.num 48099])])).toOption.isNone
    "nor another port range"

end OrchestraTest.Reattach
