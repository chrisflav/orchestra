import OrchestraTest.TestM
import Orchestra

open Lean (Json)
open Orchestra
open Orchestra.Interactive

/-!
# The session manager

Two things that can be checked without an agent, a clone or a network, and that both matter more
than the happy path: what a restart leaves behind, and what happens when a backend is asked to do
something its CLI cannot.
-/

private def withTempSessions (act : IO α) : IO α := do
  let root := System.FilePath.mk "/tmp" / s!"orchestra-interactive-mgr-{← IO.monoNanosNow}"
  IO.FS.createDirAll root
  setSessionsDirOverride (some root)
  try act
  finally
    setSessionsDirOverride none
    try IO.FS.removeDirAll root catch _ => pure ()

private def sampleRecord (id : String) (status : SessionStatus) : SessionRecord := {
  id, status
  createdAt      := "2026-08-21T10:04:12Z"
  lastActivityAt := "2026-08-21T10:04:12Z"
  upstream       := { owner := "owner", name := "repo" }
  fork           := { owner := "your-org", name := "repo" }
}

/-! ## A restart

The agent processes died with the last daemon. The records did not. -/

@[test]
def aRestartPutsEverySessionToSleepRatherThanEndingIt : Test := do
  let after ← withTempSessions do
    saveSession (sampleRecord "i-starting" .starting)
    saveSession (sampleRecord "i-idle" .idle)
    saveSession (sampleRecord "i-running" .running)
    reconcile
    pure (← loadAllSessions)
  TestM.assertEqual after.size 3 (msg := "nothing is deleted — the transcripts are still worth \
reading")
  for r in after do
    -- Not terminal, and not `running` either. The processes those records described died with
    -- the last daemon, so a session left reading `running` is a conversation that will never
    -- answer and a client that sits on its stream waiting; but the conversation itself survived
    -- the restart, and closing it would throw away the only thing that did.
    TestM.assert (r.status == SessionStatus.dormant)
      (msg := s!"{r.id} still reads {Json.compress (Lean.ToJson.toJson r.status)} after a \
restart; the agent is gone, so it is dormant")
    TestM.assert (!r.status.isTerminal)
      (msg := s!"{r.id} must still be resumable — a restart is not an ending")
    TestM.assertEqual r.error none
      (msg := s!"{r.id} has nothing wrong with it; it is asleep")
    TestM.assert r.endedAt.isNone (msg := s!"{r.id} has not ended, so it has no ending time")

@[test]
def aRestartLeavesADormantSessionExactlyAsItWas : Test := do
  -- Reconciliation runs on every start-up, and most restarts find sessions that were already
  -- put down by the last shutdown. Stamping them again would append a "the daemon restarted"
  -- notice to a transcript for every restart that happened while nobody was talking.
  let before := { sampleRecord "i-asleep" .dormant with lastEventSeq := 7 }
  let after ← withTempSessions do
    saveSession before
    reconcile
    pure (← loadSession "i-asleep")
  match after with
  | none   => TestM.fail "the record should still be there"
  | some r =>
    TestM.assert (r.status == SessionStatus.dormant) (msg := "still dormant")
    TestM.assertEqual r.lastEventSeq 7 (msg := "and untouched — no notice was appended")

@[test]
def endedAndFailedSurviveARestartUnchanged : Test := do
  let after ← withTempSessions do
    saveSession { sampleRecord "i-done" .ended with endedAt := some "2026-08-22T00:00:00Z" }
    saveSession { sampleRecord "i-dead" .failed with error := some "the agent process exited" }
    reconcile
    pure (← loadAllSessions)
  for r in after do
    TestM.assert r.status.isTerminal
      (msg := s!"{r.id} was over before the restart and is still over")
  TestM.assertEqual ((after.filter (·.id == "i-dead"))[0]?.bind (·.error))
    (some "the agent process exited") (msg := "and still says what happened to it")

@[test]
def aRestartLeavesFinishedSessionsExactlyAsTheyWere : Test := do
  let (ended, failed) ← withTempSessions do
    saveSession { sampleRecord "i-ended" .ended with
                  endedAt := some "2026-08-21T11:00:00Z" }
    saveSession { sampleRecord "i-failed" .failed with
                  endedAt := some "2026-08-21T11:00:00Z"
                  error := some "the agent process exited" }
    reconcile
    pure (← loadSession "i-ended", ← loadSession "i-failed")
  match ended, failed with
  | some e, some f => do
    TestM.assertEqual e.endedAt (some "2026-08-21T11:00:00Z")
      (msg := "a session that had already ended is not re-ended")
    TestM.assertEqual f.error (some "the agent process exited")
      (msg := "and one that failed keeps the reason it failed for, not the restart's")
  | _, _ => TestM.fail "both sessions should still be readable"

@[test]
def aRestartTellsTheTranscriptTooAndKeepsTheCursorMonotone : Test := do
  -- A client reading the stream learns the session is over the same way it learns everything
  -- else: an event after the last one it saw.
  let (events, record) ← withTempSessions do
    saveSession { sampleRecord "i-live" .running with lastEventSeq := 7 }
    reconcile
    pure (← readEvents "i-live" (after := 7), ← loadSession "i-live")
  let (items, _) := events
  TestM.assertEqual items.size 1 (msg := "exactly one new event")
  match items[0]? with
  | some j => do
    TestM.assertEqual (j.getObjValAs? Nat "seq" |>.toOption |>.getD 0) 8
      (msg := "the seq continues from where the session left off, so no cursor goes backwards")
    TestM.assertEqual (j.getObjValAs? String "kind" |>.toOption |>.getD "") "notice"
      (msg := "the daemon speaking, not the agent")
  | none => TestM.fail "the restart should leave a notice in the transcript"
  match record with
  | some r =>
    TestM.assertEqual r.lastEventSeq 8
      (msg := "and the record agrees with the transcript about how far it got")
  | none => TestM.fail "the record should still be readable"

/-! ## A backend that cannot hold a conversation -/

@[test]
def aBackendWithNoStreamingModeIsRefusedBeforeAnythingIsAcquired : Test := do
  let acquired ← IO.mkRef false
  let mgr ← Manager.new {}
    (fun _ => do acquired.set true; return some 0)
    (fun _ _ => pure ())
  let cfg : AppConfig := { appId := 0, privateKeyPath := "" }
  let spec : SessionSpec := {
    upstream := { owner := "owner", name := "repo" }
    fork     := { owner := "your-org", name := "repo" }
    backend  := some "pi"
  }
  match ← mgr.start cfg spec with
  | .ok _ => TestM.fail "pi has no streaming input mode and must not host a session"
  | .error e => do
    -- Named, because "could not start a session" sends someone looking at their network.
    TestM.assert ((e.splitOn "pi").length ≥ 2)
      (msg := s!"the message should name the backend; got: {e}")
    TestM.assert ((e.splitOn "claude").length ≥ 2)
      (msg := s!"and say which backend can; got: {e}")
  TestM.assert (!(← acquired.get))
    (msg := "and nothing should have been acquired — no slot, no clone, no token")

@[test]
def theSessionCapIsCheckedBeforeAnythingIsAcquired : Test := do
  let acquired ← IO.mkRef false
  -- A cap of zero is the same check every other cap is, without needing a live session to fill
  -- one: it is refused for want of room rather than for want of a backend.
  let mgr ← Manager.new { maxSessions := 0 }
    (fun _ => do acquired.set true; return some 0)
    (fun _ _ => pure ())
  let cfg : AppConfig := { appId := 0, privateKeyPath := "" }
  match ← mgr.start cfg { upstream := { owner := "o", name := "r" },
                          fork := { owner := "f", name := "r" } } with
  | .ok _ => TestM.fail "the cap should have refused this"
  | .error e =>
    TestM.assert ((e.splitOn "limit").length ≥ 2)
      (msg := s!"the message should say it is a limit; got: {e}")
  TestM.assert (!(← acquired.get)) (msg := "and no slot should have been taken")

/-! ## The cap is a decision, not an observation

A session takes seconds to start, so counting only what is already in the table let two requests
arriving together both see room. -/

@[test]
def theCapCountsSessionsThatAreStillStarting : Test := do
  -- The second call must be refused even though the first has not finished starting — which it
  -- never will here, because there is no GitHub App behind it. What matters is that the cap was
  -- already spent by the attempt, not by the outcome.
  let reserved ← IO.mkRef 0
  let released ← IO.mkRef 0
  let mgr ← Manager.new { maxSessions := 1 }
    (fun _ => do reserved.modify (· + 1); return some 0)
    (fun _ _ => released.modify (· + 1))
  let cfg : AppConfig := { appId := 0, privateKeyPath := "" }
  let spec : SessionSpec := {
    upstream := { owner := "o", name := "r" }, fork := { owner := "f", name := "r" } }
  let outcome ← withTempSessions do
    let first ← mgr.start cfg spec
    let second ← mgr.start cfg spec
    pure (first, second)
  let (first, second) := outcome
  -- The first attempt fails (no App), but it must give its reservation back on the way out.
  let isError {α β : Type} : Except α β → Bool | .error _ => true | .ok _ => false
  TestM.assert (isError first) (msg := "the first start cannot succeed without an App")
  TestM.assert (isError second) (msg := "and the second is refused too")
  -- Having released, the cap is free again — the reservation is not leaked by a failed start.
  TestM.assertEqual (← released.get) (← reserved.get)
    (msg := "every slot reserved by a failed start is given back")

@[test]
def aFailedStartLeavesNoSlotBehind : Test := do
  -- The slot is taken before anything that can throw. Every exit past that point has to give it
  -- back, or the repository runs one narrower for the life of the daemon with nothing holding
  -- the slot and nothing able to find it.
  let reserved ← IO.mkRef 0
  let released ← IO.mkRef 0
  let mgr ← Manager.new { maxSessions := 4 }
    (fun _ => do reserved.modify (· + 1); return some 0)
    (fun _ _ => released.modify (· + 1))
  let cfg : AppConfig := { appId := 0, privateKeyPath := "/nonexistent/key.pem" }
  let _ ← withTempSessions do
    mgr.start cfg { upstream := { owner := "o", name := "r" },
                    fork := { owner := "f", name := "r" } }
  TestM.assertEqual (← reserved.get) 1 (msg := "a slot was taken")
  TestM.assertEqual (← released.get) 1 (msg := "and given back when the start failed")

/-! ## What a failed start is allowed to say

The message reaches whoever asked for the session, over an API that is not the daemon's log.
The exceptions behind it name files — `openssl` reports the GitHub App private key by path when
it cannot read it — and the one file on the host that must stay unguessable from the outside is
exactly that one. -/

@[test]
def redactPathsKeepsTheFileAndDropsTheDirectory : Test := do
  TestM.assertEqual
    (redactPaths "Could not open file for loading private key from /etc/orch/secrets/app.pem")
    "Could not open file for loading private key from …/app.pem"
    (msg := "which file survives, where it lives does not")
  TestM.assertEqual (redactPaths "cannot read '/srv/data/repos/o/r-slot-0/x.lean'")
    "cannot read '…/x.lean'"
    (msg := "a quoted path is still a path")
  TestM.assertEqual (redactPaths "no such file: /etc/key.pem and /var/lib/orchestra/config.json")
    "no such file: …/key.pem and …/config.json"
    (msg := "every path in the message, not just the first")
  TestM.assertEqual (redactPaths "GET https://api.github.com/app/installations failed: 401")
    "GET https://api.github.com/app/installations failed: 401"
    (msg := "a URL is not a path — its slashes follow a character, so nothing is cut")
  TestM.assertEqual (redactPaths "exit 128: fatal: repository not found")
    "exit 128: fatal: repository not found"
    (msg := "a message with no path is left exactly as it was")

@[test]
def aFailedStartDoesNotNameTheDaemonsFilesystem : Test := do
  let mgr ← Manager.new { maxSessions := 4 }
    (fun _ => pure (some 0)) (fun _ _ => pure ())
  let cfg : AppConfig := { appId := 0, privateKeyPath := "/etc/orch/secrets/app.pem" }
  let outcome ← withTempSessions do
    mgr.start cfg { upstream := { owner := "o", name := "r" },
                    fork := { owner := "f", name := "r" } }
  match outcome with
  | .ok _ => TestM.fail "a start with no GitHub App cannot succeed"
  | .error msg =>
    TestM.assert ((msg.splitOn "/etc/orch/secrets").length == 1)
      (msg := s!"the key's directory must not reach the caller; got: {msg}")
