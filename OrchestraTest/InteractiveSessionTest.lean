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
def aRestartClosesEverySessionItCannotStillBeHolding : Test := do
  let after ← withTempSessions do
    saveSession (sampleRecord "i-starting" .starting)
    saveSession (sampleRecord "i-idle" .idle)
    saveSession (sampleRecord "i-running" .running)
    reconcile
    pure (← loadAllSessions)
  TestM.assertEqual after.size 3 (msg := "nothing is deleted — the transcripts are still worth \
reading")
  for r in after do
    TestM.assert r.status.isTerminal
      (msg := s!"{r.id} still reads {Json.compress (Lean.ToJson.toJson r.status)}; a session \
left alive after a restart is a conversation that will never answer, and a client will sit on \
its stream waiting for it")
    TestM.assertEqual r.error (some "the daemon restarted")
      (msg := s!"{r.id} should say why it ended")
    TestM.assert r.endedAt.isSome (msg := s!"{r.id} should say when")

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
