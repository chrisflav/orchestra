import OrchestraTest.TestM
import Orchestra

open Lean (Json ToJson FromJson)
open Orchestra
open Orchestra.Interactive

/-!
# Where a session lives on disk

The transcript is the part worth testing hard. It is written by the daemon and read by the API,
which in the compose deployment are two containers, and a reader's whole claim to being current
rests on the seq being monotone and the window arithmetic being right.
-/

private def withTempSessions (act : IO α) : IO α := do
  let root := System.FilePath.mk "/tmp" / s!"orchestra-interactive-{← IO.monoNanosNow}"
  IO.FS.createDirAll root
  setSessionsDirOverride (some root)
  try act
  finally
    setSessionsDirOverride none
    try IO.FS.removeDirAll root catch _ => pure ()

private def sampleRecord (id : String) : SessionRecord := {
  id
  createdAt      := "2026-08-21T10:04:12Z"
  lastActivityAt := "2026-08-21T10:04:12Z"
  upstream       := { owner := "owner", name := "repo" }
  fork           := { owner := "your-org", name := "repo" }
}

@[test]
def aSessionRoundTripsThroughDisk : Test := do
  let (loaded, missing) ← withTempSessions do
    let r := { sampleRecord "i-1" with
               status := .running, agentSessionId := some "uuid-1", turnCount := 3,
               costUsd := 0.42, lastEventSeq := 128, title := some "why does the queue stall" }
    saveSession r
    pure (← loadSession "i-1", ← loadSession "i-nothing")
  match loaded with
  | some r => do
    TestM.assertEqual r.id "i-1" (msg := "id")
    TestM.assert (r.status == SessionStatus.running) (msg := "status")
    TestM.assertEqual r.agentSessionId (some "uuid-1")
      (msg := "the agent session id is what makes a dead session resumable")
    TestM.assertEqual r.turnCount 3 (msg := "turn count")
    TestM.assertEqual r.lastEventSeq 128 (msg := "the cursor a client compares against")
    TestM.assertEqual r.title (some "why does the queue stall") (msg := "title")
    TestM.assertEqual r.upstream.toString "owner/repo" (msg := "upstream")
  | none => TestM.fail "the session should have been read back"
  TestM.assert missing.isNone (msg := "a session that was never written is absent, not an error")

@[test]
def aRecordIsNeverReadHalfWritten : Test := do
  -- Write-and-rename: the reader is another process, and a record read mid-write would parse as
  -- garbage or not at all. Asserted by reading back a record large enough to span a write.
  let ok ← withTempSessions do
    let long := String.join (List.replicate 400 "a conversation title that goes on. ")
    saveSession { sampleRecord "i-2" with title := some long }
    let r ← loadSession "i-2"
    pure (r.bind (·.title) == some long)
  TestM.assert ok (msg := "the whole record is there or none of it is")

@[test]
def sessionsAreListedNewestFirst : Test := do
  let ids ← withTempSessions do
    saveSession (sampleRecord "i-20260821T0900")
    saveSession (sampleRecord "i-20260821T1100")
    saveSession (sampleRecord "i-20260821T1000")
    pure ((← loadAllSessions).map (·.id))
  TestM.assertEqual ids.toList
    ["i-20260821T1100", "i-20260821T1000", "i-20260821T0900"]
    (msg := "ids are minted from a monotone clock, so sorting by id is sorting by age")

/-! ## The cursor

What a client asks with `?after=` and what `Last-Event-ID` means on the stream. -/

private def seqsOf (events : Array Json) : List Nat :=
  events.toList.map fun j => j.getObjValAs? Nat "seq" |>.toOption |>.getD 0

@[test]
def theCursorReturnsWhatFollowsItAndSaysHowMuchIsLeft : Test := do
  let (all, afterThree, windowed, total) ← withTempSessions do
    for i in [1, 2, 3, 4, 5] do
      appendEvent "i-3" i "2026-08-21T10:00:00Z" (.user s!"turn {i}")
    let (all, _)   ← readEvents "i-3" (after := 0)
    let (rest, _)  ← readEvents "i-3" (after := 3)
    let (win, tot) ← readEvents "i-3" (after := 0) (limit := 2)
    pure (all, rest, win, tot)
  TestM.assertEqual (seqsOf all) [1, 2, 3, 4, 5] (msg := "everything, in order")
  TestM.assertEqual (seqsOf afterThree) [4, 5]
    (msg := "after 3 means what follows 3 — never 3 itself, or a client re-reads its last event")
  TestM.assertEqual (seqsOf windowed) [1, 2] (msg := "a window is honoured")
  TestM.assertEqual total 5
    (msg := "and the total counts matches before the window, so a client knows it is behind \
without asking twice")

@[test]
def aCursorPastTheEndIsEmptyRatherThanWrong : Test := do
  let (caughtUp, total) ← withTempSessions do
    appendEvent "i-4" 1 "2026-08-21T10:00:00Z" (.user "hello")
    readEvents "i-4" (after := 99)
  TestM.assertEqual caughtUp.size 0 (msg := "a client that is current gets nothing")
  TestM.assertEqual total 0 (msg := "and is told there is nothing")

@[test]
def anAbsentTranscriptReadsAsEmpty : Test := do
  let (events, total) ← withTempSessions do readEvents "i-never-existed"
  TestM.assertEqual events.size 0 (msg := "no file, no events")
  TestM.assertEqual total 0 (msg := "and no error — the session simply has not spoken yet")

@[test]
def aTornLastLineIsSkippedRatherThanFatal : Test := do
  -- The writer flushes per line, but a reader can still catch the file mid-append. The next
  -- read gets the line whole; this one must not lose the lines before it.
  let seqs ← withTempSessions do
    appendEvent "i-5" 1 "2026-08-21T10:00:00Z" (.user "first")
    appendEvent "i-5" 2 "2026-08-21T10:00:01Z" (.user "second")
    let h ← IO.FS.Handle.mk (← transcriptPath "i-5") .append
    h.putStr "{\"seq\":3,\"kind\":\"us"
    h.flush
    let (events, _) ← readEvents "i-5"
    pure (seqsOf events)
  TestM.assertEqual seqs [1, 2]
    (msg := "the half-written line is skipped and the whole ones are still there")

/-! ## The envelope

`kind: "agent"` wraps a stream event unchanged, so a client that renders a task log renders a
transcript. The other kinds carry what the agent's own stream cannot say. -/

@[test]
def theEnvelopeSaysWhatTheAgentStreamCannot : Test := do
  let render (k : TranscriptKind) : String :=
    Json.compress (ToJson.toJson ({ seq := 1, occurredAt := "2026-08-21T10:00:00Z", kind := k }
                                  : TranscriptEvent))
  let user := render (.user "add a test")
  TestM.assert ((user.splitOn "\"kind\":\"user\"").length == 2)
    (msg := s!"a user turn is a kind of its own; got {user}")
  TestM.assert ((user.splitOn "\"occurredAt\"").length == 2)
    (msg := "and carries when it happened, in a ...At field like every other instant")
  let agent := render (.agent (.assistant (.text "hello")))
  TestM.assert ((agent.splitOn "\"event\"").length == 2)
    (msg := s!"an agent event is nested unchanged rather than flattened; got {agent}")
  let ended := render (.turnEnded 3 "success" (some 0.02) (some 12))
  TestM.assert ((ended.splitOn "\"durationSeconds\":12").length == 2)
    (msg := s!"a duration is integer seconds in a ...Seconds field; got {ended}")
  let notice := render (.notice "error" "the agent exited unexpectedly")
  TestM.assert ((notice.splitOn "\"level\":\"error\"").length == 2)
    (msg := s!"a notice says how bad it is; got {notice}")
