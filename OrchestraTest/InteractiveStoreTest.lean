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
  -- By `created_at`. Ids come from a clock that restarts at boot, so the id here that sorts
  -- highest belongs to the *oldest* session — which is what a reboot does to a store.
  let ids ← withTempSessions do
    saveSession { sampleRecord "i-9000" with createdAt := "2026-08-21T09:00:00Z" }
    saveSession { sampleRecord "i-0002" with createdAt := "2026-08-21T11:00:00Z" }
    saveSession { sampleRecord "i-0001" with createdAt := "2026-08-21T10:00:00Z" }
    pure ((← loadAllSessions).map (·.id))
  TestM.assertEqual ids.toList ["i-0002", "i-0001", "i-9000"]
    (msg := "newest first by when the session was made, not by how its id sorts")

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
    -- A kill lands mid-payload, so the fragment begins with the newline the writer put first.
    h.putStr "\n{\"seq\":3,\"kind\":\"us"
    h.flush
    let (events, _) ← readEvents "i-5"
    pure (seqsOf events)
  TestM.assertEqual seqs [1, 2]
    (msg := "the half-written line is skipped and the whole ones are still there")

@[test]
def anEventAppendedAfterATornWriteSurvives : Test := do
  -- The case the test above stops one step short of, and the one that actually happens: the
  -- daemon is killed mid-write and then *restarts* and keeps appending. With the newline after
  -- each record the next append lands on the fragment and the splice parses as neither, so the
  -- crash takes the following event with it too. The newline goes before the record for exactly
  -- this reason.
  let seqs ← withTempSessions do
    appendEvent "i-6" 1 "2026-08-21T10:00:00Z" (.user "before the crash")
    let h ← IO.FS.Handle.mk (← transcriptPath "i-6") .append
    h.putStr "\n{\"seq\":2,\"occurredAt\":\"2026-08-21T10:00:01Z\",\"kind\":\"us"
    h.flush
    appendEvent "i-6" 3 "2026-08-21T10:00:02Z" (.user "after the restart")
    let (events, _) ← readEvents "i-6"
    pure (seqsOf events)
  TestM.assertEqual seqs [1, 3]
    (msg := "only the torn event is lost — not the one written after it")

@[test]
def aTailTornMidCharacterDoesNotDestroyTheConversation : Test := do
  -- `readFile` throws on invalid UTF-8, below the per-line recovery, so before this a daemon
  -- killed mid-`→` made the whole transcript unreadable forever rather than costing one event.
  let seqs ← withTempSessions do
    appendEvent "i-7" 1 "2026-08-21T10:00:00Z" (.user "a turn mentioning → and ✓")
    -- One byte of a three-byte character, which is what a killed writer leaves behind.
    let h ← IO.FS.Handle.mk (← transcriptPath "i-7") .append
    h.write (ByteArray.mk #[0x0a, 0x7b, 0xe2])
    h.flush
    let (events, _) ← readEvents "i-7"
    pure (seqsOf events)
  TestM.assertEqual seqs [1]
    (msg := "the torn tail is trimmed and everything before it is still readable")

@[test]
def anIdThatWouldEscapeTheSessionRootIsRefused : Test := do
  -- Not every id reaching the store came from a path segment the HTTP layer checked —
  -- `resumeFrom` arrives in a request body — so the store holds this itself.
  let outcome ← withTempSessions do
    try
      let _ ← loadSession "../../etc/passwd"
      pure "accepted"
    catch _ => pure "refused"
  TestM.assertEqual outcome "refused" (msg := "a traversing id must not become a path")

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
