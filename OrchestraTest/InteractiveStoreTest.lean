import OrchestraTest.TestM
import Orchestra

open Lean (Json ToJson FromJson)
open Orchestra
open Orchestra.Interactive

/-!
# Where a session is kept

The transcript is the part worth testing hard. It is written by the daemon and read by the API,
which in the compose deployment are two containers, and a reader's whole claim to being current
rests on the seq being monotone and the window arithmetic being right.

The torn-write tests below are import tests rather than store tests now: a row is written whole
or not at all, so the tearing a file transcript could suffer only exists in the files the
database is being carried over from — and that is the one place it still has to be tolerated.
-/

private def withTempSessions (act : IO α) : IO α := Orchestra.withTempData "interactive" act

private def sampleRecord (id : String) : SessionRecord := {
  id
  createdAt      := "2026-08-21T10:04:12Z"
  lastActivityAt := "2026-08-21T10:04:12Z"
  upstream       := { owner := "owner", name := "repo" }
  fork           := { owner := "your-org", name := "repo" }
}

@[test]
def aSessionRoundTripsThroughTheDatabase : Test := do
  let (loaded, missing) ← withTempSessions do
    let r := { sampleRecord "i-1" with
               status := .running, agentSessionId := some "uuid-1", agentStarted := true,
               turnCount := 3, costUsd := 0.42, lastEventSeq := 128,
               budget := 0.1, slot := 2, model := some "opus",
               tools := some ["comment", "create_pr"], systemPrompt := some "system.md",
               identity := some "ada", resumedFrom := some "i-0",
               title := some "why does the queue stall" }
    saveSession r
    pure (← loadSession "i-1", ← loadSession "i-nothing")
  match loaded with
  | some r => do
    TestM.assertEqual r.id "i-1" (msg := "id")
    TestM.assert (r.status == SessionStatus.running) (msg := "status")
    TestM.assertEqual r.agentSessionId (some "uuid-1")
      (msg := "the agent session id is what makes a dead session resumable")
    TestM.assert r.agentStarted
      (msg := "and whether an agent ever announced itself under it")
    TestM.assertEqual r.turnCount 3 (msg := "turn count")
    TestM.assertEqual r.lastEventSeq 128 (msg := "the cursor a client compares against")
    TestM.assertEqual r.title (some "why does the queue stall") (msg := "title")
    TestM.assertEqual r.upstream.toString "owner/repo" (msg := "upstream")
    TestM.assertEqual r.fork.toString "your-org/repo" (msg := "fork")
    -- The columns that are not scalars. A dormant session is woken from this record, and one
    -- that came back with different tools, a different prompt or a different identity would not
    -- be the same conversation.
    TestM.assertEqual r.tools (some ["comment", "create_pr"])
      (msg := "the tools the session was granted, as a list and not as a string")
    TestM.assertEqual r.systemPrompt (some "system.md") (msg := "system prompt")
    TestM.assertEqual r.identity (some "ada") (msg := "identity")
    TestM.assertEqual r.resumedFrom (some "i-0") (msg := "what this session resumed")
    TestM.assertEqual r.slot 2 (msg := "the clone slot it holds")
    TestM.assertEqual r.model (some "opus") (msg := "model")
    -- `0.1` is not representable, so a column that rounded or reprinted it comes back different.
    TestM.assert (r.budget == 0.1) (msg := s!"the budget survives as a float; got {r.budget}")
    TestM.assert (r.costUsd == 0.42) (msg := s!"and so does the spend; got {r.costUsd}")
  | none => TestM.fail "the session should have been read back"
  TestM.assert missing.isNone (msg := "a session that was never written is absent, not an error")

@[test]
def savingASessionTwiceLeavesOneSession : Test := do
  -- The write is an upsert on the id, not an insert: the record is rewritten on every state
  -- change, and a second row would be a second conversation in every listing.
  let (count, status) ← withTempSessions do
    saveSession (sampleRecord "i-8")
    saveSession { sampleRecord "i-8" with status := .ended }
    let all ← loadAllSessions
    pure (all.size, (← loadSession "i-8").map (·.status))
  TestM.assertEqual count 1 (msg := "one session, not two")
  TestM.assert (status == some SessionStatus.ended) (msg := "and it is the later of the two")

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
    let (win, tot) ← readEvents "i-3" (after := 0) (atMost := 2)
    pure (all, rest, win, tot)
  TestM.assertEqual (seqsOf all) [1, 2, 3, 4, 5] (msg := "everything, in order")
  TestM.assertEqual (seqsOf afterThree) [4, 5]
    (msg := "after 3 means what follows 3 — never 3 itself, or a client re-reads its last event")
  TestM.assertEqual (seqsOf windowed) [1, 2] (msg := "a window is honoured")
  TestM.assertEqual total 5
    (msg := "and the total counts matches before the window, so a client knows it is behind \
without asking twice")

@[test]
def theTotalCountsEverythingAfterTheCursorAndNothingBeforeIt : Test := do
  -- The envelope arithmetic the API depends on: the total is over the rows the cursor selects,
  -- not over the window and not over the whole conversation.
  let (window, total) ← withTempSessions do
    for i in [1, 2, 3, 4, 5, 6, 7] do
      appendEvent "i-10" i "2026-08-21T10:00:00Z" (.user s!"turn {i}")
    readEvents "i-10" (after := 4) (atMost := 2)
  TestM.assertEqual (seqsOf window) [5, 6] (msg := "the first two after the cursor")
  TestM.assertEqual total 3 (msg := "and three of them in all, not seven and not two")

@[test]
def aTranscriptOfOneSessionIsNotAnothersTranscript : Test := do
  let (mine, total) ← withTempSessions do
    appendEvent "i-a" 1 "2026-08-21T10:00:00Z" (.user "mine")
    appendEvent "i-b" 1 "2026-08-21T10:00:00Z" (.user "not mine")
    appendEvent "i-b" 2 "2026-08-21T10:00:01Z" (.user "also not mine")
    readEvents "i-a"
  TestM.assertEqual (seqsOf mine) [1] (msg := "one session's events, not the table's")
  TestM.assertEqual total 1 (msg := "and a total over that session alone")

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
  TestM.assertEqual events.size 0 (msg := "no rows, no events")
  TestM.assertEqual total 0 (msg := "and no error — the session simply has not spoken yet")

@[test]
def anIdThatWouldEscapeTheSessionRootIsRefused : Test := do
  -- Not every id reaching the store came from a path segment the HTTP layer checked —
  -- `resumeFrom` arrives in a request body — so the store holds this itself. A row key escapes
  -- nothing, but an id is a name every other surface can also name, and this is what says so.
  let outcome ← withTempSessions do
    try
      let _ ← loadSession "../../etc/passwd"
      pure "accepted"
    catch _ => pure "refused"
  TestM.assertEqual outcome "refused" (msg := "a traversing id must not become a path")

/-! ## Carrying the old files over

A session used to be `<data>/interactive/<id>/` with a `session.json` and an `events.jsonl`. The
transcript was appended to and flushed per line, so a daemon killed mid-write left a fragment at
the end of it — sometimes half a line, sometimes half a character. The import is the last reader
of those files, and it is where that tolerance still has to live. -/

/-- Write a legacy session directory: the record, and a transcript whose lines are given as
    already-written text (so that a torn one can be written as the killed daemon left it). -/
private def writeLegacySession (r : SessionRecord) (transcript : Option ByteArray) : IO Unit := do
  let dir := (← legacySessionsDir) / r.id
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "session.json") (Json.compress (ToJson.toJson r))
  if let some bytes := transcript then
    IO.FS.writeBinFile (dir / "events.jsonl") bytes

/-- One transcript line as the file store wrote it: the newline first, then the record. -/
private def legacyLine (seq : Nat) (text : String) : String :=
  "\n" ++ Json.compress (ToJson.toJson
    ({ seq, occurredAt := "2026-08-21T10:00:00Z", kind := .user text } : TranscriptEvent))

@[test]
def theImportCarriesASessionAndItsTranscriptOver : Test := do
  let (record, seqs, total) ← withTempSessions do
    let record := { sampleRecord "i-old" with
                    status       := .dormant
                    lastEventSeq := 2
                    tools        := some ["comment"]
                    title        := some "the old conversation" }
    writeLegacySession record (some (legacyLine 1 "first" ++ legacyLine 2 "second").toUTF8)
    Orchestra.Store.Import.run
    let (events, total) ← readEvents "i-old"
    pure (← loadSession "i-old", seqsOf events, total)
  match record with
  | none   => TestM.fail "the session record should have been imported"
  | some r => do
    TestM.assert (r.status == SessionStatus.dormant)
      (msg := "a dormant session comes back dormant, not restarted into something else")
    TestM.assertEqual r.title (some "the old conversation") (msg := "title")
    TestM.assertEqual r.tools (some ["comment"]) (msg := "and the tools it was granted")
  TestM.assertEqual seqs [1, 2] (msg := "both transcript lines are rows now")
  TestM.assertEqual total 2 (msg := "and the count agrees with them")

@[test]
def theImportSkipsATornLastLineAndKeepsTheRest : Test := do
  -- The writer flushed per line, but a kill lands mid-payload. The fragment begins with the
  -- newline the writer put first, which is what bounds it; everything before it is whole.
  let seqs ← withTempSessions do
    writeLegacySession (sampleRecord "i-torn")
      (some (legacyLine 1 "first" ++ legacyLine 2 "second"
             ++ "\n{\"seq\":3,\"kind\":\"us").toUTF8)
    Orchestra.Store.Import.run
    let (events, _) ← readEvents "i-torn"
    pure (seqsOf events)
  TestM.assertEqual seqs [1, 2]
    (msg := "the half-written line is skipped and the whole ones are still there")

@[test]
def aTailTornMidCharacterDoesNotCostTheConversation : Test := do
  -- `readBinFile` plus a trim, because `readFile` throws on invalid UTF-8 — below the per-line
  -- recovery, so it never gets the chance. A daemon killed mid-`→` would otherwise make the
  -- whole transcript unreadable, and now unimportable, rather than costing one event.
  let seqs ← withTempSessions do
    let whole := (legacyLine 1 "a turn mentioning → and ✓").toUTF8
    -- One byte of a three-byte character, which is what a killed writer leaves behind.
    writeLegacySession (sampleRecord "i-cut") (some (whole ++ ByteArray.mk #[0x0a, 0x7b, 0xe2]))
    Orchestra.Store.Import.run
    let (events, _) ← readEvents "i-cut"
    pure (seqsOf events)
  TestM.assertEqual seqs [1]
    (msg := "the torn tail is trimmed and everything before it is still readable")

@[test]
def aSessionDirectoryWithoutARecordIsNotASession : Test := do
  -- `<data>/interactive` held one directory per session, but a daemon that was killed between
  -- creating the directory and writing the record left an empty one behind.
  let (all, imported) ← withTempSessions do
    IO.FS.createDirAll ((← legacySessionsDir) / "i-empty")
    writeLegacySession (sampleRecord "i-real") none
    Orchestra.Store.Import.run
    let rows ← Orchestra.Store.run <|
      HasModel.fetch (QuerySet.all (α := Orchestra.Store.LegacyImportRow))
    pure ((← loadAllSessions).map (·.id),
          (rows.filter (·.store == "interactive sessions")).map (·.records))
  TestM.assertEqual all.toList ["i-real"] (msg := "the directory with no record is not a session")
  TestM.assertEqual imported.toList [(1 : Int)]
    (msg := "and is not counted as one either")

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

@[test]
def anEventIsHandedBackAsWrittenRatherThanReSerialised : Test := do
  -- `doc` is the whole event, so a field a newer orchestra writes survives being read by an
  -- older one. The check that it is the transcript line and not a re-rendering of a typed value.
  let doc ← withTempSessions do
    appendEvent "i-11" 4 "2026-08-21T10:00:00Z" (.turnEnded 2 "success" (some 0.02) (some 12))
    let (events, _) ← readEvents "i-11" (after := 3)
    pure (events[0]?.map Json.compress)
  TestM.assertEqual doc
    (some (Json.compress (ToJson.toJson
      ({ seq := 4, occurredAt := "2026-08-21T10:00:00Z",
         kind := .turnEnded 2 "success" (some 0.02) (some 12) } : TranscriptEvent))))
    (msg := "exactly the text a transcript line held")
