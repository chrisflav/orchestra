import OrchestraTest.TestM
import Orchestra

open Lean (Json)
open Orchestra
open Orchestra.Usage

/-!
# Usage-limit monitoring

Everything here is the part of the subsystem that runs without a clock, a config, or a network:
timestamp parsing, the availability verdict for a given `(source, model)`, and source selection.
The paths that do need those — polling, persistence — are thin wrappers over these.
-/

deriving instance DecidableEq for LimitKind
deriving instance DecidableEq for AuthMode
deriving instance DecidableEq for AgentDef.LimitScope

/-! ## Timestamps

The endpoint and orchestra itself write reset times in different formats; comparing them as
strings gives the wrong answer, which is what `parseIso8601` exists to prevent. -/

@[test]
def parseIso8601_utcEpoch : Test := do
  TestM.assertEqual (parseIso8601 "1970-01-01T00:00:00Z") (some 0) (msg := "the epoch itself")
  TestM.assertEqual (parseIso8601 "2026-07-22T00:00:00Z") (some 1784678400)
    (msg := "a plain UTC timestamp")

@[test]
def parseIso8601_acceptsTheShapeTheEndpointActuallySends : Test := do
  -- Fractional seconds and a numeric offset, exactly as `/api/oauth/usage` writes them.
  let withFraction := parseIso8601 "2026-07-22T18:59:59.573616+00:00"
  let plain        := parseIso8601 "2026-07-22T18:59:59Z"
  TestM.assertEqual withFraction plain
    (msg := "fractional seconds and +00:00 agree with the plain UTC spelling")

@[test]
def parseIso8601_appliesTheOffset : Test := do
  -- 12:00 at +02:00 is 10:00 UTC; an offset read as UTC would be two hours wrong, which is
  -- exactly long enough to dispatch into a limit that has not actually reset.
  TestM.assertEqual (parseIso8601 "2026-07-22T12:00:00+02:00")
    (parseIso8601 "2026-07-22T10:00:00Z") (msg := "+02:00 shifts back two hours")
  TestM.assertEqual (parseIso8601 "2026-07-22T12:00:00-0530")
    (parseIso8601 "2026-07-22T17:30:00Z") (msg := "-0530, no colon, shifts forward")

@[test]
def parseIso8601_rejectsRatherThanGuesses : Test := do
  -- A timestamp we cannot read must not come back as `some 0`: that is in the past, and a limit
  -- with a past reset time reads as "already expired".
  TestM.assertEqual (parseIso8601 "not a timestamp") none (msg := "garbage")
  TestM.assertEqual (parseIso8601 "") none (msg := "empty")
  TestM.assertEqual (parseIso8601 "2026-13-01T00:00:00Z") none (msg := "month 13")
  TestM.assertEqual (parseIso8601 "2026-07-22") none (msg := "date with no time")

@[test]
def secsToIso8601_roundTripsThroughParse : Test := do
  -- The headers report resets as epoch seconds; `Limit.resetsAt` is a string every consumer
  -- re-parses with `parseIso8601`. Formatting then parsing must return the original instant, or a
  -- reset time silently shifts.
  for e in [0, 1784867400, 1785351600, 1000000000, 1609459199] do
    TestM.assertEqual (parseIso8601 (secsToIso8601 e)) (some e)
      (msg := s!"round-trips {e}")

@[test]
def secsToIso8601_knownInstant : Test := do
  -- 2026-07-24T00:00:00Z is a fixed anchor: if the civil-date arithmetic drifts, this catches it.
  TestM.assertEqual (secsToIso8601 1784851200) "2026-07-24T00:00:00Z"
    (msg := "epoch 1784851200 is midnight UTC on 2026-07-24")

/-! ## Reading limits from rate-limit headers

The headers carry the same numbers the endpoint body would, but as a fraction plus an epoch reset.
This is the live source now that a `setup-token` cannot reach `/api/oauth/usage`. -/

private def sampleHeaders : Array (String × String) := #[
  ("anthropic-ratelimit-unified-5h-status", "allowed"),
  ("anthropic-ratelimit-unified-5h-utilization", "0.02"),
  ("anthropic-ratelimit-unified-5h-reset", "1784867400"),
  ("anthropic-ratelimit-unified-7d-status", "rejected"),
  ("anthropic-ratelimit-unified-7d-utilization", "1"),
  ("anthropic-ratelimit-unified-7d-reset", "1785351600"),
  ("content-type", "application/json")
]

@[test]
def parseUnifiedHeaders_readsBothWindows : Test := do
  let ls := parseUnifiedHeaders sampleHeaders
  TestM.assertEqual ls.size 2 (msg := "session and weekly-all")
  match ls.find? (·.kind == .session) with
  | none   => TestM.fail "expected a session limit"
  | some l =>
    TestM.assertEqual l.percent 2 (msg := "0.02 fraction becomes 2 percent")
    TestM.assert (!l.isActive) (msg := "an allowed window is not binding")
    TestM.assertEqual (l.resetsAt.bind parseIso8601) (some 1784867400)
      (msg := "reset epoch survives the format/parse round-trip")
  match ls.find? (·.kind == .weeklyAll) with
  | none   => TestM.fail "expected a weekly_all limit"
  | some l =>
    TestM.assertEqual l.percent 100 (msg := "1.0 fraction is 100 percent")
    TestM.assert l.isActive (msg := "a rejected window binds")
    TestM.assertEqual l.severity "critical" (msg := "100% is critical")

@[test]
def parseUnifiedHeaders_bindsARejectedWindowThatIsStillInTheFuture : Test := do
  -- The end-to-end point: a rejected window with a future reset must read as binding, so the
  -- availability check blocks the source until it lifts.
  let now := 1785000000
  let ls := parseUnifiedHeaders sampleHeaders
  let st : SourceState := { backend := "b", label := "l", limits := ls }
  match availabilityOf st none now with
  | .blocked u _ => TestM.assertEqual u (some 1785351600) (msg := "blocked until the 7d reset")
  | .available   => TestM.fail "a rejected weekly window should block the source"

@[test]
def parseUnifiedHeaders_emptyWhenNoUnifiedHeaders : Test := do
  -- A response with no unified headers (a transport error page) yields nothing, and the caller
  -- reads the status instead of inventing limits.
  TestM.assert (parseUnifiedHeaders #[("content-type", "text/html")]).isEmpty
    (msg := "no unified headers, no limits")

/-! ## Parsing the endpoint payload -/

/-- The response shape observed from `GET /api/oauth/usage`, trimmed to the fields read here:
    a session limit well under the line, a weekly total in warning, and a weekly limit scoped to
    one model family that is exhausted. -/
private def sampleBody : String := r#"{
  "five_hour": {"utilization": 3, "resets_at": "2026-07-22T04:29:59.573594+00:00"},
  "seven_day": {"utilization": 75, "resets_at": "2026-07-22T18:59:59.573616+00:00"},
  "seven_day_opus": null,
  "tangelo": null,
  "limits": [
    {"kind": "session", "group": "session", "percent": 3, "severity": "normal",
     "resets_at": "2026-07-22T04:29:59.573594+00:00", "scope": null, "is_active": false},
    {"kind": "weekly_all", "group": "weekly", "percent": 75, "severity": "warning",
     "resets_at": "2026-07-22T18:59:59.573616+00:00", "scope": null, "is_active": false},
    {"kind": "weekly_scoped", "group": "weekly", "percent": 100, "severity": "critical",
     "resets_at": "2026-07-22T18:59:59.573865+00:00",
     "scope": {"model": {"id": null, "display_name": "Fable"}, "surface": null},
     "is_active": true}
  ]
}"#

@[test]
def parseUtilization_readsEveryLimitAndItsScope : Test := do
  match parseUtilization sampleBody with
  | .error e => TestM.fail s!"expected the sample payload to parse: {e}"
  | .ok ls =>
    TestM.assertEqual ls.size 3 (msg := "all three limits")
    match ls.find? (·.kind == .weeklyScoped) with
    | none => TestM.fail "expected a weekly_scoped limit"
    | some l =>
      TestM.assertEqual l.scopeModel (some "Fable") (msg := "scope.model.display_name is lifted")
      TestM.assertEqual l.percent 100 (msg := "percent")
      TestM.assert l.isActive (msg := "is_active")

@[test]
def parseUtilization_survivesUnknownKinds : Test := do
  -- The endpoint already ships kinds this code has never heard of. One of them must not cost us
  -- the limits we *do* understand.
  let body := r#"{"limits":[
    {"kind":"nimbus_quill","percent":10,"is_active":false},
    {"kind":"session","percent":50,"is_active":false}]}"#
  match parseUtilization body with
  | .error e => TestM.fail s!"unknown kinds should not fail the document: {e}"
  | .ok ls =>
    TestM.assertEqual ls.size 2 (msg := "both kept")
    TestM.assertEqual (ls[0]!.kind) (LimitKind.other "nimbus_quill") (msg := "kept verbatim")

@[test]
def parseUtilization_fallsBackToTheLegacyShape : Test := do
  -- A response with no `limits` array still has to yield something usable.
  let body := r#"{"five_hour":{"utilization":100,"resets_at":"2026-07-22T04:00:00Z"},
                  "seven_day":{"utilization":20,"resets_at":"2026-07-29T04:00:00Z"}}"#
  match parseUtilization body with
  | .error e => TestM.fail s!"legacy shape should parse: {e}"
  | .ok ls =>
    TestM.assertEqual ls.size 2 (msg := "both legacy windows")
    match ls.find? (·.kind == .session) with
    | none   => TestM.fail "expected a session limit"
    | some l => TestM.assert l.isActive (msg := "100% is treated as binding")

@[test]
def parseUtilization_acceptsTheOnDiskCacheShape : Test := do
  -- Claude Code stores the same payload nested under "utilization"; accepting both means a
  -- hand-pasted cache file can be used to reason about a state that is hard to reproduce live.
  let body := "{\"fetchedAtMs\":1,\"utilization\":" ++ sampleBody ++ "}"
  match parseUtilization body with
  | .error e => TestM.fail s!"nested shape should parse: {e}"
  | .ok ls   => TestM.assertEqual ls.size 3 (msg := "unwrapped and read")

/-! ## Poll failures

What the poller tells its operator when the endpoint says no. -/

@[test]
def statusError_separates401From403 : Test := do
  -- The endpoint answers a token it does not accept with 401, so only 401 may be reported as an
  -- expiry. A 403 is a token that authenticated and was then declined for this endpoint — it
  -- still runs agents, and calling it expired sends its owner to rotate the wrong thing.
  let body := r#"{"type":"error","error":{"type":"permission_error","message":"Not authorized"}}"#
  match statusError 403 body with
  | none   => TestM.fail "403 is a failure"
  | some e =>
    let m := e.message
    TestM.assert (m.contains "Not authorized") (msg := "the server's own explanation survives")
    TestM.assert (!(m.contains "expire")) (msg := "a 403 is not reported as an expiry")
  match statusError 401 "{\"error\":{\"message\":\"Invalid bearer token\"}}" with
  | none   => TestM.fail "401 is a failure"
  | some e =>
    TestM.assert (e.message.contains "expired") (msg := "401 is the expired-or-revoked case")

@[test]
def statusError_passesThroughTheOtherOutcomes : Test := do
  TestM.assert (statusError 200 sampleBody).isNone (msg := "200 is not an error")
  -- 429 is not classified here: `fetchUtilization` intercepts it to read `retry-after`, so
  -- `statusError` only reaches it as a generic non-2xx if the caller ever fell through.
  match statusError 429 "" with
  | some (.other m) => TestM.assert (m.contains "429") (msg := "reported as a plain HTTP failure")
  | _ => TestM.fail "429 should still be some failure"
  match statusError 500 "" with
  | some (.other m) =>
    TestM.assert (m.contains "no response body")
      (msg := "an empty body says so rather than trailing off")
  | _ => TestM.fail "expected a plain reported failure"

/-! ## Backing off

`/api/oauth/usage` allows on the order of five requests before answering `429` with a
`retry-after`, and every polling site in orchestra shares that one budget. What is testable
without a network is the reading of the response that says so, and the backoff derived from it. -/

/-- A `429` exactly as curl's `-D -` writes it: CRLF throughout, headers, a blank line, body. -/
private def rateLimitedDump : String :=
  "HTTP/2 429\r\nretry-after: 300\r\nContent-Type: application/json\r\ncf-ray: a1f9\r\n\r\n" ++
  "{\n  \"error\": {\"type\": \"rate_limit_error\"}\n}"

@[test]
def splitHeaders_separatesTheDumpFromTheBody : Test := do
  let (headers, body) := Utils.Http.splitHeaders rateLimitedDump
  TestM.assertEqual (Utils.Http.header? headers "Retry-After") (some "300")
    (msg := "found however the caller spells it")
  TestM.assertEqual (Utils.Http.header? headers "content-type") (some "application/json")
    (msg := "a value containing no colon")
  TestM.assertEqual (Utils.Http.header? headers "x-absent") none (msg := "absent stays absent")
  TestM.assert (body.startsWith "{") (msg := "the body survives intact")
  TestM.assert ((body.splitOn "rate_limit_error").length > 1) (msg := "…all of it")

@[test]
def splitHeaders_keepsTheLastBlockAndABlankLineInTheBody : Test := do
  -- A proxy answers CONNECT before the real response, so two header blocks arrive. Reading the
  -- first would report the proxy's status headers as the endpoint's.
  let dump := "HTTP/1.1 200 Connection established\r\n\r\n" ++ rateLimitedDump
  let (headers, _) := Utils.Http.splitHeaders dump
  TestM.assertEqual (Utils.Http.header? headers "retry-after") (some "300")
    (msg := "the real block wins")
  -- A body with a blank line in it must not be mistaken for a header boundary; only a block
  -- that starts with a status line is one.
  let withGap := "HTTP/2 200\r\ncontent-type: text/plain\r\n\r\nfirst\n\nsecond"
  TestM.assertEqual (Utils.Http.splitHeaders withGap).2 "first\n\nsecond"
    (msg := "the body is not re-split at its own blank line")
  -- …and a body that opens with something that reads like a status line is still a body: only
  -- an informational block or a proxy's CONNECT answer precedes the real response.
  let echoed := "HTTP/2 200\r\ncontent-type: text/plain\r\n\r\nHTTP/1.1 500\r\n\r\nnot a header"
  TestM.assertEqual (Utils.Http.header? (Utils.Http.splitHeaders echoed).1 "content-type")
    (some "text/plain") (msg := "the real block is not skipped for one quoted in the body")

@[test]
def splitHeaders_toleratesOutputThatIsAllBody : Test := do
  -- Nothing forces a caller to have asked for headers; the body must come back untouched.
  TestM.assertEqual (Utils.Http.splitHeaders "{\"ok\":true}").2 "{\"ok\":true}"
    (msg := "no dump, no change")

@[test]
def retryAfter_isReadOnlyWhenItIsACountOfSeconds : Test := do
  TestM.assertEqual (retryAfterSecs #[("retry-after", "300")]) (some 300) (msg := "a delta")
  TestM.assertEqual (retryAfterSecs #[("retry-after", "Wed, 22 Jul 2026 18:59:59 GMT")]) none
    (msg := "the date form is not mistaken for a number")
  TestM.assertEqual (retryAfterSecs #[("cf-ray", "a1f9")]) none (msg := "no header at all")

@[test]
def backoff_honoursTheServerAndFloorsIt : Test := do
  TestM.assertEqual (FetchError.rateLimited (some 300)).backoffSecs 300
    (msg := "what the server asked for")
  TestM.assertEqual (FetchError.rateLimited none).backoffSecs pollBackoffSecs
    (msg := "no retry-after ⇒ the observed default")
  -- A `retry-after: 0` would otherwise mean "retry now", and the next request would earn the
  -- same 429: the point of the backoff is that asking again immediately cannot work.
  TestM.assertEqual (FetchError.rateLimited (some 0)).backoffSecs errorBackoffSecs
    (msg := "floored")

@[test]
def backoff_appliesToOrdinaryFailuresToo : Test := do
  -- The one that used to be missing. A failed poll leaves `fetchedEpoch` untouched, so the
  -- source reads as stale forever after; with no backoff the claim path re-polls it on every
  -- tick and spends the endpoint's whole budget answering a blip.
  TestM.assertEqual (FetchError.other "curl failed (exit 6)").backoffSecs errorBackoffSecs
    (msg := "a network failure is throttled as well")
  TestM.assert (errorBackoffSecs > 0) (msg := "…by something, at least")

@[test]
def pollInterval_leavesRoomUnderTheEndpointBudget : Test := do
  -- The claim path's freshness TTL is this same constant, which is what makes it a fallback
  -- rather than a second poller: while the daemon is up nothing else ever reaches the network.
  -- Five requests per five minutes is the observed ceiling; one per source per interval, plus
  -- the occasional manual `orchestra usage`, has to fit under it.
  TestM.assert (pollIntervalSecs ≥ 60) (msg := "at most one request per source per minute")

/-! ## Availability

The point of the whole subsystem: a limit scoped to one model family must not idle the account
for every other model. -/

private def now : Int := 1784678400            -- 2026-07-22T00:00:00Z
private def later : String := "2026-07-23T00:00:00Z"
private def earlier : String := "2026-07-21T00:00:00Z"

private def scopedExhausted : SourceState :=
  { backend := "claude", label := "main"
    limits := #[
      { kind := .session, percent := 10, resetsAt := some later },
      { kind := .weeklyScoped, percent := 100, severity := "critical"
        resetsAt := some later, scopeModel := some "Fable", isActive := true }] }

@[test]
def availability_scopedLimitBlocksOnlyItsOwnModel : Test := do
  TestM.assert (availabilityOf scopedExhausted (some "claude-fable-5") now
      |>.isAvailable |> not)
    (msg := "a Fable task is blocked by the exhausted Fable limit")
  TestM.assert ((availabilityOf scopedExhausted (some "claude-sonnet-5") now).isAvailable)
    (msg := "a Sonnet task on the same account still runs")

@[test]
def availability_unknownModelIsNotBlockedByAScopedLimit : Test := do
  -- Deliberate: a task that names no model would otherwise be blocked by every scoped limit,
  -- idling the account for a week. A task that really does hit the limit is recorded by
  -- `markLimited`, which is the accurate signal.
  TestM.assert ((availabilityOf scopedExhausted none now).isAvailable)
    (msg := "no model named ⇒ scoped limits do not apply")

@[test]
def availability_unscopedLimitBlocksEverything : Test := do
  let st : SourceState :=
    { backend := "claude", label := "main"
      limits := #[{ kind := .weeklyAll, percent := 100, resetsAt := some later, isActive := true }] }
  TestM.assert (!(availabilityOf st (some "claude-sonnet-5") now).isAvailable)
    (msg := "an account-wide limit blocks a named model")
  TestM.assert (!(availabilityOf st none now).isAvailable)
    (msg := "and blocks an unnamed one too")

@[test]
def availability_expiredLimitIsNotBinding : Test := do
  let st : SourceState :=
    { backend := "claude", label := "main"
      limits := #[{ kind := .weeklyAll, percent := 100, resetsAt := some earlier, isActive := true }] }
  TestM.assert ((availabilityOf st (some "claude-sonnet-5") now).isAvailable)
    (msg := "a limit whose reset time has passed no longer blocks")

@[test]
def availability_observedBlockOutranksAQuietPoll : Test := do
  -- A run that actually came back rate-limited is authoritative even when the last poll showed
  -- nothing wrong — the poll may simply predate the run.
  let st : SourceState :=
    { backend := "claude", label := "main"
      limits := #[{ kind := .session, percent := 5 }]
      blocks := #[{ untilEpoch := some (now + 3600), reason := "agent reported a usage limit" }] }
  TestM.assert (!(availabilityOf st (some "claude-opus-4-8") now).isAvailable)
    (msg := "the recorded hit blocks")
  let expired : SourceState := { st with blocks := #[{ st.blocks[0]! with untilEpoch := some (now - 1) }] }
  TestM.assert ((availabilityOf expired (some "claude-opus-4-8") now).isAvailable)
    (msg := "and stops blocking once its window passes")

@[test]
def availability_observedBlockCanItselfBeScoped : Test := do
  let st : SourceState :=
    { backend := "claude", label := "main"
      blocks := #[{ untilEpoch := some (now + 3600), model := some "claude-opus-4-8"
                    reason := "agent reported a usage limit" }] }
  TestM.assert (!(availabilityOf st (some "claude-opus-4-8") now).isAvailable)
    (msg := "blocks the model that hit it")
  TestM.assert ((availabilityOf st (some "claude-sonnet-5") now).isAvailable)
    (msg := "but not a different one")

@[test]
def availability_twoScopesAreTwoBlocks : Test := do
  -- The case a single block slot could not hold: an account that is out of Fable for the week
  -- and out of its session window for the hour. Both are true, and the one that expires first
  -- must not take the other with it.
  let st : SourceState :=
    { backend := "claude", label := "main"
      blocks := #[
        { untilEpoch := some (now + 604800), model := some "Fable", reason := "Fable limit" },
        { untilEpoch := some (now + 3600), reason := "session limit" }] }
  TestM.assert (!(availabilityOf st (some "claude-sonnet-5") now).isAvailable)
    (msg := "the account-wide block covers a model the scoped one does not")
  let later := now + 7200
  TestM.assert ((availabilityOf st (some "claude-sonnet-5") later).isAvailable)
    (msg := "once the session window passes, Sonnet is runnable again")
  TestM.assert (!(availabilityOf st (some "claude-fable-5") later).isAvailable)
    (msg := "but Fable is still blocked, and was not forgotten with the session block")

@[test]
def availability_reportsTheBlockThatLiftsLast : Test := do
  -- Two blocks can cover one model now. Naming the earlier reset would send the caller back at
  -- a time the source is still shut, to be turned away and made to wait again.
  let st : SourceState :=
    { backend := "claude", label := "main"
      blocks := #[
        { untilEpoch := some (now + 3600), reason := "session limit" },
        { untilEpoch := some (now + 604800), model := some "Fable", reason := "Fable limit" }] }
  let reportedReset (s : SourceState) : Option (Option Int) :=
    match availabilityOf s (some "claude-fable-5") now with
    | .available   => none
    | .blocked u _ => some u
  TestM.assertEqual (reportedReset st) (some (some (now + 604800)))
    (msg := "the later of the two applicable resets is the one reported")
  -- Order in the array must not decide it.
  TestM.assertEqual (reportedReset { st with blocks := st.blocks.reverse })
    (some (some (now + 604800)))
    (msg := "and the answer does not depend on insertion order")

@[test]
def availability_aBlockWithNoExpiryOutlastsOneWithAnExpiry : Test := do
  let st : SourceState :=
    { backend := "claude", label := "main"
      blocks := #[
        { untilEpoch := some (now + 3600), reason := "session limit" },
        { untilEpoch := none, reason := "credit or entitlement problem" }] }
  match availabilityOf st (some "claude-fable-5") now with
  | .available   => TestM.fail "an open-ended block still blocks"
  | .blocked u r => do
    TestM.assertEqual u none (msg := "no reset time is reported, because there is none to report")
    TestM.assertEqual r "credit or entitlement problem"
      (msg := "the reason is the open-ended one, not the window that will pass")

@[test]
def availability_aScopedHitOnAnUnnamedModelStillHoldsBackUnnamedTasks : Test := do
  -- The regression that scoping an observed hit to the provider's family would otherwise
  -- introduce. A queued task names no model, runs the CLI's default family, and is told it has
  -- reached its Opus limit. Scoped to "Opus" alone the block is invisible to the next task —
  -- which also names no model, also takes the default, and hits the same wall — because
  -- `modelMatchesScope` reads "no model named" as matching no scope.
  let scopedOnly : SourceState :=
    { backend := "claude", label := "main"
      blocks := #[{ untilEpoch := some (now + 3600), model := some "Opus", reason := "Opus" }] }
  TestM.assert ((availabilityOf scopedOnly none now).isAvailable)
    (msg := "a poll-derived scoped limit leaves an unnamed task alone, as it always has")
  let observed : SourceState :=
    { scopedOnly with
      blocks := #[{ untilEpoch := some (now + 3600), model := some "Opus", reason := "Opus"
                    coversUnscoped := true }] }
  TestM.assert (!(availabilityOf observed none now).isAvailable)
    (msg := "but an observed hit from an unnamed run holds back the next unnamed run")
  TestM.assert ((availabilityOf observed (some "claude-sonnet-5") now).isAvailable)
    (msg := "and still leaves a different family runnable")
  TestM.assert (!(availabilityOf observed (some "claude-opus-4-8") now).isAvailable)
    (msg := "and still blocks the family it named")

@[test]
def markOk_retiresOnlyWhatTheRunDisproves : Test := do
  -- `markOk`'s filter, exercised directly so the interesting case needs no disk. A completed
  -- Sonnet run proves the account-wide window has passed and proves nothing about Fable; the
  -- Fable block has to survive, or the next Fable task rediscovers the limit by hitting it.
  let blocks : Array Block := #[
    { untilEpoch := some (now + 604800), model := some "Fable", reason := "Fable limit" },
    { untilEpoch := some (now + 3600), reason := "session limit" }]
  let afterSonnet := blocks.filter fun b => blockIsLive b now && !blockApplies b (some "claude-sonnet-5")
  TestM.assertEqual afterSonnet.size 1 (msg := "one block survives a Sonnet success")
  TestM.assertEqual (afterSonnet[0]!.model) (some "Fable")
    (msg := "and it is the Fable one")
  let afterFable := blocks.filter fun b => blockIsLive b now && !blockApplies b (some "claude-fable-5")
  TestM.assertEqual afterFable.size 0
    (msg := "a Fable success retires both the Fable block and the account-wide one")

@[test]
def markLimited_foldsASecondReadingInsteadOfReplacingIt : Test := do
  -- Workers run concurrently, so two tasks dispatched to one source before any block existed
  -- both report the same limit, knowing different things about it. The merge is what stops the
  -- later, less informed reading dropping what the earlier one learned. Exercised as the pure
  -- fold `markLimited` performs, so no disk or clock is needed.
  let prior : Block :=
    { untilEpoch := some (now + 21600), model := some "Opus"
      reason := "credit or entitlement problem", coversUnscoped := true, notAWindow := true }
  -- The second reading: a bare rate-limit event, naming a model id, knowing neither flag.
  let fresh : Block :=
    { untilEpoch := some (now + 3600), model := some "claude-opus-4-8", reason := "usage limit" }
  let merged := mergeBlock fresh prior
  TestM.assert merged.coversUnscoped
    (msg := "the later reading does not drop that unnamed tasks are covered")
  TestM.assert merged.notAWindow
    (msg := "nor that this is not a window at all")
  TestM.assertEqual merged.untilEpoch (some (now + 21600))
    (msg := "and cannot shorten it from six hours to one")
  TestM.assertEqual merged.reason "credit or entitlement problem"
    (msg := "the reason keeps explaining the stronger fact")
  TestM.assertEqual merged.model (some "Opus")
    (msg := "the broader spelling survives, or the block stops covering tasks asking for 'opus'")
  -- And the fold does not depend on which reading arrived first.
  TestM.assertEqual (mergeBlock prior fresh).model (some "Opus")
    (msg := "which way round the two readings arrive does not change the scope")
  TestM.assertEqual (mergeBlock prior fresh).untilEpoch (some (now + 21600))
    (msg := "nor the expiry")

@[test]
def mergeBlock_aScopeNarrowedByOrderWouldLoseCoverage : Test := do
  -- Why the broader spelling matters, stated as the thing that actually breaks.
  let broad : Block := { untilEpoch := some (now + 3600), model := some "Opus" }
  let narrow : Block := { untilEpoch := some (now + 3600), model := some "claude-opus-4-8" }
  TestM.assert (blockApplies broad (some "opus"))
    (msg := "a block scoped to the display name catches a task asking for the alias")
  TestM.assert (!(blockApplies narrow (some "opus")))
    (msg := "one scoped to the dated id does not")
  TestM.assert (blockApplies (mergeBlock narrow broad) (some "opus"))
    (msg := "so the merge must keep the broader of the two")

@[test]
def sameScope_reconcilesDisplayNameAndModelId : Test := do
  -- `markLimited` upserts by scope. The provider writes "Fable" and a task asks for
  -- "claude-fable-5"; read as different scopes they would accumulate as two blocks on one
  -- window, each expiring on its own schedule.
  TestM.assert (sameScope (some "Fable") (some "claude-fable-5"))
    (msg := "display name and model id are one scope")
  TestM.assert (sameScope (some "claude-fable-5") (some "Fable"))
    (msg := "and in the other order too")
  TestM.assert (sameScope none none) (msg := "account-wide matches account-wide")
  TestM.assert (!(sameScope none (some "Fable")))
    (msg := "account-wide is not the same window as a scoped one")
  TestM.assert (!(sameScope (some "Opus") (some "claude-fable-5")))
    (msg := "different families are different scopes")

@[test]
def sourceState_liftsALegacySingleBlock : Test := do
  -- State files written before blocks were a set carry one `"block"` object. Dropping it on
  -- upgrade would forget a live limit and send the next task into it.
  let legacy := "{\"backend\":\"claude\",\"label\":\"main\",\"limits\":[],\
    \"block\":{\"until_epoch\":1785351600,\"model\":\"Fable\",\"reason\":\"observed\"}}"
  match Json.parse legacy >>= fun j => (Lean.FromJson.fromJson? j : Except String SourceState) with
  | .error e => TestM.fail s!"a legacy state file should still parse: {e}"
  | .ok st   =>
    TestM.assertEqual st.blocks.size 1 (msg := "the single block is lifted into the set")
    TestM.assertEqual (st.blocks[0]!.model) (some "Fable") (msg := "with its scope intact")

@[test]
def sourceState_oneBadBlockDoesNotDropTheRest : Test := do
  -- Decoding the array as a whole would fail on the bad element, fall through to a `"block"`
  -- key that is not there, and forget every block the source had — leaving it looking runnable,
  -- which is the failure the migration exists to prevent, reached by another route.
  let mixed := "{\"backend\":\"claude\",\"label\":\"main\",\"limits\":[],\"blocks\":[\
    {\"until_epoch\":1785351600,\"model\":\"Fable\",\"reason\":\"observed\"},\
    \"not an object\",\
    {\"until_epoch\":1785351600,\"reason\":\"session\"}]}"
  match Json.parse mixed >>= fun j => (Lean.FromJson.fromJson? j : Except String SourceState) with
  | .error e => TestM.fail s!"a partly unreadable block list should still parse: {e}"
  | .ok st   =>
    TestM.assertEqual st.blocks.size 2 (msg := "the readable blocks survive the unreadable one")
    TestM.assertEqual (st.blocks[0]!.model) (some "Fable") (msg := "and keep their scopes")

@[test]
def sourceState_absentBlocksStayAbsent : Test := do
  -- The reason `Block`'s decoder rejects non-objects: `getObjValAs?` reads a missing key as
  -- `Json.null`, and a block with no expiry reads as "blocked forever".
  let bare := "{\"backend\":\"claude\",\"label\":\"main\",\"limits\":[]}"
  match Json.parse bare >>= fun j => (Lean.FromJson.fromJson? j : Except String SourceState) with
  | .error e => TestM.fail s!"a state file with no block should parse: {e}"
  | .ok st   => TestM.assertEqual st.blocks.size 0 (msg := "no block means no block")

@[test]
def modelMatchesScope_matchesAliasAndFullId : Test := do
  TestM.assert (modelMatchesScope "Opus" (some "claude-opus-4-8")) (msg := "dated id")
  TestM.assert (modelMatchesScope "Opus" (some "opus")) (msg := "bare alias")
  TestM.assert (!(modelMatchesScope "Opus" (some "claude-sonnet-5"))) (msg := "different family")
  TestM.assert (!(modelMatchesScope "Opus" none)) (msg := "no model named")

/-! ## Selection -/

private def candidate (label : String) (i : Nat) (avail : Availability)
    (pressure : Nat := 0) (lastUsed : Int := 0) : Candidate :=
  { label, availability := avail, pressure, lastUsed, index := i }

@[test]
def chooseFrom_orderedTakesTheFirstUsableSource : Test := do
  let cs := #[
    candidate "primary" 0 (.blocked (some (now + 60)) "weekly_all at 100%"),
    candidate "secondary" 1 .available,
    candidate "tertiary" 2 .available]
  TestM.assertEqual (chooseFrom .ordered cs) (.ok "secondary")
    (msg := "falls through the limited one, stops at the first free one")

@[test]
def chooseFrom_distributePrefersTheLeastConsumed : Test := do
  let cs := #[
    candidate "heavy" 0 .available (pressure := 80),
    candidate "light" 1 .available (pressure := 10)]
  TestM.assertEqual (chooseFrom .distribute cs) (.ok "light")
    (msg := "lowest utilisation wins regardless of order")

@[test]
def chooseFrom_distributeBreaksTiesByLeastRecentlyUsed : Test := do
  let cs := #[
    candidate "recent" 0 .available (pressure := 50) (lastUsed := now),
    candidate "stale"  1 .available (pressure := 50) (lastUsed := now - 600)]
  TestM.assertEqual (chooseFrom .distribute cs) (.ok "stale")
    (msg := "equal pressure ⇒ the one used longest ago")

/-- Choose repeatedly, stamping each winner the way `markUsed` does, and report the order.

    `tick` stands in for the dispatch clock. It is strictly increasing per selection because the
    real one is nanoseconds: the first two attempts at this used seconds and then milliseconds,
    and both let a burst of claims share a stamp — every source then ties, the index tiebreak
    fires every time, and `distribute` quietly collapses onto one account. -/
private def simulateDispatches (mode : AuthMode) (labels : List String) (n : Nat) : List String :=
  Id.run do
    let mut lastUsed : List (String × Int) := labels.map (·, 0)
    let mut picked : List String := []
    for round in List.range n do
      let cs := (labels.zipIdx).toArray.map fun (l, i) =>
        ({ label := l, availability := .available, pressure := 0
           lastUsed := (lastUsed.find? (·.1 == l)).map (·.2) |>.getD 0
           index := i } : Candidate)
      match chooseFrom mode cs 0 with
      | .error _ => pure ()
      | .ok winner =>
        picked := picked ++ [winner]
        lastUsed := lastUsed.map fun (l, t) => if l == winner then (l, (round : Int) + 1) else (l, t)
    return picked

@[test]
def distribute_roundRobinsAcrossEquallyLoadedSources : Test := do
  TestM.assertEqual (simulateDispatches .distribute ["a", "b"] 6)
    ["a", "b", "a", "b", "a", "b"] (msg := "two sources alternate")
  TestM.assertEqual (simulateDispatches .distribute ["a", "b", "c"] 6)
    ["a", "b", "c", "a", "b", "c"] (msg := "three sources cycle")

@[test]
def ordered_staysOnTheFirstSource : Test := do
  -- The contrast that gives the test above its meaning: `ordered` must *not* rotate, however
  -- recently the first source was used.
  TestM.assertEqual (simulateDispatches .ordered ["a", "b"] 4)
    ["a", "a", "a", "a"] (msg := "ordered keeps using the first usable source")

@[test]
def chooseFrom_distributeSkipsLimitedSourcesEvenWhenIdle : Test := do
  -- An untouched but exhausted account has pressure 0 and has never been used, so it would win
  -- every tiebreak if availability were not checked first.
  let cs := #[
    candidate "exhausted" 0 (.blocked (some (now + 60)) "weekly_all at 100%") (pressure := 0),
    candidate "usable"    1 .available (pressure := 90) (lastUsed := now)]
  TestM.assertEqual (chooseFrom .distribute cs) (.ok "usable")
    (msg := "availability is a gate, not a preference")

@[test]
def chooseFrom_reportsWhichSourceFreesUpFirst : Test := do
  -- With everything limited the caller is going to wait; the useful part of the message is how
  -- long, so the soonest reset is the one named.
  let cs := #[
    candidate "a" 0 (.blocked (some (now + 7200)) "weekly_all at 100%"),
    candidate "b" 1 (.blocked (some (now + 60)) "session at 100%")]
  match chooseFrom .ordered cs now with
  | .ok l    => TestM.fail s!"expected no source to be usable, got {l}"
  | .error e =>
    TestM.assert ((e.splitOn "b frees up in 1m").length > 1)
      (msg := s!"names the soonest source and when, got: {e}")
    TestM.assert ((e.splitOn "session at 100%").length > 1)
      (msg := s!"and says which limit is binding, got: {e}")

@[test]
def chooseFrom_noSourcesConfiguredIsItsOwnMessage : Test := do
  match chooseFrom .ordered #[] with
  | .ok l    => TestM.fail s!"expected an error, got {l}"
  | .error e =>
    TestM.assert ((e.splitOn "no authentication sources").length > 1)
      (msg := s!"distinguishes 'none configured' from 'all limited', got: {e}")

/-! ## Candidate resolution

Which labels a task is allowed to run on, given the three ways config can express it. -/

/-- The candidates `resolutionFor` yields, dropping the mode. Most of these tests are about
    which labels are offered, and the production callers all want both halves. -/
private def candidatesFor (cfg : AppConfig) (backend : String) (authSources : List String)
    (authSource : Option String) : List String :=
  (resolutionFor cfg backend authSources authSource none).1

private def cfgWithSources (labels : List String) (dflt : List String := [])
    (dfltMode : AuthMode := .ordered) (pollUsage : Bool := true) : AppConfig :=
  { appId := 1, privateKeyPath := ""
    agentAuthConfigs := #[{
      name := "claude"
      defaultAuthSources := dflt
      defaultAuthMode := dfltMode
      pollUsage
      authSources := labels.toArray.map fun l => { label := l, kind := .oauthToken "t" } }] }

@[test]
def candidatesFor_explicitListWins : Test := do
  let cfg := cfgWithSources ["a", "b", "c"] (dflt := ["a"])
  TestM.assertEqual (candidatesFor cfg "claude" ["b", "c"] (some "a")) ["b", "c"]
    (msg := "auth_sources beats both auth_source and the configured default")

@[test]
def candidatesFor_singleForcedLabelIsStillChecked : Test := do
  -- A pinned source gets one candidate rather than none, so a task pinned to an exhausted
  -- account waits like everything else instead of being dispatched into the wall.
  let cfg := cfgWithSources ["a", "b"]
  TestM.assertEqual (candidatesFor cfg "claude" [] (some "b")) ["b"]
    (msg := "auth_source yields exactly itself")

@[test]
def candidatesFor_fallsBackToConfiguredDefaults : Test := do
  TestM.assertEqual (candidatesFor (cfgWithSources ["a", "b"] (dflt := ["b"])) "claude" [] none)
    ["b"] (msg := "default_auth_source")
  TestM.assertEqual (candidatesFor (cfgWithSources ["only"]) "claude" [] none) ["only"]
    (msg := "a sole source needs no naming")

/-! ### The configured pool

A task that names nothing takes both the candidates and the mode from config. This is the only
way several dispatch paths can reach more than one account: `Listener.buildRoleEntry` writes no
candidates onto a dispatched role, and a concert step's YAML has no field to write them in. -/

@[test]
def resolutionFor_poolIsWalkedInTheConfiguredMode : Test := do
  let cfg := cfgWithSources ["a", "b"] (dflt := ["a", "b"]) (dfltMode := .distribute)
  let (candidates, mode) := resolutionFor cfg "claude" [] none none
  TestM.assertEqual candidates ["a", "b"] (msg := "a pooled default offers every member")
  -- A task that names no mode must not be read as asking for `ordered`: that is what every
  -- path with no auth field to fill carries, and under it `chooseFrom` takes the head every
  -- time — one account.
  TestM.assertEqual (mode == .distribute) true
    (msg := "the mode comes from the config the pool came from, not from the task")

@[test]
def resolutionFor_aTaskThatNamesItsOwnListKeepsItsOwnMode : Test := do
  let cfg := cfgWithSources ["a", "b"] (dflt := ["a", "b"]) (dfltMode := .distribute)
  let (candidates, mode) := resolutionFor cfg "claude" ["b", "a"] none (some .ordered)
  TestM.assertEqual candidates ["b", "a"] (msg := "an explicit list still wins over the pool")
  TestM.assertEqual (mode == .ordered) true
    (msg := "a task that brings its own list brings its own mode")

@[test]
def resolutionFor_anExplicitModeSurvivesTheConfiguredPool : Test := do
  -- `auth_mode` without `auth_sources` is a real combination: a listener action can set one and
  -- not the other, and `orchestra interactive --auth_mode` sets only the mode. The pool supplies
  -- the candidates there, but the mode the operator asked for is still the mode.
  let cfg := cfgWithSources ["a", "b"] (dflt := ["a", "b"]) (dfltMode := .ordered)
  let (candidates, mode) := resolutionFor cfg "claude" [] none (some .distribute)
  TestM.assertEqual candidates ["a", "b"] (msg := "still the pool's candidates")
  TestM.assertEqual (mode == .distribute) true
    (msg := "an explicitly named mode is not discarded by the pool")

@[test]
def resolutionFor_poolDropsLabelsTheBackendDoesNotConfigure : Test := do
  -- A typo'd pool member would otherwise *win* under distribute: nothing has been recorded
  -- against it, so it reads as the least-consumed source, and the task then dies in
  -- `resolveAuthEnv` against a source that does not exist instead of using the healthy one.
  let cfg := cfgWithSources ["work"] (dflt := ["work", "personl"]) (dfltMode := .distribute)
  TestM.assertEqual (candidatesFor cfg "claude" [] none) ["work"]
    (msg := "an unconfigured label is dropped from the pool")
  -- All of them unknown is not a pool at all; resolution declines and the existing error path
  -- reports it, rather than dispatching onto a label with no credentials behind it.
  let allBad := cfgWithSources ["work", "play"] (dflt := ["nope", "nada"])
  TestM.assertEqual (candidatesFor allBad "claude" [] none) []
    (msg := "a wholly unconfigured pool resolves to nothing")

@[test]
def resolutionFor_pinningStillPins : Test := do
  -- `"default_auth_source": "houyi"` parses to a one-element pool, so the pre-pool config keeps
  -- resolving to exactly that source whatever mode sits beside it.
  let cfg := cfgWithSources ["a", "b"] (dflt := ["b"]) (dfltMode := .distribute)
  TestM.assertEqual (candidatesFor cfg "claude" [] none) ["b"]
    (msg := "a single configured default is still a pin, not a pool")
  -- And an explicitly pinned task is never widened to the pool.
  let pooled := cfgWithSources ["a", "b"] (dflt := ["a", "b"]) (dfltMode := .distribute)
  TestM.assertEqual (candidatesFor pooled "claude" [] (some "a")) ["a"]
    (msg := "auth_source beats the pool")

@[test]
def candidatesFor_legacyConfigHasNothingToChooseBetween : Test := do
  -- No `agents` block at all: the flat-token config that predates named sources. Reporting
  -- "no candidates" rather than an error is what keeps those installs working.
  let cfg : AppConfig := { appId := 1, privateKeyPath := "" }
  TestM.assertEqual (candidatesFor cfg "claude" [] none) []
    (msg := "nothing named, nothing to select")
  -- Likewise when several sources exist but none was chosen: resolution declines rather than
  -- guessing, and `resolveAuthEnv` produces its "specify one" error.
  TestM.assertEqual (candidatesFor (cfgWithSources ["a", "b"]) "claude" [] none) []
    (msg := "ambiguous config is left to the existing error path")

/-! ## Spelling the default

One key carries both forms, so there is no second field to disagree with the first about what a
task that names nothing gets. -/

private def parseAuthConfig (json : String) : Except String AgentAuthConfig := do
  Lean.FromJson.fromJson? (← Lean.Json.parse json)

@[test]
def defaultAuthSource_stringStillParsesAsAPin : Test := do
  match parseAuthConfig "{\"name\":\"claude\",\"default_auth_source\":\"houyi\",\
                          \"auth_sources\":[]}" with
  | .error e => TestM.fail s!"fromJson: {e}"
  | .ok a =>
    TestM.assertEqual a.defaultAuthSources ["houyi"] (msg := "a bare string is a one-source pool")
    TestM.assertEqual (a.defaultAuthMode == .ordered) true (msg := "and the mode is irrelevant")

@[test]
def defaultAuthSource_listParsesAsAPool : Test := do
  match parseAuthConfig "{\"name\":\"claude\",\"default_auth_source\":[\"contact\",\"houyi\"],\
                          \"default_auth_mode\":\"distribute\",\"auth_sources\":[]}" with
  | .error e => TestM.fail s!"fromJson: {e}"
  | .ok a =>
    TestM.assertEqual a.defaultAuthSources ["contact", "houyi"] (msg := "both sources pooled")
    TestM.assertEqual (a.defaultAuthMode == .distribute) true (msg := "default_auth_mode read")

@[test]
def defaultAuthSource_malformedIsRejected : Test := do
  -- Swallowing this would leave the pool walking in `ordered` — pinned to its first member,
  -- which is the failure the pool exists to prevent — with nothing in the logs to say so.
  match parseAuthConfig "{\"name\":\"claude\",\"default_auth_source\":[\"a\",\"b\"],\
                          \"default_auth_mode\":\"distributed\",\"auth_sources\":[]}" with
  | .ok _    => TestM.fail "a misspelled default_auth_mode should not parse as ordered"
  | .error _ => TestM.assert true (msg := "typo in default_auth_mode is reported")

@[test]
def defaultAuthSource_malformedSourceIsRejected : Test := do
  match parseAuthConfig "{\"name\":\"claude\",\"default_auth_source\":7,\"auth_sources\":[]}" with
  | .ok _    => TestM.fail "a non-label default_auth_source should not parse as absent"
  | .error _ => TestM.assert true (msg := "malformed default_auth_source is reported")

@[test]
def defaultAuthSource_absentIsEmpty : Test := do
  match parseAuthConfig "{\"name\":\"claude\",\"auth_sources\":[]}" with
  | .error e => TestM.fail s!"fromJson: {e}"
  | .ok a =>
    -- Nothing configured, so the sole-source rule decides and an ambiguous config still errors.
    TestM.assert a.defaultAuthSources.isEmpty (msg := "no default configured")

/-! ## Opt-out of polling

Polling costs a probe request now, so a backend can turn it off and rely on observed hits. -/

@[test]
def pollUsage_defaultsToTrue : Test := do
  -- Absent from the config, polling stays on — no silent behaviour change for existing installs.
  match Lean.Json.parse "{\"name\":\"claude\",\"auth_sources\":[]}" with
  | .error e => TestM.fail s!"parse: {e}"
  | .ok j => match (Lean.FromJson.fromJson? j : Except String AgentAuthConfig) with
    | .error e => TestM.fail s!"fromJson: {e}"
    | .ok a    => TestM.assert a.pollUsage (msg := "omitted poll_usage defaults to true")

@[test]
def pollUsage_falseParses : Test := do
  match Lean.Json.parse "{\"name\":\"claude\",\"poll_usage\":false,\"auth_sources\":[]}" with
  | .error e => TestM.fail s!"parse: {e}"
  | .ok j => match (Lean.FromJson.fromJson? j : Except String AgentAuthConfig) with
    | .error e => TestM.fail s!"fromJson: {e}"
    | .ok a    => TestM.assert (!a.pollUsage) (msg := "poll_usage:false disables it")

@[test]
def pollingEnabled_readsTheBackendFlag : Test := do
  let on  := cfgWithSources ["a"]
  let off := cfgWithSources ["a"] (pollUsage := false)
  TestM.assert (pollingEnabled on "claude") (msg := "default on")
  TestM.assert (!pollingEnabled off "claude") (msg := "honours poll_usage:false")
  TestM.assert (pollingEnabled off "unknown")
    (msg := "an unconfigured backend is moot, not disabled")

/-! ## Detection

The classifier that decides a run was cut short by a limit rather than broken. -/

@[test]
def usageLimitError_matchesWhatTheCliActuallySays : Test := do
  -- Observed verbatim in a Claude Code transcript for a subscription limit. It contains none of
  -- the phrases the original pattern list looked for.
  TestM.assert (AgentDef.stdUsageLimitError 1
      "You've reached your Fable 5 limit. Run /usage-credits to continue or switch models.")
    (msg := "the subscription-limit wording is recognised")
  TestM.assert (AgentDef.stdUsageLimitError 1
      "{\"type\":\"rate_limit_error\",\"message\":\"This request would exceed your account's rate limit.\"}")
    (msg := "the API-key 429 body is recognised")

/-! ### Which limit

Detecting a limit is not enough to record one: the scope decides how much of an account gets
closed, and the message is the only thing that knows it. -/

@[test]
def classifyUsageLimit_readsTheFamilyTheProviderNamed : Test := do
  TestM.assertEqual
    (AgentDef.classifyUsageLimit
      "You've reached your Fable 5 limit. Run /usage-credits to continue or switch models.")
    (AgentDef.LimitScope.family "Fable")
    (msg := "the family in the message, not the model the task asked for")
  TestM.assertEqual
    (AgentDef.classifyUsageLimit "You've reached your Opus limit for this week.")
    (AgentDef.LimitScope.family "Opus")
    (msg := "another family, same shape")

@[test]
def classifyUsageLimit_anUnscopedWindowIsAccountWide : Test := do
  -- The direction that costs the most: a Fable task tripping the account-wide session window.
  -- Scoped to Fable, every other family keeps dispatching into an account with nothing left.
  TestM.assertEqual
    (AgentDef.classifyUsageLimit "You've reached your usage limit. Try again after 3pm.")
    AgentDef.LimitScope.account
    (msg := "a window that names no family closes the account, not one model")
  TestM.assertEqual
    (AgentDef.classifyUsageLimit
      "{\"type\":\"rate_limit_error\",\"message\":\"This request would exceed your account's rate limit.\"}")
    AgentDef.LimitScope.account
    (msg := "a transport-level 429 carries no model scope")

@[test]
def classifyUsageLimit_creditsAreNotAWindow : Test := do
  -- anthropics/claude-code#79597: the client refuses Fable on a plan that covers it, because a
  -- setup-token cannot state the plan. That never clears on a clock, so it must not be recorded
  -- as a window someone can wait out.
  TestM.assertEqual
    (AgentDef.classifyUsageLimit "Fable requires usage credits to use. Add credits to continue.")
    (AgentDef.LimitScope.credits (some "Fable"))
    (msg := "an entitlement refusal keeps the family and is marked as not-a-window")
  TestM.assertEqual
    (AgentDef.classifyUsageLimit "Your credit balance is too low to run this request.")
    (AgentDef.LimitScope.credits none)
    (msg := "a balance problem with no family named")

@[test]
def classifyUsageLimit_saysUnknownRatherThanGuessing : Test := do
  -- `unknown` is what makes this safe to land: the caller falls back to the model the task
  -- asked for, which is exactly what it did before classification existed.
  TestM.assertEqual (AgentDef.classifyUsageLimit "quota exceeded")
    AgentDef.LimitScope.unknown
    (msg := "a phrase with no scope in it does not invent one")

@[test]
def classifyUsageLimit_doesNotReadAFamilyOutOfAWholeTranscript : Test := do
  -- The text classified is a whole run's stderr with the final result appended, not a provider
  -- message. An unbounded span after a marker phrase would hand the family search the entire
  -- transcript, and an account-wide failure would be recorded as a one-family block — leaving
  -- every other family dispatching into a spent account.
  let transcript :=
    "reached the maximum number of retries; switching the reviewer to Sonnet and continuing " ++
    String.ofList (List.replicate 500 'x') ++
    " This request would exceed your account's rate limit."
  TestM.assertEqual (AgentDef.classifyUsageLimit transcript)
    AgentDef.LimitScope.account
    (msg := "a family named far from any 'limit' does not scope the block")
  -- The provider's real message lands *after* the stderr, so an early false marker must not
  -- outvote it: every occurrence is examined, not just the first.
  let both :=
    "reached the end of the Sonnet migration notes\n" ++
    "You've reached your Fable 5 limit."
  TestM.assertEqual (AgentDef.classifyUsageLimit both)
    (AgentDef.LimitScope.family "Fable")
    (msg := "the real message wins over an earlier marker that named nothing")

@[test]
def classifyUsageLimit_theMessageThatEndedTheRunWins : Test := do
  -- stderr carries a limit the run recovered from and kept going; the result event carries the
  -- one that actually stopped it. Reading the first match rather than the last would scope an
  -- account-wide block to Opus and leave every other family dispatching into a spent account.
  let text :=
    "You've reached your Opus limit, now using Sonnet\n" ++
    "You've reached your usage limit."
  TestM.assertEqual (AgentDef.classifyUsageLimit text)
    AgentDef.LimitScope.account
    (msg := "the later message decides, not the earlier one")
  -- And the other way round, so this is about position and not about preferring `account`.
  let reversed :=
    "You've reached your usage limit.\n" ++
    "You've reached your Opus limit."
  TestM.assertEqual (AgentDef.classifyUsageLimit reversed)
    (AgentDef.LimitScope.family "Opus")
    (msg := "position decides, in either direction")

@[test]
def usageLimitError_catchesTheEntitlementRefusal : Test := do
  -- anthropics/claude-code#79597. Detection has to fire on this or nothing downstream runs: the
  -- task comes back `failed` rather than `unfinished`, no block is recorded, and the next queued
  -- task is dispatched into the same refusal. Every branch built for the credits case was
  -- unreachable until this phrase was recognised.
  let refusal := "Fable requires usage credits to use. Add credits to continue."
  TestM.assert (AgentDef.stdUsageLimitError 1 refusal)
    (msg := "the entitlement refusal is recognised as a limit at all")
  TestM.assertEqual (AgentDef.classifyUsageLimit refusal)
    (AgentDef.LimitScope.credits (some "Fable"))
    (msg := "and then classifies as credits, scoped to the family it names")

@[test]
def classifyUsageLimit_anAccountWindowWithoutTheWordUsage : Test := do
  -- "5-hour limit reached" is a phrase detection already recognised but the marker list did not,
  -- so it produced no window, classified as unknown, and fell back to the model the task asked
  -- for — recording an account-wide limit as a block on whichever family happened to be running.
  TestM.assertEqual (AgentDef.classifyUsageLimit "5-hour limit reached ∙ resets 3pm")
    AgentDef.LimitScope.account
    (msg := "an account-wide limit that never says 'usage' is still account-wide")

@[test]
def classifyUsageLimit_creditsProseDoesNotOutrankTheRealWindow : Test := do
  -- The credits test used to be a search of the whole text, so an agent that merely wrote
  -- "credit balance" in its summary turned a one-hour window into a six-hour account-wide block
  -- with its reset thrown away and a reason telling the operator no reset would clear it.
  let summary :=
    "I added a check that refuses the request when the credit balance is too low, " ++
    "and wired it into the billing page.\n" ++
    "You've reached your usage limit. Try again after 3pm."
  TestM.assertEqual (AgentDef.classifyUsageLimit summary)
    AgentDef.LimitScope.account
    (msg := "credits prose far from the limit report does not decide it")
  let entitlement :=
    "this model requires usage credits before it will run.\n" ++
    "{\"type\":\"rate_limit_error\",\"message\":\"slow down\"}"
  TestM.assertEqual (AgentDef.classifyUsageLimit entitlement)
    AgentDef.LimitScope.account
    (msg := "and neither does an entitlement phrase the later report has moved past")

@[test]
def classifyUsageLimit_creditsDoesNotScopeItselfFromStrayProse : Test := do
  -- Same hazard on the credits path, and worse: a wrongly scoped credits block leaves the rest
  -- of the account dispatching into a hard billing failure that no reset will clear.
  let transcript :=
    "I moved the reviewer to Opus.\n" ++ String.ofList (List.replicate 300 'y') ++
    "\nYour credit balance is too low to run this request."
  TestM.assertEqual (AgentDef.classifyUsageLimit transcript)
    (AgentDef.LimitScope.credits none)
    (msg := "a family mentioned elsewhere in the run does not scope a credits block")

@[test]
def classifyUsageLimit_doesNotReadFamiliesOutOfOrdinaryProse : Test := do
  -- The span between "your" and "limit" is what is searched, not the whole output. A coding
  -- agent mentions model names routinely, and stderr is part of the text being classified.
  TestM.assertEqual
    (AgentDef.classifyUsageLimit
      "I switched the reviewer to Opus. You've reached your usage limit.")
    AgentDef.LimitScope.account
    (msg := "a family mentioned elsewhere does not scope the block")

@[test]
def usageLimitError_doesNotFireOnOrdinaryOutput : Test := do
  TestM.assert (!(AgentDef.stdUsageLimitError 1 "error: could not resolve host api.github.com"))
    (msg := "a network failure is not a usage limit")
  TestM.assert (!(AgentDef.stdUsageLimitError 1 "added a rate limiter to the upload path"))
    (msg := "an agent discussing rate limiting is not a usage limit")
  TestM.assert (!(AgentDef.stdUsageLimitError 0 "You've reached your Fable 5 limit."))
    (msg := "a clean exit is not a usage limit whatever the text says")

@[test]
def rateLimitEvent_yieldsItsResetTime : Test := do
  -- Suppressed from the console, but the reset time it carries is what lets an observed hit be
  -- recorded with a real expiry instead of a default backoff.
  let line := r#"{"type":"rate_limit_event","rate_limit":{"status":"rejected","resets_at":"2026-07-22T18:59:59Z"}}"#
  match StreamFormat.parseEvent line with
  | some (.rateLimit (some r)) =>
    TestM.assertEqual r "2026-07-22T18:59:59Z" (msg := "reset time found wherever it is nested")
  | some (.rateLimit none) => TestM.fail "parsed the event but found no reset time"
  | _ => TestM.fail "expected a rateLimit event"

@[test]
def rateLimitEvent_withoutAResetTimeIsStillAnEvent : Test := do
  match StreamFormat.parseEvent r#"{"type":"rate_limit_event"}"# with
  | some (.rateLimit none) => TestM.assert true
  | _ => TestM.fail "expected a rateLimit event with no reset time"

/-! ## History

The graphs on the dashboard are drawn from windows rolled up out of polls, so the rules that
decide where one window ends and the next begins are what decides whether a graph is true.
`recordWindows` is pure, which is what makes every one of those rules reachable here: a window
rolling over, a poll landing after a gap in polling, a source that reports no reset time at all.
-/

private def sessionAt (percent : Nat) (resetsAt : Option String := none) : Limit :=
  { kind := .session, group := "session", percent, resetsAt }

private def weeklyAt (percent : Nat) (resetsAt : Option String := none) : Limit :=
  { kind := .weeklyAll, group := "weekly", percent, resetsAt }

/-- Two instants an hour apart, and the reset time of the window they are both inside. -/
private def t0 : Int := 1784851200                 -- 2026-07-24T00:00:00Z
private def t1 : Int := t0 + 3600
private def reset1 : String := "2026-07-24T04:00:00Z"
private def reset2 : String := "2026-07-24T09:00:00Z"

@[test]
def recordWindows_foldsASecondPollIntoTheWindowItIsIn : Test := do
  let first := recordWindows #[] #[sessionAt 12 (some reset1)] t0
  let both  := recordWindows first #[sessionAt 31 (some reset1)] t1
  TestM.assertEqual both.size 1 (msg := "the same reset time is the same window, not a new one")
  let w := both[0]!
  TestM.assertEqual w.startEpoch t0 (msg := "the window still starts at the first poll")
  TestM.assertEqual w.lastEpoch t1
  TestM.assertEqual w.peakPercent 31 (msg := "the peak is what the window has consumed")
  TestM.assertEqual w.lastPercent 31
  TestM.assertEqual w.samples 2

@[test]
def recordWindows_aNewResetTimeIsANewWindow : Test := do
  -- The whole point of the graph: the window that just closed keeps the peak it reached, and
  -- the reading that comes back low belongs to the next one rather than erasing it.
  let history := recordWindows #[] #[sessionAt 88 (some reset1)] t0
  let rolled  := recordWindows history #[sessionAt 3 (some reset2)] t1
  TestM.assertEqual rolled.size 2 (msg := "a different reset time starts a window")
  TestM.assertEqual rolled[0]!.peakPercent 88 (msg := "the closed window keeps its peak")
  TestM.assertEqual rolled[1]!.peakPercent 3
  TestM.assertEqual rolled[1]!.startEpoch t1

@[test]
def recordWindows_keepsTheSeriesApart : Test := do
  -- Session and weekly limits arrive in the same poll and are separate counters; a weekly
  -- reading must never land in the session window's record.
  let one := recordWindows #[] #[sessionAt 12 (some reset1), weeklyAt 40] t0
  TestM.assertEqual one.size 2 (msg := "one window per series")
  let two := recordWindows one #[sessionAt 20 (some reset1), weeklyAt 41] t1
  TestM.assertEqual two.size 2 (msg := "both continue rather than duplicating")
  TestM.assertEqual two[0]!.kind LimitKind.session
  TestM.assertEqual two[0]!.lastPercent 20
  TestM.assertEqual two[1]!.kind LimitKind.weeklyAll
  TestM.assertEqual two[1]!.lastPercent 41

@[test]
def recordWindows_scopeIsPartOfTheSeries : Test := do
  -- A weekly limit scoped to one model family is a different counter from the account-wide
  -- one, and the two would otherwise be folded together by kind.
  let opusWeek : Limit := { kind := .weeklyScoped, percent := 100, scopeModel := some "Opus" }
  let out := recordWindows #[] #[weeklyAt 40, opusWeek] t0
  TestM.assertEqual out.size 2 (msg := "scoped and unscoped are separate windows")
  TestM.assertEqual out[1]!.scope (some "Opus")

@[test]
def recordWindows_withoutAResetTimeReadsTheCounter : Test := do
  -- Nothing reported a reset time, so the shape of the counter is all there is to go on:
  -- utilisation that dropped has reset, and that is where the next window starts.
  let history := recordWindows #[] #[sessionAt 61] t0
  let same    := recordWindows history #[sessionAt 74] t1
  TestM.assertEqual same.size 1 (msg := "a reading that climbed is the same window")
  let rolled  := recordWindows same #[sessionAt 2] (t1 + 3600)
  TestM.assertEqual rolled.size 2 (msg := "a reading that dropped is the next window")
  TestM.assertEqual rolled[0]!.peakPercent 74

@[test]
def recordWindows_withoutAResetTimeDoesNotBridgeAGap : Test := do
  -- The daemon was down for two days. A session window is five hours wide, so the poll that
  -- comes back cannot be another reading of the one it left, however plausible its number.
  let history := recordWindows #[] #[sessionAt 20] t0
  let later   := recordWindows history #[sessionAt 22] (t0 + 2 * 86400)
  TestM.assertEqual later.size 2 (msg := "a gap longer than the window starts a new one")

@[test]
def recordWindows_learnsAResetTimeLate : Test := do
  -- The first poll of a window came back without one; the second brought it. The window keeps
  -- it, so everything after is matched on the reset time rather than on the counter's shape.
  let history := recordWindows #[] #[sessionAt 20] t0
  let known   := recordWindows history #[sessionAt 24 (some reset1)] t1
  TestM.assertEqual known.size 1
  TestM.assertEqual known[0]!.resetEpoch (parseIso8601 reset1)

@[test]
def recordWindows_toleratesAReanchoredResetTime : Test := do
  -- A rollover moves the reset by a whole window; a second or two of movement is the same
  -- window re-anchored. Read strictly, the second reading would start a new record — and a
  -- source that drifted on every poll would leave a history of one-sample windows, which is
  -- not a history of anything.
  let history := recordWindows #[] #[sessionAt 40 (some "2026-07-24T04:00:00Z")] t0
  let same    := recordWindows history #[sessionAt 44 (some "2026-07-24T04:00:30Z")] t1
  TestM.assertEqual same.size 1 (msg := "thirty seconds of drift is the same window")
  TestM.assertEqual same[0]!.peakPercent 44
  -- And the tolerance is nowhere near wide enough to swallow a real one.
  let rolled := recordWindows same #[sessionAt 5 (some "2026-07-24T09:00:00Z")] (t1 + 3600)
  TestM.assertEqual rolled.size 2 (msg := "five hours later is the next window")

@[test]
def pruneWindows_doesNotBelieveAClockThatWouldDropEverything : Test := do
  -- A container that polls once before NTP has stepped it reports a `now` months ahead. Taken
  -- at face value that empties the file in one write, and the correction afterwards does not
  -- bring it back.
  let windows := #[
    ({ kind := .session, startEpoch := t0, lastEpoch := t0, peakPercent := 61 } : Window),
    ({ kind := .weeklyAll, startEpoch := t0, lastEpoch := t1, peakPercent := 80 } : Window)]
  let kept := pruneWindows windows (t0 + 10 * historyRetentionSecs)
  TestM.assertEqual kept.size 2 (msg := "an age filter that drops everything is not believed")

@[test]
def pruneWindows_dropsWhatIsTooOld : Test := do
  let old : Window := { kind := .session, startEpoch := t0, lastEpoch := t0, peakPercent := 50 }
  let recent : Window := { old with startEpoch := t0 + 1, lastEpoch := t0 + 1 }
  let kept := pruneWindows #[old, recent] (t0 + historyRetentionSecs + 1)
  TestM.assertEqual kept.size 1 (msg := "the window that fell out of retention is dropped")
  TestM.assertEqual kept[0]!.lastEpoch (t0 + 1)

@[test]
def pruneWindows_capsEachSeriesSeparately : Test := do
  -- A session window every five hours must not be able to evict the weekly history: the two
  -- feed different graphs, and the busier one would otherwise starve the other.
  let sessions := (Array.range (maxWindowsPerSeries + 20)).map fun (i : Nat) =>
    ({ kind := .session, startEpoch := t0 + (↑i : Int), lastEpoch := t0 + (↑i : Int) } : Window)
  let week : Window := { kind := .weeklyAll, startEpoch := t0, lastEpoch := t0, peakPercent := 77 }
  let kept := pruneWindows (#[week] ++ sessions) t0
  TestM.assertEqual (kept.filter (·.kind == .session)).size maxWindowsPerSeries
    (msg := "the busy series is capped")
  TestM.assertEqual (kept.filter (·.kind == .weeklyAll)).size 1
    (msg := "the quiet series survives it")
  -- Newest kept, oldest dropped, and still in order.
  TestM.assertEqual kept[kept.size - 1]!.lastEpoch (t0 + (maxWindowsPerSeries : Int) + 19)
    (msg := "the most recent window is the last one")

@[test]
def window_roundTripsThroughJson : Test := do
  let w : Window := {
    kind := .weeklyScoped, scope := some "Opus", resetEpoch := some 1784867400
    startEpoch := t0, lastEpoch := t1, peakPercent := 100, lastPercent := 100, samples := 9 }
  match Lean.FromJson.fromJson? (α := Window) (Lean.ToJson.toJson w) with
  | .error e => TestM.fail s!"a stored window did not read back: {e}"
  | .ok back =>
    TestM.assertEqual back.kind w.kind
    TestM.assertEqual back.scope w.scope
    TestM.assertEqual back.resetEpoch w.resetEpoch
    TestM.assertEqual back.startEpoch w.startEpoch
    TestM.assertEqual back.lastEpoch w.lastEpoch
    TestM.assertEqual back.peakPercent w.peakPercent
    TestM.assertEqual back.lastPercent w.lastPercent
    TestM.assertEqual back.samples w.samples
