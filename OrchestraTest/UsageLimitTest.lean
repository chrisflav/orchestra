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
      block := some { untilEpoch := some (now + 3600), reason := "agent reported a usage limit" } }
  TestM.assert (!(availabilityOf st (some "claude-opus-4-8") now).isAvailable)
    (msg := "the recorded hit blocks")
  let expired : SourceState := { st with block := some { st.block.get! with untilEpoch := some (now - 1) } }
  TestM.assert ((availabilityOf expired (some "claude-opus-4-8") now).isAvailable)
    (msg := "and stops blocking once its window passes")

@[test]
def availability_observedBlockCanItselfBeScoped : Test := do
  let st : SourceState :=
    { backend := "claude", label := "main"
      block := some { untilEpoch := some (now + 3600), model := some "claude-opus-4-8"
                      reason := "agent reported a usage limit" } }
  TestM.assert (!(availabilityOf st (some "claude-opus-4-8") now).isAvailable)
    (msg := "blocks the model that hit it")
  TestM.assert ((availabilityOf st (some "claude-sonnet-5") now).isAvailable)
    (msg := "but not a different one")

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

private def cfgWithSources (labels : List String) (dflt : Option String := none) : AppConfig :=
  { appId := 1, privateKeyPath := ""
    agentAuthConfigs := #[{
      name := "claude"
      defaultAuthSource := dflt
      authSources := labels.toArray.map fun l => { label := l, kind := .oauthToken "t" } }] }

@[test]
def candidatesFor_explicitListWins : Test := do
  let cfg := cfgWithSources ["a", "b", "c"] (dflt := some "a")
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
  TestM.assertEqual (candidatesFor (cfgWithSources ["a", "b"] (dflt := some "b")) "claude" [] none)
    ["b"] (msg := "default_auth_source")
  TestM.assertEqual (candidatesFor (cfgWithSources ["only"]) "claude" [] none) ["only"]
    (msg := "a sole source needs no naming")

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
