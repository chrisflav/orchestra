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

private def sampleHeaders : Array String := #[
  "HTTP/2 200",
  "anthropic-ratelimit-unified-5h-status: allowed",
  "anthropic-ratelimit-unified-5h-utilization: 0.02",
  "anthropic-ratelimit-unified-5h-reset: 1784867400",
  "anthropic-ratelimit-unified-7d-status: rejected",
  "anthropic-ratelimit-unified-7d-utilization: 1",
  "anthropic-ratelimit-unified-7d-reset: 1785351600",
  "content-type: application/json"
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
  TestM.assert (parseUnifiedHeaders #["HTTP/2 500", "content-type: text/html"]).isEmpty
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
  match statusError 429 "" with
  | some .rateLimited => TestM.assert true
  | _ => TestM.fail "429 must stay the one status that changes future behaviour"
  match statusError 500 "" with
  | some (.other m) =>
    TestM.assert (m.contains "no response body")
      (msg := "an empty body says so rather than trailing off")
  | _ => TestM.fail "expected a plain reported failure"

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

private def cfgWithSources (labels : List String) (dflt : Option String := none)
    (pollUsage : Bool := true) : AppConfig :=
  { appId := 1, privateKeyPath := ""
    agentAuthConfigs := #[{
      name := "claude"
      defaultAuthSource := dflt
      pollUsage
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
