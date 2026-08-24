import OrchestraTest.TestM
import Orchestra

/-!
# Listener dispatch rate limits

The parsing of `rate_limits` and the arithmetic that decides whether a listener may dispatch.
Both are pure, which is the reason they live outside the daemon's poll loop: the loop is a
fiber that talks to GitHub, and none of the decisions below need either.

`now` is a fixed epoch (`2024-01-01T00:00:00Z`, 1704067200) throughout, so that a test asserting
"an hour ago still counts" does not depend on when it runs.
-/

namespace OrchestraTest.ListenerRateLimit

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Listener

/-- 2024-01-01T00:00:00Z. -/
private def now : Int := 1704067200

/-- `secs` seconds before `now`, as the ISO stamp the state file holds. -/
private def ago (secs : Nat) : String := Usage.secsToIso8601 (now - (secs : Int))

@[test]
def parseWindowUnits : Test := do
  TestM.assertEqual (parseWindow? "second")  (some 1)      (msg := "second")
  TestM.assertEqual (parseWindow? "minute")  (some 60)     (msg := "minute")
  TestM.assertEqual (parseWindow? "hour")    (some 3600)   (msg := "hour")
  TestM.assertEqual (parseWindow? "day")     (some 86400)  (msg := "day")
  TestM.assertEqual (parseWindow? "week")    (some 604800) (msg := "week")
  TestM.assertEqual (parseWindow? "HOUR")    (some 3600)   (msg := "case-insensitive")
  TestM.assertEqual (parseWindow? "  hour ") (some 3600)   (msg := "surrounding space")

@[test]
def parseWindowCounts : Test := do
  TestM.assertEqual (parseWindow? "6h")         (some 21600) (msg := "6h")
  TestM.assertEqual (parseWindow? "90 minutes") (some 5400)  (msg := "90 minutes")
  TestM.assertEqual (parseWindow? "1 day")      (some 86400) (msg := "1 day")
  TestM.assertEqual (parseWindow? "30s")        (some 30)    (msg := "30s")

@[test]
def parseWindowRejectsNonsense : Test := do
  -- A window that cannot be read has to stay unread. The dangerous failure for this feature is
  -- a typo that parses as no limit at all, so every one of these must be `none` and become a
  -- parse error at the layer above.
  TestM.assertEqual (parseWindow? "")         (none : Option Nat) (msg := "empty")
  TestM.assertEqual (parseWindow? "hourly")   (none : Option Nat) (msg := "hourly")
  TestM.assertEqual (parseWindow? "5")        (none : Option Nat) (msg := "count with no unit")
  TestM.assertEqual (parseWindow? "0h")       (none : Option Nat) (msg := "zero-length window")
  TestM.assertEqual (parseWindow? "per hour") (none : Option Nat) (msg := "'per hour'")

@[test]
def rateLimitFromJsonSpellings : Test := do
  let parse (raw : String) : Option RateLimit :=
    (Json.parse raw >>= FromJson.fromJson? (α := RateLimit)).toOption
  TestM.assertEqual (parse r#"{"max": 5, "per": "hour"}"#)
    (some { max := 5, windowSeconds := 3600 }) (msg := "per: hour")
  TestM.assertEqual (parse r#"{"max": 20, "per": "day"}"#)
    (some { max := 20, windowSeconds := 86400 }) (msg := "per: day")
  TestM.assertEqual (parse r#"{"max": 2, "per": "6h"}"#)
    (some { max := 2, windowSeconds := 21600 }) (msg := "per: 6h")
  TestM.assertEqual (parse r#"{"max": 3, "per_seconds": 90}"#)
    (some { max := 3, windowSeconds := 90 }) (msg := "per_seconds")

@[test]
def rateLimitFromJsonRejects : Test := do
  let bad (raw : String) : Bool :=
    (Json.parse raw >>= FromJson.fromJson? (α := RateLimit)).toOption.isNone
  TestM.assert (bad r#"{"max": 5}"#)                    "a limit with no window is an error"
  TestM.assert (bad r#"{"max": 5, "per": "hourly"}"#)   "an unreadable 'per' is an error"
  TestM.assert (bad r#"{"max": 5, "per_seconds": 0}"#)  "a zero-length window is an error"
  TestM.assert (bad r#"{"per": "hour"}"#)               "a limit with no 'max' is an error"

@[test]
def rateLimitRoundTrip : Test := do
  let l : RateLimit := { max := 7, windowSeconds := 3600 }
  match FromJson.fromJson? (ToJson.toJson l) (α := RateLimit) with
  | .error e => TestM.fail s!"RateLimit round-trip: {e}"
  | .ok got  => TestM.assertEqual got l (msg := "round-trip")

@[test]
def rateLimitDescribe : Test := do
  TestM.assertEqual (RateLimit.describe { max := 5,  windowSeconds := 3600 })   "5 per hour"
    (msg := "hour")
  TestM.assertEqual (RateLimit.describe { max := 20, windowSeconds := 86400 })  "20 per day"
    (msg := "day")
  TestM.assertEqual (RateLimit.describe { max := 1,  windowSeconds := 60 })     "1 per minute"
    (msg := "minute")
  TestM.assertEqual (RateLimit.describe { max := 2,  windowSeconds := 21600 })  "2 per 6 hours"
    (msg := "6 hours")
  TestM.assertEqual (RateLimit.describe { max := 3,  windowSeconds := 90 })     "3 per 90s"
    (msg := "90s")

@[test]
def countWithinWindow : Test := do
  let stamps := #[ago 30, ago 3000, ago 4000, ago 100000]
  TestM.assertEqual (countWithin stamps now 60)    1 (msg := "last minute")
  TestM.assertEqual (countWithin stamps now 3600)  2 (msg := "last hour")
  TestM.assertEqual (countWithin stamps now 86400) 3 (msg := "last day")

@[test]
def countWithinIgnoresGarbage : Test := do
  -- A hand-edited state file must not be able to hold a listener shut: what cannot be read as a
  -- timestamp is not a dispatch that happened.
  TestM.assertEqual (countWithin #["not a timestamp", "", ago 30] now 3600) 1
    (msg := "unreadable stamps do not count")

@[test]
def countWithinCountsTheFuture : Test := do
  -- A clock that jumped backwards is a reason to dispatch less, not more, so a stamp ahead of
  -- `now` stays inside the window.
  TestM.assertEqual (countWithin #[Usage.secsToIso8601 (now + 500)] now 3600) 1
    (msg := "a future stamp still counts")

@[test]
def rateLimitHitPicksTheFullOne : Test := do
  let limits : List RateLimit :=
    [{ max := 5, windowSeconds := 3600 }, { max := 8, windowSeconds := 86400 }]
  -- Four in the last hour and seven in the last day: room under both.
  let underBoth := #[ago 10, ago 20, ago 30, ago 40, ago 40000, ago 50000, ago 60000]
  TestM.assertEqual (rateLimitHit? limits underBoth now) (none : Option RateLimit)
    (msg := "room under both")
  -- A fifth this hour fills the hourly one, and that is the one reported.
  TestM.assertEqual (rateLimitHit? limits (underBoth.push (ago 50)) now)
    (some { max := 5, windowSeconds := 3600 }) (msg := "hourly full")
  -- Eight spread across the day, none in the last hour: the daily one is what stops it.
  let dayFull := #[ago 20000, ago 25000, ago 30000, ago 35000,
                   ago 40000, ago 45000, ago 50000, ago 55000]
  TestM.assertEqual (rateLimitHit? limits dayFull now)
    (some { max := 8, windowSeconds := 86400 }) (msg := "daily full")

@[test]
def rateLimitHitEmptyLimits : Test := do
  -- No limits configured is the default and has to stay free: a listener that never opted in is
  -- never held.
  let many := (Array.range 100).map (fun i => ago i)
  TestM.assertEqual (rateLimitHit? [] many now) (none : Option RateLimit)
    (msg := "no limits, no ceiling")

@[test]
def rateLimitWindowMoves : Test := do
  let limits : List RateLimit := [{ max := 2, windowSeconds := 3600 }]
  -- Two dispatches, one of them 59 minutes ago: still full.
  let stamps := #[ago 3540, ago 60]
  TestM.assertEqual (rateLimitHit? limits stamps now) (some { max := 2, windowSeconds := 3600 })
    (msg := "full at 59 minutes")
  -- A minute later the older one has aged out and there is room again.
  TestM.assertEqual (rateLimitHit? limits stamps (now + 120)) (none : Option RateLimit)
    (msg := "room once it ages out")

@[test]
def pruneDropsWhatNoLimitCounts : Test := do
  let limits : List RateLimit :=
    [{ max := 5, windowSeconds := 3600 }, { max := 20, windowSeconds := 86400 }]
  let stamps := #[ago 100, ago 4000, ago 90000, "garbage"]
  -- Kept against the *longest* window, so the hourly limit does not throw away what the daily
  -- one still needs.
  TestM.assertEqual (pruneDispatches limits stamps now) #[ago 100, ago 4000]
    (msg := "pruned to the longest window")
  TestM.assertEqual (pruneDispatches [] stamps now) (#[] : Array String)
    (msg := "no limits keeps nothing")

@[test]
def statusReportsUsageAndWhen : Test := do
  let limits : List RateLimit := [{ max := 2, windowSeconds := 3600 }]
  match rateLimitStatuses limits #[ago 3000] now with
  | [s] =>
    TestM.assertEqual s.used 1 (msg := "one used")
    TestM.assertEqual s.nextAllowedAt (none : Option Int) (msg := "room now, so no wait")
  | _ => TestM.fail "expected one status"
  -- Full: room appears an hour after the oldest of the two.
  match rateLimitStatuses limits #[ago 3000, ago 60] now with
  | [s] =>
    TestM.assertEqual s.used 2 (msg := "two used")
    TestM.assertEqual s.nextAllowedAt (some (now - 3000 + 3600)) (msg := "waits on the oldest")
  | _ => TestM.fail "expected one status"

@[test]
def statusOverCapWaitsForRoom : Test := do
  -- Three on record under a cap of two, as happens when a limit is lowered on a live listener.
  -- Room is when the *second* oldest ages out, not the first: dropping one still leaves it full.
  let limits : List RateLimit := [{ max := 2, windowSeconds := 3600 }]
  match rateLimitStatuses limits #[ago 3000, ago 2000, ago 1000] now with
  | [s] =>
    TestM.assertEqual s.used 3 (msg := "three used")
    TestM.assertEqual s.nextAllowedAt (some (now - 2000 + 3600)) (msg := "second oldest")
  | _ => TestM.fail "expected one status"

@[test]
def heldEventsComeBack : Test := do
  -- The ordinary path: what the tick handled is appended, what it held is not — so the next
  -- tick offers the held one again instead of skipping it as already seen.
  TestM.assertEqual
    (nextProcessedIds #["org/repo:1"] #["org/repo:2", "org/repo:3"] #["org/repo:3"] none)
    #["org/repo:1", "org/repo:2"]
    (msg := "a held event is not marked processed")
  TestM.assertEqual
    (nextProcessedIds #["org/repo:1"] #["org/repo:2"] #[] none)
    #["org/repo:1", "org/repo:2"]
    (msg := "nothing held is the shape this always had")

@[test]
def heldEventsComeBackThroughAReplacement : Test := do
  -- `github-labels` rewrites the whole set each tick and puts *every* id it saw into the
  -- replacement, the ones the tick never reached included. Without subtracting the held ones
  -- here, a rate limit would silently drop them rather than pace them.
  TestM.assertEqual
    (nextProcessedIds #["org/repo:1"] #["org/repo:2", "org/repo:3"] #["org/repo:3"]
      (some #["org/repo:1", "org/repo:2", "org/repo:3"]))
    #["org/repo:1", "org/repo:2"]
    (msg := "held ids come back out of a wholesale replacement")
  -- And a replacement still gets to drop what it means to drop: the id it left out stays out.
  TestM.assertEqual
    (nextProcessedIds #["org/repo:1", "org/repo:9"] #["org/repo:2"] #[]
      (some #["org/repo:1", "org/repo:2"]))
    #["org/repo:1", "org/repo:2"]
    (msg := "a replacement still prunes what it dropped")

@[test]
def onlyGithubCommentsPagesByTime : Test := do
  -- What a held event costs depends on this. `github-comments` asks for the comments updated
  -- `since` the last check, so advancing that cursor past a held one loses it; every other
  -- source re-derives its candidates from the world and offers it again for free.
  let repos := [({ upstream := { owner := "org", name := "repo" },
                   fork := { owner := "my-org", name := "fork" } } : RepoEntry)]
  TestM.assert (pagesByTime (.githubComments repos [] "@bot" []))
    "github-comments pages by time"
  TestM.assert (!pagesByTime (.githubIssues repos [] "" []))
    "github-issues re-derives"
  TestM.assert (!pagesByTime (.githubPrReviews repos [] "" []))
    "github-pr-reviews re-derives"
  TestM.assert (!pagesByTime (.githubLabels repos [] "all" []))
    "github-labels re-derives"
  TestM.assert (!pagesByTime (.githubLabelCount repos [] 5 "issues"))
    "github-label-count re-derives"
  TestM.assert (!pagesByTime (.shell "echo" []))
    "shell re-derives"

@[test]
def alreadyProcessedIdsAreNeverUnprocessed : Test := do
  -- `held` is subtracted from what the tick adds, never from what was already there. No source
  -- re-emits an id it has processed, so this is not reachable today; it is asserted so that a
  -- source that starts doing so cannot quietly un-process history.
  TestM.assertEqual
    (nextProcessedIds #["org/repo:1"] #["org/repo:2"] #["org/repo:1"] none)
    #["org/repo:1", "org/repo:2"]
    (msg := "a held id that was already processed stays processed")

@[test]
def theWindowBoundaryIsExclusive : Test := do
  -- Exactly one window old is out; a second inside it is in. Pinned because `nextAllowedAt` is
  -- computed as "oldest counted stamp + window" and has to name the first instant at which a
  -- dispatch is allowed, not the last at which it is refused.
  let limits : List RateLimit := [{ max := 1, windowSeconds := 3600 }]
  TestM.assertEqual (countWithin #[ago 3600] now 3600) 0 (msg := "exactly a window old is out")
  TestM.assertEqual (countWithin #[ago 3599] now 3600) 1 (msg := "a second inside is in")
  match rateLimitStatuses limits #[ago 3599] now with
  | [s] =>
    match s.nextAllowedAt with
    | none      => TestM.fail "a full window should say when it frees up"
    | some free =>
      TestM.assertEqual (rateLimitHit? limits #[ago 3599] (free - 1)).isSome true
        (msg := "still refused the second before")
      TestM.assertEqual (rateLimitHit? limits #[ago 3599] free).isSome false
        (msg := "allowed exactly at nextAllowedAt")
  | _ => TestM.fail "expected one status"

@[test]
def aZeroCapIsSurvivableEvenThoughItIsRefusedOnWrite : Test := do
  -- `validateListenerConfig` rejects `max: 0`, but that guards the API and the CLI, not a file
  -- placed by hand. The status arithmetic indexes with `used - max`, so this pins that it stays
  -- in bounds rather than reaching for an element that is not there.
  let limits : List RateLimit := [{ max := 0, windowSeconds := 3600 }]
  TestM.assertEqual (rateLimitHit? limits #[] now) (some { max := 0, windowSeconds := 3600 })
    (msg := "a zero cap holds even an empty record")
  match rateLimitStatuses limits #[ago 30] now with
  | [s] =>
    TestM.assertEqual s.used 1 (msg := "used is still counted")
    TestM.assertEqual s.nextAllowedAt (none : Option Int)
      (msg := "and it never frees up, rather than indexing out of bounds")
  | _ => TestM.fail "expected one status"

@[test]
def describeRoundTripsEveryUnitItAccepts : Test := do
  -- Every spelling `parseWindow?` takes has to come back out of `describe` as itself, or a
  -- config round trip renames the window its author chose.
  for unit in ["second", "minute", "hour", "day", "week"] do
    match parseWindow? unit with
    | none   => TestM.fail s!"parseWindow? rejected '{unit}'"
    | some w => TestM.assertEqual (RateLimit.describe { max := 1, windowSeconds := w })
                  s!"1 per {unit}" (msg := s!"describe round-trips '{unit}'")

@[test]
def listenerConfigParsesRateLimits : Test := do
  let raw := r#"
    {"source": {"type": "shell", "command": "echo", "args": []},
     "action": {"prompt_template": "hi"},
     "interval_seconds": 300,
     "rate_limits": [{"max": 5, "per": "hour"}, {"max": 20, "per": "day"}]}
  "#
  match Json.parse raw >>= FromJson.fromJson? (α := ListenerConfig) with
  | .error e => TestM.fail s!"listener config with rate_limits: {e}"
  | .ok cfg  =>
    TestM.assertEqual cfg.intervalSeconds 300 (msg := "interval_seconds")
    TestM.assertEqual cfg.rateLimits
      [{ max := 5, windowSeconds := 3600 }, { max := 20, windowSeconds := 86400 }]
      (msg := "rate_limits")

@[test]
def listenerConfigWithoutRateLimits : Test := do
  let raw := r#"
    {"source": {"type": "shell", "command": "echo", "args": []},
     "action": {"prompt_template": "hi"}}
  "#
  match Json.parse raw >>= FromJson.fromJson? (α := ListenerConfig) with
  | .error e => TestM.fail s!"listener config without rate_limits: {e}"
  | .ok cfg  =>
    TestM.assertEqual cfg.rateLimits ([] : List RateLimit) (msg := "defaults to none")
    -- And a config that never had the field does not gain one on the way back out.
    TestM.assert ((ToJson.toJson cfg).getObjVal? "rate_limits" |>.toOption |>.isNone)
      "an empty rate_limits is not serialised"

@[test]
def listenerConfigRejectsUnreadableRateLimit : Test := do
  -- The whole point: `"per": "hourly"` must not parse as a listener with no ceiling.
  let raw := r#"
    {"source": {"type": "shell", "command": "echo", "args": []},
     "action": {"prompt_template": "hi"},
     "rate_limits": [{"max": 5, "per": "hourly"}]}
  "#
  match Json.parse raw >>= FromJson.fromJson? (α := ListenerConfig) with
  | .error _ => TestM.assert true "an unreadable window fails the whole config"
  | .ok _    => TestM.fail "a listener with an unreadable rate limit must not parse"

@[test]
def listenerConfigRoundTripsRateLimits : Test := do
  let cfg : ListenerConfig := {
    source     := .shell "echo" []
    action     := { promptTemplate := "hi" }
    rateLimits := [{ max := 5, windowSeconds := 3600 }, { max := 20, windowSeconds := 86400 }]
  }
  match FromJson.fromJson? (ToJson.toJson cfg) (α := ListenerConfig) with
  | .error e => TestM.fail s!"ListenerConfig round-trip: {e}"
  | .ok got  => TestM.assertEqual got.rateLimits cfg.rateLimits (msg := "rate_limits round-trip")

@[test]
def listenerStateCarriesDispatches : Test := do
  let st : ListenerState :=
    { lastChecked := "2024-01-01T00:00:00Z", processedIds := #["org/repo:1"],
      dispatches := #[ago 30, ago 60] }
  match FromJson.fromJson? (ToJson.toJson st) (α := ListenerState) with
  | .error e => TestM.fail s!"ListenerState round-trip: {e}"
  | .ok got  =>
    TestM.assertEqual got.dispatches st.dispatches (msg := "dispatches round-trip")
    TestM.assertEqual got.processedIds st.processedIds (msg := "processed_ids round-trip")

@[test]
def listenerStateWithoutDispatches : Test := do
  -- Every state file written before this field existed.
  let raw := r#"{"last_checked": "2024-01-01T00:00:00Z", "processed_ids": ["org/repo:1"]}"#
  match Json.parse raw >>= FromJson.fromJson? (α := ListenerState) with
  | .error e => TestM.fail s!"old listener state: {e}"
  | .ok st   =>
    TestM.assertEqual st.dispatches (#[] : Array String) (msg := "defaults to none")
    TestM.assertEqual st.enabled true (msg := "still enabled")

end OrchestraTest.ListenerRateLimit
