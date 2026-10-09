/-
Usage-limit monitoring for agent authentication sources.

A Claude subscription is not one limit but several running at once: a rolling session window, a
weekly total, and weekly limits scoped to a single model family. These are read from the
`anthropic-ratelimit-unified-*` response headers of a minimal inference call (`fetchUtilization`),
not from `GET /api/oauth/usage`: a long-lived `setup-token` carries inference scope but not the
profile scope that endpoint needs, so it is answered with 403, whereas the headers ride on any
inference call the token *can* make. The `limits[]` body shape below is still understood — see
`parseUtilization`, kept for reading a Claude Code on-disk cache — but the headers are the live
source. Either way the parsed limits are the input to every decision in this module:

```
"limits": [
  {"kind":"session",       "percent":3,  "resets_at":"…", "scope":null,                       "is_active":false},
  {"kind":"weekly_all",    "percent":75, "resets_at":"…", "scope":null,                       "is_active":false},
  {"kind":"weekly_scoped", "percent":100,"resets_at":"…", "scope":{"model":{"display_name":"Fable"}}, "is_active":true}
]
```

The scoped entry is why availability is a question about *(source, model)* rather than about a
source alone: an exhausted weekly-Opus limit leaves Sonnet work on the same account perfectly
runnable, and treating the account as dead would idle it for a week. The headers report only the
session and weekly-all windows — a scoped weekly limit is invisible to the poll and is instead
caught by `markLimited` the moment a run hits it.

Two things write to the state this module keeps, and they cover each other:

* the **poll** above, which sees a limit coming before anything runs into it, and knows the exact
  reset time; and
* an **observed hit** — a run that came back rate-limited — recorded by `markLimited`. This is
  the authoritative signal (it happened) but carries no reset time of its own, so it borrows one
  from the last poll and otherwise falls back to a conservative default. Its scope is the model
  the task asked for, which is a guess: the provider names the family in its message, and reading
  that instead is a separate change.

A source therefore accumulates a *set* of blocks, one per scope, not a single one. They are
independent facts with independent expiries, and a completed run retires only the blocks covering
the model it ran: proof that Sonnet works on an account says nothing about whether Fable does.

State lives in the database — a row of `usage_source` per `(backend, label)`, and a row of
`usage_window` per window of recorded history — because the processes that need to agree about it
are genuinely separate: `orchestra run` in a terminal and the queue daemon are different OS
processes, and a limit one of them discovers has to stop the other.
-/
import Lean.Data.Json
import Orchestra.Config
import Orchestra.Dirs
import Orchestra.Store
import Orchestra.Utils.Files
import Orchestra.Utils.Http
import Orchestra.Utils.Time
import Std.Time

open Lean (Json FromJson ToJson)

namespace Orchestra.Usage

/-! ## Time

The RFC 3339 parser and formatter live in `Orchestra.Time`, below this module, because the
stores need them to order their records too. Re-exported here: `Usage.parseIso8601` is how the
rest of the daemon has always spelled it, and a limit's `resetsAt` is a usage concern wherever
the parsing happens.
-/

export Orchestra.Time (parseIso8601 secsToIso8601)

/-- Current time in epoch seconds.

    Wall-clock, not `IO.monoNanosNow`: every reset time this is compared against is wall-clock.
    Deliberately not a `date` subprocess — a daemon whose sources are all limited re-evaluates
    every pending entry once a second, and a process spawn per evaluation adds up fast. -/
def nowEpoch : IO Int := do
  return (← Std.Time.Timestamp.now).toSecondsSinceUnixEpoch.val

/-- Current time in epoch nanoseconds, used only to order dispatches against each other.

    Seconds are the right unit for reset times — the server reports them that way — but the
    wrong one for round-robin. Workers claim under a mutex they release immediately, so a burst
    of claims can share a timestamp at second *or* millisecond resolution; every source then
    ties, and `distribute` collapses onto whichever sorts first, which is the one thing it
    exists to avoid. At nanosecond resolution two claims cannot collide. -/
def nowDispatchTick : IO Int := do
  return (← Std.Time.Timestamp.now).toNanosecondsSinceUnixEpoch.val

/-- A human-readable "in 2h 5m", for log lines and `orchestra usage`.

    Days are a bucket of their own because the weekly limits are the ones people read this
    for, and their windows are up to seven days wide — "in 3d 4h" is an answer, "in 76h 12m"
    is arithmetic homework. -/
def relativeToNow (target now : Int) : String :=
  let d := target - now
  if d ≤ 0 then "now"
  else
    let secs := d.toNat
    let days := secs / 86400
    let h := (secs % 86400) / 3600
    let m := (secs % 3600) / 60
    if days > 0 then s!"in {days}d {h}h"
    else if h > 0 then s!"in {h}h {m}m" else if m > 0 then s!"in {m}m" else s!"in {secs}s"

/-! ## The limit model -/

/-- Which limit a `limits[]` entry describes.

    Unrecognised kinds are kept verbatim rather than dropped: the endpoint already ships kinds
    this code has never heard of, and a limit we cannot name is still a limit we must respect. -/
inductive LimitKind where
  | session
  | weeklyAll
  | weeklyScoped
  | other (raw : String)
deriving Repr, BEq, Inhabited

def LimitKind.toString : LimitKind → String
  | .session      => "session"
  | .weeklyAll    => "weekly_all"
  | .weeklyScoped => "weekly_scoped"
  | .other raw    => raw

def LimitKind.ofString : String → LimitKind
  | "session"       => .session
  | "weekly_all"    => .weeklyAll
  | "weekly_scoped" => .weeklyScoped
  | raw             => .other raw

instance : ToJson LimitKind where toJson k := Json.str k.toString
instance : FromJson LimitKind where
  fromJson? | .str s => .ok (LimitKind.ofString s)
            | j      => .error s!"expected limit kind string, got {j}"

/-- One reported limit. `scopeModel` is the model family the limit applies to (`"Fable"`,
    `"Opus"`, …); `none` means it applies to everything on the account. -/
structure Limit where
  kind       : LimitKind
  group      : String := ""
  percent    : Nat := 0
  severity   : String := "normal"
  resetsAt   : Option String := none
  scopeModel : Option String := none
  isActive   : Bool := false
deriving Repr, Inhabited

instance : ToJson Limit where
  toJson l :=
    let fields : List (String × Json) := [
      ("kind",      ToJson.toJson l.kind),
      ("group",     Json.str l.group),
      ("percent",   Json.num l.percent),
      ("severity",  Json.str l.severity),
      ("is_active", Json.bool l.isActive)
    ]
    let fields := if let some r := l.resetsAt   then fields ++ [("resets_at",   Json.str r)] else fields
    let fields := if let some m := l.scopeModel then fields ++ [("scope_model", Json.str m)] else fields
    Json.mkObj fields

instance : FromJson Limit where
  fromJson? j := do
    let _ ← j.getObj?
    let kind := (j.getObjValAs? LimitKind "kind").toOption.getD (.other "unknown")
    let group := (j.getObjValAs? String "group").toOption.getD ""
    let percent := (j.getObjValAs? Nat "percent").toOption.getD 0
    let severity := (j.getObjValAs? String "severity").toOption.getD "normal"
    let resetsAt := (j.getObjValAs? String "resets_at").toOption
    let scopeModel := (j.getObjValAs? String "scope_model").toOption
    let isActive := (j.getObjValAs? Bool "is_active").toOption.getD false
    return { kind, group, percent, severity, resetsAt, scopeModel, isActive }

/-! ## Parsing the endpoint response -/

private def jNat (j : Json) (key : String) : Option Nat :=
  match j.getObjVal? key with
  | .ok (.num n) => some n.toFloat.toUInt64.toNat
  | _            => none

private def jStr? (j : Json) (key : String) : Option String :=
  (j.getObjValAs? String key).toOption

/-- Pull `scope.model.display_name` out of a `limits[]` entry. -/
private def scopeModelOf (j : Json) : Option String := do
  let scope ← (j.getObjVal? "scope").toOption
  let model ← (scope.getObjVal? "model").toOption
  jStr? model "display_name"

private def limitOfJson (j : Json) : Option Limit := do
  let kindStr ← jStr? j "kind"
  return {
    kind := LimitKind.ofString kindStr
    group := (jStr? j "group").getD ""
    percent := (jNat j "percent").getD 0
    severity := (jStr? j "severity").getD "normal"
    resetsAt := jStr? j "resets_at"
    scopeModel := scopeModelOf j
    isActive := (j.getObjValAs? Bool "is_active").toOption.getD false
  }

/-- The pre-`limits[]` shape: top-level `five_hour` / `seven_day` / `seven_day_opus` objects,
    each `{utilization, resets_at}` or `null`. Read only when `limits` is absent, so an older
    account (or an older server) still yields something usable rather than nothing. -/
private def legacyLimits (util : Json) : Array Limit := Id.run do
  let known : List (String × LimitKind × Option String) := [
    ("five_hour",        .session,      none),
    ("seven_day",        .weeklyAll,    none),
    ("seven_day_opus",   .weeklyScoped, some "Opus"),
    ("seven_day_sonnet", .weeklyScoped, some "Sonnet")
  ]
  let mut out : Array Limit := #[]
  for (key, kind, scope) in known do
    if let .ok entry := util.getObjVal? key then
      if let some pct := jNat entry "utilization" then
        out := out.push {
          kind, group := if kind == .session then "session" else "weekly"
          percent := pct
          severity := if pct ≥ 100 then "critical" else if pct ≥ 75 then "warning" else "normal"
          resetsAt := jStr? entry "resets_at"
          scopeModel := scope
          isActive := pct ≥ 100
        }
  return out

/-- Parse the body of `GET /api/oauth/usage`.

    Every field is optional by design. This is an undocumented endpoint that already returns
    kinds like `tangelo` and `nimbus_quill` alongside the ones we understand; a strict parser
    would fail the whole document the next time one is added, and a monitor that fails closed on
    an unknown field is worse than no monitor. -/
def parseUtilization (body : String) : Except String (Array Limit) := do
  let j ← Json.parse body
  -- The endpoint returns the utilization object directly; Claude Code's on-disk cache nests it
  -- under "utilization". Accept either so a hand-pasted cache file also parses.
  let util := (j.getObjVal? "utilization").toOption.getD j
  match util.getObjVal? "limits" with
  | .ok (.arr entries) =>
    let limits := entries.filterMap limitOfJson
    if limits.isEmpty then return legacyLimits util else return limits
  | _ => return legacyLimits util

/-! ## Per-source state -/

/-- A recorded block on a source: either observed (a run came back rate-limited) or derived from
    a poll. `model` narrows it to one model family, mirroring `Limit.scopeModel`. -/
structure Block where
  untilEpoch : Option Int := none
  model      : Option String := none
  reason     : String := ""
deriving Repr, Inhabited

instance : ToJson Block where
  toJson b :=
    let fields : List (String × Json) := [("reason", Json.str b.reason)]
    let fields := if let some u := b.untilEpoch then fields ++ [("until_epoch", Json.num u)] else fields
    let fields := if let some m := b.model      then fields ++ [("model",       Json.str m)] else fields
    Json.mkObj fields

instance : FromJson Block where
  fromJson? j := do
    -- `getObjValAs?` reads a *missing* key as `Json.null`, so an instance that accepts anything
    -- turns an absent `"block"` into a present-but-empty one — and an empty block with no expiry
    -- reads as "blocked forever". Rejecting non-objects is what keeps absence meaning absence.
    let _ ← j.getObj?
    let untilEpoch := (j.getObjValAs? Int "until_epoch").toOption
    let model := (j.getObjValAs? String "model").toOption
    let reason := (j.getObjValAs? String "reason").toOption.getD ""
    return { untilEpoch, model, reason }

/-- Agent time accumulated against one window of one source, for learning how fast agents fill it.

    `startPct` is where the counter stood when accumulation began. Accumulation begins whenever
    this process stops being able to account for the agents that ran — the first poll it sees of a
    window, or the first after a gap in polling — so what was consumed before then, by agents it
    did not count, is subtracted rather than charged to the agents it did. -/
structure WindowAcc where
  /-- Reset epoch identifying the window; `none` before the first poll that reported one. -/
  reset     : Option Int := none
  startPct  : Nat := 0
  lastPct   : Nat := 0
  /-- Running-agent seconds inside the window since `startPct` was read. -/
  agentSecs : Float := 0
deriving Repr, Inhabited, ToJson

/- The decoders below are written out, field by field with defaults, rather than derived: a derived
   one rejects a document missing any field, so the first field added after deployment would make
   every stored estimate unreadable and quietly reset what every source has learned. -/

instance : FromJson WindowAcc where
  fromJson? j := do
    let _ ← j.getObj?
    let d : WindowAcc := {}
    return {
      reset     := (j.getObjValAs? Int "reset").toOption
      startPct  := (j.getObjValAs? Nat "startPct").toOption.getD d.startPct
      lastPct   := (j.getObjValAs? Nat "lastPct").toOption.getD d.lastPct
      agentSecs := (j.getObjValAs? Float "agentSecs").toOption.getD d.agentSecs }

/-- What has been learned about how fast agents consume one source's limits.

    The rates are in percent of the limit per *running-agent hour*: what one agent, running for an
    hour on this source, adds to the counter. Per hour rather than per task because a task's cost
    is only known once it ends, and the moment that matters — choosing an account for the next
    task — is the moment the running ones are still running. Per source because accounts are not
    alike: a larger plan fills its windows more slowly, and an account something outside orchestra
    also uses fills them faster, and both are things the rate should say. -/
structure Estimate where
  sessionRate : Option Float := none
  weeklyRate  : Option Float := none
  session     : WindowAcc := {}
  weekly      : WindowAcc := {}
  /-- Agents running on the source at the last observation, and when that was. -/
  lastRunning : Nat := 0
  lastEpoch   : Option Int := none
deriving Repr, Inhabited, ToJson

instance : FromJson Estimate where
  fromJson? j := do
    let _ ← j.getObj?
    return {
      sessionRate := (j.getObjValAs? Float "sessionRate").toOption
      weeklyRate  := (j.getObjValAs? Float "weeklyRate").toOption
      session     := (j.getObjValAs? WindowAcc "session").toOption.getD {}
      weekly      := (j.getObjValAs? WindowAcc "weekly").toOption.getD {}
      lastRunning := (j.getObjValAs? Nat "lastRunning").toOption.getD 0
      lastEpoch   := (j.getObjValAs? Int "lastEpoch").toOption }

/-- Everything known about one `(backend, label)` authentication source. -/
structure SourceState where
  backend       : String
  label         : String
  fetchedEpoch  : Option Int := none
  limits        : Array Limit := #[]
  /-- Every block currently recorded against this source, at most one per scope.

      A set rather than a single slot, because an account runs several limits at once and they
      are independent facts about it: "Fable is spent until Friday" and "the session window is
      spent until 14:00" are both true, both matter, and neither is evidence about the other.
      Held as one slot, the second recorded silently erased the first — and when the survivor
      expired, the erased one came back as an account that looked runnable and was not. -/
  blocks        : Array Block := #[]
  /-- When this source was last handed to a task, in epoch **nanoseconds**. An ordering key
      only — never compared against a reset time, which is why the finer unit costs nothing. -/
  lastUsedTick : Option Int := none
  /-- Why the last poll failed, if it did. Reported by `orchestra usage`; never blocks dispatch
      on its own, because an unreachable endpoint is not evidence of an exhausted account. -/
  lastError     : Option String := none
  /-- Do not poll again before this time. Set when the usage endpoint itself returns 429.

      Distinct from `block`, and deliberately so: the *endpoint* refusing more requests says
      nothing about whether the *subscription* has quota left. This suppresses polling; it never
      suppresses dispatch. -/
  pollAfter     : Option Int := none
  /-- The learned consumption rates, and the accumulators that learn them. See `Estimate`. -/
  estimate      : Estimate := {}
deriving Repr, Inhabited

instance : ToJson SourceState where
  toJson s :=
    let fields : List (String × Json) := [
      ("backend", Json.str s.backend),
      ("label",   Json.str s.label),
      ("limits",  ToJson.toJson s.limits)
    ]
    let fields := if let some e := s.fetchedEpoch  then fields ++ [("fetched_epoch",   Json.num e)] else fields
    let fields := if !s.blocks.isEmpty             then fields ++ [("blocks",          ToJson.toJson s.blocks)] else fields
    let fields := if let some e := s.lastUsedTick then fields ++ [("last_used_tick", Json.num e)] else fields
    let fields := if let some e := s.lastError     then fields ++ [("last_error",      Json.str e)] else fields
    let fields := if let some e := s.pollAfter     then fields ++ [("poll_after",       Json.num e)] else fields
    Json.mkObj fields

instance : FromJson SourceState where
  fromJson? j := do
    let backend ← j.getObjValAs? String "backend"
    let label   ← j.getObjValAs? String "label"
    let limits := (j.getObjValAs? (Array Limit) "limits").toOption.getD #[]
    let fetchedEpoch := (j.getObjValAs? Int "fetched_epoch").toOption
    -- `"blocks"` is the current shape. A single `"block"` object is what state files written
    -- before this existed carry, and it is lifted rather than dropped: these files are the only
    -- record that a limit was ever observed, and an upgrade that forgot one would send the next
    -- task straight into it. Reading both is the whole migration — the files are a regenerable
    -- cache, so nothing has to be rewritten ahead of time.
    -- Element by element, and the legacy key only when `"blocks"` is genuinely absent. Decoding
    -- the array as a whole would drop *every* block on one unreadable element and then fall
    -- through to a `"block"` that is not there, leaving the source looking runnable — the very
    -- forgetting this migration exists to prevent, arrived at by a different route.
    let blocks := match j.getObjVal? "blocks" with
      | .ok (.arr items) => items.filterMap fun it =>
          (FromJson.fromJson? it : Except String Block).toOption
      | _ => match (j.getObjValAs? Block "block").toOption with
        | some b => #[b]
        | none   => #[]
    let lastUsedTick := (j.getObjValAs? Int "last_used_tick").toOption
    let lastError := (j.getObjValAs? String "last_error").toOption
    let pollAfter := (j.getObjValAs? Int "poll_after").toOption
    return { backend, label, limits, fetchedEpoch, blocks, lastUsedTick, lastError, pollAfter }

/-- Where the JSON state and history files lived before the database: `<data>/usage`, holding a
    `<backend>/<label>.json` and a `<backend>/<label>.history.json` per source. Read once, by
    `Orchestra.Store.Import`, and then left alone — the import copies, it does not delete.

    The label in those file names was flattened to what a filename may hold, which is lossy; the
    state document beside it carries the real one, and that is the one the import keys on. -/
def legacyUsageDir : IO System.FilePath :=
  return (← Dirs.dataBase) / "usage"

/-- The state as a row of `usage_source`, keyed by the pair that names the source.

    `limits` and `blocks` are the compressed JSON their own instances write: they are arrays of
    documents with no scalar form, and storing them as text is what keeps a limit kind this
    build has never heard of readable by the build that has. -/
def SourceState.toRow (s : SourceState) : Store.UsageSourceRow :=
  { backend        := s.backend
    label          := s.label
    fetched_epoch  := s.fetchedEpoch
    limits         := Store.jsonColumn s.limits
    blocks         := Store.jsonColumn s.blocks
    last_used_tick := s.lastUsedTick
    last_error     := s.lastError
    poll_after     := s.pollAfter
    estimate       := some (Store.jsonColumn s.estimate) }

/-- The state a row holds, or why this build cannot read it.

    An estimate this build cannot read is dropped rather than failing the row: it is relearned
    from the polls that follow, and refusing the limits and blocks over it would forget limits. -/
def SourceState.ofRow? (row : Store.UsageSourceRow) : Except String SourceState := do
  let limits ← Store.jsonOfColumn? "limits" row.limits
  let blocks ← Store.jsonOfColumn? "blocks" row.blocks
  let estimate := match row.estimate with
    | some e => (Store.jsonOfColumn? "estimate" e : Except String Estimate).toOption.getD {}
    | none   => {}
  return { backend      := row.backend
           estimate     := estimate
           label        := row.label
           fetchedEpoch := row.fetched_epoch
           limits       := limits
           blocks       := blocks
           lastUsedTick := row.last_used_tick
           lastError    := row.last_error
           pollAfter    := row.poll_after }

open Db.Query.DSL in
/-- What is known about one source, or a blank state for one nothing has polled yet.

    A source with no row and a source whose row this build cannot read come back the same way,
    which is what the file store did with a missing and an unparseable file: this is a cache of
    what the provider last said, the next poll refills it, and a daemon that refused to dispatch
    because a cache entry was unreadable would have its priorities backwards. -/
def loadState (backend label : String) : IO SourceState := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let s ← from Store.UsageSourceRow
    guard s.backend = backend
    guard s.label = label
    select s
  match rows[0]? with
  | none     => return { backend, label }
  | some row => match SourceState.ofRow? row with
    | .ok s    => return s
    | .error e =>
      IO.eprintln s!"[usage] {backend}/{label}: state row unreadable ({e}); starting again"
      return { backend, label }

def saveState (s : SourceState) : IO Unit :=
  Store.run <| HasModel.save s.toRow

private def modifyState (backend label : String) (f : SourceState → SourceState) : IO Unit := do
  saveState (f (← loadState backend label))

/-! ## History

A poll answers "how much is left", and that is the only question it answers. "How much did
yesterday cost", "is this week heavier than the last three", "did that concert eat a whole
session window" are questions about the past, and the past has to be kept somewhere to be
asked about.

What is kept is one record per *window* rather than one per poll. The session and weekly limits
are counters that fill and then reset, so the window is the unit that carries meaning: the peak
a closed session window reached is what that session consumed, and the peak of a week is what
that week did. Rolling polls up as they arrive is also what keeps the number of rows small enough
to read on every dashboard tick — a poll every five minutes is eight thousand samples a month, and
the same month is a few hundred windows.

Only polls are recorded. An observed hit (`markLimited`) establishes that *a* limit was reached
but neither which counter reached it nor where that counter stood, so folding it in would
invent a reading rather than record one; the poll that follows it reports the real number. -/

/-- One session or weekly window, rolled up from every poll that landed inside it. -/
structure Window where
  kind        : LimitKind
  /-- The model family a scoped window applies to; `none` for one that applies to everything. -/
  scope       : Option String := none
  /-- The reset time every poll inside this window reported, in epoch seconds. Absent when
      nothing reported one, which is what `continuesWindow` then has to work around. -/
  resetEpoch  : Option Int := none
  /-- The first poll that landed in this window, and the last. Not the window's own bounds: a
      window is only ever seen through the polls that caught it. -/
  startEpoch  : Int := 0
  lastEpoch   : Int := 0
  /-- The highest reading, which is what the window has been up to — and the number that
      survives the reset, which the next poll reports as a fresh low.

      Not the same as `lastPercent` whenever a reading inside the window came back down. Why one
      does is upstream's business and not visible from here — reconciled usage, a limit raised
      under the account, a counter that ages usage out rather than emptying at its reset — so
      the only safe reading of the pair is "the highest we saw" and "the last we saw". A reader
      that shows one where the other is meant reports a number the live limit disagrees with. -/
  peakPercent : Nat := 0
  /-- The most recent reading: on the window still open, where the source stood at the poll that
      last touched this window.

      Normally that is the same poll whose limits `refresh` stored, so this is the number the
      source's `limits` carry for the same window. It is not a guarantee: a poll that stops
      reporting a window leaves the record here untouched while `limits` is replaced wholesale,
      and a history write that fails leaves this one poll behind. -/
  lastPercent : Nat := 0
  /-- How many polls this window was built from. One is a glimpse of a window rather than a
      measurement of it, and a reader is entitled to say so. -/
  samples     : Nat := 0
deriving Repr, Inhabited, BEq

instance : ToJson Window where
  toJson w :=
    let fields : List (String × Json) := [
      ("kind",         ToJson.toJson w.kind),
      ("start_epoch",  Json.num w.startEpoch),
      ("last_epoch",   Json.num w.lastEpoch),
      ("peak_percent", Json.num w.peakPercent),
      ("last_percent", Json.num w.lastPercent),
      ("samples",      Json.num w.samples)
    ]
    let fields := if let some r := w.resetEpoch then fields ++ [("reset_epoch", Json.num r)]
                  else fields
    let fields := if let some sc := w.scope then fields ++ [("scope", Json.str sc)] else fields
    Json.mkObj fields

instance : FromJson Window where
  fromJson? j := do
    let _ ← j.getObj?
    let kind := (j.getObjValAs? LimitKind "kind").toOption.getD (.other "unknown")
    let scope := (j.getObjValAs? String "scope").toOption
    let resetEpoch := (j.getObjValAs? Int "reset_epoch").toOption
    let startEpoch := (j.getObjValAs? Int "start_epoch").toOption.getD 0
    let lastEpoch := (j.getObjValAs? Int "last_epoch").toOption.getD startEpoch
    let peakPercent := (j.getObjValAs? Nat "peak_percent").toOption.getD 0
    let lastPercent := (j.getObjValAs? Nat "last_percent").toOption.getD peakPercent
    let samples := (j.getObjValAs? Nat "samples").toOption.getD 0
    return { kind, scope, resetEpoch, startEpoch, lastEpoch, peakPercent, lastPercent, samples }

/-- The window as a row of `usage_window`, under the source it was recorded for.

    The source is not on the window — a window is a reading, and which account it is a reading of
    is the store's business — so it is supplied here rather than carried. `id` is left at zero:
    it is an `AutoKey`, which the insert leaves out for the database to assign, and it is that
    assignment which makes `ORDER BY id` the insertion order `loadHistory` reads back. -/
def Window.toRow (backend label : String) (w : Window) : Store.UsageWindowRow :=
  { id           := 0
    backend      := backend
    label        := label
    kind         := Store.enumColumn w.kind
    scope        := w.scope
    reset_epoch  := w.resetEpoch
    start_epoch  := w.startEpoch
    last_epoch   := w.lastEpoch
    peak_percent := Store.natColumn w.peakPercent
    last_percent := Store.natColumn w.lastPercent
    samples      := Store.natColumn w.samples }

/-- The window a row holds, or why this build cannot read it. -/
def Window.ofRow? (row : Store.UsageWindowRow) : Except String Window := do
  let kind ← Store.enumOfColumn? "kind" row.kind
  return { kind        := kind
           scope       := row.scope
           resetEpoch  := row.reset_epoch
           startEpoch  := row.start_epoch
           lastEpoch   := row.last_epoch
           peakPercent := row.peak_percent.toNat
           lastPercent := row.last_percent.toNat
           samples     := row.samples.toNat }

/-- Nominal length of a window. Used only to decide whether two polls that reported no reset
    time can have been inside the same one. -/
def windowLengthSecs : LimitKind → Int
  | .session => 5 * 3600
  | _        => 7 * 86400

/-- How far two reported reset times may differ and still describe the same window.

    A rollover moves the reset time by a whole window — five hours, or seven days — so a minute
    of tolerance cannot swallow one. What it does buy is a soft failure: a source whose reset
    time is re-anchored by a second or two would otherwise start a fresh window on every poll,
    and the history would quietly become one sample per record, which is not history at all. -/
def resetDriftSecs : Nat := 60

/-- Whether a limit polled at `now` is another reading of `w`, or the first reading of the
    window after it.

    The reset time settles it whenever both sides have one: every poll inside a window reports
    the same one, and a different one is by definition a different window. Without it the shape
    of the counter has to serve — utilisation that dropped has reset — with the nominal window
    length as a backstop, so a poll resuming after a day of downtime is not welded onto the
    window it left. -/
private def continuesWindow (w : Window) (l : Limit) (resetEpoch : Option Int) (now : Int)
    : Bool :=
  if w.kind != l.kind || w.scope != l.scopeModel then false
  else match w.resetEpoch, resetEpoch with
    | some a, some b => decide ((a - b).natAbs ≤ resetDriftSecs)
    | _,      _      =>
      decide (l.percent ≥ w.lastPercent) && decide (now - w.lastEpoch ≤ windowLengthSecs l.kind)

private def openWindow (l : Limit) (resetEpoch : Option Int) (now : Int) : Window :=
  { kind := l.kind, scope := l.scopeModel, resetEpoch
    startEpoch := now, lastEpoch := now
    peakPercent := l.percent, lastPercent := l.percent, samples := 1 }

/-- Fold one poll's limits into the recorded windows: into the window each limit continues, and
    as a new entry where it starts one.

    Pure, so the cases that decide whether a graph is right — a window rolling over, a poll
    arriving after a gap, a source that reports no reset time at all — are reachable from a test
    without a clock or a network. -/
def recordWindows (windows : Array Window) (limits : Array Limit) (now : Int)
    : Array Window := Id.run do
  let mut out := windows
  for l in limits do
    let resetEpoch := l.resetsAt.bind parseIso8601
    -- The open window of a series is the *last* one recorded for it: polls arrive in order, so
    -- everything before it is closed.
    let mut latest : Option Nat := none
    for i in [0:out.size] do
      let w := out[i]!
      if w.kind == l.kind && w.scope == l.scopeModel then latest := some i
    match latest with
    | some i =>
      let w := out[i]!
      if continuesWindow w l resetEpoch now then
        out := out.set! i { w with
          lastEpoch := now
          lastPercent := l.percent
          peakPercent := max w.peakPercent l.percent
          -- A window whose first poll carried no reset time keeps the one a later poll brings.
          resetEpoch := resetEpoch.orElse fun _ => w.resetEpoch
          samples := w.samples + 1 }
      else
        out := out.push (openWindow l resetEpoch now)
    | none => out := out.push (openWindow l resetEpoch now)
  return out

/-- How many windows of one series — a kind, and a model scope where it has one — are kept.
    Two hundred and forty session windows is a couple of months of continuous use. -/
def maxWindowsPerSeries : Nat := 240

/-- How far back history is kept at all, whatever the count. -/
def historyRetentionSecs : Int := 180 * 86400

/-- Drop what is too old, then what is too much — newest first, per series.

    Capping per series rather than over the whole file is what stops a session window every five
    hours from evicting the weekly history the second graph is drawn from.

    Answered as the ascending indices of the windows kept, so that `recordPoll` can tell which
    stored rows went. `pruneWindows` is the same answer as the windows themselves. -/
def pruneKept (windows : Array Window) (now : Int) : Array Nat := Id.run do
  let indices := (List.range windows.size).toArray
  let aged := indices.filter fun i => decide (now - windows[i]!.lastEpoch ≤ historyRetentionSecs)
  -- An age filter that drops *everything* is evidence about the clock, not about the data. A
  -- container that polls once before NTP has stepped it would otherwise delete six months of
  -- history in a single atomic write, and the correction afterwards would not bring it back.
  -- The count cap below still bounds the file, so keeping it costs nothing.
  let fresh := if aged.isEmpty then indices else aged
  let mut kept : Array Nat := #[]
  for i in fresh.reverse do
    let w := windows[i]!
    let seen := (kept.filter fun k => windows[k]!.kind == w.kind && windows[k]!.scope == w.scope).size
    if seen < maxWindowsPerSeries then kept := kept.push i
  return kept.reverse

/-- Drop what is too old, then what is too much. See `pruneKept`. -/
def pruneWindows (windows : Array Window) (now : Int) : Array Window :=
  (pruneKept windows now).map (windows[·]!)

/-- The row writes that take a source's stored history from `old` to what a poll made of it.

    `folded` is `recordWindows old …`, and that function only rewrites a window in place or appends
    one. So `folded[i]` for `i < old.size` is still the window stored as `old[i]`, and anything past
    that is new. `kept` is `pruneKept folded …`. From those three this reads off which stored rows
    retention dropped, which of the survivors a poll changed, and which windows are new. A poll
    typically changes one row per series and adds none.

    Indices into `old` rather than row ids, so that this stays pure. `recordPoll` maps them to
    ids. -/
structure HistoryWrite where
  /-- Stored windows retention dropped, as indices into `old`. -/
  drop   : Array Nat := #[]
  /-- Stored windows the poll changed, as an index into `old` and what it now holds. -/
  change : Array (Nat × Window) := #[]
  /-- Windows the poll opened, in the order they have to be inserted in. -/
  add    : Array Window := #[]
deriving Repr, Inhabited

def planHistoryWrite (old folded : Array Window) (kept : Array Nat) : HistoryWrite := Id.run do
  let mut plan : HistoryWrite := {}
  for i in [0:old.size] do
    if !kept.contains i then plan := { plan with drop := plan.drop.push i }
  for i in kept do
    if i < old.size then
      if folded[i]! != old[i]! then plan := { plan with change := plan.change.push (i, folded[i]!) }
    else plan := { plan with add := plan.add.push folded[i]! }
  return plan

open Db.Query.DSL in
/-- The recorded windows for one source, oldest first.

    Oldest first is `ORDER BY id`, the order they were inserted in, which `saveHistory` writes
    them in — the windows are a sequence rather than a set, and every function that folds over
    them relies on the open window of a series being the last one in it.

    A row this build cannot read is reported and left out, as a record that did not parse always
    was: history is a record of the past, nothing about the present depends on it, and a monitor
    that refused to run because one row of a graph was unreadable would have its priorities
    backwards. -/
def loadHistory (backend label : String) : IO (Array Window) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let w ← from Store.UsageWindowRow
    guard w.backend = backend
    guard w.label = label
    select w
    order_by w.id
  Store.keepConvertible "usage window" (fun r => s!"{r.backend}/{r.label}#{r.id}")
    Window.ofRow? rows

/-- Replace a source's history with `windows`, in order, in one transaction.

    For seeding a history wholesale, which is what the tests use it for. A poll does not come
    through here. `recordPoll` writes only the rows the poll touched, because rewriting a few hundred rows
    on every poll was what made each poll take minutes in a slow process. -/
def saveHistory (backend label : String) (windows : Array Window) : IO Unit :=
  Store.transaction do
    let _ ← HasModel.delete (α := Store.UsageWindowRow)
      (.and (.eq (.var Store.UsageWindowRowIndex.backend .text) (.text backend))
            (.eq (.var Store.UsageWindowRowIndex.label .text) (.text label)))
    for w in windows do
      HasModel.insert (w.toRow backend label)

open Db.Query.DSL in
/-- Fold one poll's limits into the stored history. Called on every successful poll, and by
    nothing else.

    Only the rows the poll touched are written (see `planHistoryWrite`). The history used to be
    replaced wholesale on every poll: a delete, then an insert for each of up to 480 windows per
    source. Inside a daemon slowed by a leak, that took minutes a source, which is part of how the
    queue stalled on 2026-09-27. Rows keep their ids, so `ORDER BY id` is still the order they
    were recorded in. A new window is inserted, and takes an id after every existing one, which
    makes it the last of its series. That is where `recordWindows` looks for the open window.

    A row this build cannot read is deleted along with the rest of what retention drops. The
    wholesale rewrite dropped it too, because `loadHistory` leaves it out.

    Load, fold, save, unserialised — like the state row beside it. Two polls for the same source
    that interleave and both fold into the open window update the same row, and the last one
    wins. That costs a `samples` tick and, at worst, a peak only the losing poll saw. Two that
    both see a rollover both insert the new window, which leaves one stale single-sample
    duplicate in the series. That is milder than before. With the wholesale rewrite, the second
    poll's delete could not see the first poll's fresh inserts, so it re-added the whole history
    on top of them. A lock on a table three processes reach would cost more than either. One
    transaction, so a reader never sees a poll half written. -/
def recordPoll (backend label : String) (limits : Array Limit) (now : Int) : IO Unit := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let w ← from Store.UsageWindowRow
    guard w.backend = backend
    guard w.label = label
    select w
    order_by w.id
  let mut ids : Array Int := #[]
  let mut old : Array Window := #[]
  let mut unreadable : Array Int := #[]
  for row in rows do
    match Window.ofRow? row with
    | .ok w    => ids := ids.push row.id; old := old.push w
    | .error e =>
      IO.eprintln s!"[usage] {backend}/{label}: dropping unreadable history row #{row.id}: {e}"
      unreadable := unreadable.push row.id
  let folded := recordWindows old limits now
  let plan := planHistoryWrite old folded (pruneKept folded now)
  let dropIds := unreadable ++ plan.drop.map (ids[·]!)
  if dropIds.isEmpty && plan.change.isEmpty && plan.add.isEmpty then return
  Store.transaction do
    unless dropIds.isEmpty do
      let _ ← HasModel.delete (α := Store.UsageWindowRow)
        (.inList (.var Store.UsageWindowRowIndex.id .int) dropIds.toList)
    for (i, w) in plan.change do
      let _ ← HasModel.update (α := Store.UsageWindowRow)
        { value
            | .reset_epoch  => some (match w.resetEpoch with
                                     | some r => .int r
                                     | none   => .null .int)
            | .last_epoch   => some (.int w.lastEpoch)
            | .peak_percent => some (.int (Store.natColumn w.peakPercent))
            | .last_percent => some (.int (Store.natColumn w.lastPercent))
            | .samples      => some (.int (Store.natColumn w.samples))
            | _             => none
          condition := .eq (.var Store.UsageWindowRowIndex.id .int) (.int ids[i]!) }
    for w in plan.add do
      HasModel.insert (w.toRow backend label)

/-! ## Availability

The question is never "is this account usable" but "is this account usable *for this task*",
because a `weekly_scoped` limit only closes one model family. -/

/-- Does a task running `model` fall under a limit scoped to `scope`?

    `scope` is a display name (`"Fable"`, `"Opus"`); `model` is whatever the task asked for
    (`"claude-opus-4-8"`, `"sonnet"`, or nothing at all). A substring match on the lowercased
    names covers both the alias and the dated-id spellings.

    A task with **no** model does not match. That direction is deliberate: the alternative —
    treating an unknown model as matching every scope — lets one exhausted model family idle the
    whole account for a week, and it is not needed for correctness, because a task that really
    does run into the scoped limit gets recorded by `markLimited` the moment it does. -/
def modelMatchesScope (scope : String) (model : Option String) : Bool :=
  match model with
  | none   => false
  | some m =>
    let m := m.toLower
    let s := scope.toLower
    !s.isEmpty && (m.splitOn s).length > 1

/-- Whether a block covers a task running `model`. An unscoped block covers everything. -/
def blockApplies (b : Block) (model : Option String) : Bool :=
  match b.model with
  | none   => true
  | some m => modelMatchesScope m model

/-- Whether a block has not yet lifted. One with no expiry never lifts on its own; only proof —
    a completed run, through `markOk` — retires it. -/
def blockIsLive (b : Block) (now : Int) : Bool :=
  match b.untilEpoch with
  | some u => u > now
  | none   => true

/-- Fold a second reading of one window into the first.

    A later reading is not a correction. Workers run concurrently, so two tasks dispatched to one
    source before any block existed both report the same limit seconds apart, and one of them may
    carry a reset time the other does not. Overwriting would let a later reading that learned less
    shorten a block the earlier one knew ran longer, so the expiry takes whichever says it lasts
    longer: what is known about a window only ever grows.

    The scope keeps the *broader* spelling. "Opus" and "claude-opus-4-8" name one window, but they
    do not cover the same tasks: a block scoped to the dated id is invisible to a task asking for
    `opus`, where one scoped to the display name catches both. Taking whichever arrived last would
    make coverage depend on arrival order, and half the orders lose. The shorter name is the one
    contained in the other, which is exactly the broader scope. -/
def mergeBlock (fresh prior : Block) : Block :=
  { model := match fresh.model, prior.model with
      | some a, some b => some (if a.length ≤ b.length then a else b)
      | _,      _      => fresh.model
    -- A missing expiry is "nobody said", not "never lifts". Treating it as never — which
    -- `blockIsLive` does, correctly, when that is all a block has — would make it absorbing here:
    -- one expiry-less block from a hand-edited state file would render every later merge on that
    -- scope permanent, retirable only by a run that `availabilityOf` will no longer allow.
    untilEpoch := match fresh.untilEpoch, prior.untilEpoch with
      | some a, some b => some (max a b)
      | some a, none   => some a
      | none,   some b => some b
      | none,   none   => none
    reason := fresh.reason }

/-- Whether two block scopes name the same window.

    `none` is the account-wide scope and matches only itself. Two model scopes match when either
    name contains the other, so the display name a provider writes ("Fable") and the id a task
    asked for ("claude-fable-5") are recognised as one window rather than accumulating as two
    blocks that expire independently. -/
def sameScope : Option String → Option String → Bool
  | none,   none   => true
  | some a, some b => modelMatchesScope a (some b) || modelMatchesScope b (some a)
  | _,      _      => false

/-- A limit that is currently binding: at or over the line, and not yet reset. -/
private def limitIsBinding (l : Limit) (now : Int) : Bool :=
  if !(l.isActive || l.percent ≥ 100) then false
  else match l.resetsAt.bind parseIso8601 with
    | some reset => reset > now
    | none       => true  -- no readable reset time: assume still in force

inductive Availability where
  | available
  /-- Blocked until `untilEpoch` (absent when nothing reported one). -/
  | blocked (untilEpoch : Option Int) (reason : String)
deriving Repr, Inhabited

def Availability.isAvailable : Availability → Bool
  | .available => true
  | _          => false

/-- Whether `st` can run a task using `model` right now. -/
def availabilityOf (st : SourceState) (model : Option String) (now : Int) : Availability := Id.run do
  -- Of the blocks covering this model, the binding one is whichever lifts *last*. More than one
  -- can apply now that they are a set — an account-wide window and a longer scoped limit, say —
  -- and answering with the first in the array would name a reset the source is not free at, so
  -- the caller waits for it, dispatches, and is turned away again. A block with no expiry
  -- outlasts every block that has one.
  let mut binding : Option Block := none
  for b in st.blocks do
    if blockIsLive b now && blockApplies b model then
      binding := match binding with
        | none      => some b
        | some best =>
          match best.untilEpoch, b.untilEpoch with
          | none,   _      => some best
          | _,      none   => some b
          | some x, some y => if y > x then some b else some best
  if let some b := binding then
    return .blocked b.untilEpoch (if b.reason.isEmpty then "usage limit" else b.reason)
  for l in st.limits do
    if limitIsBinding l now then
      match l.scopeModel with
      | none   => return .blocked (l.resetsAt.bind parseIso8601) s!"{l.kind.toString} limit at {l.percent}%"
      | some s =>
        if modelMatchesScope s model then
          return .blocked (l.resetsAt.bind parseIso8601)
            s!"{l.kind.toString} limit for {s} at {l.percent}%"
  return .available

/-! ## Consumption estimate

A poll says where an account's counters stand; it does not say where they are going. Choosing by
where they stand alone is what made `distribute` herd: every claim between two polls read the same
numbers, so every one of them went to the same least-used account, and a dozen agents started on
it at once filled its session window before the next poll had even noticed them. What selection
needs is where an account's counters *will* stand once the agents already on it, and the one about
to be added, have run — which is the polled reading plus a rate times the agent time to come.

The rate is learned. Each poll the daemon makes adds the agent time spent on a source since the
previous one to the window that poll belongs to; when the window rolls over, what the counter rose
by over that agent time is one sample of the rate, folded into a moving average. Until a source
has closed a window with enough agent time in it to say anything, the prior below stands in. -/

/-- Prior session-window consumption, in percent per running-agent hour. The median over 82 closed
    session windows on one deployment (Opus, Lean formalisation work, autumn 2026); the spread
    between windows is wide, which is why it is a prior and not a constant. -/
def defaultSessionRate : Float := 6.6

/-- Prior weekly consumption, in percent per running-agent hour, from the same data. -/
def defaultWeeklyRate : Float := 0.65

/-- How full a session window may be projected to get before a source stops being preferred. The
    gap to 100 is the margin for a rate that is an average over tasks whose consumption varies by
    an order of magnitude. -/
def sessionHeadroomPct : Float := 80

/-- How full the weekly window may be projected to get, likewise. Narrower than the session margin
    because a week moves slowly: the projection over one task's run is close to the truth. -/
def weeklyHeadroomPct : Float := 98

/-- How long a task just dispatched is assumed to keep running. A task does not stop consuming at
    the reset, so a source whose window closes in five minutes is not five minutes of exposure;
    the window it starts afterwards takes the rest. An hour is a little above the mean run. -/
def expectedTaskSecs : Int := 3600

/-- Weight of one closed window in the moving average of a rate. -/
def rateLearningWeight : Float := 0.3

/-- How far from the prior one window's sample may pull. Narrow, because usage from outside
    orchestra lands in the same counter as the agents' and is charged to them. -/
def rateClampFactor : Float := 3

/-- Least agent time a window must have seen to teach anything about a rate. Below it, rounding of
    an integer percentage and usage from outside orchestra dominate what the agents did. -/
def minLearningAgentSecs : LimitKind → Float
  | .session => 1800
  | _        => 6 * 3600

/-- Longest gap between two observations across which the agent time in between is still taken as
    known. Three of the daemon's poll intervals: anything longer is a daemon that was down, and
    agents it was not counting may have run in it. -/
def observeGapSecs : Int := 900

private def floatOfInt (i : Int) : Float :=
  if i ≥ 0 then i.toNat.toFloat else -((-i).toNat.toFloat)

/-- The account-wide limit of `kind` in a poll, if it reported one. -/
def unscopedLimit (limits : Array Limit) (kind : LimitKind) : Option Limit :=
  limits.find? fun l => l.kind == kind && l.scopeModel.isNone

/-- Fold one poll's reading of a window into its accumulator, and the rate it teaches if this poll
    is the first of a new window.

    `agentSecs` is the running-agent time since the previous poll, `restart` whether that previous
    poll is too far back (or absent) for the agent time in between to be known. -/
def WindowAcc.observe (acc : WindowAcc) (kind : LimitKind) (rate : Option Float) (prior : Float)
    (limit : Option Limit) (agentSecs : Float) (restart : Bool) : WindowAcc × Option Float :=
  let reset := limit.bind (·.resetsAt) |>.bind parseIso8601
  let pct := limit.map (·.percent) |>.getD 0
  let same : Bool := match acc.reset, reset with
    | some a, some b => decide ((a - b).natAbs ≤ resetDriftSecs)
    -- No reset time on either side: the counter's shape has to serve, as in `continuesWindow` —
    -- a reading that fell is the first of a new window.
    | none,   none   => decide (pct ≥ acc.lastPct)
    | _,      _      => false
  if same then
    if restart then
      -- Agents may have run unseen since the last reading; start counting again from here, so
      -- what they consumed is not charged to the agent time that was seen.
      ({ acc with startPct := pct, lastPct := pct, agentSecs := 0 }, rate)
    else
      ({ acc with lastPct := pct, agentSecs := acc.agentSecs + agentSecs }, rate)
  else
    -- The window `acc` describes has closed (or there was none). Its last reading over the agent
    -- time it saw is one sample of the rate, if it saw enough to mean anything.
    let rate :=
      if acc.agentSecs ≥ minLearningAgentSecs kind then
        let rose := floatOfInt ((acc.lastPct : Int) - acc.startPct)
        let sample := rose / (acc.agentSecs / 3600)
        -- A window that rose by far more than its agents can explain was mostly used by something
        -- else, and one that did not rise was mostly idle agents; neither is the typical agent.
        -- Kept within a factor of `rateClampFactor` of the prior, because the rate decides how
        -- many agents a source gets and one outlying window must not idle it.
        let sample := max (prior / rateClampFactor) (min (prior * rateClampFactor) sample)
        let old := rate.getD prior
        some ((1 - rateLearningWeight) * old + rateLearningWeight * sample)
      else
        -- Too little agent time to learn from. A source whose learned rate keeps it short of
        -- agents would otherwise never get the agent time to unlearn it, so the rate eases back
        -- toward the prior instead. Only for a window that was actually observed: the first
        -- reading a source ever gets closes nothing.
        if acc.reset.isSome then
          rate.map fun r => (1 - rateLearningWeight) * r + rateLearningWeight * prior
        else rate
    ({ reset, startPct := pct, lastPct := pct, agentSecs := 0 }, rate)

/-- Fold a successful poll, made while `running` agents were on the source, into its estimate. -/
def Estimate.observe (e : Estimate) (limits : Array Limit) (running : Nat) (now : Int) : Estimate :=
  let dt := match e.lastEpoch with | some l => now - l | none => 0
  let restart := e.lastEpoch.isNone || dt < 0 || dt > observeGapSecs
  let agentSecs := if restart then 0 else (e.lastRunning + running).toFloat / 2 * floatOfInt dt
  let (session, sessionRate) := e.session.observe .session e.sessionRate defaultSessionRate
    (unscopedLimit limits .session) agentSecs restart
  let (weekly, weeklyRate) := e.weekly.observe .weeklyAll e.weeklyRate defaultWeeklyRate
    (unscopedLimit limits .weeklyAll) agentSecs restart
  { sessionRate, weeklyRate, session, weekly, lastRunning := running, lastEpoch := some now }

/-! ## Selection -/

/-- How loaded a source is, as far as choosing between sources under `distribute` goes. -/
structure Load where
  /-- Percent of the session window used at the last poll. -/
  sessionPct     : Nat := 0
  /-- Seconds from now until the session window resets; `none` when no poll has said. -/
  sessionResetIn : Option Int := none
  /-- Percent of the tightest weekly limit that applies to the task's model. -/
  weeklyPct      : Nat := 0
  weeklyResetIn  : Option Int := none
  /-- Agents running on the source right now. -/
  running        : Nat := 0
  /-- Seconds since the poll the percentages come from. -/
  staleSecs      : Int := 0
  sessionRate    : Float := defaultSessionRate
  weeklyRate     : Float := defaultWeeklyRate
deriving Repr, Inhabited

/-- Where a window's counter is expected to stand once the running agents, and one more, have run
    for `horizon` seconds, given a reading `stale` seconds old and the reset `resetIn` from now.

    A window that has reset since the reading starts again from nothing. The horizon is capped at
    the window's remaining life, but never below one task: work started now runs past a reset, and
    counting it against the current window is the conservative mistake. -/
private def projected (pct : Nat) (resetIn : Option Int) (windowLen : Int) (running : Nat)
    (stale : Int) (rate : Float) (horizon : Int) : Float :=
  let resetIn := resetIn.getD windowLen
  let (base, left) :=
    if resetIn ≤ 0 then ((0 : Float), windowLen)
    else
      -- The agents already running have been consuming since the reading was taken, and the
      -- reading does not show it yet.
      let unseen := max 0 (min stale (windowLen - resetIn))
      (pct.toFloat + rate * running.toFloat * floatOfInt unseen / 3600, min resetIn windowLen)
  let exposure := max (min horizon left) expectedTaskSecs
  base + rate * (running + 1).toFloat * floatOfInt exposure / 3600

/-- Projected session usage if one more agent starts now and the source keeps its current number
    of agents until the window resets. -/
def Load.sessionProjected (l : Load) : Float :=
  projected l.sessionPct l.sessionResetIn (windowLengthSecs .session) l.running l.staleSecs
    l.sessionRate (windowLengthSecs .session)

/-- Projected weekly usage once one more agent's run is over. -/
def Load.weeklyProjected (l : Load) : Float :=
  projected l.weeklyPct l.weeklyResetIn (windowLengthSecs .weeklyAll) l.running l.staleSecs
    l.weeklyRate expectedTaskSecs

/-- Whether the source can take another agent without either window projected past its headroom. -/
def Load.hasHeadroom (l : Load) : Bool :=
  l.sessionProjected ≤ sessionHeadroomPct && l.weeklyProjected ≤ weeklyHeadroomPct

/-- How much weekly capacity the source would lose at its reset, per hour until then: what is left
    of the week is worth spending first where it expires soonest. -/
def Load.weeklyUrgency (l : Load) : Float :=
  let len := windowLengthSecs .weeklyAll
  let resetIn := l.weeklyResetIn.getD len
  let (pct, left) := if resetIn ≤ 0 then (0, len) else (l.weeklyPct, min resetIn len)
  let remaining := max 0 (100 - pct.toFloat)
  remaining / max 1 (floatOfInt left / 3600)

/-- How far past its headroom the source is projected to go; what is minimised when every source
    is past it. -/
def Load.overshoot (l : Load) : Float :=
  max (l.sessionProjected - sessionHeadroomPct) (l.weeklyProjected - weeklyHeadroomPct)

/-- The highest binding-relevant utilisation across the limits that could apply to `model`. Shown
    on the dashboard; selection uses `Load`, which also knows about resets and running agents. -/
def pressureOf (st : SourceState) (model : Option String) : Nat := Id.run do
  let mut worst := 0
  for l in st.limits do
    let applies := match l.scopeModel with
      | none   => true
      | some s => modelMatchesScope s model
    if applies && l.percent > worst then worst := l.percent
  return worst

/-- The `Load` of a source for a task running `model`, with `running` agents on it already. -/
def loadOf (st : SourceState) (model : Option String) (running : Nat) (now : Int) : Load := Id.run do
  let resetIn (l : Limit) : Option Int := (l.resetsAt.bind parseIso8601).map (· - now)
  let session := unscopedLimit st.limits .session
  -- The tightest weekly limit that applies: the account-wide one, or one scoped to this model.
  let mut weekly : Option Limit := none
  for l in st.limits do
    let applies := match l.kind, l.scopeModel with
      | .weeklyAll,    none   => true
      | .weeklyScoped, some s => modelMatchesScope s model
      | _,             _      => false
    if applies then
      weekly := match weekly with
        | some w => if l.percent > w.percent then some l else some w
        | none   => some l
  return {
    sessionPct     := session.map (·.percent) |>.getD 0
    sessionResetIn := session.bind resetIn
    weeklyPct      := weekly.map (·.percent) |>.getD 0
    weeklyResetIn  := weekly.bind resetIn
    running
    staleSecs      := st.fetchedEpoch.map (now - ·) |>.getD 0
    sessionRate    := st.estimate.sessionRate.getD defaultSessionRate
    weeklyRate     := st.estimate.weeklyRate.getD defaultWeeklyRate }

/-- A source considered for selection, with the verdict that decided it. -/
structure Candidate where
  label        : String
  availability : Availability
  load         : Load := {}
  lastUsed     : Int
  /-- Position in the configured list; the tiebreak of last resort, so selection is
      deterministic when two sources are genuinely indistinguishable. -/
  index        : Nat
deriving Repr, Inhabited

/-- Whether `a` should be chosen over `b` under `distribute`.

    Sources with headroom come before sources without. Among those with it, the weekly limit
    decides — whichever has the most of its week left to lose per hour until the reset — because
    the week is the scarcer of the two and capacity left over at a reset is gone. The session
    window only gets a say through headroom, which already counts the agents running there: that
    is what stops one account taking every claim between two polls. Among sources past their
    headroom, the one projected to go least far past it. -/
def Candidate.preferredOver (a b : Candidate) : Bool :=
  let tie : Unit → Bool := fun _ =>
    if a.lastUsed != b.lastUsed then a.lastUsed < b.lastUsed else a.index < b.index
  let close (x y : Float) : Bool := (x - y).abs < 1e-9
  match a.load.hasHeadroom, b.load.hasHeadroom with
  | true,  false => true
  | false, true  => false
  | true,  true  =>
    let (ua, ub) := (a.load.weeklyUrgency, b.load.weeklyUrgency)
    if !close ua ub then ua > ub
    else
      let (sa, sb) := (a.load.sessionProjected, b.load.sessionProjected)
      if !close sa sb then sa < sb else tie ()
  | false, false =>
    let (oa, ob) := (a.load.overshoot, b.load.overshoot)
    if !close oa ob then oa < ob else tie ()

/-- Choose a source from `candidates`, or explain why none can run.

    Pure, so the interesting cases — every source limited, a scoped limit that does not apply,
    `distribute` balancing two half-used accounts — are reachable from a test. -/
def chooseFrom (mode : AuthMode) (candidates : Array Candidate) (now : Int := 0)
    : Except String String := Id.run do
  let free := candidates.filter (·.availability.isAvailable)
  if free.isEmpty then
    if candidates.isEmpty then
      return .error "no authentication sources configured"
    -- Report the source that frees up first; that is the one the caller will be waiting for.
    let mut soonest : Option (Int × String) := none
    let mut reasons : Array String := #[]
    for c in candidates do
      match c.availability with
      | .available => pure ()
      | .blocked u r =>
        reasons := reasons.push s!"{c.label}: {r}"
        if let some u := u then
          match soonest with
          | some (best, _) => if u < best then soonest := some (u, c.label)
          | none           => soonest := some (u, c.label)
    let detail := String.intercalate "; " reasons.toList
    match soonest with
    | some (u, l) => return .error s!"all sources limited ({detail}); {l} frees up {relativeToNow u now}"
    | none        => return .error s!"all sources limited ({detail})"
  match mode with
  | .ordered =>
    -- `free` preserves the configured order, so the first entry is the first usable source.
    return .ok (free.getD 0 default).label
  | .distribute =>
    let best := free.foldl (init := free.getD 0 default) fun best c =>
      if c.preferredOver best then c else best
    return .ok best.label

/-- Build the candidate list for `labels` from persisted state and choose one.

    `running label` is how many agents are on `label` right now. Callers that cannot know pass
    nothing, and every source then looks idle — which is how selection behaved before it counted. -/
def selectSource (backend : String) (labels : List String) (mode : AuthMode) (model : Option String)
    (running : String → Nat := fun _ => 0) : IO (Except String String) := do
  let now ← nowEpoch
  let mut candidates : Array Candidate := #[]
  for (label, i) in labels.zipIdx do
    let st ← loadState backend label
    candidates := candidates.push {
      label
      availability := availabilityOf st model now
      load := loadOf st model (running label) now
      lastUsed := st.lastUsedTick.getD 0
      index := i
    }
  return chooseFrom mode candidates now

/-! ## Recording outcomes

Called on every path that launches an agent, so that what one process learns is visible to the
next one regardless of which mode discovered it.

Every writer here is load-then-save with no lock across processes, which predates blocks being a
set and is not made safe by them: two workers recording different scopes on one source in the
same instant can still lose one of them, and a `markUsed` landing between another writer's load
and save can drop a block outright. The blast radius is one re-dispatch that rediscovers the
limit and records it again, which is why this has not been worth a lock file — but it is a real
race and not a benign one, and it wants fixing before anything here is trusted to be durable. -/

/-- Default block length when a run reports a limit and nothing has told us when it lifts. Long
    enough not to spin, short enough that a wrong guess costs one poll interval. -/
def defaultBackoffSecs : Int := 3600

/-- Note that a source was just dispatched to. Only used to break ties under `distribute`. -/
def markUsed (backend label : String) : IO Unit := do
  let tick ← nowDispatchTick
  modifyState backend label fun s => { s with lastUsedTick := some tick }

/-- Record an observed usage-limit hit.

    `resetHint` is a reset time recovered from the agent's own output when it offered one.
    Otherwise the block borrows the reset time of whichever polled limit is already binding, and
    failing that falls back to `defaultBackoffSecs`. -/
def markLimited (backend label : String) (model : Option String) (reason : String)
    (resetHint : Option String := none) : IO Unit := do
  let now ← nowEpoch
  let st ← loadState backend label
  let fromPoll : Option Int :=
    st.limits.foldl (init := none) fun acc l =>
      if limitIsBinding l now then
        match l.resetsAt.bind parseIso8601, acc with
        | some r, some best => some (min r best)
        | some r, none      => some r
        | none, a           => a
      else acc
  let untilEpoch :=
    (resetHint.bind parseIso8601).orElse fun _ =>
      fromPoll.orElse fun _ => some (now + defaultBackoffSecs)
  -- Upsert by scope, dropping what has expired on the way past. Two blocks with the same scope
  -- are two readings of one window, so they fold together; two with *different* scopes are
  -- different windows and both stay. Pruning here is what keeps the array bounded by the number
  -- of scopes rather than by the number of limits ever hit.
  --
  -- Every live block this reading is about is folded in, not just the first. `sameScope` is not
  -- transitive: an account can legitimately carry blocks scoped "claude-opus-4-8" and
  -- "claude-opus-5", which are not the same window as each other, while a fresh hit classified
  -- "Opus" is the same window as both. Merging with one and dropping the other would discard a
  -- live limit — including, if it was the longer one, the expiry that mattered.
  let live := st.blocks.filter (blockIsLive · now)
  let matching := live.filter fun b => sameScope b.model model
  let kept := live.filter fun b => !sameScope b.model model
  let merged := matching.foldl (init := { untilEpoch, model, reason }) mergeBlock
  saveState { st with blocks := kept.push merged }

/-- Retire the blocks a completed run disproves.

    A run that got all the way through on `model` is proof that every window covering `model` has
    passed — and proof of nothing at all about the others. Clearing the lot, which is what this
    did before, let a Sonnet run erase "Fable is spent on this account", so the next Fable task
    rediscovered that limit the only way left to it: by walking into it, a clone and a token mint
    and a whole run to learn something already known. What survives is exactly what the run did
    not exercise. -/
def markOk (backend label : String) (model : Option String := none) : IO Unit := do
  let now ← nowEpoch
  let st ← loadState backend label
  let kept := st.blocks.filter fun b => blockIsLive b now && !blockApplies b model
  if kept.size != st.blocks.size then saveState { st with blocks := kept }

/-! ## Polling -/

def defaultBaseUrl : String := "https://api.anthropic.com"

/-- Why a poll did not produce limits.

    `rateLimited` is called out separately because it is the one failure that must change future
    behaviour rather than just be reported: continuing to retry through a 429 keeps the monitor
    blind for longer and — now that the poll is itself an inference request — wastes the very
    request it is being rate-limited for. It carries the server's own `retry-after`, in seconds,
    when it sent one. -/
inductive FetchError where
  | rateLimited (retryAfterSecs : Option Int)
  | other (msg : String)
deriving Repr, Inhabited

def FetchError.message : FetchError → String
  | .rateLimited _ => "requests are being rate-limited; backing off"
  | .other m       => m

/-- How often the background poller refreshes every source, and the age past which any other
    caller considers the stored numbers stale.

    Sized against what the endpoint actually allows: it answers roughly five requests with a
    `429` and a `retry-after: 300`, so the sustainable rate is about one request per minute per
    token and every polling site has to fit inside that together. One round per source per five
    minutes leaves the rest of the budget for the paths that poll only when they must. -/
def pollIntervalSecs : Int := 300

/-- Fallback backoff after a 429 that arrived without a usable `retry-after`. -/
def pollBackoffSecs : Int := 300

/-- Backoff after a poll that failed for any other reason — a network blip, a rejected token, an
    unparseable body.

    Short, because retrying is what recovers from all three, but not zero: a failed poll leaves
    `fetchedEpoch` untouched, so without a floor here the source stays permanently *stale* and
    every claim decision retries it immediately. That turns one blip into a request per claim
    tick, which is how the endpoint's budget gets spent and how a 429 arrives next. -/
def errorBackoffSecs : Int := 60

/-- How long to stop polling a source after a poll failed.

    `errorBackoffSecs` is a floor under the 429 case too: a server that answers `retry-after: 0`
    — or a proxy that invents one — must not be able to talk us into retrying immediately, since
    the whole point of the backoff is that the next request would fail as well. -/
def FetchError.backoffSecs : FetchError → Int
  | .rateLimited ra => max (ra.getD pollBackoffSecs) errorBackoffSecs
  | .other _        => errorBackoffSecs

/-- The `retry-after` of a rate-limited response, in seconds.

    Only the delta form is read. A date is legal HTTP but this endpoint does not send one, and
    reading `Wed, 22 Jul 2026 …` as a number would produce a nonsense backoff — so anything that
    is not a plain count of seconds is reported as absent and the default is used instead. -/
def retryAfterSecs (headers : Array (String × String)) : Option Int :=
  (Utils.Http.header? headers "retry-after").bind fun raw =>
    raw.trimAscii.toString.toNat?.map Int.ofNat

/-- What a non-2xx response with no usage headers means, or `none` on a 200. Only reached as a
    fallback — the usage numbers ride on the rate-limit headers of the inference call itself, and a
    429 is intercepted by the caller (it carries `retry-after`), so neither the ordinary path nor
    the rate-limit path consults this.

    401 and 403 are not the same failure and must not be reported as though they were. The server
    answers a token it does not accept with 401 (`authentication_error`), so *that* is the
    expired-or-revoked case. A 403 comes from a token the server authenticates fine and then
    declines; it still may run agents perfectly well, so telling its owner it has expired sends
    them to rotate a credential that was never the problem. The body explains itself, so we keep
    it. A 429 still carries the usage headers, so it is handled before this is reached. -/
def statusError (status : Nat) (body : String) : Option FetchError :=
  let detail :=
    let t := body.trimAscii.toString
    if t.isEmpty then "(no response body)" else t
  if status == 200 then none
  else if status == 401 then
    some (.other s!"HTTP 401: token rejected (expired or revoked): {detail}")
  else if status == 403 then
    some (.other s!"HTTP 403: the token authenticated but was declined \
(scope or account type, not expiry): {detail}")
  else some (.other s!"HTTP {status}: {detail}")

/-! ## Reading limits from rate-limit headers

A long-lived token minted by `claude setup-token` carries inference scope but not the profile
scope `GET /api/oauth/usage` requires, so that endpoint answers it with 403. The same subscription
windows ride on the `anthropic-ratelimit-unified-*` response headers of an ordinary inference call,
which any inference-scoped token can make — so a `max_tokens: 1` probe recovers them. This spends a
real (if tiny) request against the very windows it reads; that trade is what lets a setup-token be
monitored at all. -/

/-- The model the header probe runs against: the cheapest, with `max_tokens: 1`. The probe exists
    to read headers, not to generate anything. -/
def probeModel : String := "claude-haiku-4-5"

/-- The headers report utilisation as a fraction (`0.15`); `Limit.percent` is a whole percent. -/
private def percentOfFraction (s : String) : Option Nat :=
  match Json.parse s with
  | .ok (.num n) => let scaled := n.toFloat * 100.0 + 0.5; some scaled.toUInt64.toNat
  | _            => none

/-- Build one `Limit` from a `unified-<window>-*` header triple, or `none` when the window is
    absent. `winKey` is `5h` / `7d`; the headers carry no per-model-family (scoped) windows, so
    `scopeModel` is always `none` and an exhausted single-model weekly limit is invisible here.

    The `-status` header is not a binary allowed/rejected: the server also reports a warning state
    once a window is merely *near* its cap — the same region this function already calls
    `severity := "warning"`. Only a rejection may set `isActive`, because that is the field
    `limitIsBinding` reads to idle a source, and reading a warning as a rejection idles an account
    that still has a fifth of its week left, for the rest of the week. An unrecognised status is
    therefore usable rather than exhausted: a run that really is refused is caught by `markLimited`
    the moment it happens, so failing open here costs a rejected request per poll interval, while
    failing closed costs days of an idle account.

    The match is a lowercased prefix rather than an equality for the same reason `LimitKind` keeps
    the kinds it cannot name: the status field is an open enum, and a future `rejected_…` spelling
    is a rejection whatever else it says. Values arrive trimmed but not case-folded —
    `Utils.Http.header?` lowercases header *names* only. -/
private def limitFromHeaders (headers : Array (String × String)) (winKey : String)
    (kind : LimitKind) (group : String) : Option Limit := do
  let util ← Utils.Http.header? headers s!"anthropic-ratelimit-unified-{winKey}-utilization"
  let percent := (percentOfFraction util).getD 0
  let resetsAt := (Utils.Http.header? headers s!"anthropic-ratelimit-unified-{winKey}-reset").bind
    (·.toInt?) |>.map secsToIso8601
  let status := (Utils.Http.header? headers s!"anthropic-ratelimit-unified-{winKey}-status").getD
    "allowed"
  return {
    kind, group, percent
    severity := if percent ≥ 100 then "critical" else if percent ≥ 75 then "warning" else "normal"
    resetsAt
    scopeModel := none
    isActive := status.toLower.startsWith "rejected" || percent ≥ 100
  }

/-- Turn the `anthropic-ratelimit-unified-*` headers into the same `Limit` values the endpoint body
    would have yielded. Empty when no unified header is present (a transport error, an API-key error
    page), which the caller treats as "read the status instead". -/
def parseUnifiedHeaders (headers : Array (String × String)) : Array Limit := Id.run do
  let mut out : Array Limit := #[]
  if let some l := limitFromHeaders headers "5h" .session   "session" then out := out.push l
  if let some l := limitFromHeaders headers "7d" .weeklyAll "weekly"  then out := out.push l
  return out

/-- Read the subscription windows for one OAuth token off the rate-limit headers of a minimal
    inference call.

    Unlike `GET /api/oauth/usage` this is *not* free: it spends a real `max_tokens: 1` request that
    nudges the very windows it reads. That is the deliberate cost of supporting `setup-token`s,
    which cannot reach the metadata endpoint (403, no profile scope) but can make this call. A 429
    still carries the headers, so a subscription block reports itself with its reset time rather
    than as an opaque failure. The OAuth beta header is required for the bearer token to be
    accepted on `/v1/messages`. -/
def fetchUtilization (token : String) (baseUrl : String := defaultBaseUrl)
    : IO (Except FetchError (Array Limit)) := do
  let reqBody := "{\"model\":\"" ++ probeModel ++
    "\",\"max_tokens\":1,\"messages\":[{\"role\":\"user\",\"content\":\"hi\"}]}"
  try
    let (status, headers, body) ← Utils.Http.postBearerFull s!"{baseUrl}/v1/messages" token
      reqBody (extraHeaders := #["anthropic-version: 2023-06-01",
                                 "anthropic-beta: oauth-2025-04-20",
                                 "Content-Type: application/json"])
    let limits := parseUnifiedHeaders headers
    if !limits.isEmpty then
      return .ok limits
    -- No usage headers. A 429 is a rate limit to back off from, honouring the server's
    -- `retry-after`; anything else is an auth or other failure the status explains.
    if status == 429 then
      return .error (.rateLimited (retryAfterSecs headers))
    match statusError status body with
    | some err => return .error err
    | none     => return .error (.other "no usage headers in a 200 inference response")
  catch e =>
    return .error (.other (toString e))

/-- The OAuth token for a configured source, if it has one.

    Only OAuth sources have a subscription to report on. An API-key source bills per token
    against an organisation and has no session/weekly window to poll, so it is left out of
    polling entirely and stays available until a run proves otherwise. -/
def oauthTokenOf (cfg : AppConfig) (backend label : String) : Option String := do
  let auth ← cfg.agentAuthConfigs.find? (·.name == backend)
  let src ← auth.authSources.find? (·.label == label)
  match src.kind with
  | .oauthToken t => some t
  | .apiKey _ _   => none

/-- Poll one source and fold the result into its persisted state. `.ok false` means the source
    has nothing to poll (not an OAuth source), which is not an error.

    `running` is how many agents are on the source as the poll is made, and only the daemon's
    poller knows it; a poll made with it teaches the source's `Estimate`. Every other caller polls
    without, and leaves the estimate as it was. -/
def refresh (cfg : AppConfig) (backend label : String) (running : Option Nat := none)
    : IO (Except String Bool) := do
  match oauthTokenOf cfg backend label with
  | none => return .ok false
  | some token =>
    match ← fetchUtilization token with
    | .error err =>
      let now ← nowEpoch
      let msg := err.message
      modifyState backend label fun s => { s with
        lastError := some msg
        -- Every failure suppresses polling for a while, because every failure leaves the source
        -- stale and would otherwise be retried by the next caller through. A 429 is held off for
        -- as long as the server asked; anything else only long enough that retrying — which is
        -- what recovers from a blip — stays cheap.
        pollAfter := some (now + err.backoffSecs) }
      return .error msg
    | .ok limits =>
      let now ← nowEpoch
      let st ← loadState backend label
      -- A poll that shows nothing binding retires the blocks it is *evidence about*, and it is
      -- evidence only about the windows it can see. The probe is a one-token inference call and
      -- the headers carry the session and weekly-all windows: nothing model-scoped. So a quiet
      -- poll says nothing whatsoever about an exhausted Fable week, and retiring a scoped block
      -- here would forget a limit this poll cannot observe — which is exactly how an account
      -- comes to look runnable and is not.
      let stillBlocked := limits.any (limitIsBinding · now)
      let blocks := st.blocks.filter fun b =>
        blockIsLive b now && (stillBlocked || b.model.isSome)
      saveState { st with
        limits, fetchedEpoch := some now, lastError := none, pollAfter := none
        blocks
        estimate := match running with
          | some n => st.estimate.observe limits n now
          | none   => st.estimate }
      -- History is a nicety and monitoring is not, so a history file that cannot be written
      -- reports itself and leaves the poll — which has already been stored — successful.
      try
        recordPoll backend label limits now
      catch e =>
        IO.eprintln s!"[usage] {backend}/{label}: could not record history: {e}"
      return .ok true

/-- Every label configured for `backend`. -/
def configuredLabels (cfg : AppConfig) (backend : String) : List String :=
  match cfg.agentAuthConfigs.find? (·.name == backend) with
  | none   => []
  | some a => a.authSources.toList.map (·.label)

/-- Whether this backend should be polled automatically. Off means never spend a probe request on
    the daemon or claim-time path; a limit is then discovered only when a run hits it. A backend
    with no config is treated as pollable — there is nothing to poll, so the choice is moot. -/
def pollingEnabled (cfg : AppConfig) (backend : String) : Bool :=
  match cfg.agentAuthConfigs.find? (·.name == backend) with
  | some a => a.pollUsage
  | none   => true

/-- Poll every OAuth source configured for `backend`. Errors are recorded per source and never
    propagate: a poller that throws would take the daemon fiber down with it. A no-op when polling
    is disabled for the backend.

    `running`, when given, is how many agents each source has on it; see `refresh`. -/
def refreshAll (cfg : AppConfig) (backend : String) (running : Option (String → Nat) := none)
    : IO Unit := do
  unless pollingEnabled cfg backend do return
  let now ← nowEpoch
  for label in configuredLabels cfg backend do
    try
      let st ← loadState backend label
      let backingOff : Bool := match st.pollAfter with | some p => decide (p > now) | none => false
      unless backingOff do
        match ← refresh cfg backend label (running.map (· label)) with
        | .error e => IO.eprintln s!"[usage] {backend}/{label}: {e}"
        | .ok _    => pure ()
    catch e => IO.eprintln s!"[usage] {backend}/{label}: {e}"

/-- Poll only if the last successful poll is older than `ttlSecs`.

    The default is the background poller's own interval, which makes this a *fallback* rather
    than a second poller: while the queue daemon is up its poller keeps every source fresher
    than this and nothing here ever fires. What it still covers is the case with no daemon
    running — a bare `orchestra run`, or a `usage` invocation on a machine that only ever
    dispatches by hand — where the stored numbers would otherwise be arbitrarily old.

    It is deliberately not shorter. Selection used to refresh on the queue daemon's claim path,
    which re-resolves every pending entry once a second while the queue is not empty. At the
    minute-scale TTL this used to carry, that one path spent the endpoint's whole budget, and the
    429 it earned then blinded every other caller for five minutes. The claim path no longer
    refreshes at all (see `resolveLabel`'s `refresh`).

    A no-op when polling is disabled for the backend. -/
def ensureFresh (cfg : AppConfig) (backend label : String)
    (ttlSecs : Int := pollIntervalSecs) : IO Unit := do
  unless pollingEnabled cfg backend do return
  let st ← loadState backend label
  let now ← nowEpoch
  let stale : Bool := match st.fetchedEpoch with
    | none   => true
    | some f => decide (now - f > ttlSecs)
  -- The endpoint meters requests, so a failed poll has to actually stop us asking: `pollAfter`
  -- is the only gate that holds when `fetchedEpoch` is stale precisely *because* the poll
  -- failed. Without it a source in that state is re-polled by every caller that passes through.
  let backingOff : Bool := match st.pollAfter with | some p => decide (p > now) | none => false
  if stale && !backingOff then
    try
      let _ ← refresh cfg backend label
    catch _ => pure ()

/-! ## Resolution

The single entry point every running mode shares. -/

/-- The labels a task may run on, in preference order, and the mode that picks between them.

    Three ways to say it, narrowest first:
    * `authSources` — an explicit candidate list, the only form a task can fail over on;
    * `authSource` — a single forced label (the pre-existing field, still honoured);
    * neither — the backend's `default_auth_source`, or its sole source if it has exactly one.

    The mode is the task's when it names one and the config's otherwise, which is why `mode` is
    an `Option` all the way down rather than defaulting to `ordered` at the edges. A task that
    names nothing must not be read as asking for `ordered`: the paths that arrive here with
    nothing named are exactly the ones with no field to name a mode in (a dispatched role, a
    concert step), and walking a pool in `ordered` takes the first account every time — the
    pinning a pool exists to undo. A task that *does* say `auth_mode` still gets it, whether or
    not it brought its own candidates.

    A pool naming a label the backend does not configure drops that label. Under `distribute` an
    unknown label would otherwise win outright — nothing has been recorded against it, so it
    looks like the least-consumed source — and the task would then fail in `resolveAuthEnv`
    against a source that does not exist. Dropping it spends the pool on the members that are
    real, which is how a partly-mistyped list should degrade.

    An empty candidate list means the config has nothing to choose between, which happens on the
    legacy flat-token config that predates named sources. `resolveLabel` reports that as "no
    label", not as an error, so those installs keep working untouched. -/
def resolutionFor (cfg : AppConfig) (backend : String) (authSources : List String)
    (authSource : Option String) (mode : Option AuthMode) : List String × AuthMode :=
  let taskMode := mode.getD .ordered
  if !authSources.isEmpty then (authSources, taskMode)
  else match authSource with
    | some l => ([l], taskMode)
    | none   =>
      match cfg.agentAuthConfigs.find? (·.name == backend) with
      | none   => ([], taskMode)
      | some a =>
        let pool := a.defaultAuthSources.filter fun l => a.authSources.any (·.label == l)
        if !pool.isEmpty then (pool, mode.getD a.defaultAuthMode)
        else if a.authSources.size == 1 then ([a.authSources[0]!.label], taskMode)
        else ([], taskMode)

/-- Pick the authentication source a task should run on, refreshing stale usage data first unless
    `refresh := false`.

    This is the single entry point every running mode goes through — the queue daemon deciding
    what to claim, `orchestra run`, and an interactive session — so that a limit discovered by
    one is honoured by all of them.

    `.ok none` means "this install has no named sources"; the caller falls through to the legacy
    flat-token path. `.error` means every candidate is currently limited, and the message says
    which limit and when it lifts.

    `refresh := false` decides from the stored numbers alone, without polling a stale source
    first. The queue daemon's claim path passes it. It resolves under the claim mutex, and a
    refresh is a network probe plus a history write for every stale candidate. Doing that under
    the mutex stalls every claim, and every slot release too, for as long as it takes. On
    2026-09-27 that was hours, with a pool of ~23 sources. The daemon's own poller keeps the
    sources of its configuration about one sweep old, and a limit it has not seen yet is caught by
    `markLimited` when a run hits it.

    Some numbers go without a poll this way. These are sources the poller does not sweep: ones
    known only to an entry's own `configPath`, a backend with polling off, or a source in 429
    backoff. For them, the stored numbers stay where they were last left until a run hits a limit. That is
    the same position a backend with polling disabled is always in.

    `running label` is how many agents are on `label` right now; see `selectSource`. -/
def resolveLabel (cfg : AppConfig) (backend : String) (authSources : List String)
    (authSource : Option String) (mode : Option AuthMode) (model : Option String)
    (refresh : Bool := true) (running : String → Nat := fun _ => 0)
    : IO (Except String (Option String)) := do
  let (candidates, mode) := resolutionFor cfg backend authSources authSource mode
  if candidates.isEmpty then return .ok none
  if refresh then
    for label in candidates do
      ensureFresh cfg backend label
  match ← selectSource backend candidates mode model running with
  | .ok label => return .ok (some label)
  | .error e  => return .error e

end Orchestra.Usage
