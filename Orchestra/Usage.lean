/-
Usage-limit monitoring for agent authentication sources.

A Claude subscription is not one limit but several running at once: a rolling session window, a
weekly total, and weekly limits scoped to a single model family. `GET /api/oauth/usage` reports
all of them at once — the same call Claude Code itself makes — and the response is the input to
every decision in this module:

```
"limits": [
  {"kind":"session",       "percent":3,  "resets_at":"…", "scope":null,                       "is_active":false},
  {"kind":"weekly_all",    "percent":75, "resets_at":"…", "scope":null,                       "is_active":false},
  {"kind":"weekly_scoped", "percent":100,"resets_at":"…", "scope":{"model":{"display_name":"Fable"}}, "is_active":true}
]
```

The scoped entry is why availability is a question about *(source, model)* rather than about a
source alone: an exhausted weekly-Opus limit leaves Sonnet work on the same account perfectly
runnable, and treating the account as dead would idle it for a week.

Two things write to the state this module keeps, and they cover each other:

* the **poll** above, which sees a limit coming before anything runs into it, and knows the exact
  reset time; and
* an **observed hit** — a run that came back rate-limited — recorded by `markLimited`. This is
  the authoritative signal (it happened) but carries no reset time of its own, so it borrows one
  from the last poll and otherwise falls back to a conservative default.

State lives on disk, one file per `(backend, label)`, because the processes that need to agree
about it are genuinely separate: `orchestra run` in a terminal and the queue daemon are different
OS processes, and a limit one of them discovers has to stop the other.
-/
import Lean.Data.Json
import Orchestra.Config
import Orchestra.Dirs
import Orchestra.Utils.Http
import Std.Time

open Lean (Json FromJson ToJson)

namespace Orchestra.Usage

/-! ## Time

Reset times come back as RFC 3339 with a fractional part and a numeric offset
(`2026-07-22T18:59:59.573616+00:00`), while orchestra's own timestamps are `date -u`'s
`2026-07-22T18:59:59Z`. Comparing those as strings gives the wrong answer, so both are parsed to
epoch seconds. Parsing is pure so the interesting cases are reachable from a test without a
clock. -/

private def charAt (cs : List Char) (i : Nat) : Option Char :=
  (cs.drop i).head?

private def digitsAt (cs : List Char) (start len : Nat) : Option Nat :=
  let sub := (cs.drop start).take len
  if sub.length != len || !sub.all Char.isDigit then none
  else some (sub.foldl (fun acc c => acc * 10 + (c.toNat - '0'.toNat)) 0)

/-- Days between 1970-01-01 and `y-m-d`, by Howard Hinnant's `days_from_civil`. Valid for any
    proleptic Gregorian date at or after year 1, which covers every timestamp a server will
    hand us. -/
private def daysFromCivil (y0 m d : Nat) : Int :=
  let y := if m ≤ 2 then y0 - 1 else y0
  let era := y / 400
  let yoe := y - era * 400
  let mp := if m > 2 then m - 3 else m + 9
  let doy := (153 * mp + 2) / 5 + d - 1
  let doe := yoe * 365 + yoe / 4 - yoe / 100 + doy
  (↑(era * 146097 + doe) : Int) - 719468

/-- Parse an ISO 8601 / RFC 3339 timestamp to epoch seconds.

    Accepts `Z`, `±hh:mm`, `±hhmm`, and a missing offset (read as UTC), and ignores any
    fractional-seconds part. Returns `none` rather than a wrong answer on anything else — a
    timestamp we cannot read must not silently become "already expired". -/
def parseIso8601 (s : String) : Option Int := do
  let cs := s.trimAscii.toString.toList
  let y  ← digitsAt cs 0 4
  guard (charAt cs 4 == some '-')
  let mo ← digitsAt cs 5 2
  guard (charAt cs 7 == some '-')
  let d  ← digitsAt cs 8 2
  let sep ← charAt cs 10
  guard (sep == 'T' || sep == 't' || sep == ' ')
  let h  ← digitsAt cs 11 2
  guard (charAt cs 13 == some ':')
  let mi ← digitsAt cs 14 2
  guard (charAt cs 16 == some ':')
  let sec ← digitsAt cs 17 2
  guard (1 ≤ mo && mo ≤ 12 && 1 ≤ d && d ≤ 31 && h ≤ 23 && mi ≤ 59 && sec ≤ 60)
  let rest := cs.drop 19
  let rest := if rest.head? == some '.' then (rest.drop 1).dropWhile Char.isDigit else rest
  let offset : Option Int ←
    match rest.head? with
    | none      => pure (some 0)
    | some 'Z'  => pure (some 0)
    | some 'z'  => pure (some 0)
    | some c    =>
      if c == '+' || c == '-' then
        match digitsAt (rest.drop 1) 0 2 with
        | none    => pure none
        | some oh =>
          let t := rest.drop 1
          let t := if charAt t 2 == some ':' then t.drop 3 else t.drop 2
          let om := (digitsAt t 0 2).getD 0
          let mag : Int := ↑(oh * 3600 + om * 60)
          pure (some (if c == '-' then -mag else mag))
      else pure none
  let off ← offset
  return daysFromCivil y mo d * 86400 + ↑(h * 3600 + mi * 60 + sec) - off

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

/-- Everything known about one `(backend, label)` authentication source. -/
structure SourceState where
  backend       : String
  label         : String
  fetchedEpoch  : Option Int := none
  limits        : Array Limit := #[]
  block         : Option Block := none
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
deriving Repr, Inhabited

instance : ToJson SourceState where
  toJson s :=
    let fields : List (String × Json) := [
      ("backend", Json.str s.backend),
      ("label",   Json.str s.label),
      ("limits",  ToJson.toJson s.limits)
    ]
    let fields := if let some e := s.fetchedEpoch  then fields ++ [("fetched_epoch",   Json.num e)] else fields
    let fields := if let some b := s.block         then fields ++ [("block",           ToJson.toJson b)] else fields
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
    let block := (j.getObjValAs? Block "block").toOption
    let lastUsedTick := (j.getObjValAs? Int "last_used_tick").toOption
    let lastError := (j.getObjValAs? String "last_error").toOption
    let pollAfter := (j.getObjValAs? Int "poll_after").toOption
    return { backend, label, limits, fetchedEpoch, block, lastUsedTick, lastError, pollAfter }

def usageDir : IO System.FilePath :=
  return (← Dirs.dataBase) / "usage"

private def statePath (backend label : String) : IO System.FilePath := do
  -- Labels come from config and are used as filenames; anything that could escape the directory
  -- is flattened rather than trusted.
  let safe := label.map fun c => if c.isAlphanum || c == '-' || c == '_' then c else '_'
  return (← usageDir) / backend / s!"{safe}.json"

def loadState (backend label : String) : IO SourceState := do
  let path ← statePath backend label
  if !(← path.pathExists) then return { backend, label }
  match Json.parse (← IO.FS.readFile path) with
  | .error _ => return { backend, label }
  | .ok j    => match FromJson.fromJson? j with
    | .error _ => return { backend, label }
    | .ok s    => return s

def saveState (s : SourceState) : IO Unit := do
  let path ← statePath s.backend s.label
  if let some dir := path.parent then IO.FS.createDirAll dir
  IO.FS.writeFile path (Json.compress (ToJson.toJson s))

private def modifyState (backend label : String) (f : SourceState → SourceState) : IO Unit := do
  saveState (f (← loadState backend label))

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
  if let some b := st.block then
    let live := match b.untilEpoch with | some u => u > now | none => true
    let applies := match b.model with
      | none   => true
      | some m => modelMatchesScope m model
    if live && applies then
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

/-- The highest binding-relevant utilisation across the limits that could apply to `model`. Used
    to order sources under `distribute`; a source we have never polled reports 0 so that a
    freshly configured account is tried rather than avoided. -/
def pressureOf (st : SourceState) (model : Option String) : Nat := Id.run do
  let mut worst := 0
  for l in st.limits do
    let applies := match l.scopeModel with
      | none   => true
      | some s => modelMatchesScope s model
    if applies && l.percent > worst then worst := l.percent
  return worst

/-! ## Selection -/

/-- A source considered for selection, with the verdict that decided it. -/
structure Candidate where
  label        : String
  availability : Availability
  pressure     : Nat
  lastUsed     : Int
  /-- Position in the configured list; the tiebreak of last resort, so selection is
      deterministic when two sources are genuinely indistinguishable. -/
  index        : Nat
deriving Repr, Inhabited

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
      if c.pressure < best.pressure then c
      else if c.pressure > best.pressure then best
      else if c.lastUsed < best.lastUsed then c
      else if c.lastUsed > best.lastUsed then best
      else if c.index < best.index then c else best
    return .ok best.label

/-- Build the candidate list for `labels` from persisted state and choose one. -/
def select (backend : String) (labels : List String) (mode : AuthMode) (model : Option String)
    : IO (Except String String) := do
  let now ← nowEpoch
  let mut candidates : Array Candidate := #[]
  for (label, i) in labels.zipIdx do
    let st ← loadState backend label
    candidates := candidates.push {
      label
      availability := availabilityOf st model now
      pressure := pressureOf st model
      lastUsed := st.lastUsedTick.getD 0
      index := i
    }
  return chooseFrom mode candidates now

/-! ## Recording outcomes

Called on every path that launches an agent, so that what one process learns is visible to the
next one regardless of which mode discovered it. -/

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
  saveState { st with block := some { untilEpoch, model, reason } }

/-- Clear a block after a run on this source succeeded. The block was a guess about a window we
    could not see the end of; a completed run is proof it has passed. -/
def markOk (backend label : String) : IO Unit := do
  let st ← loadState backend label
  if st.block.isSome then saveState { st with block := none }

/-! ## Polling -/

def defaultBaseUrl : String := "https://api.anthropic.com"

/-- Why a poll did not produce limits.

    `rateLimited` is called out separately because it is the one failure that must change future
    behaviour rather than just be reported: the endpoint meters requests independently of any
    subscription, and continuing to poll through a 429 keeps the monitor blind for longer. It
    carries the server's own `retry-after`, in seconds, when it sent one. -/
inductive FetchError where
  | rateLimited (retryAfterSecs : Option Int)
  | other (msg : String)
deriving Repr, Inhabited

def FetchError.message : FetchError → String
  | .rateLimited _ => "the usage endpoint is rate-limiting requests; backing off"
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
  (Utils.Http.header? headers "retry-after").bind fun v =>
    v.trimAscii.toString.toNat?.map Int.ofNat

/-- Fetch and parse `GET /api/oauth/usage` for one OAuth token.

    Note this is an account-metadata call, not an inference call: it reports quota, it does not
    consume any. It costs no input or output tokens and does not move the very numbers it
    returns. It is metered as *requests*, though — see `pollBackoffSecs`. -/
def fetchUtilization (token : String) (baseUrl : String := defaultBaseUrl)
    : IO (Except FetchError (Array Limit)) := do
  try
    let (status, headers, body) ← Utils.Http.getBearerFull s!"{baseUrl}/api/oauth/usage" token
      (extraHeaders := #["Content-Type: application/json"])
    if status == 429 then
      return .error (.rateLimited (retryAfterSecs headers))
    if status == 401 || status == 403 then
      return .error (.other s!"HTTP {status}: token rejected (expired or revoked)")
    if status != 200 then
      return .error (.other s!"HTTP {status}: {body.trimAscii.toString}")
    match parseUtilization body with
    | .error e => return .error (.other s!"could not parse usage response: {e}")
    | .ok ls   => return .ok ls
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
    has nothing to poll (not an OAuth source), which is not an error. -/
def refresh (cfg : AppConfig) (backend label : String) : IO (Except String Bool) := do
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
      -- A poll that shows nothing binding retires a block we had guessed at: the poll knows the
      -- real window and this one has passed.
      let stillBlocked := limits.any (limitIsBinding · now)
      saveState { st with
        limits, fetchedEpoch := some now, lastError := none, pollAfter := none
        block := if stillBlocked then st.block else none }
      return .ok true

/-- Every label configured for `backend`. -/
def configuredLabels (cfg : AppConfig) (backend : String) : List String :=
  match cfg.agentAuthConfigs.find? (·.name == backend) with
  | none   => []
  | some a => a.authSources.toList.map (·.label)

/-- Poll every OAuth source configured for `backend`. Errors are recorded per source and never
    propagate: a poller that throws would take the daemon fiber down with it. -/
def refreshAll (cfg : AppConfig) (backend : String) : IO Unit := do
  let now ← nowEpoch
  for label in configuredLabels cfg backend do
    try
      let st ← loadState backend label
      let backingOff : Bool := match st.pollAfter with | some p => decide (p > now) | none => false
      unless backingOff do
        match ← refresh cfg backend label with
        | .error e => IO.eprintln s!"[usage] {backend}/{label}: {e}"
        | .ok _    => pure ()
    catch e => IO.eprintln s!"[usage] {backend}/{label}: {e}"

/-- Poll only if the last successful poll is older than `ttlSecs`.

    The default is the background poller's own interval, which makes this a *fallback* rather
    than a second poller: while the queue daemon is up its poller keeps every source fresher
    than this and nothing here ever fires. What it still covers is the case with no daemon
    running — a bare `orchestra run`, or a `usage` invocation on a machine that only ever
    dispatches by hand — where the stored numbers would otherwise be arbitrarily old.

    It is deliberately not shorter. Selection runs on the queue daemon's claim path, which
    re-resolves every pending entry once a second for as long as the queue is not empty; at the
    minute-scale TTL this used to carry, that single path spent the whole of the endpoint's
    budget on its own and the 429 it earned then blinded every other caller for five minutes. -/
def ensureFresh (cfg : AppConfig) (backend label : String)
    (ttlSecs : Int := pollIntervalSecs) : IO Unit := do
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

/-- The labels a task may run on, in preference order.

    Three ways to say it, narrowest first:
    * `authSources` — an explicit candidate list, the only form that can fail over;
    * `authSource` — a single forced label (the pre-existing field, still honoured);
    * neither — the backend's `default_auth_source`, or its sole source if it has exactly one.

    An empty result means the config has nothing to choose between, which happens on the legacy
    flat-token config that predates named sources. `resolveLabel` reports that as "no label",
    not as an error, so those installs keep working untouched. -/
def candidatesFor (cfg : AppConfig) (backend : String) (authSources : List String)
    (authSource : Option String) : List String :=
  if !authSources.isEmpty then authSources
  else match authSource with
    | some l => [l]
    | none   =>
      match cfg.agentAuthConfigs.find? (·.name == backend) with
      | none   => []
      | some a => match a.defaultAuthSource with
        | some d => [d]
        | none   => if a.authSources.size == 1 then [a.authSources[0]!.label] else []

/-- Pick the authentication source a task should run on, refreshing usage data first.

    This is the single entry point every running mode goes through — the queue daemon deciding
    what to claim, `orchestra run`, and an interactive session — so that a limit discovered by
    one is honoured by all of them.

    `.ok none` means "this install has no named sources"; the caller falls through to the legacy
    flat-token path. `.error` means every candidate is currently limited, and the message says
    which limit and when it lifts. -/
def resolveLabel (cfg : AppConfig) (backend : String) (authSources : List String)
    (authSource : Option String) (mode : AuthMode) (model : Option String)
    : IO (Except String (Option String)) := do
  let candidates := candidatesFor cfg backend authSources authSource
  if candidates.isEmpty then return .ok none
  for label in candidates do
    ensureFresh cfg backend label
  match ← select backend candidates mode model with
  | .ok label => return .ok (some label)
  | .error e  => return .error e

end Orchestra.Usage
