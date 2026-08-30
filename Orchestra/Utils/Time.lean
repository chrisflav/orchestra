/-!
# Wall-clock timestamps

Parsing and formatting RFC 3339 instants, and the record ordering built on them.

Timestamps reach orchestra in more than one shape: an API reports a limit reset as
`2026-07-22T18:59:59.573616+00:00`, while orchestra's own stores write `date -u`'s
`2026-07-22T18:59:59Z`. Comparing those as strings gives the wrong answer, so both are parsed
to epoch seconds first.

This lives below `Orchestra.Usage`, where the parser was first needed, because ordering records
by age needs it much further down: `TaskStore`, `Queue` and `Interactive.Store` all sort by when
a record was made, and none of them can import the usage tracker to do it.

Everything here is pure, so every interesting case is reachable from a test without a clock.
-/

namespace Orchestra.Time

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

/-- Civil date `(year, month, day)` for a day count since 1970-01-01, by Howard Hinnant's
    `civil_from_days` — the inverse of `daysFromCivil`. Only ever called with non-negative day
    counts (reset times are in the future), so the Nat arithmetic never underflows. -/
private def civilFromDays (z0 : Nat) : Nat × Nat × Nat :=
  let z := z0 + 719468
  let era := z / 146097
  let doe := z - era * 146097
  let yoe := (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
  let y := yoe + era * 400
  let doy := doe - (365 * yoe + yoe / 4 - yoe / 100)
  let mp := (5 * doy + 2) / 153
  let d := doy - (153 * mp + 2) / 5 + 1
  let m := if mp < 10 then mp + 3 else mp - 9
  (if m ≤ 2 then y + 1 else y, m, d)

private def pad2 (n : Nat) : String := if n < 10 then s!"0{n}" else toString n

private def pad4 (n : Nat) : String :=
  let s := toString n
  String.ofList (List.replicate (4 - s.length) '0') ++ s

/-- Format epoch seconds as `YYYY-MM-DDTHH:MM:SSZ`. The inverse direction of `parseIso8601`: the
    usage endpoint reports resets as timestamps, but the rate-limit *headers* report them as epoch
    seconds, and `Limit.resetsAt` is a string every consumer re-parses with `parseIso8601`. -/
def secsToIso8601 (epoch : Int) : String :=
  if epoch < 0 then "1970-01-01T00:00:00Z"
  else
    let e := epoch.toNat
    let (y, mo, d) := civilFromDays (e / 86400)
    let rem := e % 86400
    s!"{pad4 y}-{pad2 mo}-{pad2 d}T{pad2 (rem / 3600)}:{pad2 (rem % 3600 / 60)}:{pad2 (rem % 60)}Z"

/-! ## Ordering records by age

The stores name their records with `uniqueToken`, which mints ids from `IO.monoNanosNow` — a
clock that counts from boot on Linux. Inside one boot those ids increase, and sorting by them
sorts by age, which is what the stores used to do. Across a reboot they do not: the clock
restarts at zero, so every id minted after the reboot sorts *below* every id minted before it.
A store ordered that way answers with its pre-reboot records first and reports the run that
finished a minute ago as its oldest.

What orders records is the wall clock, so that is what these compare. Timestamps are recorded
to the second and ties are routine — a dispatcher enqueues several tasks in one second — so the
id breaks them. Inside a single second the id is a correct tiebreaker for exactly the reason it
fails across a reboot: within one boot it is monotone.

A timestamp that will not parse sorts as oldest, so a malformed record stays visible at the end
of a listing rather than displacing the newest one at the front.
-/

/-- What a record is ordered by: the instant it was made, with its id to break ties. -/
abbrev AgeKey := Option Int × String

/-- The age key of a record with timestamp `timestamp` and id `id`. -/
def ageKey (timestamp id : String) : AgeKey := (parseIso8601 timestamp, id)

/-- `true` when `a` is strictly older than `b`. Unparseable timestamps count as oldest. -/
def AgeKey.olderThan (a b : AgeKey) : Bool :=
  match a.1, b.1 with
  | none,   none   => a.2 < b.2
  | none,   some _ => true
  | some _, none   => false
  | some x, some y => if x != y then x < y else a.2 < b.2

/-- Sort records newest first, by `timeOf` with `idOf` breaking ties.

    Each timestamp is parsed once rather than once per comparison: these run on every listing
    the dashboard serves, over every task ever recorded. -/
def sortNewestFirst (timeOf idOf : α → String) (xs : Array α) : Array α :=
  let keyed := xs.map fun x => (ageKey (timeOf x) (idOf x), x)
  (keyed.qsort fun a b => AgeKey.olderThan b.1 a.1).map (·.2)

/-- Sort records oldest first, by `timeOf` with `idOf` breaking ties. -/
def sortOldestFirst (timeOf idOf : α → String) (xs : Array α) : Array α :=
  let keyed := xs.map fun x => (ageKey (timeOf x) (idOf x), x)
  (keyed.qsort fun a b => AgeKey.olderThan a.1 b.1).map (·.2)

end Orchestra.Time
