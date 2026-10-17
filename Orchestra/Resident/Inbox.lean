import Orchestra.Store
import Orchestra.Usage
import Orchestra.TaskStore
import Orchestra.Resident.Feed
import Lean.Data.Json

open Lean (Json ToJson FromJson)

/-!
# A resident's inbox, and what it has spent

The two tables a resident session adds to the record database (`docs/design/resident-sessions.md`
§4, §8):

* `resident_inbox` — everything a resident is to be handed, in the order it arrived: a human
  message from the chat, an event a listener delivered (`deliver_to`), and — in a later phase — a
  feed's events. An item is **ready** until a turn that carried it produced a result: written into
  a turn, it is marked `sent_in` that turn; on the turn's `result`, `confirmed`. A turn that never
  finished — the process died, the write failed — leaves its items ready, and the next turn carries
  them again under a note saying they may have been seen.
* `resident_spend` — USD per resident per budget day, from each turn's cost. What the day's
  remaining allowance, and so the next process's `--max-budget-usd`, is computed from.

`(resident, source, event_id)` is unique, so a source that hands the same event twice — a feed that
reconnects and replays, a listener tick that dies after delivering — inserts it once.

The pure halves — which items are ready, when a batch is due, how the turn reads, how long to back
off — are separate functions so they can be tested without a database.
-/

namespace Orchestra.Resident

/-- One inbox item, as the store reads it. -/
structure InboxItem where
  id : Nat
  resident : String
  /-- `"human"`, `"listener:<name>"`, later `"zulip:<site>"`. -/
  source : String
  eventId : String
  /-- From a sender (or a source) the resident may take instructions from. -/
  trusted : Bool
  receivedAt : String
  /-- What the turn renders: `text`, and whatever the source adds (`listener`, `from`). -/
  payload : Json
  /-- The turn it was written into, if any. -/
  sentIn : Option Nat := none
  /-- The transcript seq of that turn's text, so a client can stream from it. -/
  sentSeq : Option Nat := none
  confirmed : Bool := false
deriving Inhabited

instance : ToJson InboxItem where
  toJson i := Json.mkObj [
    ("id", ToJson.toJson i.id), ("resident", .str i.resident), ("source", .str i.source),
    ("eventId", .str i.eventId), ("trusted", .bool i.trusted), ("receivedAt", .str i.receivedAt),
    ("payload", i.payload),
    ("sentIn", match i.sentIn with | some n => ToJson.toJson n | none => .null),
    ("sentSeq", match i.sentSeq with | some n => ToJson.toJson n | none => .null),
    ("confirmed", .bool i.confirmed),
    ("state", .str (if i.confirmed then "confirmed" else if i.sentIn.isSome then "sent"
                    else "waiting"))]

private def ofRow (r : Store.ResidentInboxRow) : InboxItem :=
  { id := r.id.toNat, resident := r.resident, source := r.source, eventId := r.event_id
    trusted := r.trusted, receivedAt := r.received_at
    payload := (Json.parse r.payload).toOption.getD (Json.mkObj [("text", .str r.payload)])
    sentIn := r.sent_in.map (·.toNat), sentSeq := r.sent_seq.map (·.toNat)
    confirmed := r.confirmed }

/-- The text an item asks the turn to carry. -/
def InboxItem.text (i : InboxItem) : String :=
  (i.payload.getObjValAs? String "text").toOption.getD (i.payload.compress)

/-! ## The store -/

/-- Put one item in the inbox. Answers its id, or `none` when the same `(source, eventId)` is
    already there — which is not an error: it is the deduplication doing its job. -/
def insert (resident source eventId : String) (trusted : Bool) (payload : Json) :
    IO (Option Nat) := do
  let now ← TaskStore.currentIso8601
  let row : Store.ResidentInboxRow := {
    id := 0, resident, source, event_id := eventId, trusted, received_at := now
    payload := payload.compress, sent_in := none, sent_seq := none, confirmed := false }
  Store.transaction do
    let inserted ← HasModel.insertIfAbsent row
    if !inserted then return none
    -- Read back by its key, the only way to learn the id the database assigned.
    let rows ← HasModel.fetch <| query% do
      let r ← from Store.ResidentInboxRow
      guard r.resident = resident
      guard r.source = source
      guard r.event_id = eventId
      select r
    return rows[0]?.map (·.id.toNat)

open Db.Query.DSL in
/-- Every item not yet confirmed, oldest first: what the next turn carries. -/
def ready (resident : String) : IO (Array InboxItem) := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let r ← from Store.ResidentInboxRow
    guard r.resident = resident
    guard r.confirmed = false
    select r
    order_by r.id
  return rows.map ofRow

open Db.Query.DSL in
/-- The most recent `atMost` items, newest first, and how many there are in all. -/
def recent (resident : String) (atMost : Nat := 50) : IO (Array InboxItem × Nat) := do
  let (rows, total) ← Store.run do
    let matching := query% do
      let r ← from Store.ResidentInboxRow
      guard r.resident = resident
      select r
      order_by_desc r.id
    let rows ← HasModel.fetch (matching.limit atMost)
    let total ← HasModel.count matching
    pure (rows, total)
  return (rows.map ofRow, total.toNat)

open Db.Query.DSL in
/-- One item, by id. -/
def find? (id : Nat) : IO (Option InboxItem) := do
  let key : Int := Int.ofNat id
  let rows ← Store.run <| HasModel.fetch <| query% do
    let r ← from Store.ResidentInboxRow
    guard r.id = key
    select r
  return rows[0]?.map ofRow

/-- Mark `ids` as written into turn `turn`, whose text is at transcript seq `seq`. -/
def markSent (ids : List Nat) (turn seq : Nat) : IO Unit := do
  if ids.isEmpty then return
  let _ ← Store.run <| HasModel.update (α := Store.ResidentInboxRow)
    { value
        | .sent_in  => some (.int (Int.ofNat turn))
        | .sent_seq => some (.int (Int.ofNat seq))
        | _         => none
      condition := .inList (.var Store.ResidentInboxRowIndex.id .int) (ids.map Int.ofNat) }

/-- The turn `turn` of `resident` produced a result: everything it carried has been handed over. -/
def confirm (resident : String) (turn : Nat) : IO Nat := do
  Store.run <| HasModel.update (α := Store.ResidentInboxRow)
    { value
        | .confirmed => some .true
        | _          => none
      condition := .and (.eq (.var Store.ResidentInboxRowIndex.resident .text) (.text resident))
        (.and (.eq (.var Store.ResidentInboxRowIndex.sent_in .int) (.int (Int.ofNat turn)))
              (.eq (.var Store.ResidentInboxRowIndex.confirmed .bool) .false)) }

/-- Forget what a resident's inbox held: for `orchestra resident reset`, whose fresh conversation
    should not open with the last one's leftovers. Answers how many items went. -/
def clear (resident : String) : IO Nat :=
  Store.run <| HasModel.delete (α := Store.ResidentInboxRow)
    (.eq (.var Store.ResidentInboxRowIndex.resident .text) (.text resident))

/-! ## Spend -/

open Db.Query.DSL in
/-- What `resident` has spent on budget day `day`. -/
def spentOn (resident day : String) : IO Float := do
  let rows ← Store.run <| HasModel.fetch <| query% do
    let r ← from Store.ResidentSpendRow
    guard r.resident = resident
    guard r.day = day
    select r
  return rows[0]?.map (·.usd) |>.getD 0.0

/-- Add `usd` to what `resident` has spent on `day`. Read and write in one transaction; only the
    daemon writes it, one result at a time per resident. -/
def addSpend (resident day : String) (usd : Float) : IO Unit := do
  if !(usd > 0.0) then return
  Store.transaction do
    let rows ← HasModel.fetch <| query% do
      let r ← from Store.ResidentSpendRow
      guard r.resident = resident
      guard r.day = day
      select r
    let before := rows[0]?.map (·.usd) |>.getD 0.0
    HasModel.save ({ resident, day, usd := Store.floatColumn (before + usd) } :
      Store.ResidentSpendRow)

/-! ## The budget day -/

/-- The budget day an instant falls on, `YYYY-MM-DD`, for a zone `offsetSeconds` east of UTC. -/
def dayAt (epoch : Int) (offsetSeconds : Int := 0) : String :=
  ((Usage.secsToIso8601 (epoch + offsetSeconds)).take 10).toString

/-- `±hhmm`, as `date +%z` writes it, in seconds. -/
def parseUtcOffset (s : String) : Option Int := do
  let s := s.trimAscii.toString
  guard (s.length == 5)
  let sign ← match s.front with
    | '+' => some (1 : Int)
    | '-' => some (-1)
    | _   => none
  let h ← (s.drop 1 |>.take 2).toString.toNat?
  let m ← (s.drop 3).toString.toNat?
  return sign * (Int.ofNat (h * 3600 + m * 60))

initialize offsetCache : IO.Ref (Option (String × Int × Int)) ← IO.mkRef none

/-- The zone's offset from UTC now, in seconds. Asked of `date` with `TZ` set — the one tool that
    knows every zone the host has data for — and cached for ten minutes, since it changes twice a
    year. A zone the host does not know reads as UTC, which is what `date` does with one too. -/
def utcOffset (tz : String) : IO Int := do
  if tz.isEmpty || tz == "UTC" || tz == "Etc/UTC" then return 0
  let now ← Usage.nowEpoch
  if let some (z, at_, off) ← offsetCache.get then
    if z == tz && now < at_ + 600 then return off
  let off ← try
      let out ← IO.Process.output { cmd := "date", args := #["+%z"], env := #[("TZ", some tz)] }
      pure ((parseUtcOffset out.stdout).getD 0)
    catch _ => pure 0
  offsetCache.set (some (tz, now, off))
  return off

/-- Today's budget day in `tz`. -/
def today (tz : String) : IO String := do
  return dayAt (← Usage.nowEpoch) (← utcOffset tz)

/-! ## Delivery decisions -/

/-- When a batch of ready items is due: `window` seconds after the oldest arrived, or at once
    when a person is waiting on one of them. `none` for nothing ready. -/
def batchDueAt (items : Array InboxItem) (window : Nat) : Option Int := do
  let oldest ← items[0]?
  if items.any (·.source == "human") then
    return (Usage.parseIso8601 oldest.receivedAt).getD 0
  let at_ := (Usage.parseIso8601 oldest.receivedAt).getD 0
  return at_ + Int.ofNat window

/-- How long to wait after `failures` consecutive failures to wake or keep a resident up: 30s,
    doubling, at most five minutes. -/
def backoffSeconds (failures : Nat) : Nat :=
  if failures == 0 then 0 else min 300 (30 * 2 ^ (failures - 1))

/-- Failures in a row after which a resident is paused until an operator resumes it. -/
def maxFailures : Nat := 3

/-- How many of `stamps` (epoch seconds) fall within the hour before `now`. -/
def turnsInLastHour (stamps : Array Int) (now : Int) : Nat :=
  (stamps.filter (· > now - 3600)).size

/-- The note heading a turn that carries items an interrupted turn already carried. -/
def redeliveryNote : String :=
  "These may have reached you already; your previous turn was interrupted."

/-- How one item reads in a turn. -/
def renderItem (i : InboxItem) : String :=
  let trust := if i.trusted then "" else " [not from a trusted source]"
  if i.source == "human" then
    s!"Message from the chat (item {i.id}){trust}:\n{i.text}"
  else if i.source.startsWith "listener:" then
    let name := (i.source.drop "listener:".length).toString
    s!"Event from listener {name} (event {i.eventId}, item {i.id}){trust}:\n{i.text}"
  else
    s!"From {i.source} (event {i.eventId}, item {i.id}){trust}:\n{i.text}"

/-- Whether an item came from a Zulip feed. -/
def InboxItem.isZulip (i : InboxItem) : Bool :=
  i.source.startsWith "zulip:" &&
    (i.payload.getObjValAs? String "kind").toOption == some "zulip"

/-- The site a Zulip item came from. -/
def InboxItem.site (i : InboxItem) : String :=
  (i.payload.getObjValAs? String "site").toOption.getD (i.source.drop "zulip:".length).toString

/-- A run of Zulip items from one site, as the turn shows them (§4.3): a heading naming the
    server, one entry per message with the untrusted ones marked, and how to answer. -/
def renderZulipGroup (site : String) (items : List InboxItem) : String :=
  let entries := items.map fun i => renderZulipItem i.payload i.trusted i.id
  s!"New on Zulip ({site}):\n" ++ "\n".intercalate entries ++
    "\nReply in the same place with the zulip client (your token card says how)."

/-- Items in order, with each source's Zulip items gathered into one group where the first of
    them stood. -/
def groupItems (items : List InboxItem) : List (Sum InboxItem (String × List InboxItem)) :=
  Id.run do
    let mut out : Array (Sum InboxItem (String × List InboxItem)) := #[]
    let mut seen : List String := []
    for i in items do
      if i.isZulip then
        if seen.contains i.source then continue
        seen := i.source :: seen
        out := out.push (.inr (i.site, items.filter (fun j => j.isZulip && j.source == i.source)))
      else out := out.push (.inl i)
    return out.toList

/-- The one turn a batch of items becomes.

    A single human message with nothing to say about it is handed over as written, so a chat
    reads as a chat; anything else is a list, each item saying where it came from, whether it is
    trusted, and its id — the transcript then shows what woke the resident. A feed's items are
    gathered under one heading per server. `notes` come first: a lost conversation, an
    interrupted turn. -/
def renderTurn (items : Array InboxItem) (notes : List String := []) : String :=
  let interrupted := items.any (·.sentIn.isSome)
  let notes := notes ++ (if interrupted then [redeliveryNote] else [])
  match items.toList, notes with
  | [i], [] =>
    if i.source == "human" && i.trusted then i.text
    else if i.isZulip then renderZulipGroup i.site [i]
    else renderItem i
  | _, _ =>
    let body := "\n\n".intercalate ((groupItems items.toList).map fun
      | .inl i => renderItem i
      | .inr (site, is) => renderZulipGroup site is)
    if notes.isEmpty then body else "\n".intercalate notes ++ "\n\n" ++ body

end Orchestra.Resident
