import Orchestra.Store
import Orchestra.Usage
import Orchestra.TaskStore
import Orchestra.Resident.Feed
import Lean.Data.Json

open Lean (Json ToJson FromJson)

/-!
# Watches: topics a resident asked to be told about

`docs/design/resident-sessions.md` §6. A resident sets a watch with its `watch` MCP tool — for
Zulip, a stream and optionally a topic — and from then on the feed hands it every message there
from a sender `watch_senders` admits, and catches up on them after a reconnect. A watch may lapse
on its own (`until`); a lapsed watch is ignored, and removed the next time the resident's watches
are listed.

Who may ask for a watch is the resident's to judge — its `AGENTS.md` says to take watch requests
only from trusted senders. The tool cannot know who asked.

The pure halves — reading the tool's arguments, `until`, expiry — are separate so they can be
tested without a database. The visibility check (`watch` refuses a private stream the bot cannot
see) needs the feed's token, and lives with the feed (`Orchestra.Resident.Zulip`).
-/

namespace Orchestra.Resident

/-- One watch, as the store reads it. -/
structure Watch where
  id : Nat
  resident : String
  feed : String
  key : WatchKey
  note : Option String := none
  createdAt : String
  /-- When it lapses, ISO 8601; `none` for never. -/
  expiresAt : Option String := none
deriving Repr, Inhabited

/-- The key as the store keeps it: compact JSON, the stream and — when there is one — the topic. -/
def WatchKey.toJsonString (k : WatchKey) : String :=
  (Json.mkObj ([("stream", .str k.stream)] ++
    (k.topic.map fun t => [("topic", Json.str t)]).getD [])).compress

/-- Read a key from the tool's `key` argument (or a stored one). -/
def parseWatchKey (j : Json) : Except String WatchKey := do
  let .obj _ := j | throw "'key' is an object: {\"stream\": …, \"topic\": …}"
  let stream ← j.getObjValAs? String "stream" |>.mapError (fun _ => "'key' names a 'stream'")
  let stream := stream.trimAscii.toString
  let stream := if stream.startsWith "#" then (stream.drop 1).toString else stream
  if stream.isEmpty then throw "'key.stream' is empty"
  let topic ← match j.getObjVal? "topic" with
    | .error _ | .ok .null => pure none
    | .ok (.str t) => pure (if t.trimAscii.isEmpty then none else some t.trimAscii.toString)
    | .ok _ => throw "'key.topic' is a string"
  return { stream, topic }

instance : ToJson Watch where
  toJson w := Json.mkObj [
    ("id", ToJson.toJson w.id), ("feed", .str w.feed), ("stream", .str w.key.stream),
    ("topic", match w.key.topic with | some t => .str t | none => .null),
    ("note", match w.note with | some n => .str n | none => .null),
    ("createdAt", .str w.createdAt),
    ("until", match w.expiresAt with | some u => .str u | none => .null)]

/-- `until` as the tool takes it: an instant (`2026-11-01T00:00:00Z`, or a date, read as its
    midnight UTC), or a span from `now` — `90m`, `12h`, `3d`, `2w`. Answers the instant. -/
def parseUntil (s : String) (now : Int) : Except String String := do
  let s := s.trimAscii.toString
  if s.isEmpty then throw "'until' is empty"
  let unit := s.back
  let num := (s.dropEnd 1).toString
  let mult : Option Nat := match unit with
    | 'm' => some 60 | 'h' => some 3600 | 'd' => some 86400 | 'w' => some 604800
    | _ => none
  match mult, num.toNat? with
  | some m, some n =>
    if n == 0 then throw "'until' is a span of at least one unit"
    return Usage.secsToIso8601 (now + Int.ofNat (n * m))
  | _, _ =>
    let iso := if s.length == 10 then s ++ "T00:00:00Z" else s
    match Usage.parseIso8601 iso with
    | some t =>
      if t ≤ now then throw s!"'until' ({s}) is already past"
      return Usage.secsToIso8601 t
    | none => throw s!"'until' is an instant (2026-11-01T00:00:00Z) or a span (12h, 3d), not '{s}'"

/-- Whether a watch has lapsed at `now`. -/
def Watch.expired (w : Watch) (now : Int) : Bool :=
  match w.expiresAt.bind Usage.parseIso8601 with
  | some t => t ≤ now
  | none => false

/-- What a `watch` call asks for. -/
structure WatchRequest where
  feed : String
  key : WatchKey
  untilArg : Option String := none
  note : Option String := none
deriving Repr

/-- The feed a call names, or the only one when it names none. -/
def pickFeed (args : Json) (feeds : List String) : Except String String := do
  match args.getObjValAs? String "feed" with
  | .ok f =>
    if feeds.contains f then return f
    throw s!"there is no feed '{f}' (this resident's feeds: {if feeds.isEmpty then "none" else ", ".intercalate feeds})"
  | .error _ =>
    match feeds with
    | [f] => return f
    | [] => throw "this resident has no feed to watch anything on"
    | _ => throw s!"name the feed: one of {", ".intercalate feeds}"

/-- Read a `watch` call's arguments. -/
def parseWatchArgs (args : Json) (feeds : List String) : Except String WatchRequest := do
  let feed ← pickFeed args feeds
  let key ← parseWatchKey ((args.getObjVal? "key").toOption.getD .null)
  let str (k : String) : Option String :=
    (args.getObjValAs? String k).toOption.filter (!·.trimAscii.isEmpty)
  return { feed, key, untilArg := str "until", note := str "note" }

/-! ## The store -/

private def ofRow (r : Store.ResidentWatchRow) : Option Watch := do
  let key ← (Json.parse r.key >>= parseWatchKey).toOption
  return { id := r.id.toNat, resident := r.resident, feed := r.feed, key, note := r.note,
           createdAt := r.created_at, expiresAt := r.expires_at }

open Db.Query.DSL in
private def rowsOf (resident : String) : IO (Array Store.ResidentWatchRow) :=
  Store.run <| HasModel.fetch <| query% do
    let w ← from Store.ResidentWatchRow
    guard w.resident = resident
    select w
    order_by w.id

/-- Add a watch, or — for a key already watched — replace its note and expiry. Answers whether it
    is new. -/
def addWatch (resident feed : String) (key : WatchKey) (note expiresAt : Option String) :
    IO Bool := do
  let now ← TaskStore.currentIso8601
  let keyStr := key.toJsonString
  let row : Store.ResidentWatchRow := {
    id := 0, resident, feed, key := keyStr, note, created_at := now, expires_at := expiresAt }
  Store.transaction do
    if ← HasModel.insertIfAbsent row then return true
    let _ ← HasModel.update (α := Store.ResidentWatchRow)
      { value
          | .note       => some (match note with | some n => .text n | none => .null .text)
          | .expires_at => some (match expiresAt with | some e => .text e | none => .null .text)
          | _           => none
        condition := .and (.eq (.var Store.ResidentWatchRowIndex.resident .text) (.text resident))
          (.and (.eq (.var Store.ResidentWatchRowIndex.feed .text) (.text feed))
                (.eq (.var Store.ResidentWatchRowIndex.key .text) (.text keyStr))) }
    return false

/-- Remove a watch. Answers whether there was one. -/
def removeWatch (resident feed : String) (key : WatchKey) : IO Bool := do
  let n ← Store.run <| HasModel.delete (α := Store.ResidentWatchRow)
    (.and (.eq (.var Store.ResidentWatchRowIndex.resident .text) (.text resident))
      (.and (.eq (.var Store.ResidentWatchRowIndex.feed .text) (.text feed))
            (.eq (.var Store.ResidentWatchRowIndex.key .text) (.text key.toJsonString))))
  return n > 0

/-- A resident's watches that have not lapsed, on one feed or all of them. Lapsed ones are removed
    on the way. -/
def listWatches (resident : String) (feed : Option String := none) : IO (Array Watch) := do
  let now ← Usage.nowEpoch
  let all := (← rowsOf resident).filterMap ofRow
  let lapsed := all.filter (·.expired now)
  unless lapsed.isEmpty do
    let _ ← Store.run <| HasModel.delete (α := Store.ResidentWatchRow)
      (.inList (.var Store.ResidentWatchRowIndex.id .int) (lapsed.toList.map (Int.ofNat ·.id)))
  return all.filter fun w => !w.expired now && (feed.map (· == w.feed)).getD true

/-- The keys a feed is watching now: what `classify` and the catch-up read. -/
def activeWatchKeys (resident feed : String) : IO (List WatchKey) := do
  let now ← Usage.nowEpoch
  let ws := ((← rowsOf resident).filterMap ofRow).toList.filter fun (w : Watch) =>
    w.feed == feed && !w.expired now
  return ws.map (·.key)

/-- How the watch list reads to the agent. -/
def renderWatches (ws : Array Watch) : String :=
  if ws.isEmpty then "Nothing is being watched."
  else "\n".intercalate (ws.toList.map fun w =>
    let where_ := s!"#{w.key.stream}" ++ (w.key.topic.map (s!" > {·}")).getD " (every topic)"
    let lapses := (w.expiresAt.map (s!", until {·}")).getD ""
    let note := (w.note.map (s!" — {·}")).getD ""
    s!"- {w.feed}: {where_}{lapses}{note}")

end Orchestra.Resident
