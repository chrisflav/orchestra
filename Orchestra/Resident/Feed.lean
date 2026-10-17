import Lean.Data.Json

open Lean (Json ToJson FromJson)

/-!
# A resident's feeds: what they are, and which messages become inbox items

A **feed** is a long-lived event source the daemon holds for a resident
(`docs/design/resident-sessions.md` §5): the first, and so far the only kind, is a Zulip event
queue. This module is the pure half — the configuration a `resident` block (or a listener file)
writes, the shape of a Zulip message, and the decisions made about one:

* which messages become inbox items (`classify`): not the bot's own; a direct message or a mention
  from a sender in `allow_senders`; a message in a watched topic from a sender `watch_senders`
  admits — and whether the item is **trusted** (its sender is in `allow_senders`);
* which narrows catch a reconnecting feed up (`catchUpNarrows`), one per term, because Zulip
  combines a narrow's terms with AND;
* what an item carries (`payloadOf`) and how a turn reads it (`renderZulipItem`).

The protocol itself — kleis, the event queue, the long poll — is `Orchestra.Resident.Zulip`.
-/

namespace Orchestra.Resident

/-- Who may reach a resident through a topic it watches. -/
inductive WatchSenders where
  /-- Anyone who can post there. -/
  | any
  /-- Only these senders (patterns as in `allow_senders`). -/
  | only (patterns : List String)
deriving Repr, DecidableEq, Inhabited

/-- One Zulip feed: a Zulip server the resident's kleis spec reaches, and who may wake it there. -/
structure ZulipFeed where
  /-- The feed's name, unique among the resident's feeds: what `watch(feed, …)` names and what
      its listener is called after. Defaults to `zulip`. -/
  name : String := "zulip"
  /-- The server, as a host: `zulip.merten.dev`. -/
  site : String
  /-- What wakes the resident on its own: `dm`, `mention`, `wildcard` (`@**all**` and the like). -/
  notify : List String := ["dm", "mention"]
  /-- Who may wake it with a direct message or a mention, and whose items are trusted. An entry is
      an email pattern (`*` matches anything: `*@merten.dev`), or a Zulip user id (`id:8`, or
      just `8`) for a server that hides its users' real addresses. -/
  allowSenders : List String := []
  /-- Who may reach it through a topic it watches. -/
  watchSenders : WatchSenders := .any
deriving Repr, Inhabited

instance : ToJson ZulipFeed where
  toJson f := Json.mkObj [
    ("kind", "zulip"), ("name", .str f.name), ("site", .str f.site),
    ("notify", ToJson.toJson f.notify), ("allow_senders", ToJson.toJson f.allowSenders),
    ("watch_senders", match f.watchSenders with
      | .any => .str "any"
      | .only ps => ToJson.toJson ps)]

/-- Whether a feed name can name a listener and a kleis task id: letters, digits, `-` and `_`. -/
def feedNameValid (n : String) : Bool :=
  !n.isEmpty && n.length ≤ 64 && n.all (fun c => c.isAlphanum || c == '-' || c == '_')

/-- Whether `s` is a bare host (with an optional port): no scheme, path or whitespace. -/
def siteValid (s : String) : Bool :=
  !s.isEmpty && s.all (fun c => c.isAlphanum || c == '.' || c == '-' || c == ':')

/-- Read one feed. Strict: a feed that half-parsed would let the wrong people wake the resident. -/
def parseZulipFeed (who : String) (j : Json) : Except String ZulipFeed := do
  let .obj kvs := j | throw s!"{who}: a feed must be an object"
  let known := ["kind", "name", "site", "notify", "allow_senders", "watch_senders"]
  for (k, _) in kvs.toArray do
    unless known.contains k do
      throw s!"{who}: unknown field '{k}' in a feed (known: {", ".intercalate known})"
  let kind ← j.getObjValAs? String "kind" |>.mapError (fun _ => s!"{who}: a feed names its 'kind'")
  unless kind == "zulip" do throw s!"{who}: feeds of kind '{kind}' are not supported (only 'zulip')"
  let name := (j.getObjValAs? String "name").toOption.getD "zulip"
  unless feedNameValid name do
    throw s!"{who}: feed name '{name}' must be letters, digits, '-' and '_'"
  let site ← j.getObjValAs? String "site" |>.mapError (fun _ => s!"{who}: a zulip feed names its 'site'")
  let site := if site.startsWith "https://" then (site.drop 8).toString else site
  let site := if site.endsWith "/" then (site.dropEnd 1).toString else site
  unless siteValid site do throw s!"{who}: feed site '{site}' must be a host, like zulip.example.org"
  let strs (k : String) (dflt : List String) : Except String (List String) :=
    match j.getObjVal? k with
    | .error _ => pure dflt
    | .ok v => (FromJson.fromJson? v : Except String (List String))
        |>.mapError (fun _ => s!"{who}: feed.{k} must be a list of strings")
  let notify ← strs "notify" ["dm", "mention"]
  for n in notify do
    unless ["dm", "mention", "wildcard"].contains n do
      throw s!"{who}: feed.notify knows 'dm', 'mention' and 'wildcard', not '{n}'"
  let allowSenders ← strs "allow_senders" []
  let watchSenders ← match j.getObjVal? "watch_senders" with
    | .error _ => pure WatchSenders.any
    | .ok (.str "any") => pure .any
    | .ok v => match (FromJson.fromJson? v : Except String (List String)) with
      | .ok ps => pure (.only ps)
      | .error _ => throw s!"{who}: feed.watch_senders is \"any\" or a list of senders"
  return { name, site, notify, allowSenders, watchSenders }

/-- Read a resident's `feeds`, refusing two with one name. -/
def parseFeeds (who : String) (fs : Array Json) : Except String (Array ZulipFeed) := do
  let mut out : Array ZulipFeed := #[]
  for f in fs do
    let feed ← parseZulipFeed who f
    if out.any (·.name == feed.name) then
      throw s!"{who}: two feeds are named '{feed.name}'; give one a 'name' of its own"
    out := out.push feed
  return out

/-! ## Senders -/

/-- `*`-glob matching, case-insensitive: `*` matches any run of characters. -/
partial def globMatch (pattern s : String) : Bool :=
  go pattern.toLower.toList s.toLower.toList
where
  go : List Char → List Char → Bool
    | [], [] => true
    | '*' :: ps, cs => go ps cs || (match cs with | [] => false | _ :: cs' => go ('*' :: ps) cs')
    | p :: ps, c :: cs => p == c && go ps cs
    | _, _ => false

/-- A message's sender, as far as matching one goes. -/
structure Sender where
  id : Nat
  /-- The address Zulip shows: on a server that hides addresses, `user8@zulip.example.org`. -/
  email : String
  /-- The real address, where the server lets the bot see it (`delivery_email`). -/
  deliveryEmail : Option String := none
  name : String := ""
deriving Repr, Inhabited

/-- Whether one `allow_senders` entry admits `s`: a user id (`id:8` or `8`), else an email pattern
    against both of the sender's addresses. -/
def senderMatches (pattern : String) (s : Sender) : Bool :=
  let p := pattern.trimAscii.toString
  let idPart := if p.startsWith "id:" then (p.drop 3).toString else p
  match idPart.toNat? with
  | some n => n == s.id
  | none => globMatch p s.email || (s.deliveryEmail.map (globMatch p)).getD false

def senderAllowed (patterns : List String) (s : Sender) : Bool :=
  patterns.any (senderMatches · s)

/-! ## Messages -/

/-- A Zulip message as the feed reads one (`GET /messages`, or a `message` event). -/
structure ZulipMessage where
  id : Nat
  /-- `stream` or `private`. -/
  type : String
  sender : Sender
  /-- The stream's name, for a stream message. -/
  stream : Option String := none
  streamId : Option Nat := none
  topic : String := ""
  /-- Raw markdown (`apply_markdown=false`). -/
  content : String := ""
  /-- Seconds since the epoch. -/
  timestamp : Nat := 0
  /-- Everyone in a direct message, the bot included: `(id, email, name)`. -/
  recipients : List (Nat × String × String) := []
  flags : List String := []
deriving Repr, Inhabited

/-- Read a message object. `flags` come with the message in `GET /messages`, and beside it in an
    event; `flags` here wins when given. -/
def parseMessage (j : Json) (flags : Option (List String) := none) : Except String ZulipMessage := do
  let id ← j.getObjValAs? Nat "id"
  let type := (j.getObjValAs? String "type").toOption.getD "stream"
  let senderId := (j.getObjValAs? Nat "sender_id").toOption.getD 0
  let sender : Sender := {
    id := senderId
    email := (j.getObjValAs? String "sender_email").toOption.getD ""
    name := (j.getObjValAs? String "sender_full_name").toOption.getD "" }
  let (stream, recipients) := match j.getObjVal? "display_recipient" with
    | .ok (.str s) => (some s, [])
    | .ok (.arr rs) => (none, rs.toList.map fun r =>
        ((r.getObjValAs? Nat "id").toOption.getD 0, (r.getObjValAs? String "email").toOption.getD "",
         (r.getObjValAs? String "full_name").toOption.getD ""))
    | _ => (none, [])
  let flags := flags.getD ((j.getObjValAs? (List String) "flags").toOption.getD [])
  return { id, type, sender, stream, recipients, flags
           streamId := (j.getObjValAs? Nat "stream_id").toOption
           topic := (j.getObjValAs? String "subject").toOption.getD ""
           content := (j.getObjValAs? String "content").toOption.getD ""
           timestamp := (j.getObjValAs? Nat "timestamp").toOption.getD 0 }

/-- The bot the feed reads as. -/
structure Bot where
  id : Nat
  fullName : String
deriving Repr, Inhabited

/-- A watched topic, as far as matching a message goes: the stream, and the topic or every topic. -/
structure WatchKey where
  stream : String
  topic : Option String := none
deriving Repr, DecidableEq, Inhabited

def WatchKey.matches (k : WatchKey) (stream topic : String) : Bool :=
  k.stream.toLower == stream.toLower &&
    (k.topic.map (·.toLower == topic.toLower)).getD true

/-- Why a message became an item. -/
inductive Reason where
  | dm
  | mention
  | watch
deriving Repr, DecidableEq, Inhabited

def Reason.toString : Reason → String
  | .dm => "dm"
  | .mention => "mention"
  | .watch => "watch"

/-- Whether `content` mentions the bot by name: `@**Full Name**` or `@**Full Name|21**`. A silent
    mention (`@_**…**`) is not one: it is written not to notify. -/
def mentionsByName (bot : Bot) (content : String) : Bool :=
  let plain := s!"@**{bot.fullName}**"
  let withId := s!"@**{bot.fullName}|{bot.id}**"
  !bot.fullName.isEmpty && ((content.splitOn plain).length > 1 || (content.splitOn withId).length > 1)

/-- Whether a message is one Zulip counts as a wildcard mention of the bot. -/
def wildcardFlagged (flags : List String) : Bool :=
  flags.any (fun f => f == "wildcard_mentioned" || f == "stream_wildcard_mentioned" ||
    f == "topic_wildcard_mentioned")

/-- What a message becomes, if anything: why it reaches the resident, and whether it is trusted.
    `none` for the bot's own messages and everything no rule admits. -/
def classify (feed : ZulipFeed) (bot : Bot) (watches : List WatchKey) (m : ZulipMessage) :
    Option (Reason × Bool) :=
  if m.sender.id == bot.id then none
  else
    let allowed := senderAllowed feed.allowSenders m.sender
    let isDm := m.type == "private"
    -- `mention` and `wildcard` are two switches: a feed may wake on `@**all**` and not on its
    -- name, or the other way round.
    let byName := m.flags.contains "mentioned" || (!isDm && mentionsByName bot m.content)
    let mentioned := (feed.notify.contains "mention" && byName) ||
      (feed.notify.contains "wildcard" && wildcardFlagged m.flags)
    if isDm && feed.notify.contains "dm" && allowed then some (.dm, allowed)
    else if !isDm && mentioned && allowed then
      some (.mention, allowed)
    else if !isDm && watches.any (·.matches (m.stream.getD "") m.topic) &&
        (match feed.watchSenders with
         | .any => true
         | .only ps => senderAllowed ps m.sender) then
      some (.watch, allowed)
    else none

/-! ## Catching up -/

/-- One narrow term, as `GET /messages` takes it. -/
def term (op operand : String) : Json :=
  Json.mkObj [("operator", .str op), ("operand", .str operand)]

/-- The narrows a reconnecting feed reads, one per term, each with a label for its log line.
    Zulip combines a narrow's terms with AND, so "direct messages or mentions or this topic" is
    several requests. A mention in a public stream the bot is not subscribed to carries no
    `mentioned` flag, so public streams are also searched for its name, and `classify` decides
    each candidate. `channelOperator` is `channel` on servers that know it (feature level 250),
    else `stream`. -/
def catchUpNarrows (feed : ZulipFeed) (bot : Bot) (watches : List WatchKey)
    (channelOperator : String := "channel") : List (String × Json) :=
  let channels := if channelOperator == "channel" then "channels" else "streams"
  (if feed.notify.contains "dm" then [("is:dm", Json.arr #[term "is" "dm"])] else [])
  ++ (if feed.notify.contains "mention" || feed.notify.contains "wildcard" then
        [("is:mentioned", Json.arr #[term "is" "mentioned"])]
        ++ (if bot.fullName.isEmpty then []
            else [(s!"{channels}:public search:{bot.fullName}",
                   Json.arr #[term channels "public", term "search" bot.fullName])])
      else [])
  ++ (watches.eraseDups.map fun w =>
      let label := s!"{channelOperator}:{w.stream}" ++ (w.topic.map (s!" topic:{·}")).getD ""
      (label, Json.arr (#[term channelOperator w.stream] ++
        (w.topic.map (fun t => #[term "topic" t])).getD #[])))

/-- The checkpoint after a batch: the highest message id seen, never lower than before. -/
def nextCheckpoint (before : Option Nat) (ids : List Nat) : Option Nat :=
  ids.foldl (fun acc i => some (max i (acc.getD 0))) before

/-! ## Items -/

/-- An instant, as `YYYY-MM-DDTHH:MM:SSZ`. -/
def isoOfEpoch (secs : Nat) : String := Id.run do
  let days := secs / 86400
  let rem := secs % 86400
  -- Civil from days (Howard Hinnant's algorithm).
  let z : Int := Int.ofNat days + 719468
  let era := (if z ≥ 0 then z else z - 146096) / 146097
  let doe := z - era * 146097
  let yoe := (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
  let y := yoe + era * 400
  let doy := doe - (365 * yoe + yoe / 4 - yoe / 100)
  let mp := (5 * doy + 2) / 153
  let d := doy - (153 * mp + 2) / 5 + 1
  let m := if mp < 10 then mp + 3 else mp - 9
  let y := if m ≤ 2 then y + 1 else y
  let two (n : Nat) := if n < 10 then s!"0{n}" else toString n
  return s!"{y}-{two m.toNat}-{two d.toNat}T{two (rem / 3600)}:{two (rem % 3600 / 60)}:{two (rem % 60)}Z"

/-- `%`-encoding as Zulip's narrow links write it: `.` for `%`, and a few characters kept. -/
def narrowEncode (s : String) : String :=
  s.toUTF8.foldl (init := "") fun acc b =>
    let c := Char.ofNat b.toNat
    if c.isAlphanum || c == '-' || c == '_' || c == '~' then acc.push c
    else
      let hex := "0123456789ABCDEF".toList
      acc.push '.' |>.push (hex.getD (b.toNat / 16) '0') |>.push (hex.getD (b.toNat % 16) '0')

/-- A link to the message in the web app. -/
def messageLink (site : String) (m : ZulipMessage) : String :=
  match m.stream, m.streamId with
  | some s, some sid =>
    s!"https://{site}/#narrow/channel/{sid}-{narrowEncode s}/topic/{narrowEncode m.topic}/near/{m.id}"
  | _, _ =>
    let ids := m.recipients.map (·.1) |>.filter (· != 0)
    if ids.isEmpty then s!"https://{site}/#narrow/near/{m.id}"
    else s!"https://{site}/#narrow/dm/{",".intercalate (ids.map toString)}/near/{m.id}"

/-- The longest message an item carries; the rest is a link away. -/
def maxContentChars : Nat := 6000

/-- What an inbox item from a Zulip message carries: everything the turn says, and a `text`
    for anything that reads items without knowing Zulip. -/
def payloadOf (site : String) (bot : Bot) (reason : Reason) (m : ZulipMessage) : Json :=
  let content := if m.content.length ≤ maxContentChars then m.content
    else (m.content.take maxContentChars).toString ++ "\n[…cut; the rest is at the link]"
  let sender := Json.mkObj [("id", ToJson.toJson m.sender.id), ("email", .str m.sender.email),
    ("name", .str m.sender.name)]
  let place : List (String × Json) := match m.stream with
    | some s => [("stream", .str s), ("topic", .str m.topic)]
    | none => [("participants", .arr ((m.recipients.filter (·.1 != bot.id)).map fun (i, e, n) =>
        Json.mkObj [("id", ToJson.toJson i), ("email", .str e), ("name", .str n)]).toArray)]
  Json.mkObj (([("kind", Json.str "zulip"), ("site", Json.str site), ("reason", .str reason.toString),
    ("message_id", ToJson.toJson m.id), ("sender", sender),
    ("time", .str (isoOfEpoch m.timestamp)), ("content", .str content),
    ("link", .str (messageLink site m)), ("text", .str content)] : List (String × Json)) ++ place)

/-- The `inbox` source of a Zulip feed's items: `zulip:<site>#<feed>`. The feed's name is part of
    it because the inbox deduplicates by `(source, event_id)`: two feeds of one resident on one
    site — with different senders allowed, say — would otherwise have one's item for a message
    swallow the other's. -/
def zulipSource (site feed : String) : String := s!"zulip:{site}#{feed}"

/-- One line of a turn's heading, whatever was put in it: line breaks — which would let a field
    from outside (a topic, a sender's name, an event id) start a line of its own and pass for a
    heading — become spaces. -/
def oneLine (s : String) : String :=
  s.map fun c => if c == '\n' || c == '\r' || c == ' ' || c == ' ' then ' ' else c

/-- `text` quoted, every line of it: what an item carries is shown as `  > ` lines, and nothing it
    says can start a line that is not, so no content can pass for a heading — an item's source, its
    trust, a note — however it is written. `\r` and the Unicode separators count as line breaks,
    since a reader may take them for one. -/
def quoteLines (text : String) : String :=
  let unified := (text.replace "\r\n" "\n").map fun c =>
    if c == '\r' || c == ' ' || c == ' ' then '\n' else c
  "\n".intercalate ((unified.splitOn "\n").map ("  > " ++ ·))

/-- How one Zulip item reads in a turn (§4.3): where, who, which message, when, and the message
    quoted. `itemId` names the inbox item, so the transcript shows what woke the resident. -/
def renderZulipItem (payload : Json) (trusted : Bool) (itemId : Nat) : String :=
  -- Every field of the heading on one line: a topic or a name is written by whoever sent it.
  let s (k : String) := oneLine ((payload.getObjValAs? String k).toOption.getD "")
  let sender := payload.getObjVal? "sender" |>.toOption |>.getD Json.null
  let sname := oneLine ((sender.getObjValAs? String "name").toOption.getD "someone")
  let semail := oneLine ((sender.getObjValAs? String "email").toOption.getD "")
  let who := if semail.isEmpty then sname else s!"{sname} <{semail}>"
  let mid := match payload.getObjValAs? Nat "message_id" with
    | .ok n => toString n
    | .error _ => "?"
  let time := s "time"
  let mark := if trusted then "" else "[not from an allowed sender] "
  let head := match s "reason" with
    | "dm" =>
      let others := match payload.getObjVal? "participants" with
        | .ok (.arr ps) => ps.toList.filterMap fun (p : Json) =>
            let e := (p.getObjValAs? String "email").toOption.getD ""
            if e == semail then none else (p.getObjValAs? String "name").toOption.map oneLine
        | _ => []
      let group := if others.isEmpty then ""
        else s!" (a group DM, with {", ".intercalate others})"
      s!"DM from {who}{group}, message {mid}, {time}"
    | r =>
      let why := if r == "watch" then " (you are watching this topic)" else " (you were mentioned)"
      s!"#{s "stream"} > {s "topic"}, message {mid}{why}, from {who}, {time}"
  let quoted := quoteLines ((payload.getObjValAs? String "content").toOption.getD "")
  s!"- {mark}{head} (item {itemId}, {s "link"}):\n{quoted}"

end Orchestra.Resident
