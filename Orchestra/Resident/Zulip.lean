import Std.Data.HashMap
import Orchestra.Kleis
import Orchestra.Identity
import Orchestra.Listener
import Orchestra.Resident.Feed
import Orchestra.Resident.Inbox
import Orchestra.Resident.Watch
import Orchestra.Utils.Http

open Lean (Json ToJson FromJson)

/-!
# The Zulip feed: an event queue, through kleis, with the resident's own key

`docs/design/resident-sessions.md` §5.2. Every request a feed makes goes through the kleis proxy on
a token derived from the resident's identity — the same key the agent's own requests spend — and
narrowed twice: `zulip: only read` (whose `read` group includes the event queue) and
`services: zulip`. The daemon never holds a Zulip credential.

The token is derived with `task_id` `resident:<name>/feed:<feed>` and a few hours' life, derived
again before it expires, and revoked when the feed stops. Its revocation id is kept on the
listener's state row while it is live, so a daemon that died holding one has it revoked by the
next at its start.

Requests go to `https://<site>` through the proxy at kleis's `url` — the address the daemon reaches
kleis at, not the pods' `proxy` — with the token on curl's config stdin (never argv) and kleis's CA
bundle. The long poll gets `--max-time 120`; Zulip answers within ~90s with a heartbeat if nothing
happened.

The protocol (`runFeed`):

1. `GET /users/me` — the bot's id and name: its own messages are never items, and a mention by
   name is matched against it.
2. `POST /register` with `event_types=["message"]`, `all_public_streams=true`,
   `apply_markdown=false`.
3. Catch up from the checkpoint (the last message id seen) to the queue's `max_message_id`, one
   `GET /messages` per narrow term (`Resident.catchUpNarrows`). Registering first means nothing
   sent while reconnecting is missed; the inbox's unique key absorbs the overlap. A feed with no
   checkpoint yet starts at `max_message_id`: it does not replay the server's history.
4. Long-poll `GET /events`, again at once when it answers. `BAD_EVENT_QUEUE_ID` goes back to 2.

A failure backs off — 5s, doubling, at most five minutes — and is written to the listener's state
for the dashboard; the inbox is untouched.
-/

namespace Orchestra.Resident.Zulip

/-- How long a feed's token lives. Derived again `renewMarginSeconds` before it expires. -/
def tokenTtl : String := "4h"
def renewMarginSeconds : Nat := 1800

/-- The narrowings every feed token carries (§5.2, §13). -/
def feedNarrowings : List String := ["zulip: only read", "services: zulip"]

/-- The kleis task id of a feed's token. -/
def feedTaskId (resident feed : String) : String := s!"resident:{resident}/feed:{feed}"

/-- How long to wait after `failures` failures in a row: 5s, doubling, at most five minutes. -/
def backoffSeconds (failures : Nat) : Nat :=
  if failures == 0 then 0 else min 300 (5 * 2 ^ (min 10 (failures - 1)))

/-- Whether a token expiring at `expires` (epoch seconds; `0` unknown) and derived at `derivedAt`
    should be derived again at `now`. One whose expiry kleis did not say is renewed after three
    hours, inside `tokenTtl`. -/
def needsRenewal (expires : Nat) (derivedAt now : Int) : Bool :=
  if expires == 0 then now - derivedAt ≥ 3 * 3600
  else Int.ofNat expires - now ≤ Int.ofNat renewMarginSeconds

/-! ## Answers -/

/-- What `POST /register` answered. -/
structure Registered where
  queueId : String
  lastEventId : Int
  maxMessageId : Nat
  featureLevel : Nat := 0
deriving Repr

/-- The Zulip error code in an answer, if it is one. -/
def errorCode (j : Json) : Option String :=
  if (j.getObjValAs? String "result").toOption == some "error" then
    some ((j.getObjValAs? String "code").toOption.getD "BAD_REQUEST")
  else none

/-- The error message in an answer. -/
def errorMsg (j : Json) : String :=
  (j.getObjValAs? String "msg").toOption.getD j.compress

def parseRegistered (j : Json) : Except String Registered := do
  if let some code := errorCode j then throw s!"register refused ({code}): {errorMsg j}"
  let queueId ← j.getObjValAs? String "queue_id" |>.mapError (fun _ => "register answered no queue_id")
  let lastEventId := (j.getObjValAs? Int "last_event_id").toOption.getD (-1)
  let maxMessageId := (j.getObjValAs? Nat "max_message_id").toOption.getD 0
  let featureLevel := (j.getObjValAs? Nat "zulip_feature_level").toOption.getD 0
  return { queueId, lastEventId, maxMessageId, featureLevel }

/-- `channel` from feature level 250 (Zulip 9) on, `stream` before. -/
def channelOperator (featureLevel : Nat) : String :=
  if featureLevel ≥ 250 then "channel" else "stream"

def parseBot (j : Json) : Except String Bot := do
  if let some code := errorCode j then throw s!"users/me refused ({code}): {errorMsg j}"
  let id ← j.getObjValAs? Nat "user_id" |>.mapError (fun _ => "users/me answered no user_id")
  return { id, fullName := (j.getObjValAs? String "full_name").toOption.getD "" }

/-- One event of a `GET /events` answer: its id, and the message, for a `message` event. -/
def parseEvents (j : Json) : Except String (Array (Int × Option ZulipMessage)) := do
  if let some code := errorCode j then throw code
  let evs := (j.getObjValAs? (Array Json) "events").toOption.getD #[]
  evs.mapM fun e => do
    let id := (e.getObjValAs? Int "id").toOption.getD (-1)
    if (e.getObjValAs? String "type").toOption == some "message" then
      let flags := (e.getObjValAs? (List String) "flags").toOption
      match e.getObjVal? "message" with
      | .ok m => return (id, some (← parseMessage m flags))
      | .error _ => return (id, none)
    else return (id, none)

/-- The messages of a `GET /messages` answer, and whether it reached the newest. -/
def parseMessages (j : Json) : Except String (Array ZulipMessage × Bool) := do
  if let some code := errorCode j then throw s!"messages refused ({code}): {errorMsg j}"
  let ms := (j.getObjValAs? (Array Json) "messages").toOption.getD #[]
  return (← ms.mapM (parseMessage ·), (j.getObjValAs? Bool "found_newest").toOption.getD true)

/-- The real addresses in a `GET /users` answer, where the server shows them to the bot. -/
def parseDeliveryEmails (j : Json) : Std.HashMap Nat String := Id.run do
  let mut m : Std.HashMap Nat String := {}
  for u in (j.getObjValAs? (Array Json) "members").toOption.getD #[] do
    match u.getObjValAs? Nat "user_id", u.getObjValAs? String "delivery_email" with
    | .ok id, .ok e => if !e.isEmpty then m := m.insert id e
    | _, _ => pure ()
  return m

/-- Whether the bot can see a stream, and its name as the server writes it: from a
    `GET /streams` answer, which lists the public streams and the private ones it is
    subscribed to. -/
def visibleStream (j : Json) (name : String) : Except String (Option (String × Bool)) := do
  if let some code := errorCode j then throw s!"streams refused ({code}): {errorMsg j}"
  let ss := (j.getObjValAs? (Array Json) "streams").toOption.getD #[]
  return ss.findSome? fun s =>
    match s.getObjValAs? String "name" with
    | .ok n => if n.toLower == name.toLower then
        some (n, (s.getObjValAs? Bool "invite_only").toOption.getD false) else none
    | .error _ => none

/-! ## Requests through kleis -/

/-- What a feed's requests need: kleis, the CA bundle, the token, the server. -/
structure Conn where
  kleis : KleisConfig
  bundle : System.FilePath
  token : String
  site : String

/-- kleis's `url` without its path: where the daemon reaches the proxy. -/
def daemonProxyUrl (cfg : KleisConfig) : String :=
  let u := if cfg.url.endsWith "/" then (cfg.url.dropEnd 1).toString else cfg.url
  match u.splitOn "://" with
  | [scheme, rest] => s!"{scheme}://{(rest.splitOn "/").headD rest}"
  | _ => u

/-- One request to the server's API through kleis: `method` `/api/v1/<path>` with `params` (query
    for GET and DELETE, a form body for POST). Answers the status and the JSON. -/
def request (c : Conn) (method path : String) (params : List (String × String) := [])
    (maxTime : Nat := 30) : IO (Nat × Json) := do
  let proxy := daemonProxyUrl c.kleis
  let mut args : Array String := #["-x", proxy, "--cacert", c.bundle.toString]
  if proxy.startsWith "https:" then args := args ++ #["--proxy-cacert", c.bundle.toString]
  args := match method with
    | "POST" => args.push "-X" |>.push "POST"
    | "DELETE" => args ++ #["-G", "-X", "DELETE"]
    | _ => args.push "-G"
  for (k, v) in params do
    args := args.push "--data-urlencode" |>.push s!"{k}={v}"
  args := args.push s!"https://{c.site}/api/v1/{path}"
  let (status, _, body) ← Utils.Http.curlFull args (maxTime := maxTime)
    (extraConfig := Utils.Http.proxyUserConfig "orchestra" c.token)
  match Json.parse body with
  | .ok j =>
    -- Zulip's own refusals carry `result: error` and are the caller's to read (an expired queue
    -- is one); anything else that is not a success is kleis's or the network's.
    if status ≥ 400 && (j.getObjVal? "result").toOption.isNone then
      throw (.userError s!"{method} {path} answered {status}: {(body.take 300).toString}")
    return (status, j)
  | .error _ =>
    throw (.userError s!"{method} {path} answered {status}, not JSON: {(body.take 300).toString}")

/-! ## The feeds this daemon holds -/

/-- One feed while its runner is up: what the watch tool and the shutdown need of it. -/
structure Handle where
  listener : String
  resident : String
  feed : ZulipFeed
  conn : IO.Ref (Option Conn)
  minted : IO.Ref (Option Kleis.Minted)
  queue : IO.Ref (Option String)
  /-- `connecting`, `connected`, `error`, `disabled`, `refused` (no kleis source of its own). -/
  status : IO.Ref String
  /-- When the last long poll was answered, epoch seconds; `0` before the first. -/
  lastPoll : IO.Ref Int

initialize registry : IO.Ref (Std.HashMap String Handle) ← IO.mkRef {}

/-- The live handle of `resident`'s feed `feed`, if its runner is up. -/
def handleOf (resident feed : String) : IO (Option Handle) := do
  return (← registry.get).fold (init := none) fun acc _ h =>
    if acc.isNone && h.resident == resident && h.feed.name == feed then some h else acc

/-- When a feed's last long poll was answered (epoch seconds, `0` before the first), if its runner
    is up. -/
def lastPollOf (listener : String) : IO (Option Int) := do
  match (← registry.get).get? listener with
  | some h => some <$> h.lastPoll.get
  | none => return none

/-- A feed's connection state, for the dashboard: `none` when no runner holds it. -/
def statusOf (listener : String) : IO (Option String) := do
  match (← registry.get).get? listener with
  | some h => some <$> h.status.get
  | none => return none

/-- The Zulip feeds that deliver to `resident`: its own and any listener file's, by feed name. -/
def feedsOf (resident : String) : IO (List (String × ZulipFeed)) := do
  let cfgs ← Listener.loadAllListenerConfigs
  return cfgs.toList.filterMap fun (n, c) => match c.source with
    | .zulipFeed f => if c.action.deliverTo == some resident then some (n, f) else none
    | _ => none

/-- Update a feed's state row with `f`, applied to the row as it is now under the listener-state
    lock (`Listener.modifyListenerState`), so a change made meanwhile — the dashboard's enable
    switch — is kept. -/
def updateState (listener : String) (f : Listener.ListenerState → Listener.ListenerState) :
    IO Unit := do
  let _ ← Listener.modifyListenerState listener f

/-- Why resident `resident` cannot have a feed, or `none` when it can: a feed reads Zulip with the
    resident's *own* key — its identity's `kleis_spec` or `kleis_token` — and nothing else.
    `kleis.default_spec` in particular is not the resident's: a feed on it would read Zulip as
    whatever that spec allows, which is nobody's choice for this resident. -/
def missingSource (idn : Option Identity.Identity) (resident : String) : Option String :=
  match idn with
  | none => some s!"there is no identity named {resident} to read Zulip as"
  | some i =>
    if i.kleisSpec.isNone && i.kleisToken.isNone then
      some s!"identity {resident} has no kleis source of its own (kleis_spec or kleis_token); a \
feed reads Zulip only with the resident's own key, never with kleis.default_spec, so it is not \
started"
    else none

/-- Derive a token for the feed from the resident's identity, and record its revocation id
    before it is used. -/
def deriveToken (kc : KleisConfig) (listener resident feed : String) : IO Kleis.Minted := do
  let idn? ← Identity.loadIdentity resident
  if let some why := missingSource idn? resident then throw (.userError why)
  let some idn := idn? | throw (.userError s!"there is no identity named {resident}")
  -- The identity's own source only: no `defaultSpec` (see `missingSource`).
  let choice := Kleis.choose { identitySpec := idn.kleisSpec, identityToken := idn.kleisToken.isSome }
  let some src ← Kleis.resolve choice idn.kleisToken
    | throw (.userError s!"identity {resident} has no kleis source (kleis_spec or kleis_token); a \
feed reads Zulip only through kleis with the resident's own key")
  let facts : Kleis.TaskFacts := { taskId := feedTaskId resident feed, identity := some resident
                                   readOnly := true }
  let minted ← Kleis.derive kc src facts (idn.kleisNarrow ++ feedNarrowings) (ttl := some tokenTtl)
  updateState listener fun s => { s with revocations := s.revocations.push minted.revocationId }
  return minted

/-- Revoke a feed token and forget its revocation id. -/
def revokeToken (kc : KleisConfig) (listener : String) (m : Kleis.Minted) : IO Unit := do
  try
    Kleis.revokeId kc m.revocationId
    updateState listener fun s =>
      { s with revocations := s.revocations.filter (· != m.revocationId) }
  catch e => IO.eprintln s!"  Feed '{listener}': could not revoke its kleis token: {e}"

/-- Revoke what a previous run left recorded: tokens of a daemon that died holding them. -/
def revokeLeftovers (kc : KleisConfig) (listener : String) : IO Unit := do
  let st ← Listener.loadListenerState listener
  for r in st.revocations do
    try
      Kleis.revokeId kc r
      updateState listener fun s => { s with revocations := s.revocations.filter (· != r) }
      IO.println s!"  Feed '{listener}': revoked a token the previous run left"
    catch e => IO.eprintln s!"  Feed '{listener}': could not revoke a leftover token: {e}"

/-- Let go of the event queue, quietly: it expires on its own anyway. -/
def dropQueue (h : Handle) : IO Unit := do
  let some q ← h.queue.get | return
  h.queue.set none
  let some c ← h.conn.get | return
  try let _ ← request c "DELETE" "events" [("queue_id", q)] (maxTime := 10) catch _ => pure ()

/-- Stop holding the feed: drop its queue, revoke its token. -/
def release (kc : KleisConfig) (h : Handle) : IO Unit := do
  dropQueue h
  h.conn.set none
  if let some m ← h.minted.get then
    h.minted.set none
    revokeToken kc h.listener m

/-- Every feed's queue and token, released. The daemon's shutdown path. -/
def stopAll (kc? : Option KleisConfig) : IO Unit := do
  let some kc := kc? | return
  for (_, h) in (← registry.get).toList do
    try release kc h catch _ => pure ()

/-! ## Delivering -/

/-- Hand one message to the inbox if the feed admits it. Answers whether it was a new item. -/
def deliver (h : Handle) (bot : Bot) (watches : List WatchKey)
    (emails : Std.HashMap Nat String) (m : ZulipMessage) : IO Bool := do
  let m := { m with sender := { m.sender with deliveryEmail := emails.get? m.sender.id } }
  let some (reason, trusted) := classify h.feed bot watches m | return false
  let payload := payloadOf h.feed.site bot reason m
  match ← Resident.insert h.resident (zulipSource h.feed.site h.feed.name) (toString m.id) trusted
      payload with
  | some item =>
    IO.println s!"  Feed '{h.listener}': message {m.id} ({reason.toString}) → resident \
{h.resident}'s inbox (item {item})"
    return true
  | none => return false

/-- Catch up from `checkpoint` to `upTo`, one narrow at a time. -/
def catchUp (h : Handle) (c : Conn) (bot : Bot) (emails : Std.HashMap Nat String)
    (checkpoint upTo : Nat) (op : String) : IO Unit := do
  if checkpoint ≥ upTo then return
  let watches ← activeWatchKeys h.resident h.feed.name
  for (label, narrow) in catchUpNarrows h.feed bot watches op do
    let mut anchor := checkpoint
    -- Bounded: a feed that was down for a month does not read a month in one go; what is past
    -- the bound is in the server, a link away.
    for _ in [0:20] do
      let (_, j) ← request c "GET" "messages"
        [("anchor", toString anchor), ("include_anchor", "false"), ("num_before", "0"),
         ("num_after", "200"), ("narrow", narrow.compress), ("apply_markdown", "false")]
      let (ms, newest) ← match parseMessages j with
        | .ok r => pure r
        | .error e => throw (.userError s!"catching up on {label}: {e}")
      for m in ms do
        if m.id ≤ upTo then let _ ← deliver h bot watches emails m
      let last := ms.foldl (fun a m => max a m.id) anchor
      if newest || ms.isEmpty || last ≥ upTo || last == anchor then break
      anchor := last

/-- Hand a batch of events to `deliverOne`, in order, moving `lastEvent` past each only once it is
    in — an event that carries no message is past at once. Answers the ids of the messages handed
    over. A `deliverOne` that throws stops the batch with `lastEvent` on the last event that got in.

    `last_event_id` is what tells Zulip an event was taken. Moved before an insert that then
    failed, the event was acknowledged and gone, and the message with it; left where it is, the
    next long poll answers it again (and the runner registers the queue again besides, whose
    catch-up reads it back from the checkpoint). -/
def takeEvents (evs : Array (Int × Option ZulipMessage)) (lastEvent : IO.Ref Int)
    (deliverOne : ZulipMessage → IO Unit) : IO (List Nat) := do
  let mut ids : List Nat := []
  for (eid, m?) in evs do
    if let some m := m? then
      deliverOne m
      ids := m.id :: ids
    if eid > (← lastEvent.get) then lastEvent.set eid
  return ids

/-! ## The runner -/

/-- Run one feed until its listener goes away or the daemon stops: the long-poll loop. `stop`
    says the daemon is shutting down; `enabled` whether residents are on at all. -/
partial def runFeed (kc : KleisConfig) (listener : String) (stop : IO Bool) : IO Unit := do
  let some cfg ← Listener.loadListenerConfig listener | return
  let .zulipFeed feed := cfg.source | return
  let some resident := cfg.action.deliverTo | return
  let h : Handle := { listener, resident, feed, conn := ← IO.mkRef none, minted := ← IO.mkRef none
                      queue := ← IO.mkRef none, status := ← IO.mkRef "connecting"
                      lastPoll := ← IO.mkRef 0 }
  registry.modify (·.insert listener h)
  revokeLeftovers kc listener
  let bundle ← IO.mkRef (none : Option System.FilePath)
  let derivedAt ← IO.mkRef (0 : Int)
  let botRef ← IO.mkRef (none : Option Bot)
  let emails ← IO.mkRef ({} : Std.HashMap Nat String)
  let registered ← IO.mkRef (none : Option Registered)
  let lastEvent ← IO.mkRef (-1 : Int)
  let deliveryFailed ← IO.mkRef false
  let failures ← IO.mkRef 0
  let sleepUnlessStopped (secs : Nat) : IO Unit := do
    for _ in [0:secs] do
      if ← stop then return
      IO.sleep 1000
  let body : IO Unit := do
    while !(← stop) do
      -- The feed as configured now: a changed block takes effect, a removed one ends the runner.
      let some live ← Listener.loadListenerConfig listener | break
      let .zulipFeed feedNow := live.source | break
      unless live.action.deliverTo == some resident && feedNow.site == feed.site do break
      let h := { h with feed := feedNow }
      registry.modify (·.insert listener h)
      let st ← Listener.loadListenerState listener
      unless st.enabled do
        if (← h.status.get) != "disabled" then
          release kc h
          registered.set none
          h.status.set "disabled"
        sleepUnlessStopped 5
        continue
      -- The resident's own key or nothing: a feed whose identity has no kleis source of its own is
      -- not started — said on the listener, and looked at again a minute later, so one added to
      -- the identity is picked up without a restart.
      if let some why := missingSource (← Identity.loadIdentity resident) resident then
        if (← h.status.get) != "refused" then
          IO.eprintln s!"  Feed '{listener}': {why}"
          release kc h
          registered.set none
          h.status.set "refused"
          updateState listener fun s => { s with error := some why }
        sleepUnlessStopped 60
        continue
      if (← h.status.get) == "refused" then
        h.status.set "connecting"
        updateState listener fun s => { s with error := none }
      try
        -- The token: derived when there is none or it is close to expiring.
        let now ← Usage.nowEpoch
        let renew ← match ← h.minted.get with
          | none => pure true
          | some m => pure (needsRenewal m.expires (← derivedAt.get) now)
        if renew then
          let old ← h.minted.get
          let b ← match ← bundle.get with
            | some b => pure b
            | none => do let b ← Kleis.caBundle kc; bundle.set (some b); pure b
          let m ← deriveToken kc listener resident feed.name
          h.minted.set (some m)
          derivedAt.set now
          -- The queue belongs to the account, not the token: it carries on under the new one.
          h.conn.set (some { kleis := kc, bundle := b, token := m.token, site := feedNow.site })
          if let some o := old then revokeToken kc listener o
        let some c ← h.conn.get | throw (.userError "no connection")
        let bot ← match ← botRef.get with
          | some b => pure b
          | none => do
            let (_, j) ← request c "GET" "users/me"
            let b ← IO.ofExcept (parseBot j)
            botRef.set (some b)
            pure b
        match ← registered.get with
        | none =>
          let (_, j) ← request c "POST" "register"
            [("event_types", "[\"message\"]"), ("all_public_streams", "true"),
             ("apply_markdown", "false")]
          let r ← IO.ofExcept (parseRegistered j)
          -- The real addresses, where the server shows them: an `allow_senders` written with
          -- them then matches on a server that hides addresses from everyone else.
          try
            let (_, uj) ← request c "GET" "users"
            emails.set (parseDeliveryEmails uj)
          catch _ => pure ()
          let checkpoint := (st.checkpoint.bind (·.toNat?))
          match checkpoint with
          | some cp => catchUp h c bot (← emails.get) cp r.maxMessageId (channelOperator r.featureLevel)
          | none => pure ()
          let next := max r.maxMessageId (checkpoint.getD 0)
          updateState listener fun s => { s with checkpoint := some (toString next), error := none }
          registered.set (some r)
          h.queue.set (some r.queueId)
          lastEvent.set r.lastEventId
          h.status.set "connected"
          IO.println s!"  Feed '{listener}': connected to {feedNow.site} as {bot.fullName} \
(queue registered, caught up to message {next})"
        | some r =>
          let (_, j) ← try
              request c "GET" "events"
                [("queue_id", r.queueId), ("last_event_id", toString (← lastEvent.get))]
                (maxTime := 120)
            catch e =>
              -- curl's own timeout is a long poll that outlived Zulip's heartbeat: ask again.
              if (toString e).startsWith "curl failed (exit 28)" then pure (0, Json.mkObj [])
              else throw e
          match parseEvents j with
          | .error "BAD_EVENT_QUEUE_ID" =>
            IO.println s!"  Feed '{listener}': the event queue expired; registering again"
            registered.set none
            h.queue.set none
          | .error code => throw (.userError s!"events refused ({code}): {errorMsg j}")
          | .ok evs =>
            h.lastPoll.set (← Usage.nowEpoch)
            if !evs.isEmpty then
              let watches ← activeWatchKeys resident feedNow.name
              let ems ← emails.get
              let ids ← try takeEvents evs lastEvent fun m => do
                  let _ ← deliver h bot watches ems m
                catch e =>
                  deliveryFailed.set true
                  throw e
              unless ids.isEmpty do
                let nowIso ← TaskStore.currentIso8601
                updateState listener fun s =>
                  { s with checkpoint := (nextCheckpoint (s.checkpoint.bind (·.toNat?)) ids).map toString
                           lastChecked := nowIso, error := none }
        if (← failures.get) > 0 then
          failures.set 0
          updateState listener fun s => { s with error := none }
          h.status.set "connected"
      catch e =>
        let n := (← failures.get) + 1
        failures.set n
        let wait := backoffSeconds n
        let msg := toString e
        IO.eprintln s!"  Feed '{listener}': {msg}; trying again in {wait}s"
        updateState listener fun s => { s with error := some msg }
        h.status.set "error"
        -- An event that did not reach the inbox: register again, so the catch-up reads it back.
        if ← deliveryFailed.modifyGet (fun b => (b, false)) then
          registered.set none
          h.queue.set none
        -- A refusal may be the token's: derive a new one, and start the protocol over.
        if (msg.splitOn "403").length > 1 || (msg.splitOn "401").length > 1 || n % 3 == 0 then
          if let some m ← h.minted.get then
            h.minted.set none
            revokeToken kc listener m
          registered.set none
        sleepUnlessStopped wait
  try body
  finally
    release kc h
    registry.modify (·.erase listener)

/-! ## The watch tools -/

/-- One of a resident's own tools (`watch`, `unwatch`, `list_watches`). Answers the text the agent
    is shown, or why the call was refused. -/
def residentTool (resident tool : String) (args : Json) : IO (Except String String) := do
  let feeds ← feedsOf resident
  let names := feeds.map (·.2.name)
  match tool with
  | "watch" =>
    let req ← match parseWatchArgs args names with
      | .ok r => pure r
      | .error e => return .error e
    let now ← Usage.nowEpoch
    let expiresAt ← match req.untilArg with
      | none => pure none
      | some u => match parseUntil u now with
        | .ok t => pure (some t)
        | .error e => return .error e
    -- Visible to the bot, through the feed's own token: a private stream it is not subscribed to
    -- is refused — subscribing it is a person's job, which the spec does not allow.
    let some h ← handleOf resident req.feed
      | return .error s!"feed '{req.feed}' is not connected right now; try again in a minute"
    let some c ← h.conn.get
      | return .error s!"feed '{req.feed}' is not connected right now; try again in a minute"
    let visible ← try
        let (_, j) ← request c "GET" "streams"
        pure (visibleStream j req.key.stream)
      catch e => pure (.error s!"could not ask {h.feed.site} which streams the bot sees: {e}")
    match visible with
    | .error e => return .error e
    | .ok none =>
      return .error s!"the bot cannot see a stream named '{req.key.stream}' on {h.feed.site}: \
it is private and the bot is not subscribed to it, or there is none. Subscribing the bot is for a \
person to do."
    | .ok (some (name, _)) =>
      let key := { req.key with stream := name }
      let fresh ← addWatch resident req.feed key req.note expiresAt
      let where_ := s!"#{name}" ++ (key.topic.map (s!" > {·}")).getD " (every topic)"
      let lapses := (expiresAt.map (s!" until {·}")).getD ""
      return .ok s!"{if fresh then "Watching" else "Still watching"} {where_} on {req.feed}{lapses}."
  | "unwatch" =>
    let feed ← match pickFeed args names with
      | .ok f => pure f
      | .error e => return .error e
    let key ← match parseWatchKey ((args.getObjVal? "key").toOption.getD .null) with
      | .ok k => pure k
      | .error e => return .error e
    -- By the name as stored, which `watch` took from the server: matched case-insensitively.
    let ws ← listWatches resident (some feed)
    let stored := ws.find? fun w => w.key.stream.toLower == key.stream.toLower &&
      w.key.topic.map (·.toLower) == key.topic.map (·.toLower)
    match stored with
    | none => return .error "no such watch"
    | some w =>
      let _ ← removeWatch resident feed w.key
      return .ok s!"No longer watching #{w.key.stream}{(w.key.topic.map (s!" > {·}")).getD ""}."
  | "list_watches" =>
    let feed := (args.getObjValAs? String "feed").toOption
    return .ok (renderWatches (← listWatches resident feed))
  | _ => return .error s!"unknown resident tool {tool}"

end Orchestra.Resident.Zulip
