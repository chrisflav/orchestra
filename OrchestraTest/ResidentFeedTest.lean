import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Resident

/-!
# A resident's feeds and watches

Phase 2 of `docs/design/resident-sessions.md`, checked without a network: the feed configuration,
which Zulip messages become items and whether they are trusted, the narrows a reconnect catches up
with, the checkpoint, how a turn reads, the Zulip answers the feed reads, the watch tools and their
expiry, the listener a feed becomes, its state row, the resident-only tools on the MCP server, and
the derive a feed token is asked for.

`Zulip.e2e` at the end runs against a real kleis and a real Zulip, read-only, when
`ORCHESTRA_E2E_KLEIS_URL` says where one is (see the test for the rest).
-/

namespace OrchestraTest.ResidentFeeds

private def mentions (hay needle : String) : Bool := (hay.splitOn needle).length > 1

private def withDirs (act : IO α) : IO α := do
  let root : System.FilePath :=
    System.FilePath.mk "/tmp" / s!"orchestra-feed-test-{← IO.monoNanosNow}"
  IO.FS.createDirAll (root / "identities")
  IO.FS.createDirAll (root / "listeners")
  let prevI ← Identity.identitiesDirOverride.get
  let prevL ← Listener.listenersConfigDirOverride.get
  Identity.setIdentitiesDirOverride (some (root / "identities"))
  Listener.setListenersConfigDirOverride (some (root / "listeners"))
  try act
  finally
    Identity.setIdentitiesDirOverride prevI
    Listener.setListenersConfigDirOverride prevL
    try IO.FS.removeDirAll root catch _ => pure ()

private def feedOf (s : String) : Except String ZulipFeed :=
  Json.parse s >>= parseZulipFeed "test"

private def feed : ZulipFeed :=
  { site := "zulip.example.org", allowSenders := ["christian@flavigny.de", "*@merten.dev", "id:8"] }

private def bot : Bot := { id := 21, fullName := "Castor sysadmin" }

private def sender (id : Nat) (email : String) (name := "Someone") : Sender :=
  { id, email, name }

private def dm (id : Nat) (from_ : Sender) (content := "hi") : ZulipMessage :=
  { id, type := "private", sender := from_, content, timestamp := 1791672496
    recipients := [(from_.id, from_.email, from_.name), (21, "castor-bot@x", "Castor sysadmin")] }

private def streamMsg (id : Nat) (from_ : Sender) (stream topic : String) (content := "x")
    (flags : List String := []) : ZulipMessage :=
  { id, type := "stream", sender := from_, stream := some stream, streamId := some 5, topic,
    content, flags, timestamp := 1791672496 }

/-! ## Configuration -/

@[test]
def aZulipFeedHasTheDesignsDefaultsAndIsStrict : Test := do
  match feedOf "{\"kind\": \"zulip\", \"site\": \"https://zulip.merten.dev/\"}" with
  | .error e => TestM.fail e
  | .ok f =>
    TestM.assertEqual f.name "zulip"
    TestM.assertEqual f.site "zulip.merten.dev" (msg := "a scheme and a slash are dropped")
    TestM.assertEqual f.notify ["dm", "mention"]
    TestM.assertEqual f.watchSenders .any
    TestM.assertEqual f.allowSenders [] (msg := "nobody is allowed unless named")
  match feedOf "{\"kind\": \"zulip\", \"site\": \"z.org\", \"watch_senders\": [\"a@b\"], \"notify\": [\"dm\", \"wildcard\"], \"name\": \"work\"}" with
  | .error e => TestM.fail e
  | .ok f =>
    TestM.assertEqual f.watchSenders (.only ["a@b"])
    TestM.assertEqual f.notify ["dm", "wildcard"]
    TestM.assertEqual f.name "work"
  for (body, needle) in [
      ("{\"kind\": \"mail\", \"site\": \"x\"}", "not supported"),
      ("{\"site\": \"x\"}", "kind"),
      ("{\"kind\": \"zulip\"}", "site"),
      ("{\"kind\": \"zulip\", \"site\": \"a/b\"}", "host"),
      ("{\"kind\": \"zulip\", \"site\": \"x\", \"notify\": [\"everything\"]}", "notify"),
      ("{\"kind\": \"zulip\", \"site\": \"x\", \"allow\": []}", "unknown field"),
      ("{\"kind\": \"zulip\", \"site\": \"x\", \"watch_senders\": 3}", "watch_senders"),
      ("{\"kind\": \"zulip\", \"site\": \"x\", \"name\": \"a b\"}", "feed name")] do
    match feedOf body with
    | .ok _ => TestM.fail s!"{body} should be refused"
    | .error e => TestM.assert (mentions e needle) (msg := s!"{body}: {e}")
  let two := #[Json.mkObj [("kind", "zulip"), ("site", "a")], Json.mkObj [("kind", "zulip"), ("site", "b")]]
  TestM.assert (parseFeeds "t" two).toOption.isNone (msg := "two feeds named zulip are refused")

/-! ## Senders and messages -/

@[test]
def sendersMatchByAddressPatternOrUserId : Test := do
  TestM.assert (globMatch "*@merten.dev" "Alice@Merten.dev") (msg := "case-insensitive glob")
  TestM.assert (!globMatch "*@merten.dev" "alice@merten.dev.evil.org")
  TestM.assert (globMatch "a*c*e" "abcde")
  TestM.assert (senderMatches "id:8" (sender 8 "user8@zulip.example.org"))
  TestM.assert (senderMatches "8" (sender 8 "user8@zulip.example.org"))
  TestM.assert (!senderMatches "id:8" (sender 80 "x"))
  TestM.assert (senderMatches "christian@flavigny.de"
    { sender 8 "user8@zulip.example.org" with deliveryEmail := some "christian@flavigny.de" })
    (msg := "the real address counts where the server shows it")

@[test]
def onlyAllowedSendersWakeTheResidentAndTheBotNever : Test := do
  let alice := sender 3 "alice@merten.dev" "Alice"
  let eve := sender 4 "eve@evil.org" "Eve"
  TestM.assertEqual (classify feed bot [] (dm 1 alice)) (some (.dm, true))
  TestM.assertEqual (classify feed bot [] (dm 2 eve)) none (msg := "a stranger's DM is not an item")
  TestM.assertEqual (classify feed bot [] (dm 3 (sender 21 "castor-bot@x"))) none
    (msg := "the bot's own message never is")
  TestM.assertEqual (classify feed bot [] (streamMsg 4 alice "ops" "t" (flags := ["mentioned"])))
    (some (.mention, true))
  TestM.assertEqual (classify feed bot [] (streamMsg 5 eve "ops" "t" (flags := ["mentioned"]))) none
  TestM.assertEqual (classify feed bot [] (streamMsg 6 alice "ops" "t" "hey @**Castor sysadmin**"))
    (some (.mention, true)) (msg := "a mention by name, where the bot is not subscribed")
  TestM.assertEqual (classify feed bot [] (streamMsg 7 alice "ops" "t" "hey @**Castor sysadmin|21**"))
    (some (.mention, true))
  TestM.assertEqual (classify feed bot [] (streamMsg 8 alice "ops" "t" "fyi @_**Castor sysadmin**"))
    none (msg := "a silent mention is not one")
  TestM.assertEqual (classify feed bot [] (streamMsg 9 alice "ops" "t" (flags := ["wildcard_mentioned"])))
    none (msg := "wildcards only when notify says so")
  -- `wildcard` and `mention` are two switches: a feed on `["wildcard"]` alone wakes on `@**all**`
  -- (it used to wake on nothing at all) and not on its name.
  TestM.assertEqual (classify { feed with notify := ["wildcard"] } bot []
      (streamMsg 9 alice "ops" "t" (flags := ["stream_wildcard_mentioned"])))
    (some (.mention, true)) (msg := "wildcard alone admits a wildcard mention")
  TestM.assertEqual (classify { feed with notify := ["wildcard"] } bot []
      (streamMsg 9 alice "ops" "t" (flags := ["mentioned"])))
    none (msg := "and not a mention by name")
  TestM.assertEqual (classify { feed with notify := ["wildcard"] } bot []
      (streamMsg 9 eve "ops" "t" (flags := ["wildcard_mentioned"])))
    none (msg := "from an allowed sender only")
  TestM.assertEqual (classify { feed with notify := ["mention", "wildcard"] } bot []
      (streamMsg 9 alice "ops" "t" (flags := ["stream_wildcard_mentioned"])))
    (some (.mention, true))
  TestM.assertEqual (classify { feed with notify := ["mention"] } bot [] (dm 10 alice)) none
    (msg := "no dm in notify, no DM items")
  TestM.assertEqual (classify feed bot [] (streamMsg 11 alice "ops" "t")) none
    (msg := "an ordinary stream message is nothing")

@[test]
def watchedTopicsAdmitWhomWatchSendersSaysUntrusted : Test := do
  let eve := sender 4 "eve@evil.org" "Eve"
  let alice := sender 3 "alice@merten.dev" "Alice"
  let w := [({ stream := "ops", topic := some "Deploys" } : WatchKey)]
  TestM.assertEqual (classify feed bot w (streamMsg 1 eve "OPS" "deploys")) (some (.watch, false))
    (msg := "anyone, by default, and marked untrusted")
  TestM.assertEqual (classify feed bot w (streamMsg 2 alice "ops" "deploys")) (some (.watch, true))
  TestM.assertEqual (classify feed bot w (streamMsg 3 eve "ops" "other")) none
  TestM.assertEqual (classify feed bot [{ stream := "ops" }] (streamMsg 4 eve "ops" "anything"))
    (some (.watch, false)) (msg := "a stream watch covers every topic")
  TestM.assertEqual (classify { feed with watchSenders := .only ["*@merten.dev"] } bot w
    (streamMsg 5 eve "ops" "deploys")) none (msg := "watch_senders as a list")
  TestM.assertEqual (classify feed bot w (streamMsg 6 (sender 21 "bot") "ops" "deploys")) none

@[test]
def catchingUpReadsOneNarrowPerTerm : Test := do
  let w := [({ stream := "ops", topic := some "deploys" } : WatchKey), { stream := "general" }]
  let ns := catchUpNarrows feed bot w
  TestM.assertEqual (ns.map (·.1))
    ["is:dm", "is:mentioned", "channels:public search:Castor sysadmin", "channel:ops topic:deploys",
     "channel:general"]
  TestM.assertEqual ((ns.find? (·.1 == "channel:ops topic:deploys")).map (·.2.compress))
    (some "[{\"operand\":\"ops\",\"operator\":\"channel\"},{\"operand\":\"deploys\",\"operator\":\"topic\"}]")
    (msg := "a watch is one narrow of its terms, ANDed as Zulip does")
  let old := catchUpNarrows { feed with notify := ["dm"] } bot [{ stream := "ops" }] "stream"
  TestM.assertEqual (old.map (·.1)) ["is:dm", "stream:ops"]
    (msg := "an older server's operator; no mention terms without mention")
  TestM.assertEqual (Zulip.channelOperator 500) "channel"
  TestM.assertEqual (Zulip.channelOperator 100) "stream"

@[test]
def theCheckpointOnlyMovesForward : Test := do
  TestM.assertEqual (nextCheckpoint none [5, 3]) (some 5)
  TestM.assertEqual (nextCheckpoint (some 9) [5, 3]) (some 9)
  TestM.assertEqual (nextCheckpoint (some 9) []) (some 9)
  TestM.assertEqual (nextCheckpoint none []) none

/-! ## The turn -/

private def zitem (id : Nat) (payload : Json) (trusted : Bool) : InboxItem :=
  { id, resident := "secretary", source := "zulip:zulip.example.org", eventId := toString id
    trusted, receivedAt := "2026-10-17T09:14:00Z", payload }

@[test]
def zulipItemsReadAsTheDesignShows : Test := do
  let alice := sender 3 "alice@merten.dev" "Alice"
  let bob := sender 4 "bob@evil.org" "Bob"
  let p1 := payloadOf "zulip.example.org" bot .dm (dm 4711 alice "Can you move my 3pm to Thursday?")
  let p2 := payloadOf "zulip.example.org" bot .watch (streamMsg 4712 bob "ops" "deploys" "castor is back up\nall good")
  let turn := renderTurn #[zitem 1 p1 true, zitem 2 p2 false]
  TestM.assert (turn.startsWith "New on Zulip (zulip.example.org):\n") (msg := turn)
  TestM.assert (mentions turn "- DM from Alice <alice@merten.dev>, message 4711, 2026-10-10T22:48:16Z") (msg := turn)
  TestM.assert (mentions turn "  > Can you move my 3pm to Thursday?") (msg := turn)
  TestM.assert (mentions turn "- [not from an allowed sender] #ops > deploys, message 4712 (you are watching this topic), from Bob <bob@evil.org>")
    (msg := turn)
  TestM.assert (mentions turn "  > castor is back up\n  > all good") (msg := "every line quoted")
  TestM.assert (mentions turn "(item 2, https://zulip.example.org/#narrow/channel/5-ops/topic/deploys/near/4712)")
    (msg := turn)
  TestM.assert (turn.endsWith "Reply in the same place with the zulip client (your token card says how).")
  TestM.assert (!mentions turn "[not from an allowed sender] DM") (msg := "a trusted item is not marked")
  -- One Zulip item alone still reads as one, not as a bare text.
  TestM.assert ((renderTurn #[zitem 1 p1 true]).startsWith "New on Zulip") (msg := "alone too")
  -- Mixed with a chat message, and an interrupted turn's note first.
  let human : InboxItem := { id := 3, resident := "secretary", source := "human", eventId := "h"
                             trusted := true, receivedAt := "", payload := Json.mkObj [("text", "hello")] }
  let mixed := renderTurn #[zitem 1 p1 true, human, { zitem 2 p2 false with sentIn := some 4 }]
  TestM.assert (mixed.startsWith redeliveryNote) (msg := mixed)
  TestM.assert ((mixed.splitOn "New on Zulip").length == 2) (msg := "one heading per server")
  TestM.assert (mentions mixed "Message from the chat (item 3)") (msg := mixed)

@[test]
def aGroupDmNamesTheOthersAndALongMessageIsCut : Test := do
  let alice := sender 3 "alice@merten.dev" "Alice"
  let m := { dm 9 alice with recipients := [(3, "alice@merten.dev", "Alice"), (21, "bot", "Castor"),
                                            (22, "fuxi@x", "Fuxi")] }
  let p := payloadOf "z" bot .dm m
  TestM.assert (mentions (renderZulipItem p true 1) "DM from Alice <alice@merten.dev> (a group DM, with Fuxi)")
    (msg := renderZulipItem p true 1)
  let long := payloadOf "z" bot .dm (dm 10 alice (String.ofList (List.replicate 7000 'a')))
  let c := (long.getObjValAs? String "content").toOption.getD ""
  TestM.assert (c.length < 6100 && mentions c "cut") (msg := "cut, with a word saying so")
  TestM.assertEqual (isoOfEpoch 0) "1970-01-01T00:00:00Z"
  TestM.assertEqual (isoOfEpoch 1791672496) "2026-10-10T22:48:16Z"

/-! ## What Zulip answers -/

-- Shaped as zulip.merten.dev answered a read-only probe through kleis.
private def realDm : String :=
  "{\"id\":220,\"type\":\"private\",\"sender_email\":\"user8@zulip.kompass.merten.dev\",\"sender_id\":8,\"sender_full_name\":\"Christian Merten\",\"subject\":\"\",\"display_recipient\":[{\"id\":8,\"email\":\"user8@zulip.kompass.merten.dev\",\"full_name\":\"Christian Merten\"},{\"id\":21,\"email\":\"castor-bot@zulip.merten.dev\",\"full_name\":\"Castor sysadmin\"}],\"flags\":[],\"stream_id\":null,\"timestamp\":1791672496,\"content\":\"hello\"}"

@[test]
def theFeedReadsZulipsAnswers : Test := do
  match Json.parse realDm >>= (parseMessage ·) with
  | .error e => TestM.fail e
  | .ok m =>
    TestM.assertEqual m.id 220
    TestM.assertEqual m.sender.id 8
    TestM.assertEqual m.recipients.length 2
    TestM.assertEqual m.stream none
    TestM.assertEqual (classify { feed with allowSenders := ["id:8"] } bot [] m) (some (.dm, true))
  let events := s!"\{\"result\":\"success\",\"events\":[\{\"type\":\"heartbeat\",\"id\":3},\{\"type\":\"message\",\"id\":4,\"flags\":[\"mentioned\"],\"message\":{realDm}}]}"
  match Json.parse events >>= Zulip.parseEvents with
  | .error e => TestM.fail e
  | .ok evs =>
    TestM.assertEqual (evs.map (·.1)).toList [3, 4]
    TestM.assertEqual (evs.toList.filterMap (·.2) |>.map (·.flags)) [["mentioned"]]
      (msg := "an event's flags are beside its message")
  let bad := "{\"result\":\"error\",\"msg\":\"Bad event queue ID: nope\",\"queue_id\":\"nope\",\"code\":\"BAD_EVENT_QUEUE_ID\"}"
  TestM.assertEqual ((Json.parse bad >>= Zulip.parseEvents).toOption.map (·.size)) none
  match Json.parse bad >>= Zulip.parseEvents with
  | .error e => TestM.assertEqual e "BAD_EVENT_QUEUE_ID"
  | .ok _ => TestM.fail "an expired queue is an error"
  match Json.parse "{\"result\":\"success\",\"queue_id\":\"q1\",\"last_event_id\":-1,\"max_message_id\":222,\"zulip_feature_level\":500}"
      >>= Zulip.parseRegistered with
  | .error e => TestM.fail e
  | .ok r =>
    TestM.assertEqual r.queueId "q1"
    TestM.assertEqual r.lastEventId (-1)
    TestM.assertEqual r.maxMessageId 222
  let streams := "{\"result\":\"success\",\"streams\":[{\"name\":\"general\",\"invite_only\":false},{\"name\":\"castor\",\"invite_only\":true}]}"
  TestM.assertEqual ((Json.parse streams >>= (Zulip.visibleStream · "Castor")).toOption) (some (some ("castor", true)))
  TestM.assertEqual ((Json.parse streams >>= (Zulip.visibleStream · "secret")).toOption) (some none)
    (msg := "a private stream the bot is not in is not listed")
  let users := "{\"members\":[{\"user_id\":8,\"delivery_email\":\"christian@flavigny.de\"},{\"user_id\":9}]}"
  TestM.assertEqual ((Json.parse users).toOption.map (fun j => (Zulip.parseDeliveryEmails j).toList))
    (some [(8, "christian@flavigny.de")])
  match Json.parse "{\"result\":\"success\",\"user_id\":21,\"full_name\":\"Castor sysadmin\"}" >>= Zulip.parseBot with
  | .ok b => TestM.assertEqual (b.id, b.fullName) (21, "Castor sysadmin")
  | .error e => TestM.fail e

@[test]
def theFeedTokenIsRenewedBeforeItExpiresAndFailuresBackOff : Test := do
  TestM.assert (!Zulip.needsRenewal 10000 0 1000) (msg := "far from expiry")
  TestM.assert (Zulip.needsRenewal 10000 0 8300) (msg := "within half an hour of it")
  TestM.assert (Zulip.needsRenewal 0 0 (3 * 3600)) (msg := "an unknown expiry: after three hours")
  TestM.assertEqual ((List.range 9).map Zulip.backoffSeconds) [0, 5, 10, 20, 40, 80, 160, 300, 300]
  TestM.assertEqual (Zulip.feedTaskId "secretary" "zulip") "resident:secretary/feed:zulip"
  TestM.assertEqual (Zulip.daemonProxyUrl { url := "http://127.0.0.1:8080/" }) "http://127.0.0.1:8080"

@[test]
def theFeedTokenIsDerivedNarrowedToReadingZulip : Test := do
  let cfg : KleisConfig := { url := "http://k:1" }
  let body := Kleis.deriveBody cfg (.spec "secretary")
    { taskId := Zulip.feedTaskId "secretary" "zulip", identity := some "secretary", readOnly := true }
    Zulip.feedNarrowings (ttl := some Zulip.tokenTtl)
  TestM.assertEqual (body.getObjValAs? String "ttl").toOption (some "4h")
  TestM.assertEqual (body.getObjValAs? (List String) "narrow").toOption
    (some ["zulip: only read", "services: zulip"])
  TestM.assertEqual (body.getObjValAs? String "bearer").toOption
    (some "orchestra:resident:secretary/feed:zulip")
  TestM.assertEqual ((Kleis.deriveBody cfg (.spec "s") { taskId := "t" } []).getObjValAs? String "ttl").toOption
    (some "12h") (msg := "kleis.ttl otherwise")

/-! ## Watches -/

@[test]
def watchCallsAreReadStrictly : Test := do
  let args (s : String) := (Json.parse s).toOption.getD .null
  match parseWatchArgs (args "{\"key\": {\"stream\": \"#ops\", \"topic\": \" deploys \"}, \"note\": \"release\"}") ["zulip"] with
  | .error e => TestM.fail e
  | .ok r =>
    TestM.assertEqual r.feed "zulip" (msg := "the only feed, when none is named")
    TestM.assertEqual r.key { stream := "ops", topic := some "deploys" }
    TestM.assertEqual r.note (some "release")
  for (body, feeds, needle) in [
      ("{\"key\": {\"stream\": \"ops\"}}", ["a", "b"], "name the feed"),
      ("{\"feed\": \"c\", \"key\": {\"stream\": \"ops\"}}", ["a", "b"], "no feed 'c'"),
      ("{\"key\": {\"stream\": \"ops\"}}", [], "no feed"),
      ("{\"key\": {\"topic\": \"x\"}}", ["zulip"], "stream"),
      ("{\"key\": \"ops\"}", ["zulip"], "object"),
      ("{\"key\": {\"stream\": \"ops\", \"topic\": 3}}", ["zulip"], "topic")] do
    match parseWatchArgs (args body) feeds with
    | .ok _ => TestM.fail s!"{body} should be refused"
    | .error e => TestM.assert (mentions e needle) (msg := s!"{body}: {e}")
  TestM.assertEqual ({ stream := "ops", topic := some "d" } : WatchKey).toJsonString
    "{\"stream\":\"ops\",\"topic\":\"d\"}"
  TestM.assertEqual ({ stream := "ops" } : WatchKey).toJsonString "{\"stream\":\"ops\"}"

@[test]
def untilIsAnInstantOrASpanAndWatchesLapse : Test := do
  let now : Int := 1791672496
  TestM.assertEqual (parseUntil "12h" now).toOption (some (Usage.secsToIso8601 (now + 43200)))
  TestM.assertEqual (parseUntil "3d" now).toOption (some (Usage.secsToIso8601 (now + 259200)))
  TestM.assertEqual (parseUntil "2030-01-01" now).toOption (some "2030-01-01T00:00:00Z")
  TestM.assertEqual (parseUntil "2030-01-01T10:00:00Z" now).toOption (some "2030-01-01T10:00:00Z")
  for bad in ["2020-01-01", "soon", "0h", ""] do
    TestM.assert (parseUntil bad now).toOption.isNone (msg := s!"'{bad}' is refused")
  let w : Watch := { id := 1, resident := "s", feed := "zulip", key := { stream := "ops" }
                     createdAt := "", expiresAt := some (Usage.secsToIso8601 (now + 10)) }
  TestM.assert (!w.expired now)
  TestM.assert (w.expired (now + 10))
  TestM.assert (!{ w with expiresAt := none }.expired (now + 100000)) (msg := "no until, no lapse")

@[test]
def watchesAreStoredOncePerKeyAndLapse : Test := do
  let (fresh, again, listed, active, removed, gone, lapsed) ← withTempData "resident-watch" do
    let fresh ← addWatch "secretary" "zulip" { stream := "ops", topic := some "deploys" } (some "n") none
    let again ← addWatch "secretary" "zulip" { stream := "ops", topic := some "deploys" } (some "m")
      (some "2099-01-01T00:00:00Z")
    let _ ← addWatch "secretary" "zulip" { stream := "general" } none (some "2000-01-01T00:00:00Z")
    let _ ← addWatch "secretary" "work" { stream := "x" } none none
    let _ ← addWatch "other" "zulip" { stream := "ops" } none none
    let active ← activeWatchKeys "secretary" "zulip"
    let listed ← listWatches "secretary" (some "zulip")
    let lapsed := (← listWatches "secretary").size
    let removed ← removeWatch "secretary" "zulip" { stream := "ops", topic := some "deploys" }
    let gone ← removeWatch "secretary" "zulip" { stream := "ops", topic := some "deploys" }
    pure (fresh, again, listed, active, removed, gone, lapsed)
  TestM.assert fresh (msg := "a new watch")
  TestM.assert (!again) (msg := "the same key again replaces it")
  TestM.assertEqual (listed.map (·.note)).toList [some "m"] (msg := "with its new note; the lapsed one gone")
  TestM.assertEqual (listed.map (·.expiresAt)).toList [some "2099-01-01T00:00:00Z"]
  TestM.assertEqual active [{ stream := "ops", topic := some "deploys" }]
    (msg := "this resident's, this feed's, not lapsed")
  TestM.assertEqual lapsed 2 (msg := "every feed's, without the lapsed one")
  TestM.assert removed
  TestM.assert (!gone)
  TestM.assert (mentions (renderWatches listed) "- zulip: #ops > deploys, until 2099-01-01T00:00:00Z — m")
  TestM.assertEqual (renderWatches #[]) "Nothing is being watched."

/-! ## The listener a feed is, and its state -/

@[test]
def aZulipFeedSourceRoundTripsAndNeedsAResident : Test := do
  let raw := "{\"source\": {\"type\": \"zulip-feed\", \"site\": \"zulip.example.org\", \"allow_senders\": [\"id:8\"]}, \"action\": {\"prompt_template\": \"\", \"deliver_to\": \"secretary\"}}"
  match Json.parse raw >>= (FromJson.fromJson? · : Json → Except String Listener.ListenerConfig) with
  | .error e => TestM.fail e
  | .ok c =>
    TestM.assert c.source.isLongPoll
    match c.source with
    | .zulipFeed f => TestM.assertEqual f.allowSenders ["id:8"]
    | _ => TestM.fail "a zulip-feed source"
    match (FromJson.fromJson? (ToJson.toJson c.source) : Except String Listener.SourceConfig) with
    | .ok (.zulipFeed f) => TestM.assertEqual f.site "zulip.example.org" (msg := "round trip")
    | _ => TestM.fail "the source round-trips"
  let (ok, noResident) ← withDirs do
    let ok ← Listener.validateListenerConfig "feed" raw
    let noResident ← Listener.validateListenerConfig "feed"
      "{\"source\": {\"type\": \"zulip-feed\", \"site\": \"z\"}, \"action\": {\"prompt_template\": \"\"}}"
    pure (ok, noResident)
  TestM.assert ok.toOption.isSome (msg := "a feed delivering to a resident is a listener")
  match noResident with
  | .ok _ => TestM.fail "a feed with nobody to read as is refused"
  | .error e => TestM.assert (mentions e "deliver_to") (msg := e)

@[test]
def aResidentsFeedsBecomeItsListeners : Test := do
  let (all, one, clash) ← withDirs do
    let dir ← Identity.identitiesDir
    IO.FS.createDirAll (dir / "secretary")
    IO.FS.writeFile (dir / "secretary" / "identity.json")
      "{\"name\": \"secretary\", \"kleis_spec\": \"secretary\", \"resident\": {\"feeds\": [{\"kind\": \"zulip\", \"site\": \"zulip.example.org\"}]}}"
    let all ← Listener.loadAllListenerConfigs
    let one ← Listener.loadListenerConfig "resident-secretary-zulip"
    let clash ← Listener.validateListenerConfig "resident-secretary-zulip"
      "{\"source\": {\"type\": \"shell\", \"command\": \"true\"}, \"action\": {\"prompt_template\": \"\"}}"
    pure (all, one, clash)
  TestM.assertEqual (all.map (·.1)).toList ["resident-secretary-zulip"]
  match one with
  | some c =>
    TestM.assertEqual c.action.deliverTo (some "secretary") (msg := "owned by the resident")
    TestM.assert c.source.isLongPoll
  | none => TestM.fail "the feed's listener is found by name"
  TestM.assert clash.toOption.isNone (msg := "a file may not take a feed's listener name")

@[test]
def aFeedsCheckpointErrorAndTokensAreItsListenerState : Test := do
  let (st, cleared) ← withTempData "feed-state" do
    Listener.saveListenerState "resident-secretary-zulip"
      { lastChecked := "", processedIds := #[], checkpoint := some "4711", error := some "kleis said 403"
        revocations := #["r1", "r2"] }
    let st ← Listener.loadListenerState "resident-secretary-zulip"
    Zulip.updateState "resident-secretary-zulip" fun s =>
      { s with revocations := s.revocations.filter (· != "r1"), error := none }
    pure (st, ← Listener.loadListenerState "resident-secretary-zulip")
  TestM.assertEqual st.checkpoint (some "4711")
  TestM.assertEqual st.error (some "kleis said 403")
  TestM.assertEqual st.revocations #["r1", "r2"]
  TestM.assertEqual cleared.revocations #["r2"]
  TestM.assertEqual cleared.error none
  TestM.assertEqual cleared.checkpoint (some "4711") (msg := "an update keeps what it does not touch")

/-! ## The resident's tools on its MCP server -/

private def serverState (resident : Bool) (calls : IO.Ref (List String)) : Server.State :=
  { repo := none, allowedTools := [], appId := 0, privateKeyPath := "", installationId := none
    pat := ""
    residentTools := if resident then some ("secretary", fun tool _ => do
      calls.modify (tool :: ·)
      return if tool == "unwatch" then .error "no such watch" else .ok s!"did {tool}") else none }

private def offered (st : Server.State) : List String :=
  match (Server.toolsList st).getObjVal? "tools" |>.toOption |>.bind (·.getArr? |>.toOption) with
  | none => []
  | some defs => defs.toList.filterMap (·.getObjValAs? String "name" |>.toOption)

private def textOf (j : Json) : String :=
  ((j.getObjValAs? (Array Json) "content").toOption.bind (·[0]?)).bind
    (·.getObjValAs? String "text" |>.toOption) |>.getD ""

@[test]
def onlyAResidentIsOfferedTheWatchTools : Test := do
  let calls ← IO.mkRef []
  for t in Server.residentToolNames.filter (· != "refresh_kleis_token") do
    TestM.assert ((offered (serverState true calls)).contains t) (msg := s!"{t} for a resident")
    TestM.assert (!(offered (serverState false calls)).contains t) (msg := s!"{t} for nobody else")
  let refused ← Server.evalToolCall (serverState false calls)
    (Server.parseToolCall "watch" (Json.mkObj []))
  TestM.assert (mentions (textOf refused) "not a resident") (msg := textOf refused)
  let ok ← Server.evalToolCall (serverState true calls) (Server.parseToolCall "list_watches" (Json.mkObj []))
  TestM.assertEqual (textOf ok) "did list_watches"
  let err ← Server.evalToolCall (serverState true calls) (Server.parseToolCall "unwatch" (Json.mkObj []))
  TestM.assertEqual (err.getObjValAs? Bool "isError").toOption (some true)
  TestM.assertEqual (← calls.get) ["unwatch", "list_watches"]

/-! ## Against a real kleis and a real Zulip

Read-only: the feed derives its token, reads `users/me`, registers a queue, catches up from a
checkpoint, holds one long poll open until Zulip answers, checks a watch's visibility, and lets
everything go. Nothing is sent. Set up by `tmp/real/feed-e2e.sh`, which runs kleis and exports:

* `ORCHESTRA_E2E_KLEIS_URL`, `ORCHESTRA_E2E_KLEIS_ISSUER_FILE` — kleis, and its issuer credential;
* `ORCHESTRA_E2E_KLEIS_SPEC` — a spec reaching the Zulip account (`allow read` is enough);
* `ORCHESTRA_E2E_ZULIP_SITE`; `ORCHESTRA_E2E_CHECKPOINT`, a message id to catch up from;
* `ORCHESTRA_E2E_EXPECT_DM`, a message id at or below the newest that is a DM to the bot from
  `ORCHESTRA_E2E_ALLOW` (an `allow_senders` entry), and `ORCHESTRA_E2E_PRIVATE_STREAM`, a private
  stream the bot is subscribed to. -/

private def waitFor (secs : Nat) (cond : IO Bool) : IO Bool := do
  for _ in [0:secs * 2] do
    if ← cond then return true
    IO.sleep 500
  cond

@[test]
def zulipFeedEndToEnd : Test := do
  let some url ← IO.getEnv "ORCHESTRA_E2E_KLEIS_URL"
    | TestM.skip "ORCHESTRA_E2E_KLEIS_URL is unset; no real kleis and Zulip to read through"
  let env (k d : String) : IO String := return (← IO.getEnv k).getD d
  let issuerFile ← env "ORCHESTRA_E2E_KLEIS_ISSUER_FILE" ""
  let spec ← env "ORCHESTRA_E2E_KLEIS_SPEC" "reader"
  let site ← env "ORCHESTRA_E2E_ZULIP_SITE" "zulip.merten.dev"
  let checkpoint ← env "ORCHESTRA_E2E_CHECKPOINT" "210"
  let expectDm ← env "ORCHESTRA_E2E_EXPECT_DM" "220"
  let allow ← env "ORCHESTRA_E2E_ALLOW" "id:8"
  let privateStream ← env "ORCHESTRA_E2E_PRIVATE_STREAM" "castor"
  let kc : KleisConfig := { url, issuerTokenFile := some issuerFile }
  let listener := Listener.residentFeedListenerName "e2e" "zulip"
  let result ← withDirs <| withTempData "feed-e2e" do
    let dir ← Identity.identitiesDir
    IO.FS.createDirAll (dir / "e2e")
    IO.FS.writeFile (dir / "e2e" / "identity.json") (Json.mkObj [
      ("name", "e2e"), ("kleis_spec", .str spec),
      ("resident", Json.mkObj [("feeds", Json.arr #[Json.mkObj [
        ("kind", "zulip"), ("site", .str site), ("allow_senders", Json.arr #[.str allow])]])])]).compress
    Listener.saveListenerState listener { lastChecked := "", processedIds := #[], checkpoint := some checkpoint
                                          revocations := #["not-a-real-revocation-id"] }
    let stop ← IO.mkRef false
    let started ← Usage.nowEpoch
    let task ← IO.asTask (prio := .dedicated) (Zulip.runFeed kc listener stop.get)
    let connected ← waitFor 60 do return (← Zulip.statusOf listener) == some "connected"
    let st ← Listener.loadListenerState listener
    let items := (← Resident.recent "e2e" 200).1
    -- The watch tool, through the feed's own token: a private stream the bot is in is visible.
    let watched ← Zulip.residentTool "e2e" "watch"
      (Json.mkObj [("key", Json.mkObj [("stream", .str privateStream), ("topic", "e2e-test")]),
                   ("until", "1h")])
    let refused ← Zulip.residentTool "e2e" "watch"
      (Json.mkObj [("key", Json.mkObj [("stream", "no-such-stream-orchestra-e2e")])])
    let listed ← Zulip.residentTool "e2e" "list_watches" (Json.mkObj [])
    -- One long poll answered: a heartbeat within ~90s, held open through kleis.
    let polled ← waitFor 130 do return ((← Zulip.lastPollOf listener).getD 0) > 0
    let pollSecs := ((← Zulip.lastPollOf listener).getD 0) - started
    stop.set true
    let _ ← IO.wait task
    let after ← Listener.loadListenerState listener
    let status ← Zulip.statusOf listener
    pure (connected, st, items, watched, refused, listed, polled, pollSecs, after, status)
  let (connected, st, items, watched, refused, listed, polled, pollSecs, after, status) := result
  IO.println s!"  [e2e] connected={connected} checkpoint={st.checkpoint} error={st.error} items={items.map (·.eventId)} watched={repr watched} refused={repr refused} polled={polled} after {pollSecs}s revocations-after={after.revocations}"
  TestM.assert connected (msg := s!"the feed connected (error: {st.error})")
  TestM.assert ((st.checkpoint.bind (·.toNat?)).getD 0 ≥ expectDm.toNat!) (msg := "caught up past the checkpoint")
  match items.find? (·.eventId == expectDm) with
  | none => TestM.fail s!"message {expectDm}, a DM from {allow}, is in the inbox"
  | some i =>
    TestM.assert i.trusted (msg := "from an allowed sender")
    TestM.assertEqual (i.payload.getObjValAs? String "reason").toOption (some "dm")
  TestM.assert (items.all (·.source.startsWith s!"zulip:{site}#")) (msg := "every item is the feed's")
  TestM.assert (items.all fun i => (i.payload.getObjVal? "sender" >>= (·.getObjValAs? Nat "id")).toOption != some 21)
    (msg := "none is the bot's own")
  TestM.assert (watched matches .ok _) (msg := s!"a private stream the bot is in can be watched: {repr watched}")
  TestM.assert (match refused with | .error e => mentions e "cannot see" | .ok _ => false)
    (msg := s!"a stream it cannot see is refused: {repr refused}")
  TestM.assert (match listed with | .ok t => mentions t privateStream | .error _ => false)
  TestM.assert polled (msg := "a long poll through kleis was answered")
  TestM.assertEqual after.revocations #[] (msg := "the token is revoked at the end, and the leftover at the start")
  TestM.assertEqual status none (msg := "the runner let go of the feed")

end OrchestraTest.ResidentFeeds
