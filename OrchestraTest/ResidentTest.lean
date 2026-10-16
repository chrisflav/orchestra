import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Interactive

/-!
# Resident sessions

Phase 1 of `docs/design/resident-sessions.md`, checked without an agent: the configuration, the
inbox and its delivery semantics, the budget day, the turn a batch becomes, the state transitions a
resident's record goes through (created asleep, paused, resumed, reset, never ended), the listener
action that delivers into an inbox, and the command line.
-/

namespace OrchestraTest.Residents

private def withIdentities (act : IO α) : IO α := do
  let root : System.FilePath :=
    System.FilePath.mk "/tmp" / s!"orchestra-resident-test-{← IO.monoNanosNow}"
  IO.FS.createDirAll root
  let previous ← Identity.identitiesDirOverride.get
  Identity.setIdentitiesDirOverride (some root)
  try act
  finally
    Identity.setIdentitiesDirOverride previous
    try IO.FS.removeDirAll root catch _ => pure ()

private def mentions (hay needle : String) : Bool := (hay.splitOn needle).length > 1

private def parseApp (extra : String) : Except String AppConfig :=
  Json.parse s!"\{\"github_app\": \{\"app_id\": 1, \"private_key_path\": \"/k\"}{extra}}" >>=
    FromJson.fromJson?

private def settings : Identity.ResidentSettings :=
  (({} : Identity.ResidentSpec).settings {})

/-! ## Configuration -/

@[test]
def theResidentsBlockDefaultsToTheDesign : Test := do
  match parseApp "" with
  | .error e => TestM.fail e
  | .ok cfg =>
    TestM.assert cfg.residents.enabled (msg := "on unless turned off")
    TestM.assert (cfg.residents.budgetPerDay == 100.0) (msg := "a hundred dollars a day")
    TestM.assertEqual cfg.residents.processIdleSeconds 1800
    TestM.assertEqual cfg.residents.podIdleSeconds 21600
    TestM.assertEqual cfg.residents.batchWindowSeconds 3
    TestM.assertEqual cfg.residents.maxAwake 4 (msg := "max_awake")
    TestM.assertEqual cfg.residents.timezone "UTC"
    TestM.assertEqual cfg.residents.maxTurnsPerHour 60

@[test]
def theResidentsBlockIsReadAndItsBudgetIsStrict : Test := do
  match parseApp ", \"residents\": {\"budget_per_day\": 5, \"max_awake\": 2, \"timezone\": \"Europe/Berlin\", \"volume_size\": \"5Gi\", \"pod_idle_seconds\": 60}" with
  | .error e => TestM.fail e
  | .ok cfg =>
    TestM.assert (cfg.residents.budgetPerDay == 5.0) (msg := "the budget written")
    TestM.assertEqual cfg.residents.maxAwake 2
    TestM.assertEqual cfg.residents.timezone "Europe/Berlin"
    TestM.assertEqual cfg.residents.volumeSize (some "5Gi")
    TestM.assertEqual cfg.residents.podIdleSeconds 60
  match parseApp ", \"residents\": {\"budget_per_day\": \"lots\"}" with
  | .ok _ => TestM.fail "a budget that is not a number must not fall back to a hundred dollars"
  | .error e => TestM.assert (mentions e "budget_per_day") (msg := s!"names the field: {e}")

@[test]
def anIdentitysResidentBlockFillsInFromConfiguration : Test := do
  let j := Json.parse "{\"budget_per_day\": 7.5, \"process_idle_seconds\": 60, \"model\": \"m\", \"feeds\": [{\"kind\": \"zulip\"}]}"
  match j >>= Identity.parseResidentSpec "identity 'secretary'" with
  | .error e => TestM.fail e
  | .ok spec =>
    let s := spec.settings { budgetPerDay := 100.0, podIdleSeconds := 30 }
    TestM.assert (s.budgetPerDay == 7.5) (msg := "the block's own budget wins")
    TestM.assertEqual s.processIdleSeconds 60
    TestM.assertEqual s.podIdleSeconds 60
      (msg := "the pod is never kept for less than the process: warm needs a pod")
    TestM.assertEqual s.batchWindowSeconds 3 (msg := "absent fields come from configuration")
    TestM.assertEqual s.model (some "m")
    TestM.assertEqual s.agent "claude"
    TestM.assertEqual spec.feeds.size 1 (msg := "feeds are kept, though not acted on yet")

@[test]
def aResidentBlockWithARepositoryOrAnUnknownFieldIsRefused : Test := do
  for (body, needle) in [("{\"upstream\": \"a/b\"}", "upstream"), ("{\"budget\": 3}", "budget"),
                         ("{\"budget_per_day\": -1}", "negative"), ("[]", "object")] do
    match Json.parse body >>= Identity.parseResidentSpec "identity 'x'" with
    | .ok _ => TestM.fail s!"{body} should be refused"
    | .error e => TestM.assert (mentions e needle) (msg := s!"{body}: {e}")

@[test]
def anIdentityFileCarriesItsResidentBlock : Test := do
  let (plain, resident) ← withIdentities do
    for (name, body) in [("plain", "{\"name\": \"plain\"}"),
        ("secretary", "{\"name\": \"secretary\", \"resident\": {\"budget_per_day\": 2}}")] do
      let dir := (← Identity.identitiesDir) / name
      IO.FS.createDirAll dir
      IO.FS.writeFile (dir / "identity.json") body
    pure (← Identity.loadIdentity "plain", ← Identity.loadIdentity "secretary")
  TestM.assert ((plain.bind (·.resident)).isNone) (msg := "an identity without the block is not one")
  TestM.assert (((resident.bind (·.resident)).bind (·.budgetPerDay)) == some 2.0)
    (msg := "the block is read off identity.json")

/-! ## The budget day and the turn -/

@[test]
def theBudgetDayFollowsTheZone : Test := do
  -- 2026-10-16T23:30:00Z
  let t : Int := 1792193400
  TestM.assertEqual (Resident.dayAt t) "2026-10-16"
  TestM.assertEqual (Resident.dayAt t (2 * 3600)) "2026-10-17" (msg := "already tomorrow in Berlin")
  TestM.assertEqual (Resident.dayAt t (-5 * 3600)) "2026-10-16"
  TestM.assertEqual (Resident.parseUtcOffset "+0200") (some 7200)
  TestM.assertEqual (Resident.parseUtcOffset "-0530") (some (-19800))
  TestM.assertEqual (Resident.parseUtcOffset "CEST") none

private def item (id : Nat) (source : String) (text : String) (trusted : Bool := true)
    (sentIn : Option Nat := none) (at_ : String := "2026-10-16T10:00:00Z") : Resident.InboxItem :=
  { id, resident := "secretary", source, eventId := s!"e{id}", trusted, receivedAt := at_
    payload := Json.mkObj [("text", .str text)], sentIn }

@[test]
def aSingleChatMessageIsHandedOverAsWritten : Test := do
  TestM.assertEqual (Resident.renderTurn #[item 1 "human" "hello"]) "hello"
    (msg := "a chat reads as a chat")
  let t := Resident.renderTurn #[item 1 "human" "hello", item 2 "listener:prs" "PR 7 opened" false]
  TestM.assert (mentions t "hello" && mentions t "PR 7 opened") (msg := "both are carried")
  TestM.assert (mentions t "listener prs") (msg := "each says where it came from")
  TestM.assert (mentions t "not from a trusted source") (msg := "and whether it is trusted")
  TestM.assert (!mentions t Resident.redeliveryNote) (msg := "nothing was interrupted")

@[test]
def anInterruptedTurnsItemsComeBackUnderANote : Test := do
  let t := Resident.renderTurn #[item 1 "human" "hello" (sentIn := some 3)]
  TestM.assert (mentions t Resident.redeliveryNote) (msg := s!"says they may have been seen: {t}")
  TestM.assert (mentions t "hello")
  let lost := Resident.renderTurn #[item 2 "human" "again"] ["Your previous conversation was lost."]
  TestM.assert (lost.startsWith "Your previous conversation was lost.")
    (msg := "a note heads the turn")

@[test]
def aBatchWaitsForItsWindowUnlessAPersonIsWaiting : Test := do
  let at0 := (Usage.parseIso8601 "2026-10-16T10:00:00Z").getD 0
  TestM.assertEqual (Resident.batchDueAt #[item 1 "listener:x" "a"] 3) (some (at0 + 3))
  TestM.assertEqual (Resident.batchDueAt #[item 1 "listener:x" "a", item 2 "human" "b"] 3) (some at0)
    (msg := "a person is not kept waiting for the window")
  TestM.assertEqual (Resident.batchDueAt #[] 3) none

@[test]
def failuresBackOffAndTurnsAreCountedPerHour : Test := do
  TestM.assertEqual (Resident.backoffSeconds 0) 0
  TestM.assertEqual (Resident.backoffSeconds 1) 30
  TestM.assertEqual (Resident.backoffSeconds 2) 60
  TestM.assertEqual (Resident.backoffSeconds 9) 300 (msg := "at most five minutes")
  TestM.assertEqual (Resident.turnsInLastHour #[100, 3000, 3700] 3800) 2

@[test]
def aBudgetPauseLiftsWithTheDayAndNoOtherDoes : Test := do
  TestM.assert (!pauseLifted (some "budget:2026-10-16") "2026-10-16") (msg := "same day: still paused")
  TestM.assert (pauseLifted (some "budget:2026-10-16") "2026-10-17") (msg := "next day: lifted")
  TestM.assert (!pauseLifted (some "failed") "2026-10-17") (msg := "failures need an operator")
  TestM.assert (!pauseLifted (some "operator") "2026-10-17")

/-! ## The inbox -/

@[test]
def theInboxDeduplicatesAndHandsItemsOverUntilConfirmed : Test := do
  let (first, again, ready1, ready2, ready3, ready4, recent) ← withTempData "resident-inbox" do
    let first ← Resident.insert "secretary" "listener:prs" "7" false (Json.mkObj [("text", "a")])
    let again ← Resident.insert "secretary" "listener:prs" "7" false (Json.mkObj [("text", "a")])
    let _ ← Resident.insert "secretary" "human" "h1" true (Json.mkObj [("text", "b")])
    let _ ← Resident.insert "other" "human" "h1" true (Json.mkObj [("text", "c")])
    let ready1 ← Resident.ready "secretary"
    Resident.markSent (ready1.toList.map (·.id)) 4 40
    -- Sent but not confirmed: still ready, as an interrupted turn's items are.
    let ready2 ← Resident.ready "secretary"
    -- A result for some other turn confirms nothing of this one.
    let _ ← Resident.confirm "secretary" 3
    let ready3 ← Resident.ready "secretary"
    let _ ← Resident.confirm "secretary" 4
    let ready4 ← Resident.ready "secretary"
    pure (first, again, ready1, ready2, ready3, ready4, ← Resident.recent "secretary")
  TestM.assert first.isSome (msg := "the first insert is an item")
  TestM.assertEqual again none (msg := "the same event from the same source is inserted once")
  TestM.assertEqual (ready1.map (·.text)).toList ["a", "b"] (msg := "oldest first, this resident's")
  TestM.assertEqual ready2.size 2 (msg := "sent is not handed over")
  TestM.assertEqual (ready2.map (·.sentSeq)).toList [some 40, some 40]
    (msg := "the seq a client streams from")
  TestM.assertEqual ready3.size 2 (msg := "another turn's result confirms nothing")
  TestM.assertEqual ready4.size 0 (msg := "the turn's result confirms what it carried")
  TestM.assertEqual recent.2 2 (msg := "nothing is deleted")

@[test]
def spendAddsUpPerResidentAndDay : Test := do
  let (a, b, c) ← withTempData "resident-spend" do
    Resident.addSpend "secretary" "2026-10-16" 1.5
    Resident.addSpend "secretary" "2026-10-16" 2.0
    Resident.addSpend "secretary" "2026-10-17" 0.25
    Resident.addSpend "secretary" "2026-10-17" (-3.0)
    pure (← Resident.spentOn "secretary" "2026-10-16", ← Resident.spentOn "secretary" "2026-10-17",
          ← Resident.spentOn "other" "2026-10-16")
  TestM.assert (a == 3.5) (msg := s!"the day adds up: {a}")
  TestM.assert (b == 0.25) (msg := s!"a negative cost is not a refund: {b}")
  TestM.assert (c == 0.0) (msg := "another resident's spend is its own")

/-! ## The session -/

@[test]
def aResidentHasOneSessionCreatedAsleepAndUniqueByName : Test := do
  let (r, again, clash) ← withTempData "resident-session" do
    let r ← ensureResidentSession "secretary" { settings with budgetPerDay := 12.0 }
    let again ← ensureResidentSession "secretary" { settings with model := some "m2" }
    -- A second record claiming the same resident is refused by the unique index.
    let clash ← try
        saveSession { r with id := "someone-else" }
        pure false
      catch _ => pure true
    pure (r, again, clash)
  TestM.assertEqual r.id "resident-secretary" (msg := "a stable id, which keys its workspace")
  TestM.assert (r.status == .dormant) (msg := "asleep: a record, no process")
  TestM.assertEqual r.resident (some "secretary")
  TestM.assertEqual r.identity (some "secretary") (msg := "held as its identity")
  TestM.assert r.upstream.isNone (msg := "no repository")
  TestM.assertEqual again.id r.id (msg := "one per identity")
  TestM.assertEqual again.model (some "m2") (msg := "an asleep record follows configuration")
  TestM.assert clash (msg := "the resident column is unique")

@[test]
def aResidentIsPausedResumedAndResetButNeverEnded : Test := do
  let mgr ← Manager.new {} (fun _ => pure (some 0)) (fun _ _ => pure ())
  let (closed, paused, resumed, reset, before) ← withTempData "resident-states" do
    let before ← ensureResidentSession "secretary" settings
    mgr.setConfigured #[("secretary", settings)]
    let closed ← mgr.close before.id
    let _ ← mgr.pauseResident "secretary"
    let paused ← loadSession before.id
    let _ ← mgr.resumeResident "secretary"
    let resumed ← loadSession before.id
    let _ ← mgr.resetResident "secretary"
    pure (closed, paused, resumed, ← loadSession before.id, before)
  match closed with
  | .ok _ => TestM.fail "a resident's session does not end"
  | .error e => TestM.assert (mentions e "reset") (msg := s!"and says what to do instead: {e}")
  TestM.assertEqual (paused.map (·.pausedReason)) (some (some "operator"))
  TestM.assert ((paused.map (·.status == .paused)).getD false) (msg := "paused")
  TestM.assert ((resumed.map (·.status == .dormant)).getD false) (msg := "resumed is asleep")
  TestM.assertEqual (resumed.bind (·.pausedReason)) none
  match reset with
  | none => TestM.fail "the record is still there"
  | some r =>
    TestM.assert (r.agentSessionId != before.agentSessionId) (msg := "a new conversation")
    TestM.assert (!r.agentStarted) (msg := "which nothing has spoken in yet")
    TestM.assert (!r.status.isTerminal) (msg := "and it is not over")

@[test]
def aResidentsTurnsGoThroughItsInbox : Test := do
  let mgr ← Manager.new {} (fun _ => pure (some 0)) (fun _ _ => pure ())
  let cfg : AppConfig := { appId := 0, privateKeyPath := "" }
  let (direct, queued, ready) ← withTempData "resident-human" do
    let r ← ensureResidentSession "secretary" settings
    mgr.setConfigured #[("secretary", settings)]
    let direct ← mgr.send cfg r.id "hello"
    let _ ← mgr.pauseResident "secretary"
    let queued ← mgr.residentMessage cfg "secretary" "are you there?"
    pure (direct, queued, ← Resident.ready "secretary")
  match direct with
  | .ok _ => TestM.fail "a turn written past the inbox would be one the delivery loop never saw"
  | .error e => TestM.assert (mentions e "inbox") (msg := e)
  match queued with
  | .ok (.queued _ why) => TestM.assert (mentions why "operator") (msg := s!"says why: {why}")
  | .ok (.sent _) => TestM.fail "a paused resident takes no turn"
  | .error e => TestM.fail e
  TestM.assertEqual (ready.map (·.text)).toList ["are you there?"]
    (msg := "a person writing to a paused resident is queued, not refused")

@[test]
def aSpentDayPausesTheResidentUntilTheNextOne : Test := do
  let mgr ← Manager.new {} (fun _ => pure (some 0)) (fun _ _ => pure ())
  let cfg : AppConfig := { appId := 0, privateKeyPath := "" }
  let s := { settings with budgetPerDay := 1.0 }
  let (outcome, record, today) ← withTempData "resident-budget" do
    let r ← ensureResidentSession "secretary" s
    mgr.setConfigured #[("secretary", s)]
    let today ← Resident.today "UTC"
    Resident.addSpend "secretary" today 1.0
    let _ ← Resident.insert "secretary" "human" "h" true (Json.mkObj [("text", "hi")])
    let outcome ← mgr.deliver cfg "secretary" (urgent := true)
    pure (outcome, ← loadSession r.id, today)
  match outcome with
  | .paused why => TestM.assertEqual why s!"budget:{today}"
  | other => TestM.fail s!"expected a budget pause, got {repr other}"
  TestM.assertEqual (record.bind (·.pausedReason)) (some s!"budget:{today}")
  TestM.assert ((record.map (·.status == .paused)).getD false) (msg := "the record says so")

@[test]
def nothingReadyIsNothingToDeliverAndAnUnknownResidentIsRefused : Test := do
  let mgr ← Manager.new {} (fun _ => pure (some 0)) (fun _ _ => pure ())
  let cfg : AppConfig := { appId := 0, privateKeyPath := "" }
  let (empty, unknown) ← withTempData "resident-nothing" do
    let _ ← ensureResidentSession "secretary" settings
    mgr.setConfigured #[("secretary", settings)]
    pure (← mgr.deliver cfg "secretary", ← mgr.deliver cfg "nobody")
  match empty with
  | .nothing => TestM.assert true
  | other => TestM.fail s!"expected nothing, got {repr other}"
  match unknown with
  | .failed _ => TestM.assert true
  | other => TestM.fail s!"expected a refusal, got {repr other}"

@[test]
def aRestartLeavesAPausedResidentPausedAndPutsAWarmOneToSleep : Test := do
  let after ← withTempData "resident-restart" do
    let r ← ensureResidentSession "secretary" settings
    saveSession { r with status := .paused, pausedReason := some "operator" }
    let w ← ensureResidentSession "butler" settings
    saveSession { w with status := .warm }
    reconcile
    pure (← loadSession r.id, ← loadSession w.id)
  TestM.assert ((after.1.map (·.status == .paused)).getD false)
    (msg := "a restart does not lift a pause")
  TestM.assert ((after.2.map (·.status == .dormant)).getD false)
    (msg := "a warm environment did not survive the restart")
  TestM.assert (Daemon.isLeftoverStatus none (some .paused))
    (msg := "a paused resident holds nothing a starting daemon must keep")

/-! ## Listeners into an inbox -/

@[test]
def aListenerActionDeliversToAResident : Test := do
  let parse (s : String) := Json.parse s >>= FromJson.fromJson? (α := Listener.ActionConfig)
  match parse "{\"prompt_template\": \"PR {{number}}\", \"deliver_to\": \"secretary\"}" with
  | .error e => TestM.fail e
  | .ok a =>
    TestM.assertEqual a.deliverTo (some "secretary")
    TestM.assert (!a.trusted) (msg := "untrusted unless the action says so")
    let (source, eventId, trusted, payload) :=
      Listener.inboxItemFor a [("number", "7")] "prs" "acme/w#7" "fallback"
    TestM.assertEqual source "listener:prs"
    TestM.assertEqual eventId "acme/w#7" (msg := "keyed by the event, so a replay is one item")
    TestM.assert (!trusted)
    TestM.assertEqual (payload.getObjValAs? String "text" |>.toOption) (some "PR 7")
    let (_, fallback, _, _) := Listener.inboxItemFor a [] "prs" "" "fallback"
    TestM.assertEqual fallback "fallback" (msg := "an event without an id gets one")
  match parse "{\"prompt_template\": \"x\", \"deliver_to\": \"s\", \"workflow_path\": \"w.yaml\"}" with
  | .ok _ => TestM.fail "deliver_to and workflow_path are two different things to do"
  | .error e => TestM.assert (mentions e "deliver_to") (msg := e)
  match parse "{\"prompt_template\": \"x\", \"deliver_to\": \"../etc\"}" with
  | .ok _ => TestM.fail "a resident is named by a name"
  | .error _ => TestM.assert true
  match parse "{\"prompt_template\": \"x\", \"deliver_to\": \"s\", \"trusted\": true}" with
  | .ok a => TestM.assert a.trusted
  | .error e => TestM.fail e

/-! ## The command line -/

@[test]
def residentCommandsParse : Test := do
  TestM.assertEqual (Resident.parseCommand ["list"]).toOption (some .list)
  TestM.assertEqual (Resident.parseCommand ["show", "secretary"]).toOption (some (.show "secretary"))
  TestM.assertEqual (Resident.parseCommand ["pause", "secretary"]).toOption (some (.pause "secretary"))
  TestM.assertEqual (Resident.parseCommand ["resume", "secretary"]).toOption (some (.resume "secretary"))
  TestM.assertEqual (Resident.parseCommand ["reset", "secretary"]).toOption (some (.reset "secretary"))
  TestM.assertEqual (Resident.parseCommand ["inbox", "secretary"]).toOption (some (.inbox "secretary"))
  TestM.assertEqual (Resident.parseCommand ["send", "secretary", "hello", "there"]).toOption
    (some (.send "secretary" "hello there"))
  for bad in [[], ["show"], ["pause", "a", "b"], ["list", "x"], ["frobnicate", "a"],
              ["show", "../x"], ["send", "secretary"]] do
    TestM.assert (Resident.parseCommand bad).toOption.isNone (msg := s!"{bad} is refused")
  TestM.assertEqual (Resident.Command.route (.pause "s")) ("/api/v1/residents/s/pause", true)
  TestM.assertEqual (Resident.Command.route .list) ("/api/v1/residents", false)

/-! ## The migration -/

@[test]
def theResidentMigrationIsTheLatest : Test := do
  TestM.assertEqual (Store.migrations.getLast?.map (·.name)) (some "0006_resident_sessions")

end OrchestraTest.Residents
