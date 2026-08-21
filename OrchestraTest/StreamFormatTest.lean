import OrchestraTest.TestM
import Orchestra

open Lean (Json ToJson)
open Orchestra
open Orchestra.StreamFormat

deriving instance DecidableEq for ResultSubtype

@[test]
def resultSubtypeSuccess : Test := do
  let line := "{\"type\":\"result\",\"subtype\":\"success\"," ++
    "\"result\":\"all done\",\"num_turns\":5," ++
    "\"duration_ms\":3000,\"total_cost_usd\":0.01}"
  match parseEvent line with
  | some (.result sub _ _ _ _) =>
    TestM.assertEqual sub ResultSubtype.success (msg := "success subtype")
  | _ => TestM.fail "expected result event"

@[test]
def resultSubtypeErrorMaxBudget : Test := do
  let line :=
    r#"{"type":"result","subtype":"error_max_budget_usd","result":""}"#
  match parseEvent line with
  | some (.result sub _ _ _ _) =>
    TestM.assertEqual sub ResultSubtype.errorMaxBudgetUsd
      (msg := "error_max_budget_usd subtype")
  | _ => TestM.fail "expected result event"

@[test]
def resultSubtypeIsError : Test := do
  let line :=
    r#"{"type":"result","subtype":"success","is_error":true,"result":"API Error: permission denied"}"#
  match parseEvent line with
  | some (.result sub _ _ _ _) =>
    TestM.assertEqual sub (ResultSubtype.error "API Error: permission denied")
      (msg := "is_error=true should produce error subtype with message")
  | _ => TestM.fail "expected result event"

@[test]
def resultSubtypeUnknown : Test := do
  let line :=
    r#"{"type":"result","subtype":"some_new_error","result":""}"#
  match parseEvent line with
  | some (.result sub _ _ _ _) =>
    TestM.assertEqual sub (ResultSubtype.unknown "some_new_error")
      (msg := "unknown subtype")
  | _ => TestM.fail "expected result event"

@[test]
def resultSubtypeToJson : Test := do
  let toJsonStr (sub : ResultSubtype) : String :=
    Json.compress (ToJson.toJson sub)
  TestM.assertEqual (toJsonStr ResultSubtype.success) "\"success\""
    (msg := "toJson success")
  TestM.assertEqual (toJsonStr ResultSubtype.errorMaxBudgetUsd)
    "\"error_max_budget_usd\"" (msg := "toJson errorMaxBudgetUsd")
  TestM.assertEqual (toJsonStr (ResultSubtype.error "some message"))
    "\"error\"" (msg := "toJson error")
  TestM.assertEqual (toJsonStr (ResultSubtype.unknown "my_error"))
    "\"my_error\"" (msg := "toJson unknown")

@[test]
def resultEventRoundTrip : Test := do
  let ev : Event := .result .success (some 3) (some 1000) none "done"
  let j := ToJson.toJson ev
  match parseEvent (Json.compress j) with
  | some (.result sub _ _ _ _) =>
    TestM.assertEqual sub ResultSubtype.success
      (msg := "round-trip success")
  | _ => TestM.fail "expected result event"

/-! ## One line, several events

A turn that thinks, says what it is about to do, and then calls a tool arrives as one assistant
message with three content items. Every one of them is part of the conversation. -/

@[test]
def assistantMessageKeepsEveryContentItem : Test := do
  let line := r#"{"type":"assistant","message":{"content":[
    {"type":"thinking","thinking":"the retry loop resumes the same session"},
    {"type":"text","text":"Let me look at TaskRunner."},
    {"type":"tool_use","name":"Read","id":"toolu_01","input":{"file_path":"TaskRunner.lean"}}]}}"#
  let events : Array Event := parseEvents line
  TestM.assertEqual events.size 3 (msg := "all three content items survive")
  match events.toList with
  | [Event.assistant (.thinking t), Event.assistant (.text txt),
     Event.assistant (.toolUse name _ id)] => do
    TestM.assertEqual t "the retry loop resumes the same session" (msg := "thinking first")
    TestM.assertEqual txt "Let me look at TaskRunner." (msg := "text second")
    TestM.assertEqual name "Read" (msg := "tool call third")
    TestM.assertEqual id (some "toolu_01") (msg := "the call carries its id")
  | _ => TestM.fail "expected thinking, text and tool_use in that order"

@[test]
def emptyContentItemsAreDroppedButTheRestSurvive : Test := do
  let line := r#"{"type":"assistant","message":{"content":[
    {"type":"text","text":""},
    {"type":"text","text":"done"}]}}"#
  let events := parseEvents line
  TestM.assertEqual events.size 1 (msg := "the empty text is dropped, the real one is not")

@[test]
def anAssistantMessageWithNothingToShowIsEmpty : Test := do
  TestM.assertEqual (parseEvents r#"{"type":"assistant","message":{"content":[]}}"#).size 0
    (msg := "no content items, no events")
  TestM.assertEqual (parseEvents "not json at all").size 0
    (msg := "a line that does not parse carries nothing")

/-! ## A tool result with no output is still a result

Dropped, a tool call reads as one that never returned. -/

@[test]
def toolResultWithNoOutputIsKept : Test := do
  let line := r#"{"type":"user","tool_use_id":"toolu_01","tool_use_result":{"stdout":"","stderr":""}}"#
  match parseEvent line with
  | some (.toolResult out err id) => do
    TestM.assertEqual out "" (msg := "empty stdout")
    TestM.assertEqual err "" (msg := "empty stderr")
    TestM.assertEqual id (some "toolu_01") (msg := "paired with the call that made it")
  | _ => TestM.fail "expected the empty tool result to be kept"

@[test]
def toolUseIdRoundTripsThroughJson : Test := do
  let ev : Event := .toolResult "ok" "" (some "toolu_42")
  let j := Json.compress (ToJson.toJson ev)
  TestM.assert ((j.splitOn "toolu_42").length == 2)
    (msg := s!"the id survives serialisation; got {j}")
  let noId : Event := .toolResult "ok" "" none
  let j2 := Json.compress (ToJson.toJson noId)
  TestM.assert ((j2.splitOn "\"tool_use_id\":null").length == 2)
    (msg := s!"an absent id is null, not an omitted key; got {j2}")
