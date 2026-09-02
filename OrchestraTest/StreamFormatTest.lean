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

/-- The shape Claude Code actually emits for a tool result: the id and the payload live in the
    `tool_result` content block, and `tool_use_result` carries the raw tool return — which has
    `stdout`/`stderr` only for Bash. -/
private def bashResultLine : String :=
  r#"{"type":"user","message":{"role":"user","content":[
       {"type":"tool_result","tool_use_id":"toolu_01","content":"hi"}]},
      "tool_use_result":{"stdout":"","stderr":""}}"#

@[test]
def toolResultWithNoOutputIsKept : Test := do
  match parseEvent bashResultLine with
  | some (.toolResult _ err id) => do
    TestM.assertEqual err "" (msg := "empty stderr")
    -- The id is read from `message.content[i].tool_use_id`, which is where the CLI puts it —
    -- not from the top level and not from inside `tool_use_result`, neither of which has it.
    TestM.assertEqual id (some "toolu_01") (msg := "paired with the call that made it")
  | _ => TestM.fail "expected the empty tool result to be kept"

@[test]
def aUserMessageThatIsNotAToolResultIsNotAnEvent : Test := do
  -- The CLI emits these constantly: feedback from a `Stop` hook (which is what `goalArgs`
  -- installs), "continue from where you left off", and other synthetic injections. Read as a
  -- tool result they put an answer in the log for a call that never happened.
  let line := r#"{"type":"user","message":{"role":"user","content":[
      {"type":"text","text":"The goal is not met because there are no tests."}]},
     "isSynthetic":true,"parent_tool_use_id":null}"#
  TestM.assertEqual (parseEvents line).size 0
    (msg := "a user line with no tool_use_result is an ordinary message, not a tool result")

@[test]
def aNonBashToolResultCarriesItsContent : Test := do
  -- Only Bash reports stdout/stderr. Everything else answers in the content block, and reading
  -- only the two streams rendered every Read and Edit as an empty `[output]`.
  let line := r#"{"type":"user","message":{"role":"user","content":[
      {"type":"tool_result","tool_use_id":"toolu_02","content":[
        {"type":"text","text":"the file contents"}]}]},
     "tool_use_result":{"type":"text","file":{"filePath":"a.lean"}}}"#
  match parseEvent line with
  | some (.toolResult out _ id) => do
    TestM.assertEqual out "the file contents"
      (msg := "the content block is the payload when there are no streams")
    TestM.assertEqual id (some "toolu_02") (msg := "and it still pairs with its call")
  | _ => TestM.fail "expected a tool result carrying its content"

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

@[test]
def theCliSProgressBookkeepingIsNotAnEvent : Test := do
  -- Captured from `claude --print --output-format stream-json --verbose`, which emits one of
  -- these per hundred tokens of reasoning. A turn that thinks for a minute produces hundreds,
  -- and a chat transcript that keeps them is hundreds of lines reading "thinking_tokens" around
  -- one paragraph of answer.
  let thinkingTokens :=
    r#"{"type":"system","subtype":"thinking_tokens","estimated_tokens":100,
       "estimated_tokens_delta":100,"session_id":"s","uuid":"u"}"#
  TestM.assertEqual (parseEvents thinkingTokens).size 0
    (msg := "a thinking-token counter is not something the agent said")
  -- These two land *after* the model's closing prose. Kept, they are what a view pinned to the
  -- bottom of a conversation shows instead of the answer.
  let postTurn :=
    r#"{"type":"system","subtype":"post_turn_summary","summarizes_uuid":"u",
       "status_category":"review_ready","status_detail":"done","needs_action":""}"#
  TestM.assertEqual (parseEvents postTurn).size 0 (msg := "nor is a summary of the turn")
  let taskSummary :=
    r#"{"type":"system","subtype":"task_summary","detail":"Running python3 -c …"}"#
  TestM.assertEqual (parseEvents taskSummary).size 0 (msg := "nor a label for the CLI's own UI")
  let commands :=
    r#"{"type":"system","subtype":"commands_changed","commands":[{"name":"compact"}]}"#
  TestM.assertEqual (parseEvents commands).size 0
    (msg := "nor the slash commands the CLI has loaded")

@[test]
def aSystemLineThatSaysSomethingSurvives : Test := do
  -- The filter is a list of names, not "everything but init", because this is a `system` line
  -- too and it is the only record that a tool was refused.
  let denied :=
    r#"{"type":"system","subtype":"permission_denied","tool_name":"Bash",
       "tool_use_id":"toolu_01","message":"This command requires approval"}"#
  match parseEvent denied with
  | some (.system sub) => TestM.assertEqual sub "permission_denied" (msg := "kept, as itself")
  | _ => TestM.fail "expected a system event"
  -- And `init` is still lifted out of the `system` family, as it always was.
  let init := r#"{"type":"system","subtype":"init","session_id":"abc","model":"claude-opus-5"}"#
  match parseEvent init with
  | some (.init sid model) => do
    TestM.assertEqual sid "abc" (msg := "the session id the CLI settled on")
    TestM.assertEqual model "claude-opus-5" (msg := "and the model it is running")
  | _ => TestM.fail "expected an init event"

@[test]
def theCliSOwnStateIsNotAnEvent : Test := do
  -- Two top-level line types this project has no rendering for; both would otherwise show up in
  -- the middle of a conversation as `{"type":"unknown","event_type":"…"}`.
  TestM.assertEqual (parseEvents r#"{"type":"active_goal","value":null}"#).size 0
    (msg := "whether a /goal is set is not conversation")
  TestM.assertEqual
    (parseEvents r#"{"type":"autocompact_state","value":{"enabled":true}}"#).size 0
    (msg := "nor is how close the context window is to compaction")
  -- A type nobody has catalogued is still reported rather than dropped: a filter that swallows
  -- everything it does not recognise is how a real event goes missing without a trace.
  match parseEvent r#"{"type":"something_new_entirely"}"# with
  | some (.unknown t) => TestM.assertEqual t "something_new_entirely" (msg := "still surfaced")
  | _ => TestM.fail "expected an unknown event"
