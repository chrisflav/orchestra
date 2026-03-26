import Test.TestM
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
