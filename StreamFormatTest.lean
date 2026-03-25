import Lean.Data.Json
import Orchestra.StreamFormat

open Lean (Json ToJson)
open Orchestra.StreamFormat

private def fail (msg : String) : IO α := do
  IO.eprintln s!"FAIL: {msg}"
  IO.Process.exit 1

private def passTest (msg : String) : IO Unit :=
  IO.println s!"PASS: {msg}"

private def expectEq [BEq α] [Repr α] (msg : String) (expected : α) (got : α) : IO Unit := do
  if got != expected then
    fail s!"{msg}: expected {repr expected}, got {repr got}"
  else
    passTest msg

def main : IO Unit := do

  -- Test 1: Parse result event with subtype "success" → ResultSubtype.success
  let successLine := r#"{"type":"result","subtype":"success","result":"all done","num_turns":5,"duration_ms":3000,"total_cost_usd":0.01}"#
  match parseEvent successLine with
  | some (.result sub _ _ _ _) =>
    expectEq "success subtype" ResultSubtype.success sub
  | _ => fail "success: expected result event"

  -- Test 2: Parse result event with subtype "error_max_budget_usd" → ResultSubtype.errorMaxBudgetUsd
  let budgetLine := r#"{"type":"result","subtype":"error_max_budget_usd","result":""}"#
  match parseEvent budgetLine with
  | some (.result sub _ _ _ _) =>
    expectEq "error_max_budget_usd subtype" ResultSubtype.errorMaxBudgetUsd sub
  | _ => fail "error_max_budget_usd: expected result event"

  -- Test 3: Parse result event with unknown subtype → ResultSubtype.unknown "some_new_error"
  let unknownLine := r#"{"type":"result","subtype":"some_new_error","result":""}"#
  match parseEvent unknownLine with
  | some (.result sub _ _ _ _) =>
    expectEq "unknown subtype" (ResultSubtype.unknown "some_new_error") sub
  | _ => fail "unknown subtype: expected result event"

  -- Test 4: ResultSubtype serialises back to the original string values
  let toJsonStr (sub : ResultSubtype) : String := Lean.Json.compress (ToJson.toJson sub)
  expectEq "toJson success"            "\"success\""             (toJsonStr ResultSubtype.success)
  expectEq "toJson errorMaxBudgetUsd"  "\"error_max_budget_usd\"" (toJsonStr ResultSubtype.errorMaxBudgetUsd)
  expectEq "toJson unknown"            "\"my_error\""            (toJsonStr (ResultSubtype.unknown "my_error"))

  -- Test 5: Event.result round-trips through JSON (subtype preserved)
  let ev : Event := .result .success (some 3) (some 1000) none "done"
  let j := ToJson.toJson ev
  match parseEvent (Lean.Json.compress j) with
  | some (.result sub _ _ _ _) => expectEq "round-trip success" ResultSubtype.success sub
  | _ => fail "round-trip success: expected result event"

  IO.println "\nAll tests passed!"
