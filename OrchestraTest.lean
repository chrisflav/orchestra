import Lean.Data.Json
import Orchestra.Config
import Orchestra.Listener
import Orchestra.Queue

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Listener

private def assertEq [BEq α] [Repr α] (desc : String) (expected actual : α) : IO Unit :=
  if expected == actual then
    IO.println s!"  PASS: {desc}"
  else do
    IO.eprintln s!"  FAIL: {desc}"
    IO.eprintln s!"    expected: {repr expected}"
    IO.eprintln s!"    actual:   {repr actual}"
    throw (.userError s!"test failed: {desc}")

private def testTaskExtraPorts : IO Unit := do
  IO.println "Task.extraPorts"
  let json ← IO.ofExcept <| Json.parse
    r#"{"upstream":"u","fork":"f","mode":"fork","prompt":"p","extra_ports":[8080,9090]}"#
  let task ← IO.ofExcept (FromJson.fromJson? json : Except String Task)
  assertEq "extra_ports parsed correctly" [8080, 9090] task.extraPorts

private def testTaskExtraPortsDefault : IO Unit := do
  IO.println "Task.extraPorts default"
  let json ← IO.ofExcept <| Json.parse
    r#"{"upstream":"u","fork":"f","mode":"fork","prompt":"p"}"#
  let task ← IO.ofExcept (FromJson.fromJson? json : Except String Task)
  assertEq "extra_ports defaults to []" [] task.extraPorts

private def testActionConfigExtraPorts : IO Unit := do
  IO.println "ActionConfig.extraPorts"
  let json ← IO.ofExcept <| Json.parse
    r#"{"upstream":"u","fork":"f","mode":"fork","prompt_template":"tmpl","extra_ports":[443,8443]}"#
  let cfg ← IO.ofExcept (FromJson.fromJson? json : Except String ActionConfig)
  assertEq "extra_ports parsed correctly" [443, 8443] cfg.extraPorts

private def testActionConfigExtraPortsDefault : IO Unit := do
  IO.println "ActionConfig.extraPorts default"
  let json ← IO.ofExcept <| Json.parse
    r#"{"upstream":"u","fork":"f","mode":"fork","prompt_template":"tmpl"}"#
  let cfg ← IO.ofExcept (FromJson.fromJson? json : Except String ActionConfig)
  assertEq "extra_ports defaults to []" [] cfg.extraPorts

private def testQueueEntryExtraPortsRoundTrip : IO Unit := do
  IO.println "QueueEntry.extraPorts round-trip"
  let entry : Queue.QueueEntry := {
    id := "test-id"
    createdAt := "2026-01-01T00:00:00Z"
    upstream := "u"
    fork := "f"
    mode := .fork
    prompt := "p"
    extraPorts := [3000, 8080]
  }
  let json := ToJson.toJson entry
  let entry' ← IO.ofExcept (FromJson.fromJson? json : Except String Queue.QueueEntry)
  assertEq "extra_ports survives round-trip" [3000, 8080] entry'.extraPorts

private def testQueueEntryExtraPortsEmpty : IO Unit := do
  IO.println "QueueEntry.extraPorts empty round-trip"
  let entry : Queue.QueueEntry := {
    id := "test-id"
    createdAt := "2026-01-01T00:00:00Z"
    upstream := "u"
    fork := "f"
    mode := .fork
    prompt := "p"
  }
  let json := ToJson.toJson entry
  let entry' ← IO.ofExcept (FromJson.fromJson? json : Except String Queue.QueueEntry)
  assertEq "empty extra_ports survives round-trip" [] entry'.extraPorts

def main : IO UInt32 := do
  let tests : List (IO Unit) := [
    testTaskExtraPorts,
    testTaskExtraPortsDefault,
    testActionConfigExtraPorts,
    testActionConfigExtraPortsDefault,
    testQueueEntryExtraPortsRoundTrip,
    testQueueEntryExtraPortsEmpty
  ]
  let mut failed := 0
  for test in tests do
    try
      test
    catch e =>
      IO.eprintln s!"  Error: {e}"
      failed := failed + 1
  if failed == 0 then
    IO.println s!"\nAll {tests.length} tests passed."
    return 0
  else
    IO.eprintln s!"\n{failed}/{tests.length} tests FAILED."
    return 1
