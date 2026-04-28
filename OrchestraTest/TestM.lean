import Lean
import Orchestra

/-!
# Test monad and `@[test]` attribute

This module provides:
- `TestM`: a monad for writing tests, combining a reader for `AppConfig` access
  and a state monad for accumulating results.
- `Test`: an abbreviation for `TestM Unit`.
- `TestM.assert` / `TestM.fail`: basic assertion primitives.
- `@[test]`: an attribute that registers a declaration of type `Test` so that
  the test driver can discover and run it automatically.
- `Orchestra.runTests`: runs all registered tests and prints a summary.
-/

namespace Orchestra

open Lean

/-- State accumulated during a single test run. -/
structure TestState where
  /-- Number of assertions that passed. -/
  passed : Nat := 0
  /-- Accumulated failure messages (empty means the test passed). -/
  failures : Array String := #[]

/-- A monad for writing tests.
    It provides `assert` and `fail` commands and read-only access to the
    `AppConfig` via the `ReaderT` layer. -/
abbrev TestM := ReaderT AppConfig (StateT TestState IO)

/-- Short alias; test declarations should have type `Test`. -/
abbrev Test := TestM Unit

namespace TestM

/-- Record a failure with the given message without stopping the test. -/
def fail (msg : String) : TestM Unit :=
  modify fun s => { s with failures := s.failures.push msg }

/-- Assert that `cond` holds.  Records a failure with `msg` if it does not,
    or increments the passed-assertion counter if it does. -/
def assert (cond : Bool) (msg : String := "assertion failed") : TestM Unit := do
  if cond then
    modify fun s => { s with passed := s.passed + 1 }
  else
    fail msg

/-- Assert that two values are equal, printing both on failure. -/
def assertEqual [DecidableEq α] [Repr α] (a b : α)
    (msg : String := "") : TestM Unit :=
  if a == b then
    modify fun s => { s with passed := s.passed + 1 }
  else
    fail (if msg.isEmpty
      then s!"expected {repr a} = {repr b}"
      else s!"{msg}: expected {repr a} = {repr b}")

end TestM

/-! ## Global test registry -/

/-- Global registry of `(name, test)` pairs populated at module-init time
    by `@[test]` initializers.  Never mutate this directly; use
    `registerTest` instead. -/
initialize testRegistryRef : IO.Ref (Array (String × Test)) ←
  IO.mkRef #[]

/-- Register a test by name.  Called automatically by the `@[test]` attribute;
    you normally do not need to call this directly. -/
def registerTest (name : String) (t : Test) : IO Unit :=
  testRegistryRef.modify (· ++ #[(name, t)])

/-! ## `@[test]` attribute -/

private def testAttrImpl : AttributeImpl where
  name  := `test
  descr := "Register a declaration of type `Test` as a test case."
  applicationTime := .afterCompilation
  add := fun declName _stx kind => do
    if kind != .global then
      throwError "@[test] must be applied to top-level (global) declarations"
    -- Build the initializer body: Orchestra.registerTest "declName" declName
    let body := mkApp2 (mkConst ``registerTest) (mkStrLit declName.toString)
                       (mkConst declName)
    -- Declare an auxiliary `IO Unit` function that registers the test,
    -- then tag it with `@[init]` so it runs when the module is imported.
    let auxName := declName ++ `_testRegister
    let ioUnit  := mkApp (mkConst ``IO) (mkConst ``Unit)
    let auxDecl : Declaration := .defnDecl {
      name        := auxName
      levelParams := []
      type        := ioUnit
      value       := body
      hints       := .opaque
      safety      := .safe
    }
    addAndCompile auxDecl
    -- Register the auxiliary as an `@[init]`-style initializer
    match regularInitAttr.setParam (← getEnv) auxName .anonymous with
    | .ok env' => setEnv env'
    | .error e => throwError e

initialize registerBuiltinAttribute testAttrImpl

/-! ## Test runner -/

/-- Run all tests that have been registered with `@[test]`, loading the
    `AppConfig` from `configPath` (defaults to `~/.agent/config.json`).
    Returns `true` if every test passed, `false` otherwise. -/
def runTests (configPath : Option System.FilePath := none) : IO Bool := do
  let cfgResult ← try
    let cfg ← loadAppConfig configPath
    pure (some cfg)
  catch e =>
    IO.eprintln s!"[orchestra-test] warning: could not load AppConfig: {e.toString}"
    IO.eprintln   "[orchestra-test] running tests with a default (empty) config"
    pure none
  let cfg := cfgResult.getD { appId := 0, privateKeyPath := "" }
  let tests ← testRegistryRef.get
  if tests.isEmpty then
    IO.println "[orchestra-test] No tests registered."
    return true
  let mut allPassed := true
  let mut totalPassed := 0
  let mut totalFailed := 0
  for (name, test) in tests do
    let (_, state) ← (test.run cfg).run {}
    if state.failures.isEmpty then
      IO.println s!"  ✓ {name} ({state.passed} assertions)"
      totalPassed := totalPassed + 1
    else
      allPassed := false
      totalFailed := totalFailed + 1
      IO.println s!"  ✗ {name}"
      for msg in state.failures do
        IO.println s!"    - {msg}"
  IO.println ""
  IO.println s!"Results: {totalPassed} passed, {totalFailed} failed \
               out of {tests.size} tests."
  return allPassed

end Orchestra
