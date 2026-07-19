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
  /-- Set when the test declined to run (see `TestM.skip`). Reported separately from pass/fail
      so a test that never ran can't masquerade as a passing one. -/
  skipped : Option String := none

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

/-- Mark the test as skipped, with the reason it couldn't run. Use for tests that need an
    external resource that isn't necessarily available (see `ensureTestTaxisConfigured`); the
    test body should `return` immediately after. A skipped test is neither passed nor failed. -/
def skip (reason : String) : TestM Unit :=
  modify fun s => { s with skipped := some reason }

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

/-! ## Real-taxis-instance opt-in for taxis-backed tests

`Project.*`/`Claim.*` are backed by a real taxis HTTP API (see the "Migrate Project/Issue data
layer" tracking issue), so exercising them for real needs a real taxis instance to talk to — a
fake in-process double would miss real wire-format/validation bugs (see e.g. the `github-pr`
artifact's required `url` field, caught only by testing against the genuine server). Rather than
spawning one automatically (fragile in CI: port allocation, migrations, admin bootstrap, cleanup
on crash), these tests opt in via two environment variables and skip themselves — not fail —
when they're unset, so the rest of the suite stays green without a taxis instance around.

To run them locally: start a taxis instance (`lake exe taxis` in a taxis checkout, or
`docker compose up` in its `docker/` directory — see that repo's README) with an admin actor and
an API token for it (`POST /me/tokens` while authenticated as that actor), then:

```sh
ORCHESTRA_TEST_TAXIS_URL=http://localhost:8080 ORCHESTRA_TEST_TAXIS_TOKEN=<token> lake test
```

The token's actor must be a taxis admin — creating the `t-project`/`o-claimed`/`o-rejected`
labels on first use requires it (see `Project.ensureTaxisConfigured`'s
non-test counterpart, wired into `Main.main` the same way). -/

/-- Configure `Orchestra.Taxis` from `ORCHESTRA_TEST_TAXIS_URL`/`ORCHESTRA_TEST_TAXIS_TOKEN` if
    both are set. Returns whether it did — taxis-backed tests should check this and skip
    themselves (see this module's docs) rather than fail when it's `false`. -/
def ensureTestTaxisConfigured : IO Bool := do
  match ← IO.getEnv "ORCHESTRA_TEST_TAXIS_URL", ← IO.getEnv "ORCHESTRA_TEST_TAXIS_TOKEN" with
  | some url, some token =>
    Orchestra.Taxis.setConfig (some { url, token := some token })
    pure true
  | _, _ => pure false

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
  let mut totalSkipped := 0
  for (name, test) in tests do
    let (_, state) ← (test.run cfg).run {}
    if !state.failures.isEmpty then
      allPassed := false
      totalFailed := totalFailed + 1
      IO.println s!"  ✗ {name}"
      for msg in state.failures do
        IO.println s!"    - {msg}"
    else if let some reason := state.skipped then
      totalSkipped := totalSkipped + 1
      IO.println s!"  ○ {name} (skipped: {reason})"
    else
      IO.println s!"  ✓ {name} ({state.passed} assertions)"
      totalPassed := totalPassed + 1
  let skippedNote := if totalSkipped == 0 then "" else s!", {totalSkipped} skipped"
  IO.println ""
  IO.println s!"Results: {totalPassed} passed, {totalFailed} failed{skippedNote} \
               out of {tests.size} tests."
  return allPassed

end Orchestra
