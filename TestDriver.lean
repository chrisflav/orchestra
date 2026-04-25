/-
  Orchestra test driver.

  Import all modules that contain `@[test]` declarations here so that their
  initializers fire and populate the global test registry, then call
  `Orchestra.runTests`.

  Add an import for each new test file you create.
-/
import Test

def main : IO UInt32 := do
  let passed ← Orchestra.runTests
  return if passed then 0 else 1
