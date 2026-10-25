/-
  Orchestra test driver.

  Import all modules that contain `@[test]` declarations here so that their
  initializers fire and populate the global test registry, then call
  `Orchestra.runTests`.

  Add an import for each new test file you create.
-/
import OrchestraTest

def main (args : List String) : IO UInt32 := do
  -- The landrun backend re-executes this binary as `__landlock-scope` (`Orchestra.Exec.Scope`),
  -- and the tests that run it for real run it from here.
  if let some code ← Orchestra.Exec.Scope.handle? args then return code
  let passed ← Orchestra.runTests
  (← IO.getStdout).flush
  -- Exited rather than returned, for the same reason the queue daemon exits (`Daemon.run`): a
  -- test that started an MCP server leaves a listening socket behind, and the TCP API this is
  -- built on has no way to close one. The runtime then waits on it forever, and a suite that has
  -- printed every result still never ends.
  IO.Process.exit (if passed then 0 else 1)
