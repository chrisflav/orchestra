/-
  Orchestra test driver.

  Import all modules that contain `@[test]` declarations here so that their
  initializers fire and populate the global test registry, then call
  `Orchestra.runTests`.

  Add an import for each new test file you create.
-/
import OrchestraTest

def main : IO UInt32 := do
  let passed ← Orchestra.runTests
  (← IO.getStdout).flush
  -- Exited rather than returned, for the same reason the queue daemon exits (`Daemon.run`): a
  -- test that started an MCP server leaves a listening socket behind, and the TCP API this is
  -- built on has no way to close one. The runtime then waits on it forever, and a suite that has
  -- printed every result still never ends.
  IO.Process.exit (if passed then 0 else 1)
