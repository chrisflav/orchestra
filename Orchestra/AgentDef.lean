import Orchestra.StreamFormat
import Orchestra.Config
import Lean.Data.Json

open Lean (Json)
open Orchestra.StreamFormat

namespace Orchestra

/-- Describes how to invoke and communicate with a specific coding agent backend. -/
structure AgentDef where
  /-- The executable name (e.g., "claude"). -/
  command : String
  /-- Filesystem paths the agent needs inside the sandbox. -/
  sandboxPaths : SandboxPaths
  /-- Set up agent-specific infrastructure before launch (e.g., write MCP config files).
      Receives the MCP server port, optional model override, and optional appended system prompt.
      Returns a context string (passed to buildArgs, extractSessionId, and cleanup)
      and any extra sandbox env vars. -/
  setupMcp : UInt16 → Option String → Option String → IO (String × Array (String × Option String))
  /-- Build command-line args for a specific invocation.
      Receives: context string from setupMcp, plugin directories, sub-agent name,
      model override, appended system prompt, session ID to resume, budget in USD,
      and the user prompt. -/
  buildArgs : String → Array String → Option String → Option String → Option String → Option String → Float → String
            → Array String
  /-- Parse one line of the agent's stdout stream output.
      Returns `none` for events that should be suppressed. -/
  parseOutputLine : String → Option Event
  /-- Try to extract the session ID after the run.
      Used for agents that don't emit the session ID in the output stream.
      Receives the context string from setupMcp. -/
  extractSessionId : String → IO (Option String)
  /-- Clean up any resources created by setupMcp. -/
  cleanup : String → IO Unit
  /-- Return true if the agent exited because it hit a usage/quota limit.
      Receives the process exit code and the full stderr content. -/
  isUsageLimitError : UInt32 → String → Bool
  /-- Map an authentication source to environment variable names for this backend.
      Called to determine which environment variables to inject when a specific auth source is selected.
      Authentication kinds not supported by the backend produce an empty array. -/
  envVarsOfAuthSource : AuthSource → Array (String × String)
  /-- Query the provider API to find when the usage limit for the given auth source will reset.
      Returns an ISO 8601 UTC timestamp string if the limit is currently active and the backend
      supports this query. Returns `none` if the backend does not support proactive limit checking
      or if no limit is currently active. -/
  queryUsageReset : AuthSource → IO (Option String) := fun _ => pure none
  /-- Proactively query all current usage limits for the given auth source before starting a task.
      Checks every limit type the backend exposes (e.g. per-session, weekly all-models,
      weekly per-model).  Returns `some resetTime` (ISO 8601 UTC) if *any* limit is currently
      exhausted, `none` when there is sufficient headroom. -/
  checkCurrentUsageLimits : AuthSource → IO (Option String) := fun _ => pure none

namespace AgentDef

/-- Case-insensitive substring check. -/
def containsCI (haystack needle : String) : Bool :=
  (haystack.toLower.splitOn needle.toLower).length > 1

/-- Shared usage-limit detection patterns used by all backends. -/
def stdUsageLimitError (exitCode : UInt32) (stderr : String) : Bool :=
  exitCode != 0 &&
    let s := stderr.toLower
    containsCI s "usage limit" ||
    containsCI s "rate limit exceeded" ||
    containsCI s "exceeded your" ||
    containsCI s "insufficient credits" ||
    containsCI s "credit balance"

end AgentDef

end Orchestra
