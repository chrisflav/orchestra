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
  /-- Build command-line args for an interactive (TUI) invocation.
      Same parameters as `buildArgs` minus the prompt; omits headless-only flags
      like `--print` and `--output-format=stream-json`. -/
  buildInteractiveArgs : String → Array String → Option String → Option String → Option String → Option String → Float
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
  /-- Whether two tasks on this backend may run at the same time in one daemon.

      False for backends whose CLI keeps per-run state at a fixed, process-global location —
      a hard-coded port, or a config file under `$HOME` that `setupMcp` overwrites and
      `cleanup` restores. Two concurrent runs then read each other's configuration, so an
      agent can end up talking to another task's MCP server and acting on its repository and
      issue. The queue daemon runs such backends exclusively; see `Main.queueStartHandler`. -/
  parallelSafe : Bool := true

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
