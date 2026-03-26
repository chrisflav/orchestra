import Orchestra.StreamFormat
import Orchestra.Config
import Lean.Data.Json

open Lean (Json)
open Orchestra.StreamFormat

namespace Orchestra

-- Local JSON helpers (mirrors private helpers in StreamFormat)

private def jStr' (j : Json) (key : String) : String :=
  j.getObjValAs? String key |>.toOption |>.getD ""

private def jVal' (j : Json) (key : String) : Option Json :=
  j.getObjVal? key |>.toOption

private def jArr' (j : Json) (key : String) : Option (Array Json) :=
  match j.getObjVal? key with
  | .ok (.arr a) => some a
  | _ => none

/-- Filesystem paths to expose inside the landrun sandbox. -/
structure SandboxPaths where
  /-- Absolute paths needing read+execute (binaries/libraries). -/
  rox : List String := []
  /-- Absolute paths needing read-only access. -/
  ro : List String := []
  /-- Absolute paths needing read-write access. -/
  rw : List String := []
  /-- Paths relative to $HOME needing read+execute. -/
  homeRox : List String := []
  /-- Paths relative to $HOME needing read-write access. -/
  homeRw : List String := []

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

namespace AgentDef

/-- Case-insensitive substring check. -/
private def containsCI (haystack needle : String) : Bool :=
  (haystack.toLower.splitOn needle.toLower).length > 1

/-- Shared usage-limit detection patterns used by all backends. -/
private def stdUsageLimitError (exitCode : UInt32) (stderr : String) : Bool :=
  exitCode != 0 &&
    let s := stderr.toLower
    containsCI s "usage limit" ||
    containsCI s "rate limit exceeded" ||
    containsCI s "exceeded your" ||
    containsCI s "insufficient credits" ||
    containsCI s "credit balance"

-- Claude

/-- The Claude coding agent backend. -/
def claude : AgentDef where
  command := "claude"
  sandboxPaths := {
    rox     := ["/usr", "/lib", "/lib64", "/bin", "/sbin", "/nix"]
    ro      := ["/etc", "/run", "/dev", "/proc", "/sys"]
    rw      := ["/dev/null"]
    homeRox := [".elan", ".cache", ".local"]
    homeRw  := [".claude", ".claude.json", ".gitconfig",
                ".config/claude", ".config/gh", ".config/git"]
  }
  setupMcp port _ _ := do
    let mcpConfig := Json.mkObj [("mcpServers", Json.mkObj [
      ("agent", Json.mkObj [
        ("command", .str "nc"),
        ("args", .arr #[.str "127.0.0.1", .str (toString port)])
      ])
    ])]
    let ts ← IO.monoNanosNow
    let path := s!"/tmp/agent-mcp-{ts}.json"
    IO.FS.writeFile (System.FilePath.mk path) mcpConfig.compress
    return (path, #[])
  buildArgs mcpConfigPath pluginDirs subAgent model systemPrompt resume budget prompt := Id.run do
    let mut args : Array String := #[
      "--print", "--output-format=stream-json", "--verbose",
      "--dangerously-skip-permissions", "--mcp-config", mcpConfigPath,
      "--max-budget-usd", s!"{budget}"
    ]
    for p in pluginDirs do
      args := args.push "--plugin-dir" |>.push p
    if let some name := subAgent then
      args := args.push "--agent" |>.push name
    if let some m := model then
      args := args.push "--model" |>.push m
    if let some content := systemPrompt then
      args := args.push "--append-system-prompt" |>.push content
    if let some sid := resume then
      args := args.push "--resume" |>.push sid
    return args.push "-p" |>.push prompt
  parseOutputLine := StreamFormat.parseEvent
  extractSessionId _ := pure none
  cleanup path := try IO.FS.removeFile (System.FilePath.mk path) catch _ => pure ()
  isUsageLimitError := stdUsageLimitError
  envVarsOfAuthSource src := match src.kind with
    | .oauthToken token => #[("CLAUDE_CODE_OAUTH_TOKEN", token)]
    | .apiKey key baseUrl =>
      let vars := #[("ANTHROPIC_API_KEY", key)]
      match baseUrl with
      | some url => vars.push ("ANTHROPIC_BASE_URL", url)
      | none => vars

-- Vibe

/-- Parse one line of vibe's `--output streaming` (newline-delimited LLMMessage JSON)
    into a `StreamFormat.Event`. Returns `none` for suppressed messages. -/
private def vibeParseOutputLine (line : String) : Option Event :=
  match (Json.parse line.trimAscii.toString).toOption with
  | none => none
  | some json =>
    match jStr' json "role" with
    | "assistant" =>
      -- Reasoning / thinking content takes priority
      let reasoning := jStr' json "reasoning_content"
      if !reasoning.isEmpty then
        some (.assistant (.thinking reasoning))
      else
        -- Tool calls
        let toolCallEvent : Option Event :=
          match jArr' json "tool_calls" with
          | none => none
          | some toolCalls =>
            match toolCalls.back? with
            | none => none
            | some tc =>
              let fn := jVal' tc "function" |>.getD (Json.mkObj [])
              let name := jStr' fn "name"
              let argsStr := jStr' fn "arguments"
              let input := (Json.parse argsStr).toOption |>.getD (Json.mkObj [])
              some (.assistant (.toolUse name input))
        match toolCallEvent with
        | some e => some e
        | none =>
          -- Plain text content
          let content := jStr' json "content"
          if content.isEmpty then none
          else some (.assistant (.text content))
    | "tool" =>
      some (.toolResult (jStr' json "content") "")
    | _ => none

/-- Read the full session ID from the most recent session log under `vibeHome/logs/session/`. -/
private def vibeExtractSessionId (vibeHome : String) : IO (Option String) := do
  let logsDir := System.FilePath.mk vibeHome / "logs" / "session"
  if !(← logsDir.pathExists) then return none
  let entries ← System.FilePath.readDir logsDir
  let sessions := entries.filter (fun e => e.fileName.startsWith "session_")
  -- Folder names are "session_YYYYMMDD_HHMMSS_{id_prefix}", so lex-max = most recent
  match sessions.toList with
  | [] => return none
  | first :: rest =>
    let latest := rest.foldl
      (fun acc e => if e.path.toString > acc.path.toString then e else acc)
      first
    let metaPath := latest.path / "meta.json"
    if !(← metaPath.pathExists) then return none
    let raw ← IO.FS.readFile metaPath
    return match (Json.parse raw).toOption with
      | none => none
      | some json => json.getObjValAs? String "session_id" |>.toOption

/-- Produce a config.toml for the temp VIBE_HOME by injecting the MCP server and optional
    model override into the user's existing config. -/
private def vibeConfigToml (serverPort : UInt16) (model : Option String) (base : String) : String :=
  let mcpEntry :=
    "[[mcp_servers]]\n" ++
    "name = \"agent\"\n" ++
    "transport = \"stdio\"\n" ++
    "command = \"nc\"\n" ++
    s!"args = [\"127.0.0.1\", \"{serverPort}\"]\n"
  let withMcp := base.replace "mcp_servers = []" mcpEntry
  match model with
  | none => withMcp
  | some m =>
    let lines := withMcp.splitOn "\n"
    if lines.any (fun l => l.startsWith "active_model = ") then
      -- Replace the existing active_model line in-place
      let updated := lines.map (fun l =>
        if l.startsWith "active_model = " then s!"active_model = \"{m}\"" else l)
      String.intercalate "\n" updated
    else
      -- Prepend the setting so it takes effect even when absent from the base config
      s!"active_model = \"{m}\"\n" ++ withMcp

/-- The Vibe (Mistral AI) coding agent backend. -/
def vibe : AgentDef where
  command := "vibe"
  sandboxPaths := {
    rox     := ["/usr", "/lib", "/lib64", "/bin", "/sbin", "/nix"]
    ro      := ["/etc", "/run", "/dev", "/proc", "/sys"]
    rw      := ["/dev/null"]
    homeRox := [".elan", ".cache", ".local"]
    homeRw  := [".gitconfig", ".config/gh", ".config/git"]
  }
  setupMcp port model systemPrompt := do
    let ts ← IO.monoNanosNow
    let vibeHome := s!"/tmp/agent-vibe-{ts}"
    let vibeHomePath := System.FilePath.mk vibeHome
    -- Create the temp VIBE_HOME
    IO.FS.createDir vibeHomePath
    -- Copy the user's existing vibe config and inject the MCP server and model
    let baseConfig ← do
      match ← IO.getEnv "HOME" with
      | some h =>
        let src := System.FilePath.mk h / ".vibe" / "config.toml"
        if ← src.pathExists then IO.FS.readFile src else pure ""
      | none => pure ""
    IO.FS.writeFile (vibeHomePath / "config.toml") (vibeConfigToml port model baseConfig)
    -- If a system prompt is provided, write a custom agent profile and prompt file
    if let some sp := systemPrompt then
      IO.FS.createDir (vibeHomePath / "agents")
      IO.FS.createDir (vibeHomePath / "prompts")
      IO.FS.writeFile (vibeHomePath / "agents" / "task.toml")
        "safety = \"yolo\"\nauto_approve = true\nsystem_prompt_id = \"task\"\n"
      IO.FS.writeFile (vibeHomePath / "prompts" / "task.md") sp
    -- Pass VIBE_HOME and MISTRAL_API_KEY into the sandbox env
    let mistralKey ← IO.getEnv "MISTRAL_API_KEY"
    return (vibeHome, #[
      ("VIBE_HOME", some vibeHome),
      ("MISTRAL_API_KEY", mistralKey)
    ])
  buildArgs _ctx _pluginDirs subAgent _model systemPrompt resume _budget prompt := Id.run do
    let mut args : Array String := #["-p", prompt, "--output", "streaming"]
    -- Use the task agent (with custom system prompt) if one was configured in setupMcp,
    -- or the explicitly requested sub-agent; otherwise let vibe default to auto-approve.
    let agentName := match subAgent with
      | some n => some n
      | none   => if systemPrompt.isSome then some "task" else none
    if let some name := agentName then
      args := args.push "--agent" |>.push name
    if let some sid := resume then
      args := args.push "--resume" |>.push sid
    return args
  parseOutputLine := vibeParseOutputLine
  extractSessionId := vibeExtractSessionId
  cleanup _ := pure ()
  isUsageLimitError exitCode stderr :=
    stdUsageLimitError exitCode stderr ||
    (exitCode != 0 && containsCI stderr "quota exceeded")
  envVarsOfAuthSource src := match src.kind with
    | .apiKey key _ => #[("MISTRAL_API_KEY", key)]
    | _ => #[]

end AgentDef

end Orchestra
