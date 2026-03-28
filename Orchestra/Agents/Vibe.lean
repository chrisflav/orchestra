import Orchestra.AgentDef
import Orchestra.StreamFormat
import Lean.Data.Json

open Lean (Json)
open Orchestra.StreamFormat

namespace Orchestra.AgentDef

private def jStr' (j : Json) (key : String) : String :=
  j.getObjValAs? String key |>.toOption |>.getD ""

private def jVal' (j : Json) (key : String) : Option Json :=
  j.getObjVal? key |>.toOption

private def jArr' (j : Json) (key : String) : Option (Array Json) :=
  match j.getObjVal? key with
  | .ok (.arr a) => some a
  | _ => none

/-- Parse one line of vibe's `--output streaming` (newline-delimited LLMMessage JSON)
    into a `StreamFormat.Event`. Returns `none` for suppressed messages.

    Vibe's log format uses role-tagged JSON objects:
    - "assistant": LLM response, with optional `tool_calls` array and/or `content`/`reasoning_content`
    - "tool": tool result; content may be wrapped in `<tool_error>…</tool_error>` on failure
    - "system"/"user": the initial system/user prompts — suppressed from display
-/
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
        -- Tool calls: `tool_calls` is a JSON array of {id, function: {name, arguments}} objects
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
      -- Tool results: strip `<tool_error>…</tool_error>` wrapper and route to stderr
      let raw := jStr' json "content"
      let (stdout, stderr) :=
        if raw.startsWith "<tool_error>" then
          let inner := (raw.drop "<tool_error>".length).toString
          let content := match inner.splitOn "</tool_error>" with
            | head :: _ => head
            | _         => inner
          ("", content)
        else
          (raw, "")
      if stdout.isEmpty && stderr.isEmpty then none
      else some (.toolResult stdout stderr)
    | "system" | "user" =>
      -- Suppress the initial system/user prompt messages; they are not agent events
      none
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
    homeRw  := [".gitconfig", ".config/gh", ".config/git", ".vibe"]
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
    let configWithMcp := vibeConfigToml port model baseConfig
    IO.FS.writeFile (vibeHomePath / "config.toml") configWithMcp
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
    -- Use the explicitly requested sub-agent if provided
    if let some name := subAgent then
      args := args.push "--agent" |>.push name
    else
      -- Default to lean-auto-approve agent for auto-approval of edits
      args := args.push "--agent" |>.push "lean-auto-approve"
      -- If a custom system prompt is provided, also register the task agent
      if systemPrompt.isSome then
        args := args.push "task"
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

end Orchestra.AgentDef
