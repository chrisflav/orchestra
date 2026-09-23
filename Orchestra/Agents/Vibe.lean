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

/-- A string field that may be absent, where absent and empty mean the same thing. -/
private def jStrOpt' (j : Json) (key : String) : Option String :=
  match j.getObjValAs? String key with
  | .ok s => if s.isEmpty then none else some s
  | _     => none

private def jArr' (j : Json) (key : String) : Option (Array Json) :=
  match j.getObjVal? key with
  | .ok (.arr a) => some a
  | _ => none

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
              -- `id` on a tool call and `tool_call_id` on the message that answers it are the
              -- OpenAI message shape vibe speaks. Read rather than required: a release that
              -- stops sending them leaves the pairing unknown, which is what `none` says.
              some (.assistant (.toolUse name input (jStrOpt' tc "id")))
        match toolCallEvent with
        | some e => some e
        | none =>
          -- Plain text content
          let content := jStr' json "content"
          if content.isEmpty then none
          else some (.assistant (.text content))
    | "tool" =>
      some (.toolResult (jStr' json "content") "" (jStrOpt' json "tool_call_id"))
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

/-- A TOML basic string: the two characters that end it or start an escape, escaped.

    Written out rather than asserted about the caller. What goes in here is a shell command line
    assembled from an operator's `mcp_host`, and a comment claiming it can never contain a quote
    is a claim that has to stay true through every later change to how that command is built. -/
private def tomlBasicString (s : String) : String :=
  "\"" ++ (s.replace "\\" "\\\\" |>.replace "\"" "\\\"") ++ "\""

/-- Produce a config.toml for the temp VIBE_HOME by injecting the MCP server and optional
    model override into the user's existing config. -/
private def vibeConfigToml (mcp : Exec.McpEndpoint) (model : Option String) (base : String) : String :=
  let (cmd, cmdArgs) := mcp.stdioCommand
  let renderedArgs := String.intercalate ", " (cmdArgs.toList.map tomlBasicString)
  let mcpEntry :=
    "[[mcp_servers]]\n" ++
    "name = \"agent\"\n" ++
    "transport = \"stdio\"\n" ++
    s!"command = {tomlBasicString cmd}\n" ++
    s!"args = [{renderedArgs}]\n"
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
    homeRox := [".local"]
    homeRw  := [".gitconfig", ".config/gh", ".config/git"]
    homeRwx := [".elan", ".cache"]
  }
  setupMcp mcp model systemPrompt := do
    let ts ← uniqueToken
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
    IO.FS.writeFile (vibeHomePath / "config.toml") (vibeConfigToml mcp model baseConfig)
    -- If a system prompt is provided, write a custom agent profile and prompt file
    if let some sp := systemPrompt then
      IO.FS.createDir (vibeHomePath / "agents")
      IO.FS.createDir (vibeHomePath / "prompts")
      IO.FS.writeFile (vibeHomePath / "agents" / "task.toml")
        "safety = \"yolo\"\nauto_approve = true\nsystem_prompt_id = \"task\"\n"
      IO.FS.writeFile (vibeHomePath / "prompts" / "task.md") sp
    -- Pass VIBE_HOME and MISTRAL_API_KEY into the sandbox env
    let mistralKey ← IO.getEnv "MISTRAL_API_KEY"
    -- The whole directory, writable: `VIBE_HOME` names it in the agent's environment, and vibe
    -- writes its session state back into it. Under a backend that runs the agent elsewhere it has
    -- to be carried there, or `VIBE_HOME` points at nothing.
    return (vibeHome, #[
      ("VIBE_HOME", some vibeHome),
      ("MISTRAL_API_KEY", mistralKey)
    ], #[{ path := vibeHome, access := .rwx, from_ := .orchestra }])
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
  buildInteractiveArgs _ctx _pluginDirs subAgent _model systemPrompt resume _budget := Id.run do
    let mut args : Array String := #[]
    let agentName := match subAgent with
      | some n => some n
      | none   => if systemPrompt.isSome then some "task" else none
    if let some name := agentName then
      args := args.push "--agent" |>.push name
    if let some sid := resume then
      args := args.push "--resume" |>.push sid
    return args
  parseOutputLine := fun line => StreamFormat.one (vibeParseOutputLine line)
  extractSessionId := vibeExtractSessionId
  cleanup _ := pure ()
  isUsageLimitError exitCode stderr :=
    stdUsageLimitError exitCode stderr ||
    (exitCode != 0 && containsCI stderr "quota exceeded")
  envVarsOfAuthSource src := match src.kind with
    | .apiKey key _ => #[("MISTRAL_API_KEY", key)]
    | _ => #[]

end Orchestra.AgentDef
