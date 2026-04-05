import Orchestra.AgentDef
import Orchestra.StreamFormat
import Lean.Data.Json

open Lean (Json)
open Orchestra.StreamFormat

namespace Orchestra.AgentDef

private def piJStr (j : Json) (key : String) : String :=
  j.getObjValAs? String key |>.toOption |>.getD ""

private def piJVal (j : Json) (key : String) : Option Json :=
  j.getObjVal? key |>.toOption

/-- Extract plain text from a pi tool result value.
    Tool results are `{ content: [{ type: "text", text: "..." }], details: ... }`.
    Falls back to raw JSON compression if no text content is found. -/
private def piToolResultText (result : Json) : String :=
  match result.getObjVal? "content" with
  | .ok (.arr items) =>
    let texts := items.filterMap fun item =>
      match item.getObjVal? "text" with
      | .ok (.str t) => some t
      | _ => none
    let joined := String.intercalate "\n" texts.toList
    if joined.isEmpty then result.compress else joined
  | _ =>
    match result with
    | .str s => s
    | _ => result.compress

/-- Parse one line of pi's `--mode json` output (newline-delimited JSON) into a
    `StreamFormat.Event`. Returns `none` for events that should be suppressed. -/
private def piParseOutputLine (line : String) : Option Event :=
  match (Json.parse line.trimAscii.toString).toOption with
  | none => none
  | some json =>
    match piJStr json "type" with
    | "session" =>
      -- First line: {"type":"session","version":3,"id":"uuid","cwd":"..."}
      some (.init (piJStr json "id") "")
    -- Lifecycle events surfaced for debug visibility
    | "agent_start" => some (.unknown "agent_start")
    | "turn_start"  => some (.unknown "turn_start")
    | "turn_end"    => some (.unknown "turn_end")
    | "message_update" =>
      -- Streaming assistant content deltas
      let ame := piJVal json "assistantMessageEvent" |>.getD (Json.mkObj [])
      match piJStr ame "type" with
      | "text_delta" =>
        let delta := piJStr ame "delta"
        if delta.isEmpty then none else some (.assistant (.text delta))
      | "thinking_delta" =>
        let delta := piJStr ame "delta"
        if delta.isEmpty then none else some (.assistant (.thinking delta))
      | _ => none
    | "tool_execution_start" =>
      let toolName := piJStr json "toolName"
      let args := piJVal json "args" |>.getD (Json.mkObj [])
      some (.assistant (.toolUse toolName args))
    | "tool_execution_end" =>
      let result := piJVal json "result" |>.getD (Json.mkObj [])
      let isError := json.getObjValAs? Bool "isError" |>.toOption |>.getD false
      let text := piToolResultText result
      if isError then
        some (.toolResult "" text)
      else if text.isEmpty then
        none
      else
        some (.toolResult text "")
    | "agent_end" =>
      -- Use the number of messages in the transcript as a proxy for numTurns
      let msgCount := match json.getObjVal? "messages" with
        | .ok (.arr a) => some a.size
        | _ => none
      some (.result .success msgCount none none "")
    | "auto_retry_start" =>
      let attempt := json.getObjValAs? Nat "attempt" |>.toOption |>.getD 0
      let msg     := piJStr json "errorMessage"
      some (.unknown s!"auto_retry attempt={attempt} reason={msg}")
    | "compaction_start" =>
      let reason := piJStr json "reason"
      some (.unknown s!"compaction_start reason={reason}")
    | "compaction_end" =>
      let aborted := json.getObjValAs? Bool "aborted" |>.toOption |>.getD false
      let reason  := piJStr json "reason"
      some (.unknown s!"compaction_end aborted={aborted} reason={reason}")
    | _ => none

/-- The pi coding agent backend (https://github.com/badlogic/pi-mono). -/
def pi : AgentDef where
  command := "pi"
  sandboxPaths := {
    rox        := ["/usr", "/lib", "/lib64", "/bin", "/sbin", "/nix"]
    ro         := ["/etc", "/run", "/dev", "/proc", "/sys"]
    rw         := ["/dev/null"]
    homeRox    := [".elan", ".cache", ".local"]
    homeRw     := [".pi", ".gitconfig", ".config/gh", ".config/git"]
    extraPorts := [11434]  -- Ollama
  }
  -- Pi does not have native MCP support.  Return an empty context and inject
  -- PI_SKIP_VERSION_CHECK so the sandbox startup is quiet.
  setupMcp _port _model _systemPrompt :=
    return ("", #[("PI_SKIP_VERSION_CHECK", some "1")])
  buildArgs _ctx _pluginDirs _subAgent model systemPrompt resume _budget prompt := Id.run do
    -- --print makes pi non-interactive (process prompt and exit);
    -- without it pi starts in interactive mode and exits immediately when stdin is /dev/null.
    let mut args : Array String := #["--print", "--mode", "json"]
    if let some m := model then
      args := args.push "--model" |>.push m
    if let some content := systemPrompt then
      args := args.push "--append-system-prompt" |>.push content
    if let some sid := resume then
      args := args.push "--session" |>.push sid
    return args.push prompt
  parseOutputLine := piParseOutputLine
  -- The session UUID is captured from the "session" header line emitted to stdout,
  -- so there is nothing left to extract after the run.
  extractSessionId _ := pure none
  cleanup _ := pure ()
  isUsageLimitError := stdUsageLimitError
  -- Map an API key auth source to ANTHROPIC_API_KEY (pi's most common provider).
  -- Users relying on other providers should set the appropriate env var directly.
  envVarsOfAuthSource src := match src.kind with
    | .apiKey key baseUrl =>
      let vars := #[("ANTHROPIC_API_KEY", key)]
      match baseUrl with
      | some url => vars.push ("ANTHROPIC_BASE_URL", url)
      | none => vars
    | _ => #[]

end Orchestra.AgentDef
