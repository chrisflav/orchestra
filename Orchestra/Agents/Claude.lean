import Orchestra.AgentDef
import Orchestra.StreamFormat
import Lean.Data.Json

open Lean (Json)

namespace Orchestra.AgentDef

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

end Orchestra.AgentDef
