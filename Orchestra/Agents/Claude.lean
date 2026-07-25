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
    homeRox := [".local"]
    homeRw  := [".claude", ".claude.json", ".gitconfig",
                ".config/claude", ".config/gh", ".config/git"]
    -- Both need write *and* execute, not the read+execute the other home paths get:
    --   .elan  — installs toolchains and then runs them from there.
    --   .cache — build caches are written, not just read. `lake exe cache get` populates it, and
    --            read-only was only ever useful on a machine something else had warmed; a fresh
    --            container starts empty, so nothing could cache anything at all.
    homeRwx := [".elan", ".cache"]
  }
  setupMcp port _ _ := do
    let mcpConfig := Json.mkObj [("mcpServers", Json.mkObj [
      ("agent", Json.mkObj [
        ("command", .str "nc"),
        ("args", .arr #[.str "127.0.0.1", .str (toString port)])
      ])
    ])]
    let ts ← uniqueToken
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
  buildInteractiveArgs mcpConfigPath pluginDirs subAgent model systemPrompt resume budget := Id.run do
    let mut args : Array String := #[
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
    return args
  goalArgs goal :=
    -- What `/goal <condition>` does inside a Claude Code session is register one session-scoped
    -- `Stop` hook of type `prompt` carrying the condition: on every attempt to stop, a second
    -- model call judges the condition and sends the agent back to work until it holds. That is
    -- reachable from the command line through `--settings`, which takes a settings JSON string
    -- as readily as a path, so the goal travels as one argument with no file to write or clean
    -- up (and none to leak into a second concurrent run's sandbox).
    --
    -- Typing the slash command instead is not an option here: headless runs get exactly one
    -- prompt (`-p`), so `/goal ...` would have to *be* that prompt and there would be no turn
    -- left to state the task in.
    let stopHook := Json.mkObj [("type", .str "prompt"), ("prompt", .str goal)]
    let matcher := Json.mkObj [("matcher", .str ""), ("hooks", Json.arr #[stopHook])]
    let settings := Json.mkObj [("hooks", Json.mkObj [("Stop", Json.arr #[matcher])])]
    some #["--settings", settings.compress]
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
