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
  setupMcp mcp _ _ := do
    let (cmd, cmdArgs) := mcp.stdioCommand
    let mcpConfig := Json.mkObj [("mcpServers", Json.mkObj [
      ("agent", Json.mkObj [
        ("command", .str cmd),
        ("args", .arr (cmdArgs.map Json.str))
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
  buildStreamArgs o := Id.run do
    -- `--input-format stream-json` is what makes the process read turns from stdin instead of
    -- taking one prompt and exiting, and the CLI accepts it only alongside `--print`. There is
    -- no `-p <prompt>` here for the same reason: the prompt arrives on stdin, one JSON line per
    -- turn, for as long as the session lives.
    let mut args : Array String := #[
      "--print", "--input-format", "stream-json", "--output-format", "stream-json", "--verbose",
      "--dangerously-skip-permissions", "--mcp-config", o.mcpContext,
      -- Bounds the whole session, not a turn — see `StreamOptions.budget`.
      "--max-budget-usd", s!"{o.budget}"
    ]
    for p in o.pluginDirs do
      args := args.push "--plugin-dir" |>.push p
    if let some name := o.subAgent then
      args := args.push "--agent" |>.push name
    if let some m := o.model then
      args := args.push "--model" |>.push m
    if let some content := o.systemPrompt then
      args := args.push "--append-system-prompt" |>.push content
    if o.partialMessages then
      args := args.push "--include-partial-messages"
    -- Resuming names a session that already exists, so there is nothing left to assign: the two
    -- are alternatives, and passing both would be asking the CLI to call one session two things.
    if let some sid := o.resume then
      args := args.push "--resume" |>.push sid
    else if let some sid := o.sessionId then
      args := args.push "--session-id" |>.push sid
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
  parseOutputLine := StreamFormat.parseEvents
  extractSessionId _ := pure none
  cleanup path := try IO.FS.removeFile (System.FilePath.mk path) catch _ => pure ()
  isUsageLimitError := stdUsageLimitError
  envVarsOfAuthSource src := match src.kind with
    -- The plan travels with the token, because the token cannot state it for itself.
    --
    -- Claude Code checks a model's entitlement against the subscription it is holding, and it
    -- learns that subscription from the account profile it fetches when a person runs `/login`.
    -- A long-lived `claude setup-token` — which is what an `oauth_token` source carries — has
    -- inference scope and nothing else, so that profile is never fetched and the client ends up
    -- holding no plan at all. A check that cannot confirm the subscription covers a model then
    -- fails closed, which is how a Max account gets told that Fable, a standard part of that
    -- plan, requires usage credits (anthropics/claude-code#79597). The server grants the very
    -- same token Fable perfectly well; it is the client refusing, in the plan's name.
    --
    -- In this mode the client reads the plan and the rate-limit tier from the environment
    -- instead of from a profile, precisely because there is no profile to read — so saying it
    -- here is the whole fix.
    --
    -- Fixed rather than configurable: every account orchestra runs an `oauth_token` on is a Max
    -- 20x subscription, and this is a fact about them, not a preference. It also grants nothing.
    -- Each request is still authorised and priced by the server against the token, so the value
    -- can only make the client's local guess right or wrong — never buy access to anything. If
    -- an account on some other plan is ever configured here, this is the line to revisit.
    | .oauthToken token => #[("CLAUDE_CODE_OAUTH_TOKEN", token),
                             ("CLAUDE_CODE_SUBSCRIPTION_TYPE", "max"),
                             ("CLAUDE_CODE_RATE_LIMIT_TIER", "default_claude_max_20x")]
    | .apiKey key baseUrl =>
      let vars := #[("ANTHROPIC_API_KEY", key)]
      match baseUrl with
      | some url => vars.push ("ANTHROPIC_BASE_URL", url)
      | none => vars

end Orchestra.AgentDef
