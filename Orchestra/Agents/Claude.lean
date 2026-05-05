import Orchestra.AgentDef
import Orchestra.StreamFormat
import Orchestra.UsageLimits
import Lean.Data.Json

open Lean (Json)

namespace Orchestra.AgentDef

/-- Run curl with the given args; return (stdout, exit code). -/
private def runCurl (args : Array String) : IO (String × UInt32) := do
  let child ← IO.Process.spawn {
    cmd  := "curl"
    args := args
    stdout := .piped
    stderr := .null
    stdin  := .null
  }
  let out ← child.stdout.readToEnd
  let code ← child.wait
  return (out, code)

/-- Find the value of an HTTP response header in curl `-D -` output (case-insensitive). -/
private def findHeader (output : String) (name : String) : Option String :=
  output.splitOn "\n" |>.findSome? fun line =>
    if line.toLower.startsWith (name.toLower ++ ":") then
      some (line.drop (name.length + 1)).trimAscii.toString
    else none

/-- Return the earliest reset timestamp among all exhausted rate-limit buckets, or `none`
    if every bucket still has remaining capacity.  Checks the standard Anthropic headers for
    requests, tokens, input-tokens, and output-tokens, plus any model-specific variants
    (e.g. weekly Sonnet / weekly all-models limits). -/
private def exhaustedResetTime (headers : String) : Option String :=
  let buckets := [
    "requests", "tokens", "input-tokens", "output-tokens"
  ]
  -- Collect the reset times of all exhausted standard buckets.
  let standardResets := buckets.filterMap fun bucket =>
    let remKey := s!"anthropic-ratelimit-{bucket}-remaining"
    let rstKey := s!"anthropic-ratelimit-{bucket}-reset"
    match findHeader headers remKey with
    | none => none
    | some rem =>
      if rem.trimAscii.toString == "0" then findHeader headers rstKey
      else none
  -- Also capture any model-specific limit headers (weekly Sonnet-only, weekly all-models).
  -- These follow the pattern: anthropic-ratelimit-<model-slug>-tokens-remaining
  let modelSpecific := headers.splitOn "\n" |>.filterMap fun line =>
    let lower := line.toLower
    if lower.startsWith "anthropic-ratelimit-" && lower.contains "-remaining:" then
      let remStr := (lower.splitOn "-remaining:" |>.getD 1 "").trimAscii.toString
      if remStr == "0" then
        let resetLine := line.replace "-remaining:" "-reset:"
        let resetVal := (resetLine.splitOn "-reset:" |>.getD 1 "").trimAscii.toString
        if resetVal.isEmpty then none else some resetVal
      else none
    else none
  let allResets := standardResets ++ modelSpecific
  -- Return the earliest reset time (lexicographic ISO 8601 comparison is correct).
  allResets.foldl (fun acc t =>
    match acc with
    | none   => some t
    | some a => some (if a ≤ t then a else t)) none

/-- Proactively query the Anthropic API to check whether any usage limit is currently
    exhausted.  Makes a minimal probe request (1 output token) and inspects the
    rate-limit response headers for all limit types:
      • per-minute request / token limits
      • weekly all-models limits
      • weekly per-model limits (e.g. Sonnet-only)
    A HTTP 429 response means the limit is already exceeded right now.
    A 200 with any remaining-count of 0 means we are at the edge.
    Returns `some resetTime` if the auth source should be blocked, `none` otherwise. -/
private def queryClaudeUsageLimits (src : AuthSource) : IO (Option String) := do
  let (authHeader, baseUrl) := match src.kind with
    | .oauthToken token => (s!"Authorization: Bearer {token}", "https://api.anthropic.com")
    | .apiKey key mBase => (s!"x-api-key: {key}", mBase.getD "https://api.anthropic.com")
  -- Use claude-haiku for the probe: cheapest model, minimises cost and quota impact.
  let body := "{\"model\":\"claude-haiku-4-5-20251001\",\"max_tokens\":1,\"messages\":[{\"role\":\"user\",\"content\":\".\"}]}"
  let (output, _) ← runCurl #[
    "-s", "-D", "-", "-o", "/dev/null",
    "-X", "POST",
    "-H", authHeader,
    "-H", "anthropic-version: 2023-06-01",
    "-H", "content-type: application/json",
    "-d", body,
    s!"{baseUrl}/v1/messages"
  ]
  -- Parse HTTP status from the first line of the header dump.
  let statusLine := (output.splitOn "\r\n").headD ((output.splitOn "\n").headD "")
  let httpStatus := (statusLine.splitOn " ").getD 1 ""
  match httpStatus with
  | "429" =>
    -- Currently rate-limited: pick the earliest reset from the response headers.
    let resetTime :=
      (exhaustedResetTime output).orElse (fun _ => findHeader output "retry-after")
        |>.orElse (fun _ => findHeader output "x-ratelimit-reset")
    match resetTime with
    | some t => return some t
    | none   => return some (← UsageLimits.estimateResetTime)
  | "200" =>
    -- Request succeeded; check whether any bucket is already at zero.
    return exhaustedResetTime output
  | _ =>
    -- Unexpected error (auth failure, network issue, etc.) — don't block.
    return none

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
  checkCurrentUsageLimits := queryClaudeUsageLimits

end Orchestra.AgentDef
