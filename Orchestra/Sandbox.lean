import Orchestra.AgentDef
import Orchestra.StreamFormat

namespace Orchestra.Sandbox

/-- Expand home-relative path strings to absolute FilePaths using $HOME. -/
private def expandHomePaths (rel : List String) : IO (List System.FilePath) := do
  match ← IO.getEnv "HOME" with
  | none   => return []
  | some h =>
    let home := System.FilePath.mk h
    return rel.map (fun r => home / System.FilePath.mk r)

/-- Result of launching an agent. -/
structure LaunchResult where
  exitCode      : UInt32
  sessionId     : Option String
  /-- True if the agent exited because it hit a usage or quota limit. -/
  usageLimitHit : Bool

/--
Launch the coding agent inside a landrun sandbox.
The agent backend's setupMcp hook runs before launch to configure MCP connectivity.
Returns a LaunchResult with exit code, session ID, and usage-limit flag.
-/
private def shellEscape (s : String) : String :=
  if s.any (fun c => c == ' ' || c == '"' || c == '\'' || c == '\\' || c == '$' || c == '`'
                   || c == '(' || c == ')' || c == '!' || c == '&' || c == '|'
                   || c == ';' || c == '\n' || c == '\t') then
    "'" ++ s.replace "'" "'\\''" ++ "'"
  else s

def launchAgent (agentDef : AgentDef) (repoPath : System.FilePath) (prompt : String)
    (serverPort : UInt16)
    (ghToken : String)
    (debug : Bool := false)
    (extraEnv : Array (String × Option String) := #[])
    (pluginDirs : Array String := #[])
    (subAgent : Option String := none)
    (model : Option String := none)
    (systemPrompt : Option String := none)
    (resume : Option String := none) : IO LaunchResult := do
  -- Run agent-specific MCP setup (writes config files, returns extra env vars)
  let (mcpContext, agentEnv) ← agentDef.setupMcp serverPort model systemPrompt
  let paths := agentDef.sandboxPaths
  let mut args : Array String := #[]
  -- Read-write access to the repo and /tmp
  args := args.push "--rwx" |>.push repoPath.toString
  args := args.push "--rw" |>.push "/tmp"
  -- Read+execute system paths (binaries, libraries)
  for p in paths.rox do
    if ← System.FilePath.pathExists p then
      args := args.push "--rox" |>.push p
  -- Read-only system paths
  for p in paths.ro do
    if ← System.FilePath.pathExists p then
      args := args.push "--ro" |>.push p
  -- Read-write system paths (e.g. /dev/null)
  for p in paths.rw do
    if ← System.FilePath.pathExists p then
      args := args.push "--rw" |>.push p
  -- Home-relative paths with execute
  for p in ← expandHomePaths paths.homeRox do
    if ← p.pathExists then
      args := args.push "--rox" |>.push p.toString
  -- Home-relative paths read-write (agent config/state)
  for p in ← expandHomePaths paths.homeRw do
    if ← p.pathExists then
      args := args.push "--rw" |>.push p.toString
  -- Plugin directories (read+execute access)
  for p in pluginDirs do
    if ← System.FilePath.pathExists p then
      args := args.push "--rox" |>.push p
  -- Network: allow connecting to the local MCP server and external HTTPS
  args := args.push "--connect-tcp" |>.push (toString serverPort)
  args := args.push "--connect-tcp" |>.push "443"
  -- Environment variables for the sandboxed command
  args := args.push "--env" |>.push s!"GH_TOKEN={ghToken}"
  -- Pass through inherited env vars by name
  for name in ["SHELL", "PATH", "HOME", "USER", "TERM"] do
    args := args.push "--env" |>.push name
  -- Agent-specific env vars (e.g. VIBE_HOME, MISTRAL_API_KEY)
  for (k, v) in agentEnv do
    match v with
    | some val => args := args.push "--env" |>.push s!"{k}={val}"
    | none => pure ()
  -- Caller-supplied extra env vars
  for (k, v) in extraEnv do
    match v with
    | some val => args := args.push "--env" |>.push s!"{k}={val}"
    | none => pure ()
  -- Separator and the actual command with its args
  args := args.push "--"
  args := args.push agentDef.command
  let agentArgs := agentDef.buildArgs mcpContext pluginDirs subAgent model systemPrompt resume prompt
  args := args ++ agentArgs
  if debug then
    let argsStr := String.intercalate " " (args.toList.map shellEscape)
    IO.eprintln s!"[debug] cd {shellEscape repoPath.toString} && landrun {argsStr}"
  let child ← IO.Process.spawn {
    cmd := "landrun"
    args
    cwd := repoPath
    stdin := .null
    stdout := .piped
    stderr := .piped
  }
  -- Stream stdout, parse events and format for display; capture session ID if emitted
  let sessionIdRef ← IO.mkRef (none : Option String)
  let outTask ← IO.asTask (prio := .dedicated) do
    let out ← IO.getStdout
    repeat do
      let line ← child.stdout.getLine
      if line.isEmpty then return
      match agentDef.parseOutputLine line with
      | none => pure ()
      | some event =>
        if let .init sid _ := event then
          sessionIdRef.set (some sid)
        out.putStrLn (StreamFormat.format event)
        out.flush
  -- Stream stderr to console and capture it for usage-limit detection
  let stderrRef ← IO.mkRef ""
  let errTask ← IO.asTask (prio := .dedicated) do
    let err ← IO.getStderr
    repeat do
      let line ← child.stderr.getLine
      if line.isEmpty then return
      stderrRef.modify (· ++ line)
      err.putStr line
      err.flush
  -- Wait for streams to drain (EOF when child exits), then collect exit code
  let _ ← IO.wait outTask
  let _ ← IO.wait errTask
  let exitCode ← child.wait
  -- If the stream didn't yield a session ID, ask the backend (e.g. read from log files)
  let sessionId ← match ← sessionIdRef.get with
    | some sid => pure (some sid)
    | none     => agentDef.extractSessionId mcpContext
  -- Detect usage limit from exit code and captured stderr
  let stderrContent ← stderrRef.get
  let usageLimitHit := agentDef.isUsageLimitError exitCode stderrContent
  -- Clean up agent-specific resources (e.g. temp MCP config file)
  agentDef.cleanup mcpContext
  return { exitCode, sessionId, usageLimitHit }

end Orchestra.Sandbox
