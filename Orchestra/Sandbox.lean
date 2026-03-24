import Orchestra.AgentDef
import Orchestra.StreamFormat
import Std.Sync

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
  /-- True if the agent was killed because the cancel token was signalled. -/
  wasCancelled  : Bool := false

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
    (memoryDirs : Array String := #[])
    (subAgent : Option String := none)
    (model : Option String := none)
    (systemPrompt : Option String := none)
    (resume : Option String := none)
    (budget : Float := 4.0)
    (cancelToken : Option Std.CancellationToken := none)
    (debugLogFile : Option System.FilePath := none) : IO LaunchResult := do
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
  -- Memory directories (read-write access so the agent can persist memories)
  for p in memoryDirs do
    if ← System.FilePath.pathExists p then
      args := args.push "--rw" |>.push p
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
  -- Memory dirs are exposed as plugin dirs to the agent (so they appear as --plugin-dir args)
  let allPluginDirs := pluginDirs ++ memoryDirs
  let agentArgs := agentDef.buildArgs mcpContext allPluginDirs subAgent model systemPrompt resume budget prompt
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
  -- If a cancel token is provided, set up an async kill task.
  -- It blocks (without polling) until the token is cancelled, then kills the child.
  -- When the child exits normally we signal the token with a custom "done" reason
  -- so this task wakes up and exits, breaking any reference cycles.
  if let some ct := cancelToken then
    let _killTask ← IO.asTask (prio := .dedicated) do
      let asyncTask ← ct.wait
      let result ← IO.wait asyncTask
      match result with
      | .error _ => pure ()  -- token dropped unexpectedly
      | .ok () =>
        match ← ct.getCancellationReason with
        | some .cancel =>
          -- User-requested cancellation: kill the child process
          try
            let killer ← IO.Process.spawn {
              cmd := "kill"
              args := #["-9", toString child.pid]
              stdin := .null
              stdout := .null
              stderr := .null
            }
            let _ ← killer.wait
          catch _ => pure ()
        | _ => pure ()  -- "done" or other reason: child already exited, nothing to do
  -- Open debug log file if requested (one per task, created fresh)
  let debugHandle : Option IO.FS.Handle ← match debugLogFile with
    | none      => pure none
    | some path => some <$> IO.FS.Handle.mk path .write
  -- Stream stdout, parse events and format for display; capture session ID if emitted
  let sessionIdRef ← IO.mkRef (none : Option String)
  let outTask ← IO.asTask (prio := .dedicated) do
    let out ← IO.getStdout
    repeat do
      let line ← child.stdout.getLine
      if line.isEmpty then return
      -- Write every raw line to the debug log
      if let some h := debugHandle then
        h.putStrLn line
        h.flush
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
  -- Signal the kill task to clean up (breaks reference cycle; no-op if already cancelled)
  if let some ct := cancelToken then
    if !(← ct.isCancelled) then
      ct.cancel (.custom "done")
  -- If the stream didn't yield a session ID, ask the backend (e.g. read from log files)
  let sessionId ← match ← sessionIdRef.get with
    | some sid => pure (some sid)
    | none     => agentDef.extractSessionId mcpContext
  -- Detect usage limit from exit code and captured stderr
  let stderrContent ← stderrRef.get
  let usageLimitHit := agentDef.isUsageLimitError exitCode stderrContent
  -- Determine whether this run ended due to user cancellation
  let wasCancelled ← match cancelToken with
    | none => pure false
    | some ct => do
      let reason ← ct.getCancellationReason
      pure (reason == some .cancel)
  -- Clean up agent-specific resources (e.g. temp MCP config file)
  agentDef.cleanup mcpContext
  return { exitCode, sessionId, usageLimitHit, wasCancelled }

end Orchestra.Sandbox
