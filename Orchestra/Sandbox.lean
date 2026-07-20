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
  exitCode       : UInt32
  sessionId      : Option String
  /-- True if the agent exited because it hit a usage or quota limit. -/
  usageLimitHit  : Bool
  /-- True if the agent was killed because the cancel token was signalled. -/
  wasCancelled   : Bool := false
  /-- The subtype from the agent's result event, if one was emitted. -/
  resultSubtype  : Option StreamFormat.ResultSubtype := none
  /-- The result text from the agent's result event, if one was emitted. -/
  resultText     : Option String := none

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

/-- Byte ceiling for each prompt every backend passes to its CLI as a single argument: the task
    prompt and the appended system prompt.

    `execve` rejects any single argument longer than `MAX_ARG_STRLEN` — 32 pages, so 131072
    bytes on every architecture orchestra runs on — with `E2BIG`. The failure lands in the
    forked child, which can only report it as a bare `could not execute external process
    'landrun'` and exit 255: nothing names the prompt, and the task then burns its whole retry
    budget failing the same way. The margin below the hard limit is for the rest of the command
    line, which is bounded (flags, paths, one env var per name) but not free.

    The ceiling is per argument, not shared between the two: the *combined* size of a command
    line is governed by a separate and far larger limit (`ARG_MAX`, a couple of megabytes),
    which two arguments of this size come nowhere near. -/
private def maxPromptBytes : Nat := 120000

/-- Appended to a prompt that had to be cut, so the agent is told rather than left to infer it
    from a sentence ending mid-word. Counted against the budget, not added on top of it. -/
private def promptTruncationNotice : String :=
  "\n\n[Truncated by orchestra: this prompt exceeded the maximum length a single command-line \
argument can carry, so everything past this point was cut from the end. What you can see above \
is complete and unmodified.]"

/-- `s` cut to at most `maxPromptBytes` UTF-8 bytes, with `promptTruncationNotice` appended.

    Cuts on a character boundary rather than a byte one: a prompt carrying an issue thread is
    full of non-ASCII, and half a code point would leave the agent's CLI to reject the argument
    as invalid UTF-8 instead — trading one opaque launch failure for another. -/
private def truncatePrompt (s : String) : String :=
  if s.utf8ByteSize ≤ maxPromptBytes then s else Id.run do
    let budget := maxPromptBytes - promptTruncationNotice.utf8ByteSize
    let mut out   := ""
    let mut used  := 0
    for c in s.toList do
      let width := c.utf8Size
      if used + width > budget then break
      out  := out.push c
      used := used + width
    return out ++ promptTruncationNotice

/-- `s` capped to `maxPromptBytes`, saying so on stderr when it had to cut. `label` names which
    prompt overflowed, since the two are built from entirely different inputs and the fix
    differs accordingly.

    Loud, because an over-long prompt means something expanded without a bound —
    `{{issue_comments}}` on a long thread is the usual one — and cutting it only keeps the task
    running, it does not make the prompt right. -/
private def capPromptArg (label : String) (s : String) : IO String := do
  if s.utf8ByteSize ≤ maxPromptBytes then return s
  IO.eprintln s!"  [sandbox] warning: the {label} is {s.utf8ByteSize} bytes, over the \
{maxPromptBytes}-byte ceiling for a single command-line argument; cutting it off at the end. \
The agent will not see what was cut — check whether a prompt template is expanding something \
unbounded."
  return truncatePrompt s

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
    (debugLogFile : Option System.FilePath := none)
    (logFile : Option System.FilePath := none)
    -- If true, mount the project repository read-only in the sandbox.
    (readOnly : Bool := false)
    -- Additional TCP ports to allow, beyond what the agent backend already opens.
    (extraPorts : Array Nat := #[])
    -- Additional sandbox paths from global app config, merged with the agent-backend's built-in paths.
    (additionalPaths : SandboxPaths := {})
    -- If true, launch the agent in interactive (TUI) mode: inherit stdio and omit -p <prompt>.
    (interactiveAgent : Bool := false) : IO LaunchResult := do
  -- Run agent-specific MCP setup (writes config files, returns extra env vars)
  let (mcpContext, agentEnv) ← agentDef.setupMcp serverPort model systemPrompt
  let paths := agentDef.sandboxPaths
  let mut args : Array String := #[]
  -- Repo access: read-only or read-write depending on the task's readOnly flag
  if readOnly then
    args := args.push "--rox" |>.push repoPath.toString
  else
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
  -- Home-relative paths read-write (agent config/state).
  --
  -- A missing path here is not benign, unlike the system paths above. Landlock can only attach a
  -- rule to a path that exists, so a missing one is dropped — and since $HOME itself is never
  -- granted, the agent cannot create it either. It typically reports that it cannot write its
  -- config directory and then hangs, with nothing in the log to explain why. Warn instead: the
  -- fix is to create the path in whatever image or machine image is being used.
  for p in ← expandHomePaths paths.homeRw do
    if ← p.pathExists then
      args := args.push "--rw" |>.push p.toString
    else
      IO.eprintln s!"  [sandbox] warning: {p} does not exist, so the agent gets no write access \
        to it and cannot create it — the agent may hang on startup. Create it and retry."
  -- Home-relative paths needing write *and* execute (toolchain managers — see `SandboxPaths`).
  for p in ← expandHomePaths paths.homeRwx do
    if ← p.pathExists then
      args := args.push "--rwx" |>.push p.toString
    else
      IO.eprintln s!"  [sandbox] warning: {p} does not exist, so the agent gets no write access \
        to it and cannot create it — the agent may hang on startup. Create it and retry."
  -- Additional paths from global app config
  for p in additionalPaths.rox do
    if ← System.FilePath.pathExists p then
      args := args.push "--rox" |>.push p
  for p in additionalPaths.ro do
    if ← System.FilePath.pathExists p then
      args := args.push "--ro" |>.push p
  for p in additionalPaths.rw do
    if ← System.FilePath.pathExists p then
      args := args.push "--rw" |>.push p
  for p in ← expandHomePaths additionalPaths.homeRox do
    if ← p.pathExists then
      args := args.push "--rox" |>.push p.toString
  for p in ← expandHomePaths additionalPaths.homeRw do
    if ← p.pathExists then
      args := args.push "--rw" |>.push p.toString
  for p in ← expandHomePaths additionalPaths.homeRwx do
    if ← p.pathExists then
      args := args.push "--rwx" |>.push p.toString
  for p in additionalPaths.extraPorts do
    args := args.push "--connect-tcp" |>.push (toString p)
    args := args.push "--bind-tcp" |>.push (toString p)
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
  -- Agent-specific extra ports (e.g. local Ollama on 11434)
  for p in paths.extraPorts do
    args := args.push "--connect-tcp" |>.push (toString p)
    args := args.push "--bind-tcp" |>.push (toString p)
  -- Task-level extra ports configured in the action/task config
  for p in extraPorts do
    args := args.push "--connect-tcp" |>.push (toString p)
    args := args.push "--bind-tcp" |>.push (toString p)
  -- Environment variables for the sandboxed command
  args := args.push "--env" |>.push s!"GH_TOKEN={ghToken}"
  args := args.push "--env" |>.push "CLAUDE_CODE_DISABLE_AUTO_MEMORY=1"
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
  -- Enforced here rather than where the prompts are built: every backend and every caller
  -- reaches the CLI through this one spawn, and the limit is a property of `execve`, not of any
  -- one template. The system prompt is capped even in interactive mode, where there is no task
  -- prompt but `--append-system-prompt` is still passed.
  let prompt ← capPromptArg "prompt" prompt
  let systemPrompt ← systemPrompt.mapM (capPromptArg "system prompt")
  let agentArgs :=
    if interactiveAgent then
      agentDef.buildInteractiveArgs mcpContext allPluginDirs subAgent model systemPrompt resume budget
    else
      agentDef.buildArgs mcpContext allPluginDirs subAgent model systemPrompt resume budget prompt
  args := args ++ agentArgs
  if debug then
    let argsStr := String.intercalate " " (args.toList.map shellEscape)
    IO.eprintln s!"[debug] cd {shellEscape repoPath.toString} && landrun {argsStr}"
  if interactiveAgent then
    -- Interactive mode: inherit stdio so the user drops into the agent TUI.
    -- No stream parsing; just wait for the process to exit.
    let child ← IO.Process.spawn {
      cmd := "landrun"
      args
      cwd := repoPath
      stdin := .inherit
      stdout := .inherit
      stderr := .inherit
    }
    if let some ct := cancelToken then
      let _killTask ← IO.asTask (prio := .dedicated) do
        let asyncTask ← ct.wait
        let result ← IO.wait asyncTask
        match result with
        | .error _ => pure ()
        | .ok () =>
          match ← ct.getCancellationReason with
          | some .cancel =>
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
          | _ => pure ()
    let exitCode ← child.wait
    if let some ct := cancelToken then
      if !(← ct.isCancelled) then
        ct.cancel (.custom "done")
    let wasCancelled ← match cancelToken with
      | none => pure false
      | some ct => do
        let reason ← ct.getCancellationReason
        pure (reason == some .cancel)
    let sessionId ← agentDef.extractSessionId mcpContext
    agentDef.cleanup mcpContext
    return { exitCode, sessionId, usageLimitHit := false, wasCancelled }
  else
  -- Non-interactive (headless) mode: pipe stdio and parse the output stream.
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
  -- Open the structured JSON log file (one per task, always created when path is given)
  let logHandle : Option IO.FS.Handle ← match logFile with
    | none      => pure none
    | some path =>
      if let some dir := path.parent then
        IO.FS.createDirAll dir
      some <$> IO.FS.Handle.mk path .write
  -- Stream stdout, parse events and format for display; capture session ID if emitted
  let sessionIdRef    ← IO.mkRef (none : Option String)
  let resultSubtypeRef ← IO.mkRef (none : Option StreamFormat.ResultSubtype)
  let resultTextRef   ← IO.mkRef (none : Option String)
  let outTask ← IO.asTask (prio := .dedicated) do
    let out ← IO.getStdout
    let err ← IO.getStderr
    repeat do
      let line ← child.stdout.getLine
      if line.isEmpty then return
      -- Write every raw line to the debug log
      if let some h := debugHandle then
        h.putStrLn line
        h.flush
      -- When debug is on, echo every raw stdout line to stderr
      if debug then
        err.putStrLn s!"[raw] {line.trimAscii}"
        err.flush
      match agentDef.parseOutputLine line with
      | none =>
        if debug then
          err.putStrLn s!"[suppressed] {line.trimAscii}"
          err.flush
      | some event =>
        if let .init sid _ := event then
          sessionIdRef.set (some sid)
        if let .result sub _ _ _ res := event then
          resultSubtypeRef.set (some sub)
          unless res.isEmpty do resultTextRef.set (some res)
        out.putStrLn (StreamFormat.format event)
        out.flush
        -- Write the parsed event as a JSON line to the structured log
        if let some h := logHandle then
          h.putStrLn (Lean.Json.compress (Lean.ToJson.toJson event))
          h.flush
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
  let resultSubtype ← resultSubtypeRef.get
  let resultText    ← resultTextRef.get
  -- Clean up agent-specific resources (e.g. temp MCP config file)
  agentDef.cleanup mcpContext
  return { exitCode, sessionId, usageLimitHit, wasCancelled, resultSubtype, resultText }

end Orchestra.Sandbox
