import Orchestra.AgentDef
import Orchestra.StreamFormat
import Orchestra.Exec
import Std.Sync

/-!
# Launching an agent, and supervising it while it runs

This module answers *what the run needs* and *what the run said*; it no longer answers *how the
run is confined*. The first is `RunSpec` (`Orchestra.Exec.Spec`), built here from the task's
parameters and the agent backend's declared needs. The second is everything below the launch:
parsing the output stream, writing the logs, honouring the cancel token, and deciding whether the
run hit a usage limit. Confinement is an `Exec.Backend` — landrun by default — and swapping it
changes nothing in this file.

That split is what makes another execution model tractable: supervision is identical whether the
agent runs behind Landlock on this machine or in a pod elsewhere, and it is the bulk of the code.
See `docs/execution.md`.
-/

namespace Orchestra.Sandbox

open Orchestra.Exec

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
  /-- Reset timestamp reported by a `rate_limit_event`, if the agent emitted one. Lets the usage
      monitor record when a limit actually lifts instead of falling back to a default backoff. -/
  rateLimitReset : Option String := none

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

/-! ## Building the spec

Two pure functions, so that what a task is granted can be checked without launching anything —
which is the point of having a spec at all. -/

/-- Every path an agent run may touch, in the order they are granted.

    `paths` is the agent backend's own list and `additional` is the instance-wide
    `additional_sandbox_paths` from `config.json`. The two are not quite equal in standing: a
    missing path in the backend's list means the machine is not set up to run that agent, and is
    reported (`PathGrant.required`), while the configured list is advisory — it exists to grant
    access to directories that may or may not be there — and stays quiet. -/
def grantsFor (paths additional : SandboxPaths) (repoPath : System.FilePath) (readOnly : Bool)
    (pluginDirs memoryDirs : Array String) : Array PathGrant := Id.run do
  let ofList (scope : Scope) (access : Access) (required : Bool) (ps : List String) :=
    ps.toArray.map fun p => { path := p, access, scope, required : PathGrant }
  let mut grants : Array PathGrant := #[]
  -- The repository, read-only for review tasks and writable for everything else, and `/tmp`,
  -- which every agent CLI uses for its own scratch files. Both are required: the run cannot do
  -- anything useful without them.
  grants := grants.push
    { path := repoPath.toString, access := if readOnly then .rox else .rwx, required := true
    , from_ := .orchestra }
  grants := grants.push { path := "/tmp", access := .rw, required := true }
  -- The agent backend's declared needs.
  grants := grants ++ ofList .absolute .rox false paths.rox
  grants := grants ++ ofList .absolute .ro  false paths.ro
  grants := grants ++ ofList .absolute .rw  false paths.rw
  grants := grants ++ ofList .home     .rox false paths.homeRox
  grants := grants ++ ofList .home     .rw  true  paths.homeRw
  grants := grants ++ ofList .home     .rwx true  paths.homeRwx
  -- This instance's extra paths, on top.
  grants := grants ++ ofList .absolute .rox false additional.rox
  grants := grants ++ ofList .absolute .ro  false additional.ro
  grants := grants ++ ofList .absolute .rw  false additional.rw
  grants := grants ++ ofList .home     .rox false additional.homeRox
  grants := grants ++ ofList .home     .rw  false additional.homeRw
  grants := grants ++ ofList .home     .rwx false additional.homeRwx
  -- Plugins are read and run; memories are written back by the agent. Both are orchestra's own
  -- content, like the checkout: they sit on the daemon's disk and in no image, so a backend that
  -- runs the agent elsewhere has to carry them there — and carry the memories back.
  grants := grants ++ pluginDirs.map fun p =>
    { path := p, access := .rox, from_ := .orchestra : PathGrant }
  grants := grants ++ memoryDirs.map fun p =>
    { path := p, access := .rw, from_ := .orchestra : PathGrant }
  return grants

/-- The ports an agent run may use: the MCP server it was started for, HTTPS, and whatever the
    backend, the instance config and the task itself asked for on top.

    Everything but the MCP port and 443 is granted in both directions, because the reason a port
    is named here is a local service the agent both starts and talks to (an Ollama, a language
    server). The two orchestra grants are outbound only: the agent has no business listening on
    them. -/
def portsFor (paths additional : SandboxPaths) (mcp : McpEndpoint) (extraPorts : Array Nat)
    : Ports := Id.run do
  let extra : Array UInt16 :=
    paths.extraPorts.toArray ++ additional.extraPorts.toArray ++ extraPorts.map UInt16.ofNat
  return { connect := #[mcp.port, 443] ++ extra, bind := extra }

/-! ## Supervising the run -/

/-- Kill `handle` if and when `cancelToken` is cancelled.

    Blocks (without polling) on the token in a task of its own. When the run finishes normally the
    caller signals the token with a custom `"done"` reason, which wakes this task and lets it
    exit, breaking the reference cycle that would otherwise keep the handle alive. -/
private def killOnCancel (handle : Handle) (cancelToken : Option Std.CancellationToken)
    : IO Unit := do
  let some ct := cancelToken | return ()
  let _killTask ← IO.asTask (prio := .dedicated) do
    let asyncTask ← ct.wait
    match ← IO.wait asyncTask with
    | .error _ => pure ()             -- token dropped unexpectedly
    | .ok () =>
      match ← ct.getCancellationReason with
      | some .cancel => handle.kill   -- user-requested cancellation
      | _            => pure ()       -- "done" or other reason: the run already ended
  return ()

/-- Signal a still-live cancel token that the run is over, so `killOnCancel` can retire. -/
private def signalDone (cancelToken : Option Std.CancellationToken) : IO Unit := do
  if let some ct := cancelToken then
    if !(← ct.isCancelled) then
      ct.cancel (.custom "done")

/-- Whether the run ended because someone cancelled it, as opposed to any other reason the token
    may have been signalled for. -/
private def endedCancelled (cancelToken : Option Std.CancellationToken) : IO Bool := do
  match cancelToken with
  | none    => pure false
  | some ct => pure ((← ct.getCancellationReason) == some .cancel)

/--
Launch the coding agent in `session` — the environment the configured execution backend opened for
this task — and supervise the run.

The agent backend's `setupMcp` hook runs before launch to configure MCP connectivity, at the
address the session says the MCP server is reachable at. Returns a `LaunchResult` with the exit
code, session ID, and usage-limit flag.
-/
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
    (interactiveAgent : Bool := false)
    -- Condition the run is held to: the agent must not stop before it holds. Passed to the
    -- backend on its own, never folded into `prompt` — see `AgentDef.goalArgs`.
    (goal : Option String := none)
    -- The environment the agent runs in, opened for the whole task by whichever execution
    -- backend is configured. Defaults to this machine under landrun, which is what a caller that
    -- has not read `execution.backend` from the config should get.
    (session : Exec.Session := Exec.Landrun.session)
    -- Secret the agent must present to the MCP server, when the server had to listen somewhere
    -- other than loopback for this backend to reach it. Minted with the server by
    -- `Exec.mcpBinding`; `none` for every loopback run.
    (mcpToken : Option String := none) : IO LaunchResult := do
  -- Where the agent reaches the MCP server: loopback for a backend that runs it on this machine,
  -- and whatever a remote one says instead. Resolved before `setupMcp`, which writes it into the
  -- agent's config file.
  let mcp ← session.mcpEndpoint { host := "127.0.0.1", port := serverPort, token := mcpToken }
  let (mcpContext, agentEnv) ← agentDef.setupMcp mcp model systemPrompt
  -- Enforced here rather than where the prompts are built: every backend and every caller
  -- reaches the CLI through this one launch, and the limit is a property of `execve`, not of any
  -- one template. The system prompt is capped even in interactive mode, where there is no task
  -- prompt but `--append-system-prompt` is still passed.
  let prompt ← capPromptArg "prompt" prompt
  let systemPrompt ← systemPrompt.mapM (capPromptArg "system prompt")
  -- The goal rides alongside the built args rather than through them: every backend would
  -- otherwise need one more parameter it ignores. A backend with no goal mechanism says so by
  -- returning `none`, and the run proceeds without one — loudly, since a task that asked to be
  -- held to a condition and silently was not looks exactly like one that met it.
  --
  -- Placed ahead of the built args because those end in the prompt, which several backends pass
  -- as a positional argument; a flag after it is not reliably read as a flag.
  let goalArgs : Array String ← match goal with
    | none => pure #[]
    | some g =>
      match agentDef.goalArgs g with
      | some extra => pure extra
      | none =>
        IO.eprintln s!"  [sandbox] warning: backend '{agentDef.command}' cannot be held to a \
goal; running without the goal condition."
        pure #[]
  -- Memory dirs are exposed as plugin dirs to the agent (so they appear as --plugin-dir args)
  let allPluginDirs := pluginDirs ++ memoryDirs
  let agentArgs :=
    if interactiveAgent then
      agentDef.buildInteractiveArgs mcpContext allPluginDirs subAgent model systemPrompt resume budget
    else
      agentDef.buildArgs mcpContext allPluginDirs subAgent model systemPrompt resume budget prompt
  let env : Array (String × String) :=
    #[("GH_TOKEN", ghToken), ("CLAUDE_CODE_DISABLE_AUTO_MEMORY", "1")]
      -- Agent-specific env vars (e.g. VIBE_HOME, MISTRAL_API_KEY), then the caller's.
      ++ (agentEnv ++ extraEnv).filterMap fun (k, v) => v.map ((k, ·))
  let spec : RunSpec := {
    command := agentDef.command
    args    := goalArgs ++ agentArgs
    workdir := repoPath
    grants  := grantsFor agentDef.sandboxPaths additionalPaths repoPath readOnly
                 pluginDirs memoryDirs
    ports   := portsFor agentDef.sandboxPaths additionalPaths mcp extraPorts
    env
    -- Inherited by name, not by value: `PATH` and `HOME` mean what they mean wherever the agent
    -- ends up running, which is not necessarily here.
    envPassthrough := #["SHELL", "PATH", "HOME", "USER", "TERM"]
    stdio   := if interactiveAgent then .inherit else .piped
    label   := s!"orchestra-{agentDef.command}"
  }
  if debug then
    IO.eprintln (← session.describe spec)
  let handle ← session.start spec
  killOnCancel handle cancelToken
  if interactiveAgent then
    -- Interactive mode: the run has orchestra's own terminal, and the user is looking at the
    -- agent's TUI. Nothing to parse; just wait for it to exit.
    let exitCode ← handle.wait
    signalDone cancelToken
    let wasCancelled ← endedCancelled cancelToken
    let sessionId ← agentDef.extractSessionId mcpContext
    agentDef.cleanup mcpContext
    return { exitCode, sessionId, usageLimitHit := false, wasCancelled }
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
  let rateLimitResetRef ← IO.mkRef (none : Option String)
  let outTask ← match handle.stdout with
    | none => pure none
    | some stdout => some <$> IO.asTask (prio := .dedicated) do
      let out ← IO.getStdout
      let err ← IO.getStderr
      repeat do
        let line ← stdout.getLine
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
          -- Rate-limit events are bookkeeping, not progress: they are recorded for the usage
          -- monitor but kept off the console, which is what the old `parseOutputLine` returning
          -- `none` for them achieved before there was anywhere to record them.
          if let .rateLimit reset := event then
            if reset.isSome then rateLimitResetRef.set reset
          else
            out.putStrLn (StreamFormat.format event)
            out.flush
          -- Write the parsed event as a JSON line to the structured log
          if let some h := logHandle then
            h.putStrLn (Lean.Json.compress (Lean.ToJson.toJson event))
            h.flush
  -- Stream stderr to console and capture it for usage-limit detection
  let stderrRef ← IO.mkRef ""
  let errTask ← match handle.stderr with
    | none => pure none
    | some stderr => some <$> IO.asTask (prio := .dedicated) do
      let err ← IO.getStderr
      repeat do
        let line ← stderr.getLine
        if line.isEmpty then return
        stderrRef.modify (· ++ line)
        err.putStr line
        err.flush
  -- Wait for streams to drain (EOF when the run ends), then collect the exit code
  if let some t := outTask then let _ ← IO.wait t
  if let some t := errTask then let _ ← IO.wait t
  let exitCode ← handle.wait
  -- Signal the kill task to clean up (breaks reference cycle; no-op if already cancelled)
  signalDone cancelToken
  -- If the stream didn't yield a session ID, ask the backend (e.g. read from log files)
  let sessionId ← match ← sessionIdRef.get with
    | some sid => pure (some sid)
    | none     => agentDef.extractSessionId mcpContext
  -- Determine whether this run ended due to user cancellation
  let wasCancelled ← endedCancelled cancelToken
  let resultSubtype ← resultSubtypeRef.get
  let resultText    ← resultTextRef.get
  let rateLimitReset ← rateLimitResetRef.get
  -- Detect a usage limit from everything the run said about itself, not just stderr.
  --
  -- Both halves matter. A subscription run reports the limit through the output stream — an
  -- error result reading "You've reached your <model> limit" — and writes nothing to stderr, so
  -- stderr alone never sees it. And an error result is itself evidence the run failed, so it
  -- stands in for a non-zero exit: a backend that reports the limit in the stream and still
  -- exits 0 would otherwise read as a clean success.
  let stderrContent ← stderrRef.get
  let combinedOutput := stderrContent ++ "\n" ++ resultText.getD ""
  let effectiveExit :=
    if exitCode != 0 then exitCode
    else match resultSubtype with
      | some (.error _) => 1
      | _               => 0
  let usageLimitHit := agentDef.isUsageLimitError effectiveExit combinedOutput
  -- Clean up agent-specific resources (e.g. temp MCP config file)
  agentDef.cleanup mcpContext
  return { exitCode, sessionId, usageLimitHit, wasCancelled, resultSubtype, resultText,
           rateLimitReset }

end Orchestra.Sandbox
