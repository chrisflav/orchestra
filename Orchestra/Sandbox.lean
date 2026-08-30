import Orchestra.AgentDef
import Orchestra.StreamFormat
import Orchestra.Exec
import Std.Sync

/-!
# Launching an agent, and supervising it while it runs

This module answers *what the run needs* and *what the run said*; it no longer answers *how the
run is confined*. The first is a `RunSpec` (`Orchestra.Exec.Spec`), built here from the task's
parameters and the agent backend's declared needs. The second is everything below the launch:
parsing the output stream, writing the logs, honouring the cancel token, and deciding whether the
run hit a usage limit. Confinement is an `Exec.Session`, opened for the whole task by whichever
backend is configured — landrun by default — and swapping it changes nothing in this file.

All three ways to launch an agent go through the same session, and therefore through the same
environment: headless, the TUI, and the bidirectional stream an interactive session holds open.
That is the same reason the landrun arguments used to be factored into one place, taken one step
further — a sandbox that differs by launch mode is a sandbox nobody can reason about, and so is
one that differs by *where* it runs.

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

Three pure functions, so that what a run is granted can be checked without launching anything —
which is the point of having a spec at all. They are also what keeps the three launch modes
honest: headless, TUI and streaming differ in their command and their streams, and in nothing
else.

They replace what used to be one `sandboxArgs` producing landrun flags. Same reasoning, one level
up: the flags belonged in one place so they could not drift between launch modes, and the *needs*
belong in one place so they cannot drift between execution backends either. -/

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
  -- The workspace, read-only for review tasks and writable for everything else, and `/tmp`, which
  -- every agent CLI uses for its own scratch files. Both are required: the run cannot do anything
  -- useful without them.
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

/-- The environment every agent run starts with, whichever way it was launched.

    An empty token is left unset rather than exported empty: that is what a task with no GitHub
    App installation behind it gets, and `gh` treats `GH_TOKEN=` as a credential it must use,
    failing every call with an authentication error instead of saying it has none. -/
def envFor (ghToken : String) (agentEnv extraEnv : Array (String × Option String))
    : Array (String × String) :=
  (if ghToken.isEmpty then #[] else #[("GH_TOKEN", ghToken)])
    ++ #[("CLAUDE_CODE_DISABLE_AUTO_MEMORY", "1")]
    -- Agent-specific env vars (e.g. VIBE_HOME, MISTRAL_API_KEY), then the caller's.
    ++ (agentEnv ++ extraEnv).filterMap fun (k, v) => v.map ((k, ·))

/-- Everything an agent run needs except its own command line: the same spec for all three launch
    modes, which then differ only in `command`, `args` and `stdio`. -/
def specFor (agentDef : AgentDef) (repoPath : System.FilePath) (mcp : McpEndpoint)
    (ghToken : String) (agentEnv extraEnv : Array (String × Option String))
    (pluginDirs memoryDirs : Array String) (readOnly : Bool) (extraPorts : Array Nat)
    (additionalPaths : SandboxPaths) : RunSpec :=
  { command := agentDef.command
    workdir := repoPath
    grants  := grantsFor agentDef.sandboxPaths additionalPaths repoPath readOnly
                 pluginDirs memoryDirs
    ports   := portsFor agentDef.sandboxPaths additionalPaths mcp extraPorts
    env     := envFor ghToken agentEnv extraEnv
    -- Inherited by name, not by value: `PATH` and `HOME` mean what they mean wherever the agent
    -- ends up running, which is not necessarily here.
    envPassthrough := #["SHELL", "PATH", "HOME", "USER", "TERM"]
    label   := s!"orchestra-{agentDef.command}" }

/-! ## Supervising a run -/

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
    -- The environment the agent runs in, opened for the whole task by whichever execution backend
    -- is configured. Defaults to this machine under landrun, which is what a caller that has not
    -- read `execution.backend` from the config should get.
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
  -- Memory dirs are exposed as plugin dirs to the agent (so they appear as --plugin-dir args)
  let allPluginDirs := pluginDirs ++ memoryDirs
  -- Enforced here rather than where the prompts are built: every backend and every caller
  -- reaches the CLI through this one spawn, and the limit is a property of `execve`, not of any
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
  let agentArgs :=
    if interactiveAgent then
      agentDef.buildInteractiveArgs mcpContext allPluginDirs subAgent model systemPrompt resume budget
    else
      agentDef.buildArgs mcpContext allPluginDirs subAgent model systemPrompt resume budget prompt
  let spec : RunSpec :=
    { specFor agentDef repoPath mcp ghToken agentEnv extraEnv pluginDirs memoryDirs readOnly
        extraPorts additionalPaths with
      args  := goalArgs ++ agentArgs
      stdio := if interactiveAgent then .inherit else .piped }
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
        let events := agentDef.parseOutputLine line
        if events.isEmpty && debug then
          err.putStrLn s!"[suppressed] {line.trimAscii}"
          err.flush
        -- One line can carry several events — an assistant message that thinks, narrates and
        -- then calls a tool is three — so each is handled in the order the agent emitted it.
        for event in events do
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

/-! ## Streaming mode

The third way to launch an agent, alongside headless and TUI: one process that stays up across
many turns, reading each as a JSON line on stdin and streaming its events back on stdout. It is
what an interactive session holds — the sandbox, the clone, the MCP server and the credentials
are acquired once and kept, and a turn costs a line on a pipe rather than a process start.

Unlike the other two, this one does not block until the agent exits. It hands back a handle, and
the caller decides when the conversation is over. -/

/-- Ceiling on the stderr a streaming session keeps.

    Unbounded here is not what it is on a one-shot run: this process lives for hours across
    many turns, and `String.append` is O(n), so an unbounded tail is a leak and quadratic at
    once. The end is the part worth keeping — a usage limit or a crash reason is written last. -/
private def maxStderrChars : Nat := 32768

/-- How long `shutdown` gives the agent to go on its own before insisting, in 100ms polls. -/
private def shutdownGracePolls : Nat := 50

/-- A live agent process: turns go in, events come out, and it stays up in between. -/
structure StreamingSession where
  /-- The run, whatever is running it: a process here, an exec into a pod elsewhere. What this
      needs of it — a stream in, two streams out, and a way to ask it to stop and then insist — is
      exactly what an `Exec.Handle` promises. -/
  private handle : Exec.Handle
  /-- The agent's stdin, held here rather than on the run, and `none` once closed.

      A handle closes when its last reference drops, and a `Child` holds one of its own — so
      while the child owned this handle nothing could deliver EOF, however hard it tried.
      `Handle.ofStreamChild` takes it off the child at launch, which is what makes dropping this
      the last reference; EOF is how a CLI reading turns from a pipe is told there are no more. -/
  private stdinRef : IO.Ref (Option IO.FS.Handle)
  /-- Serialises writes to stdin, and the close in `shutdown` against them. Two turns posted at
      once — one from the CLI, one from a dashboard — would otherwise interleave halfway
      through a JSON line and give the agent neither of them. -/
  private stdinLock : Std.BaseMutex
  /-- The last `maxStderrChars` the process has said on stderr. -/
  private stderrRef : IO.Ref String
  /-- Set once `shutdown` has run, so a second call is a no-op rather than a second `cleanup`. -/
  private closed : IO.Ref Bool
  /-- The task draining stdout. It finishes when the agent closes it.

      Deliberately *not* what `hasExited` reads: this task can also end because the caller's
      `onEvent` threw, which says nothing about the process. -/
  pump : _root_.Task (Except IO.Error Unit)
  /-- Handed back to `AgentDef.cleanup` at teardown. -/
  mcpContext : String
  private agentDef : AgentDef

namespace StreamingSession

/-- Write one line to the agent's stdin, under the lock. -/
def sendLine (s : StreamingSession) (line : String) : IO Unit := do
  s.stdinLock.lock
  try
    match ← s.stdinRef.get with
    | none   => throw (.userError "the agent's input is closed")
    | some h => h.putStrLn line; h.flush
  finally s.stdinLock.unlock

/-- The tail of what the agent has written to stderr.

    Cumulative across turns, not per turn: it is a diagnostic string for a caller reporting why
    a session died, and nothing should decide a *turn's* outcome from it. -/
def stderrSoFar (s : StreamingSession) : IO String := s.stderrRef.get

/-- Whether the agent process has exited.

    Asks the process, not the pump. The pump also ends when `onEvent` throws — a full disk while
    appending to a transcript — and reading that as "the agent is gone" would have the daemon
    kill a healthy session mid-conversation. -/
def hasExited (s : StreamingSession) : IO Bool :=
  return (← s.handle.tryWait).isSome

/-- Stop the process and release what the backend set up for it.

    Three steps, in this order and for these reasons.

    **Close stdin.** That is the graceful ending: a CLI reading turns from a pipe stops when the
    pipe does. It is also the only step that works on an agent which is ignoring signals.

    **`SIGTERM`, then poll.** Polling `tryWait` rather than waiting on the pump, because the pump
    only finishes when the agent closes stdout — precisely what a wedged agent will not do. An
    earlier version waited on the pump first and then asked whether the process had gone, which
    made the escalation below unreachable by construction *and* let one stuck agent block the
    reaper for every session.

    **`SIGKILL`.** Reached now, for an agent that ignored the first two. -/
def shutdown (s : StreamingSession) : IO Unit := do
  if ← s.closed.modifyGet (fun c => (c, true)) then return
  s.stdinLock.lock
  try s.stdinRef.set none finally s.stdinLock.unlock
  try s.handle.terminate catch _ => pure ()
  let mut gone := false
  for _ in List.range shutdownGracePolls do
    if (← s.handle.tryWait).isSome then
      gone := true
      break
    IO.sleep 100
  unless gone do
    -- `terminate` is the polite signal; `kill` is the one that does not ask. On a backend that
    -- runs the agent elsewhere the second is also what releases what was holding it.
    try s.handle.kill catch _ => pure ()
  let _ ← try s.handle.wait catch _ => pure (0 : UInt32)
  s.agentDef.cleanup s.mcpContext

end StreamingSession

/-- Launch the agent in bidirectional streaming mode and hand back the live process.

    `opts.mcpContext` is filled in here from the backend's own `setupMcp`; whatever the caller
    put there is replaced. Everything else in `opts` is the caller's.

    Answers `none` when the backend has no streaming mode — the caller is expected to say so
    rather than quietly launch something else. -/
def launchStreaming (agentDef : AgentDef) (repoPath : System.FilePath)
    (serverPort : UInt16) (ghToken : String)
    (opts : StreamOptions)
    (onEvent : StreamFormat.Event → IO Unit)
    (debug : Bool := false)
    (extraEnv : Array (String × Option String) := #[])
    (pluginDirs : Array String := #[])
    (memoryDirs : Array String := #[])
    (readOnly : Bool := false)
    (extraPorts : Array Nat := #[])
    (additionalPaths : SandboxPaths := {})
    -- The environment the session's agent lives in, held for as long as the conversation is.
    (session : Exec.Session := Exec.Landrun.session)
    -- Secret the agent presents to the MCP server, when the server had to listen off loopback.
    (mcpToken : Option String := none) : IO (Option StreamingSession) := do
  let mcp ← session.mcpEndpoint { host := "127.0.0.1", port := serverPort, token := mcpToken }
  let (mcpContext, agentEnv) ← agentDef.setupMcp mcp opts.model opts.systemPrompt
  -- Capped for the same reason as everywhere else: it is one `execve` argument, and the limit
  -- belongs to `execve` rather than to any one caller.
  let systemPrompt ← opts.systemPrompt.mapM (capPromptArg "system prompt")
  let opts := { opts with
    mcpContext, systemPrompt
    -- One list, not two. The sandbox grant and the `--plugin-dir` flag have to name the same
    -- directories: granted but not passed and the agent silently runs with no plugins and no
    -- skills; passed but not granted and landrun denies the read. Whatever the caller put in
    -- `opts.pluginDirs` is replaced for that reason — the parameter is the one that also
    -- reaches the sandbox. Memory dirs reach the agent as plugin dirs, as on every other path.
    pluginDirs := pluginDirs ++ memoryDirs }
  let some agentArgs := agentDef.buildStreamArgs opts | do
    agentDef.cleanup mcpContext
    return none
  let spec : RunSpec :=
    { specFor agentDef repoPath mcp ghToken agentEnv extraEnv pluginDirs memoryDirs readOnly
        extraPorts additionalPaths with
      args  := agentArgs
      -- The mode that keeps stdin open: a turn is a line written to it, and closing it is how the
      -- conversation ends.
      stdio := .stream }
  if debug then
    IO.eprintln (← session.describe spec)
  -- Cleaned up on the way out of a failed start — a missing `landrun`, an unreachable cluster, a
  -- clone that vanished — which would otherwise leave the backend's temp MCP config behind, one
  -- per attempt.
  let handle ← try session.start spec
    catch e =>
      agentDef.cleanup mcpContext
      throw e
  -- A streaming session needs all three: turns go in, events come out, and a death rattle comes
  -- out beside them. A backend that cannot supply one says so here rather than handing back a
  -- session that silently never speaks.
  let some stdinHandle := handle.stdin | do
    handle.kill; agentDef.cleanup mcpContext
    throw (IO.userError "the execution backend gave this session no way to send it turns")
  let some stdout := handle.stdout | do
    handle.kill; agentDef.cleanup mcpContext
    throw (IO.userError "the execution backend gave this session no output to read")
  let some stderrStream := handle.stderr | do
    handle.kill; agentDef.cleanup mcpContext
    throw (IO.userError "the execution backend gave this session no stderr to read")
  let stderrRef ← IO.mkRef ""
  -- One line can carry several events, and each is delivered in the order the agent emitted it.
  -- A throw from `onEvent` ends the pump, which is what closing the transcript file underneath
  -- it would look like; the process is then still alive but unheard, and `hasExited` says so.
  let pump ← IO.asTask (prio := .dedicated) do
    let err ← IO.getStderr
    repeat do
      let line ← stdout.getLine
      if line.isEmpty then return
      if debug then
        err.putStrLn s!"[raw] {line.trimAscii}"
        err.flush
      for event in agentDef.parseOutputLine line do
        -- A throw here must not end the pump. Nothing else drains stdout, so an agent whose
        -- output pipe fills stops working — and since `hasExited` asks the process rather than
        -- this task, it would do so with nothing reporting it. One event is worth losing; the
        -- session is not.
        try onEvent event
        catch e =>
          err.putStrLn s!"  [sandbox] dropped an event: {e}"
          err.flush
  let _errTask ← IO.asTask (prio := .dedicated) do
    repeat do
      let line ← stderrStream.getLine
      if line.isEmpty then return
      stderrRef.modify fun acc =>
        let acc := acc ++ line
        if acc.length ≤ maxStderrChars then acc else (acc.takeEnd maxStderrChars).toString
  return some {
    handle, stderrRef, pump, mcpContext, agentDef
    stdinRef  := ← IO.mkRef (some stdinHandle)
    stdinLock := ← Std.BaseMutex.new
    closed    := ← IO.mkRef false
  }

end Orchestra.Sandbox
