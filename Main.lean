import Orchestra
import Cli
import Std.Sync

open Cli
open Orchestra

/-- Raw command-line args stored at startup so background re-exec can reconstruct them. -/
initialize gRawArgs : IO.Ref (List String) ← IO.mkRef []

-- Helpers

private def parseFloat? (s : String) : Option Float :=
  match Lean.Json.parse s with
  | .ok (.num n) => some n.toFloat
  | _ => none

private def padRight (s : String) (n : Nat) : String :=
  let truncated := String.ofList (s.toList.take n)
  truncated ++ String.ofList (List.replicate (n - truncated.length) ' ')

private def stripExt (s ext : String) : String :=
  if s.endsWith ext then
    s.dropEnd ext.length |>.toString
  else s

/-- Single-quote a string for safe use in a POSIX shell command. -/
private def shellQuote (s : String) : String :=
  "'" ++ s.replace "'" "'\\''" ++ "'"

-- Task execution

/-- Run a single task: clone repo, start MCP server, run validation loop.
    Returns the task ID and whether the run was cut short by a usage limit. -/
private def runTask (appConfig : AppConfig) (task : Task) (idx : Nat) (debug : Bool)
    (continuesFrom : Option String := none)
    (series : Option String := none)
    (cancelToken : Option Std.CancellationToken := none)
    (interactive : Bool := true) : IO (String × Bool) := do
  IO.println s!"=== Task {idx}: {task.fork} ({repr task.mode}) ==="
  -- Record this run in the task store
  let taskId ← TaskStore.generateId
  let createdAt ← TaskStore.currentIso8601
  let initialRecord : TaskStore.TaskRecord := {
    id := taskId, createdAt
    upstream := task.upstream, fork := task.fork, mode := task.mode, prompt := task.prompt
    continuesFrom, series
    backend := task.backend, model := task.model, agent := task.agent
    systemPrompt := task.systemPrompt, budget := task.budget
  }
  TaskStore.saveTask initialRecord
  -- Resolve initial resume session from the continued task
  let initialResume : Option String ← match continuesFrom with
    | none => pure none
    | some prevId =>
      match ← TaskStore.loadTask prevId with
      | none =>
        IO.eprintln s!"  Warning: task '{prevId}' not found, ignoring --continues"
        pure none
      | some prev => pure prev.sessionId
  -- 1. Create GitHub App token
  IO.println "  Creating GitHub App token..."
  let jwt ← GitHub.createJWT appConfig.appId appConfig.privateKeyPath
  let (forkOwner, _) ← Repo.splitRepo task.fork
  let installationId ← match appConfig.installationId with
    | some id => pure id
    | none => GitHub.getInstallationId jwt forkOwner
  let token ← GitHub.createInstallationToken jwt installationId
  GitHub.setupGhAuth token
  IO.println "  Token ready"
  -- 2. Clone / update repo
  IO.println s!"Cloning/updating {task.fork}..."
  let repoPath ← Repo.ensureCloned task.fork task.upstream interactive
  IO.println s!"  Repo at {repoPath}"
  -- 3. Start MCP server (runs in this process, outside the sandbox)
  let serverState : Server.State := {
    upstream := task.upstream
    fork := task.fork
    allowPR := match task.mode with | .pr => true | .fork => false
    appId := appConfig.appId
    privateKeyPath := appConfig.privateKeyPath
    installationId
    pat := appConfig.pat
  }
  let (port, shutdown) ← Server.start serverState
  IO.println s!"  MCP server on port {port}"
  -- 4. Run init hook and load per-repository config
  RepoConfig.runInitIfNeeded repoPath
  let repoConfig ← RepoConfig.loadRepoConfig repoPath
  -- 5. Validation loop: before.sh → agent → validation.sh, retry on failure
  let systemPrompt ← loadSystemPrompt task.systemPrompt
  let mut sessionId : Option String := none
  let mut usageLimitHit := false
  let mut wasCancelled := false
  let maxAttempts := repoConfig.validation.maxRetries + 1
  for attempt in List.range maxAttempts do
    RepoConfig.runHook repoPath "before.sh"
    let prompt := if attempt == 0 then task.prompt else repoConfig.validation.retryPrompt
    let resume := if attempt == 0 then initialResume else sessionId
    IO.println s!"  Launching agent (attempt {attempt + 1}/{maxAttempts})..."
    let agentDef := match task.backend with
      | some "vibe" => AgentDef.vibe
      | _           => AgentDef.claude
    let result ← Sandbox.launchAgent agentDef repoPath prompt port token
      (debug := debug) (pluginDirs := appConfig.pluginDirs) (subAgent := task.agent)
      (model := task.model) (systemPrompt := systemPrompt) (resume := resume)
      (budget := task.budget.getD 4.0) (cancelToken := cancelToken)
    IO.println s!"  Agent exited with code {result.exitCode}"
    sessionId := result.sessionId
    if result.wasCancelled then
      IO.println "  Agent was cancelled."
      wasCancelled := true
      break
    if result.usageLimitHit then
      IO.println "  Agent hit usage limit."
      usageLimitHit := true
      break
    let valid ← RepoConfig.runValidation repoPath
    if valid then break
    if attempt + 1 < maxAttempts then
      IO.println s!"  Validation failed, retrying ({attempt + 1}/{repoConfig.validation.maxRetries})..."
    else
      IO.eprintln s!"  Validation still failing after {repoConfig.validation.maxRetries} retries"
  -- 6. Run after hook and shut down MCP server
  RepoConfig.runHook repoPath "after.sh"
  shutdown
  -- 7. Persist final task state
  let finalStatus :=
    if wasCancelled then .cancelled
    else if usageLimitHit then .unfinished
    else .completed
  TaskStore.saveTask { initialRecord with sessionId, status := finalStatus }
  if let some seriesName := series then
    TaskStore.updateSeriesPointer seriesName taskId
  IO.println s!"=== Task {idx} done ===\n"
  return (taskId, usageLimitHit)

-- Helpers

/-- If `series` is already set, return it unchanged.
    Otherwise, if `continuesFrom` references a task that belongs to a series,
    inherit that series so the new task is automatically tagged. -/
private def inheritSeries (continuesFrom : Option String) (series : Option String) :
    IO (Option String) := do
  match series with
  | some _ => return series
  | none =>
    let some prevId := continuesFrom | return none
    let some prev ← TaskStore.loadTask prevId | return none
    return prev.series

-- Handlers

private def runHandler (p : Parsed) : IO UInt32 := do
  let taskFile      := p.positionalArg! "task-file" |>.as! String
  let configPath    := p.flag? "config"    |>.map (·.as! String)
  let taskIdx       := p.flag? "task"      |>.map (·.as! Nat)
  let debug         := p.hasFlag "debug"
  let continuesFrom := p.flag? "continues" |>.map (·.as! String)
  let series        := p.flag? "series"    |>.map (·.as! String)
  let budgetFlag    := p.flag? "budget"    |>.bind (fun v => parseFloat? (v.as! String))
  let appConfig ← loadAppConfig (configPath.map System.FilePath.mk)
  let taskFileData ← loadTaskFile taskFile
  if taskFileData.tasks.isEmpty then
    IO.eprintln "No tasks found in task file"
    return 1
  let tasks := match taskIdx with
    | some idx =>
      if h : idx < taskFileData.tasks.size then #[taskFileData.tasks[idx]]
      else #[]
    | none => taskFileData.tasks
  if tasks.isEmpty then
    IO.eprintln "Task index out of range"
    return (1 : UInt32)
  if continuesFrom.isSome && tasks.size > 1 then
    IO.eprintln "--continues requires --task when the task file has multiple tasks"
    return (1 : UInt32)
  let series ← inheritSeries continuesFrom series
  for i in [:tasks.size] do
    try
      -- CLI --budget overrides the task file budget
      let task := match budgetFlag with
        | none   => tasks[i]!
        | some b => { tasks[i]! with budget := some b }
      let _ ← runTask appConfig task i debug (continuesFrom := continuesFrom) (series := series)
    catch e =>
      IO.eprintln s!"Task {i} failed: {e}"
  return (0 : UInt32)

private def mcpServerHandler (p : Parsed) : IO UInt32 := do
  let upstream := p.positionalArg! "upstream" |>.as! String
  let fork := p.positionalArg! "fork" |>.as! String
  let allowPR := p.hasFlag "allow_pr"
  let configPath := p.flag? "config" |>.map (·.as! String)
  let appConfig ← loadAppConfig (configPath.map System.FilePath.mk)
  let jwt ← GitHub.createJWT appConfig.appId appConfig.privateKeyPath
  let (forkOwner, _) ← Repo.splitRepo fork
  let installationId ← match appConfig.installationId with
    | some id => pure id
    | none => GitHub.getInstallationId jwt forkOwner
  let token ← GitHub.createInstallationToken jwt installationId
  GitHub.setupGhAuth token
  let serverState : Server.State := {
    upstream, fork, allowPR
    appId := appConfig.appId
    privateKeyPath := appConfig.privateKeyPath
    installationId
    pat := appConfig.pat
  }
  let (port, _shutdown) ← Server.start serverState
  IO.println s!"MCP server listening on port {port}"
  repeat do
    IO.sleep 60000
  return (0 : UInt32)

private def prepareHandler (p : Parsed) : IO UInt32 := do
  let upstream := p.positionalArg! "upstream" |>.as! String
  let fork := p.positionalArg! "fork" |>.as! String
  let repoPath ← Repo.ensureCloned fork upstream
  IO.println repoPath.toString
  return (0 : UInt32)

private def cleanupHandler (_ : Parsed) : IO UInt32 := do
  Repo.cleanup
  return (0 : UInt32)

private def tasksHandler (p : Parsed) : IO UInt32 := do
  let limit := p.flag? "limit" |>.map (·.as! Nat) |>.getD 20
  let records := (← TaskStore.loadAllTasks).toList.take limit
  if records.isEmpty then
    IO.println "No tasks found."
    return (0 : UInt32)
  IO.println s!"{padRight "ID" 16} {padRight "CREATED" 20} {padRight "FORK" 28} {padRight "STATUS" 11} SERIES"
  IO.println (String.ofList (List.replicate 90 '-'))
  for r in records do
    let status := match r.status with
      | .running => "running" | .completed => "completed" | .failed => "failed"
      | .unfinished => "unfinished" | .cancelled => "cancelled"
    IO.println s!"{padRight r.id 16} {padRight r.createdAt 20} {padRight r.fork 28} {padRight status 11} {r.series.getD ""}"
  return (0 : UInt32)

private def taskShowHandler (p : Parsed) : IO UInt32 := do
  let id := p.positionalArg! "id" |>.as! String
  match ← TaskStore.loadTask id with
  | none =>
    IO.eprintln s!"Task '{id}' not found"
    return 1
  | some r =>
    let status := match r.status with
      | .running => "running" | .completed => "completed" | .failed => "failed"
      | .unfinished => "unfinished" | .cancelled => "cancelled"
    let mode := match r.mode with | .fork => "fork" | .pr => "pr"
    IO.println s!"ID:             {r.id}"
    IO.println s!"Created:        {r.createdAt}"
    IO.println s!"Status:         {status}"
    IO.println s!"Fork:           {r.fork}"
    IO.println s!"Upstream:       {r.upstream}"
    IO.println s!"Mode:           {mode}"
    IO.println s!"Series:         {r.series.getD "-"}"
    IO.println s!"Continues from: {r.continuesFrom.getD "-"}"
    IO.println s!"Session ID:     {r.sessionId.getD "-"}"
    IO.println "Prompt:"
    for line in r.prompt.splitOn "\n" do
      IO.println s!"  {line}"
    return (0 : UInt32)

private def seriesHandler (_ : Parsed) : IO UInt32 := do
  let dir ← TaskStore.seriesDir
  if !(← dir.pathExists) then
    IO.println "No series found."
    return (0 : UInt32)
  let entries ← System.FilePath.readDir dir
  let entries := entries.filter (fun e => e.fileName.endsWith ".json")
  if entries.isEmpty then
    IO.println "No series found."
    return (0 : UInt32)
  IO.println s!"{padRight "SERIES" 24} LATEST TASK ID"
  IO.println (String.ofList (List.replicate 42 '-'))
  for entry in entries do
    let name := stripExt entry.fileName ".json"
    let latestId := (← TaskStore.latestInSeries name).getD "?"
    IO.println s!"{padRight name 24} {latestId}"
  return (0 : UInt32)

private def tagHandler (p : Parsed) : IO UInt32 := do
  let id         := p.positionalArg! "id"     |>.as! String
  let seriesName := p.positionalArg! "series" |>.as! String
  let some r ← TaskStore.loadTask id
    | IO.eprintln s!"Task '{id}' not found"; return 1
  TaskStore.saveTask { r with series := some seriesName }
  TaskStore.updateSeriesPointer seriesName id
  IO.println s!"Task {id} added to series '{seriesName}'"
  return (0 : UInt32)

private def resumeHandler (p : Parsed) : IO UInt32 := do
  let seriesName := p.positionalArg! "series" |>.as! String
  let prompt     := p.flag? "prompt" |>.map (·.as! String) |>.getD ""
  if prompt.isEmpty then
    throw (.userError "missing required flag: --prompt")
  let configPath  := p.flag? "config"  |>.map (·.as! String)
  let debug       := p.hasFlag "debug"
  let budgetFlag  := p.flag? "budget"  |>.bind (fun v => parseFloat? (v.as! String))
  let appConfig ← loadAppConfig (configPath.map System.FilePath.mk)
  let some prevId ← TaskStore.latestInSeries seriesName
    | throw (.userError s!"series '{seriesName}' not found")
  let some prevRecord ← TaskStore.loadTask prevId
    | throw (.userError s!"task '{prevId}' not found in store")
  let task : Task := {
    upstream     := prevRecord.upstream
    fork         := prevRecord.fork
    mode         := prevRecord.mode
    prompt
    backend      := prevRecord.backend
    model        := prevRecord.model
    agent        := prevRecord.agent
    systemPrompt := prevRecord.systemPrompt
    budget       := budgetFlag.orElse (fun _ => prevRecord.budget)
  }
  let _ ← runTask appConfig task 0 debug (continuesFrom := some prevId) (series := some seriesName)
  return (0 : UInt32)

-- Queue helpers

/-- Read the current process PID from /proc/self/stat (Linux-specific). -/
private def getOwnPid : IO UInt32 := do
  let stat ← IO.FS.readFile (System.FilePath.mk "/proc/self/stat")
  match stat.splitOn " " with
  | pid :: _ => return (pid.toNat?.getD 0).toUInt32
  | _        => return 0

private def enqueueHandler (p : Parsed) : IO UInt32 := do
  let configPath    := p.flag? "config"    |>.map (·.as! String)
  let taskIdx       := p.flag? "task"      |>.map (·.as! Nat)
  let continuesFrom := p.flag? "continues" |>.map (·.as! String)
  let series        := p.flag? "series"    |>.map (·.as! String)
  let resumeSeries  := p.flag? "resume"    |>.map (·.as! String)
  let prompt        := p.flag? "prompt"    |>.map (·.as! String)
  let budgetFlag    := p.flag? "budget"    |>.bind (fun v => parseFloat? (v.as! String))
  let taskFile?     := (p.variableArgsAs? String |>.getD #[])[0]?
  match resumeSeries, taskFile? with
  | some _, some _ =>
    IO.eprintln "Cannot use both a task file and --resume"
    return 1
  | none, none =>
    IO.eprintln "Provide a task file or --resume <series> --prompt <text>"
    return 1
  | some seriesName, none =>
    -- Series-continuation mode: inherit repo details from the latest task in the series
    let promptText ← match prompt with
      | some t => pure t
      | none   => throw (.userError "missing required flag: --prompt")
    let some prevId ← TaskStore.latestInSeries seriesName
      | throw (.userError s!"series '{seriesName}' not found")
    let some prevRecord ← TaskStore.loadTask prevId
      | throw (.userError s!"task '{prevId}' not found in store")
    let id ← TaskStore.generateId
    let createdAt ← TaskStore.currentIso8601
    let entry : Queue.QueueEntry := {
      id, createdAt
      upstream      := prevRecord.upstream
      fork          := prevRecord.fork
      mode          := prevRecord.mode
      prompt        := promptText
      continuesFrom := some prevId
      series        := some seriesName
      configPath
      backend       := prevRecord.backend
      model         := prevRecord.model
      agent         := prevRecord.agent
      systemPrompt  := prevRecord.systemPrompt
      budget        := budgetFlag.orElse (fun _ => prevRecord.budget)
    }
    Queue.saveEntry entry
    IO.println entry.id
    return (0 : UInt32)
  | none, some taskFile =>
    -- Task-file mode: enqueue tasks from a JSON task file
    let taskFileData ← loadTaskFile taskFile
    if taskFileData.tasks.isEmpty then
      IO.eprintln "No tasks found in task file"
      return 1
    let tasks := match taskIdx with
      | some idx =>
        if h : idx < taskFileData.tasks.size then #[taskFileData.tasks[idx]]
        else #[]
      | none => taskFileData.tasks
    if tasks.isEmpty then
      IO.eprintln "Task index out of range"
      return 1
    if continuesFrom.isSome && tasks.size > 1 then
      IO.eprintln "--continues requires --task when the task file has multiple tasks"
      return 1
    let series ← inheritSeries continuesFrom series
    for task in tasks do
      let id ← TaskStore.generateId
      let createdAt ← TaskStore.currentIso8601
      let entry : Queue.QueueEntry := {
        id, createdAt
        upstream     := task.upstream
        fork         := task.fork
        mode         := task.mode
        prompt       := task.prompt
        agent        := task.agent
        systemPrompt := task.systemPrompt
        backend      := task.backend
        model        := task.model
        continuesFrom, series
        configPath
        budget       := budgetFlag.orElse (fun _ => task.budget)
      }
      Queue.saveEntry entry
      IO.println entry.id
    return (0 : UInt32)

private def queueStartHandler (p : Parsed) : IO UInt32 := do
  let configPath   := p.flag? "config"       |>.map (·.as! String)
  let listenerDir  := p.flag? "listener_dir" |>.map (·.as! String)
  let debug        := p.hasFlag "debug"
  let background   := p.hasFlag "background"
  -- Background mode: re-exec as a detached daemon and exit
  if background then
    if ← Queue.daemonRunning then
      IO.eprintln "Queue daemon is already running."
      return 1
    let rawArgs ← gRawArgs.get
    let filteredArgs := rawArgs.filter (fun s => s != "--background" && s != "-b")
    let dir ← Queue.queueDir
    IO.FS.createDirAll dir
    let logFile ← Queue.daemonLogFile
    let exePath ← IO.appPath
    let quotedArgs := filteredArgs.map shellQuote |> String.intercalate " "
    let shellCmd :=
      s!"exec {shellQuote exePath.toString} {quotedArgs} >> {shellQuote logFile.toString} 2>&1 & echo $!"
    let launcher ← IO.Process.spawn {
      cmd := "sh"
      args := #["-c", shellCmd]
      stdin := .null
      stdout := .piped
      stderr := .null
    }
    let _ ← launcher.stdout.readToEnd
    let _ ← launcher.wait
    -- Wait up to 3 seconds for the daemon to write its own PID file
    let rec waitForDaemon : Nat → IO Bool
      | 0 => return false
      | n + 1 => do
        IO.sleep 300
        if ← Queue.daemonRunning then return true
        waitForDaemon n
    if ← waitForDaemon 10 then
      let pid := (← Queue.readPid).getD 0
      IO.println s!"Queue daemon started in background (PID {pid}), log: {logFile}"
      return 0
    else
      IO.eprintln "Queue daemon failed to start. Log output:"
      let log ← try IO.FS.readFile logFile catch _ => pure "(log file not found)"
      IO.eprintln log
      return 1
  -- Foreground mode
  if ← Queue.daemonRunning then
    IO.eprintln "Queue daemon is already running."
    return 1
  let pid ← getOwnPid
  Queue.writePid pid
  IO.println s!"Queue daemon started (PID {pid})"
  -- Mark entries left in 'running' by a previously killed daemon as unfinished
  Queue.markStaleRunningAsUnfinished
  -- Clear any sentinel files left over from a previous run
  Queue.clearShutdownRequest
  Queue.clearCancelRequest
  let appConfig ← loadAppConfig (configPath.map System.FilePath.mk)
  -- Shutdown token: cancelled by the file watcher when shutdown.request appears
  let shutdownToken ← Std.CancellationToken.new
  -- Current task cancel token: set while a task is running, read by the file watcher
  let currentTaskToken ← Std.Mutex.new (none : Option Std.CancellationToken)
  -- File watcher task: polls sentinel files every 500ms and updates tokens.
  -- This is the only place that reads the filesystem for control signals.
  let _watcher ← IO.asTask (prio := .dedicated) do
    while !(← shutdownToken.isCancelled) do
      IO.sleep 500
      if ← Queue.checkCancelRequested then
        Queue.clearCancelRequest
        let optToken ← currentTaskToken.atomically (·.get)
        if let some token := optToken then
          token.cancel .cancel
      if ← Queue.checkShutdownRequested then
        Queue.clearShutdownRequest
        shutdownToken.cancel .shutdown
  -- Load listener configs
  let lDir ← match listenerDir with
    | some d => pure (System.FilePath.mk d)
    | none   => Listener.listenersDir
  let listenerConfigs ← Listener.loadAllListenerConfigs lDir
  if !listenerConfigs.isEmpty then
    IO.println s!"Loaded {listenerConfigs.size} listener(s) from {lDir}"
  -- Next-poll timestamps: (listenerName, nextPollNanos) pairs. Nat matches IO.monoNanosNow.
  -- Initialised empty so every listener fires on the first iteration (due = 0).
  let nextPollRef ← IO.mkRef (Array.empty : Array (String × Nat))
  -- Helper: look up due time for a listener (0 = always due)
  let getDue (arr : Array (String × Nat)) (name : String) : Nat :=
    arr.find? (fun p => p.1 == name) |>.map (·.2) |>.getD 0
  -- Helper: upsert due time
  let setDue (arr : Array (String × Nat)) (name : String) (t : Nat) : Array (String × Nat) :=
    match arr.findIdx? (fun p => p.1 == name) with
    | some i => arr.set! i (name, t)
    | none   => arr.push (name, t)
  try
  while true do
    -- Check for graceful shutdown request between tasks
    if ← shutdownToken.isCancelled then
      break
    -- Run the next pending queue entry (if any)
    match ← Queue.nextPending with
    | none => pure ()
    | some entry =>
      -- Create a fresh cancel token for this task
      let taskToken ← Std.CancellationToken.new
      currentTaskToken.atomically (·.set (some taskToken))
      Queue.saveEntry { entry with status := .running }
      let task : Task := {
        upstream     := entry.upstream
        fork         := entry.fork
        mode         := entry.mode
        prompt       := entry.prompt
        agent        := entry.agent
        systemPrompt := entry.systemPrompt
        backend      := entry.backend
        model        := entry.model
        budget       := entry.budget
      }
      let cfg ← match entry.configPath with
        | none    => pure appConfig
        | some cp => loadAppConfig (some (System.FilePath.mk cp))
      try
        let (taskId, usageLimitHit) ← runTask cfg task 0 debug
          (continuesFrom := entry.continuesFrom) (series := entry.series)
          (cancelToken := some taskToken) (interactive := false)
        currentTaskToken.atomically (·.set none)
        if ← taskToken.isCancelled then
          Queue.saveEntry { entry with status := .cancelled, taskId := some taskId }
          IO.println s!"  Task cancelled."
        else if usageLimitHit then
          Queue.saveEntry { entry with status := .unfinished, taskId := some taskId }
          Queue.cancelPendingByBackend entry.backend entry.id
          Queue.cancelDependents taskId
          IO.println s!"  Cancelled pending {entry.backend.getD "claude"} tasks and dependents."
        else
          Queue.saveEntry { entry with status := .done, taskId := some taskId }
      catch e =>
        currentTaskToken.atomically (·.set none)
        if ← taskToken.isCancelled then
          IO.eprintln s!"  Task cancelled (with error: {e})"
          try Queue.saveEntry { entry with status := .cancelled } catch _ => pure ()
        else
          IO.eprintln s!"Queue entry {entry.id} failed: {e}"
          try Queue.saveEntry { entry with status := .failed } catch _ => pure ()
    -- Poll each listener whose interval has elapsed
    let now ← IO.monoNanosNow
    let nextPoll ← nextPollRef.get
    for lcfg in listenerConfigs do
      let due := getDue nextPoll lcfg.name
      if now >= due then
        try
          let state  ← Listener.loadListenerState lcfg.name
          let events ← Listener.pollSource lcfg.source state appConfig.pat
          for ev in (events : Array (String × List (String × String))) do
            let qentry ← Listener.buildQueueEntry lcfg.action ev.2
            Queue.saveEntry qentry
            IO.println s!"  Listener '{lcfg.name}': queued entry {qentry.id}"
          -- Update state: record processed IDs and last-checked timestamp
          let newIds   := events.filterMap (fun ev =>
            if (ev.1 : String).isEmpty then none else some ev.1)
          let newState : Listener.ListenerState := {
            lastChecked  := ← TaskStore.currentIso8601
            processedIds := state.processedIds ++ newIds
          }
          Listener.saveListenerState lcfg.name newState
        catch e =>
          IO.eprintln s!"  Listener '{lcfg.name}' poll error: {e}"
        -- Schedule next poll regardless of success/failure
        let intervalNanos := lcfg.intervalSeconds * 1_000_000_000
        nextPollRef.modify (setDue · lcfg.name (now + intervalNanos))
    IO.sleep 2000
  -- Graceful shutdown: remaining pending tasks stay queued for next restart
  finally
    Queue.deletePid
  IO.println "Queue daemon shut down gracefully."
  return 0

private def queueListHandler (p : Parsed) : IO UInt32 := do
  let limit := p.flag? "limit" |>.map (·.as! Nat) |>.getD 20
  if ← Queue.daemonRunning then
    match ← Queue.readPid with
    | some pid => IO.println s!"Daemon running (PID {pid})"
    | none     => IO.println "Daemon running"
  else
    IO.println "Daemon not running"
  let entries := (← Queue.loadAllEntries).toList.take limit
  if entries.isEmpty then
    IO.println "No queue entries found."
    return (0 : UInt32)
  IO.println ""
  IO.println s!"{padRight "ID" 16} {padRight "CREATED" 20} {padRight "FORK" 28} {padRight "STATUS" 9} SERIES"
  IO.println (String.ofList (List.replicate 85 '-'))
  for e in entries do
    let status := match e.status with
      | .pending => "pending" | .running => "running" | .done => "done" | .failed => "failed"
      | .unfinished => "unfinished" | .cancelled => "cancelled"
    IO.println s!"{padRight e.id 16} {padRight e.createdAt 20} {padRight e.fork 28} {padRight status 10} {e.series.getD ""}"
  return (0 : UInt32)

private def queueListenersHandler (p : Parsed) : IO UInt32 := do
  let listenerDir := p.flag? "listener_dir" |>.map (·.as! String)
  let lDir ← match listenerDir with
    | some d => pure (System.FilePath.mk d)
    | none   => Listener.listenersDir
  let configs ← Listener.loadAllListenerConfigs lDir
  if configs.isEmpty then
    IO.println s!"No listeners found in {lDir}"
    return (0 : UInt32)
  IO.println s!"{padRight "LISTENER" 20} {padRight "INTERVAL" 9} {padRight "LAST CHECKED" 22} PROCESSED"
  IO.println (String.ofList (List.replicate 70 '-'))
  for cfg in configs do
    let state ← Listener.loadListenerState cfg.name
    let lastChecked := if state.lastChecked.isEmpty then "never" else state.lastChecked
    let processed   := toString state.processedIds.size ++ " events"
    let interval    := s!"{cfg.intervalSeconds}s"
    IO.println s!"{padRight cfg.name 20} {padRight interval 9} {padRight lastChecked 22} {processed}"
  return (0 : UInt32)

private def queueStatusHandler (_ : Parsed) : IO UInt32 := do
  -- Daemon status
  if ← Queue.daemonRunning then
    match ← Queue.readPid with
    | some pid => IO.println s!"Daemon: running (PID {pid})"
    | none     => IO.println "Daemon: running"
  else
    IO.println "Daemon: not running"
  -- Running and pending entries only
  let all ← Queue.loadAllEntries
  let active := all.filter (fun e => e.status == .running || e.status == .pending)
  if active.isEmpty then
    IO.println "Queue: empty"
    return 0
  IO.println s!"Queue: {active.size} task(s)"
  IO.println ""
  IO.println s!"{padRight "ID" 16} {padRight "FORK" 32} {padRight "STATUS" 9} SERIES"
  IO.println (String.ofList (List.replicate 70 '-'))
  -- Show running first, then pending in oldest-first order
  let running := active.filter (fun e => e.status == .running)
  let pending := (active.filter (fun e => e.status == .pending)).toList.reverse.toArray
  for e in running ++ pending do
    let status := if e.status == .running then "running" else "pending"
    IO.println s!"{padRight e.id 16} {padRight e.fork 32} {padRight status 9} {e.series.getD ""}"
  return 0

private def queueShutdownHandler (p : Parsed) : IO UInt32 := do
  let force := p.hasFlag "force"
  if !(← Queue.daemonRunning) then
    IO.eprintln "Queue daemon is not running."
    return 1
  if force then
    Queue.requestCancel
    IO.println "Sent cancel request."
  Queue.requestShutdown
  if force then
    IO.println "Sent shutdown request. Daemon will stop after cancelling the current task."
  else
    IO.println "Sent shutdown request. Daemon will stop after the current task finishes."
  return 0

private def queueCancelHandler (_ : Parsed) : IO UInt32 := do
  if !(← Queue.daemonRunning) then
    IO.eprintln "Queue daemon is not running."
    return 1
  Queue.requestCancel
  IO.println "Cancel request sent. The current task will be stopped."
  return 0

-- CLI definitions

private def runCmd' : Cmd := `[Cli|
  run VIA runHandler; ["0.1.0"]
  "Run coding agent tasks from a task file."

  FLAGS:
    c, config : String; "Path to config file (default: ~/.agent/config.json)"
    t, task : Nat; "Run only the task at this index (0-based)"
    d, debug; "Print the landrun command before executing it"
    continues : String; "Continue from a previous task by ID (requires --task with multi-task files)"
    series : String; "Assign this run to a named task series"
    budget : String; "Maximum spend in USD, overrides task file (default: 4.0)"

  ARGS:
    "task-file" : String; "Path to the JSON task file"
]

private def mcpServerCmd : Cmd := `[Cli|
  mcp VIA mcpServerHandler; ["0.1.0"]
  "Start the MCP server and print the port it is listening on."

  FLAGS:
    c, config : String; "Path to config file (default: ~/.agent/config.json)"
    allow_pr; "Allow the create_pr tool (disabled by default)"

  ARGS:
    "upstream" : String; "Upstream repository in 'owner/repo' format"
    "fork" : String; "Fork repository in 'owner/repo' format"
]

private def prepareCmd : Cmd := `[Cli|
  prepare VIA prepareHandler; ["0.1.0"]
  "Clone the fork and configure the upstream remote."

  ARGS:
    "upstream" : String; "Upstream repository in 'owner/repo' format"
    "fork" : String; "Fork repository in 'owner/repo' format"
]

private def cleanupCmd : Cmd := `[Cli|
  cleanup VIA cleanupHandler; ["0.1.0"]
  "Remove all cloned repositories."
]

private def tasksCmd : Cmd := `[Cli|
  tasks VIA tasksHandler; ["0.1.0"]
  "List recent task runs."

  FLAGS:
    limit : Nat; "Maximum number of tasks to show (default: 20)"
]

private def taskCmd : Cmd := `[Cli|
  task VIA taskShowHandler; ["0.1.0"]
  "Show details of a task run."

  ARGS:
    "id" : String; "Task ID"
]

private def seriesCmd : Cmd := `[Cli|
  series VIA seriesHandler; ["0.1.0"]
  "List all task series."
]

private def tagCmd : Cmd := `[Cli|
  tag VIA tagHandler; ["0.1.0"]
  "Add a completed task to a series, making it the latest entry."

  ARGS:
    "id" : String; "Task ID to tag"
    "series" : String; "Series name"
]

private def resumeCmd : Cmd := `[Cli|
  resume VIA resumeHandler; ["0.1.0"]
  "Resume the latest run in a series with a new prompt."

  FLAGS:
    c, config : String; "Path to config file (default: ~/.agent/config.json)"
    p, prompt : String; "Prompt for the new agent run"
    d, debug; "Print the landrun command before executing it"
    budget : String; "Maximum spend in USD (default: inherited from previous task, or 4.0)"

  ARGS:
    "series" : String; "Series name to resume"
]

private def queueAddCmd : Cmd := `[Cli|
  add VIA enqueueHandler; ["0.1.0"]
  "Add tasks to the queue from a task file, or continue a series with a new prompt."

  FLAGS:
    c, config : String; "Path to config file (default: ~/.agent/config.json)"
    t, task : Nat; "Enqueue only the task at this index (0-based, task-file mode only)"
    continues : String; "Continue from a previous task by ID (task-file mode only)"
    series : String; "Assign queued task(s) to a named series (task-file mode only)"
    r, resume : String; "Continue the latest run in a named series (requires --prompt)"
    p, prompt : String; "Prompt for the new agent run (used with --resume)"
    budget : String; "Maximum spend in USD, overrides task file (default: 4.0)"

  ARGS:
    ..."task-file" : String; "Path to the JSON task file (omit when using --resume)"
]

private def queueStartCmd : Cmd := `[Cli|
  start VIA queueStartHandler; ["0.1.0"]
  "Start the queue daemon. Polls for pending tasks and runs them serially."

  FLAGS:
    c, config : String; "Path to config file (default: ~/.agent/config.json)"
    d, debug; "Print the landrun command before executing it"
    listener_dir : String; "Directory of listener configs (default: ~/.agent/listeners/)"
    b, background; "Run the daemon in the background, detached from the terminal"
]

private def queueStatusCmd : Cmd := `[Cli|
  status VIA queueStatusHandler; ["0.1.0"]
  "Show daemon status and currently queued tasks."
]

private def queueShutdownCmd : Cmd := `[Cli|
  shutdown VIA queueShutdownHandler; ["0.1.0"]
  "Stop the queue daemon gracefully after the current task finishes."

  FLAGS:
    f, force; "Cancel the current task immediately and shut down"
]

private def queueCancelCmd : Cmd := `[Cli|
  cancel VIA queueCancelHandler; ["0.1.0"]
  "Cancel the currently running task (daemon continues with remaining queued tasks)."
]

private def queueRetryHandler (p : Parsed) : IO UInt32 := do
  let seriesFilter := p.flag? "series" |>.map (·.as! String)
  let all ← Queue.loadAllEntries
  -- Collect unfinished and cancelled entries, optionally filtered by series,
  -- reversed so we enqueue them in original (oldest-first) order
  let retryable := (all.filter (fun e =>
    (e.status == .unfinished || e.status == .cancelled) &&
    match seriesFilter with
    | none   => true
    | some s => e.series == some s)).toList.reverse
  if retryable.isEmpty then
    IO.println "No unfinished or cancelled entries to retry."
    return (0 : UInt32)
  for entry in retryable do
    let id ← TaskStore.generateId
    let createdAt ← TaskStore.currentIso8601
    -- Unfinished tasks: continue from their partial session (taskId).
    -- Cancelled tasks: keep the original continuesFrom (they never ran).
    let continuesFrom := match entry.status with
      | .unfinished => entry.taskId
      | _           => entry.continuesFrom
    let newEntry : Queue.QueueEntry := {
      id, createdAt
      upstream     := entry.upstream
      fork         := entry.fork
      mode         := entry.mode
      prompt       := entry.prompt
      agent        := entry.agent
      systemPrompt := entry.systemPrompt
      backend      := entry.backend
      model        := entry.model
      continuesFrom
      series       := entry.series
      configPath   := entry.configPath
    }
    Queue.saveEntry newEntry
    IO.println newEntry.id
  return (0 : UInt32)

private def queueRetryCmd : Cmd := `[Cli|
  retry VIA queueRetryHandler; ["0.1.0"]
  "Re-enqueue all unfinished and cancelled queue entries."

  FLAGS:
    series : String; "Only retry entries belonging to this series"
]

private def queueListenersCmd : Cmd := `[Cli|
  listeners VIA queueListenersHandler; ["0.1.0"]
  "List configured listeners and their polling state."

  FLAGS:
    listener_dir : String; "Directory of listener configs (default: ~/.agent/listeners/)"
]

private def queueCmd : Cmd := `[Cli|
  queue VIA queueListHandler; ["0.1.0"]
  "Manage the task queue."

  FLAGS:
    limit : Nat; "Maximum number of entries to show (default: 20)"

  SUBCOMMANDS:
    queueAddCmd;
    queueStartCmd;
    queueStatusCmd;
    queueShutdownCmd;
    queueCancelCmd;
    queueRetryCmd;
    queueListenersCmd
]

private def defaultHandler (_ : Parsed) : IO UInt32 := do
  IO.eprintln "Use a subcommand. Try 'orchestra --help'."
  return 1

def orchestraCmd : Cmd := `[Cli|
  orchestra VIA defaultHandler; ["0.1.0"]
  "CLI tool for managing and sandboxing coding agents."

  SUBCOMMANDS:
    runCmd';
    mcpServerCmd;
    prepareCmd;
    cleanupCmd;
    tasksCmd;
    taskCmd;
    seriesCmd;
    tagCmd;
    resumeCmd;
    queueCmd
]

def main (args : List String) : IO UInt32 := do
  gRawArgs.set args
  orchestraCmd.validate args
