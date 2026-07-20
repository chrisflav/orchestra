import Orchestra
import Orchestra.Migrate
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

private def stripExt (s ext : String) : String :=
  if s.endsWith ext then
    s.dropEnd ext.length |>.toString
  else s

/-- Single-quote a string for safe use in a POSIX shell command. -/
private def shellQuote (s : String) : String :=
  "'" ++ s.replace "'" "'\\''" ++ "'"

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

private def isWorkflowFile (path : String) : Bool :=
  path.endsWith ".yaml" || path.endsWith ".yml"

private def parseVarsJson (s : String) : List (String × Lean.Json) :=
  match Lean.Json.parse s with
  | .error _ => []
  | .ok j    => (j.getObj? |>.toOption |>.getD {}).toList

private def runHandler (p : Parsed) : IO UInt32 := do
  let taskFile      := p.positionalArg! "task-file" |>.as! String
  let configPath    := p.flag? "config"    |>.map (·.as! String)
  let taskIdx       := p.flag? "task"      |>.map (·.as! Nat)
  let debug         := p.hasFlag "debug"
  let continuesFrom := p.flag? "continues" |>.map (·.as! String)
  let series        := p.flag? "series"    |>.map (·.as! String)
  let budgetFlag    := p.flag? "budget"    |>.bind (fun v => parseFloat? (v.as! String))
  let initVars      := p.flag? "vars"      |>.map (fun v => parseVarsJson (v.as! String)) |>.getD []
  let appConfig ← loadAppConfig (configPath.map System.FilePath.mk)
  if isWorkflowFile taskFile then
    let yaml ← IO.FS.readFile taskFile
    match Workflow.WorkflowProgram.parseYaml yaml with
    | .error e =>
      IO.eprintln s!"Failed to parse workflow: {e}"
      return 1
    | .ok prog =>
      let concert := Workflow.WorkflowProgram.toConcert prog initVars
      Concert.eval appConfig debug none concert
      return 0
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
      -- CLI flags override the task file values
      let task := match budgetFlag with
        | none   => tasks[i]!
        | some b =>
          let t := tasks[i]!
          { t with ioTask := { t.ioTask with budget := some b } }
      let _ ← TaskRunner.runTask appConfig task i debug (continuesFrom := continuesFrom) (series := series)
    catch e =>
      IO.eprintln s!"Task {i} failed: {e}"

  return (0 : UInt32)

private def mcpServerHandler (p : Parsed) : IO UInt32 := do
  let upstream ← IO.ofExcept (Repository.parse (p.positionalArg! "upstream" |>.as! String))
  let fork ← IO.ofExcept (Repository.parse (p.positionalArg! "fork" |>.as! String))
  let allowPR := p.hasFlag "allow_pr"
  let configPath := p.flag? "config" |>.map (·.as! String)
  let appConfig ← loadAppConfig (configPath.map System.FilePath.mk)
  let jwt ← GitHub.createJWT appConfig.appId appConfig.privateKeyPath
  let installationId ← match appConfig.installationId with
    | some id => pure id
    | none => GitHub.getInstallationId jwt fork.owner
  let token ← GitHub.createInstallationToken jwt installationId
  GitHub.setupGhAuth token
  let serverState : Server.State := {
    upstream, fork
    allowedTools := if allowPR then ["create_pr"] else []
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
  let upstream ← IO.ofExcept (Repository.parse (p.positionalArg! "upstream" |>.as! String))
  let fork ← IO.ofExcept (Repository.parse (p.positionalArg! "fork" |>.as! String))
  let slots := max 1 (p.flag? "slots" |>.map (·.as! Nat) |>.getD 1)
  -- Queue tasks never run in the cache clone, they run in slots — so preparing only the
  -- cache clone would warm a directory the daemon never works in, and every slot would
  -- still pay a full cold build on its first task. `git clone --local` does not carry
  -- gitignored build output across either, so each slot has to be initialised on its own.
  let repoPath ← Repo.ensureCloned fork upstream
  IO.println repoPath.toString
  for slot in List.range slots do
    let slotPath ← Repo.ensureSlot fork upstream { slot }
    -- Run the repo's init hook now rather than inside the first task that lands here, so a
    -- toolchain install or a cold `lake exe cache get` is paid up front instead of counting
    -- against that task's budget and wall clock.
    RepoConfig.runInitIfNeeded slotPath
    IO.println slotPath.toString
  return (0 : UInt32)

private def cleanupHandler (_ : Parsed) : IO UInt32 := do
  Repo.cleanup
  return (0 : UInt32)

private def cleanupListHandler (_ : Parsed) : IO UInt32 := do
  let clones ← Repo.listClones
  if clones.isEmpty then
    IO.println "No repository clones found."
    return (0 : UInt32)
  for (mainPath, slots) in clones do
    IO.println s!"  {mainPath}"
    for slot in slots do
      IO.println s!"    slot: {slot}"
  return (0 : UInt32)

private def tasksHandler (p : Parsed) : IO UInt32 := do
  let limit := p.flag? "limit" |>.map (·.as! Nat) |>.getD 20
  let records := (← TaskStore.loadAllTasks).toList.take limit
  if records.isEmpty then
    IO.println "No tasks found."
    return (0 : UInt32)
  let queueEntries ← Queue.loadAllEntries
  let allConcerts ← Queue.loadAllConcertRuns
  IO.println s!"{padRight "ID" 16} {padRight "CREATED" 20} {padRight "FORK" 28} {padRight "STATUS" 11} {padRight "SERIES" 16} CONCERT"
  IO.println (String.ofList (List.replicate 117 '-'))
  for r in records do
    let status := match r.status with
      | .running => "running" | .completed => "completed" | .failed => "failed"
      | .unfinished => "unfinished" | .cancelled => "cancelled"
    let concertLabel :=
      let mConcert : Option Queue.ConcertRun := do
        let e ← queueEntries.find? (fun e => e.taskId == some r.id)
        let cid ← e.concertId
        allConcerts.find? (fun cr => cr.id == cid)
      match mConcert with
      | some run => run.id
      | none     => ""
    let seriesLabel := r.series.getD ""
    IO.println s!"{padRight r.id 16} {padRight r.createdAt 20} {padRight r.fork.toString 28} {padRight status 11} {padRight seriesLabel 16} {concertLabel}"
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

-- The project / issue subcommands live in `Orchestra.Project.Cli` so the
-- domain code stays self-contained. We only re-export the top-level cmds
-- here so the macro that builds `orchestraCmd` can find them by name.
open Orchestra.Project.Cli (projectCmd issueCmd spawnCmd rolesCmd)

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
    i := .unit, o := .unit
    ioTask := {
      upstream      := prevRecord.upstream
      fork          := prevRecord.fork
      mode          := prevRecord.mode
      prompt
      backend       := prevRecord.backend
      model         := prevRecord.model
      agent         := prevRecord.agent
      systemPrompt  := prevRecord.systemPrompt
      prependPrompt := prevRecord.prependPrompt
      budget        := budgetFlag.orElse (fun _ => prevRecord.budget)
      priority      := prevRecord.priority
    }
  }
  let _ ← TaskRunner.runTask appConfig task 0 debug
    (continuesFrom := some prevId) (series := some seriesName)
  return (0 : UInt32)

-- Queue helpers

/-- Send a JSON request to the daemon socket and return the parsed response.
    Throws if the daemon returns an "error" field. -/
private def daemonRequest (req : Lean.Json) : IO Lean.Json := do
  let socketPath ← Queue.socketFile
  let conn ← Utils.UnixSocket.Connection.connect socketPath
  conn.sendLine req.compress
  let line ← conn.recvLine
  conn.close
  let resp ← IO.ofExcept (Lean.Json.parse line |>.mapError IO.userError)
  if let .ok msg := resp.getObjValAs? String "error" then
    throw (IO.userError s!"Daemon error: {msg}")
  return resp

private def enqueueHandler (p : Parsed) : IO UInt32 := do
  if !(← Queue.daemonRunning) then
    IO.eprintln "Queue daemon is not running. Start it with 'orchestra queue start'."
    return 1
  let configPath    := p.flag? "config"    |>.map (·.as! String)
  let taskIdx       := p.flag? "task"      |>.map (·.as! Nat)
  let continuesFrom := p.flag? "continues" |>.map (·.as! String)
  let series        := p.flag? "series"    |>.map (·.as! String)
  let resumeSeries  := p.flag? "resume"    |>.map (·.as! String)
  let prompt        := p.flag? "prompt"    |>.map (·.as! String)
  let budgetFlag    := p.flag? "budget"    |>.bind (fun v => parseFloat? (v.as! String))
  let priorityFlag  := p.flag? "priority"  |>.map (·.as! Nat)
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
      prependPrompt := prevRecord.prependPrompt
      budget        := budgetFlag.orElse (fun _ => prevRecord.budget)
      priority      := priorityFlag.getD prevRecord.priority
    }
    let req := Lean.Json.mkObj [("type", "add_task"), ("entry", Lean.ToJson.toJson entry)]
    let _ ← daemonRequest req
    IO.println entry.id
    return (0 : UInt32)
  | none, some taskFile =>
    -- Workflow-file mode: validate the YAML locally then ask the daemon to start the concert.
    if isWorkflowFile taskFile then
      let yaml ← IO.FS.readFile taskFile
      match Workflow.WorkflowProgram.parseYaml yaml with
      | .error e =>
        IO.eprintln s!"Failed to parse workflow: {e}"
        return 1
      | .ok _ =>
        let fp := System.FilePath.mk taskFile
        let absTaskFile ← if fp.isAbsolute then pure taskFile
          else pure ((← IO.currentDir) / taskFile |>.toString)
        let vars := p.flag? "vars" |>.map (fun v => parseVarsJson (v.as! String)) |>.getD []
        let varsJson := if vars.isEmpty then Lean.Json.mkObj [] else Lean.Json.mkObj vars
        let req := Lean.Json.mkObj
          [ ("type",          "add_concert")
          , ("workflow_file", absTaskFile)
          , ("vars",          varsJson)
          , ("config_path",   match configPath with
                               | some c => c
                               | none   => Lean.Json.null) ]
        let resp ← daemonRequest req
        match resp.getObjValAs? String "id" with
        | .ok id => IO.println id
        | .error _ => pure ()
        return 0
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
        upstream      := task.ioTask.upstream
        fork          := task.ioTask.fork
        mode          := task.ioTask.mode
        prompt        := task.ioTask.prompt
        agent         := task.ioTask.agent
        systemPrompt  := task.ioTask.systemPrompt
        prependPrompt := task.ioTask.prependPrompt
        backend       := task.ioTask.backend
        model         := task.ioTask.model
        continuesFrom, series
        configPath
        budget           := budgetFlag.orElse (fun _ => task.ioTask.budget)
        authSource       := task.ioTask.authSource
        tools            := task.ioTask.tools
        readOnly         := task.ioTask.readOnly
        priority         := priorityFlag.getD task.ioTask.priority
        issueNumber      := task.ioTask.issueNumber
      }
      let req := Lean.Json.mkObj [("type", "add_task"), ("entry", Lean.ToJson.toJson entry)]
      let _ ← daemonRequest req
      IO.println entry.id
    return (0 : UInt32)

private def handleSocketRequest
    (conn             : Utils.UnixSocket.Connection)
    (appConfig        : Orchestra.AppConfig)
    (concertMgr       : ConcertManager.ConcertManager)
    (debug            : Bool)
    (shutdownToken    : Std.CancellationToken)
    (activeTaskTokens : Std.Mutex (Array (Nat × Std.CancellationToken)))
    : IO Unit := do
  try
    let line ← conn.recvLine
    let response : DaemonRequest.DaemonResponse ← match Lean.Json.parse line with
      | .error e => pure (.error s!"invalid JSON: {e}")
      | .ok j    =>
        match (Lean.FromJson.fromJson? j : Except String DaemonRequest.DaemonRequest) with
        | .error e => pure (.error s!"invalid request: {e}")
        | .ok msg  => match msg with
        | .addTask entry =>
          Queue.saveEntry entry
          pure (.withId entry.id)
        | .addConcert wfPath vars cfgPath =>
          let result : Except String String ← try
            let yaml ← IO.FS.readFile wfPath
            match Workflow.WorkflowProgram.parseYaml yaml with
            | .error e => pure (Except.error s!"workflow parse failed: {e}")
            | .ok prog =>
              let concertId ← TaskStore.generateId
              let varsList := vars
                |>.bind (·.getObj?.toOption)
                |>.map (·.toList)
                |>.getD []
              let cfg ← match cfgPath with
                | none    => pure appConfig
                | some cp => loadAppConfig (some (System.FilePath.mk cp))
              let concert := Workflow.WorkflowProgram.toConcert prog varsList
              let run : Queue.ConcertRun := {
                id           := concertId
                startedAt    := ← TaskStore.currentIso8601
                name         := if prog.name.isEmpty then none else some prog.name
                workflowFile := some wfPath
              }
              Queue.saveConcertRun run
              IO.println s!"  Concert {concertId}: starting from {wfPath}"
              let _concertTask ← IO.asTask (prio := .dedicated) do
                try
                  Concert.evalQueued concertMgr cfg debug none (some concertId) concert
                  let t ← TaskStore.currentIso8601
                  Queue.saveConcertRun { run with status := .done, finishedAt := some t }
                catch e =>
                  IO.eprintln s!"  Concert {concertId} failed: {e}"
                  let t ← TaskStore.currentIso8601
                  Queue.saveConcertRun { run with status := .failed, finishedAt := some t }
              pure (Except.ok concertId)
          catch e => pure (Except.error s!"failed to start concert: {e}")
          match result with
          | .ok id   => pure (.withId id)
          | .error e => pure (.error e)
        | .cancel =>
          let pairs ← activeTaskTokens.atomically (·.get)
          for (_, token) in pairs do
            token.cancel .cancel
          pure DaemonRequest.DaemonResponse.ok
        | .shutdown force =>
          if force then
            let pairs ← activeTaskTokens.atomically (·.get)
            for (_, token) in pairs do
              token.cancel .cancel
          shutdownToken.cancel .shutdown
          pure DaemonRequest.DaemonResponse.ok
        | .claimIssue pid iid taskId agent series =>
          let now ← TaskStore.currentIso8601
          match ← Project.tryClaim TaskRunner.globalClaimManager pid iid taskId agent now series with
          | .acquired _            => pure (.withId taskId)
          | .alreadyClaimed exist  => pure (.error s!"already_claimed by task {exist.taskId}")
          | .invalid reason        => pure (.error reason)
    conn.sendLine (Lean.ToJson.toJson response).compress
  catch e =>
    IO.eprintln s!"Socket request error: {e}"
  try conn.close catch _ => pure ()

private def queueStartHandler (p : Parsed) : IO UInt32 := do
  let configPath           := p.flag? "config" |>.map (·.as! String)
  let debug                := p.hasFlag "debug"
  let background           := p.hasFlag "background"
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
  let pid ← Queue.ownPid
  Queue.writePid pid
  IO.println s!"Queue daemon started (PID {pid})"
  -- Startup cleanup
  Queue.markStaleRunningAsUnfinished
  Queue.cancelStaleConcertEntries
  Queue.cancelStaleRunningConcerts
  let appConfig ← loadAppConfig (configPath.map System.FilePath.mk)
  -- Concurrency limits: the `queue` block in config.json, overridden by the flags for a single
  -- run. Resolved here rather than at the top of the handler because it needs the config, and
  -- `max 1` because zero workers would be a daemon that silently never runs anything.
  let parallelLimit := max 1 <|
    (p.flag? "parallel" |>.map (·.as! Nat)).getD appConfig.queue.parallel
  let parallelLimitPerRepo := max 1 <|
    (p.flag? "parallel-per-repo" |>.map (·.as! Nat)).getD appConfig.queue.parallelPerRepo
  -- Shared concurrency primitives
  let shutdownToken  ← Std.CancellationToken.new
  -- Map of (id → cancel token) for all currently running tasks (one per worker).
  let activeTaskTokens ← Std.Mutex.new (Array.empty : Array (Nat × Std.CancellationToken))
  let nextTokenId ← IO.mkRef (0 : Nat)
  -- Mutex serialising the "find next pending + mark running" claim operation.
  let claimMutex ← Std.BaseMutex.new
  -- Tracks which task slots are currently occupied per repo (fork key), and the total
  -- number of running tasks. Both are protected by claimMutex.
  let activeSlots ← IO.mkRef ({} : Std.HashMap String (Array Nat))
  let totalActive ← IO.mkRef (0 : Nat)
  -- Set while a task on a backend that is not parallel-safe holds the daemon exclusively.
  -- Also protected by claimMutex.
  let exclusiveActive ← IO.mkRef false
  -- Concert manager: handles suspended concert fibers waiting for task results.
  let concertMgr ← ConcertManager.new
  -- Socket server: receives control requests (add_task, add_concert, cancel, shutdown).
  let socketPath ← Queue.socketFile
  try Utils.UnixSocket.Server.unlink socketPath catch _ => pure ()
  let socketServerRef ← IO.mkRef (none : Option Utils.UnixSocket.Server)
  let _socketTask ← IO.asTask (prio := .dedicated) do
    try
      let server ← Utils.UnixSocket.Server.listen socketPath
      socketServerRef.set (some server)
      repeat do
        let conn ← server.accept
        let _h ← IO.asTask (prio := .dedicated) do
          handleSocketRequest conn appConfig concertMgr debug shutdownToken activeTaskTokens
    catch _ => pure ()
  -- Signal watcher: turns SIGTERM/SIGINT into the same graceful drain as `queue shutdown`.
  -- Needed because the daemon is PID 1 in the container image, where `docker stop` is the way in
  -- and `queue shutdown` is not: draining works, but the daemon's exit stops the container and
  -- the restart policy immediately brings up a new one. Handling the signal is also what makes
  -- it deliverable at all — PID 1 ignores signals left at their default disposition, so before
  -- this, `docker stop` fell through to SIGKILL and left in-flight entries stuck in `running`.
  --
  -- Polled rather than acted on inside the handler itself: a signal handler may not touch the
  -- Lean heap, so it only bumps a counter (ffi/Signal.c) and the real work happens here. The
  -- 200ms tick is well inside any sane `docker stop` grace period and costs nothing when idle.
  Utils.Signals.install
  let _signalTask ← IO.asTask (prio := .dedicated) do
    let mut announced := false
    repeat
      let n ← Utils.Signals.count
      if n > 0 && !announced then
        announced := true
        IO.println "Received termination signal; finishing in-flight tasks before shutting down."
        IO.println "Send it again to cancel them instead."
        shutdownToken.cancel .shutdown
      -- A second signal escalates to `queue shutdown --force`: whoever is stopping us has said
      -- once that they are willing to wait, and then changed their mind.
      if n > 1 then
        IO.println "Second termination signal; cancelling in-flight tasks."
        let pairs ← activeTaskTokens.atomically (·.get)
        for (_, token) in pairs do
          token.cancel .cancel
        break
      IO.sleep 200
  -- Helper: atomically claim the next pending entry, marking it as running.
  -- Serialised by claimMutex so that multiple workers cannot claim the same entry.
  -- Returns (entry, slot, tokenId, reuseTree):
  --   slot       index of the per-repo task slot reserved for this entry. Slots are
  --              independent clones, so the entry's agent can create any branch name it
  --              likes without colliding with another task running on the same repository.
  --   tokenId    cancellation-token id, allocated here rather than in `runEntry` because
  --              `IO.Ref.modifyGet` is not atomic across workers: two of them racing on it
  --              are handed the same id, and `removeToken` then drops the other task's
  --              token, leaving `queue cancel` unable to reach a running task.
  --   resumeFrom set when this entry continues a previous task and got that task's slot back
  --              *with its tree intact*, naming the predecessor entry.
  -- The decision itself lives in `Queue.claimDecision`, which is a function of its inputs and
  -- therefore testable; this wrapper is only the locking and the bookkeeping around it.
  -- Both helpers unlock via `finally`: reading the queue directory and saving an entry
  -- are file operations that can throw, and leaving the mutex held would wedge every
  -- worker in the pool for the rest of the daemon's life.
  let claimNextEntry : IO (Option (Queue.Claim × Nat)) := do
    claimMutex.lock
    try
      let slotMap ← activeSlots.get
      let ctx : Queue.ClaimContext := {
        occupiedSlots   := slotMap
        total           := ← totalActive.get
        exclusiveActive := ← exclusiveActive.get
        parallelLimit
        perRepoLimit    := parallelLimitPerRepo
        parallelSafe    := TaskRunner.backendIsParallelSafe
      }
      let allEntries ← Queue.loadAllEntries
      let some claim ← Queue.claimDecision ctx allEntries Repo.slotOccupant | return none
      let e := claim.entry
      let occupied := slotMap.getD e.fork.toString #[]
      let tokenId ← nextTokenId.modifyGet (fun n => (n, n + 1))
      Queue.saveEntry { e with status := .running, slot := some claim.slot }
      activeSlots.modify (fun m => m.insert e.fork.toString (occupied.push claim.slot))
      totalActive.modify (· + 1)
      if !TaskRunner.backendIsParallelSafe e.backend then exclusiveActive.set true
      if e.continuesFrom.isSome && claim.resumeFrom.isNone then
        IO.eprintln s!"  Note: queue entry {e.id} continues a previous task but no longer has \
its workspace; it will start from a clean checkout."
      return some (claim, tokenId)
    finally
      claimMutex.unlock
  -- Helper: release the slot held by a completed entry.
  let releaseEntry (entry : Queue.QueueEntry) (slot : Nat) : IO Unit := do
    claimMutex.lock
    try
      let occupied := (← activeSlots.get).getD entry.fork.toString #[]
      activeSlots.modify (fun m => m.insert entry.fork.toString (occupied.filter (· != slot)))
      totalActive.modify (fun t => if t > 0 then t - 1 else 0)
      if !TaskRunner.backendIsParallelSafe entry.backend then exclusiveActive.set false
    finally
      claimMutex.unlock
  -- Helper: run one queue entry to completion and update its status.
  -- Also signals the ConcertManager if the entry belongs to a concert.
  -- slot: index of the per-repo task slot reserved for this entry by `claimNextEntry`.
  -- The slot directory is prepared (created if absent, otherwise reset to a clean default
  -- branch) inside the try below. The slot is released by `runEntry`, which wraps this.
  let runEntryBody (entry : Queue.QueueEntry) (slot : Nat) (tokenId : Nat)
      (resumeFrom : Option String) : IO Unit := do
    let taskToken ← Std.CancellationToken.new
    activeTaskTokens.atomically (·.modify (·.push (tokenId, taskToken)))
    let removeToken : IO Unit :=
      activeTaskTokens.atomically (·.modify (·.filter (·.1 != tokenId)))
    -- Terminal writes go through here rather than saving the claim-time snapshot: a socket
    -- `cancel`, a listener, or a cascade from another worker can rewrite this entry's file
    -- while the task runs, and writing the stale snapshot back would silently revert it.
    let finish (status : Queue.QueueStatus) (taskId : Option String)
        (outputJson : Option Lean.Json) : IO Unit := do
      let cur := (← Queue.loadEntry entry.id).getD entry
      Queue.saveEntry { cur with
        status
        taskId     := taskId.orElse (fun _ => cur.taskId)
        outputJson := outputJson.orElse (fun _ => cur.outputJson) }
    let task : Task := {
      i := entry.inputType, o := entry.outputType
      ioTask := {
        upstream         := entry.upstream
        fork             := entry.fork
        mode             := entry.mode
        prompt           := entry.prompt
        agent            := entry.agent
        systemPrompt     := entry.systemPrompt
        prependPrompt    := entry.prependPrompt
        backend          := entry.backend
        model            := entry.model
        budget           := entry.budget
        memory           := entry.memory
        authSource       := entry.authSource
        tools            := entry.tools
        readOnly         := entry.readOnly
        priority         := entry.priority
        issueNumber        := entry.issueNumber
        projectId          := entry.projectId
        issueId            := entry.issueId
        role               := entry.role
        prLabels           := entry.prLabels
        triageAddLabels    := entry.triageAddLabels
        triageRemoveLabels := entry.triageRemoveLabels
      }
    }
    -- If this entry holds a pre-claimed issue, release it back to open on any
    -- unhandled exception so the issue never gets permanently stuck.
    let releaseClaimOnError : IO Unit := do
      match entry.projectId, entry.issueId with
      | some pid, some iid =>
        let now ← TaskStore.currentIso8601
        let _ ← Project.release TaskRunner.globalClaimManager pid iid .open now
      | _, _ => pure ()
    try
      -- Inside the `try` so that a bad per-entry config path is reported as a failed
      -- entry rather than escaping and taking the worker down with it.
      let cfg ← match entry.configPath with
        | none    => pure appConfig
        | some cp => loadAppConfig (some (System.FilePath.mk cp))
      -- Preparing the slot sits inside the `try` so that a failure here — a clone that
      -- cannot be created, a fetch that cannot reach GitHub — is reported through the same
      -- path as any other task failure: the entry is marked failed, a pre-claimed issue is
      -- released, and a concert waiting on this step is signalled instead of hanging until
      -- the daemon shuts down.
      -- `resumeFrom` for a continuation that got its predecessor's slot back: the resumed
      -- agent's context describes a tree it expects to still be there. `ensureSlot` re-checks
      -- that the tree really is the predecessor's before keeping it.
      -- `occupant` stamps this entry as the owner of the tree it leaves behind, which is what
      -- a later continuation of *this* task will check.
      -- `runTask` prepares the slot itself, since doing so needs the installation token.
      let (taskId, usageLimitHit, outputJson) ← TaskRunner.runTask cfg task 0 debug
        (continuesFrom := entry.continuesFrom) (series := entry.series)
        (cancelToken := some taskToken) (interactive := false)
        (slotOverride := some { slot, occupant := some entry.id, resumeFrom })
      removeToken
      -- The sandbox always cancels taskToken with `.custom "done"` on normal exit, so
      -- `isCancelled` is true for both normal completion and watcher-triggered cancellation.
      -- Check the reason to distinguish the two cases.
      let explicitlyCancelled := (← taskToken.getCancellationReason) == some .cancel
      if explicitlyCancelled then
        finish .cancelled (some taskId) none
        IO.println s!"  Task cancelled."
        ConcertManager.signal concertMgr (entry.concertStepKey.getD "") outputJson
      else if usageLimitHit then
        finish .unfinished (some taskId) none
        Queue.cancelPendingByBackend entry.backend entry.id
        Queue.cancelDependents taskId
        IO.println s!"  Cancelled pending {entry.backend.getD "claude"} tasks and dependents."
        ConcertManager.signal concertMgr (entry.concertStepKey.getD "") none
      else
        finish .done (some taskId) outputJson
        if let some key := entry.concertStepKey then
          ConcertManager.signal concertMgr key outputJson
    catch e =>
      removeToken
      let explicitlyCancelled := (← taskToken.getCancellationReason) == some .cancel
      if explicitlyCancelled then
        IO.eprintln s!"  Task cancelled (with error: {e})"
        try finish .cancelled none none catch _ => pure ()
        ConcertManager.signal concertMgr (entry.concertStepKey.getD "") none
      else
        IO.eprintln s!"Queue entry {entry.id} failed: {e}"
        try finish .failed none none catch _ => pure ()
        try releaseClaimOnError catch _ => pure ()
        ConcertManager.signal concertMgr (entry.concertStepKey.getD "") none
  -- The release wraps the *whole* body, not just the part that runs the task: allocating the
  -- cancellation token and registering it are themselves IO, and an exception there would
  -- otherwise escape past the release and leak the slot. A leaked slot is never recovered, so
  -- a handful of them permanently shrink the pool until the daemon is restarted.
  let runEntry (entry : Queue.QueueEntry) (slot : Nat) (tokenId : Nat)
      (resumeFrom : Option String) : IO Unit := do
    try runEntryBody entry slot tokenId resumeFrom
    finally releaseEntry entry slot
  -- Load listener configs and spawn one fiber per listener.
  let listenerConfigs ← Listener.loadAllListenerConfigs
  if !listenerConfigs.isEmpty then
    IO.println s!"Loaded {listenerConfigs.size} listener(s)"
  for lcfg in listenerConfigs do
    let _listenerTask ← IO.asTask (prio := .dedicated) do
      -- Fire immediately on first iteration, then respect the configured interval.
      let mut firstRun := true
      while !(← shutdownToken.isCancelled) do
        if !firstRun then
          IO.sleep (lcfg.intervalSeconds * 1000).toUInt32
        firstRun := false
        try
          -- Re-read the config each tick so config changes take effect live.
          let liveCfg := (← Listener.loadListenerConfig lcfg.name).getD lcfg
          let state  ← Listener.loadListenerState lcfg.name
          if !state.enabled then pure () else
          let (events, processedIdsReplacement) ← Listener.pollSource liveCfg.source state appConfig.pat
            appConfig.authorizedUsers
          for (_, vars) in (events : Array (String × List (String × String))) do
            -- github-label-count: skip if a task from this listener is already active.
            if let .githubLabelCount .. := liveCfg.source then
              if ← Queue.hasActiveEntryForListener lcfg.name then
                IO.println s!"  Listener '{lcfg.name}': skipping (active entry already in queue)"
                continue
            -- Project-dispatcher source: synthetic events carry only `role_name`
            -- and (optionally) `issue_id`. Build the queue entry directly from
            -- the named role template, pre-claiming through the in-process
            -- ClaimManager when the role wants it.
            match liveCfg.source with
            | .projectDispatcher pid _ =>
              let roleName := vars.find? (·.1 == "role_name") |>.map (·.2) |>.getD ""
              let issueId := vars.find? (·.1 == "issue_id") |>.bind (fun p => Taxis.IssueId.parse? p.2)
              let some project ← Project.loadProject pid | continue
              let some role ← Project.loadRole pid roleName | continue
              let issue? : Option Project.Issue ← match issueId with
                | none => pure none
                | some iid => Project.loadIssue pid iid
              let entryOpt ← Listener.buildRoleEntry project role issue?
              match entryOpt with
              | none =>
                IO.eprintln s!"  Listener '{lcfg.name}': cannot dispatch {roleName}: no effective target"
              | some entry =>
                -- Pre-claim if the role wants it and we have an issue.
                let needsClaim :=
                  match role.dispatch with
                  | some d => d.preClaim
                  | none   => false
                let claimed ← match needsClaim, issue? with
                  | true, some i =>
                    let now ← TaskStore.currentIso8601
                    let agent := role.backend.getD "claude"
                    match ← Project.tryClaim TaskRunner.globalClaimManager pid i.id
                                             entry.id agent now none with
                    | .acquired _ => pure true
                    | .alreadyClaimed e =>
                      IO.eprintln s!"  Listener '{lcfg.name}': skipping {roleName}: \
                        issue {i.id.toString} already claimed by {e.taskId}"
                      pure false
                    | .invalid r =>
                      IO.eprintln s!"  Listener '{lcfg.name}': skipping {roleName}: {r}"
                      pure false
                  | _, _ => pure true
                if claimed then
                  Queue.saveEntry entry
                  IO.println s!"  Listener '{lcfg.name}': dispatched {roleName} → {entry.id}"
              continue
            | .labelDispatcher label _ =>
              -- Same shape as the branch above, but the project and target come from the event
              -- rather than the listener config: this source spans projects, and each issue's
              -- target was resolved from its taxis artifacts at poll time
              -- (Project.artifactTarget) rather than inherited from a project default.
              let getVar (k : String) := vars.find? (·.1 == k) |>.map (·.2)
              let roleName := getVar "role_name" |>.getD ""
              -- Absent for an unbound role (`always` trigger): it is scoped to the labelled root
              -- in `project_id` and picks its own issues from there, claiming each through the
              -- daemon. Only a malformed id is an error; no id at all is a valid event.
              let issueIdVar := getVar "issue_id"
              let issueId? := issueIdVar.bind Taxis.IssueId.parse?
              if issueIdVar.isSome && issueId?.isNone then
                IO.eprintln s!"  Listener '{lcfg.name}': label-dispatcher event with an \
                  unparseable issue_id; skipping"
                continue
              let some projectId := getVar "project_id" |>.bind Taxis.IssueId.parse?
                | IO.eprintln s!"  Listener '{lcfg.name}': label-dispatcher event without a \
                    usable project_id; skipping"
                  continue
              let target? : Option Project.RepoTarget :=
                match getVar "target_repo", getVar "target_branch" with
                | some r, some b => (Repository.parse r).toOption.map ({ repo := ·, branch := b })
                | _, _ => none
              let some target := target?
                | IO.eprintln s!"  Listener '{lcfg.name}': label-dispatcher event for \
                    {issueId?.map (·.toString) |>.getD projectId.toString} carried no usable \
                    target; skipping"
                  continue
              let some project ← Project.loadProject projectId | continue
              -- Global roles only: these issues can span projects, so no single project's
              -- roles/ directory takes precedence (see Project.loadGlobalRoles).
              let some role := (← Project.loadGlobalRoles).find? (·.name == roleName)
                | IO.eprintln s!"  Listener '{lcfg.name}': no global role '{roleName}'; skipping"
                  continue
              let issue? ← match issueId? with
                | none     => pure none
                | some iid => Project.loadIssue projectId iid
              -- A bound event naming an issue that cannot be loaded is a real failure; an
              -- unbound one legitimately has none.
              if issueId?.isSome && issue?.isNone then continue
              let scope := match issue? with
                | some i => s!"issue {i.id.toString}"
                | none   => s!"root {projectId.toString}"
              let entryOpt ← Listener.buildRoleEntry project role issue?
                (targetOverride := some target)
              match entryOpt with
              | none =>
                IO.eprintln s!"  Listener '{lcfg.name}': cannot dispatch {roleName} for \
                  {scope}: no effective target"
              | some entry =>
                -- Pre-claiming needs an issue to claim. An unbound role has none by construction;
                -- it claims for itself through the daemon once it picks one, which is the same
                -- mutex this would have taken.
                let needsClaim := match issue?, role.dispatch with
                  | some _, some d => d.preClaim
                  | _,      _      => false
                let claimed ← match issue?, needsClaim with
                  | some issue, true =>
                    let now ← TaskStore.currentIso8601
                    let agent := role.backend.getD "claude"
                    match ← Project.tryClaim TaskRunner.globalClaimManager projectId issue.id
                                             entry.id agent now none with
                    | .acquired _ => pure true
                    | .alreadyClaimed e =>
                      IO.eprintln s!"  Listener '{lcfg.name}': skipping {roleName}: issue \
                        {issue.id.toString} already claimed by {e.taskId}"
                      pure false
                    | .invalid r =>
                      IO.eprintln s!"  Listener '{lcfg.name}': skipping {roleName}: {r}"
                      pure false
                  | _, _ => pure true
                if claimed then
                  Queue.saveEntry entry
                  IO.println s!"  Listener '{lcfg.name}': dispatched {roleName} for \
                    {scope} ({label}) → {entry.id}"
              continue
            | _ => pure ()
            if let some wfPath := liveCfg.action.workflowPath then
              -- Concert mode: parse the YAML, apply template vars, start a concert fiber.
              let resolvedPath := Listener.renderTemplate wfPath vars
              try
                let rawYaml ← IO.FS.readFile resolvedPath
                let yaml := Listener.renderTemplate rawYaml vars
                match Workflow.WorkflowProgram.parseYaml yaml with
                | .error e =>
                  IO.eprintln s!"  Listener '{liveCfg.name}': workflow parse error: {e}"
                | .ok prog =>
                  let upstreamStr :=
                    let r := Listener.renderTemplate liveCfg.action.upstream vars
                    if r.isEmpty then vars.find? (·.1 == "upstream") |>.map (·.2) |>.getD ""
                    else r
                  let forkStr :=
                    let r := Listener.renderTemplate liveCfg.action.fork vars
                    if r.isEmpty then vars.find? (·.1 == "fork") |>.map (·.2) |>.getD ""
                    else r
                  let upstream := Repository.parse upstreamStr |>.toOption
                  let fork     := Repository.parse forkStr     |>.toOption
                  let prog := { prog with upstream, fork }
                  let jsonVars := vars.map fun (k, v) => (k, Lean.Json.str v)
                  let concert := Workflow.WorkflowProgram.toConcert prog jsonVars
                  IO.println s!"  Listener '{lcfg.name}': starting concert from {resolvedPath}"
                  let concertId ← TaskStore.generateId
                  let concertRun : Queue.ConcertRun := {
                    id           := concertId
                    startedAt    := ← TaskStore.currentIso8601
                    name         := if prog.name.isEmpty then none else some prog.name
                    workflowFile := some resolvedPath
                  }
                  Queue.saveConcertRun concertRun
                  let _concertTask ← IO.asTask (prio := .dedicated) do
                    try
                      Concert.evalQueued concertMgr appConfig debug none (some concertId) concert
                      let t ← TaskStore.currentIso8601
                      Queue.saveConcertRun { concertRun with status := .done, finishedAt := some t }
                    catch e =>
                      IO.eprintln s!"  Concert {concertId} failed: {e}"
                      let t ← TaskStore.currentIso8601
                      Queue.saveConcertRun { concertRun with status := .failed, finishedAt := some t }
                  pure ()
              catch e =>
                IO.eprintln s!"  Listener '{liveCfg.name}': failed to load workflow: {e}"
            else
              -- Single-task mode: enqueue a QueueEntry as before.
              let qentry ← Listener.buildQueueEntry liveCfg.action vars (some lcfg.name)
              Queue.saveEntry qentry
              IO.println s!"  Listener '{liveCfg.name}': queued entry {qentry.id}"
          let newIds := events.filterMap (fun ev =>
            if (ev.1 : String).isEmpty then none else some ev.1)
          -- Re-read enabled so a disable issued mid-tick is not overwritten.
          let currentEnabled := (← Listener.loadListenerState lcfg.name).enabled
          let newState : Listener.ListenerState := {
            lastChecked  := ← TaskStore.currentIso8601
            processedIds := processedIdsReplacement.getD (state.processedIds ++ newIds)
            enabled      := currentEnabled
          }
          Listener.saveListenerState lcfg.name newState
        catch e =>
          IO.eprintln s!"  Listener '{lcfg.name}' poll error: {e}"
  -- Queue worker loop: claim and run one entry at a time.
  -- Spawning parallelLimit copies of this loop enables parallel execution.
  let workerLoop : IO Unit := do
    while !(← shutdownToken.isCancelled) do
      -- A worker must never die on an unhandled exception. `runEntry` already records
      -- per-entry failures, but anything escaping here (or out of `claimNextEntry`)
      -- would silently retire this worker and shrink the pool permanently — invisible
      -- with one worker on the main thread, and invisible *and* silent for the spawned
      -- ones, whose task results are discarded.
      try
        match ← claimNextEntry with
        | none => IO.sleep 1000
        | some (claim, tokenId) =>
          runEntry claim.entry claim.slot tokenId claim.resumeFrom
      catch e =>
        IO.eprintln s!"Queue worker error: {e}"
        IO.sleep 1000
  -- Spawn additional workers beyond the first (which runs on the main thread below).
  let mut workerTasks : Array (Task (Except IO.Error Unit)) := #[]
  for _ in List.range (parallelLimit - 1) do
    workerTasks := workerTasks.push (← IO.asTask (prio := .dedicated) workerLoop)
  if parallelLimit > 1 then
    IO.println s!"Queue daemon running with parallelLimit={parallelLimit}, parallelLimitPerRepo={parallelLimitPerRepo}"
  try
    workerLoop
    -- Let the other workers finish their in-flight tasks before the socket server and
    -- PID file are torn down; otherwise `IO.Process.exit 0` below kills them mid-task
    -- and their entries are left stuck in `running`.
    for t in workerTasks do
      let _ ← IO.wait t
  finally
    match ← socketServerRef.get with
    | some s => try s.close catch _ => pure ()
    | none   => pure ()
    try Utils.UnixSocket.Server.unlink socketPath catch _ => pure ()
    ConcertManager.cancelAll concertMgr
    Queue.deletePid
  IO.println "Queue daemon shut down gracefully."
  IO.Process.exit 0

private def queueListHandler (p : Parsed) : IO UInt32 := do
  let limit := p.flag? "limit" |>.map (·.as! Nat) |>.getD 20
  if ← Queue.daemonRunning then
    match ← Queue.readPid with
    | some pid => IO.println s!"Daemon running (PID {pid})"
    | none     => IO.println "Daemon running"
  else
    IO.println "Daemon not running"
  -- Concert run history
  let concertRuns := (← Queue.loadAllConcertRuns).toList.take limit
  if !concertRuns.isEmpty then
    IO.println ""
    IO.println s!"{padRight "CONCERT ID" 16} {padRight "STARTED" 20} {padRight "STATUS" 9} NAME"
    IO.println (String.ofList (List.replicate 80 '-'))
    for r in concertRuns do
      let status := match r.status with
        | .running => "running" | .done => "done" | .failed => "failed" | .cancelled => "cancelled"
      IO.println s!"{padRight r.id 16} {padRight r.startedAt 20} {padRight status 9} {r.name.getD (r.workflowFile.getD "")}"
  -- Queue entries
  let entries := (← Queue.loadAllEntries).toList.take limit
  if entries.isEmpty then
    IO.println "No queue entries found."
    return (0 : UInt32)
  IO.println ""
  IO.println s!"{padRight "ID" 16} {padRight "CREATED" 20} {padRight "FORK" 28} {padRight "STATUS" 9} {padRight "PRI" 4} {padRight "SERIES" 16} CONCERT"
  IO.println (String.ofList (List.replicate 110 '-'))
  for e in entries do
    let status := match e.status with
      | .pending => "pending" | .running => "running" | .done => "done" | .failed => "failed"
      | .unfinished => "unfinished" | .cancelled => "cancelled"
    let concertLabel := e.concertId.getD ""
    let seriesLabel := e.series.getD ""
    IO.println s!"{padRight e.id 16} {padRight e.createdAt 20} {padRight e.fork.toString 28} {padRight status 10} {padRight (toString e.priority) 4} {padRight seriesLabel 16} {concertLabel}"
  return (0 : UInt32)

private def listenerListHandler (_ : Parsed) : IO UInt32 := do
  let configs ← Listener.loadAllListenerConfigs
  if configs.isEmpty then
    IO.println "No listeners configured"
    return (0 : UInt32)
  IO.println s!"{padRight "LISTENER" 24} {padRight "ON" 4} {padRight "INTERVAL" 9} {padRight "LAST CHECKED" 22} PROCESSED"
  IO.println (String.ofList (List.replicate 80 '-'))
  for cfg in configs do
    let state ← Listener.loadListenerState cfg.name
    let lastChecked := if state.lastChecked.isEmpty then "never" else state.lastChecked
    let processed   := toString state.processedIds.size ++ " events"
    let interval    := s!"{cfg.intervalSeconds}s"
    let enabled     := if state.enabled then "yes" else "no"
    IO.println s!"{padRight cfg.name 24} {padRight enabled 4} {padRight interval 9} {padRight lastChecked 22} {processed}"
  return (0 : UInt32)

private def listenerEnableHandler (p : Parsed) : IO UInt32 := do
  let name := p.positionalArg! "name" |>.as! String
  let some _ ← Listener.loadListenerConfig name
    | IO.eprintln s!"Listener '{name}' not found"; return 1
  let state ← Listener.loadListenerState name
  Listener.saveListenerState name { state with enabled := true }
  IO.println s!"Listener '{name}': enabled (takes effect on next tick)"
  return 0

private def listenerDisableHandler (p : Parsed) : IO UInt32 := do
  let name := p.positionalArg! "name" |>.as! String
  let some _ ← Listener.loadListenerConfig name
    | IO.eprintln s!"Listener '{name}' not found"; return 1
  let state ← Listener.loadListenerState name
  Listener.saveListenerState name { state with enabled := false }
  IO.println s!"Listener '{name}': disabled (takes effect on next tick)"
  return 0

private def queueStatusHandler (_ : Parsed) : IO UInt32 := do
  -- Daemon status
  if ← Queue.daemonRunning then
    match ← Queue.readPid with
    | some pid => IO.println s!"Daemon: running (PID {pid})"
    | none     => IO.println "Daemon: running"
  else
    IO.println "Daemon: not running"
  -- Running concerts
  let allConcerts ← Queue.loadAllConcertRuns
  let runningConcerts := allConcerts.filter (fun r => r.status == .running)
  if !runningConcerts.isEmpty then
    IO.println ""
    IO.println s!"Concerts: {runningConcerts.size} running"
    IO.println ""
    IO.println s!"{padRight "ID" 16} {padRight "STARTED" 20} NAME"
    IO.println (String.ofList (List.replicate 60 '-'))
    for r in runningConcerts do
      IO.println s!"{padRight r.id 16} {padRight r.startedAt 20} {r.name.getD (r.workflowFile.getD "")}"
  -- Running and pending entries only
  let all ← Queue.loadAllEntries
  let active := all.filter (fun e => e.status == .running || e.status == .pending)
  if active.isEmpty then
    IO.println "Queue: empty"
  else
    IO.println ""
    IO.println s!"Queue: {active.size} task(s)"
    IO.println ""
    IO.println s!"{padRight "ID" 16} {padRight "FORK" 28} {padRight "STATUS" 9} {padRight "PRIORITY" 8} {padRight "SERIES" 16} CONCERT"
    IO.println (String.ofList (List.replicate 102 '-'))
    -- Show running first, then pending ordered by priority desc, then oldest first
    let running := active.filter (fun e => e.status == .running)
    let pendingArr := active.filter (fun e => e.status == .pending)
    let pendingByPriority := pendingArr.qsort (fun a b => a.priority > b.priority)
    for e in running ++ pendingByPriority do
      let status := if e.status == .running then "running" else "pending"
      let concertLabel := e.concertId.getD ""
      let seriesLabel := e.series.getD ""
      IO.println s!"{padRight e.id 16} {padRight e.fork.toString 28} {padRight status 9} {padRight (toString e.priority) 8} {padRight seriesLabel 16} {concertLabel}"
  -- Listener status
  let listenerConfigs ← Listener.loadAllListenerConfigs
  if !listenerConfigs.isEmpty then
    IO.println ""
    IO.println s!"Listeners: {listenerConfigs.size}"
    IO.println ""
    IO.println s!"{padRight "LISTENER" 24} {padRight "ON" 4} {padRight "INTERVAL" 9} {padRight "LAST CHECKED" 22} QUEUED"
    IO.println (String.ofList (List.replicate 80 '-'))
    for cfg in listenerConfigs do
      let state       ← Listener.loadListenerState cfg.name
      let lastChecked := if state.lastChecked.isEmpty then "never" else state.lastChecked
      let queued      := toString state.processedIds.size ++ " events"
      let interval    := s!"{cfg.intervalSeconds}s"
      let enabled     := if state.enabled then "yes" else "no"
      IO.println s!"{padRight cfg.name 24} {padRight enabled 4} {padRight interval 9} {padRight lastChecked 22} {queued}"
  return 0

private def queueShutdownHandler (p : Parsed) : IO UInt32 := do
  let force := p.hasFlag "force"
  if !(← Queue.daemonRunning) then
    IO.eprintln "Queue daemon is not running."
    return 1
  let req := Lean.Json.mkObj [("type", "shutdown"), ("force", Lean.Json.bool force)]
  let _ ← daemonRequest req
  if force then
    IO.println "Shutdown request sent. Daemon will stop after cancelling the current task."
  else
    IO.println "Shutdown request sent. Daemon will stop after the current task finishes."
  return 0

private def queueCancelHandler (_ : Parsed) : IO UInt32 := do
  if !(← Queue.daemonRunning) then
    IO.eprintln "Queue daemon is not running."
    return 1
  let req := Lean.Json.mkObj [("type", "cancel")]
  let _ ← daemonRequest req
  IO.println "Cancel request sent. The current task will be stopped."
  return 0

-- CLI definitions

private def runCmd' : Cmd := `[Cli|
  run VIA runHandler; ["0.1.0"]
  "Run coding agent tasks from a task file."

  FLAGS:
    c, config : String; "Path to config file (default: ~/.config/orchestra/config.json)"
    t, task : Nat; "Run only the task at this index (0-based)"
    d, debug; "Print the landrun command before executing it"
    continues : String; "Continue from a previous task by ID (requires --task with multi-task files)"
    series : String; "Assign this run to a named task series"
    budget : String; "Maximum spend in USD, overrides task file (default: 4.0)"
    vars : String; "Initial workflow variable bindings as a JSON object, e.g. '{\"key\":\"value\"}' (workflow files only)"

  ARGS:
    "task-file" : String; "Path to the JSON task file"
]

private def mcpServerCmd : Cmd := `[Cli|
  mcp VIA mcpServerHandler; ["0.1.0"]
  "Start the MCP server and print the port it is listening on."

  FLAGS:
    c, config : String; "Path to config file (default: ~/.config/orchestra/config.json)"
    allow_pr; "Allow the create_pr tool (disabled by default)"

  ARGS:
    "upstream" : String; "Upstream repository in 'owner/repo' format"
    "fork" : String; "Fork repository in 'owner/repo' format"
]

private def prepareCmd : Cmd := `[Cli|
  prepare VIA prepareHandler; ["0.1.0"]
  "Clone the fork, configure the upstream remote, and warm the task slots the queue runs in."

  FLAGS:
    slots : Nat; "Number of task slots to create and initialise, matching the \
--parallel-per-repo the daemon will use (default: 1)"

  ARGS:
    "upstream" : String; "Upstream repository in 'owner/repo' format"
    "fork" : String; "Fork repository in 'owner/repo' format"
]

private def cleanupListCmd : Cmd := `[Cli|
  list VIA cleanupListHandler; ["0.1.0"]
  "List all repository clones and their task slots."
]

private def cleanupCmd : Cmd := `[Cli|
  cleanup VIA cleanupHandler; ["0.1.0"]
  "Manage cloned repositories. Without a subcommand, removes all clones and task slots."

  SUBCOMMANDS:
    cleanupListCmd
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
    c, config : String; "Path to config file (default: ~/.config/orchestra/config.json)"
    p, prompt : String; "Prompt for the new agent run"
    d, debug; "Print the landrun command before executing it"
    budget : String; "Maximum spend in USD (default: inherited from previous task, or 4.0)"

  ARGS:
    "series" : String; "Series name to resume"
]

private def queueAddCmd : Cmd := `[Cli|
  add VIA enqueueHandler; ["0.1.0"]
  "Add tasks to the queue from a task file or workflow file, or continue a series with a new prompt."

  FLAGS:
    c, config : String; "Path to config file (default: ~/.config/orchestra/config.json)"
    t, task : Nat; "Enqueue only the task at this index (0-based, task-file mode only)"
    continues : String; "Continue from a previous task by ID (task-file mode only)"
    series : String; "Assign queued task(s) to a named series (task-file mode only)"
    r, resume : String; "Continue the latest run in a named series (requires --prompt)"
    p, prompt : String; "Prompt for the new agent run (used with --resume)"
    budget : String; "Maximum spend in USD, overrides task file (default: 4.0)"
    priority : Nat; "Priority for the queued entry (default: 10)"
    vars : String; "Initial workflow variable bindings as a JSON object, e.g. '{\"key\":\"value\"}' (workflow files only)"

  ARGS:
    ..."task-file" : String; "Path to the JSON task or workflow file (omit when using --resume)"
]

private def queueStartCmd : Cmd := `[Cli|
  start VIA queueStartHandler; ["0.1.0"]
  "Start the queue daemon. Polls for pending tasks and runs them in parallel up to the configured limit."

  FLAGS:
    c, config : String; "Path to config file (default: ~/.config/orchestra/config.json)"
    d, debug; "Print the landrun command before executing it"
    b, background; "Run the daemon in the background, detached from the terminal"
    parallel : Nat; "Maximum number of tasks to run in parallel. Overrides queue.parallel in \
config.json (default: 1). Backends that keep per-run state at a fixed global path (pi, \
opencode) always run exclusively, so they start only while the daemon is otherwise idle and \
may wait a long time under steady load"
    "parallel-per-repo" : Nat; "Maximum parallel tasks per repository; each runs in its own clone. \
Overrides queue.parallel_per_repo in config.json (default: 1). \
Run `orchestra prepare --slots N` first so the clones are warm"
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
      priority     := entry.priority
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

private def listenerSubDefault (_ : Parsed) : IO UInt32 := do
  IO.eprintln "Use a subcommand (list, enable, disable). Try '--help'."
  return 1

private def listenerListCmd : Cmd := `[Cli|
  list VIA listenerListHandler; ["0.1.0"]
  "List configured listeners and their state."
]

private def listenerEnableCmd : Cmd := `[Cli|
  enable VIA listenerEnableHandler; ["0.1.0"]
  "Enable a listener by name (takes effect on next tick)."

  ARGS:
    "name" : String; "Listener name"
]

private def listenerDisableCmd : Cmd := `[Cli|
  disable VIA listenerDisableHandler; ["0.1.0"]
  "Disable a listener by name (takes effect on next tick)."

  ARGS:
    "name" : String; "Listener name"
]

private def listenerCmd : Cmd := `[Cli|
  listener VIA listenerSubDefault; ["0.1.0"]
  "Manage listeners (list, enable, disable)."

  SUBCOMMANDS:
    listenerListCmd;
    listenerEnableCmd;
    listenerDisableCmd
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
    queueRetryCmd
]

private def migrateHandler (_ : Parsed) : IO UInt32 := do
  try
    Migrate.run
    return 0
  catch e =>
    IO.eprintln s!"Migration failed: {e}"
    return 1

private def migrateCmd : Cmd := `[Cli|
  migrate VIA migrateHandler; ["0.1.0"]
  "Migrate configuration and state from ~/.agent/ to XDG directories (~/.config/orchestra/ and ~/.local/share/orchestra/)."
]

-- All optional tool permission tokens recognised by --tools.
private def allOptionalTools : List String :=
  ["create_pr", "comment", "manage_issues", "work_issues", "review_issues"]

private def interactiveHandler (p : Parsed) : IO UInt32 := do
  let upstreamStr := p.flag! "upstream" |>.as! String
  let forkStr     := p.flag! "fork"     |>.as! String
  let toolsStr    := p.flag? "tools"    |>.map (·.as! String)
  let backend     := p.flag? "backend"  |>.map (·.as! String)
  let model       := p.flag? "model"    |>.map (·.as! String)
  let budget      := p.flag? "budget"   |>.bind (fun v => parseFloat? (v.as! String)) |>.getD 4.0
  let debug       := p.hasFlag "debug"
  let configPath  := p.flag? "config"   |>.map (·.as! String)
  let authSource  := p.flag? "auth_source" |>.map (·.as! String)
  let upstream ← IO.ofExcept (Repository.parse upstreamStr)
  let fork     ← IO.ofExcept (Repository.parse forkStr)
  let allowedTools : List String := match toolsStr with
    | none | some "all" => allOptionalTools
    | some s => s.splitOn ","
  let appConfig ← loadAppConfig (configPath.map System.FilePath.mk)
  let jwt ← GitHub.createJWT appConfig.appId appConfig.privateKeyPath
  let installationId ← match appConfig.installationId with
    | some id => pure id
    | none    => GitHub.getInstallationId jwt fork.owner
  let token ← GitHub.createInstallationToken jwt installationId
  GitHub.setupGhAuth token
  IO.println s!"Cloning/updating {fork}..."
  let repoPath ← Repo.ensureCloned fork upstream
  IO.println s!"  Repo at {repoPath}"
  let backendName := backend.getD "claude"
  let serverState : Server.State := {
    upstream, fork
    allowedTools
    appId          := appConfig.appId
    privateKeyPath := appConfig.privateKeyPath
    installationId
    pat            := appConfig.pat
    agentBackend   := backendName
  }
  let (port, shutdown) ← Server.start serverState
  IO.println s!"  MCP server on port {port}"
  let agentDef := match backend with
    | some "pi"       => AgentDef.pi
    | some "vibe"     => AgentDef.vibe
    | some "opencode" => AgentDef.opencode
    | _               => AgentDef.claude
  let extraPorts := appConfig.agentAuthConfigs.find? (fun c => c.name == backendName)
    |>.map (·.extraPorts) |>.getD #[]
  let apiKeyEnv ← TaskRunner.resolveAuthEnv appConfig agentDef backendName authSource
  IO.println "  Launching agent..."
  let result ← Sandbox.launchAgent agentDef repoPath "" port token
    (debug := debug) (pluginDirs := appConfig.pluginDirs)
    (model := model) (budget := budget)
    (extraEnv := apiKeyEnv) (extraPorts := extraPorts)
    (additionalPaths := appConfig.additionalSandboxPaths)
    (interactiveAgent := true)
  IO.println s!"  Agent exited with code {result.exitCode}"
  shutdown
  return if result.exitCode == 0 then 0 else 1

private def interactiveCmd : Cmd := `[Cli|
  interactive VIA interactiveHandler; ["0.1.0"]
  "Drop into the agent's interactive TUI inside a sandboxed environment."

  FLAGS:
    c, config   : String; "Path to config file (default: ~/.agent/config.json)"
    d, debug;             "Print the landrun command before executing it"
    upstream    : String; "Upstream repository in 'owner/repo' format"
    fork        : String; "Fork repository in 'owner/repo' format"
    tools       : String; "Comma-separated optional tools to enable, or 'all' (default: all)"
    backend     : String; "Agent backend: claude (default), vibe, opencode, pi"
    model       : String; "Model override passed to the agent"
    budget      : String; "Maximum spend in USD (default: 4.0)"
    auth_source : String; "Authentication source label to use (overrides default_auth_source)"
]

private def defaultHandler (_ : Parsed) : IO UInt32 := do
  IO.eprintln "Use a subcommand. Try 'orchestra --help'."
  return 1

def orchestraCmd : Cmd := `[Cli|
  orchestra VIA defaultHandler; ["0.1.0"]
  "CLI tool for managing and sandboxing coding agents."

  SUBCOMMANDS:
    runCmd';
    interactiveCmd;
    mcpServerCmd;
    prepareCmd;
    cleanupCmd;
    tasksCmd;
    taskCmd;
    seriesCmd;
    tagCmd;
    resumeCmd;
    queueCmd;
    listenerCmd;
    projectCmd;
    issueCmd;
    spawnCmd;
    rolesCmd;
    migrateCmd
]

/-- Wrap a stream so that every write is flushed immediately.

    Lean block-buffers its output streams when they are not a TTY. A long-running process writing
    to a pipe therefore produces *nothing* until it exits or fills the buffer — so the queue
    daemon, which is meant to run for days, shows no output at all under `docker compose logs`,
    systemd's journal, `tee`, or anything else that isn't a terminal. Short-lived commands hide
    the problem by flushing on exit.

    Only applied when the stream is not already a TTY: an interactive terminal is line-buffered
    by default and does not need a flush per write. -/
private def autoFlushing (s : IO.FS.Stream) : IO.FS.Stream :=
  { s with
    write  := fun bs  => do s.write bs;  s.flush
    putStr := fun str => do s.putStr str; s.flush }

private def unbufferIfPiped : IO Unit := do
  let out ← IO.getStdout
  unless ← out.isTty do discard <| IO.setStdout (autoFlushing out)
  let err ← IO.getStderr
  unless ← err.isTty do discard <| IO.setStderr (autoFlushing err)

def main (args : List String) : IO UInt32 := do
  unbufferIfPiped
  gRawArgs.set args
  Project.ensureTaxisConfigured
  orchestraCmd.validate args
