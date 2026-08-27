import Orchestra.AgentDef
import Orchestra.Agents.Claude
import Orchestra.Agents.Opencode
import Orchestra.Agents.Pi
import Orchestra.Agents.Vibe
import Orchestra.Client
import Orchestra.Concert
import Orchestra.ConcertManager
import Orchestra.Config
import Orchestra.DaemonRequest
import Orchestra.Dirs
import Orchestra.GitHub
import Orchestra.Listener
import Orchestra.Migrate
import Orchestra.Project
import Orchestra.Queue
import Orchestra.Repo
import Orchestra.RepoConfig
import Orchestra.Sandbox
import Orchestra.Secret
import Orchestra.Server
import Orchestra.Skill
import Orchestra.StreamFormat
import Orchestra.TaskRunner
import Orchestra.TaskStore
import Orchestra.Usage
import Orchestra.Utils.Format
import Orchestra.Utils.Http
import Orchestra.Utils.Streams
import Orchestra.Utils.UnixSocket
import Orchestra.Workflow
import Orchestra.WorkflowParser
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
      let (_, status, _) ← TaskRunner.runTask appConfig task i debug
        (continuesFrom := continuesFrom) (series := series)
      -- `unfinished` is what a usage limit looks like from here. Saying so is the difference
      -- between "that task is done" and "that task stopped early and can be resumed".
      if status matches .unfinished then
        IO.eprintln s!"Task {i} did not finish (usage limit or budget exhausted); \
resume it with 'orchestra continue'."
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
    repo := some { upstream, fork }
    installationId := some installationId
    allowedTools := if allowPR then ["create_pr"] else []
    appId := appConfig.appId
    privateKeyPath := appConfig.privateKeyPath
    pat := appConfig.pat
    defaultOrganization := appConfig.defaultOrganization
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
  let workspaces ← Repo.listWorkspaces
  if clones.isEmpty && workspaces.isEmpty then
    IO.println "No repository clones or workspaces found."
    return (0 : UInt32)
  for (mainPath, slots) in clones do
    IO.println s!"  {mainPath}"
    for slot in slots do
      IO.println s!"    slot: {slot}"
  -- Listed alongside the clones because `cleanup` removes both, and a workspace holding a
  -- repository-independent task's scratch files is exactly as much disk as a slot.
  for workspace in workspaces do
    IO.println s!"  {workspace} (no repository)"
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
    IO.println s!"{padRight r.id 16} {padRight r.createdAt 20} {padRight (repoLabel r.repo) 28} {padRight status 11} {padRight seriesLabel 16} {concertLabel}"
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
    match r.repo with
    | some repo =>
      IO.println s!"Fork:           {repo.fork}"
      IO.println s!"Upstream:       {repo.upstream}"
    | none => IO.println "Repository:     none (repository-independent)"
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
      repo          := prevRecord.repo
      mode          := prevRecord.mode
      prompt
      goal          := prevRecord.goal
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
      repo          := prevRecord.repo
      mode          := prevRecord.mode
      prompt        := promptText
      goal          := prevRecord.goal
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
        repo          := task.ioTask.repo
        mode          := task.ioTask.mode
        prompt        := task.ioTask.prompt
        goal          := task.ioTask.goal
        agent         := task.ioTask.agent
        systemPrompt  := task.ioTask.systemPrompt
        prependPrompt := task.ioTask.prependPrompt
        backend       := task.ioTask.backend
        model         := task.ioTask.model
        continuesFrom, series
        configPath
        budget           := budgetFlag.orElse (fun _ => task.ioTask.budget)
        authSource       := task.ioTask.authSource
        authSources      := task.ioTask.authSources
        authMode         := task.ioTask.authMode
        tools            := task.ioTask.tools
        readOnly         := task.ioTask.readOnly
        priority         := priorityFlag.getD task.ioTask.priority
        issueNumber      := task.ioTask.issueNumber
        spawnPolicy      := task.ioTask.spawnPolicy
      }
      let req := Lean.Json.mkObj [("type", "add_task"), ("entry", Lean.ToJson.toJson entry)]
      let _ ← daemonRequest req
      IO.println entry.id
    return (0 : UInt32)

/-! ## Starting the backend

`orchestra` is a client; it does not run a queue or serve an API. `queue start` and `dashboard`
are kept under their old names because they are what people, scripts, systemd units and the
Docker image already type — but what they now do is start `orchestrad`, which is the binary that
holds those jobs. Everything they accepted before, they still accept.
-/

/-- Names an `orchestrad` to run, for an install that does not put the two binaries together. -/
private def serverBinEnvVar : String := "ORCHESTRA_SERVER_BIN"

/-- Where the backend binary is.

    `$ORCHESTRA_SERVER_BIN` first, so an unusual layout can say so; then a sibling of this
    executable, which is what makes both a `lake build` checkout (`.lake/build/bin/`) and an
    ordinary install work with no configuration at all; then bare `orchestrad`, resolved through
    `PATH`. -/
private def serverBinary : IO String := do
  if let some p ← IO.getEnv serverBinEnvVar then
    if !p.trimAscii.isEmpty then return p.trimAscii.toString
  if let some parent := (← IO.appPath).parent then
    let sibling := parent / "orchestrad"
    if ← sibling.pathExists then return sibling.toString
  return "orchestrad"

private def serverMissingHelp (bin : String) (e : IO.Error) : IO Unit := do
  IO.eprintln s!"Could not start the orchestra backend ('{bin}'): {e}"
  IO.eprintln ""
  IO.eprintln "The daemon and the API live in a separate binary, 'orchestrad'. Build it with"
  IO.eprintln s!"'lake build orchestrad', put it on PATH, or point ${serverBinEnvVar} at it."

/-- Run `orchestrad` in the foreground, wired to this process's streams, and return its status. -/
private def execServer (args : Array String) : IO UInt32 := do
  let bin ← serverBinary
  let child? ← try
      let child ← IO.Process.spawn { cmd := bin, args, stdin := .inherit, stdout := .inherit,
                                     stderr := .inherit }
      pure (some child)
    catch e =>
      serverMissingHelp bin e
      pure none
  let some child := child? | return 1
  child.wait

/-- Start `orchestrad` detached, with its output appended to the daemon log, and wait for it to
    announce itself by writing a PID file.

    The double fork is `sh`'s: `IO.Process.spawn` has no detach, and a child that shares this
    process's session would die with the terminal that started it. -/
private def spawnServerBackground (args : Array String) : IO UInt32 := do
  if ← Queue.daemonRunning then
    IO.eprintln "Queue daemon is already running."
    return 1
  let dir ← Queue.queueDir
  IO.FS.createDirAll dir
  let logFile ← Queue.daemonLogFile
  let bin ← serverBinary
  let quoted := String.intercalate " " ((#[bin] ++ args).toList.map shellQuote)
  let shellCmd := s!"exec {quoted} >> {shellQuote logFile.toString} 2>&1 & echo $!"
  let launcher ← IO.Process.spawn {
    cmd := "sh"
    args := #["-c", shellCmd]
    stdin := .null
    stdout := .piped
    stderr := .piped
  }
  let _ ← launcher.stdout.readToEnd
  -- The shell's own complaints, kept because they are the only account of a failure that happens
  -- before the redirect takes effect — an unwritable queue directory leaves no log to read, and an
  -- unwritable log file leaves a readable one whose contents are from some earlier run.
  let launcherErr ← launcher.stderr.readToEnd
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
    let log ← try IO.FS.readFile logFile catch _ => pure s!"(no log at {logFile})"
    IO.eprintln log
    -- Unconditionally, not just when the log is missing: a log file that exists but cannot be
    -- appended to still reads back fine, so the redirect's failure would otherwise be reported as
    -- whatever some earlier run happened to leave there.
    unless launcherErr.trimAscii.isEmpty do IO.eprintln launcherErr.trimAscii
    return 1

/-- Re-emit a string flag as the pair `orchestrad` expects, or nothing when it was not given. -/
private def passFlag (p : Parsed) (name : String) : Array String :=
  match p.flag? name with
  | some v => #[s!"--{name}", v.as! String]
  | none   => #[]

private def passNatFlag (p : Parsed) (name : String) : Array String :=
  match p.flag? name with
  | some v => #[s!"--{name}", toString (v.as! Nat)]
  | none   => #[]

private def passSwitch (p : Parsed) (name : String) : Array String :=
  if p.hasFlag name then #[s!"--{name}"] else #[]

private def queueStartHandler (p : Parsed) : IO UInt32 := do
  let args := #["queue"]
    ++ passFlag p "config" ++ passSwitch p "debug"
    ++ passNatFlag p "parallel" ++ passNatFlag p "parallel-per-repo"
  if p.hasFlag "background" then spawnServerBackground args else execServer args

private def dashboardHandler (p : Parsed) : IO UInt32 := do
  let args := #["dashboard"]
    ++ passNatFlag p "port" ++ passFlag p "host" ++ passFlag p "password"
    ++ passFlag p "site" ++ passFlag p "config" ++ passNatFlag p "session-ttl"
    ++ passSwitch p "secure-cookie"
  execServer args

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
    IO.println s!"{padRight e.id 16} {padRight e.createdAt 20} {padRight (repoLabel e.repo) 28} {padRight status 10} {padRight (toString e.priority) 4} {padRight seriesLabel 16} {concertLabel}"
  return (0 : UInt32)

/-! ## Configuration, over the API

Listeners, roles and skills are configuration, and configuration is the backend's to hold: it is
what the daemon reads on every tick and what the dashboard shows. So these commands are HTTP
clients of `orchestrad` rather than second writers of the same files — which is the point of
taxis #433. Two writers of one directory is a race nobody can see and a validation rule that has
to be right in two places; one writer with a client in front of it is neither.

The three resources answer the same four verbs, so they share one command with the resource as
its first argument: `orchestra config list roles`, `orchestra config set skills my-skill x.md`.
That shape is the API's own — a kind and a name — which keeps the CLI a thin thing you can read
the server's behaviour out of.

`orchestra listener list|enable|disable` are kept as they were, because they are what is already
typed; they are the same requests with a table in front of them.
-/

/-- The three writable configuration resources, as they are spelled in a URL. -/
private def configKinds : List String := ["listeners", "roles", "skills"]

/-- Resolve the API endpoint, or print why it could not be and stop. -/
private def withClient (p : Parsed) (act : Client.Config → IO UInt32) : IO UInt32 := do
  match ← Client.resolve (p.flag? "api-url" |>.map (·.as! String))
                         (p.flag? "api-token" |>.map (·.as! String)) with
  | .error e  => IO.eprintln e; return 1
  | .ok cfg   => act cfg

/-- Run a request, printing the server's sentence and failing on it. -/
private def apiCall (act : IO (Except String Lean.Json)) (onOk : Lean.Json → IO Unit) :
    IO UInt32 := do
  match ← act with
  | .error e => IO.eprintln e; return 1
  | .ok j    => onOk j; return 0

private def checkKind (kind : String) : IO Bool := do
  if configKinds.contains kind then return true
  IO.eprintln s!"Unknown configuration kind '{kind}'; \
expected one of {String.intercalate ", " configKinds}."
  return false

/-- Read the document a `set` is going to send: a file, or standard input for `-` and for no
    argument at all, so that a config can be piped in without a temporary file. -/
private def readDocument (path : Option String) : IO String := do
  match path with
  | none | some "-" => (← IO.getStdin).readToEnd
  | some f          => IO.FS.readFile f

private def configListHandler (p : Parsed) : IO UInt32 := do
  let kind := p.positionalArg! "kind" |>.as! String
  unless ← checkKind kind do return 1
  withClient p fun cfg => do
    apiCall (Client.get cfg s!"/api/v1/{kind}") fun j => do
      let (rows, total) := Client.items j
      if rows.isEmpty then
        IO.println s!"No {kind} configured."
      else
        match kind with
        | "listeners" =>
          IO.println s!"{padRight "LISTENER" 24} {padRight "ON" 4} {padRight "SOURCE" 20} \
{padRight "INTERVAL" 9} {padRight "LAST CHECKED" 22} EVENTS"
          IO.println (String.ofList (List.replicate 96 '-'))
          for r in rows do
            IO.println s!"{padRight (Client.str r "name") 24} \
{padRight (if Client.bool r "enabled" true then "yes" else "no") 4} \
{padRight (Client.str r "sourceType") 20} \
{padRight s!"{Client.nat r "intervalSeconds"}s" 9} \
{padRight (Client.str r "lastCheckedAt" "never") 22} {Client.nat r "eventCount"}"
        | "roles" =>
          IO.println s!"{padRight "ROLE" 24} {padRight "BACKEND" 12} {padRight "PRI" 4} \
{padRight "DISPATCH" 26} PERMISSIONS"
          IO.println (String.ofList (List.replicate 96 '-'))
          for r in rows do
            let dispatch := match r.getObjVal? "dispatch" |>.toOption with
              | some d => s!"{Client.str d "trigger" "-"} (max {Client.nat d "max"})"
              | none   => "-"
            let permArr := (r.getObjVal? "permissions" |>.toOption).bind (·.getArr?.toOption)
            let perms := match permArr with
              | some a =>
                String.intercalate ","
                  (a.toList.filterMap (fun (x : Lean.Json) => x.getStr?.toOption))
              | none   => ""
            IO.println s!"{padRight (Client.str r "name") 24} \
{padRight (Client.str r "backend" "-") 12} {padRight (toString (Client.nat r "priority")) 4} \
{padRight dispatch 26} {perms}"
        | _ =>
          IO.println s!"{padRight "SKILL" 32} DESCRIPTION"
          IO.println (String.ofList (List.replicate 96 '-'))
          for r in rows do
            let desc := Client.str r "description" "(no description)"
            IO.println s!"{padRight (Client.str r "name") 32} {desc}"
      if total > rows.size then
        IO.println s!"({rows.size} of {total}; the API pages at 50 by default)"

private def configShowHandler (p : Parsed) : IO UInt32 := do
  let kind := p.positionalArg! "kind" |>.as! String
  let name := p.positionalArg! "name" |>.as! String
  unless ← checkKind kind do return 1
  withClient p fun cfg => do
    apiCall (Client.get cfg s!"/api/v1/{kind}/{Client.encodeSegment name}") fun j => do
      -- A skill is prose and a config is a document; printing either as pretty JSON would be
      -- printing an escaped copy of the thing the user asked to see.
      if kind == "skills" then
        IO.println (Client.str j "content")
      else
        match j.getObjVal? "config" |>.toOption with
        | some c => IO.println c.pretty
        | none   => IO.println j.pretty

private def configSetHandler (p : Parsed) : IO UInt32 := do
  let kind := p.positionalArg! "kind" |>.as! String
  let name := p.positionalArg! "name" |>.as! String
  unless ← checkKind kind do return 1
  let doc ← readDocument ((p.variableArgsAs? String |>.getD #[])[0]?)
  -- Skills are Markdown, so they travel wrapped; the other two *are* JSON documents.
  let body := if kind == "skills"
    then Lean.Json.compress (Lean.Json.mkObj [("content", Lean.Json.str doc)])
    else doc
  withClient p fun cfg => do
    apiCall (Client.put cfg s!"/api/v1/{kind}/{Client.encodeSegment name}" body) fun _ => do
      IO.println s!"{kind}/{name}: saved"
      if kind == "listeners" then
        IO.println "The daemon re-reads listener configs each tick, and picks up new ones \
within 15 seconds; no restart is needed."

private def configRemoveHandler (p : Parsed) : IO UInt32 := do
  let kind := p.positionalArg! "kind" |>.as! String
  let name := p.positionalArg! "name" |>.as! String
  unless ← checkKind kind do return 1
  withClient p fun cfg => do
    apiCall (Client.delete cfg s!"/api/v1/{kind}/{Client.encodeSegment name}") fun _ => do
      IO.println s!"{kind}/{name}: removed"

/-! ### The listener commands, unchanged in spelling -/

private def listenerListHandler (p : Parsed) : IO UInt32 :=
  withClient p fun cfg => do
    apiCall (Client.get cfg "/api/v1/listeners") fun j => do
      let (rows, _) := Client.items j
      if rows.isEmpty then
        IO.println "No listeners configured"
      else
        IO.println s!"{padRight "LISTENER" 24} {padRight "ON" 4} {padRight "INTERVAL" 9} \
{padRight "LAST CHECKED" 22} PROCESSED"
        IO.println (String.ofList (List.replicate 80 '-'))
        for r in rows do
          IO.println s!"{padRight (Client.str r "name") 24} \
{padRight (if Client.bool r "enabled" true then "yes" else "no") 4} \
{padRight s!"{Client.nat r "intervalSeconds"}s" 9} \
{padRight (Client.str r "lastCheckedAt" "never") 22} {Client.nat r "eventCount"} events"

private def listenerShowHandler (p : Parsed) : IO UInt32 := do
  let name := p.positionalArg! "name" |>.as! String
  withClient p fun cfg => do
    apiCall (Client.get cfg s!"/api/v1/listeners/{Client.encodeSegment name}") fun j => do
      IO.println s!"Name:         {Client.str j "name"}"
      IO.println s!"Enabled:      {if Client.bool j "enabled" true then "yes" else "no"}"
      IO.println s!"Source:       {Client.str j "sourceType"} — {Client.str j "sourceDetail"}"
      IO.println s!"Interval:     {Client.nat j "intervalSeconds"}s"
      IO.println s!"Last checked: {Client.str j "lastCheckedAt" "never"}"
      IO.println s!"Events seen:  {Client.nat j "eventCount"}"
      -- Printed only when there are ceilings to print: a listener with none is not paced, and
      -- "Rate limits: none" is a line that says nothing on every other listener there is.
      let rateLimits := (j.getObjVal? "rateLimits" |>.toOption).bind (·.getArr?.toOption)
      if let some rls := rateLimits then
        if !rls.isEmpty then
          IO.println "Rate limits:"
          for r in rls do
            let nextAt := Client.str r "nextAllowedAt"
            let when_  := if nextAt.isEmpty then "" else s!", next at {nextAt}"
            IO.println s!"  {Client.str r "description"} \
({Client.nat r "used"} used{when_})"
      if let some c := j.getObjVal? "config" |>.toOption then
        IO.println "Config:"
        for line in c.pretty.splitOn "\n" do
          IO.println s!"  {line}"

private def setEnabled (p : Parsed) (enabled : Bool) : IO UInt32 := do
  let name := p.positionalArg! "name" |>.as! String
  let body := Lean.Json.compress (Lean.Json.mkObj [("enabled", Lean.Json.bool enabled)])
  withClient p fun cfg => do
    apiCall (Client.put cfg s!"/api/v1/listeners/{Client.encodeSegment name}/enabled" body)
      fun _ => do
        let word := if enabled then "enabled" else "disabled"
        IO.println s!"Listener '{name}': {word} (takes effect on next tick)"

private def listenerEnableHandler (p : Parsed) : IO UInt32 := setEnabled p true

private def listenerDisableHandler (p : Parsed) : IO UInt32 := setEnabled p false

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
      IO.println s!"{padRight e.id 16} {padRight (repoLabel e.repo) 28} {padRight status 9} {padRight (toString e.priority) 8} {padRight seriesLabel 16} {concertLabel}"
  -- Listener status
  let listenerConfigs ← Listener.loadAllListenerConfigs
  if !listenerConfigs.isEmpty then
    IO.println ""
    IO.println s!"Listeners: {listenerConfigs.size}"
    IO.println ""
    IO.println s!"{padRight "LISTENER" 24} {padRight "ON" 4} {padRight "INTERVAL" 9} {padRight "LAST CHECKED" 22} QUEUED"
    IO.println (String.ofList (List.replicate 80 '-'))
    for (name, cfg) in listenerConfigs do
      let state       ← Listener.loadListenerState name
      let lastChecked := if state.lastChecked.isEmpty then "never" else state.lastChecked
      let queued      := toString state.processedIds.size ++ " events"
      let interval    := s!"{cfg.intervalSeconds}s"
      let enabled     := if state.enabled then "yes" else "no"
      IO.println s!"{padRight name 24} {padRight enabled 4} {padRight interval 9} {padRight lastChecked 22} {queued}"
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
  "List all repository clones and their task slots, plus the scratch workspaces repository-independent tasks run in."
]

private def cleanupCmd : Cmd := `[Cli|
  cleanup VIA cleanupHandler; ["0.1.0"]
  "Manage cloned repositories. Without a subcommand, removes all clones, task slots and scratch workspaces."

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
    --
    -- Only from a run that recorded a session, though. A daemon killed mid-task leaves its
    -- entry `unfinished` with a task id but no session id — nothing was resumable, because
    -- nothing was written. Continuing from it anyway would still hand the retry its
    -- predecessor's workspace, since slot reuse keys off `continuesFrom` and not off the
    -- session: the agent would open a brand-new conversation on top of the dead run's
    -- uncommitted edits. With nothing to resume, a clean checkout is the honest start.
    let continuesFrom ← match entry.status with
      | .unfinished =>
        match entry.taskId with
        | none     => pure none
        | some tid =>
          match ← TaskStore.loadTask tid with
          | some r => pure (if r.sessionId.isSome then some tid else none)
          | none   => pure none
      | _           => pure entry.continuesFrom
    let newEntry : Queue.QueueEntry := {
      id, createdAt
      repo         := entry.repo
      mode         := entry.mode
      prompt       := entry.prompt
      goal         := entry.goal
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
  IO.eprintln "Use a subcommand (list, show, enable, disable). Try '--help'."
  return 1

private def listenerListCmd : Cmd := `[Cli|
  list VIA listenerListHandler; ["0.1.0"]
  "List configured listeners and their state."

  FLAGS:
    "api-url" : String; "Backend to talk to (default: $ORCHESTRA_API_URL, or \
http://127.0.0.1:8080)"
    "api-token" : String; "Shared secret (default: $ORCHESTRA_DASHBOARD_PASSWORD, or the \
persisted one)"
]

private def listenerShowCmd : Cmd := `[Cli|
  "show" VIA listenerShowHandler; ["0.1.0"]
  "Show one listener: its state, what it watches, and the config file behind it."

  FLAGS:
    "api-url" : String; "Backend to talk to (default: $ORCHESTRA_API_URL, or \
http://127.0.0.1:8080)"
    "api-token" : String; "Shared secret (default: $ORCHESTRA_DASHBOARD_PASSWORD, or the \
persisted one)"

  ARGS:
    "name" : String; "Listener name"
]

private def listenerEnableCmd : Cmd := `[Cli|
  enable VIA listenerEnableHandler; ["0.1.0"]
  "Enable a listener by name (takes effect on next tick)."

  FLAGS:
    "api-url" : String; "Backend to talk to (default: $ORCHESTRA_API_URL, or \
http://127.0.0.1:8080)"
    "api-token" : String; "Shared secret (default: $ORCHESTRA_DASHBOARD_PASSWORD, or the \
persisted one)"

  ARGS:
    "name" : String; "Listener name"
]

private def listenerDisableCmd : Cmd := `[Cli|
  disable VIA listenerDisableHandler; ["0.1.0"]
  "Disable a listener by name (takes effect on next tick)."

  FLAGS:
    "api-url" : String; "Backend to talk to (default: $ORCHESTRA_API_URL, or \
http://127.0.0.1:8080)"
    "api-token" : String; "Shared secret (default: $ORCHESTRA_DASHBOARD_PASSWORD, or the \
persisted one)"

  ARGS:
    "name" : String; "Listener name"
]

private def listenerCmd : Cmd := `[Cli|
  listener VIA listenerSubDefault; ["0.1.0"]
  "Manage listeners (list, show, enable, disable). See 'orchestra config' to edit them."

  SUBCOMMANDS:
    listenerListCmd;
    listenerShowCmd;
    listenerEnableCmd;
    listenerDisableCmd
]

private def configSubDefault (_ : Parsed) : IO UInt32 := do
  IO.eprintln "Use a subcommand (list, show, set, remove). Try 'orchestra config --help'."
  return 1

private def configListCmd : Cmd := `[Cli|
  list VIA configListHandler; ["0.1.0"]
  "List configured listeners, roles or skills."

  FLAGS:
    "api-url" : String; "Backend to talk to (default: $ORCHESTRA_API_URL, or \
http://127.0.0.1:8080)"
    "api-token" : String; "Shared secret (default: $ORCHESTRA_DASHBOARD_PASSWORD, or the \
persisted one)"

  ARGS:
    "kind" : String; "listeners, roles or skills"
]

private def configShowCmd : Cmd := `[Cli|
  "show" VIA configShowHandler; ["0.1.0"]
  "Print one configuration record as stored: the JSON document, or a skill's Markdown."

  FLAGS:
    "api-url" : String; "Backend to talk to (default: $ORCHESTRA_API_URL, or \
http://127.0.0.1:8080)"
    "api-token" : String; "Shared secret (default: $ORCHESTRA_DASHBOARD_PASSWORD, or the \
persisted one)"

  ARGS:
    "kind" : String; "listeners, roles or skills"
    "name" : String; "Name of the record"
]

private def configSetCmd : Cmd := `[Cli|
  set VIA configSetHandler; ["0.1.0"]
  "Create or replace a configuration record from a file, or from standard input when the file \
is omitted or given as '-'. The backend validates before storing, so a rejected document \
changes nothing."

  FLAGS:
    "api-url" : String; "Backend to talk to (default: $ORCHESTRA_API_URL, or \
http://127.0.0.1:8080)"
    "api-token" : String; "Shared secret (default: $ORCHESTRA_DASHBOARD_PASSWORD, or the \
persisted one)"

  ARGS:
    "kind" : String; "listeners, roles or skills"
    "name" : String; "Name to store it under; it must match the name inside the document"
    ...file : String; "File to read (default: standard input)"
]

private def configRemoveCmd : Cmd := `[Cli|
  remove VIA configRemoveHandler; ["0.1.0"]
  "Delete a configuration record. Removing a listener removes its processed-event state too."

  FLAGS:
    "api-url" : String; "Backend to talk to (default: $ORCHESTRA_API_URL, or \
http://127.0.0.1:8080)"
    "api-token" : String; "Shared secret (default: $ORCHESTRA_DASHBOARD_PASSWORD, or the \
persisted one)"

  ARGS:
    "kind" : String; "listeners, roles or skills"
    "name" : String; "Name of the record"
]

private def configCmd : Cmd := `[Cli|
  config VIA configSubDefault; ["0.1.0"]
  "Read and change orchestra's configuration — listeners, roles and skills — through the \
backend's API, so a running daemon picks the change up without a restart."

  SUBCOMMANDS:
    configListCmd;
    configShowCmd;
    configSetCmd;
    configRemoveCmd
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

/-- `orchestra usage` — what every configured authentication source has left.

    The thing to look at when the queue seems stalled: it names the limit that is binding, when
    it lifts, and which model families are affected. -/
private def usageHandler (p : Parsed) : IO UInt32 := do
  let configPath := p.flag? "config" |>.map (·.as! String)
  let appConfig ← loadAppConfig (configPath.map System.FilePath.mk)
  let backends : List String := match p.flag? "backend" |>.map (·.as! String) with
    | some b => [b]
    | none   => appConfig.agentAuthConfigs.toList.map (fun a => a.name)
  let model := p.flag? "model" |>.map (·.as! String)
  -- Default to a TTL-bounded poll rather than an unconditional one: the usage endpoint meters
  -- requests, and this command is the kind of thing that ends up in a status line or a watch
  -- loop. `--refresh` forces, `--cached` never polls.
  let pollMode := if p.hasFlag "cached" then 0 else if p.hasFlag "refresh" then 2 else 1
  let now ← Usage.nowEpoch
  if backends.isEmpty then
    IO.println "No agent auth sources configured (see the 'agents' block in config.json)."
    return 0
  for backend in backends do
    let labels := Usage.configuredLabels appConfig backend
    if labels.isEmpty then continue
    IO.println s!"{backend}:"
    -- `--refresh` (mode 2) forces a poll even when polling is disabled; the automatic modes honour
    -- the config, so say so rather than silently showing stale numbers.
    if pollMode != 2 && !Usage.pollingEnabled appConfig backend then
      IO.println "  usage polling disabled (poll_usage: false); showing cached data \
— run with --refresh to force one poll"
    for label in labels do
      if pollMode == 2 then
        match ← Usage.refresh appConfig backend label with
        | .error e => IO.println s!"  {label}: poll failed: {e}"
        | .ok _    => pure ()
      else if pollMode == 1 then
        Usage.ensureFresh appConfig backend label
      -- The way back from a block that should not be there.
      --
      -- An observed block outlives its evidence by design: a poll cannot see a model-scoped
      -- window or a billing failure, so neither can retire one, and a run that would prove it
      -- gone is exactly what the block prevents. That is right when the block is right and leaves
      -- no way out when it is not — a misread message can otherwise only be waited out. This is
      -- the way out, and it is deliberately manual: nothing infers that a block was wrong.
      if p.hasFlag "clear_blocks" then
        let st ← Usage.loadState backend label
        if !st.blocks.isEmpty then
          Usage.saveState { st with blocks := #[] }
          IO.println s!"  {label}: forgot {st.blocks.size} observed block(s)"
      let st ← Usage.loadState backend label
      let verdict := match Usage.availabilityOf st model now with
        | .available          => "available"
        | .blocked until' r =>
          match until' with
          | some u => s!"BLOCKED ({r}, resets {Usage.relativeToNow u now})"
          | none   => s!"BLOCKED ({r})"
      let kind := if (Usage.oauthTokenOf appConfig backend label).isSome then "oauth" else "api-key"
      IO.println s!"  {label} [{kind}]: {verdict}"
      if let some e := st.lastError then
        IO.println s!"    last poll error: {e}"
      if let some pa := st.pollAfter then
        if pa > now then
          IO.println s!"    not polling until {Usage.relativeToNow pa now} \
(the usage endpoint is rate-limiting requests)"
      -- Blocks before limits, because they are the ones nothing else here can show. The limit
      -- rows come from polls, and a poll reads the account's own windows only — so a limit
      -- observed by a run, on one model family, appears in neither the rows below nor the verdict
      -- above (which is judged for `model`, and defaults to naming none). Without a line of its
      -- own it appears nowhere, and this is the command an operator asks "why did Fable stop?"
      for b in st.blocks do
        if Usage.blockIsLive b now then
          let scope := match b.model with | some m => m | none => "whole account"
          let lifts := match b.untilEpoch with
            | some u => s!", lifts {Usage.relativeToNow u now}"
            | none   => ""
          IO.println s!"    observed ({scope}): {b.reason}{lifts}"
      for l in st.limits do
        let scope := match l.scopeModel with | some m => s!" ({m})" | none => ""
        let resets := match l.resetsAt.bind Usage.parseIso8601 with
          | some r => s!", resets {Usage.relativeToNow r now}"
          | none   => ""
        IO.println s!"    {l.kind.toString}{scope}: {l.percent}% [{l.severity}]{resets}"
      if st.limits.isEmpty && (Usage.oauthTokenOf appConfig backend label).isNone then
        IO.println "    no subscription limits to report (API-key sources are billed per token)"
    -- What a task queued right now would actually be dispatched to. Answers the operator
    -- question the per-source list only implies, and exercises the same resolver the daemon
    -- uses at claim time.
    if p.hasFlag "select" then
      -- Resolved the way a task that names no source is, so the answer is the config's rather
      -- than this command's: a configured pool decides both the candidates and the mode. Listing
      -- every source under a mode of its own would report a dispatch that cannot happen —
      -- `default_auth_source` may name a subset, and it carries its own `default_auth_mode`.
      -- `--auth_mode` still overrides, which is what makes it a simulation: it answers "and if I
      -- set distribute?" without having to edit the config to find out. Absent, the config's own
      -- mode is what gets reported.
      let flagMode := (p.flag? "auth_mode" |>.map (·.as! String)).bind AuthMode.ofString?
      let (pooled, mode) := Usage.resolutionFor appConfig backend [] none flagMode
      -- Empty means the config has nothing to choose between — the legacy flat-token install.
      -- Falling back to every known label keeps the line informative there.
      let candidates := if pooled.isEmpty then labels else pooled
      match ← Usage.select backend candidates mode model with
      | .ok label => IO.println s!"  → would select: {label} ({mode.toString})"
      | .error e  => IO.println s!"  → would select: nothing ({e})"
  return 0

private def usageCmd : Cmd := `[Cli|
  usage VIA usageHandler; ["0.1.0"]
  "Show the usage limits of every configured authentication source."

  FLAGS:
    c, config : String; "Path to config file"
    backend   : String; "Only show this backend (default: all configured)"
    model     : String; "Judge availability for this model (affects model-scoped limits)"
    cached    ;         "Do not poll; report the last stored values"
    refresh   ;         "Force a poll even if the stored values are still fresh"
    select    ;         "Also show which source a task queued now would be dispatched to"
    clear_blocks;       "Forget limits observed by a run on the selected sources, then report"
    auth_mode : String; "Selection mode to simulate with --select: ordered (default) or distribute"
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

private def dashboardCmd : Cmd := `[Cli|
  dashboard VIA dashboardHandler; ["0.1.0"]
  "Serve the web dashboard: the JSON API, its SSE streams, and the built front-end."

  FLAGS:
    p, port : Nat; "Port to listen on (default: 8080)"
    host : String; "Address to bind (default: 127.0.0.1; use 0.0.0.0 in a container)"
    password : String; "Password to require (default: $ORCHESTRA_DASHBOARD_PASSWORD or a generated, persisted one)"
    s, site : String; "Serve the front-end built into this directory (web/dist)"
    c, config : String; "Path to config file (read for the auth-sources page)"
    "session-ttl" : Nat; "Session cookie lifetime in seconds (default: 43200)"
    "secure-cookie"; "Mark the session cookie Secure (only behind a TLS-terminating proxy)"
]

-- All optional tool permission tokens recognised by --tools.
private def allOptionalTools : List String :=
  ["create_pr", "merge_pr", "label_issue", "comment", "create_repository",
   "manage_issues", "work_issues", "review_issues"]

/-! ## `orchestra chat` — a session on the backend, from a terminal

`orchestra interactive` is unchanged and still local: it drops you into the agent's own TUI on
the machine you are sitting at. This is the other thing — a conversation the *daemon* holds,
which the dashboard and a phone can be looking at too, and which survives closing this terminal.

Nothing here executes anything. It posts turns and renders the transcript stream, and every
event it prints goes through `StreamFormat.format`, the same renderer `orchestra run` prints
task output with. A chat therefore looks like a run that answers back.
-/

/-- Render one transcript envelope for a terminal.

    Agent events go through the shared formatter; the kinds the agent's own stream cannot carry
    are rendered here, because they are this client's news rather than the agent's. -/
private def renderTranscriptEvent (j : Lean.Json) : Option String :=
  match Client.str j "kind" with
  | "user"        => some s!"\n> {Client.str j "text"}"
  | "agent"       =>
    match j.getObjVal? "event" |>.toOption with
    | some ev =>
      match (Lean.FromJson.fromJson? ev : Except String StreamFormat.Event) with
      | .ok e    => some (StreamFormat.format e)
      | .error _ => none
    | none => none
  -- Turn boundaries are structure, not content: the prompt below already shows when a turn is
  -- in flight, and printing both says the same thing twice.
  | "turnStarted" => none
  | "turnEnded"   =>
    let cost := match j.getObjValAs? Float "costUsd" |>.toOption with
      | some c => s!" | ${c}"
      | none   => ""
    some s!"[turn done]{cost}"
  | "notice"      => some s!"[{Client.str j "level"}] {Client.str j "message"}"
  | _             => none

/-- Exclusive use of the terminal for one burst of output.

    A chat has two writers — the follower thread printing the transcript, the input loop
    printing the prompt — and unsynchronised they interleave, putting a line of agent output
    through the middle of the prompt. It deliberately does not redraw the prompt afterwards:
    what has been typed so far is held by the terminal's line discipline rather than by this
    process, and a reprinted prompt would appear empty above characters that are still there. -/
private def withTerminal (lock : Std.BaseMutex) (act : IO.FS.Stream → IO Unit) : IO Unit := do
  let out ← IO.getStdout
  lock.lock
  try
    act out
    out.flush
  finally
    lock.unlock

/-- Print every event in one SSE payload, and answer the highest seq it carried.

    Rendered first and printed second, so one frame reaches the terminal as one burst rather
    than as lines the input loop can get between. -/
private def renderTranscriptPage (lock : Std.BaseMutex) (payload : Lean.Json) (after : Nat)
    : IO Nat := do
  let (items, _) := Client.items payload
  let mut last := after
  let mut lines : Array String := #[]
  for item in items do
    last := max last (Client.nat item "seq")
    if let some line := renderTranscriptEvent item then
      lines := lines.push line
  unless lines.isEmpty do
    withTerminal lock fun out => for line in lines do out.putStrLn line
  return last

private def chatSessionPath (id : String) : String :=
  s!"/api/v1/interactive/{Client.encodeSegment id}"

/-- Read one transcript stream until it ends, printing as it goes. -/
private partial def drainTranscript (lock : Std.BaseMutex) (stream : Utils.Http.Stream)
    (cursor : IO.Ref Nat) : IO Unit := do
  repeat do
    let some line ← stream.nextLine | break
    if let some data := Client.sseData line then
      if let .ok payload := Lean.Json.parse data then
        cursor.set (← renderTranscriptPage lock payload (← cursor.get))

/-- Follow a session's transcript for as long as someone is attached, reconnecting from the
    cursor whenever the stream drops.

    Run on its own thread while the main one reads the keyboard: a chat where you cannot type
    until the agent has finished talking is not a chat.

    A stream ends for three quite different reasons, and a client that cannot tell them apart is
    a client that goes quiet and says nothing about it. The conversation is over and the server
    closed it deliberately; the connection broke, which for a chat left open on a laptop is the
    common case rather than the exceptional one; or this client detached. Only the last is a
    reason to stop. Reconnecting from `cursor` is exact in both directions — the server replays
    what follows it and nothing else — so a reattach costs neither a missed event nor a repeated
    one.

    Detaching is what `stopped` and `current` are for. This thread spends almost all its time
    blocked inside `nextLine`, so a flag it polled between frames would be read once per event
    and never while it mattered; closing the stream from the other thread is what unblocks it,
    and the flag is what tells it afterwards not to open another. -/
private partial def followTranscript (cfg : Client.Config) (id : String) (lock : Std.BaseMutex)
    (cursor : IO.Ref Nat) (current : IO.Ref (Option Utils.Http.Stream))
    (stopped : IO.Ref Bool) (over : IO.Ref Bool) : IO Unit := do
  let mut live ← current.get
  let mut failures := 0
  repeat do
    if let some stream := live then
      try drainTranscript lock stream cursor catch _ => pure ()
      stream.close
      live := none
      current.set none
    if ← stopped.get then break
    -- The server closes the stream once the session is terminal. Ask which it was rather than
    -- reconnecting forever to a conversation that will never say anything again.
    match ← Client.get cfg (chatSessionPath id) with
    | .ok j =>
      let status := Client.str j "status"
      if status == "ended" || status == "failed" then
        over.set true
        withTerminal lock fun out => out.putStrLn s!"\n[session {status}]"
        break
    | .error _ => pure ()
    IO.sleep (500 + 1000 * min failures 4).toUInt32
    if ← stopped.get then break
    match ← (try some <$> Client.openTranscript cfg id (← cursor.get) catch _ => pure none) with
    | none =>
      failures := failures + 1
      if failures ≥ 5 then
        withTerminal lock fun out => out.putStrLn "\n[lost the transcript stream; the session \
is still on the backend — /quit and reattach with `orchestra chat --session ...`]"
        break
    | some next =>
      -- A clean drop is routine — the server's own connection timeout ends a quiet stream every
      -- few seconds — and since the cursor makes the reattach exact, there is nothing to say
      -- about it. Only a reconnect that had to be retried is news.
      let recovered := failures > 0
      failures := 0
      live := some next
      current.set (some next)
      -- A detach that landed while this was being opened would otherwise leave a stream running
      -- with nobody left to close it.
      if ← stopped.get then
        next.close
        live := none
        current.set none
        break
      if recovered then
        withTerminal lock fun out => out.putStrLn "\n[reconnected]"
  current.set none

/-- Post turns from standard input until it ends.

    A blank line is not a turn — it is someone pressing enter — and `/quit` detaches without
    ending the session, because detaching and ending are different things and only one of them
    can be undone.

    A turn the backend refused becomes the `draft`: it is shown again above the next prompt and
    an empty line re-sends it. Losing what someone typed because a connection blinked is a poor
    trade for four lines of state. -/
private partial def chatInputLoop (cfg : Client.Config) (id : String) (lock : Std.BaseMutex)
    (draft : Option String) : IO Unit := do
  let stdin ← IO.getStdin
  match draft with
  | none   => withTerminal lock fun out => out.putStr "\nyou> "
  | some d => withTerminal lock fun out =>
      out.putStr s!"\n[not sent] {d}\nretry> (enter re-sends it) "
  let line ← stdin.getLine
  -- End of input: the terminal closed, or someone typed Ctrl-D. Detach, do not end.
  if line.isEmpty then return
  let typed := line.trimAscii.toString
  let text := if typed.isEmpty then draft.getD "" else typed
  if text.isEmpty then return ← chatInputLoop cfg id lock none
  if text == "/quit" || text == "/detach" then return
  let body := Lean.Json.compress (Lean.Json.mkObj [("text", Lean.Json.str text)])
  -- The same fifteen minutes the start route gets, and for the same reason: a turn posted to a
  -- session the daemon has put down wakes it first, which is a clone, a token, an MCP server and
  -- a sandbox before anything is answered. At the default thirty seconds the client reports a
  -- failure for a turn that is being delivered, and the retry prompt then sends it twice.
  match ← Client.post cfg s!"{chatSessionPath id}/messages" body (maxTime := 900) with
  | .error e =>
    withTerminal lock fun out => out.putStrLn s!"\nThe turn was not delivered: {e}"
    chatInputLoop cfg id lock (some text)
  | .ok _ => chatInputLoop cfg id lock none

private def chatAttach (cfg : Client.Config) (id : String) (after : Nat) : IO UInt32 := do
  IO.println s!"Attached to session {id}. Type a turn and press enter; /quit detaches without \
ending it."
  let lock ← Std.BaseMutex.new
  let cursor ← IO.mkRef after
  let stopped ← IO.mkRef false
  let over ← IO.mkRef false
  let current ← IO.mkRef (some (← Client.openTranscript cfg id after))
  let follower ← IO.asTask (prio := .dedicated)
    (followTranscript cfg id lock cursor current stopped over)
  try chatInputLoop cfg id lock none
  finally
    -- Closing is what stops the follower: it is blocked on a read that only ends when the
    -- connection does. The flag goes up first, so a reconnect cannot start between the two.
    -- Waited on afterwards so the last frames are printed before the line below, rather than
    -- racing it or being lost when the process exits.
    stopped.set true
    if let some stream ← current.get then stream.close
    let _ ← IO.wait follower
  if ← over.get then
    IO.println "\nThe conversation is over."
  else
    IO.println s!"\nDetached. The session is still running; `orchestra chat --session {id}` \
picks it up, and `orchestra chat --end {id}` ends it."
  return 0

private def chatHandler (p : Parsed) : IO UInt32 := do
  withClient p fun cfg => do
    -- Four different things to do, and exactly one of them per invocation. Silently preferring
    -- whichever was checked first is how a mistyped flag becomes "why did that do nothing" —
    -- `--session x --budget 3` looked like it set a budget and did not, and `--list --end x`
    -- listed and left the session up.
    let startFlags := ["upstream", "fork", "backend", "model", "budget", "tools", "resume-from"]
      |>.filter (fun f => (p.flag? f).isSome)
      |>.map ("--" ++ ·)
    let modes := [("--list", p.hasFlag "list"),
                  ("--end", (p.flag? "end").isSome),
                  ("--session", (p.flag? "session").isSome),
                  (", ".intercalate startFlags, !startFlags.isEmpty)]
      |>.filter (·.2) |>.map (·.1)
    if modes.length > 1 then
      IO.eprintln s!"{", ".intercalate modes} are different things; give one of them."
      return 1
    -- List
    if p.hasFlag "list" then
      return ← apiCall (Client.get cfg "/api/v1/interactive") fun j => do
        let (rows, total) := Client.items j
        if rows.isEmpty then
          IO.println "No sessions."
        else
          IO.println s!"{padRight "SESSION" 22} {padRight "STATUS" 9} {padRight "REPO" 28} \
{padRight "TURNS" 6} TITLE"
          IO.println (String.ofList (List.replicate 96 '-'))
          for r in rows do
            IO.println s!"{padRight (Client.str r "id") 22} \
{padRight (Client.str r "status") 9} {padRight (Client.str r "fork") 28} \
{padRight (toString (Client.nat r "turnCount")) 6} {Client.str r "title"}"
          if total > rows.size then
            IO.println s!"\n{rows.size} of {total}."
    -- End
    if let some idFlag := p.flag? "end" then
      let id := idFlag.as! String
      return ← apiCall (Client.delete cfg (chatSessionPath id)) fun _ => do
        IO.println s!"Session {id} ended."
    -- Attach to one that exists. The transcript is replayed from the beginning, because a
    -- person re-attaching wants to see what was said, not an empty screen above a live tail.
    if let some idFlag := p.flag? "session" then
      let id := idFlag.as! String
      match ← Client.get cfg (chatSessionPath id) with
      | .error e => IO.eprintln e; return 1
      | .ok _    => return ← chatAttach cfg id 0
    -- Otherwise start one.
    let some upstream := p.flag? "upstream" |>.map (·.as! String)
      | do IO.eprintln "Give --upstream and --fork to start a session, --session to attach to \
one, --list to see them, or --end to end one."
           return 1
    let some fork := p.flag? "fork" |>.map (·.as! String)
      | do IO.eprintln "A session needs --fork as well as --upstream."
           return 1
    let mut fields : List (String × Lean.Json) :=
      [("upstream", Lean.Json.str upstream), ("fork", Lean.Json.str fork)]
    if let some b := p.flag? "backend" then
      fields := fields ++ [("backend", Lean.Json.str (b.as! String))]
    if let some m := p.flag? "model" then
      fields := fields ++ [("model", Lean.Json.str (m.as! String))]
    if let some bud := p.flag? "budget" then
      -- A budget that did not parse used to be dropped, and the session ran on the 20.0
      -- default: the one flag whose whole purpose is to bound spending, ignored in silence.
      let raw := bud.as! String
      let some v := parseFloat? raw
        | do IO.eprintln s!"--budget takes an amount in dollars; '{raw}' is not one."
             return 1
      fields := fields ++ [("budget", Lean.toJson v)]
    if let some t := p.flag? "tools" then
      let names := ((t.as! String).splitOn ",").map (·.trimAscii.toString)
                     |>.filter (!·.isEmpty)
      unless names == ["all"] do
        fields := fields ++ [("tools", Lean.Json.arr (names.map Lean.Json.str).toArray)]
    if let some r := p.flag? "resume-from" then
      fields := fields ++ [("resumeFrom", Lean.Json.str (r.as! String))]
    IO.println "Starting a session; this clones the repository and launches the agent..."
    -- The default 30 s is a read-a-file-off-disk timeout. This route clones a repository, mints
    -- a token, starts an MCP server and launches an agent inside the sandbox before it answers,
    -- and a client that gives up at 30 s reports a failure for a session that is starting
    -- perfectly well — and then leaves it running with nobody attached.
    match ← Client.post cfg "/api/v1/interactive"
            (Lean.Json.compress (Lean.Json.mkObj fields)) (maxTime := 900) with
    | .error e => IO.eprintln e; return 1
    | .ok j    => chatAttach cfg (Client.str j "id") 0

private def chatCmd : Cmd := `[Cli|
  chat VIA chatHandler; ["0.1.0"]
  "Talk to an agent the backend holds open: a session the dashboard and a phone can see too."

  FLAGS:
    upstream     : String; "Upstream repository in 'owner/repo' format"
    fork         : String; "Fork repository in 'owner/repo' format"
    session      : String; "Attach to an existing session by id instead of starting one"
    list;                  "List sessions and exit"
    "end"        : String; "End the session with this id and exit"
    backend      : String; "Agent backend (default: claude; it is the only one that can host a session)"
    model        : String; "Model override passed to the agent"
    budget       : String; "Maximum spend in USD for the whole session (default: 20.0)"
    tools        : String; "Comma-separated optional tools to enable, or 'all' (default: all)"
    "resume-from" : String; "Start a session that picks up the conversation of this one"
    "api-url" : String; "Backend to talk to (default: $ORCHESTRA_API_URL, or \
http://127.0.0.1:8080)"
    "api-token" : String; "Shared secret (default: $ORCHESTRA_DASHBOARD_PASSWORD, or the \
persisted one)"
]

private def interactiveHandler (p : Parsed) : IO UInt32 := do
  let upstreamStr := p.flag? "upstream" |>.map (·.as! String)
  let forkStr     := p.flag? "fork"     |>.map (·.as! String)
  let toolsStr    := p.flag? "tools"    |>.map (·.as! String)
  let backend     := p.flag? "backend"  |>.map (·.as! String)
  let model       := p.flag? "model"    |>.map (·.as! String)
  let budget      := p.flag? "budget"   |>.bind (fun v => parseFloat? (v.as! String)) |>.getD 4.0
  let debug       := p.hasFlag "debug"
  let configPath  := p.flag? "config"   |>.map (·.as! String)
  let authSource  := p.flag? "auth_source" |>.map (·.as! String)
  let authSources := (p.flag? "auth_sources" |>.map (·.as! String)).map
    (fun s => (s.splitOn ",").map (·.trimAscii.toString) |>.filter (!·.isEmpty)) |>.getD []
  -- Left as `none` when the flag is absent, so an interactive run takes the backend's
  -- `default_auth_mode` like every other path rather than forcing `ordered` onto a pool.
  let authMode    := (p.flag? "auth_mode" |>.map (·.as! String)).bind AuthMode.ofString?
  -- Both flags or neither, as everywhere else: with neither, this is a repository-independent
  -- session — a sandbox with the tracker tools and an empty workspace, which is how you get a
  -- look at what a repository-independent task sees.
  let repo : Option RepoPair ← match upstreamStr, forkStr with
    | none, none => pure none
    | some u, some f =>
      pure (some { upstream := ← IO.ofExcept (Repository.parse u)
                 , fork     := ← IO.ofExcept (Repository.parse f) })
    | _, _ => throw (.userError "--upstream and --fork are given together or not at all")
  let requested : List String := match toolsStr with
    | none | some "all" => allOptionalTools
    | some s => s.splitOn ","
  -- The repository-scoped tools are not offered to a session that has no repository; the MCP
  -- server refuses them anyway, and `--tools all` should not read as a promise it cannot keep.
  -- Through the same helper a queued task goes through, so `--tools create_pr` typed at a session
  -- with no repository is reported rather than dropped in silence — but not `--tools all` or its
  -- absence, which named nothing in particular and would say this on every such session.
  let namedTools := match toolsStr with
    | some s => s != "all"
    | none   => false
  let allowedTools ← Server.withoutRepoScopedTools repo requested (warn := namedTools)
  let appConfig ← loadAppConfig (configPath.map System.FilePath.mk)
  let jwt ← GitHub.createJWT appConfig.appId appConfig.privateKeyPath
  let installationId : Option Nat ← match appConfig.installationId with
    | some id => pure (some id)
    | none    =>
      match repo, appConfig.defaultOrganization with
      | some r, _      => some <$> GitHub.getInstallationId jwt r.fork.owner
      | none,   some o => some <$> GitHub.getInstallationId jwt o
      | none,   none   => pure none
  let token ← match installationId with
    | some id => GitHub.createInstallationToken jwt id
    | none    => pure ""
  unless token.isEmpty do GitHub.setupGhAuth token
  let repoPath ← match repo with
    | some r =>
      IO.println s!"Cloning/updating {r.fork}..."
      let p ← Repo.ensureCloned r.fork r.upstream
      IO.println s!"  Repo at {p}"
      pure p
    | none =>
      IO.println "Preparing the scratch workspace (no repository)..."
      let p ← Repo.ensureAdhocWorkspace
      IO.println s!"  Workspace at {p}"
      pure p
  let backendName := backend.getD "claude"
  let serverState : Server.State := {
    repo
    installationId
    allowedTools
    appId          := appConfig.appId
    privateKeyPath := appConfig.privateKeyPath
    pat            := appConfig.pat
    agentBackend   := backendName
    defaultOrganization := appConfig.defaultOrganization
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
  -- Interactive sessions go through the same resolver as queued and one-shot runs, so an
  -- account the daemon has already found to be out of quota is not handed to a human either.
  let resolved ← match ← Usage.resolveLabel appConfig backendName authSources authSource
                          authMode model with
    | .ok label => pure label
    | .error e  =>
      IO.eprintln s!"No usable authentication source for '{backendName}': {e}"
      shutdown
      return 1
  let apiKeyEnv ← TaskRunner.resolveAuthEnv appConfig agentDef backendName resolved
  if let some label := resolved then
    IO.println s!"  Auth source: {label}"
    Usage.markUsed backendName label
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
    upstream    : String; "Upstream repository in 'owner/repo' format (omit with --fork for a session with no repository)"
    fork        : String; "Fork repository in 'owner/repo' format (given together with --upstream, or not at all)"
    tools       : String; "Comma-separated optional tools to enable, or 'all' (default: all)"
    backend     : String; "Agent backend: claude (default), vibe, opencode, pi"
    model       : String; "Model override passed to the agent"
    budget      : String; "Maximum spend in USD (default: 4.0)"
    auth_source : String; "Authentication source label to use (overrides default_auth_source)"
    auth_sources : String; "Comma-separated candidate auth source labels, tried per --auth_mode"
    auth_mode   : String; "How to pick among --auth_sources: ordered (default) or distribute"
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
    chatCmd;
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
    configCmd;
    projectCmd;
    issueCmd;
    spawnCmd;
    rolesCmd;
    usageCmd;
    migrateCmd;
    dashboardCmd
]

def main (args : List String) : IO UInt32 := do
  Utils.unbufferIfPiped
  gRawArgs.set args
  Project.ensureTaxisConfigured
  orchestraCmd.validate args
