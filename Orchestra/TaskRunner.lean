import Orchestra.Config
import Orchestra.AgentDef
import Orchestra.Agents.Claude
import Orchestra.Agents.Opencode
import Orchestra.Agents.Pi
import Orchestra.Agents.Vibe
import Orchestra.GitHub
import Orchestra.Monad
import Orchestra.Project.Enqueue
import Orchestra.Repo
import Orchestra.RepoConfig
import Orchestra.Sandbox
import Orchestra.Project
import Orchestra.Queue
import Orchestra.Server
import Orchestra.StreamFormat
import Orchestra.TaskStore
import Std.Sync

open Orchestra

namespace Orchestra.TaskRunner

/-- Process-wide claim manager. The Std.BaseMutex inside it serialises
    intra-process claim acquisition; on-disk files survive restarts. -/
initialize globalClaimManager : Project.ClaimManager ← Project.ClaimManager.new


/-- Run a merger task: checkout the PR branch, run the validation script, then
    shell out to `gh pr merge`. If validation fails the issue is marked
    `.rejected` and the PR is not merged. Skips the entire agent / sandbox /
    MCP path. Used when `ioTask.backend = some "merger"`. -/
private def runMerger {i o : ResultType} (ioTask : IOTask i o)
    (repoPath : System.FilePath) (initialRecord : TaskStore.TaskRecord) : IO Unit := do
  logInfo "  [merger] merge backend"
  let some pid := ioTask.projectId
    | throw (.userError "merger task missing project_id")
  let some iid := ioTask.issueId
    | throw (.userError "merger task missing issue_id")
  let some (_, issue) ← Project.findIssue iid
    | throw (.userError s!"merger: issue {iid.value} not found")
  let some pr := issue.attachedPRs.toList.reverse.head?
    | throw (.userError s!"merger: issue {iid.value} has no attached PRs")
  let prRef := s!"{pr.repo}#{pr.number}"
  -- Checkout the PR branch in the shared repo clone.
  logInfo s!"  [merger] gh pr checkout {pr.number}"
  let coChild ← IO.Process.spawn
    { cmd := "gh"
      args := #["pr", "checkout", toString pr.number, "--repo", pr.repo.toString]
      cwd  := repoPath
      stdout := .piped
      stderr := .piped }
  let _ ← coChild.stdout.readToEnd
  let coStderr ← coChild.stderr.readToEnd
  let coExit ← coChild.wait
  if coExit != 0 then
    logError s!"  [merger] pr checkout failed:\n{coStderr}"
    throw (.userError "pr checkout failed")
  -- Run the validation script before merging.
  logInfo "  [merger] running validation script"
  let (valid, validOutput) ← RepoConfig.runValidation repoPath
  if !validOutput.isEmpty then
    logInfo s!"  [merger] validation output:\n{validOutput}"
  if !valid then
    logError s!"  [merger] validation failed for {prRef}, rejecting"
    let now ← currentIso8601
    Project.saveIssue { issue with status := .rejected, updatedAt := now }
    let _ ← Project.forceRelease globalClaimManager pid iid
    saveTask { initialRecord with status := .failed }
    throw (.userError s!"validation failed for {prRef}")
  -- Validation passed: merge the PR.
  logInfo s!"  [merger] gh pr merge {prRef}"
  let mergeChild ← IO.Process.spawn
    { cmd := "gh"
      args := #["pr", "merge", toString pr.number, "--repo", pr.repo.toString, "--squash", "--delete-branch"]
      stdout := .piped
      stderr := .piped }
  let mergeOut ← mergeChild.stdout.readToEnd
  let mergeErr ← mergeChild.stderr.readToEnd
  let mergeExit ← mergeChild.wait
  let now ← currentIso8601
  if mergeExit != 0 then
    logError s!"  [merger] gh pr merge failed (exit {mergeExit}):\n{mergeErr}"
    -- Leave the issue in .inReview so the reviewer can rerun the merger or re-decide.
    saveTask { initialRecord with status := .failed }
    throw (.userError s!"gh pr merge {prRef} failed")
  logInfo s!"  [merger] merged {prRef}\n{mergeOut}"
  Project.saveIssue { issue with status := .completed, updatedAt := now }
  let _ ← Project.forceRelease globalClaimManager pid iid
  saveTask { initialRecord with status := .completed }
  -- If this issue has a parent that is blocked, check whether all siblings
  -- are now completed and unblock the parent if so.
  if let some parentId := issue.parentId then
    if let some parent ← Project.loadIssue pid parentId then
      if parent.status == .blocked then
        let siblings ← Project.childrenOf pid parentId
        if siblings.all (·.status == .completed) then
          let now2 ← currentIso8601
          Project.saveIssue { parent with status := .open, updatedAt := now2 }
          logInfo s!"  [merger] all children of {parentId.value} completed; unblocked → open"

private def sanitizeProjectName (upstream : Repository) : String :=
  s!"{upstream.owner}-{upstream.name}"

/-- Return the active memory directories for the given mode and upstream repo.
    Creates the directories if they do not yet exist. -/
private def resolveMemoryDirs (mode : MemoryMode) (upstream : Repository) : IO (Array String) := do
  let memBase    := (← Dirs.dataBase) / "memory"
  let globalDir  := memBase
  let projectDir := memBase / sanitizeProjectName upstream
  let dirs : Array System.FilePath :=
    match mode with
    | .none    => #[]
    | .global  => #[globalDir]
    | .project => #[projectDir]
    | .both    => #[globalDir, projectDir]
  for dir in dirs do
    IO.FS.createDirAll dir
  return dirs.map (·.toString)

/-- Build a system-prompt addition describing the available memory directories. -/
private def memorySystemPrompt (memoryDirs : Array String) : Option String :=
  if memoryDirs.isEmpty then none
  else
    let bullet := fun d => s!"- {d}"
    let list   := String.intercalate "\n" (memoryDirs.toList.map bullet)
    some s!"## Memory\n\nYou have access to a persistent memory system. \
The following director{if memoryDirs.size == 1 then "y is" else "ies are"} \
mounted read-write inside your sandbox:\n\n{list}\n\n\
Use these directories to store information that should persist across tasks. \
For example, maintain a `MEMORY.md` file with important context, decisions, and findings \
that will be valuable for future runs."

/-- Derive the list of allowed optional tools from a task's `tools` and `mode` fields.
    If `tools` is `some list`, use it directly.
    If `tools` is `none`, fall back to mode-based rules for backwards compatibility:
    - `pr`   → `["create_pr"]`
    - `fork` → `[]`
    Returns the resolved tool list and a flag indicating whether the legacy `mode` fallback was used. -/
private def resolveTools (mode : TaskMode) (tools : Option (List String)) :
    List String × Bool :=
  match tools with
  | some ts => (ts, false)
  | none    => match mode with
    | .pr   => (["create_pr"], true)
    | .fork => ([], true)

/-- Resolve the authentication environment variables for a given backend and optional auth source label.
    If the task specifies an auth source label (or a default is configured), looks it up in the
    per-agent auth configs and returns its env vars via the agent's `envVarsOfAuthSource`.
    Falls back to the legacy flat API key fields when no per-agent auth is configured,
    preserving backward compatibility with existing configs. -/
def resolveAuthEnv (appConfig : AppConfig) (agentDef : AgentDef)
    (backendName : String) (requestedLabel : Option String)
    : IO (Array (String × Option String)) := do
  match appConfig.agentAuthConfigs.find? (fun c => c.name == backendName) with
  | none =>
    -- No per-agent auth configured: use legacy flat fields for backward compatibility
    return #[("ANTHROPIC_API_KEY",       appConfig.anthropicApiKey),
             ("ANTHROPIC_BASE_URL",      appConfig.anthropicBaseUrl),
             ("ANTHROPIC_AUTH_TOKEN",    appConfig.anthropicAuthToken),
             ("CLAUDE_CODE_OAUTH_TOKEN", appConfig.claudeToken)]
  | some agentAuth =>
    -- Determine which label to use
    let label ← match requestedLabel with
      | some l => pure l
      | none   =>
        match agentAuth.defaultAuthSource with
        | some d => pure d
        | none   =>
          if agentAuth.authSources.size == 1 then
            pure agentAuth.authSources[0]!.label
          else
            throw (.userError
              s!"Multiple auth sources configured for backend '{backendName}'. \
                 Specify one via the 'auth_source' field.")
    -- Validate label uniqueness (warn on duplicates)
    let dupeCount := agentAuth.authSources.filter (fun s => s.label == label) |>.size
    if dupeCount > 1 then
      logError s!"  Warning: duplicate auth source label '{label}' for backend '{backendName}'"
    -- Find the source with the matching label
    match agentAuth.authSources.find? (fun s => s.label == label) with
    | none => throw (.userError
        s!"Auth source '{label}' not found for backend '{backendName}'. \
           Available: {", ".intercalate (agentAuth.authSources.toList.map (·.label))}")
    | some src =>
      let vars := agentDef.envVarsOfAuthSource src
      return vars.map fun (k, v) => (k, some v)

/-- Run a single IOTask: clone repo, start MCP server, run validation loop.
    Returns the task ID, whether the run was cut short by a usage limit, and the typed output. -/
def runIOTask {i o : ResultType} (appConfig : AppConfig) (ioTask : IOTask i o)
    (idx : Nat) (debug : Bool) (input : i.Type)
    (continuesFrom : Option String := none)
    (series : Option String := none)
    (cancelToken : Option Std.CancellationToken := none)
    (interactive : Bool := true)
    (interactiveAgent : Bool := false) : IO ((String × Bool) × Option o.Type × Option Lean.Json) := do
  logInfo s!"=== Task {idx}: {ioTask.fork} ({repr ioTask.mode}) ==="
  -- Record this run in the task store
  -- TODO: unify queue entry IDs and task IDs. Currently the queue entry gets
  -- one ID at enqueue time and the task gets a second ID here at run time,
  -- requiring the pre-claim retag in `updateClaimTaskId`. The fix is to
  -- pre-assign the task ID on the queue entry and pass it in instead of
  -- generating a new one, making `entry.taskId : Option String` redundant.
  let taskId ← generateTaskId
  let createdAt ← currentIso8601
  let initialRecord : TaskStore.TaskRecord := {
    id := taskId, createdAt
    upstream := ioTask.upstream, fork := ioTask.fork, mode := ioTask.mode
    prompt := ioTask.prompt, continuesFrom, series
    backend := ioTask.backend, model := ioTask.model, agent := ioTask.agent
    systemPrompt := ioTask.systemPrompt, budget := ioTask.budget
    priority := ioTask.priority
    projectId := ioTask.projectId
    issueId   := ioTask.issueId
    role      := ioTask.role
  }
  if !interactiveAgent then
    saveTask initialRecord
    -- If this task was pre-claimed (daemon wrote the claim with the queue-entry
    -- ID before we ran), retag the claim with the real generated taskId so that
    -- ownership checks in attach_pr / split_issue pass.
    if let (some pid, some iid) := (ioTask.projectId, ioTask.issueId) then
      Project.updateClaimTaskId globalClaimManager pid iid taskId
  -- Resolve initial resume session from the continued task
  let initialResume : Option String ← match continuesFrom with
    | none => pure none
    | some prevId =>
      match ← loadTask prevId with
      | none =>
        logError s!"  Warning: task '{prevId}' not found, ignoring --continues"
        pure none
      | some prev => pure prev.sessionId
  -- Diagnostic block: surface the context the agent is starting with so the
  -- daemon log shows project/issue/series/resume info without having to grep
  -- the per-task .log file. Kept compact: prompt is dumped verbatim, but the
  -- prepend-prompt is shown by name only (its body is loaded later).
  logInfo s!"  Task ID: {taskId}"
  match ioTask.projectId with
  | none     => pure ()
  | some pid =>
    match ← Project.loadProject pid with
    | some pr => logInfo s!"  Project: {pid.value} ({pr.name})"
    | none    => logInfo s!"  Project: {pid.value} (not found)"
  match ioTask.issueId, ioTask.projectId with
  | some iid, some pid =>
    match ← Project.loadIssue pid iid with
    | some i => logInfo s!"  Issue:   {iid.value} ({i.title})"
    | none   => logInfo s!"  Issue:   {iid.value} (not found)"
  | some iid, none => logInfo s!"  Issue:   {iid.value}"
  | none,     _    => pure ()
  match continuesFrom, initialResume with
  | some prevId, some sid =>
      logInfo s!"  Resuming task {prevId} (session {sid})"
  | some prevId, none =>
      logInfo s!"  Resuming task {prevId} (no session id recorded)"
  | none,        _    => pure ()
  match series with
  | some s => logInfo s!"  Series:  {s}"
  | none   => pure ()
  match ioTask.prependPrompt with
  | some name => logInfo s!"  Prepend prompt: {name}"
  | none      => pure ()
  logInfo "  Prompt:"
  for line in ioTask.prompt.splitOn "\n" do
    logInfo s!"    {line}"
  -- 1. Create GitHub App token
  logInfo "  Creating GitHub App token..."
  let jwt ← createJWT appConfig.appId appConfig.privateKeyPath
  let forkOwner := ioTask.fork.owner
  let installationId ← match appConfig.installationId with
    | some id => pure id
    | none => getInstallationId jwt forkOwner
  let token ← createInstallationToken jwt installationId
  setupGhAuth token
  logInfo "  Token ready"
  -- 2. Clone / update repo
  logInfo s!"Cloning/updating {ioTask.fork}..."
  let repoPath ← Repo.ensureCloned ioTask.fork ioTask.upstream interactive
  logInfo s!"  Repo at {repoPath}"
  -- Merger: checkout the PR branch, run validation, then merge. Shares auth +
  -- clone setup with all other backends but skips the MCP server and agent.
  if ioTask.backend == some "merger" then
    runMerger ioTask repoPath initialRecord
    return ((taskId, false), none, none)
  -- 3. Start MCP server (runs in this process, outside the sandbox)
  -- Resolve allowed tools: prefer explicit `tools` list, fall back to `mode` for backwards compat
  let (allowedTools, usingModeFallback) := resolveTools ioTask.mode ioTask.tools
  if usingModeFallback then
    logError s!"  Deprecation warning: the 'mode' field is deprecated. \
      Use 'tools' instead (e.g. {repr allowedTools}) and optionally 'read_only: true/false'."
  let inputJson := some (ResultType.valueToJson i input)
  let outputRef ← IO.mkRef (none : Option Lean.Json)
  let serverState : Server.State := {
    upstream := ioTask.upstream
    fork := ioTask.fork
    allowedTools
    appId := appConfig.appId
    privateKeyPath := appConfig.privateKeyPath
    installationId
    pat := appConfig.pat
    inputType := i
    outputType := o
    inputJson
    issueNumber := ioTask.issueNumber
    claimManager := some globalClaimManager
    taskId := some taskId
    agentBackend := ioTask.backend.getD "claude"
    series
    projectId := ioTask.projectId
    issueId   := ioTask.issueId
    canEnqueueMerger   := !interactiveAgent
    canEnqueueReviewer := !interactiveAgent
  }
  let (port, shutdown) ← Server.start serverState outputRef
  logInfo s!"  MCP server on port {port}"
  -- 4. Run init hook and load per-repository config
  RepoConfig.runInitIfNeeded repoPath
  let repoConfig ← RepoConfig.loadRepoConfig repoPath
  -- 5. Validation loop: before.sh → agent → validation.sh, retry on failure
  let baseSystemPrompt ← readSystemPrompt ioTask.systemPrompt
  -- 5a. Resolve memory directories and amend system prompt
  let memoryDirs ← resolveMemoryDirs ioTask.memory ioTask.upstream
  let systemPrompt :=
    match baseSystemPrompt, memorySystemPrompt memoryDirs with
    | none,    none    => none
    | some sp, none    => some sp
    | none,    some mp => some mp
    | some sp, some mp => some (sp ++ "\n\n" ++ mp)
  -- 5b. Load prepend prompt and apply to task prompt
  let prependPrompt ← readPrependPrompt ioTask.prependPrompt
  let baseTaskPrompt :=
    match prependPrompt with
    | none    => ioTask.prompt
    | some pp => pp ++ "\n\n" ++ ioTask.prompt
  let mut sessionId : Option String := none
  let mut usageLimitHit := false
  let mut wasCancelled := false
  let mut lastValidationOutput : String := ""
  let mut lastResultSubtype : Option StreamFormat.ResultSubtype := none
  let maxAttempts := if interactiveAgent then 1 else repoConfig.validation.maxRetries + 1
  for attempt in List.range maxAttempts do
    if !interactiveAgent then
      RepoConfig.runHook repoPath "before.sh"
    let prompt :=
      if attempt == 0 then baseTaskPrompt
      else repoConfig.validation.retryPrompt.replace "{{validation_output}}" lastValidationOutput
    let resume := if attempt == 0 then initialResume else sessionId
    logInfo s!"  Launching agent (attempt {attempt + 1}/{maxAttempts})..."
    let agentDef := match ioTask.backend with
      | some "pi"       => AgentDef.pi
      | some "vibe"     => AgentDef.vibe
      | some "opencode" => AgentDef.opencode
      | _           => AgentDef.claude
    let backendName := ioTask.backend.getD "claude"
    let apiKeyEnv ← resolveAuthEnv appConfig agentDef backendName ioTask.authSource
    let extraPorts := appConfig.agentAuthConfigs.find? (fun c => c.name == backendName)
      |>.map (·.extraPorts) |>.getD #[]
    let debugLogFile : Option System.FilePath ←
      if debug then
        let suffix := if attempt == 0 then "" else s!".retry{attempt}"
        pure (some ((← TaskStore.tasksDir) / s!"{taskId}{suffix}.debug.jsonl"))
      else pure none
    let taskLogFile : Option System.FilePath ← do
      let suffix := if attempt == 0 then "" else s!".retry{attempt}"
      pure (some ((← Dirs.dataBase) / "logs" / ioTask.fork.toString / s!"{taskId}{suffix}.log"))
    let result ← Sandbox.launchAgent agentDef repoPath prompt port token
      (debug := debug) (pluginDirs := appConfig.pluginDirs) (memoryDirs := memoryDirs)
      (subAgent := ioTask.agent) (model := ioTask.model) (systemPrompt := systemPrompt)
      (resume := resume) (budget := ioTask.budget.getD 4.0) (cancelToken := cancelToken)
      (extraEnv := apiKeyEnv) (debugLogFile := debugLogFile) (logFile := taskLogFile)
      (readOnly := ioTask.readOnly) (extraPorts := extraPorts)
      (additionalPaths := appConfig.additionalSandboxPaths)
      (interactiveAgent := interactiveAgent)
    logInfo s!"  Agent exited with code {result.exitCode}"
    sessionId := result.sessionId
    lastResultSubtype := result.resultSubtype
    if interactiveAgent then
      break
    if result.wasCancelled then
      logInfo "  Agent was cancelled."
      wasCancelled := true
      break
    if result.usageLimitHit then
      logInfo "  Agent hit usage limit."
      usageLimitHit := true
      break
    if !(← RepoConfig.hasValidationScript repoPath) then
      logInfo "  No validation script found, skipping validation."
      break
    logInfo "  Running validation script..."
    let (valid, validationOutput) ← RepoConfig.runValidation repoPath
    lastValidationOutput := validationOutput
    if !validationOutput.isEmpty then
      logInfo s!"  Validation output:\n{validationOutput}"
    if valid then
      logInfo "  Validation passed."
      break
    if attempt + 1 < maxAttempts then
      logInfo s!"  Validation failed, retrying ({attempt + 1}/{repoConfig.validation.maxRetries})..."
    else
      logError s!"  Validation still failing after {repoConfig.validation.maxRetries} retries"
  -- 6. Run after hook and shut down MCP server
  if !interactiveAgent then
    RepoConfig.runHook repoPath "after.sh"
  shutdown
  if !interactiveAgent then
    -- 7. Persist final task state
    let finalStatus :=
      if wasCancelled then .cancelled
      else match lastResultSubtype with
        | some .success           => .completed
        | some .errorMaxBudgetUsd => .unfinished
        | some (.error _)         => .failed
        | _                       => if usageLimitHit then .unfinished else .completed
    saveTask { initialRecord with sessionId, status := finalStatus }
    -- Release the orchestra-issue claim on terminal status. If the worker
    -- already moved the issue to .inReview via attach_pr, leave that status
    -- alone and only delete the lock file (forceRelease). Otherwise hand the
    -- issue back to the open pool so another worker can pick it up.
    match ioTask.projectId, ioTask.issueId with
    | some _pid, some iid =>
      match ← Project.findIssue iid with
      | some (project, issue) =>
        let now ← currentIso8601
        let succeeded := finalStatus matches .completed
        if succeeded && issue.status == .inReview then
          let _ ← Project.forceRelease globalClaimManager project.id iid
        else
          let _ ← Project.release globalClaimManager project.id iid .open now
      | none => pure ()
    | _, _ => pure ()
    if let some seriesName := series then
      updateSeriesPointer seriesName taskId
  logInfo s!"=== Task {idx} done ===\n"
  let outputJson ← outputRef.get
  let typedOutput : Option o.Type ← do
    match outputJson with
    | none => pure none
    | some j =>
      match ResultType.valueFromJson o j with
      | .ok v    => pure (some v)
      | .error e =>
        logError s!"  Warning: failed to parse task output: {e}"
        pure none
  return ((taskId, usageLimitHit), typedOutput, outputJson)

/-- Run a single task: clone repo, start MCP server, run validation loop.
    Returns the task ID, whether the run was cut short by a usage limit, and the raw output JSON. -/
def runTask (appConfig : AppConfig) (task : Task) (idx : Nat) (debug : Bool)
    (continuesFrom : Option String := none)
    (series : Option String := none)
    (cancelToken : Option Std.CancellationToken := none)
    (interactive : Bool := true)
    (interactiveAgent : Bool := false) : IO (String × Bool × Option Lean.Json) := do
  let ((taskId, usageLimitHit), _, outputJson) ←
    runIOTask appConfig task.ioTask idx debug default
      continuesFrom series cancelToken interactive interactiveAgent
  return (taskId, usageLimitHit, outputJson)

end Orchestra.TaskRunner
