import Orchestra.Config
import Orchestra.AgentDef
import Orchestra.Agents.Claude
import Orchestra.Agents.Pi
import Orchestra.Agents.Vibe
import Orchestra.GitHub
import Orchestra.Repo
import Orchestra.RepoConfig
import Orchestra.Sandbox
import Orchestra.Server
import Orchestra.StreamFormat
import Orchestra.TaskStore
import Std.Sync

open Orchestra

namespace Orchestra.TaskRunner

private def sanitizeProjectName (upstream : String) : String :=
  upstream.map (fun c => if c == '/' then '-' else c)

/-- Return the active memory directories for the given mode and upstream repo.
    Creates the directories if they do not yet exist. -/
private def resolveMemoryDirs (mode : MemoryMode) (upstream : String) : IO (Array String) := do
  match ← IO.getEnv "HOME" with
  | none => return #[]
  | some home =>
    let memBase := System.FilePath.mk home / ".agent" / "memory"
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
private def resolveAuthEnv (appConfig : AppConfig) (agentDef : AgentDef)
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
      IO.eprintln s!"  Warning: duplicate auth source label '{label}' for backend '{backendName}'"
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
    (interactive : Bool := true) : IO ((String × Bool) × Option o.Type) := do
  IO.println s!"=== Task {idx}: {ioTask.fork} ({repr ioTask.mode}) ==="
  -- Record this run in the task store
  let taskId ← TaskStore.generateId
  let createdAt ← TaskStore.currentIso8601
  let initialRecord : TaskStore.TaskRecord := {
    id := taskId, createdAt
    upstream := ioTask.upstream, fork := ioTask.fork, mode := ioTask.mode
    prompt := ioTask.prompt, continuesFrom, series
    backend := ioTask.backend, model := ioTask.model, agent := ioTask.agent
    systemPrompt := ioTask.systemPrompt, budget := ioTask.budget
    priority := ioTask.priority
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
  let (forkOwner, _) ← Repo.splitRepo ioTask.fork
  let installationId ← match appConfig.installationId with
    | some id => pure id
    | none => GitHub.getInstallationId jwt forkOwner
  let token ← GitHub.createInstallationToken jwt installationId
  GitHub.setupGhAuth token
  IO.println "  Token ready"
  -- 2. Clone / update repo
  IO.println s!"Cloning/updating {ioTask.fork}..."
  let repoPath ← Repo.ensureCloned ioTask.fork ioTask.upstream interactive
  IO.println s!"  Repo at {repoPath}"
  -- 3. Start MCP server (runs in this process, outside the sandbox)
  -- Resolve allowed tools: prefer explicit `tools` list, fall back to `mode` for backwards compat
  let (allowedTools, usingModeFallback) := resolveTools ioTask.mode ioTask.tools
  if usingModeFallback then
    IO.eprintln s!"  Deprecation warning: the 'mode' field is deprecated. \
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
    outputRef := some outputRef
  }
  let (port, shutdown) ← Server.start serverState
  IO.println s!"  MCP server on port {port}"
  -- 4. Run init hook and load per-repository config
  RepoConfig.runInitIfNeeded repoPath
  let repoConfig ← RepoConfig.loadRepoConfig repoPath
  -- 5. Validation loop: before.sh → agent → validation.sh, retry on failure
  let baseSystemPrompt ← loadSystemPrompt ioTask.systemPrompt
  -- 5a. Resolve memory directories and amend system prompt
  let memoryDirs ← resolveMemoryDirs ioTask.memory ioTask.upstream
  let systemPrompt :=
    match baseSystemPrompt, memorySystemPrompt memoryDirs with
    | none,    none    => none
    | some sp, none    => some sp
    | none,    some mp => some mp
    | some sp, some mp => some (sp ++ "\n\n" ++ mp)
  -- 5b. Load prepend prompt and apply to task prompt
  let prependPrompt ← loadPrependPrompt
  let baseTaskPrompt :=
    match prependPrompt with
    | none    => ioTask.prompt
    | some pp => pp ++ "\n\n" ++ ioTask.prompt
  let mut sessionId : Option String := none
  let mut usageLimitHit := false
  let mut wasCancelled := false
  let mut lastValidationOutput : String := ""
  let mut lastResultSubtype : Option StreamFormat.ResultSubtype := none
  let maxAttempts := repoConfig.validation.maxRetries + 1
  for attempt in List.range maxAttempts do
    RepoConfig.runHook repoPath "before.sh"
    let prompt :=
      if attempt == 0 then baseTaskPrompt
      else repoConfig.validation.retryPrompt.replace "{{validation_output}}" lastValidationOutput
    let resume := if attempt == 0 then initialResume else sessionId
    IO.println s!"  Launching agent (attempt {attempt + 1}/{maxAttempts})..."
    let agentDef := match ioTask.backend with
      | some "pi"   => AgentDef.pi
      | some "vibe" => AgentDef.vibe
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
    -- Structured JSON log: ~/.agent/logs/{fork}/{taskId}.log (always)
    let taskLogFile : Option System.FilePath ← do
      match ← IO.getEnv "HOME" with
      | none => pure none
      | some home =>
        let suffix := if attempt == 0 then "" else s!".retry{attempt}"
        pure (some (System.FilePath.mk home / ".agent" / "logs" / ioTask.fork / s!"{taskId}{suffix}.log"))
    let result ← Sandbox.launchAgent agentDef repoPath prompt port token
      (debug := debug) (pluginDirs := appConfig.pluginDirs) (memoryDirs := memoryDirs)
      (subAgent := ioTask.agent) (model := ioTask.model) (systemPrompt := systemPrompt)
      (resume := resume) (budget := ioTask.budget.getD 4.0) (cancelToken := cancelToken)
      (extraEnv := apiKeyEnv) (debugLogFile := debugLogFile) (logFile := taskLogFile)
      (readOnly := ioTask.readOnly) (extraPorts := extraPorts)
    IO.println s!"  Agent exited with code {result.exitCode}"
    sessionId := result.sessionId
    lastResultSubtype := result.resultSubtype
    if result.wasCancelled then
      IO.println "  Agent was cancelled."
      wasCancelled := true
      break
    if result.usageLimitHit then
      IO.println "  Agent hit usage limit."
      usageLimitHit := true
      break
    if !(← RepoConfig.hasValidationScript repoPath) then
      IO.println "  No validation script found, skipping validation."
      break
    IO.println "  Running validation script..."
    let (valid, validationOutput) ← RepoConfig.runValidation repoPath
    lastValidationOutput := validationOutput
    if !validationOutput.isEmpty then
      IO.println s!"  Validation output:\n{validationOutput}"
    if valid then
      IO.println "  Validation passed."
      break
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
    else match lastResultSubtype with
      | some .success           => .completed
      | some .errorMaxBudgetUsd => .unfinished
      | some (.error _)         => .failed
      | _                       => if usageLimitHit then .unfinished else .completed
  TaskStore.saveTask { initialRecord with sessionId, status := finalStatus }
  if let some seriesName := series then
    TaskStore.updateSeriesPointer seriesName taskId
  IO.println s!"=== Task {idx} done ===\n"
  let typedOutput : Option o.Type ← do
    match ← outputRef.get with
    | none => pure none
    | some j =>
      match ResultType.valueFromJson o j with
      | .ok v    => pure (some v)
      | .error e =>
        IO.eprintln s!"  Warning: failed to parse task output: {e}"
        pure none
  return ((taskId, usageLimitHit), typedOutput)

end Orchestra.TaskRunner
