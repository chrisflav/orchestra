import Orchestra.Config
import Orchestra.AgentDef
import Orchestra.Agents.Claude
import Orchestra.Agents.Opencode
import Orchestra.Agents.Pi
import Orchestra.Agents.Vibe
import Orchestra.GitHub
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

/-- Enqueue a merger task for `pr` so the queue daemon will merge it later.
    Used by `decide_issue approve` via the `enqueueMerger` hook. Returns the
    new queue entry's ID on success, or an error when the GitHub App has no
    write access to `pr.repo` and therefore could not merge it. -/
def enqueueMergerImpl (appConfig : AppConfig) (pid : Taxis.IssueId) (iid : Taxis.IssueId)
    (pr : Project.PRRef) : IO (Except String String) := do
  try
    -- The merger is the one role-based task that cannot be served by a fork. It merges with
    -- `gh pr merge --repo <pr.repo>` using the App's installation token, so it needs write access
    -- to the PR's own repository; a fork would only give it somewhere else to push. If the App
    -- cannot push to `pr.repo` the merger is disabled rather than dispatched at a merge it cannot
    -- perform. An inconclusive probe is treated the same way — retried, then refused.
    let writable ← GitHub.probeWriteAccessRetrying appConfig pr.repo
    if writable != some true then
      let why := if writable == some false then "cannot push to" else "could not determine \
        whether it can push to"
      return .error s!"no merger was queued: the GitHub App {why} {pr.repo}, and merging requires \
        write access to the pull request's own repository (forking does not help). Merge \
        {pr.repo}#{pr.number} manually or grant the App write access."
    let id ← TaskStore.generateId
    let createdAt ← TaskStore.currentIso8601
    let entry : Queue.QueueEntry :=
      { id, createdAt
      -- Both sides are the PR's repo: the merger works directly in it, never in a fork.
      , upstream := pr.repo, fork := pr.repo
      , mode := .pr
      , prompt := s!"merge {pr.repo}#{pr.number}"
      , backend := some "merger"
      , projectId := some pid
      , issueId := some iid
      , priority := 100 }
    Queue.saveEntry entry
    return .ok id
  catch e =>
    return .error (toString e)

/-- Render the reviewer prompt template with the PR / issue context. -/
private def renderReviewerPrompt (tmpl : String) (pr : Project.PRRef) (iid : Taxis.IssueId)
    : String :=
  tmpl.replace "{{repo}}"      pr.repo.toString
    |>.replace "{{pr_number}}" (toString pr.number)
    |>.replace "{{branch}}"    pr.branch
    |>.replace "{{issue_id}}"  iid.toString

/-- Enqueue a reviewer task for `pr` against `tmpl`. Used by `attach_pr` via
    the `enqueueReviewer` hook when a project has a `reviewer` template. -/
def enqueueReviewerImpl (project : Project.Project) (iid : Taxis.IssueId)
    (pr : Project.PRRef) (tmpl : Project.ReviewerTemplate) : IO (Except String String) := do
  try
    let id ← TaskStore.generateId
    let createdAt ← TaskStore.currentIso8601
    let entry : Queue.QueueEntry :=
      { id, createdAt
      -- Both sides are the PR's repo, and no fork is resolved: the reviewer is `readOnly` with
      -- `review_issues`/`comment`/`get_pr_comments` below and no `create_pr`, so it never pushes
      -- anything. It needs a readable checkout, which the PR's own repo already is. Forking to
      -- give it one would create a repository per reviewed upstream for nothing.
      , upstream := pr.repo, fork := pr.repo
      , mode := .pr
      , prompt := renderReviewerPrompt tmpl.promptTemplate pr iid
      , backend := tmpl.backend
      , projectId := some project.id
      , issueId := some iid
      -- The reviewer needs review tools but should also be able to comment / read PRs.
      , tools := some ["review_issues", "comment", "get_pr_comments"]
      , readOnly := true
      , issueNumber := some pr.number
      , priority := 50 }
    Queue.saveEntry entry
    return .ok id
  catch e =>
    return .error (toString e)

/-- Run a merger task: checkout the PR branch, run the validation script, then shell out to
    `gh pr merge`. If validation fails the PR is not merged and the reason is recorded as a
    request-changes review on the issue, which returns to the open pool. Skips the entire agent /
    sandbox / MCP path. Used when `ioTask.backend = some "merger"`. -/
private def runMerger {i o : ResultType} (token : String) (ioTask : IOTask i o)
    (repoPath : System.FilePath) (initialRecord : TaskStore.TaskRecord) : IO Unit := do
  IO.println "  [merger] merge backend"
  -- Bound only to be validated: the merger reaches the issue through `findIssue` below, but a
  -- task arriving here without a project is malformed and should say so rather than fail later
  -- on something less obvious.
  let some _pid := ioTask.projectId
    | throw (.userError "merger task missing project_id")
  let some iid := ioTask.issueId
    | throw (.userError "merger task missing issue_id")
  let some (_, issue) ← Project.findIssue iid
    | throw (.userError s!"merger: issue {iid.toString} not found")
  let some pr := issue.attachedPRs.toList.reverse.head?
    | throw (.userError s!"merger: issue {iid.toString} has no attached PRs")
  let prRef := s!"{pr.repo}#{pr.number}"
  -- Checkout the PR branch in the shared repo clone.
  IO.println s!"  [merger] gh pr checkout {pr.number}"
  let coChild ← IO.Process.spawn
    { cmd := "gh"
      args := #["pr", "checkout", toString pr.number, "--repo", pr.repo.toString]
      cwd  := repoPath
      -- This task's own token, not whatever `gh` last had logged in globally.
      env  := #[("GH_TOKEN", some token), ("GITHUB_TOKEN", some token)]
      stdout := .piped
      stderr := .piped }
  let _ ← coChild.stdout.readToEnd
  let coStderr ← coChild.stderr.readToEnd
  let coExit ← coChild.wait
  if coExit != 0 then
    IO.eprintln s!"  [merger] pr checkout failed:\n{coStderr}"
    throw (.userError "pr checkout failed")
  -- Run the validation script before merging.
  IO.println "  [merger] running validation script"
  let (valid, validOutput) ← RepoConfig.runValidation repoPath
  if !validOutput.isEmpty then
    IO.println s!"  [merger] validation output:\n{validOutput}"
  if !valid then
    IO.eprintln s!"  [merger] validation failed for {prRef}, requesting changes"
    -- Recorded as a request-changes review rather than a status. The issue goes back to the open
    -- pool carrying the reason, so the next worker sees what failed instead of finding an issue
    -- parked in a state nothing dispatches. Failing to record must not swallow the failure
    -- itself, hence the catch.
    try
      Project.addComment iid
        s!"Validation failed for {prRef}, so it was not merged.\n\n```\n{validOutput}\n```"
        (review := some .requestChanges)
    catch e => IO.eprintln s!"  [merger] could not record the validation failure: {e}"
    let _ ← Project.forceRelease globalClaimManager iid
    TaskStore.saveTask { initialRecord with status := .failed }
    throw (.userError s!"validation failed for {prRef}")
  -- Validation passed: merge the PR.
  IO.println s!"  [merger] gh pr merge {prRef}"
  let mergeChild ← IO.Process.spawn
    { cmd := "gh"
      args := #["pr", "merge", toString pr.number, "--repo", pr.repo.toString, "--squash", "--delete-branch"]
      env  := #[("GH_TOKEN", some token), ("GITHUB_TOKEN", some token)]
      stdout := .piped
      stderr := .piped }
  let mergeOut ← mergeChild.stdout.readToEnd
  let mergeErr ← mergeChild.stderr.readToEnd
  let mergeExit ← mergeChild.wait
  if mergeExit != 0 then
    IO.eprintln s!"  [merger] gh pr merge failed (exit {mergeExit}):\n{mergeErr}"
    -- Leave the issue in .inReview so the reviewer can rerun the merger or re-decide.
    TaskStore.saveTask { initialRecord with status := .failed }
    throw (.userError s!"gh pr merge {prRef} failed")
  IO.println s!"  [merger] merged {prRef}\n{mergeOut}"
  -- Merging a pull request does not finish the issue. An issue can carry several PRs, and
  -- approving one says "this should land", not "the work is done" — completing is a separate
  -- decision a reviewer makes with `decide_issue complete`. The issue stays open, and since its
  -- PR is now merged the dispatcher stops queueing reviewers for it.
  let _ ← Project.forceRelease globalClaimManager iid
  TaskStore.saveTask { initialRecord with status := .completed }

/-- Run a triage task: add and/or remove labels on a GitHub issue or pull request.
    Skips the entire agent / sandbox / MCP path.
    Used when `ioTask.backend = some "triage"`. -/
private def runTriage {i o : ResultType} (pat : String) (ioTask : IOTask i o)
    (initialRecord : TaskStore.TaskRecord) : IO Unit := do
  IO.println "  [triage] label backend"
  let some issueNumber := ioTask.issueNumber
    | throw (.userError "triage task missing issue_number")
  let addLabels    := ioTask.triageAddLabels
  let removeLabels := ioTask.triageRemoveLabels
  if !addLabels.isEmpty then
    IO.println s!"  [triage] adding labels {addLabels} to {ioTask.upstream}#{issueNumber}"
    GitHub.addIssueLabels pat ioTask.upstream issueNumber addLabels
  for label in removeLabels do
    IO.println s!"  [triage] removing label '{label}' from {ioTask.upstream}#{issueNumber}"
    try
      GitHub.removeIssueLabel pat ioTask.upstream issueNumber label
    catch e =>
      IO.eprintln s!"  [triage] failed to remove label '{label}': {e}"
  TaskStore.saveTask { initialRecord with status := .completed }
  IO.println s!"  [triage] done"

/-- Resolve a task's `backend` field to the agent that implements it.
    Unknown and absent names both fall back to claude, which is the default backend. -/
def agentDefOfBackend (backend : Option String) : AgentDef :=
  match backend with
  | some "pi"       => AgentDef.pi
  | some "vibe"     => AgentDef.vibe
  | some "opencode" => AgentDef.opencode
  | _               => AgentDef.claude

/-- Whether a queue entry's backend tolerates another task running beside it.
    The non-agent backends (`merger`, `triage`) never launch a sandbox at all, so they are
    unconditionally safe rather than falling through to claude's answer. -/
def backendIsParallelSafe (backend : Option String) : Bool :=
  match backend with
  | some "merger" | some "triage" => true
  | _                             => (agentDefOfBackend backend).parallelSafe

/-- `plugin_dirs` from the config, with orchestra's own skills directory prepended when it
    exists (`Dirs.skillsDir`). The skills tell an agent to reach for the MCP tools rather than
    `gh` for anything touching pull requests or taxis issues, which it has no way to know
    otherwise. Silently skipped when not installed. -/
private def defaultPluginDirs (appConfig : AppConfig) : IO (Array String) := do
  let skills ← Dirs.skillsDir
  if ← skills.pathExists then
    return #[skills.toString] ++ appConfig.pluginDirs
  return appConfig.pluginDirs

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

/-- Build a system-prompt addition describing the available memory directories.

    Memory directories are shared by every task on the same upstream, and the queue daemon
    can run several of those at once. The one-file-per-memory convention below is what keeps
    that safe: two agents appending to a single `MEMORY.md` have no way to see each other's
    write, so the second one to save silently discards the first one's memory. Distinct files
    make concurrent writes disjoint, and reading the directory still reconstructs everything. -/
private def memorySystemPrompt (memoryDirs : Array String) : Option String :=
  if memoryDirs.isEmpty then none
  else
    let bullet := fun d => s!"- {d}"
    let list   := String.intercalate "\n" (memoryDirs.toList.map bullet)
    some s!"## Memory\n\nYou have access to a persistent memory system. \
The following director{if memoryDirs.size == 1 then "y is" else "ies are"} \
mounted read-write inside your sandbox:\n\n{list}\n\n\
Use these directories to store information that should persist across tasks: important \
context, decisions, and findings that will be valuable for future runs.\n\n\
Read the existing files at the start of a task to recover what previous runs learned.\n\n\
When you save something, write it as its **own new file** named after its subject \
(`build-cache-layout.md`, `flaky-integration-tests.md`), and never rewrite or append to a \
file another run created. Other tasks may be running against this same directory right now, \
and they cannot see your edits — appending to one shared file means whichever agent saves \
last silently erases what the others wrote. Correcting a memory you find to be wrong is \
fine; do it by adding a file that supersedes it rather than editing that file in place."

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
    (interactive : Bool := true)
    (interactiveAgent : Bool := false)
    -- When set, run in the per-repo clone slot it names instead of the shared cache clone.
    -- The slot is prepared here rather than by the caller because preparing it (cloning,
    -- fetching upstream) needs the installation token, which is only minted below.
    (slotOverride : Option Repo.SlotAssignment := none) : IO ((String × Bool) × Option o.Type × Option Lean.Json) := do
  IO.println s!"=== Task {idx}: {ioTask.fork} ({repr ioTask.mode}) ==="
  -- Record this run in the task store
  -- TODO: unify queue entry IDs and task IDs. Currently the queue entry gets
  -- one ID at enqueue time and the task gets a second ID here at run time,
  -- requiring the pre-claim retag in `updateClaimTaskId`. The fix is to
  -- pre-assign the task ID on the queue entry and pass it in instead of
  -- generating a new one, making `entry.taskId : Option String` redundant.
  let taskId ← TaskStore.generateId
  let createdAt ← TaskStore.currentIso8601
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
  TaskStore.saveTask initialRecord
  -- If this task was pre-claimed (daemon wrote the claim with the queue-entry
  -- ID before we ran), retag the claim with the real generated taskId so that
  -- ownership checks in attach_pr / split_issue pass.
  if let some iid := ioTask.issueId then
    Project.updateClaimTaskId globalClaimManager iid taskId
  -- Resolve initial resume session from the continued task
  let initialResume : Option String ← match continuesFrom with
    | none => pure none
    | some prevId =>
      match ← TaskStore.loadTask prevId with
      | none =>
        IO.eprintln s!"  Warning: task '{prevId}' not found, ignoring --continues"
        pure none
      | some prev => pure prev.sessionId
  -- Diagnostic block: surface the context the agent is starting with so the
  -- daemon log shows project/issue/series/resume info without having to grep
  -- the per-task .log file. Kept compact: prompt is dumped verbatim, but the
  -- prepend-prompt is shown by name only (its body is loaded later).
  IO.println s!"  Task ID: {taskId}"
  match ioTask.projectId with
  | none     => pure ()
  | some pid =>
    match ← Project.loadProject pid with
    | some pr => IO.println s!"  Project: {pid.toString} ({pr.name})"
    | none    => IO.println s!"  Project: {pid.toString} (not found)"
  match ioTask.issueId, ioTask.projectId with
  | some iid, some pid =>
    match ← Project.loadIssue pid iid with
    | some i => IO.println s!"  Issue:   {iid.toString} ({i.title})"
    | none   => IO.println s!"  Issue:   {iid.toString} (not found)"
  | some iid, none => IO.println s!"  Issue:   {iid.toString}"
  | none,     _    => pure ()
  match continuesFrom, initialResume with
  | some prevId, some sid =>
      IO.println s!"  Resuming task {prevId} (session {sid})"
  | some prevId, none =>
      IO.println s!"  Resuming task {prevId} (no session id recorded)"
  | none,        _    => pure ()
  match series with
  | some s => IO.println s!"  Series:  {s}"
  | none   => pure ()
  match ioTask.prependPrompt with
  | some name => IO.println s!"  Prepend prompt: {name}"
  | none      => pure ()
  IO.println "  Prompt:"
  for line in ioTask.prompt.splitOn "\n" do
    IO.println s!"    {line}"
  -- 1. Create GitHub App token
  IO.println "  Creating GitHub App token..."
  let jwt ← GitHub.createJWT appConfig.appId appConfig.privateKeyPath
  let forkOwner := ioTask.fork.owner
  let installationId ← match appConfig.installationId with
    | some id => pure id
    | none => GitHub.getInstallationId jwt forkOwner
  let token ← GitHub.createInstallationToken jwt installationId
  -- Deliberately no `GitHub.setupGhAuth` here. That writes the token to
  -- `~/.config/gh/hosts.yml`, which every concurrently running task shares, so under
  -- `--parallel > 1` the last task to authenticate supplies the credentials for all of them.
  -- The token is threaded explicitly instead: into `git`/`gh` through `Repo`, and into the
  -- sandbox through `Sandbox.launchAgent`'s `GH_TOKEN`.
  IO.println "  Token ready"
  -- Triage: add/remove labels on an issue or PR. Purely a GitHub API call, so it runs before
  -- any repository work — it needs no checkout, and provisioning a clone slot for it would
  -- mean a full `git clone` plus fetch to change a label.
  if ioTask.backend == some "triage" then
    runTriage appConfig.pat ioTask initialRecord
    return ((taskId, false), none, none)
  -- 2. Prepare the task slot, or clone / update the shared repo when running outside the queue
  let repoPath ← match slotOverride with
    | some assign =>
      IO.println s!"Preparing slot {assign.slot} for {ioTask.fork}..."
      let p ← Repo.ensureSlot ioTask.fork ioTask.upstream assign (token := some token)
      IO.println s!"  Slot at {p}"
      pure p
    | none   =>
      IO.println s!"Cloning/updating {ioTask.fork}..."
      let p ← Repo.ensureCloned ioTask.fork ioTask.upstream interactive (token := some token)
      IO.println s!"  Repo at {p}"
      pure p
  -- Merger: checkout the PR branch, run validation, then merge. Shares auth +
  -- clone setup with all other backends but skips the MCP server and agent.
  if ioTask.backend == some "merger" then
    runMerger token ioTask repoPath initialRecord
    return ((taskId, false), none, none)
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
    issueNumber := ioTask.issueNumber
    claimManager := some globalClaimManager
    taskId := some taskId
    agentBackend := ioTask.backend.getD "claude"
    series
    projectId := ioTask.projectId
    issueId   := ioTask.issueId
    enqueueMerger   := some (enqueueMergerImpl appConfig)
    enqueueReviewer := some enqueueReviewerImpl
    prLabels  := ioTask.prLabels
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
  let prependPrompt ← loadPrependPrompt ioTask.prependPrompt
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
    let agentDef := agentDefOfBackend ioTask.backend
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
      (debug := debug) (pluginDirs := ← defaultPluginDirs appConfig) (memoryDirs := memoryDirs)
      (subAgent := ioTask.agent) (model := ioTask.model) (systemPrompt := systemPrompt)
      (resume := resume) (budget := ioTask.budget.getD 4.0) (cancelToken := cancelToken)
      (extraEnv := apiKeyEnv) (debugLogFile := debugLogFile) (logFile := taskLogFile)
      (readOnly := ioTask.readOnly) (extraPorts := extraPorts)
      (additionalPaths := appConfig.additionalSandboxPaths)
      (interactiveAgent := interactiveAgent)
    IO.println s!"  Agent exited with code {result.exitCode}"
    sessionId := result.sessionId
    lastResultSubtype := result.resultSubtype
    if interactiveAgent then
      break
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
  -- Release the orchestra-issue claim on terminal status. A worker that succeeded and attached a
  -- PR leaves the issue awaiting review, so only drop the lock (forceRelease) and leave its
  -- status alone; anything else hands the issue back to the open pool for another worker.
  let releaseIssue (iid : Taxis.IssueId) : IO Unit := do
    match ← Project.findIssue iid with
    | none => pure ()
    | some (project, issue) =>
      let now ← TaskStore.currentIso8601
      let succeeded := finalStatus matches .completed
      if succeeded && !issue.attachedPRs.isEmpty then
        let _ ← Project.forceRelease globalClaimManager iid
      else
        let _ ← Project.release globalClaimManager project.id iid .open now
  match ioTask.projectId, ioTask.issueId with
  | some _pid, some iid => releaseIssue iid
  | some pid, none =>
    -- An unbound role (`always` trigger) was handed no issue to release, but it may have claimed
    -- any number of them itself via `claim_issue`. Those claims carry this task's id, and the
    -- branch above — the only thing that normally releases a claim at task end — never fires for
    -- them, so without this sweep they would stay `.claimed` for good. One `GET /issues/:id` per
    -- issue in the project (`loadClaims`), paid once at teardown of an unbound task only.
    for (iid, claim) in ← Project.loadClaims pid do
      if claim.taskId == taskId then releaseIssue iid
  | none, _ => pure ()
  if let some seriesName := series then
    TaskStore.updateSeriesPointer seriesName taskId
  IO.println s!"=== Task {idx} done ===\n"
  let outputJson ← outputRef.get
  let typedOutput : Option o.Type ← do
    match outputJson with
    | none => pure none
    | some j =>
      match ResultType.valueFromJson o j with
      | .ok v    => pure (some v)
      | .error e =>
        IO.eprintln s!"  Warning: failed to parse task output: {e}"
        pure none
  return ((taskId, usageLimitHit), typedOutput, outputJson)

/-- Run a single task: clone repo, start MCP server, run validation loop.
    Returns the task ID, whether the run was cut short by a usage limit, and the raw output JSON. -/
def runTask (appConfig : AppConfig) (task : Task) (idx : Nat) (debug : Bool)
    (continuesFrom : Option String := none)
    (series : Option String := none)
    (cancelToken : Option Std.CancellationToken := none)
    (interactive : Bool := true)
    (interactiveAgent : Bool := false)
    (slotOverride : Option Repo.SlotAssignment := none) : IO (String × Bool × Option Lean.Json) := do
  let ((taskId, usageLimitHit), _, outputJson) ←
    runIOTask appConfig task.ioTask idx debug default
      continuesFrom series cancelToken interactive interactiveAgent slotOverride
  return (taskId, usageLimitHit, outputJson)

end Orchestra.TaskRunner
