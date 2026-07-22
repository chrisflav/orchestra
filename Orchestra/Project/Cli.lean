import Cli
import Orchestra.Config
import Orchestra.GitHub
import Orchestra.Queue
import Orchestra.DaemonRequest
import Orchestra.TaskStore
import Orchestra.Utils.Format
import Orchestra.Utils.UnixSocket
import Orchestra.Project.Basic
import Orchestra.Project.Claim
import Orchestra.Project.Role

open Cli
open Orchestra (Repository Task)
open Orchestra.TaskStore (currentIso8601)
open Orchestra.Project (Project Issue IssueStatus RepoTarget
                        Role RoleTrigger DispatchPolicy
                        loadProject saveProject loadAllProjects createProject
                        loadIssue saveIssue loadIssues childrenOf findIssue createIssue
                        effectiveTarget
                        loadRole loadAllRoles roleSearchPaths render renderVarsFor)

namespace Orchestra.Project.Cli

/-! # CLI commands for projects and issues

Handlers and `Cli.Cmd` definitions live next to the project domain code so
that `Main.lean` only needs to import this module and reference `projectCmd`
/ `issueCmd` from its top-level subcommand list. -/

/-- Parse a `--flag`/positional string argument as a project/issue id (taxis's own numeric ids,
    see `Taxis.IssueId`), printing a CLI-appropriate error and returning `none` on anything that
    isn't a plain integer. -/
private def parseProjectIdArg (context : String) (s : String) : IO (Option Taxis.IssueId) := do
  match Taxis.IssueId.parse? s with
  | some pid => pure (some pid)
  | none => IO.eprintln s!"{context}: invalid project id '{s}' (expected a number)"; pure none

private def parseIssueIdArg (context : String) (s : String) : IO (Option Taxis.IssueId) := do
  match Taxis.IssueId.parse? s with
  | some iid => pure (some iid)
  | none => IO.eprintln s!"{context}: invalid issue id '{s}' (expected a number)"; pure none

private def parseRepoFlag? (p : Parsed) : Except String (Option Repository) := do
  match p.flag? "default-repo" |>.map (·.as! String) with
  | none   => return none
  | some s => return some (← Repository.parse s)

private def parseTargetFlags? (p : Parsed) : Except String (Option RepoTarget) := do
  let mRepo  ← match p.flag? "target-repo" |>.map (·.as! String) with
    | none   => Except.ok none
    | some s => (Repository.parse s).map some
  let mBranch := p.flag? "target-branch" |>.map (·.as! String)
  match mRepo, mBranch with
  | none,   none   => return none
  | some r, some b => return some { repo := r, branch := b }
  | _,      _      => Except.error "--target-repo and --target-branch must be set together"

private def issueStatusOfString? : String → Option IssueStatus
  | "open"      => some .open
  | "claimed"   => some .claimed
  | "completed" => some .completed
  | "abandoned" => some .abandoned
  | _           => none

private def issueStatusToString : IssueStatus → String
  | .open      => "open"
  | .claimed   => "claimed"
  | .completed => "completed"
  | .abandoned => "abandoned"

/-! ## Handlers -/

def projectCreateHandler (p : Parsed) : IO UInt32 := do
  let name        := p.positionalArg! "name" |>.as! String
  let description := p.flag? "description" |>.map (·.as! String)
  let defaultRepo ← match parseRepoFlag? p with
    | .ok x => pure x
    | .error e => IO.eprintln s!"orchestra project create: {e}"; return (1 : UInt32)
  let defaultBranch := p.flag? "default-branch" |>.map (·.as! String)
  let defaultTarget : Option RepoTarget ← match defaultRepo, defaultBranch with
    | none,   none   => pure none
    | some r, some b => pure (some { repo := r, branch := b })
    | _,      _      =>
      IO.eprintln "orchestra project create: --default-repo and --default-branch must be set together"
      return (1 : UInt32)
  let project ← createProject name description defaultTarget
  IO.println s!"Created project {project.id.toString} ({name})"
  return (0 : UInt32)

def projectListHandler (_ : Parsed) : IO UInt32 := do
  let projects ← loadAllProjects
  if projects.isEmpty then
    IO.println "No projects found."
    return (0 : UInt32)
  IO.println s!"{padRight "ID" 18} {padRight "CREATED" 22} {padRight "NAME" 30} DEFAULT TARGET"
  IO.println (String.ofList (List.replicate 100 '-'))
  for pr in projects do
    let target := match pr.defaultTarget with
      | some t => s!"{t.repo}@{t.branch}"
      | none   => "-"
    IO.println s!"{padRight pr.id.toString 18} {padRight pr.createdAt 22} {padRight pr.name 30} {target}"
  return (0 : UInt32)

/-- Whether the task holding a claim is still running — the first half of "is this claim live",
    shared by `project health` and `project releaseOrphans` so the two cannot disagree about
    which issues are orphaned.

    Still *running*, not merely "a record exists": records outlive the run, so testing only for
    existence made a claim left behind by a *finished* task read as healthy forever — exactly the
    claims that need clearing, and the ones an unbound `always` role produces if its teardown
    sweep does not run. A pre-claimed issue whose task has not started yet has no record at all
    (the claim carries the queue-entry id until `updateClaimTaskId` retags it); the caller's
    `onQueue` check is what covers that window. -/
private def claimTaskRunning (allTasks : Array TaskStore.TaskRecord) (taskId : Option String) :
    Bool :=
  taskId.any fun tid => allTasks.any fun t => t.id == tid && t.status matches .running

/-- Find all claimed issues in `pid` whose claiming task is no longer running and which have no
    active queue entry. Returns an array of `(issue, staleTaskId)` pairs. -/
private def findOrphanedIssues (pid : Taxis.IssueId) : IO (Array (Issue × String)) := do
  let issues ← loadIssues pid
  let claimed := issues.filter (·.status == .claimed)
  let allTasks ← TaskStore.loadAllTasks
  let allEntries ← Queue.loadAllEntries
  let activeEntries := allEntries.filter fun e =>
    e.status == .pending || e.status == .running
  let mut orphans : Array (Issue × String) := #[]
  for issue in claimed do
    let claim ← Project.loadClaim issue.id
    let taskId := claim.map (·.taskId)
    let hasTask := claimTaskRunning allTasks taskId
    let onQueue := activeEntries.any (·.issueId == some issue.id)
    if !hasTask && !onQueue then
      orphans := orphans.push (issue, taskId.getD "(no claim)")
  return orphans

def projectHealthHandler (p : Parsed) : IO UInt32 := do
  let id := p.positionalArg! "id" |>.as! String
  let some pid ← parseProjectIdArg "orchestra project health" id | return (1 : UInt32)
  match ← loadProject pid with
  | none =>
    IO.eprintln s!"Project '{id}' not found"
    return (1 : UInt32)
  | some _ =>
    let issues ← loadIssues pid
    let claimed := issues.filter (·.status == .claimed)
    if claimed.isEmpty then
      IO.println "All issues healthy (no claimed issues)."
      return (0 : UInt32)
    let allTasks ← TaskStore.loadAllTasks
    let allEntries ← Queue.loadAllEntries
    let activeEntries := allEntries.filter fun e =>
      e.status == .pending || e.status == .running
    let mut ok := true
    for issue in claimed do
      let claim ← Project.loadClaim issue.id
      let taskId := claim.map (·.taskId)
      let hasTask := claimTaskRunning allTasks taskId
      if hasTask then
        IO.println s!"[ok]      {issue.id.toString}  {issue.title}"
      else if activeEntries.any (·.issueId == some issue.id) then
        IO.println s!"[ok]      {issue.id.toString}  {issue.title}  — pending on queue"
      else
        ok := false
        let tid := taskId.getD "(no claim)"
        IO.println s!"[orphan]  {issue.id.toString}  {issue.title}  \
— claimed by task {tid}, which is no longer running"
    return if ok then 0 else 1

def projectReleaseOrphansHandler (p : Parsed) : IO UInt32 := do
  let id := p.positionalArg! "id" |>.as! String
  let some pid ← parseProjectIdArg "orchestra project releaseOrphans" id | return (1 : UInt32)
  match ← loadProject pid with
  | none =>
    IO.eprintln s!"Project '{id}' not found"
    return (1 : UInt32)
  | some _ =>
    let mgr ← Project.ClaimManager.new
    let now ← TaskStore.currentIso8601
    -- Release orphaned claimed issues.
    let orphans ← findOrphanedIssues pid
    for (issue, tid) in orphans do
      let _ ← Project.release mgr pid issue.id .open now
      IO.println s!"[released]  {issue.id.toString}  {issue.title}  (was claimed by {tid})"
    -- No unblock sweep: a decomposed parent is recognised by still having open children, so it
    -- becomes workable again the moment the last one closes. Nothing to reconcile.
    if orphans.isEmpty then
      IO.println "Nothing to do."
    return (0 : UInt32)

def projectShowHandler (p : Parsed) : IO UInt32 := do
  let id := p.positionalArg! "id" |>.as! String
  let some pid ← parseProjectIdArg "orchestra project show" id | return (1 : UInt32)
  match ← loadProject pid with
  | none =>
    IO.eprintln s!"Project '{id}' not found"
    return (1 : UInt32)
  | some pr =>
    IO.println s!"ID:          {pr.id.toString}"
    IO.println s!"Name:        {pr.name}"
    IO.println s!"Description: {pr.description.getD "-"}"
    IO.println s!"Created:     {pr.createdAt}"
    let target := match pr.defaultTarget with
      | some t => s!"{t.repo}@{t.branch}"
      | none   => "-"
    IO.println s!"Default tgt: {target}"
    let issues ← loadIssues pr.id
    IO.println s!"Issues:      {issues.size}"
    return (0 : UInt32)

def issueAddHandler (p : Parsed) : IO UInt32 := do
  let pidStr  := p.positionalArg! "project-id" |>.as! String
  let title   := p.flag?  "title" |>.map (·.as! String) |>.getD ""
  let descr   := p.flag?  "description" |>.map (·.as! String) |>.getD ""
  if title.isEmpty then
    IO.eprintln "orchestra issue add: --title is required"; return (1 : UInt32)
  let some pid ← parseProjectIdArg "orchestra issue add" pidStr | return (1 : UInt32)
  let parentId : Option Taxis.IssueId ← match p.flag? "parent" |>.map (·.as! String) with
    | none => pure none
    | some s =>
      let some iid ← parseIssueIdArg "orchestra issue add" s | return (1 : UInt32)
      pure (some iid)
  let project ← match ← loadProject pid with
    | none => IO.eprintln s!"Project '{pidStr}' not found"; return (1 : UInt32)
    | some pr => pure pr
  let target ← match parseTargetFlags? p with
    | .ok x => pure x
    | .error e => IO.eprintln s!"orchestra issue add: {e}"; return (1 : UInt32)
  if target.isNone && project.defaultTarget.isNone then
    IO.eprintln "orchestra issue add: project has no default target; pass --target-repo and --target-branch"
    return (1 : UInt32)
  let issue ← createIssue project.id title descr (parentId := parentId) (target := target)
  IO.println s!"Created issue {issue.id.toString} in project {project.id.toString}"
  return (0 : UInt32)

def issueListHandler (p : Parsed) : IO UInt32 := do
  let pidStr := p.positionalArg! "project-id" |>.as! String
  let mFilter := p.flag? "status" |>.map (·.as! String)
  let statusFilter? : Option IssueStatus ← match mFilter with
    | none   => pure none
    | some s => match issueStatusOfString? s with
      | some st => pure (some st)
      | none    =>
        IO.eprintln s!"orchestra issue list: invalid --status (got {s})"
        return (1 : UInt32)
  let some pid ← parseProjectIdArg "orchestra issue list" pidStr | return (1 : UInt32)
  let project ← match ← loadProject pid with
    | none => IO.eprintln s!"Project '{pidStr}' not found"; return (1 : UInt32)
    | some pr => pure pr
  let all ← loadIssues project.id
  let issues := match statusFilter? with
    | none   => all
    | some s => all.filter (·.status == s)
  if issues.isEmpty then
    IO.println "No matching issues."
    return (0 : UInt32)
  IO.println s!"{padRight "ID" 18} {padRight "STATUS" 10} {padRight "PARENT" 18} TITLE"
  IO.println (String.ofList (List.replicate 90 '-'))
  for i in issues do
    let parent := i.parentId.map (·.toString) |>.getD "-"
    IO.println s!"{padRight i.id.toString 18} {padRight (issueStatusToString i.status) 10} {padRight parent 18} {i.title}"
  return (0 : UInt32)

def issueShowHandler (p : Parsed) : IO UInt32 := do
  let id := p.positionalArg! "id" |>.as! String
  let some iid ← parseIssueIdArg "orchestra issue show" id | return (1 : UInt32)
  match ← findIssue iid with
  | none => IO.eprintln s!"Issue '{id}' not found"; return (1 : UInt32)
  | some (project, i) =>
    let target := (effectiveTarget project i).map (fun t => s!"{t.repo}@{t.branch}") |>.getD "-"
    IO.println s!"ID:           {i.id.toString}"
    IO.println s!"Project:      {project.id.toString} ({project.name})"
    IO.println s!"Parent:       {i.parentId.map (·.toString) |>.getD "-"}"
    IO.println s!"Title:        {i.title}"
    IO.println s!"Status:       {issueStatusToString i.status}"
    IO.println s!"Target:       {target}"
    let depsStr := if i.dependencies.isEmpty then "-"
                   else String.intercalate ", " (i.dependencies.map (·.toString)).toList
    IO.println s!"Dependencies: {depsStr}"
    IO.println s!"Created:      {i.createdAt}"
    IO.println s!"Updated:      {i.updatedAt}"
    if i.attachedPRs.isEmpty then
      IO.println "Attached PRs: -"
    else
      IO.println "Attached PRs:"
      for pr in i.attachedPRs do
        IO.println s!"  - {pr.repo}#{pr.number} (branch {pr.branch})"
    let children ← childrenOf project.id i.id
    if !children.isEmpty then
      IO.println "Children:"
      for c in children do
        IO.println s!"  - {c.id.toString}  {issueStatusToString c.status}  {c.title}"
    let allTasks ← TaskStore.loadAllTasks
    let issueTasks := allTasks.filter (·.issueId == some i.id)
      |>.toList.mergeSort (·.createdAt < ·.createdAt) |>.toArray
    if issueTasks.isEmpty then
      IO.println "Tasks:        -"
    else
      IO.println "Tasks:"
      for t in issueTasks do
        let role := t.role.map (s!" ({·})") |>.getD ""
        IO.println s!"  - {t.createdAt}  {repr t.status}  {t.id}{role}"
    IO.println "Description:"
    for line in i.description.splitOn "\n" do
      IO.println s!"  {line}"
    return (0 : UInt32)

/-! ## Daemon socket helper for pre-claim

Pre-claiming an issue must go through the daemon's `ClaimManager` so the
in-process mutex serialises CLI claims against agent claims. We talk to the
daemon over the same UNIX socket that `enqueueHandler` uses, sending a
`claim_issue` request and parsing the structured response. -/

private def daemonClaim (pid : Taxis.IssueId) (iid : Taxis.IssueId) (taskId : String)
    (agent : String) (series : Option String) : IO (Except String Unit) := do
  if !(← Queue.daemonRunning) then
    return .error "queue daemon is not running (start it with 'orchestra queue start')"
  let socketPath ← Queue.socketFile
  let conn ← Utils.UnixSocket.Connection.connect socketPath
  let req := Lean.Json.mkObj
    [ ("type",       "claim_issue")
    , ("project_id", Lean.ToJson.toJson pid)
    , ("issue_id",   Lean.ToJson.toJson iid)
    , ("task_id",    Lean.Json.str taskId)
    , ("agent",      Lean.Json.str agent)
    , match series with
      | some s => ("series", Lean.Json.str s)
      | none   => ("series", Lean.Json.null) ]
  conn.sendLine req.compress
  let line ← conn.recvLine
  conn.close
  match Lean.Json.parse line with
  | .error e => return .error s!"invalid daemon response: {e}"
  | .ok j =>
    if let .ok msg := j.getObjValAs? String "error" then
      return .error msg
    return .ok ()

/-! ## Spawn handler -/

private def renderTargetStr : RepoTarget → String
  | { repo, branch } => s!"{repo}@{branch}"

def spawnHandler (p : Parsed) : IO UInt32 := do
  let roleName := p.positionalArg! "role"       |>.as! String
  let pidStr   := p.positionalArg! "project-id" |>.as! String
  let issueIdFlag : Option Taxis.IssueId ← match p.flag? "issue" |>.map (·.as! String) with
    | none => pure none
    | some s =>
      let some iid ← parseIssueIdArg "orchestra spawn" s | return 1
      pure (some iid)
  let extraInstructions := p.flag? "prompt" |>.map (·.as! String) |>.getD ""
  -- 1. Resolve project + role.
  let some pid ← parseProjectIdArg "orchestra spawn" pidStr | return 1
  let project ← match ← loadProject pid with
    | none => IO.eprintln s!"Project '{pidStr}' not found"; return 1
    | some pr => pure pr
  let role ← match ← loadRole project.id roleName with
    | some r => pure r
    | none =>
      let (pPath, gPath) ← roleSearchPaths project.id roleName
      IO.eprintln s!"Role '{roleName}' not found. Searched:"
      IO.eprintln s!"  {pPath}"
      IO.eprintln s!"  {gPath}"
      return 1
  -- 2. Optionally bind an issue + pre-claim.
  let mIssue : Option Issue ← match issueIdFlag with
    | none => pure none
    | some iid =>
      match ← loadIssue project.id iid with
      | none =>
        IO.eprintln s!"Issue '{iid.toString}' not found in project {project.id.toString}"
        return 1
      | some i => pure (some i)
  -- 3. Determine pre-claim. Required for the issue to be safely held.
  let shouldPreClaim :=
    match mIssue, role.dispatch with
    | some _, some d => d.preClaim
    | some _, none   => true   -- default: if user gave --issue, claim it
    | none,   _      => false
  -- 4. Build the queue entry first (we need its id as the claim's task id).
  let entryId ← TaskStore.generateId
  let createdAt ← currentIso8601
  let target := mIssue.bind (effectiveTarget project ·)
            <|> project.defaultTarget
  let some target' := target
    | IO.eprintln "Cannot spawn: no effective target (project has no default and issue has no override)"
      return 1
  -- The agent pushes to `fork`; PRs land in `upstream` (= the target repo). When the App can push
  -- to the target directly the fork is the target itself, otherwise it is a fork in the configured
  -- default organisation.
  let appConfig ← loadAppConfig
  let some fork ← GitHub.resolveFork appConfig target'.repo
    | IO.eprintln s!"Cannot spawn: the GitHub App cannot push to {target'.repo} and it could not \
        be forked (see [fork] logs above; set default_organization to enable forking)"
      return 1
  let comments ← match mIssue with
    | some i => renderCommentThread i.id
    | none   => pure none
  let vars := renderVarsFor project mIssue extraInstructions (comments := comments)
  let prompt := render role.promptTemplate vars
  let entry : Queue.QueueEntry :=
    { id := entryId, createdAt
    , upstream      := target'.repo
    , fork
    , mode          := .pr
    , prompt
    , backend       := role.backend
    , model         := role.model
    , systemPrompt  := role.systemPrompt
    , prependPrompt := role.prependPrompt
    , budget        := role.budget
    , priority      := role.priority
    , readOnly      := role.readOnly
    , tools         := some role.permissions
    , projectId     := some project.id
    , issueId       := mIssue.map (·.id)
    , role          := some role.name }
  -- 5. Pre-claim if requested. We claim *before* writing the entry so a
  --    failed claim leaves no orphan queue entry.
  if shouldPreClaim then
    let some issue := mIssue
      | IO.eprintln "internal: pre-claim requested without --issue"; return (1 : UInt32)
    let agent := role.backend.getD "claude"
    match ← daemonClaim project.id issue.id entryId agent none with
    | .error msg =>
      IO.eprintln s!"Pre-claim failed: {msg}"
      return (1 : UInt32)
    | .ok () => pure ()
  -- 6. Persist the entry. The daemon picks it up on its next poll.
  Queue.saveEntry entry
  IO.println entryId
  return (0 : UInt32)

/-! ## Roles list -/

private def renderTriggerStr : RoleTrigger → String
  | .hasOpenIssues     => "has_open_issues"
  | .hasInReviewIssues => "has_in_review_issues"
  | .idle              => "idle"
  | .always            => "always"

def rolesListHandler (p : Parsed) : IO UInt32 := do
  let pidStr := p.positionalArg! "project-id" |>.as! String
  let some pid ← parseProjectIdArg "orchestra roles list" pidStr | return 1
  let project ← match ← loadProject pid with
    | none => IO.eprintln s!"Project '{pidStr}' not found"; return 1
    | some pr => pure pr
  let roles ← loadAllRoles project.id
  if roles.isEmpty then
    IO.println "No roles defined (looked under <data>/projects/<pid>/roles and <config>/roles)."
    return 0
  IO.println s!"{padRight "ROLE" 18} {padRight "PERMISSIONS" 36} TRIGGER  MAX  PRE-CLAIM"
  IO.println (String.ofList (List.replicate 90 '-'))
  for r in roles do
    let perms := String.intercalate "," r.permissions
    let (trig, maxStr, pcStr) := match r.dispatch with
      | none   => ("-", "-", "-")
      | some d => (renderTriggerStr d.trigger, toString d.max,
                   if d.preClaim then "yes" else "no")
    IO.println s!"{padRight r.name 18} {padRight perms 36} {padRight trig 8} {padRight maxStr 4} {pcStr}"
  return 0

/-- Default tools granted to a continuation that doesn't override `--tools`.
    Matches the canonical worker tool set: project work tools + PR + comment. -/
private def defaultContinueTools : List String :=
  ["work_issues", "create_pr", "comment"]

def issueContinueHandler (p : Parsed) : IO UInt32 := do
  let id      := p.positionalArg! "id" |>.as! String
  let prompt? := p.flag? "prompt" |>.map (·.as! String)
  let toolsOverride? := p.flag? "tools" |>.map (fun v =>
    (v.as! String).splitOn "," |>.map (·.trimAscii.toString) |>.filter (fun s => !s.isEmpty))
  let priorityFlag := p.flag? "priority" |>.map (·.as! Nat)
  let prompt ← match prompt? with
    | some t => pure t
    | none   => IO.eprintln "orchestra issue continue: --prompt is required"; return 1
  -- 1. Locate issue + claim.
  let some iid ← parseIssueIdArg "orchestra issue continue" id | return 1
  let some (project, issue) ← findIssue iid
    | IO.eprintln s!"Issue '{id}' not found"; return 1
  let some claim ← loadClaim issue.id
    | IO.eprintln s!"Issue '{id}' has no active claim — nothing to continue. \
        Either claim it first or use 'orchestra queue add'."; return 1
  -- 2. Pull repo / backend / model details from the prior task record.
  let some prevRecord ← TaskStore.loadTask claim.taskId
    | IO.eprintln s!"Claim references task '{claim.taskId}' which is not in the task store"
      return 1
  -- 3. Build the continuation entry. projectId/issueId are inherited from the
  --    issue (so the daemon's claim-release hook fires on terminal status).
  let id ← TaskStore.generateId
  let createdAt ← currentIso8601
  let entry : Queue.QueueEntry :=
    { id, createdAt
    , upstream      := prevRecord.upstream
    , fork          := prevRecord.fork
    , mode          := prevRecord.mode
    , prompt
    , continuesFrom := some claim.taskId
    , series        := claim.series
    , backend       := prevRecord.backend
    , model         := prevRecord.model
    , agent         := prevRecord.agent
    , systemPrompt  := prevRecord.systemPrompt
    , prependPrompt := prevRecord.prependPrompt
    , budget        := prevRecord.budget
    , priority      := priorityFlag.getD prevRecord.priority
    , projectId     := some project.id
    , issueId       := some issue.id
    , tools         := some (toolsOverride?.getD defaultContinueTools) }
  -- 4. Drop into the queue dir; the running daemon picks it up on next poll.
  --    No daemon-running precheck — the entry persists and runs whenever the
  --    daemon next starts, matching how task files behave.
  Queue.saveEntry entry
  IO.println entry.id
  if claim.series.isNone then
    IO.eprintln s!"  (note: claim has no series — new task continues from {claim.taskId} but is not series-tagged)"
  return 0

def issueCloseHandler (p : Parsed) : IO UInt32 := do
  let id := p.positionalArg! "id" |>.as! String
  let some iid ← parseIssueIdArg "orchestra issue close" id | return (1 : UInt32)
  match ← findIssue iid with
  | none => IO.eprintln s!"Issue '{id}' not found"; return (1 : UInt32)
  | some (_, i) =>
    let now ← TaskStore.currentIso8601
    saveIssue { i with status := .abandoned, updatedAt := now }
    IO.println s!"Issue {i.id.toString} marked abandoned"
    return (0 : UInt32)

/-! ## Issue → tasks listing -/

private def taskStatusToString : TaskStore.TaskStatus → String
  | .running    => "running"
  | .completed  => "completed"
  | .failed     => "failed"
  | .unfinished => "unfinished"
  | .cancelled  => "cancelled"

def issueTasksHandler (p : Parsed) : IO UInt32 := do
  let id := p.positionalArg! "id" |>.as! String
  let some iid ← parseIssueIdArg "orchestra issue tasks" id | return 1
  let some (_project, issue) ← findIssue iid
    | IO.eprintln s!"Issue '{id}' not found"; return 1
  let all ← TaskStore.loadAllTasks
  let matching := all.filter (fun r => r.issueId == some issue.id)
  if matching.isEmpty then
    IO.println s!"No tasks recorded for issue {issue.id.toString}."
    return 0
  IO.println s!"{padRight "TASK ID" 18} {padRight "CREATED" 22} {padRight "STATUS" 11} {padRight "ROLE" 16} SERIES"
  IO.println (String.ofList (List.replicate 90 '-'))
  -- Sort newest first (TaskStore.loadAllTasks already does this, but be explicit).
  let sorted := matching.qsort (fun a b => a.id > b.id)
  for r in sorted do
    let role   := r.role.getD "-"
    let series := r.series.getD "-"
    IO.println s!"{padRight r.id 18} {padRight r.createdAt 22} \
      {padRight (taskStatusToString r.status) 11} {padRight role 16} {series}"
  return 0


/-! ## `Cli.Cmd` definitions -/

private def subcommandDefault (_ : Parsed) : IO UInt32 := do
  IO.eprintln "Use a subcommand. Try '--help'."
  return 1

private def projectCreateCmd : Cmd := `[Cli|
  create VIA projectCreateHandler; ["0.1.0"]
  "Create a new project."

  FLAGS:
    description      : String; "Optional human description"
    "default-repo"   : String; "Default target repo for issues (owner/repo)"
    "default-branch" : String; "Default target branch for issues"

  ARGS:
    "name" : String; "Human-readable project name"
]

private def projectListCmd : Cmd := `[Cli|
  list VIA projectListHandler; ["0.1.0"]
  "List all projects."
]

private def projectShowCmd : Cmd := `[Cli|
  «show» VIA projectShowHandler; ["0.1.0"]
  "Show project details."

  ARGS:
    "id" : String; "Project ID"
]

private def projectHealthCmd : Cmd := `[Cli|
  health VIA projectHealthHandler; ["0.1.0"]
  "Check health of claimed issues: flag any whose claiming task record is missing."

  ARGS:
    "id" : String; "Project ID"
]

private def projectReleaseOrphansCmd : Cmd := `[Cli|
  releaseOrphans VIA projectReleaseOrphansHandler; ["0.1.0"]
  "Release all orphaned claimed issues (missing task, not on queue) back to open."

  ARGS:
    "id" : String; "Project ID"
]

def projectCmd : Cmd := `[Cli|
  project VIA subcommandDefault; ["0.1.0"]
  "Manage orchestra projects (organizational units grouping issues + tasks)."

  SUBCOMMANDS:
    projectCreateCmd;
    projectListCmd;
    projectShowCmd;
    projectHealthCmd;
    projectReleaseOrphansCmd
]

private def issueAddCmd : Cmd := `[Cli|
  add VIA issueAddHandler; ["0.1.0"]
  "Add an issue to a project."

  FLAGS:
    title           : String; "Issue title (required)"
    description     : String; "Issue description"
    parent          : String; "Parent issue ID (creates a sub-issue)"
    "target-repo"   : String; "Per-issue target repo override (owner/repo)"
    "target-branch" : String; "Per-issue target branch override"

  ARGS:
    "project-id" : String; "Project ID"
]

private def issueListCmd : Cmd := `[Cli|
  list VIA issueListHandler; ["0.1.0"]
  "List issues in a project."

  FLAGS:
    status : String; "Filter by status: open|claimed|completed|abandoned"

  ARGS:
    "project-id" : String; "Project ID"
]

private def issueShowCmd : Cmd := `[Cli|
  «show» VIA issueShowHandler; ["0.1.0"]
  "Show issue details (incl. children and attached PRs)."

  ARGS:
    "id" : String; "Issue ID"
]

private def issueCloseCmd : Cmd := `[Cli|
  close VIA issueCloseHandler; ["0.1.0"]
  "Mark an issue abandoned (use the review flow to mark it completed)."

  ARGS:
    "id" : String; "Issue ID"
]

private def issueTasksCmd : Cmd := `[Cli|
  tasks VIA issueTasksHandler; ["0.1.0"]
  "List all tasks recorded for an issue, newest first."

  ARGS:
    "id" : String; "Issue ID"
]

private def issueContinueCmd : Cmd := `[Cli|
  «continue» VIA issueContinueHandler; ["0.1.0"]
  "Enqueue a new task continuing the holder's series on a claimed issue."

  FLAGS:
    p, prompt : String; "New prompt for the continuation (required)"
    tools     : String; "Comma-separated tools list (default: work_issues,create_pr,comment)"
    priority  : Nat;    "Priority for the queued entry (default: inherited from prior task)"

  ARGS:
    "id" : String; "Issue ID currently held by a worker task"
]

def issueCmd : Cmd := `[Cli|
  issue VIA subcommandDefault; ["0.1.0"]
  "Manage project issues."

  SUBCOMMANDS:
    issueAddCmd;
    issueListCmd;
    issueShowCmd;
    issueCloseCmd;
    issueContinueCmd;
    issueTasksCmd
]

/-! ## Top-level spawn + roles commands -/

def spawnCmd : Cmd := `[Cli|
  spawn VIA spawnHandler; ["0.1.0"]
  "Spawn a task for a role from a role template (<config>/roles/<role>.json or per-project)."

  FLAGS:
    issue    : String; "Bind the task to this issue (required for hasOpenIssues roles to pre-claim)"
    p, prompt : String; "Free-form text passed as {{instructions}} to the role template"

  ARGS:
    "role"       : String; "Role name (filename in roles/ without .json)"
    "project-id" : String; "Project ID"
]

private def rolesListCmd : Cmd := `[Cli|
  list VIA rolesListHandler; ["0.1.0"]
  "List role templates available for a project (project-scoped overrides global)."

  ARGS:
    "project-id" : String; "Project ID"
]

def rolesCmd : Cmd := `[Cli|
  roles VIA subcommandDefault; ["0.1.0"]
  "Inspect role templates."

  SUBCOMMANDS:
    rolesListCmd
]

end Orchestra.Project.Cli
