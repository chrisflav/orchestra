import Cli
import Orchestra.Config
import Orchestra.Queue
import Orchestra.TaskStore
import Orchestra.Project.Basic
import Orchestra.Project.Claim

open Cli
open Orchestra (Repository Task)
open Orchestra.TaskStore (currentIso8601)
open Orchestra.Project (Project Issue ProjectId IssueId IssueStatus RepoTarget
                        loadProject saveProject loadAllProjects
                        loadIssue saveIssue loadIssues childrenOf findIssue
                        freshProjectId freshIssueId effectiveTarget)

namespace Orchestra.Project.Cli

/-! # CLI commands for projects and issues

Handlers and `Cli.Cmd` definitions live next to the project domain code so
that `Main.lean` only needs to import this module and reference `projectCmd`
/ `issueCmd` from its top-level subcommand list. -/

/-- Right-pad / truncate `s` to width `n`. Local copy to avoid pulling
    Main-only helpers across the module boundary. -/
private def padRight (s : String) (n : Nat) : String :=
  let truncated := String.ofList (s.toList.take n)
  truncated ++ String.ofList (List.replicate (n - truncated.length) ' ')

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
  | "in_review" => some .inReview
  | "completed" => some .completed
  | "blocked"   => some .blocked
  | "abandoned" => some .abandoned
  | _           => none

private def issueStatusToString : IssueStatus → String
  | .open      => "open"
  | .claimed   => "claimed"
  | .inReview  => "in_review"
  | .blocked   => "blocked"
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
  let now ← TaskStore.currentIso8601
  let pid ← freshProjectId
  let project : Project := { id := pid, name, description, createdAt := now, defaultTarget }
  saveProject project
  IO.println s!"Created project {pid.value} ({name})"
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
    IO.println s!"{padRight pr.id.value 18} {padRight pr.createdAt 22} {padRight pr.name 30} {target}"
  return (0 : UInt32)

def projectShowHandler (p : Parsed) : IO UInt32 := do
  let id := p.positionalArg! "id" |>.as! String
  match ← loadProject ⟨id⟩ with
  | none =>
    IO.eprintln s!"Project '{id}' not found"
    return (1 : UInt32)
  | some pr =>
    IO.println s!"ID:          {pr.id.value}"
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
  let parentId : Option IssueId := p.flag? "parent" |>.map (fun v => ⟨v.as! String⟩)
  let project ← match ← loadProject ⟨pidStr⟩ with
    | none => IO.eprintln s!"Project '{pidStr}' not found"; return (1 : UInt32)
    | some pr => pure pr
  let target ← match parseTargetFlags? p with
    | .ok x => pure x
    | .error e => IO.eprintln s!"orchestra issue add: {e}"; return (1 : UInt32)
  if target.isNone && project.defaultTarget.isNone then
    IO.eprintln "orchestra issue add: project has no default target; pass --target-repo and --target-branch"
    return (1 : UInt32)
  let now ← TaskStore.currentIso8601
  let iid ← freshIssueId
  let issue : Issue :=
    { id := iid, projectId := project.id, parentId, title, description := descr
    , target, createdAt := now, updatedAt := now }
  saveIssue issue
  IO.println s!"Created issue {iid.value} in project {project.id.value}"
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
  let project ← match ← loadProject ⟨pidStr⟩ with
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
    let parent := i.parentId.map (·.value) |>.getD "-"
    IO.println s!"{padRight i.id.value 18} {padRight (issueStatusToString i.status) 10} {padRight parent 18} {i.title}"
  return (0 : UInt32)

def issueShowHandler (p : Parsed) : IO UInt32 := do
  let id := p.positionalArg! "id" |>.as! String
  match ← findIssue ⟨id⟩ with
  | none => IO.eprintln s!"Issue '{id}' not found"; return (1 : UInt32)
  | some (project, i) =>
    let target := (effectiveTarget project i).map (fun t => s!"{t.repo}@{t.branch}") |>.getD "-"
    IO.println s!"ID:           {i.id.value}"
    IO.println s!"Project:      {project.id.value} ({project.name})"
    IO.println s!"Parent:       {i.parentId.map (·.value) |>.getD "-"}"
    IO.println s!"Title:        {i.title}"
    IO.println s!"Status:       {issueStatusToString i.status}"
    IO.println s!"Target:       {target}"
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
        IO.println s!"  - {c.id.value}  {issueStatusToString c.status}  {c.title}"
    IO.println "Description:"
    for line in i.description.splitOn "\n" do
      IO.println s!"  {line}"
    return (0 : UInt32)

/-- Default tools granted to a continuation that doesn't override `--tools`.
    Matches the canonical worker tool set: project work tools + PR + comment. -/
private def defaultContinueTools : List String :=
  ["work_issues", "create_pr", "comment"]

def issueContinueHandler (p : Parsed) : IO UInt32 := do
  let id      := p.positionalArg! "id" |>.as! String
  let prompt? := p.flag? "prompt" |>.map (·.as! String)
  let toolsOverride? := p.flag? "tools" |>.map (fun v =>
    (v.as! String).splitOn "," |>.map (·.trim) |>.filter (fun s => !s.isEmpty))
  let priorityFlag := p.flag? "priority" |>.map (·.as! Nat)
  let prompt ← match prompt? with
    | some t => pure t
    | none   => IO.eprintln "orchestra issue continue: --prompt is required"; return 1
  -- 1. Locate issue + claim.
  let some (project, issue) ← findIssue ⟨id⟩
    | IO.eprintln s!"Issue '{id}' not found"; return 1
  let some claim ← loadClaim project.id issue.id
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
  match ← findIssue ⟨id⟩ with
  | none => IO.eprintln s!"Issue '{id}' not found"; return (1 : UInt32)
  | some (_, i) =>
    let now ← TaskStore.currentIso8601
    saveIssue { i with status := .abandoned, updatedAt := now }
    IO.println s!"Issue {i.id.value} marked abandoned"
    return (0 : UInt32)

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

def projectCmd : Cmd := `[Cli|
  project VIA subcommandDefault; ["0.1.0"]
  "Manage orchestra projects (organizational units grouping issues + tasks)."

  SUBCOMMANDS:
    projectCreateCmd;
    projectListCmd;
    projectShowCmd
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
    status : String; "Filter by status: open|claimed|in_review|completed|abandoned"

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
    issueContinueCmd
]

end Orchestra.Project.Cli
