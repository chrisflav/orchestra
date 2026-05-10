import Lean.Data.Json
import Orchestra.Monad.Log
import Orchestra.Monad.TaskStore
import Orchestra.Monad.Queue
import Orchestra.Monad.Project
import Orchestra.Monad.Claim
import Orchestra.Project.Enqueue

open Lean (Json FromJson ToJson)
open Orchestra (logInfo logError currentIso8601 MonadProject MonadClaim)
open Orchestra.Project.Enqueue (enqueueMergerImpl enqueueReviewerImpl)

namespace Orchestra.Project.Tools

/-! # MCP tools for projects and issues

This module defines the three tool groups (`manage_issues`, `work_issues`,
`review_issues`) as a single `ProjectTool` enum plus a parser and evaluator.
`Orchestra.Server` only needs to call into `tryParseToolCall` /
`evalProjectTool` and surface the resulting JSON to the agent.

Permission gating happens here, not in the server: each evaluator branch
checks `env.allowedTools` against the appropriate group label and returns an
error toolContent if the agent is not allowed. -/

/-- Permission-group labels the server's `allowedTools` list may contain. -/
def manageIssuesPerm : String := "manage_issues"
def workIssuesPerm   : String := "work_issues"
def reviewIssuesPerm : String := "review_issues"

inductive ReviewDecision where
  | approve
  | reject
deriving Repr, Inhabited

/-- One sub-issue spec passed to `split_issue`. Per-child target is optional;
    when `none`, the child inherits the parent's `effectiveTarget`. -/
structure NewSubissueSpec where
  title       : String
  description : String
  target      : Option RepoTarget := none
deriving Repr, Inhabited

inductive ProjectTool where
  -- manage_issues
  | listProjects
  | listIssues       (projectId : ProjectId) (statusFilter : Option IssueStatus)
                     (parentId : Option IssueId)
  | getIssue         (issueId : IssueId)
  | createIssue      (projectId : ProjectId) (title description : String)
                     (parentId : Option IssueId) (target : Option RepoTarget)
                     (dependencies : Array IssueId := #[])
  | updateIssue      (issueId : IssueId) (title description : Option String)
                     (status : Option IssueStatus) (target : Option RepoTarget)
                     (dependencies : Option (Array IssueId) := none)
  -- work_issues
  | listOpenIssues   (projectId : ProjectId) (targetRepo : Option Repository)
  | claimIssue       (issueId : IssueId)
  | releaseClaim     (issueId : IssueId) (reason : String)
  | attachPr         (issueId : IssueId) (repo : Repository) (number : Nat) (branch : String)
  /-- Worker-driven decomposition: replace the held parent issue with a set of
      sub-issues, move the parent to `.blocked`, and release the claim. -/
  | splitIssue       (parentId : IssueId) (children : Array NewSubissueSpec) (reason : String)
  -- review_issues
  | listIssuesInReview (projectId : ProjectId)
  | decideIssue        (issueId : IssueId) (decision : ReviewDecision) (notes : String)
  -- always-available (when attached to a project)
  | projectInfo
deriving Repr, Inhabited

/-! ## JSON tool definitions

Returned to the agent in `tools/list`. Keep schemas small and explicit so
the agent has everything it needs in one round-trip. -/

private def strProp (desc : String) : Json :=
  Json.mkObj [("type", "string"), ("description", desc)]

private def intProp (desc : String) : Json :=
  Json.mkObj [("type", "integer"), ("description", desc)]

private def obj (props : List (String × Json)) (required : List String) : Json :=
  Json.mkObj
    [ ("type", "object")
    , ("properties", Json.mkObj props)
    , ("required", Json.arr (required.toArray.map Json.str)) ]

/-- All optional tools provided by this module, paired with the permission
    label that gates them. The server filters by label against
    `state.allowedTools`. -/
def toolDefs : List (String × String × Json) :=
  [ -- manage_issues
    (manageIssuesPerm, "list_projects",
      Json.mkObj
        [ ("name", "list_projects")
        , ("description", "List all orchestra projects.")
        , ("inputSchema", obj [] []) ])
  , (manageIssuesPerm, "list_issues",
      Json.mkObj
        [ ("name", "list_issues")
        , ("description", "List issues within a project. Optional status / parent_id filters.")
        , ("inputSchema", obj
            [ ("project_id", strProp "Project ID")
            , ("status", strProp "Optional status filter (open|claimed|in_review|completed|abandoned)")
            , ("parent_id", strProp "Optional parent issue ID; only direct children are returned") ]
            ["project_id"]) ])
  , (manageIssuesPerm, "get_issue",
      Json.mkObj
        [ ("name", "get_issue")
        , ("description", "Get full issue detail (children + attached PRs).")
        , ("inputSchema", obj
            [ ("issue_id", strProp "Issue ID") ]
            ["issue_id"]) ])
  , (manageIssuesPerm, "create_issue",
      Json.mkObj
        [ ("name", "create_issue")
        , ("description",
            "Create a new issue in a project. To create a sub-issue, set parent_id to " ++
            "the parent issue ID. Per-issue target_repo / target_branch override the " ++
            "project default. Use dependency_ids to list issues that must be completed " ++
            "before this one is dispatched.")
        , ("inputSchema", obj
            [ ("project_id", strProp "Project ID")
            , ("title", strProp "Issue title")
            , ("description", strProp "Issue description (markdown allowed)")
            , ("parent_id", strProp "Optional parent issue ID")
            , ("target_repo", strProp "Optional target repo (owner/name)")
            , ("target_branch", strProp "Optional target branch")
            , ("dependency_ids", Json.mkObj
                [ ("type", "array")
                , ("description", "Optional list of issue IDs that must be completed before this issue is dispatched")
                , ("items", Json.mkObj [("type", "string")]) ]) ]
            ["project_id", "title", "description"]) ])
  , (manageIssuesPerm, "update_issue",
      Json.mkObj
        [ ("name", "update_issue")
        , ("description",
            "Update an issue's title, description, status, target, or dependencies. " ++
            "Pass dependency_ids to replace the full dependency list; omit to leave it unchanged.")
        , ("inputSchema", obj
            [ ("issue_id", strProp "Issue ID")
            , ("title", strProp "New title")
            , ("description", strProp "New description")
            , ("status", strProp "New status (open|claimed|in_review|completed|abandoned)")
            , ("target_repo", strProp "New target repo (owner/name)")
            , ("target_branch", strProp "New target branch")
            , ("dependency_ids", Json.mkObj
                [ ("type", "array")
                , ("description", "Replace the dependency list with these issue IDs (issues that must be completed before this one is dispatched)")
                , ("items", Json.mkObj [("type", "string")]) ]) ]
            ["issue_id"]) ])
    -- work_issues
  , (workIssuesPerm, "list_open_issues",
      Json.mkObj
        [ ("name", "list_open_issues")
        , ("description", "List open (unclaimed) issues a worker can pick up.")
        , ("inputSchema", obj
            [ ("project_id", strProp "Project ID")
            , ("target_repo", strProp "Optional: only issues whose effective target repo matches (owner/name)") ]
            ["project_id"]) ])
  , (workIssuesPerm, "claim_issue",
      Json.mkObj
        [ ("name", "claim_issue")
        , ("description",
            "Claim an open issue for this task. Returns the effective target repo and " ++
            "branch on success. Fails with 'already_claimed' if another task holds it.")
        , ("inputSchema", obj
            [ ("issue_id", strProp "Issue ID") ]
            ["issue_id"]) ])
  , (workIssuesPerm, "release_claim",
      Json.mkObj
        [ ("name", "release_claim")
        , ("description", "Release a claim, returning the issue to the open pool.")
        , ("inputSchema", obj
            [ ("issue_id", strProp "Issue ID")
            , ("reason", strProp "Free-text reason recorded in the response") ]
            ["issue_id", "reason"]) ])
  , (workIssuesPerm, "split_issue",
      Json.mkObj
        [ ("name", "split_issue")
        , ("description",
            "Decompose the issue this task currently holds into one or more sub-issues. " ++
            "The parent moves to `blocked`, the claim is released, and the children become " ++
            "open and pickable. Caller MUST hold the claim on `parent_id`. Each child " ++
            "inherits the parent's effective target unless target_repo + target_branch are set.")
        , ("inputSchema", obj
            [ ("parent_id", strProp "ID of the issue this task currently holds")
            , ("reason",    strProp "Why the issue needed to be split (recorded in the response)")
            , ("children",  Json.mkObj
                [ ("type", "array")
                , ("description", "Sub-issues to create. Order is preserved.")
                , ("items", Json.mkObj
                    [ ("type", "object")
                    , ("properties", Json.mkObj
                        [ ("title", strProp "Sub-issue title")
                        , ("description", strProp "Sub-issue description (markdown allowed)")
                        , ("target_repo", strProp "Optional sub-issue target repo (owner/name)")
                        , ("target_branch", strProp "Optional sub-issue target branch") ])
                    , ("required", Json.arr #["title", "description"]) ]) ]) ]
            ["parent_id", "reason", "children"]) ])
  , (workIssuesPerm, "attach_pr",
      Json.mkObj
        [ ("name", "attach_pr")
        , ("description",
            "Attach a pull request to an issue and move it to in_review. " ++
            "Call after create_pr returns the PR number.")
        , ("inputSchema", obj
            [ ("issue_id", strProp "Issue ID")
            , ("repo", strProp "PR target repo (owner/name)")
            , ("number", intProp "PR number")
            , ("branch", strProp "PR head branch") ]
            ["issue_id", "repo", "number", "branch"]) ])
    -- review_issues
  , (reviewIssuesPerm, "list_issues_in_review",
      Json.mkObj
        [ ("name", "list_issues_in_review")
        , ("description", "List issues currently in_review (i.e. with attached PRs awaiting decision).")
        , ("inputSchema", obj
            [ ("project_id", strProp "Project ID") ]
            ["project_id"]) ])
  , (reviewIssuesPerm, "decide_issue",
      Json.mkObj
        [ ("name", "decide_issue")
        , ("description",
            "Approve or reject the issue under review. Approve enqueues a merger task " ++
            "for the latest attached PR; reject returns the issue to open.")
        , ("inputSchema", obj
            [ ("issue_id", strProp "Issue ID")
            , ("decision", strProp "approve | reject")
            , ("notes",    strProp "Reviewer notes") ]
            ["issue_id", "decision", "notes"]) ])
  ]

/-- Tool definition for `project_info`, exposed separately because it is
    always-available (no permission gate) when a task is attached to a project. -/
def projectInfoToolDef : Json :=
  Json.mkObj
    [ ("name", "project_info")
    , ("description",
        "Return information about the orchestra project and issue this task is working on: " ++
        "project id/name, issue id/title/status, claim status, and attached PRs.")
    , ("inputSchema", Json.mkObj [("type", "object"), ("properties", Json.mkObj [])]) ]

/-! ## Parsing -/

private def issueStatusOfString? : String → Option IssueStatus
  | "open"      => some .open
  | "claimed"   => some .claimed
  | "in_review" => some .inReview
  | "blocked"   => some .blocked
  | "completed" => some .completed
  | "abandoned" => some .abandoned
  | "rejected"  => some .rejected
  | _           => none

private def parseTarget? (args : Json) : Except String (Option RepoTarget) := do
  let mRepo  := args.getObjValAs? String "target_repo"   |>.toOption
  let mBranch := args.getObjValAs? String "target_branch" |>.toOption
  match mRepo, mBranch with
  | none,   none   => return none
  | some r, some b =>
    let repo ← Repository.parse r
    return some { repo, branch := b }
  | _,      _      =>
    Except.error "target_repo and target_branch must be provided together"

/-- Parse a `tools/call` for a project tool. Returns `none` if the tool name
    is not one of ours (the server should fall through to its other tools). -/
def tryParseToolCall (name : String) (args : Json) : Option (Except String ProjectTool) :=
  match name with
  | "list_projects" => some (.ok .listProjects)
  | "list_issues" =>
    some <| do
      let pid ← args.getObjValAs? String "project_id"
      let statusFilter := (args.getObjValAs? String "status" |>.toOption).bind issueStatusOfString?
      let parentId : Option IssueId :=
        args.getObjValAs? String "parent_id" |>.toOption |>.map (fun s => ⟨s⟩)
      return .listIssues ⟨pid⟩ statusFilter parentId
  | "get_issue" =>
    some <| do
      let iid ← args.getObjValAs? String "issue_id"
      return .getIssue ⟨iid⟩
  | "create_issue" =>
    some <| do
      let pid     ← args.getObjValAs? String "project_id"
      let title   ← args.getObjValAs? String "title"
      let descr   ← args.getObjValAs? String "description"
      let parent : Option IssueId :=
        args.getObjValAs? String "parent_id" |>.toOption |>.map (fun s => ⟨s⟩)
      let target  ← parseTarget? args
      let dependencies : Array IssueId :=
        (args.getObjValAs? (Array String) "dependency_ids" |>.toOption).getD #[]
        |>.map (fun s => ⟨s⟩)
      return .createIssue ⟨pid⟩ title descr parent target dependencies
  | "update_issue" =>
    some <| do
      let iid ← args.getObjValAs? String "issue_id"
      let title  := args.getObjValAs? String "title"       |>.toOption
      let descr  := args.getObjValAs? String "description" |>.toOption
      let status :=
        match args.getObjValAs? String "status" |>.toOption with
        | none => none
        | some s => issueStatusOfString? s
      let target ← parseTarget? args
      let dependencies : Option (Array IssueId) :=
        args.getObjValAs? (Array String) "dependency_ids" |>.toOption |>.map (·.map (⟨·⟩))
      return .updateIssue ⟨iid⟩ title descr status target dependencies
  | "list_open_issues" =>
    some <| do
      let pid ← args.getObjValAs? String "project_id"
      let mRepo := args.getObjValAs? String "target_repo" |>.toOption
      let target? : Option Repository ← match mRepo with
        | none => Except.ok none
        | some s => (Repository.parse s).map some
      return .listOpenIssues ⟨pid⟩ target?
  | "claim_issue" =>
    some <| do
      let iid ← args.getObjValAs? String "issue_id"
      return .claimIssue ⟨iid⟩
  | "release_claim" =>
    some <| do
      let iid    ← args.getObjValAs? String "issue_id"
      let reason ← args.getObjValAs? String "reason"
      return .releaseClaim ⟨iid⟩ reason
  | "attach_pr" =>
    some <| do
      let iid    ← args.getObjValAs? String "issue_id"
      let repoS  ← args.getObjValAs? String "repo"
      let number ← args.getObjValAs? Nat "number"
      let branch ← args.getObjValAs? String "branch"
      let repo   ← Repository.parse repoS
      return .attachPr ⟨iid⟩ repo number branch
  | "split_issue" =>
    some <| do
      let parentS ← args.getObjValAs? String "parent_id"
      let reason  ← args.getObjValAs? String "reason"
      let arr     ← args.getObjValAs? (Array Json) "children"
      if arr.isEmpty then
        Except.error "split_issue requires at least one child"
      else
        let children ← arr.mapM fun item => do
          let title ← item.getObjValAs? String "title"
          let descr ← item.getObjValAs? String "description"
          let target ← parseTarget? item
          (Except.ok { title, description := descr, target } : Except String NewSubissueSpec)
        return .splitIssue ⟨parentS⟩ children reason
  | "list_issues_in_review" =>
    some <| do
      let pid ← args.getObjValAs? String "project_id"
      return .listIssuesInReview ⟨pid⟩
  | "decide_issue" =>
    some <| do
      let iid      ← args.getObjValAs? String "issue_id"
      let decStr   ← args.getObjValAs? String "decision"
      let notes    ← args.getObjValAs? String "notes"
      let decision ← match decStr with
        | "approve" => Except.ok ReviewDecision.approve
        | "reject"  => Except.ok ReviewDecision.reject
        | s         => Except.error s!"decision must be 'approve' or 'reject', got {repr s}"
      return .decideIssue ⟨iid⟩ decision notes
  | "project_info" => some (.ok .projectInfo)
  | _ => none

/-! ## Evaluation environment

The server passes us its current state via `Env`. All side-effects
(claim acquisition, queue insertion for the merger task) flow through
function pointers so the evaluator does not need a transitive dependency
on `Orchestra.Queue` (which would create a cycle). -/

structure Env where
  /-- Current task's claim-manager handle. `none` outside a task context. -/
  claimManager : Option ClaimManager := none
  /-- Permission labels granted to this task (subset of {manage,work,review}_issues). -/
  allowedTools : List String
  /-- Current task ID, used as the claim holder. -/
  taskId : Option String := none
  /-- Backend label of the current agent (e.g. "claude"), recorded with the claim. -/
  agentBackend : String := "unknown"
  /-- Optional series the task belongs to. Stored alongside the claim. -/
  series : Option String := none
  /-- Whether `decideIssue .approve` is permitted to enqueue a merger task. -/
  canEnqueueMerger : Bool := false
  /-- Whether `attachPr` is permitted to enqueue an auto-reviewer task. -/
  canEnqueueReviewer : Bool := false
  /-- Orchestra project this task belongs to. Used by `project_info`. -/
  projectId : Option ProjectId := none
  /-- Orchestra issue this task is working on. Used by `project_info`. -/
  issueId : Option IssueId := none

private def deny (perm : String) : String :=
  s!"this task is not authorized for the {perm} tool group"

private def has (env : Env) (perm : String) : Bool :=
  env.allowedTools.contains perm

private def renderTarget : RepoTarget → String
  | { repo, branch } => s!"{repo}@{branch}"

private def issueStatusToString : IssueStatus → String
  | .open      => "open"
  | .claimed   => "claimed"
  | .inReview  => "in_review"
  | .blocked   => "blocked"
  | .completed => "completed"
  | .abandoned => "abandoned"
  | .rejected  => "rejected"

/-- Render an issue summary line (used in list responses). -/
private def issueLine (i : Issue) : String :=
  let parent := i.parentId.map (·.value) |>.getD "-"
  s!"{i.id.value}  [{issueStatusToString i.status}]  parent={parent}  {i.title}"

/-- Render a project summary line. -/
private def projectLine (p : Project) : String :=
  let target := p.defaultTarget.map renderTarget |>.getD "-"
  s!"{p.id.value}  {p.name}  default={target}  ({p.createdAt})"

private def joinLines (xs : Array String) : String :=
  String.join (xs.toList.intersperse "\n")

/-- Lightweight tool-content shape (mirrors `Server.toolContent`). -/
private def content (text : String) (isError : Bool := false) : Json :=
  Json.mkObj
    [ ("content", .arr #[Json.mkObj [("type", "text"), ("text", .str text)]])
    , ("isError", isError) ]

/-! ## Evaluator -/

def evalProjectTool [Monad m] [MonadProject m] [MonadClaim m] [MonadLog m]
    [MonadTaskStore m] [MonadQueue m] [MonadExcept IO.Error m]
    (env : Env) (call : ProjectTool) : m Json := do
  let now ← currentIso8601
  match call with
  -- ---------------- manage_issues ----------------
  | .listProjects =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    let projects ← MonadProject.loadAllProjects
    if projects.isEmpty then return content "No projects."
    return content (joinLines (projects.map projectLine))
  | .listIssues pid statusFilter parentId =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    let some _ ← MonadProject.loadProject pid
      | return content s!"project {pid.value} not found" (isError := true)
    let mut issues ← MonadProject.loadIssues pid
    if let some s := statusFilter then issues := issues.filter (·.status == s)
    if let some p := parentId then issues := issues.filter (·.parentId == some p)
    if issues.isEmpty then return content "No matching issues."
    return content (joinLines (issues.map issueLine))
  | .getIssue iid =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    match ← MonadProject.findIssue iid with
    | none => return content s!"issue {iid.value} not found" (isError := true)
    | some (project, i) =>
      let target := (effectiveTarget project i).map renderTarget |>.getD "-"
      let prs := i.attachedPRs.map (fun p => s!"  - {p.repo}#{p.number} (branch {p.branch})")
      let children ← MonadProject.childrenOf project.id i.id
      let childLines := children.map (fun c => s!"  - {c.id.value}  [{issueStatusToString c.status}]  {c.title}")
      let depsStr := if i.dependencies.isEmpty then "-"
                     else String.intercalate ", " (i.dependencies.map (·.value)).toList
      let header : Array String := #[
        s!"id:           {i.id.value}",
        s!"project:      {project.id.value} ({project.name})",
        s!"parent:       {i.parentId.map (·.value) |>.getD "-"}",
        s!"title:        {i.title}",
        s!"status:       {issueStatusToString i.status}",
        s!"target:       {target}",
        s!"dependencies: {depsStr}",
        s!"created:      {i.createdAt}",
        s!"updated:      {i.updatedAt}",
        if i.attachedPRs.isEmpty then "attached_prs: -" else "attached_prs:" ]
      let descrLines := (i.description.splitOn "\n").toArray.map (fun l => s!"  {l}")
      let mut body := header ++ prs
      if !children.isEmpty then body := body ++ #["children:"] ++ childLines
      body := body ++ #["description:"] ++ descrLines
      return content (joinLines body)
  | .createIssue pid title descr parent target dependencies =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    let some project ← MonadProject.loadProject pid
      | return content s!"project {pid.value} not found" (isError := true)
    if target.isNone && project.defaultTarget.isNone then
      return content
        "project has no default target; pass target_repo and target_branch" (isError := true)
    if let some parentId := parent then
      match ← MonadProject.findIssue parentId with
      | none => return content s!"parent issue {parentId.value} not found" (isError := true)
      | some (_, parentIssue) =>
        MonadProject.saveIssue { parentIssue with status := .blocked, updatedAt := now }
    let iid ← MonadProject.freshIssueId
    let issue : Issue :=
      { id := iid, projectId := pid, parentId := parent, title, description := descr
      , target, dependencies, createdAt := now, updatedAt := now }
    MonadProject.saveIssue issue
    return content s!"created issue {iid.value} in project {pid.value}"
  | .updateIssue iid title descr status target dependencies =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    match ← MonadProject.findIssue iid with
    | none => return content s!"issue {iid.value} not found" (isError := true)
    | some (_, i) =>
      let updated : Issue :=
        { i with
          title        := title.getD i.title
          description  := descr.getD i.description
          status       := status.getD i.status
          target       := match target with | some t => some t | none => i.target
          dependencies := dependencies.getD i.dependencies
          updatedAt    := now }
      MonadProject.saveIssue updated
      return content s!"updated issue {iid.value}"
  -- ---------------- work_issues ----------------
  | .listOpenIssues pid targetRepo? =>
    if !has env workIssuesPerm then return content (deny workIssuesPerm) (isError := true)
    let some project ← MonadProject.loadProject pid
      | return content s!"project {pid.value} not found" (isError := true)
    let issues ← MonadProject.loadIssues pid
    let openIssues := issues.filter (·.status == .open)
    let filtered := match targetRepo? with
      | none => openIssues
      | some r =>
        openIssues.filter fun i =>
          match effectiveTarget project i with
          | some t => t.repo == r
          | none   => false
    if filtered.isEmpty then return content "No open issues."
    return content (joinLines (filtered.map issueLine))
  | .claimIssue iid =>
    if !has env workIssuesPerm then return content (deny workIssuesPerm) (isError := true)
    let some mgr := env.claimManager
      | return content "claim manager not available in this context" (isError := true)
    let some taskId := env.taskId
      | return content "no task id in context (cannot record claim)" (isError := true)
    match ← MonadProject.findIssue iid with
    | none => return content s!"issue {iid.value} not found" (isError := true)
    | some (project, i) =>
      logInfo s!"  [mcp] claim_issue: {iid.value} \"{i.title}\""
      match ← MonadClaim.tryClaim mgr project.id iid taskId env.agentBackend now env.series with
      | .acquired _ =>
        let target := (effectiveTarget project i).map renderTarget |>.getD ""
        logInfo s!"  [mcp] claim_issue: acquired (target={target})"
        let payload := Json.mkObj
          [ ("ok", true)
          , ("issue_id",     Json.str iid.value)
          , ("project_id",   Json.str project.id.value)
          , ("target",       Json.str target) ]
        return content payload.compress
      | .alreadyClaimed existing =>
        logInfo s!"  [mcp] claim_issue: already claimed by task {existing.taskId}"
        let payload := Json.mkObj
          [ ("ok", false)
          , ("error", Json.str "already_claimed")
          , ("held_by_task", Json.str existing.taskId) ]
        return content payload.compress (isError := true)
      | .invalid reason =>
        logInfo s!"  [mcp] claim_issue: invalid — {reason}"
        return content s!"invalid claim: {reason}" (isError := true)
  | .releaseClaim iid reason =>
    if !has env workIssuesPerm then return content (deny workIssuesPerm) (isError := true)
    let some mgr := env.claimManager
      | return content "claim manager not available in this context" (isError := true)
    match ← MonadProject.findIssue iid with
    | none => return content s!"issue {iid.value} not found" (isError := true)
    | some (project, i) =>
      logInfo s!"  [mcp] release_claim: {iid.value} \"{i.title}\" — {reason}"
      let newStatus := match i.status with
        | .blocked  => .blocked
        | .inReview => .inReview
        | _         => .open
      let _ ← MonadClaim.release mgr project.id iid newStatus now
      return content s!"released claim on {iid.value} ({reason})"
  | .attachPr iid repo number branch =>
    if !has env workIssuesPerm then return content (deny workIssuesPerm) (isError := true)
    let some taskId := env.taskId
      | return content "no task id in context (cannot verify claim ownership)" (isError := true)
    match ← MonadProject.findIssue iid with
    | none => return content s!"issue {iid.value} not found" (isError := true)
    | some (project, i) =>
      match ← MonadClaim.loadClaim project.id iid with
      | none =>
        return content s!"cannot attach PR to {iid.value}: issue is not claimed" (isError := true)
      | some claim =>
        if claim.taskId != taskId then
          return content
            s!"cannot attach PR to {iid.value}: held by task {claim.taskId}, not this task"
            (isError := true)
        else
          logInfo s!"  [mcp] attach_pr: {iid.value} \"{i.title}\" ← {repo}#{number} (branch {branch})"
          let pr : PRRef := { repo, number, branch, taskId := env.taskId }
          let updated : Issue :=
            { i with
              attachedPRs := i.attachedPRs.push pr
              status      := .inReview
              updatedAt   := now }
          MonadProject.saveIssue updated
          -- F1: if the project configures an auto-reviewer, enqueue it now.
          let reviewerNote ← match project.reviewer with
            | some tmpl =>
              if env.canEnqueueReviewer then
                try
                  let rid ← enqueueReviewerImpl project iid pr tmpl
                  logInfo s!"  [mcp] attach_pr: reviewer task {rid} enqueued"
                  pure s!"; reviewer task {rid} enqueued"
                catch e =>
                  logInfo s!"  [mcp] attach_pr: reviewer enqueue failed — {toString e}"
                  pure s!"; reviewer enqueue failed: {toString e}"
              else pure ""
            | none => pure ""
          return content
            s!"attached {repo}#{number} to {iid.value}; issue moved to in_review{reviewerNote}"
  | .splitIssue parentId children reason =>
    if !has env workIssuesPerm then return content (deny workIssuesPerm) (isError := true)
    let some mgr := env.claimManager
      | return content "claim manager not available in this context" (isError := true)
    let some taskId := env.taskId
      | return content "no task id in context (cannot verify claim ownership)" (isError := true)
    match ← MonadProject.findIssue parentId with
    | none => return content s!"issue {parentId.value} not found" (isError := true)
    | some (project, parent) =>
      logInfo s!"  [mcp] split_issue: {parentId.value} \"{parent.title}\" → {children.size} sub-issues — {reason}"
      -- Caller must already hold the claim on this parent. We don't allow
      -- workers to split issues they don't own — that would let a stray
      -- agent rearrange someone else's work.
      match ← MonadClaim.loadClaim project.id parentId with
      | none =>
        return content s!"cannot split {parentId.value}: not currently claimed" (isError := true)
      | some claim =>
        if claim.taskId != taskId then
          return content
            s!"cannot split {parentId.value}: held by task {claim.taskId}, not this task"
            (isError := true)
        else
          let inheritedTarget := effectiveTarget project parent
          let mut createdIds : Array String := #[]
          for spec in children do
            let iid ← MonadProject.freshIssueId
            let issue : Issue :=
              { id := iid, projectId := project.id
              , parentId := some parentId
              , title := spec.title, description := spec.description
              , target := spec.target <|> inheritedTarget
              , createdAt := now, updatedAt := now }
            MonadProject.saveIssue issue
            logInfo s!"  [mcp] split_issue: created sub-issue {iid.value} \"{spec.title}\""
            createdIds := createdIds.push iid.value
          -- Move parent to .blocked and clear the claim. We don't reuse
          -- `release` here because it sets status unconditionally; we want
          -- `.blocked` specifically.
          MonadProject.saveIssue { parent with status := .blocked, updatedAt := now }
          let _ ← MonadClaim.forceRelease mgr project.id parentId
          let payload := Json.mkObj
            [ ("ok",         true)
            , ("parent_id",  Json.str parentId.value)
            , ("reason",     Json.str reason)
            , ("created",    Json.arr (createdIds.map Json.str)) ]
          return content payload.compress
  -- ---------------- review_issues ----------------
  | .listIssuesInReview pid =>
    if !has env reviewIssuesPerm then return content (deny reviewIssuesPerm) (isError := true)
    let some _ ← MonadProject.loadProject pid
      | return content s!"project {pid.value} not found" (isError := true)
    let issues := (← MonadProject.loadIssues pid).filter (·.status == .inReview)
    if issues.isEmpty then return content "No issues awaiting review."
    return content (joinLines (issues.map issueLine))
  | .decideIssue iid decision notes =>
    if !has env reviewIssuesPerm then return content (deny reviewIssuesPerm) (isError := true)
    match ← MonadProject.findIssue iid with
    | none => return content s!"issue {iid.value} not found" (isError := true)
    | some (project, i) =>
      let decisionStr := match decision with | .approve => "approve" | .reject => "reject"
      logInfo s!"  [mcp] decide_issue: {iid.value} \"{i.title}\" → {decisionStr} — {notes}"
      match decision with
      | .reject =>
        -- Move back to .open and clear any claim. Notes are echoed back; the
        -- comment tool is the right place to post them on the PR if desired.
        if let some mgr := env.claimManager then
          let _ ← MonadClaim.release mgr project.id iid .open now
        else
          MonadProject.saveIssue { i with status := .open, updatedAt := now }
        logInfo s!"  [mcp] decide_issue: {iid.value} moved to open"
        return content s!"rejected {iid.value}: {notes}"
      | .approve =>
        match i.attachedPRs.toList.reverse with
        | [] => return content "no attached PRs to merge" (isError := true)
        | pr :: _ =>
          if !env.canEnqueueMerger then
            return content "merger enqueue not available in this context" (isError := true)
          try
            let mergerTaskId ← enqueueMergerImpl project.id iid pr
            logInfo s!"  [mcp] decide_issue: {iid.value} approved; merger task {mergerTaskId} enqueued"
            return content s!"approved {iid.value}; merger task {mergerTaskId} enqueued ({notes})"
          catch e =>
            return content s!"failed to enqueue merger: {toString e}" (isError := true)
  | .projectInfo =>
    logInfo "  [mcp] project_info"
    match env.projectId with
    | none => return content "this task is not attached to a project" (isError := true)
    | some pid =>
      match ← MonadProject.loadProject pid with
      | none => return content s!"project {pid.value} not found" (isError := true)
      | some project =>
        let mut lines : Array String := #[
          s!"project_id:   {project.id.value}",
          s!"project_name: {project.name}"
        ]
        match env.issueId with
        | none => lines := lines.push "issue:        none"
        | some iid =>
          match ← MonadProject.findIssue iid with
          | none => lines := lines.push s!"issue_id:     {iid.value} (not found)"
          | some (_, issue) =>
            lines := lines.push s!"issue_id:     {issue.id.value}"
            lines := lines.push s!"issue_title:  {issue.title}"
            lines := lines.push s!"issue_status: {issueStatusToString issue.status}"
            if let some claim ← MonadClaim.loadClaim project.id iid then
              if some claim.taskId == env.taskId then
                lines := lines.push s!"claim:        held by this task (since {claim.claimedAt})"
              else
                lines := lines.push s!"claim:        held by task {claim.taskId}"
            else
              lines := lines.push "claim:        none"
            if issue.attachedPRs.isEmpty then
              lines := lines.push "attached_prs: none"
            else
              lines := lines.push "attached_prs:"
              for pr in issue.attachedPRs.toList.reverse do
                lines := lines.push s!"  {pr.repo}#{pr.number}  branch={pr.branch}"
        return content (String.intercalate "\n" lines.toList)

end Orchestra.Project.Tools
