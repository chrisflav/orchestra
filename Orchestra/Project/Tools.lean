import Lean.Data.Json
import Orchestra.Project.Basic
import Orchestra.Project.Claim
import Orchestra.TaskStore

open Lean (Json FromJson ToJson)

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

inductive ProjectTool where
  -- manage_issues
  | listProjects
  | listIssues       (projectId : ProjectId) (statusFilter : Option IssueStatus)
                     (parentId : Option IssueId)
  | getIssue         (issueId : IssueId)
  | createIssue      (projectId : ProjectId) (title description : String)
                     (parentId : Option IssueId) (target : Option RepoTarget)
  | updateIssue      (issueId : IssueId) (title description : Option String)
                     (status : Option IssueStatus) (target : Option RepoTarget)
  -- work_issues
  | listOpenIssues   (projectId : ProjectId) (targetRepo : Option Repository)
  | claimIssue       (issueId : IssueId)
  | releaseClaim     (issueId : IssueId) (reason : String)
  | attachPr         (issueId : IssueId) (repo : Repository) (number : Nat) (branch : String)
  -- review_issues
  | listIssuesInReview (projectId : ProjectId)
  | decideIssue        (issueId : IssueId) (decision : ReviewDecision) (notes : String)
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
            "project default.")
        , ("inputSchema", obj
            [ ("project_id", strProp "Project ID")
            , ("title", strProp "Issue title")
            , ("description", strProp "Issue description (markdown allowed)")
            , ("parent_id", strProp "Optional parent issue ID")
            , ("target_repo", strProp "Optional target repo (owner/name)")
            , ("target_branch", strProp "Optional target branch") ]
            ["project_id", "title", "description"]) ])
  , (manageIssuesPerm, "update_issue",
      Json.mkObj
        [ ("name", "update_issue")
        , ("description", "Update an issue's title, description, status, or target.")
        , ("inputSchema", obj
            [ ("issue_id", strProp "Issue ID")
            , ("title", strProp "New title")
            , ("description", strProp "New description")
            , ("status", strProp "New status (open|claimed|in_review|completed|abandoned)")
            , ("target_repo", strProp "New target repo (owner/name)")
            , ("target_branch", strProp "New target branch") ]
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

/-! ## Parsing -/

private def issueStatusOfString? : String → Option IssueStatus
  | "open"      => some .open
  | "claimed"   => some .claimed
  | "in_review" => some .inReview
  | "completed" => some .completed
  | "abandoned" => some .abandoned
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
      return .createIssue ⟨pid⟩ title descr parent target
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
      return .updateIssue ⟨iid⟩ title descr status target
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
  /-- Hook called by `decideIssue .approve` to enqueue the merger task.
      Receives (projectId, issueId, prRef). Returning `.ok ()` is success;
      `.error msg` is surfaced to the agent. -/
  enqueueMerger : Option (ProjectId → IssueId → PRRef → IO (Except String String)) := none
  /-- Optional auto-reviewer hook (F1). Called by `attachPr` when the
      project has a `reviewer` template configured. Receives
      `(project, issueId, prRef, template)`. -/
  enqueueReviewer : Option
    (Project → IssueId → PRRef → ReviewerTemplate → IO (Except String String)) := none

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
  | .completed => "completed"
  | .abandoned => "abandoned"

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

def evalProjectTool (env : Env) (call : ProjectTool) : IO Json := do
  let now ← TaskStore.currentIso8601
  match call with
  -- ---------------- manage_issues ----------------
  | .listProjects =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    let projects ← loadAllProjects
    if projects.isEmpty then return content "No projects."
    return content (joinLines (projects.map projectLine))
  | .listIssues pid statusFilter parentId =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    let some _ ← loadProject pid
      | return content s!"project {pid.value} not found" (isError := true)
    let mut issues ← loadIssues pid
    if let some s := statusFilter then issues := issues.filter (·.status == s)
    if let some p := parentId then issues := issues.filter (·.parentId == some p)
    if issues.isEmpty then return content "No matching issues."
    return content (joinLines (issues.map issueLine))
  | .getIssue iid =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    match ← findIssue iid with
    | none => return content s!"issue {iid.value} not found" (isError := true)
    | some (project, i) =>
      let target := (effectiveTarget project i).map renderTarget |>.getD "-"
      let prs := i.attachedPRs.map (fun p => s!"  - {p.repo}#{p.number} (branch {p.branch})")
      let children ← childrenOf project.id i.id
      let childLines := children.map (fun c => s!"  - {c.id.value}  [{issueStatusToString c.status}]  {c.title}")
      let header : Array String := #[
        s!"id:        {i.id.value}",
        s!"project:   {project.id.value} ({project.name})",
        s!"parent:    {i.parentId.map (·.value) |>.getD "-"}",
        s!"title:     {i.title}",
        s!"status:    {issueStatusToString i.status}",
        s!"target:    {target}",
        s!"created:   {i.createdAt}",
        s!"updated:   {i.updatedAt}",
        if i.attachedPRs.isEmpty then "attached_prs: -" else "attached_prs:" ]
      let descrLines := (i.description.splitOn "\n").toArray.map (fun l => s!"  {l}")
      let mut body := header ++ prs
      if !children.isEmpty then body := body ++ #["children:"] ++ childLines
      body := body ++ #["description:"] ++ descrLines
      return content (joinLines body)
  | .createIssue pid title descr parent target =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    let some project ← loadProject pid
      | return content s!"project {pid.value} not found" (isError := true)
    if target.isNone && project.defaultTarget.isNone then
      return content
        "project has no default target; pass target_repo and target_branch" (isError := true)
    let iid ← freshIssueId
    let issue : Issue :=
      { id := iid, projectId := pid, parentId := parent, title, description := descr
      , target, createdAt := now, updatedAt := now }
    saveIssue issue
    return content s!"created issue {iid.value} in project {pid.value}"
  | .updateIssue iid title descr status target =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    match ← findIssue iid with
    | none => return content s!"issue {iid.value} not found" (isError := true)
    | some (_, i) =>
      let updated : Issue :=
        { i with
          title       := title.getD i.title
          description := descr.getD i.description
          status      := status.getD i.status
          target      := match target with | some t => some t | none => i.target
          updatedAt   := now }
      saveIssue updated
      return content s!"updated issue {iid.value}"
  -- ---------------- work_issues ----------------
  | .listOpenIssues pid targetRepo? =>
    if !has env workIssuesPerm then return content (deny workIssuesPerm) (isError := true)
    let some project ← loadProject pid
      | return content s!"project {pid.value} not found" (isError := true)
    let issues ← loadIssues pid
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
    match ← findIssue iid with
    | none => return content s!"issue {iid.value} not found" (isError := true)
    | some (project, i) =>
      match ← tryClaim mgr project.id iid taskId env.agentBackend now env.series with
      | .acquired _ =>
        let target := (effectiveTarget project i).map renderTarget |>.getD ""
        let payload := Json.mkObj
          [ ("ok", true)
          , ("issue_id",     Json.str iid.value)
          , ("project_id",   Json.str project.id.value)
          , ("target",       Json.str target) ]
        return content payload.compress
      | .alreadyClaimed existing =>
        let payload := Json.mkObj
          [ ("ok", false)
          , ("error", Json.str "already_claimed")
          , ("held_by_task", Json.str existing.taskId) ]
        return content payload.compress (isError := true)
      | .invalid reason =>
        return content s!"invalid claim: {reason}" (isError := true)
  | .releaseClaim iid reason =>
    if !has env workIssuesPerm then return content (deny workIssuesPerm) (isError := true)
    let some mgr := env.claimManager
      | return content "claim manager not available in this context" (isError := true)
    match ← findIssue iid with
    | none => return content s!"issue {iid.value} not found" (isError := true)
    | some (project, _) =>
      let _ ← release mgr project.id iid .open now
      return content s!"released claim on {iid.value} ({reason})"
  | .attachPr iid repo number branch =>
    if !has env workIssuesPerm then return content (deny workIssuesPerm) (isError := true)
    match ← findIssue iid with
    | none => return content s!"issue {iid.value} not found" (isError := true)
    | some (project, i) =>
      let pr : PRRef := { repo, number, branch, taskId := env.taskId }
      let updated : Issue :=
        { i with
          attachedPRs := i.attachedPRs.push pr
          status      := .inReview
          updatedAt   := now }
      saveIssue updated
      -- F1: if the project configures an auto-reviewer, enqueue it now.
      let reviewerNote ← match project.reviewer, env.enqueueReviewer with
        | some tmpl, some hook =>
          match ← hook project iid pr tmpl with
          | .ok rid    => pure s!"; reviewer task {rid} enqueued"
          | .error msg => pure s!"; reviewer enqueue failed: {msg}"
        | _, _ => pure ""
      return content
        s!"attached {repo}#{number} to {iid.value}; issue moved to in_review{reviewerNote}"
  -- ---------------- review_issues ----------------
  | .listIssuesInReview pid =>
    if !has env reviewIssuesPerm then return content (deny reviewIssuesPerm) (isError := true)
    let some _ ← loadProject pid
      | return content s!"project {pid.value} not found" (isError := true)
    let issues := (← loadIssues pid).filter (·.status == .inReview)
    if issues.isEmpty then return content "No issues awaiting review."
    return content (joinLines (issues.map issueLine))
  | .decideIssue iid decision notes =>
    if !has env reviewIssuesPerm then return content (deny reviewIssuesPerm) (isError := true)
    match ← findIssue iid with
    | none => return content s!"issue {iid.value} not found" (isError := true)
    | some (project, i) =>
      match decision with
      | .reject =>
        -- Move back to .open and clear any claim. Notes are echoed back; the
        -- comment tool is the right place to post them on the PR if desired.
        if let some mgr := env.claimManager then
          let _ ← release mgr project.id iid .open now
        else
          saveIssue { i with status := .open, updatedAt := now }
        return content s!"rejected {iid.value}: {notes}"
      | .approve =>
        match i.attachedPRs.toList.reverse with
        | [] => return content "no attached PRs to merge" (isError := true)
        | pr :: _ =>
          match env.enqueueMerger with
          | none =>
            return content "merger enqueue hook not configured" (isError := true)
          | some hook =>
            match ← hook project.id iid pr with
            | .error msg => return content s!"failed to enqueue merger: {msg}" (isError := true)
            | .ok mergerTaskId =>
              return content s!"approved {iid.value}; merger task {mergerTaskId} enqueued ({notes})"

end Orchestra.Project.Tools
