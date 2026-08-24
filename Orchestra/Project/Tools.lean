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
  /-- The pull request should land. Enqueues the merger; the issue stays open, because merging a
      PR is not the same as finishing the issue — see `complete`. -/
  | approve
  /-- The issue's work is done. Marks it completed without merging anything. -/
  | complete
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
  | listIssues       (projectId : Taxis.IssueId) (statusFilter : Option IssueStatus)
                     (parentId : Option Taxis.IssueId)
  | getIssue         (issueId : Taxis.IssueId)
  | createIssue      (projectId : Taxis.IssueId) (title description : String)
                     (parentId : Option Taxis.IssueId) (target : Option RepoTarget)
                     (dependencies : Array Taxis.IssueId := #[])
  | updateIssue      (issueId : Taxis.IssueId) (title description : Option String)
                     (status : Option IssueStatus) (target : Option RepoTarget)
                     (dependencies : Option (Array Taxis.IssueId) := none)
                     (labelsAdd labelsRemove : List String := [])
                     (assigneesAdd assigneesRemove : List String := [])
  /-- The labels the tracker defines: the vocabulary a relabelling request is resolved against,
      and the only one it may name. -/
  | listLabels
  /-- The actors the tracker knows, so an issue can be handed to a named human. -/
  | listActors
  -- work_issues
  | listOpenIssues   (projectId : Taxis.IssueId) (targetRepo : Option Repository)
  | claimIssue       (issueId : Taxis.IssueId)
  | releaseClaim     (issueId : Taxis.IssueId) (reason : String)
  | attachPr         (issueId : Taxis.IssueId) (repo : Repository) (number : Nat) (branch : String)
  /-- Worker-driven decomposition: replace the held parent issue with a set of
      sub-issues and release the claim on the parent. -/
  | splitIssue       (parentId : Taxis.IssueId) (children : Array NewSubissueSpec) (reason : String)
  /-- Read an issue's comment thread. Available to worker and reviewer roles alike: a reviewer
      writes the verdict there, and the next worker to pick the issue up reads why. -/
  | listIssueComments  (issueId : Taxis.IssueId)
  /-- Add a comment to an issue's thread. -/
  | commentIssue       (issueId : Taxis.IssueId) (body : String)
  /-- Read an issue's context notes: what earlier runs on it worked out. -/
  | listContext        (issueId : Taxis.IssueId)
  /-- Attach a new context note to an issue. -/
  | addContext         (issueId : Taxis.IssueId) (title text : String)
  /-- Rewrite one context note in place, keeping its id. -/
  | updateContext      (issueId : Taxis.IssueId) (contextId : Taxis.ArtifactId)
                       (title text : String)
  -- review_issues
  | listIssuesInReview (projectId : Taxis.IssueId)
  | decideIssue        (issueId : Taxis.IssueId) (decision : ReviewDecision) (notes : String)
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

private def strArrayProp (desc : String) : Json :=
  Json.mkObj
    [ ("type", "array")
    , ("description", desc)
    , ("items", Json.mkObj [("type", "string")]) ]

private def obj (props : List (String × Json)) (required : List String) : Json :=
  Json.mkObj
    [ ("type", "object")
    , ("properties", Json.mkObj props)
    , ("required", Json.arr (required.toArray.map Json.str)) ]

/-- All optional tools provided by this module, paired with the permission
    label that gates them. The server filters by label against
    `state.allowedTools`. -/
private def commentsToolDef : Json :=
  Json.mkObj
    [ ("name", "list_issue_comments")
    , ("description",
        "Read the comment thread on a taxis issue. This is where a reviewer records why an " ++
        "issue was approved or rejected — read it before reworking an issue that came back.")
    , ("inputSchema", obj [ ("issue_id", intProp "Issue ID") ] ["issue_id"]) ]

private def commentIssueToolDef : Json :=
  Json.mkObj
    [ ("name", "comment_issue")
    , ("description",
        "Add a comment to a taxis issue's thread. Reviewers: record your review here. " ++
        "This is the taxis issue tracker, not GitHub — use `comment` for the GitHub issue or " ++
        "pull request the task was launched from.")
    , ("inputSchema", obj
        [ ("issue_id", intProp "Issue ID")
        , ("body", strProp "Comment text (markdown allowed)") ]
        ["issue_id", "body"]) ]

/-! ### Context notes

Offered under all three groups, reads and writes alike. Every role that touches an issue
accumulates something the next one would otherwise rediscover — a worker what it tried, a
reviewer what it checked, a manager what it decided not to do — and the point of the artifact is
that recording it costs the description and the thread nothing. -/

private def listContextToolDef : Json :=
  Json.mkObj
    [ ("name", "list_context")
    , ("description",
        "Read the context notes attached to a taxis issue: what an earlier run worked out, an " ++
        "approach already tried and abandoned, what the build environment needs. Read these " ++
        "before starting work — this is where the last agent on the issue left its findings. " ++
        "get_issue shows only their titles and the taxis UI keeps them folded, so this is what " ++
        "reads the notes themselves.")
    , ("inputSchema", obj [ ("issue_id", intProp "Issue ID") ] ["issue_id"]) ]

private def addContextToolDef : Json :=
  Json.mkObj
    [ ("name", "add_context")
    , ("description",
        "Attach a context note to a taxis issue: a title and a block of markdown, held beside " ++
        "the issue rather than in it. This is where implementation detail, findings and results " ++
        "belong — not in the description, which states what the work is and is read by every " ++
        "person who opens the issue, and not in the comment thread, which is discussion and " ++
        "review. Notes stay folded, so they can accumulate over an issue's life without " ++
        "crowding either.")
    , ("inputSchema", obj
        [ ("issue_id", intProp "Issue ID")
        , ("title", strProp "Short label for the note — the one line shown before it is unfolded")
        , ("text", strProp "The note itself (markdown, with $…$ math)") ]
        ["issue_id", "title", "text"]) ]

private def updateContextToolDef : Json :=
  Json.mkObj
    [ ("name", "update_context")
    , ("description",
        "Rewrite a context note in place, keeping its id. Title and text replace what was " ++
        "there — a note is revised whole rather than appended to, so pass the full text you " ++
        "want it to end up with; list_context gives you the current one. Prefer revising a note " ++
        "that has gone out of date over attaching a second one that contradicts it.")
    , ("inputSchema", obj
        [ ("issue_id", intProp "ID of the issue the note is attached to")
        , ("context_id", intProp "ID of the note to rewrite (from list_context)")
        , ("title", strProp "Replacement title")
        , ("text", strProp "Replacement text (markdown)") ]
        ["issue_id", "context_id", "title", "text"]) ]

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
            [ ("project_id", intProp "Project ID")
            , ("status", strProp "Optional status filter (open|claimed|completed|abandoned)")
            , ("parent_id", intProp "Optional parent issue ID; only direct children are returned") ]
            ["project_id"]) ])
  , (manageIssuesPerm, "get_issue",
      Json.mkObj
        [ ("name", "get_issue")
        , ("description",
            "Get full issue detail: children, attached PRs, the comment thread, and the " ++
            "titles of any context notes (read them with list_context).")
        , ("inputSchema", obj
            [ ("issue_id", intProp "Issue ID") ]
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
            [ ("project_id", intProp "Project ID")
            , ("title", strProp "Issue title")
            , ("description", strProp "Issue description (markdown allowed)")
            , ("parent_id", intProp "Optional parent issue ID")
            , ("target_repo", strProp "Optional target repo (owner/name)")
            , ("target_branch", strProp "Optional target branch")
            , ("dependency_ids", Json.mkObj
                [ ("type", "array")
                , ("description", "Optional list of issue IDs that must be completed before this issue is dispatched")
                , ("items", Json.mkObj [("type", "integer")]) ]) ]
            ["project_id", "title", "description"]) ])
  , (manageIssuesPerm, "update_issue",
      Json.mkObj
        [ ("name", "update_issue")
        , ("description",
            "Update an issue's title, description, status, target, dependencies, labels or " ++
            "assignees. Pass dependency_ids to replace the full dependency list; omit to leave " ++
            "it unchanged. Labels and assignees are add/remove lists rather than a set to " ++
            "write, so a call that adds one label leaves the others alone.")
        , ("inputSchema", obj
            [ ("issue_id", intProp "Issue ID")
            , ("title", strProp "New title")
            , ("description", strProp "New description")
            , ("status", strProp "New status (open|claimed|completed|abandoned)")
            , ("target_repo", strProp "New target repo (owner/name)")
            , ("target_branch", strProp "New target branch")
            , ("dependency_ids", Json.mkObj
                [ ("type", "array")
                , ("description", "Replace the dependency list with these issue IDs (issues that must be completed before this one is dispatched)")
                , ("items", Json.mkObj [("type", "integer")]) ])
            , ("labels_add", strArrayProp
                ("Taxis labels to put on the issue. These are what the dispatcher routes on, " ++
                 "so this is how work is marked ready and steered. Only labels the tracker " ++
                 "already defines (list_labels); orchestra's own o-claimed / t-project are refused."))
            , ("labels_remove", strArrayProp "Taxis labels to take off the issue.")
            , ("assignees_add", strArrayProp
                ("Actors to assign, by email or display name (list_actors). Assigning a human " ++
                 "is how an issue is escalated to one."))
            , ("assignees_remove", strArrayProp "Actors to unassign, by email or display name.") ]
            ["issue_id"]) ])
  , (manageIssuesPerm, "list_labels",
      Json.mkObj
        [ ("name", "list_labels")
        , ("description",
            "List the labels the taxis tracker defines. These are the only names update_issue " ++
            "will accept — it creates none — so check here before routing an issue with one. " ++
            "Not GitHub labels: use label_issue for those.")
        , ("inputSchema", obj [] []) ])
  , (manageIssuesPerm, "list_actors",
      Json.mkObj
        [ ("name", "list_actors")
        , ("description",
            "List the actors (people and bots) the taxis tracker knows, with the email that " ++
            "names each one uniquely. Use before assigning an issue to a human.")
        , ("inputSchema", obj [] []) ])
    -- work_issues
  , (workIssuesPerm, "list_open_issues",
      Json.mkObj
        [ ("name", "list_open_issues")
        , ("description", "List open (unclaimed) issues a worker can pick up.")
        , ("inputSchema", obj
            [ ("project_id", intProp "Project ID")
            , ("target_repo", strProp "Optional: only issues whose effective target repo matches (owner/name)") ]
            ["project_id"]) ])
  , (workIssuesPerm, "claim_issue",
      Json.mkObj
        [ ("name", "claim_issue")
        , ("description",
            "Claim an open issue for this task. Returns the effective target repo and " ++
            "branch on success. Fails with 'already_claimed' if another task holds it.")
        , ("inputSchema", obj
            [ ("issue_id", intProp "Issue ID") ]
            ["issue_id"]) ])
  , (workIssuesPerm, "release_claim",
      Json.mkObj
        [ ("name", "release_claim")
        , ("description", "Release a claim, returning the issue to the open pool.")
        , ("inputSchema", obj
            [ ("issue_id", intProp "Issue ID")
            , ("reason", strProp "Free-text reason recorded in the response") ]
            ["issue_id", "reason"]) ])
  , (workIssuesPerm, "split_issue",
      Json.mkObj
        [ ("name", "split_issue")
        , ("description",
            "Decompose the issue this task currently holds into one or more sub-issues. " ++
            "The parent moves to `a container (it now has open children), the claim is released, and the children become " ++
            "open and pickable. Caller MUST hold the claim on `parent_id`. Each child " ++
            "inherits the parent's effective target unless target_repo + target_branch are set.")
        , ("inputSchema", obj
            [ ("parent_id", intProp "ID of the issue this task currently holds")
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
            "Attach a pull request to an issue. This is what puts it in front of a reviewer: " ++
            "one is queued for any open issue with an unmerged PR attached. " ++
            "Call after create_pr returns the PR number.")
        , ("inputSchema", obj
            [ ("issue_id", intProp "Issue ID")
            , ("repo", strProp "PR target repo (owner/name)")
            , ("number", intProp "PR number")
            , ("branch", strProp "PR head branch") ]
            ["issue_id", "repo", "number", "branch"]) ])
  , (workIssuesPerm, "list_issue_comments", commentsToolDef)
  , (reviewIssuesPerm, "list_issue_comments", commentsToolDef)
  , (manageIssuesPerm, "list_issue_comments", commentsToolDef)
  , (workIssuesPerm, "comment_issue", commentIssueToolDef)
  , (reviewIssuesPerm, "comment_issue", commentIssueToolDef)
  , (workIssuesPerm, "list_context", listContextToolDef)
  , (reviewIssuesPerm, "list_context", listContextToolDef)
  , (manageIssuesPerm, "list_context", listContextToolDef)
  , (workIssuesPerm, "add_context", addContextToolDef)
  , (reviewIssuesPerm, "add_context", addContextToolDef)
  , (manageIssuesPerm, "add_context", addContextToolDef)
  , (workIssuesPerm, "update_context", updateContextToolDef)
  , (reviewIssuesPerm, "update_context", updateContextToolDef)
  , (manageIssuesPerm, "update_context", updateContextToolDef)
    -- review_issues
  , (reviewIssuesPerm, "list_issues_in_review",
      Json.mkObj
        [ ("name", "list_issues_in_review")
        , ("description", "List issues awaiting review: open, with at least one attached pull request.")
        , ("inputSchema", obj
            [ ("project_id", intProp "Project ID") ]
            ["project_id"]) ])
  , (reviewIssuesPerm, "decide_issue",
      Json.mkObj
        [ ("name", "decide_issue")
        , ("description",
            "Decide on an issue under review. `approve` enqueues a merger for the latest " ++
            "attached PR — the PR lands but the issue stays open, since one PR is not " ++
            "necessarily the whole issue. `complete` marks the issue finished without merging " ++
            "anything. `reject` returns it to open. All three record your notes as a review " ++
            "comment on the issue.")
        , ("inputSchema", obj
            [ ("issue_id", intProp "Issue ID")
            , ("decision", strProp "approve | complete | reject")
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
  | "completed" => some .completed
  | "abandoned" => some .abandoned
  | _           => none

/-- Read an optional string argument, telling *absent* from *malformed*.

    A missing key and an explicit `null` — which is how a client serialises "not set" — both mean
    leave it alone. Anything else that is not a string is the caller's mistake and is reported as
    one: read as absent instead, a mistyped field is a call that changes nothing while answering
    as though it had nothing to change. -/
private def optStrArg (args : Json) (field : String) : Except String (Option String) :=
  match args.getObjVal? field with
  | .error _  => .ok none
  | .ok .null => .ok none
  | .ok _     =>
    match args.getObjValAs? String field with
    | .ok v    => .ok (some v)
    | .error e => .error s!"{field} must be a string: {e}"

private def parseTarget? (args : Json) : Except String (Option RepoTarget) := do
  -- Mistyped rather than lenient, so that "provided together" means what it says: read through
  -- `.toOption`, `target_repo: 1` was indistinguishable from an absent one, and naming both with
  -- one of them mistyped was reported as failing to provide both.
  let mRepo   ← optStrArg args "target_repo"
  let mBranch ← optStrArg args "target_branch"
  match mRepo, mBranch with
  | none,   none   => return none
  | some r, some b =>
    let repo ← Repository.parse r
    return some { repo, branch := b }
  | _,      _      =>
    Except.error "target_repo and target_branch must be provided together"

/-- Parse a `tools/call` for a project tool. Returns `none` if the tool name
    is not one of ours (the server should fall through to its other tools).
    `project_id`/`issue_id`/`parent_id`/`dependency_ids` are JSON integers (taxis's own id
    convention), decoded via `Taxis.IssueId`/`Taxis.IssueId`'s `FromJson` — not strings. -/
def tryParseToolCall (name : String) (args : Json) : Option (Except String ProjectTool) :=
  match name with
  | "list_projects" => some (.ok .listProjects)
  | "list_issues" =>
    some <| do
      let pid ← args.getObjValAs? Taxis.IssueId "project_id"
      let statusFilter := (args.getObjValAs? String "status" |>.toOption).bind issueStatusOfString?
      let parentId := args.getObjValAs? Taxis.IssueId "parent_id" |>.toOption
      return .listIssues pid statusFilter parentId
  | "get_issue" =>
    some <| do
      let iid ← args.getObjValAs? Taxis.IssueId "issue_id"
      return .getIssue iid
  | "create_issue" =>
    some <| do
      let pid     ← args.getObjValAs? Taxis.IssueId "project_id"
      let title   ← args.getObjValAs? String "title"
      let descr   ← args.getObjValAs? String "description"
      let parent := args.getObjValAs? Taxis.IssueId "parent_id" |>.toOption
      let target  ← parseTarget? args
      let dependencies : Array Taxis.IssueId :=
        (args.getObjValAs? (Array Taxis.IssueId) "dependency_ids" |>.toOption).getD #[]
      return .createIssue pid title descr parent target dependencies
  | "update_issue" =>
    some <| do
      let iid ← args.getObjValAs? Taxis.IssueId "issue_id"
      -- Every optional field here distinguishes *absent* from *malformed*. Absent means "leave
      -- this alone" and is the ordinary case — a retitle names one field and omits six. Malformed
      -- is a mistake, and reading it as absent is how `labels_add: "auto-work-opus"` (an array
      -- field given a bare string, the likeliest way to get this wrong) or `status: "done"` used
      -- to become a call that reported success and changed nothing, which is exactly the silent
      -- failure a closed label vocabulary exists to prevent.
      let present (field : String) : Bool :=
        match args.getObjVal? field with
        | .error _  => false   -- no such key
        | .ok .null => false   -- explicit null: how a client spells "not set"
        | .ok _     => true
      let title ← optStrArg args "title"
      let descr ← optStrArg args "description"
      let status ← do
        match ← optStrArg args "status" with
        | none   => pure none
        | some s =>
          match issueStatusOfString? s with
          | some st => pure (some st)
          | none    =>
            Except.error s!"status must be open, claimed, completed or abandoned, got {repr s}"
      let target ← parseTarget? args
      let dependencies ←
        if !present "dependency_ids" then pure none
        else match args.getObjValAs? (Array Taxis.IssueId) "dependency_ids" with
          | .ok v    => pure (some v)
          | .error e => Except.error s!"dependency_ids must be an array of issue ids: {e}"
      -- Names are trimmed, and a blank one is refused rather than passed down to be reported as
      -- `no such label: `. `label_issue` already draws the line here on the GitHub side.
      let names (field : String) : Except String (List String) :=
        if !present field then .ok []
        else match args.getObjValAs? (Array String) field with
          | .error e => .error s!"{field} must be an array of names: {e}"
          | .ok a    =>
            let trimmed := (a.map (·.trimAscii.toString)).toList
            if trimmed.any (·.isEmpty) then .error s!"{field} contains an empty name"
            else .ok trimmed
      let labelsAdd       ← names "labels_add"
      let labelsRemove    ← names "labels_remove"
      let assigneesAdd    ← names "assignees_add"
      let assigneesRemove ← names "assignees_remove"
      return .updateIssue iid title descr status target dependencies
        labelsAdd labelsRemove assigneesAdd assigneesRemove
  | "list_labels" => some (.ok .listLabels)
  | "list_actors" => some (.ok .listActors)
  | "list_open_issues" =>
    some <| do
      let pid ← args.getObjValAs? Taxis.IssueId "project_id"
      let mRepo := args.getObjValAs? String "target_repo" |>.toOption
      let target? : Option Repository ← match mRepo with
        | none => Except.ok none
        | some s => (Repository.parse s).map some
      return .listOpenIssues pid target?
  | "claim_issue" =>
    some <| do
      let iid ← args.getObjValAs? Taxis.IssueId "issue_id"
      return .claimIssue iid
  | "release_claim" =>
    some <| do
      let iid    ← args.getObjValAs? Taxis.IssueId "issue_id"
      let reason ← args.getObjValAs? String "reason"
      return .releaseClaim iid reason
  | "attach_pr" =>
    some <| do
      let iid    ← args.getObjValAs? Taxis.IssueId "issue_id"
      let repoS  ← args.getObjValAs? String "repo"
      let number ← args.getObjValAs? Nat "number"
      let branch ← args.getObjValAs? String "branch"
      let repo   ← Repository.parse repoS
      return .attachPr iid repo number branch
  | "split_issue" =>
    some <| do
      let parentId ← args.getObjValAs? Taxis.IssueId "parent_id"
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
        return .splitIssue parentId children reason
  | "list_issue_comments" =>
    some <| do
      let iid ← args.getObjValAs? Taxis.IssueId "issue_id"
      return .listIssueComments iid
  | "comment_issue" =>
    some <| do
      let iid  ← args.getObjValAs? Taxis.IssueId "issue_id"
      let body ← args.getObjValAs? String "body"
      return .commentIssue iid body
  | "list_context" =>
    some <| do
      let iid ← args.getObjValAs? Taxis.IssueId "issue_id"
      return .listContext iid
  | "add_context" =>
    some <| do
      let iid   ← args.getObjValAs? Taxis.IssueId "issue_id"
      let title ← args.getObjValAs? String "title"
      let text  ← args.getObjValAs? String "text"
      return .addContext iid title text
  | "update_context" =>
    some <| do
      let iid   ← args.getObjValAs? Taxis.IssueId "issue_id"
      let cid   ← args.getObjValAs? Taxis.ArtifactId "context_id"
      let title ← args.getObjValAs? String "title"
      let text  ← args.getObjValAs? String "text"
      return .updateContext iid cid title text
  | "list_issues_in_review" =>
    some <| do
      let pid ← args.getObjValAs? Taxis.IssueId "project_id"
      return .listIssuesInReview pid
  | "decide_issue" =>
    some <| do
      let iid      ← args.getObjValAs? Taxis.IssueId "issue_id"
      let decStr   ← args.getObjValAs? String "decision"
      let notes    ← args.getObjValAs? String "notes"
      let decision ← match decStr with
        | "approve" => Except.ok ReviewDecision.approve
        | "reject"  => Except.ok ReviewDecision.reject
        | "complete" => Except.ok ReviewDecision.complete
        | s => Except.error s!"decision must be 'approve', 'complete' or 'reject', got {repr s}"
      return .decideIssue iid decision notes
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
  /-- Hook called by `decideIssue .approve` to enqueue the merger task.
      Receives (projectId, issueId, prRef). Returning `.ok ()` is success;
      `.error msg` is surfaced to the agent. -/
  enqueueMerger : Option (Taxis.IssueId → Taxis.IssueId → PRRef → IO (Except String String)) := none
  /-- Optional auto-reviewer hook (F1). Called by `attachPr` when the
      project has a `reviewer` template configured. Receives
      `(project, issueId, prRef, template)`. -/
  enqueueReviewer : Option
    (Project → Taxis.IssueId → PRRef → ReviewerTemplate → IO (Except String String)) := none
  /-- Orchestra project this task belongs to. Used by `project_info`. -/
  projectId : Option Taxis.IssueId := none
  /-- Orchestra issue this task is working on. Used by `project_info`. -/
  issueId : Option Taxis.IssueId := none

private def deny (perm : String) : String :=
  s!"this task is not authorized for the {perm} tool group"

/-- Refusal for the tools gated on holding *any* issue group rather than a named one. Naming a
    single group, as `deny` does, would send an agent looking for the wrong permission. -/
private def denyAnyGroup : String :=
  "this task is not authorized for any of the manage_issues, work_issues or review_issues \
   tool groups"

/-- The issue bounding what this task may create or update: the root of its own project subtree
    (`Project.projectRootOf`). Anchored on the task's issue when it has one, otherwise on its
    project — a role dispatched without an issue (a planner or a maintainer) is still confined to
    its project. `none` means the task is attached to neither and cannot be scoped at all.

    An issue-less task's `projectId` is taken as the root as given, *not* re-derived: the
    dispatcher already chose it deliberately, and for a label-dispatched maintainer it is the
    labelled issue, which anchors nothing of its own when the repository artifact sits on an
    ancestor. Re-deriving would walk up to that ancestor and hand the agent write access to
    sibling subtrees nobody labelled. Where the project id *is* an anchor — every
    project-dispatcher role — `projectRootOf` returned it unchanged anyway. -/
private def writeScopeRoot (env : Env) : IO (Option Taxis.IssueId) := do
  match env.issueId, env.projectId with
  | some iid, _ => return some (← projectRootOf iid)
  | none, some pid => return some pid
  | none, none => return none

/-- Reject a write that would land outside the task's subtree. Returns an error string to
    surface, or `none` when the write is allowed. Reads are not scoped — see
    `Project.Basic`'s "Write scoping". -/
private def refuseOutsideScope (env : Env) (target : Taxis.IssueId) (what : String) :
    IO (Option String) := do
  match ← writeScopeRoot env with
  | none =>
    return some s!"cannot {what}: this task is attached to no project or issue, so there is no \
      subtree to scope the change to"
  | some root =>
    if ← isWithinSubtree root target then return none
    return some s!"cannot {what}: issue {target.toString} is outside this task's project \
      subtree (root {root.toString}); a task may only write to issues at or below it"

/-- Refuse a context note with a blank field, before taxis does.

    Taxis requires both and rejects a blank one with a 422, which reaches the agent as a thrown
    `IO.userError` rather than as a tool error: the server catches it by closing the connection,
    so an empty title costs the task every tool it had. Neither field is optional in any sense
    worth passing on — a note with no title is one the rail cannot show, and one with no text is
    not a note. -/
private def refuseBlankNote (title text : String) : Option String :=
  if title.trimAscii.isEmpty then some "a context note needs a title: it is the only part of it \
    shown before someone unfolds it"
  else if text.trimAscii.isEmpty then some "a context note needs text — there is nothing else to it"
  else none

private def has (env : Env) (perm : String) : Bool :=
  env.allowedTools.contains perm

private def renderTarget : RepoTarget → String
  | { repo, branch } => s!"{repo}@{branch}"

private def issueStatusToString : IssueStatus → String
  | .open      => "open"
  | .claimed   => "claimed"
  | .completed => "completed"
  | .abandoned => "abandoned"

/-- Render an issue summary line (used in list responses). -/
private def issueLine (i : Issue) : String :=
  let parent := i.parentId.map (·.toString) |>.getD "-"
  s!"{i.id.toString}  [{issueStatusToString i.status}]  parent={parent}  {i.title}"

/-- Render a project summary line. -/
private def projectLine (p : Project) : String :=
  let target := p.defaultTarget.map renderTarget |>.getD "-"
  s!"{p.id.toString}  {p.name}  default={target}  ({p.createdAt})"

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
      | return content s!"project {pid.toString} not found" (isError := true)
    let mut issues ← loadIssues pid
    if let some s := statusFilter then issues := issues.filter (·.status == s)
    if let some p := parentId then issues := issues.filter (·.parentId == some p)
    if issues.isEmpty then return content "No matching issues."
    return content (joinLines (issues.map issueLine))
  | .getIssue iid =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    match ← findIssue iid with
    | none => return content s!"issue {iid.toString} not found" (isError := true)
    | some (project, i) =>
      let target := (effectiveTarget project i).map renderTarget |>.getD "-"
      let prs := i.attachedPRs.map (fun p => s!"  - {p.repo}#{p.number} (branch {p.branch})")
      let children ← childrenOf project.id i.id
      let childLines := children.map (fun c => s!"  - {c.id.toString}  [{issueStatusToString c.status}]  {c.title}")
      let depsStr := if i.dependencies.isEmpty then "-"
                     else String.intercalate ", " (i.dependencies.map (·.toString)).toList
      -- The two fields `Issue` does not carry. Shown because they are what routes the issue —
      -- which worker the dispatcher offers it to, and which human is on the hook for it — and an
      -- agent asked to change them can otherwise only guess at what they are.
      let routing ← issueLabelsAndAssignees iid
      let orDash (pick : Array String × Array String → Array String) : String :=
        match routing with
        | none    => "(could not be read from taxis)"
        | some rs => let xs := pick rs
                     if xs.isEmpty then "-" else String.intercalate ", " xs.toList
      let header : Array String := #[
        s!"id:           {i.id.toString}",
        s!"project:      {project.id.toString} ({project.name})",
        s!"parent:       {i.parentId.map (·.toString) |>.getD "-"}",
        s!"title:        {i.title}",
        s!"status:       {issueStatusToString i.status}",
        s!"target:       {target}",
        s!"labels:       {orDash (·.1)}",
        s!"assignees:    {orDash (·.2)}",
        s!"dependencies: {depsStr}",
        s!"created:      {i.createdAt}",
        s!"updated:      {i.updatedAt}",
        if i.attachedPRs.isEmpty then "attached_prs: -" else "attached_prs:" ]
      let descrLines := (i.description.splitOn "\n").toArray.map (fun l => s!"  {l}")
      let mut body := header ++ prs
      if !children.isEmpty then body := body ++ #["children:"] ++ childLines
      body := body ++ #["description:"] ++ descrLines
      let comments ← loadComments iid
      if !comments.isEmpty then
        let rendered ← comments.mapM renderComment
        body := body ++ #["comments:"] ++ rendered
      -- Titles only, as the taxis rail shows them. Notes accumulate over an issue's life and
      -- are the one part of it with no bound, so listing them whole here would make the cost of
      -- reading an issue grow with how much has been written beside it.
      let notes ← loadContext iid
      if !notes.isEmpty then
        body := body ++ #["context (read with list_context):"] ++
          notes.map (fun n => s!"  [{n.id.val}] {n.title}")
      return content (joinLines body)
  | .createIssue pid title descr parent target dependencies =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    let some project ← loadProject pid
      | return content s!"project {pid.toString} not found" (isError := true)
    if target.isNone && project.defaultTarget.isNone then
      return content
        "project has no default target; pass target_repo and target_branch" (isError := true)
    -- A new issue is placed under `parent` when given, otherwise directly under the project;
    -- either way that anchor is what has to sit inside this task's subtree.
    if let some msg ← refuseOutsideScope env (parent.getD pid) "create an issue there" then
      return content msg (isError := true)
    -- The parent needs no status change: having an open child is what makes it a container,
    -- and the dispatcher reads that off the tree (`Project.dispatchCandidates`).
    if let some parentId := parent then
      if (← findIssue parentId).isNone then
        return content s!"parent issue {parentId.toString} not found" (isError := true)
    let issue ← createIssue pid title descr (parentId := parent) (target := target)
      (dependencies := dependencies)
    return content s!"created issue {issue.id.toString} in project {pid.toString}"
  | .updateIssue iid title descr status target dependencies
      labelsAdd labelsRemove assigneesAdd assigneesRemove =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    if let some msg ← refuseOutsideScope env iid "update that issue" then
      return content msg (isError := true)
    match ← findIssue iid with
    | none => return content s!"issue {iid.toString} not found" (isError := true)
    | some (_, i) =>
      let mut done : Array String := #[]
      -- Only when a field was actually named. `saveIssue` asserts the *whole* issue — including
      -- `state` and the status labels, both derived from the `i` read a moment ago — so firing it
      -- for a call that carried only `labels_add` would write back a status nobody mentioned,
      -- from a read that a worker completing the issue in between has already invalidated.
      let touchesFields :=
        title.isSome || descr.isSome || status.isSome || target.isSome || dependencies.isSome
      if touchesFields then
        let updated : Issue :=
          { i with
            title        := title.getD i.title
            description  := descr.getD i.description
            status       := status.getD i.status
            target       := match target with | some t => some t | none => i.target
            dependencies := dependencies.getD i.dependencies
            updatedAt    := now }
        saveIssue updated
        done := done.push s!"updated issue {iid.toString}"
      -- After the field write, not before: `saveIssue` reconciles the status labels against the
      -- issue as taxis holds it, so a label delta applied first would be read back and rewritten
      -- from a set that no longer matches.
      --
      -- Each half is refused on its own, and a refusal carries whatever already landed with it.
      -- An unknown label leaves the title change standing, and an agent told only "no such label"
      -- would reasonably retry the call that made it.
      -- `soFar` is a parameter and not a capture of `done`: a closure in a `do` block takes the
      -- value the variable had where the closure was written, so one reading `done` directly
      -- would report the state before the label write and lose the very line this exists to keep.
      let failure (soFar : Array String) (e : IO.Error) : Json :=
        content (joinLines (soFar ++ #[toString e])) (isError := true)
      unless labelsAdd.isEmpty && labelsRemove.isEmpty do
        try
          let change ← setIssueLabels iid labelsAdd labelsRemove
          done := done.push s!"labels: {change.summary iid.toString}"
        catch e => return failure done e
      unless assigneesAdd.isEmpty && assigneesRemove.isEmpty do
        try
          let change ← setIssueAssignees iid assigneesAdd assigneesRemove
          done := done.push s!"assignees: {change.summary iid.toString}"
        catch e => return failure done e
      if done.isEmpty then
        return content s!"issue {iid.toString}: nothing to change — no field, label or assignee \
          was named"
      return content (joinLines done)
  | .listLabels =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    let names ← listLabelNames
    if names.isEmpty then return content "The tracker defines no labels."
    return content (joinLines (names.map (s!"  - {·}")))
  | .listActors =>
    if !has env manageIssuesPerm then return content (deny manageIssuesPerm) (isError := true)
    let actors ← listActorSummaries
    if actors.isEmpty then return content "The tracker knows no actors."
    return content (joinLines (actors.map fun (name, email, bot) =>
      s!"  - {email}  ({name}){if bot then "  [bot]" else ""}"))
  -- ---------------- work_issues ----------------
  | .listOpenIssues pid targetRepo? =>
    if !has env workIssuesPerm then return content (deny workIssuesPerm) (isError := true)
    let some project ← loadProject pid
      | return content s!"project {pid.toString} not found" (isError := true)
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
    | none => return content s!"issue {iid.toString} not found" (isError := true)
    | some (project, i) =>
      IO.println s!"  [mcp] claim_issue: {iid.toString} \"{i.title}\""
      match ← tryClaim mgr project.id iid taskId env.agentBackend now env.series with
      | .acquired _ =>
        let target := (effectiveTarget project i).map renderTarget |>.getD ""
        IO.println s!"  [mcp] claim_issue: acquired (target={target})"
        let payload := Json.mkObj
          [ ("ok", true)
          , ("issue_id",     ToJson.toJson iid)
          , ("project_id",   ToJson.toJson project.id)
          , ("target",       Json.str target) ]
        return content payload.compress
      | .alreadyClaimed existing =>
        IO.println s!"  [mcp] claim_issue: already claimed by task {existing.taskId}"
        let payload := Json.mkObj
          [ ("ok", false)
          , ("error", Json.str "already_claimed")
          , ("held_by_task", Json.str existing.taskId) ]
        return content payload.compress (isError := true)
      | .invalid reason =>
        IO.println s!"  [mcp] claim_issue: invalid — {reason}"
        return content s!"invalid claim: {reason}" (isError := true)
  | .releaseClaim iid reason =>
    if !has env workIssuesPerm then return content (deny workIssuesPerm) (isError := true)
    let some mgr := env.claimManager
      | return content "claim manager not available in this context" (isError := true)
    match ← findIssue iid with
    | none => return content s!"issue {iid.toString} not found" (isError := true)
    | some (project, i) =>
      IO.println s!"  [mcp] release_claim: {iid.toString} \"{i.title}\" — {reason}"
      -- Nothing to preserve: whether the issue is under review is derived from its attached PRs,
      -- not from a status, so releasing simply returns it to the open pool.
      let newStatus : IssueStatus := .open
      let _ ← release mgr project.id iid newStatus now
      return content s!"released claim on {iid.toString} ({reason})"
  | .attachPr iid repo number branch =>
    if !has env workIssuesPerm then return content (deny workIssuesPerm) (isError := true)
    let some taskId := env.taskId
      | return content "no task id in context (cannot verify claim ownership)" (isError := true)
    match ← findIssue iid with
    | none => return content s!"issue {iid.toString} not found" (isError := true)
    | some (project, i) =>
      match ← loadClaim iid with
      | none =>
        return content s!"cannot attach PR to {iid.toString}: issue is not claimed" (isError := true)
      | some claim =>
        if claim.taskId != taskId then
          return content
            s!"cannot attach PR to {iid.toString}: held by task {claim.taskId}, not this task"
            (isError := true)
        else
          IO.println s!"  [mcp] attach_pr: {iid.toString} \"{i.title}\" ← {repo}#{number} (branch {branch})"
          let pr : PRRef := { repo, number, branch, taskId := env.taskId }
          -- Attach first: if this fails the issue stays claimed rather than looking review-ready
          -- with no PR to review.
          try
            attachPR iid pr
          catch e =>
            return content s!"failed to attach {repo}#{number} to {iid.toString}: {e}"
              (isError := true)
          -- No status change: an issue is under review because it has an unmerged PR attached,
          -- which is now true by virtue of the attach itself.
          saveIssue { i with updatedAt := now }
          -- F1: if the project configures an auto-reviewer, enqueue it now.
          let reviewerNote ← match project.reviewer, env.enqueueReviewer with
            | some tmpl, some hook =>
              match ← hook project iid pr tmpl with
              | .ok rid    =>
                IO.println s!"  [mcp] attach_pr: reviewer task {rid} enqueued"
                pure s!"; reviewer task {rid} enqueued"
              | .error msg =>
                IO.println s!"  [mcp] attach_pr: reviewer enqueue failed — {msg}"
                pure s!"; reviewer enqueue failed: {msg}"
            | _, _ => pure ""
          return content
            s!"attached {repo}#{number} to {iid.toString}; it is now awaiting review{reviewerNote}"
  | .splitIssue parentId children reason =>
    if !has env workIssuesPerm then return content (deny workIssuesPerm) (isError := true)
    let some mgr := env.claimManager
      | return content "claim manager not available in this context" (isError := true)
    let some taskId := env.taskId
      | return content "no task id in context (cannot verify claim ownership)" (isError := true)
    match ← findIssue parentId with
    | none => return content s!"issue {parentId.toString} not found" (isError := true)
    | some (project, parent) =>
      IO.println s!"  [mcp] split_issue: {parentId.toString} \"{parent.title}\" → {children.size} sub-issues — {reason}"
      -- Caller must already hold the claim on this parent. We don't allow
      -- workers to split issues they don't own — that would let a stray
      -- agent rearrange someone else's work.
      match ← loadClaim parentId with
      | none =>
        return content s!"cannot split {parentId.toString}: not currently claimed" (isError := true)
      | some claim =>
        if claim.taskId != taskId then
          return content
            s!"cannot split {parentId.toString}: held by task {claim.taskId}, not this task"
            (isError := true)
        else
          let inheritedTarget := effectiveTarget project parent
          let mut createdIds : Array Taxis.IssueId := #[]
          for spec in children do
            let issue ← createIssue project.id spec.title spec.description
              (parentId := some parentId) (target := spec.target <|> inheritedTarget)
            IO.println s!"  [mcp] split_issue: created sub-issue {issue.id.toString} \"{spec.title}\""
            createdIds := createdIds.push issue.id
          -- Clear the claim without touching status: the new children are open, which is what
          -- makes the parent a container as far as the dispatcher is concerned. `forceRelease`
          -- rather than `release` because the latter sets a status unconditionally.
          let _ ← forceRelease mgr parentId
          let payload := Json.mkObj
            [ ("ok",         true)
            , ("parent_id",  ToJson.toJson parentId)
            , ("reason",     Json.str reason)
            , ("created",    Json.arr (createdIds.map ToJson.toJson)) ]
          return content payload.compress
  | .listIssueComments iid =>
    -- Offered under every group that lists it, so accept any of them here rather than a single
    -- permission: the reviewer writes the thread and the worker has to be able to read it.
    if !(has env workIssuesPerm || has env reviewIssuesPerm || has env manageIssuesPerm) then
      return content (deny reviewIssuesPerm) (isError := true)
    let comments ← loadComments iid
    if comments.isEmpty then return content s!"No comments on issue {iid.toString}."
    let rendered ← comments.mapM renderComment
    return content (joinLines rendered)
  | .commentIssue iid body =>
    if !(has env workIssuesPerm || has env reviewIssuesPerm) then
      return content (deny reviewIssuesPerm) (isError := true)
    addComment iid body
    IO.println s!"  [mcp] comment_issue: {iid.toString}"
    return content s!"commented on issue {iid.toString}"
  -- ---------------- context notes (all three groups) ----------------
  | .listContext iid =>
    if !(has env workIssuesPerm || has env reviewIssuesPerm || has env manageIssuesPerm) then
      return content denyAnyGroup (isError := true)
    match ← renderContextNotes iid with
    | none   => return content s!"No context notes on issue {iid.toString}."
    | some s => return content s
  | .addContext iid title text =>
    if !(has env workIssuesPerm || has env reviewIssuesPerm || has env manageIssuesPerm) then
      return content denyAnyGroup (isError := true)
    if let some msg := refuseBlankNote title text then
      return content msg (isError := true)
    if let some msg ← refuseOutsideScope env iid "add a context note there" then
      return content msg (isError := true)
    let note ← attachContext iid title text
    IO.println s!"  [mcp] add_context: {iid.toString} note {note.id.val} — {note.title}"
    return content s!"context note {note.id.val} attached to issue {iid.toString}"
  | .updateContext iid cid title text =>
    if !(has env workIssuesPerm || has env reviewIssuesPerm || has env manageIssuesPerm) then
      return content denyAnyGroup (isError := true)
    if let some msg := refuseBlankNote title text then
      return content msg (isError := true)
    if let some msg ← refuseOutsideScope env iid "revise a context note there" then
      return content msg (isError := true)
    -- Checked against the issue named rather than patched blind: an artifact id is
    -- tracker-wide, so a stale or guessed one otherwise reaches straight past the scope check
    -- above into another issue's subtree.
    let notes ← loadContext iid
    if !notes.any (·.id == cid) then
      return content s!"issue {iid.toString} has no context note {cid.val} — list_context \
        shows the ones it has" (isError := true)
    reviseContext cid title text
    IO.println s!"  [mcp] update_context: {iid.toString} note {cid.val}"
    return content s!"context note {cid.val} on issue {iid.toString} rewritten"
  -- ---------------- review_issues ----------------
  | .listIssuesInReview pid =>
    if !has env reviewIssuesPerm then return content (deny reviewIssuesPerm) (isError := true)
    let some _ ← loadProject pid
      | return content s!"project {pid.toString} not found" (isError := true)
    -- Derived, not a status: open issues carrying at least one attached PR. `loadIssues` leaves
    -- attachedPRs empty for speed, so each open issue is re-fetched; and merge state is not
    -- consulted here, so a PR merged by hand still shows until the issue is completed.
    let mut issues : Array Issue := #[]
    for i in (← loadIssues pid).filter (·.status == .open) do
      if let some full ← loadIssue pid i.id then
        if !full.attachedPRs.isEmpty then issues := issues.push full
    if issues.isEmpty then return content "No issues awaiting review."
    return content (joinLines (issues.map issueLine))
  | .decideIssue iid decision notes =>
    if !has env reviewIssuesPerm then return content (deny reviewIssuesPerm) (isError := true)
    match ← findIssue iid with
    | none => return content s!"issue {iid.toString} not found" (isError := true)
    | some (project, i) =>
      let decisionStr := match decision with
        | .approve => "approve" | .complete => "complete" | .reject => "reject"
      IO.println s!"  [mcp] decide_issue: {iid.toString} \"{i.title}\" → {decisionStr} — {notes}"
      match decision with
      | .reject =>
        -- The verdict belongs on the issue, not just in the tool response: the next worker to
        -- pick this up needs to know why it came back, and the response is seen only by this
        -- reviewer. Failing to record it must not fail the decision itself.
        try addComment iid notes (review := some .requestChanges)
        catch e => IO.eprintln s!"  [mcp] decide_issue: could not record the review: {e}"
        -- Move back to .open and clear any claim. Notes are echoed back; the
        -- comment tool is the right place to post them on the PR if desired.
        if let some mgr := env.claimManager then
          let _ ← release mgr project.id iid .open now
        else
          saveIssue { i with status := .open, updatedAt := now }
        IO.println s!"  [mcp] decide_issue: {iid.toString} moved to open"
        return content s!"rejected {iid.toString}: {notes}"
      | .complete =>
        try addComment iid notes (review := some .approve)
        catch e => IO.eprintln s!"  [mcp] decide_issue: could not record the review: {e}"
        if let some mgr := env.claimManager then
          let _ ← release mgr project.id iid .completed now
        else
          saveIssue { i with status := .completed, updatedAt := now }
        IO.println s!"  [mcp] decide_issue: {iid.toString} completed"
        return content s!"completed {iid.toString}: {notes}"
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
              try addComment iid notes (review := some .approve)
              catch e => IO.eprintln s!"  [mcp] decide_issue: could not record the review: {e}"
              IO.println s!"  [mcp] decide_issue: {iid.toString} approved; merger task {mergerTaskId} enqueued"
              return content
                s!"approved {iid.toString}; merger task {mergerTaskId} enqueued. The issue stays \
                   open — use decide_issue with 'complete' when the work is finished. ({notes})"
  | .projectInfo =>
    IO.println "  [mcp] project_info"
    match env.projectId with
    | none => return content "this task is not attached to a project" (isError := true)
    | some pid =>
      match ← loadProject pid with
      | none => return content s!"project {pid.toString} not found" (isError := true)
      | some project =>
        let mut lines : Array String := #[
          s!"project_id:   {project.id.toString}",
          s!"project_name: {project.name}"
        ]
        match env.issueId with
        | none => lines := lines.push "issue:        none"
        | some iid =>
          match ← findIssue iid with
          | none => lines := lines.push s!"issue_id:     {iid.toString} (not found)"
          | some (_, issue) =>
            lines := lines.push s!"issue_id:     {issue.id.toString}"
            lines := lines.push s!"issue_title:  {issue.title}"
            lines := lines.push s!"issue_status: {issueStatusToString issue.status}"
            if let some claim ← loadClaim iid then
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
