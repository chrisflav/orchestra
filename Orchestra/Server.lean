import Lean.Data.Json
import Std.Internal.UV.TCP
import Std.Net
import Orchestra.Config
import Orchestra.GitHub
import Orchestra.Project.Tools

open Lean (Json)
open Std.Net
open Std.Internal.UV.TCP

namespace Orchestra.Server

/-- Mutable state for the server, shared with request handlers. -/
structure State where
  upstream : Repository
  fork : Repository
  /-- Optional tools enabled for this run.
      Always-available tools (health, refresh_token, get_pr_comments) are never in this list.
      The names this server itself understands are `"create_pr"`, `"merge_pr"`, `"label_issue"`,
      `"comment"` and `"create_repository"` (see `optionalToolDefs`); the rest are the permission
      groups of `Orchestra.Project.Tools`. -/
  allowedTools : List String
  appId : Nat
  privateKeyPath : String
  installationId : Nat
  pat : String
  /-- Input type of the current task. When not `.unit`, the `get_task_input` tool is exposed. -/
  inputType : ResultType := .unit
  /-- Output type of the current task. When not `.unit`, the `submit_task_output` tool is exposed. -/
  outputType : ResultType := .unit
  /-- The task input serialized as JSON. `none` when `inputType = .unit`. -/
  inputJson : Option Json := none
  /-- Mutable cell where the agent stores its typed output via `submit_task_output`. -/
  outputRef : Option (IO.Ref (Option Json)) := none
  /-- Issue or PR number this task was launched from. Required for the `comment` tool. -/
  issueNumber : Option Nat := none
  /-- Claim manager handle. Required for `claim_issue` / `release_claim` /
      `decide_issue reject`. `none` outside the daemon. -/
  claimManager : Option Project.ClaimManager := none
  /-- Orchestra task ID, recorded as the holder when claims are taken. -/
  taskId : Option String := none
  /-- Orchestra project this task belongs to. Enables the `project_info` tool. -/
  projectId : Option Taxis.IssueId := none
  /-- Orchestra issue this task is working on (may be pre-claimed or runtime-claimed). -/
  issueId : Option Taxis.IssueId := none
  /-- Backend label of the running agent (e.g. "claude"). Recorded with claims. -/
  agentBackend : String := "unknown"
  /-- Series the task belongs to. Recorded with claims. -/
  series : Option String := none
  /-- Hook that enqueues a merger task, set by the daemon. Plumbed by
      `Project.Tools.Env` so `decide_issue approve` can request a merge. -/
  enqueueMerger : Option (Taxis.IssueId → Taxis.IssueId → Project.PRRef →
                          IO (Except String String)) := none
  /-- Optional auto-reviewer hook (F1). Plumbed to `Project.Tools.Env.enqueueReviewer`. -/
  enqueueReviewer : Option (Project.Project → Taxis.IssueId → Project.PRRef →
                            Project.ReviewerTemplate → IO (Except String String)) := none
  /-- Labels to apply automatically to every PR created via `create_pr`.
      Missing labels are created on the target repository before the PR is opened. -/
  prLabels : List String := []
  /-- The organisation `create_repository` creates repositories in: `default_organization` from
      the config, the same org tasks are forked into.

      Deliberately the only destination the tool offers. An agent that could name the owner could
      create repositories anywhere the App is installed, including under the accounts it is
      working on; the configured fork org is the one place the operator has already agreed is
      orchestra's to write to. `none` — no `default_organization` — means the tool has nowhere to
      create anything and refuses. -/
  defaultOrganization : Option String := none

private def log (msg : String) : IO Unit := do
  let err ← IO.getStderr
  err.putStrLn s!"[mcp] {msg}"
  err.flush

-- JSON-RPC helpers

private def jsonrpcResult (id : Json) (result : Json) : Json :=
  Json.mkObj [("jsonrpc", "2.0"), ("id", id), ("result", result)]

private def jsonrpcError (id : Json) (code : Int) (msg : String) : Json :=
  Json.mkObj [
    ("jsonrpc", "2.0"),
    ("id", id),
    ("error", Json.mkObj [("code", .num ⟨code, 0⟩), ("message", .str msg)])
  ]

private def toolContent (text : String) (isError : Bool := false) : Json :=
  Json.mkObj [
    ("content", .arr #[Json.mkObj [("type", "text"), ("text", .str text)]]),
    ("isError", isError)
  ]

private def initializeResult : Json :=
  Json.mkObj [
    ("protocolVersion", "2024-11-05"),
    ("capabilities", Json.mkObj [("tools", Json.mkObj [])]),
    ("serverInfo", Json.mkObj [("name", "agent"), ("version", "0.1.0")])
  ]

private def alwaysAvailableTools : Array Json := #[
  Json.mkObj [
    ("name", "health"),
    ("description", "Check that the agent MCP server is running."),
    ("inputSchema", Json.mkObj [("type", "object"), ("properties", Json.mkObj [])])
  ],
  Json.mkObj [
    ("name", "refresh_token"),
    ("description", "Mint a fresh GitHub App installation token and return it. \
Export it as GH_TOKEN to use it; it is not applied to the gh CLI for you, because other \
tasks may be running in the same daemon and share that configuration."),
    ("inputSchema", Json.mkObj [("type", "object"), ("properties", Json.mkObj [])])
  ],
  Json.mkObj [
    ("name", "get_pr_comments"),
    ("description", "Fetch review comments on a pull request from the upstream repository."),
    ("inputSchema", Json.mkObj [
      ("type", "object"),
      ("properties", Json.mkObj [
        ("pr_number", Json.mkObj [
          ("type", "integer"),
          ("description", "Pull request number.")
        ]),
        ("unresolved_only", Json.mkObj [
          ("type", "boolean"),
          ("description", "Only show unresolved conversations (default: false).")
        ]),
        ("exclude_outdated", Json.mkObj [
          ("type", "boolean"),
          ("description", "Exclude outdated comments (default: false).")
        ])
      ]),
      ("required", .arr #["pr_number"])
    ])
  ]
]

private def optionalToolDefs : List (String × Json) := [
  ("create_pr", Json.mkObj [
    ("name", "create_pr"),
    ("description",
      "Create a pull request. By default the PR is opened on the upstream " ++
      "repository (cross-repo, head=fork-owner:branch, authenticated by the " ++
      "configured PAT). Set target=\"fork\" to open the PR on the fork " ++
      "repository instead — same-repo head, authenticated by a freshly-minted " ++
      "GitHub App installation token, no PAT required."),
    ("inputSchema", Json.mkObj [
      ("type", "object"),
      ("properties", Json.mkObj [
        ("title", Json.mkObj [("type", "string")]),
        ("body", Json.mkObj [("type", "string")]),
        ("head", Json.mkObj [
          ("type", "string"),
          ("description", "Branch name in the fork.")
        ]),
        ("base", Json.mkObj [("type", "string")]),
        ("target", Json.mkObj [
          ("type", "string"),
          ("enum", Json.arr #["upstream", "fork"]),
          ("description",
            "Where to open the PR. \"upstream\" (default) targets " ++
            "state.upstream via PAT; \"fork\" targets state.fork via the " ++
            "GitHub App installation token.")
        ])
      ]),
      ("required", .arr #["head"])
    ])
  ]),
  ("merge_pr", Json.mkObj [
    ("name", "merge_pr"),
    ("description",
      "Merge a pull request on the upstream repository. Squash-merges it and deletes the head " ++
      "branch by default, which is what orchestra's own merger does; pass merge_method and " ++
      "delete_branch to change that.\n\n" ++
      "Refuses, with the reason, when the pull request is already merged, closed, still a " ++
      "draft, in conflict with its base branch, or held back by branch protection (a missing " ++
      "review or a failing required check). Report that reason back rather than calling again — " ++
      "none of those clear up by retrying."),
    ("inputSchema", Json.mkObj [
      ("type", "object"),
      ("properties", Json.mkObj [
        ("pr_number", Json.mkObj [
          ("type", "integer"),
          ("description", "Pull request number on the upstream repository.")
        ]),
        ("merge_method", Json.mkObj [
          ("type", "string"),
          ("enum", Json.arr #["merge", "squash", "rebase"]),
          ("description", "How to merge (default: squash).")
        ]),
        ("delete_branch", Json.mkObj [
          ("type", "boolean"),
          ("description", "Delete the head branch after merging (default: true).")
        ])
      ]),
      ("required", .arr #["pr_number"])
    ])
  ]),
  ("label_issue", Json.mkObj [
    ("name", "label_issue"),
    ("description",
      "Add and remove labels on an issue or pull request of the upstream repository — the " ++
      "triage tool. Give the number and at least one label in add or remove; both may be " ++
      "given at once.\n\n" ++
      "Only labels the repository already defines can be applied: an unknown name is refused " ++
      "with the list of labels that do exist, rather than inventing one. Names are matched " ++
      "case-insensitively. An addition the issue already carries and a removal it does not are " ++
      "reported and skipped, so a repeated call changes nothing.\n\n" ++
      "Any issue or pull request may be labelled, not only the one this task was launched " ++
      "from — unlike comment."),
    ("inputSchema", Json.mkObj [
      ("type", "object"),
      ("properties", Json.mkObj [
        ("issue_number", Json.mkObj [
          ("type", "integer"),
          ("description",
            "Issue or pull request number on the upstream repository (they share one numbering).")
        ]),
        ("add", Json.mkObj [
          ("type", "array"),
          ("items", Json.mkObj [("type", "string")]),
          ("description", "Labels to add. Must already exist on the repository.")
        ]),
        ("remove", Json.mkObj [
          ("type", "array"),
          ("items", Json.mkObj [("type", "string")]),
          ("description", "Labels to remove.")
        ])
      ]),
      ("required", .arr #["issue_number"])
    ])
  ]),
  ("create_repository", Json.mkObj [
    ("name", "create_repository"),
    ("description",
      "Create a new GitHub repository in orchestra's configured organisation (the one tasks " ++
      "are forked into). Only `name` is required.\n\n" ++
      "The owner is not yours to choose: the repository is always created in that organisation, " ++
      "and the tool refuses when none is configured. It is created **private** unless you pass " ++
      "private=false, and with no commits unless you pass auto_init=true — an empty repository " ++
      "accepts a push of any history, whereas an auto-initialised one already has a README " ++
      "commit that a local history would have to be reconciled with.\n\n" ++
      "Not idempotent, unlike forking: a name the organisation already uses is refused rather " ++
      "than handed back, so nothing is pushed to a repository other than the one just made. " ++
      "Pick another name or work on the existing repository.\n\n" ++
      "Push to it with a token from refresh_token; the one the task started with is scoped to " ++
      "the repositories it was launched against and does not reach this one."),
    ("inputSchema", Json.mkObj [
      ("type", "object"),
      ("properties", Json.mkObj [
        ("name", Json.mkObj [
          ("type", "string"),
          ("description",
            "Repository name, without an owner. Letters, digits, '-', '_' and '.' only.")
        ]),
        ("description", Json.mkObj [
          ("type", "string"),
          ("description", "Repository description (default: empty).")
        ]),
        ("private", Json.mkObj [
          ("type", "boolean"),
          ("description", "Create it private (default: true).")
        ]),
        ("auto_init", Json.mkObj [
          ("type", "boolean"),
          ("description",
            "Create an initial commit with a README (default: false, i.e. an empty repository).")
        ])
      ]),
      ("required", .arr #["name"])
    ])
  ]),
  ("comment", Json.mkObj [
    ("name", "comment"),
    ("description",
      "Post a comment on the issue or pull request this task was launched from.\n\n" ++
      "Four modes determined by the arguments provided:\n" ++
      "• Regular comment: provide only `body`.\n" ++
      "• Pull-request review: provide `body` and set `review` to true. " ++
        "Optionally include `inline_comments` — an array of `{path, line, body, side}` objects " ++
        "to attach inline comments to the review (COMMENT event, no approval/rejection).\n" ++
      "• Reply to an existing inline PR review comment: provide `body` and `reply_to_comment_id`.\n" ++
      "• New inline PR review comment on a specific file and line: provide `body`, `path`, and `line`.\n\n" ++
      "`review`, `reply_to_comment_id`, and `path`/`line` are mutually exclusive."),
    ("inputSchema", Json.mkObj [
      ("type", "object"),
      ("properties", Json.mkObj [
        ("body", Json.mkObj [
          ("type", "string"),
          ("description", "The comment text.")
        ]),
        ("review", Json.mkObj [
          ("type", "boolean"),
          ("description",
            "When true, post body as a pull-request review (COMMENT event) rather than a plain comment. " ++
            "Mutually exclusive with reply_to_comment_id and path/line.")
        ]),
        ("inline_comments", Json.mkObj [
          ("type", "array"),
          ("description",
            "Optional list of inline comments to include in the review (only valid with review=true). " ++
            "Each item must have path (string), line (integer), and body (string); " ++
            "side (\"LEFT\" or \"RIGHT\", default \"RIGHT\") is optional."),
          ("items", Json.mkObj [
            ("type", "object"),
            ("properties", Json.mkObj [
              ("path",  Json.mkObj [("type", "string")]),
              ("line",  Json.mkObj [("type", "integer")]),
              ("body",  Json.mkObj [("type", "string")]),
              ("side",  Json.mkObj [("type", "string"), ("enum", .arr #["LEFT", "RIGHT"])])
            ]),
            ("required", .arr #["path", "line", "body"])
          ])
        ]),
        ("reply_to_comment_id", Json.mkObj [
          ("type", "integer"),
          ("description",
            "ID of the inline PR review comment to reply to. " ++
            "Mutually exclusive with review, path and line.")
        ]),
        ("path", Json.mkObj [
          ("type", "string"),
          ("description",
            "File path for a new inline review comment. " ++
            "Required together with line. Mutually exclusive with review and reply_to_comment_id.")
        ]),
        ("line", Json.mkObj [
          ("type", "integer"),
          ("description",
            "Line number for a new inline review comment. " ++
            "Required together with path. Mutually exclusive with review and reply_to_comment_id.")
        ]),
        ("side", Json.mkObj [
          ("type", "string"),
          ("description",
            "Which side of the diff to comment on when creating a new inline review comment " ++
            "(default: RIGHT)."),
          ("enum", .arr #["LEFT", "RIGHT"])
        ])
      ]),
      ("required", .arr #["body"])
    ])
  ]),
]

private def ioToolDefs (inputType outputType : ResultType) : Array Json :=
  let inputTool := match inputType with
    | .unit => #[]
    | t => #[Json.mkObj [
        ("name", "get_task_input"),
        ("description", s!"Retrieve the input value for this task. \
The value is {t.toDescription}. \
JSON schema: {t.toJsonSchema.compress}"),
        ("inputSchema", Json.mkObj [("type", "object"), ("properties", Json.mkObj [])])
      ]]
  let outputTool := match outputType with
    | .unit => #[]
    | t => #[Json.mkObj [
        ("name", "submit_task_output"),
        ("description", s!"Submit the output value for this task. \
The value must be {t.toDescription}. \
JSON schema for the 'value' field: {t.toJsonSchema.compress}. \
Call this tool exactly once when the task is complete."),
        ("inputSchema", Json.mkObj [
          ("type", "object"),
          ("properties", Json.mkObj [
            ("value", t.toJsonSchema)
          ]),
          ("required", Json.arr #[.str "value"])
        ])
      ]]
  inputTool ++ outputTool

/-- The `tools/list` answer for `state`: the always-available tools, plus every optional one the
    task was granted.

    Not private, for the same reason `evalToolCall` is not: a tool is offered under one name and
    gated under another, in two lists written out separately, and a disagreement between them
    leaves a tool that is listed and always refused — or granted and never offered. That is worth
    a test, and the test needs no network. -/
def toolsList (state : State) : Json :=
  let optional := optionalToolDefs.filterMap fun entry =>
    if state.allowedTools.contains entry.1 then some entry.2 else none
  -- Deduped by name: a tool may be listed under more than one permission group (an issue's
  -- comment thread is readable by workers and reviewers alike), and a task holding two of them
  -- would otherwise be offered the same tool twice.
  let project := (Project.Tools.toolDefs.filterMap fun (perm, name, def_) =>
      if state.allowedTools.contains perm then some (name, def_) else none)
    |>.foldl (fun acc (name, def_) =>
        if acc.any (·.1 == name) then acc else acc ++ [(name, def_)]) []
    |>.map (·.2)
  let io := ioToolDefs state.inputType state.outputType
  let projectInfo := if state.projectId.isSome then #[Project.Tools.projectInfoToolDef] else #[]
  Json.mkObj [("tools",
    .arr (alwaysAvailableTools ++ projectInfo ++ optional.toArray ++ project.toArray ++ io))]

-- Types

/-- Where a `create_pr` call should open its PR. -/
inductive PrTarget where
  /-- Cross-repo PR: head=`fork.owner:branch`, against `state.upstream`. PAT-auth.
      This is the default and the original behaviour. -/
  | upstream
  /-- Same-repo PR: head=bare branch, against `state.fork`. Authenticated by a
      freshly-minted GitHub App installation token, no PAT required. -/
  | fork
deriving Repr, Inhabited

/-- The action performed by the `comment` tool. -/
inductive CommentAction where
  /-- Post a top-level comment on the issue or PR. -/
  | issue (body : String)
  /-- Post a pull-request review body with optional inline comments (COMMENT event, no approval/rejection). -/
  | review (body : String) (inlineComments : Array GitHub.InlineComment)
  /-- Reply to an existing inline PR review comment. -/
  | replyInline (body : String) (commentId : Nat)
  /-- Create a new inline PR review comment on a specific file and line. -/
  | newInline (comment : GitHub.InlineComment)

/-- A parsed and validated tool call. `parseError` carries an argument validation failure. -/
inductive ToolCall where
  | health
  | refreshToken
  /-- Where a `create_pr` call should open its PR. -/
  | createPr (title : String) (body : String) (head : String) (base : String)
             (target : PrTarget)
  /-- Merge a pull request on the upstream repository. -/
  | mergePr (prNumber : Nat) (method : GitHub.MergeMethod) (deleteBranch : Bool)
  /-- Add and remove labels on an issue or pull request of the upstream repository. -/
  | labelIssue (issueNumber : Nat) (add : List String) (remove : List String)
  /-- Create a repository in the configured `default_organization`. -/
  | createRepository (name : String) (description : String) (isPrivate : Bool) (autoInit : Bool)
  | getPrComments (prNumber : Nat) (unresolvedOnly : Bool) (excludeOutdated : Bool)
  | comment (action : CommentAction)
  | getTaskInput
  | submitTaskOutput (value : Json)
  /-- A project / issue tool from `Orchestra.Project.Tools`. -/
  | project (call : Project.Tools.ProjectTool)
  | unknown (name : String)
  | parseError (msg : String)

/-- A parsed JSON-RPC request or notification. -/
inductive Request where
  | initialize (id : Json)
  | initialized
  | toolsList (id : Json)
  | toolsCall (id : Json) (call : ToolCall)
  | unknown (id : Json) (method : String)

-- Parsing

/-- Read one of `label_issue`'s label lists. An absent list is empty; anything that is not an
    array of non-empty strings is an error rather than a silently dropped label, since a triage
    agent told its call succeeded would report a label it never applied. -/
private def parseLabelList (args : Json) (key : String) : Except String (List String) :=
  match args.getObjVal? key |>.toOption with
  | none => .ok []
  | some j =>
    match j.getArr? |>.toOption with
    | none => .error s!"'{key}' must be an array of label names"
    | some items =>
      items.toList.foldlM (init := []) fun acc item =>
        match item.getStr? |>.toOption with
        | none => .error s!"'{key}' must contain only strings"
        | some s =>
          let trimmed := s.trimAscii.toString
          if trimmed.isEmpty then .error s!"'{key}' must not contain an empty label name"
          else .ok (acc ++ [trimmed])

def parseToolCall (name : String) (args : Json) : ToolCall :=
  match name with
  | "health" => .health
  | "refresh_token" => .refreshToken
  | "project_info" => .project .projectInfo
  | "create_pr" =>
    let title := args.getObjValAs? String "title" |>.toOption |>.getD "Agent PR"
    let body  := args.getObjValAs? String "body"  |>.toOption |>.getD ""
    let head  := args.getObjValAs? String "head"  |>.toOption |>.getD ""
    let base  := args.getObjValAs? String "base"  |>.toOption |>.getD "main"
    let targetStr := args.getObjValAs? String "target" |>.toOption |>.getD "upstream"
    let target? : Option PrTarget := match targetStr with
      | "upstream" => some .upstream
      | "fork"     => some .fork
      | _          => none
    match target? with
    | none =>
      .parseError s!"invalid 'target' (expected \"upstream\" or \"fork\", got {repr targetStr})"
    | some target =>
      if head.isEmpty then .parseError "missing 'head' (branch name)"
      else .createPr title body head base target
  | "merge_pr" =>
    match args.getObjVal? "pr_number" |>.toOption with
    | none => .parseError "missing required argument: pr_number"
    | some prNumJson =>
      match prNumJson.getInt? |>.toOption with
      | none => .parseError "pr_number must be an integer"
      | some prNumInt =>
        if prNumInt <= 0 then .parseError "pr_number must be a positive integer"
        else
          let methodStr := args.getObjValAs? String "merge_method" |>.toOption |>.getD "squash"
          match GitHub.MergeMethod.ofString? methodStr with
          | none =>
            .parseError s!"invalid 'merge_method' (expected \"merge\", \"squash\" or \"rebase\", \
              got {repr methodStr})"
          | some method =>
            let deleteBranch := args.getObjValAs? Bool "delete_branch" |>.toOption |>.getD true
            .mergePr prNumInt.toNat method deleteBranch
  | "label_issue" =>
    match args.getObjVal? "issue_number" |>.toOption with
    | none => .parseError "missing required argument: issue_number"
    | some numJson =>
      match numJson.getInt? |>.toOption with
      | none => .parseError "issue_number must be an integer"
      | some numInt =>
        if numInt <= 0 then .parseError "issue_number must be a positive integer"
        else
          match parseLabelList args "add" with
          | .error e => .parseError e
          | .ok add =>
            match parseLabelList args "remove" with
            | .error e => .parseError e
            | .ok remove =>
              if add.isEmpty && remove.isEmpty then
                .parseError "nothing to do: give at least one label in 'add' or 'remove'"
              else .labelIssue numInt.toNat add remove
  | "create_repository" =>
    match args.getObjValAs? String "name" |>.toOption with
    | none => .parseError "missing required argument: name"
    | some rawName =>
      let name := rawName.trimAscii.toString
      match GitHub.repoNameError? name with
      | some e => .parseError e
      | none =>
        let description := args.getObjValAs? String "description" |>.toOption |>.getD ""
        -- Both default to the cautious answer: a repository that is private and empty can be
        -- opened up or filled in afterwards, while neither publishing nor an unwanted initial
        -- commit can be taken back from a caller that simply omitted the argument.
        let isPrivate := args.getObjValAs? Bool "private"   |>.toOption |>.getD true
        let autoInit  := args.getObjValAs? Bool "auto_init" |>.toOption |>.getD false
        .createRepository name description isPrivate autoInit
  | "get_pr_comments" =>
    match args.getObjVal? "pr_number" |>.toOption with
    | none => .parseError "missing required argument: pr_number"
    | some prNumJson =>
      match prNumJson.getInt? |>.toOption with
      | none => .parseError "pr_number must be an integer"
      | some prNumInt =>
        if prNumInt <= 0 then .parseError "pr_number must be a positive integer"
        else
          let unresolvedOnly  := args.getObjValAs? Bool "unresolved_only"  |>.toOption |>.getD false
          let excludeOutdated := args.getObjValAs? Bool "exclude_outdated" |>.toOption |>.getD false
          .getPrComments prNumInt.toNat unresolvedOnly excludeOutdated
  | "comment" =>
    match args.getObjValAs? String "body" |>.toOption with
    | none => .parseError "missing required 'body' argument"
    | some body =>
      if body.isEmpty then .parseError "'body' must not be empty"
      else
        let review    := args.getObjValAs? Bool "review"              |>.toOption |>.getD false
        let replyToId := args.getObjValAs? Nat "reply_to_comment_id" |>.toOption
        let path      := args.getObjValAs? String "path"             |>.toOption
        let line      := args.getObjValAs? Nat "line"                |>.toOption
        let side      := args.getObjValAs? String "side"             |>.toOption |>.getD "RIGHT"
        if review then
          match replyToId, path, line with
          | none, none, none =>
            let inlineComments : Array GitHub.InlineComment :=
              match args.getObjVal? "inline_comments" |>.toOption with
              | none => #[]
              | some arr =>
                match arr.getArr? |>.toOption with
                | none => #[]
                | some items =>
                  items.filterMap fun item =>
                    match item.getObjValAs? String "path" |>.toOption,
                          item.getObjValAs? Nat  "line"   |>.toOption,
                          item.getObjValAs? String "body" |>.toOption with
                    | some p, some l, some b =>
                      let s := item.getObjValAs? String "side" |>.toOption |>.getD "RIGHT"
                      some { path := p, line := l, body := b, side := s }
                    | _, _, _ => none
            .comment (.review body inlineComments)
          | _, _, _          =>
            .parseError "'review' is mutually exclusive with 'reply_to_comment_id' and 'path'/'line'"
        else
          match replyToId, path, line with
          | some cid, none, none => .comment (.replyInline body cid)
          | none, some p, some l => .comment (.newInline { path := p, line := l, body, side })
          | none, none, none     => .comment (.issue body)
          | some _, _, _         =>
            .parseError "'reply_to_comment_id' and 'path'/'line' are mutually exclusive"
          | none, some _, none   => .parseError "'path' requires 'line'"
          | none, none, some _   => .parseError "'line' requires 'path'"
  | "get_task_input" => .getTaskInput
  | "submit_task_output" =>
    match args.getObjVal? "value" with
    | .ok v  => .submitTaskOutput v
    | .error _ => .parseError "missing required 'value' argument"
  | _ =>
    match Project.Tools.tryParseToolCall name args with
    | some (.ok call) => .project call
    | some (.error e) => .parseError e
    | none            => .unknown name

/-- Parse a JSON-RPC message into a typed `Request`.
    Returns `none` if the message has no method field. -/
def parseRequest (msg : Json) : Option Request :=
  let id     := msg.getObjVal? "id"     |>.toOption |>.getD .null
  let params := msg.getObjVal? "params" |>.toOption |>.getD (Json.mkObj [])
  match msg.getObjValAs? String "method" |>.toOption with
  | none => none
  | some method => some <| match method with
    | "initialize"                => .initialize id
    | "notifications/initialized" => .initialized
    | "tools/list"                => .toolsList id
    | "tools/call" =>
      let toolName := params.getObjValAs? String "name"      |>.toOption |>.getD ""
      let toolArgs := params.getObjVal?    "arguments"        |>.toOption |>.getD (Json.mkObj [])
      .toolsCall id (parseToolCall toolName toolArgs)
    | _ => .unknown id method

-- Evaluation

/-- Evaluate a parsed tool call against the server's state.

    Not private: the permission gates live here, and a call the task was not granted is refused
    before anything reaches GitHub — which makes the refusals the one part of this worth testing
    without a network. `tools/list` already hides an ungranted tool, but an agent naming it
    anyway must be turned away rather than served. -/
def evalToolCall (state : State) (call : ToolCall) : IO Json := do
  match call with
  | .health =>
    log "tool health"
    return toolContent "ok"
  | .refreshToken =>
    log "tool refresh_token: creating new installation token"
    try
      let jwt ← GitHub.createJWT state.appId state.privateKeyPath
      let token ← GitHub.createInstallationToken jwt state.installationId
      -- Returned to the calling agent only, never written to `~/.config/gh/hosts.yml`.
      -- This tool is reachable from inside any task's sandbox at any moment, so a global
      -- `gh auth login` here would swap the credentials out from under every other task
      -- running in the daemon. The agent uses the value by exporting it as `GH_TOKEN`,
      -- which is also how the sandbox supplied its original token.
      log "tool refresh_token: ok"
      return toolContent token
    catch e =>
      log s!"tool refresh_token: error: {e}"
      return toolContent (toString e) (isError := true)
  | .createPr title body head base target =>
    if !state.allowedTools.contains "create_pr" then
      log "tool create_pr: denied (not in allowed tools)"
      return toolContent "PR creation is not enabled for this task" (isError := true)
    match target with
    | .upstream =>
      if state.pat.isEmpty then
        log "tool create_pr: error: PAT not configured (target=upstream)"
        return toolContent
          "github.pat not set in config (required when target=upstream; pass target=\"fork\" to use the App token)"
          (isError := true)
      log s!"tool create_pr [upstream]: {state.fork}:{head} -> {state.upstream} base={base} title={repr title}"
      try
        let result ← GitHub.createPullRequest state.pat state.upstream
          s!"{state.fork.owner}:{head}" base title body state.prLabels
        log s!"tool create_pr: ok: {result.trimAscii}"
        return toolContent result
      catch e =>
        log s!"tool create_pr: error: {e}"
        return toolContent (toString e) (isError := true)
    | .fork =>
      log s!"tool create_pr [fork]: {state.fork}:{head} base={base} title={repr title}"
      try
        -- Mint a fresh installation token so the PR is attributed to the
        -- GitHub App, not the PAT owner.
        let jwt ← GitHub.createJWT state.appId state.privateKeyPath
        let token ← GitHub.createInstallationToken jwt state.installationId
        let result ← GitHub.createPullRequestOnRepo token state.fork
          head base title body state.prLabels
        log s!"tool create_pr: ok: {result.trimAscii}"
        return toolContent result
      catch e =>
        log s!"tool create_pr: error: {e}"
        return toolContent (toString e) (isError := true)
  | .mergePr prNumber method deleteBranch =>
    if !state.allowedTools.contains "merge_pr" then
      log "tool merge_pr: denied (not in allowed tools)"
      return toolContent "merging pull requests is not enabled for this task" (isError := true)
    if state.pat.isEmpty then
      log "tool merge_pr: error: PAT not configured"
      return toolContent
        "github.pat not set in config (required to merge on the upstream repository)"
        (isError := true)
    log s!"tool merge_pr: {state.upstream}#{prNumber} {method.flag} \
      delete_branch={deleteBranch}"
    try
      let result ← GitHub.mergePullRequest state.pat state.upstream prNumber method deleteBranch
      log s!"tool merge_pr: ok: {result.trimAscii}"
      return toolContent result
    catch e =>
      log s!"tool merge_pr: error: {e}"
      return toolContent (toString e) (isError := true)
  | .labelIssue issueNumber add remove =>
    if !state.allowedTools.contains "label_issue" then
      log "tool label_issue: denied (not in allowed tools)"
      return toolContent "labelling issues is not enabled for this task" (isError := true)
    if state.pat.isEmpty then
      log "tool label_issue: error: PAT not configured"
      return toolContent
        "github.pat not set in config (required to label on the upstream repository)"
        (isError := true)
    log s!"tool label_issue: {state.upstream}#{issueNumber} \
      add=[{String.intercalate ", " add}] remove=[{String.intercalate ", " remove}]"
    try
      let change ← GitHub.setIssueLabels state.pat state.upstream issueNumber add remove
      let summary := change.summary s!"{state.upstream}#{issueNumber}"
      log s!"tool label_issue: ok: {summary}"
      return toolContent summary
    catch e =>
      log s!"tool label_issue: error: {e}"
      return toolContent (toString e) (isError := true)
  | .createRepository name description isPrivate autoInit =>
    if !state.allowedTools.contains "create_repository" then
      log "tool create_repository: denied (not in allowed tools)"
      return toolContent "creating repositories is not enabled for this task" (isError := true)
    match state.defaultOrganization with
    | none =>
      log "tool create_repository: no default_organization configured"
      return toolContent
        "default_organization not set in config (it names the organisation repositories are \
created in; there is no other destination this tool will use)"
        (isError := true)
    | some org =>
      log s!"tool create_repository: {org}/{name} private={isPrivate} auto_init={autoInit}"
      try
        -- The task's own installation token is scoped to the repositories it was launched
        -- against, so creating in the organisation needs the organisation's installation.
        let token ← GitHub.orgInstallationToken state.appId state.privateKeyPath org
          s!"cannot create '{name}' in '{org}'"
        let repo ← GitHub.createRepoInOrg token org name description isPrivate autoInit
        let visibility := if isPrivate then "private" else "public"
        let contents := if autoInit then "initialised with a README commit" else "empty"
        log s!"tool create_repository: ok: {repo}"
        return toolContent s!"created {repo} ({visibility}, {contents})\n\
          https://github.com/{repo}\n\
          Push to it with a token from refresh_token."
      catch e =>
        log s!"tool create_repository: error: {e}"
        return toolContent (toString e) (isError := true)
  | .getPrComments prNumber unresolvedOnly excludeOutdated =>
    log s!"tool get_pr_comments: pr={prNumber} unresolved_only={unresolvedOnly} \
      exclude_outdated={excludeOutdated}"
    try
      let response ← GitHub.getPrReviewThreads state.upstream prNumber state.pat
      let text := GitHub.formatPrReviewThreads response unresolvedOnly excludeOutdated
      log "tool get_pr_comments: ok"
      return toolContent text
    catch e =>
      log s!"tool get_pr_comments: error: {e}"
      return toolContent (toString e) (isError := true)
  | .comment action =>
    if !state.allowedTools.contains "comment" then
      log "tool comment: denied (not in allowed tools)"
      return toolContent "comment tool is not enabled for this task" (isError := true)
    match state.issueNumber with
    | none =>
      log "tool comment: no issue number configured"
      return toolContent "no issue_number configured for this task" (isError := true)
    | some n =>
      match action with
      | .issue body =>
        log s!"tool comment: posting to {state.upstream}#{n}"
        try
          let result ← GitHub.createIssueComment state.pat state.upstream n body
          log "tool comment: ok"
          return toolContent result
        catch e =>
          log s!"tool comment: error: {e}"
          return toolContent (toString e) (isError := true)
      | .review body inlineComments =>
        log s!"tool comment: posting review to {state.upstream}#{n} \
          ({inlineComments.size} inline comments)"
        try
          let result ← GitHub.createPrReview state.pat state.upstream n body inlineComments
          log "tool comment: ok"
          return toolContent result
        catch e =>
          log s!"tool comment: error: {e}"
          return toolContent (toString e) (isError := true)
      | .replyInline body cid =>
        log s!"tool comment: replying to inline comment {cid} on {state.upstream}#{n}"
        try
          let cidPr ← GitHub.getPrReviewCommentPrNumber state.pat state.upstream cid
          if cidPr ≠ n then
            log s!"tool comment: comment {cid} belongs to PR #{cidPr}, not #{n}"
            return toolContent s!"comment {cid} does not belong to issue #{n}" (isError := true)
          let result ← GitHub.replyToPrReviewComment state.pat state.upstream n cid body
          log "tool comment: ok"
          return toolContent result
        catch e =>
          log s!"tool comment: error: {e}"
          return toolContent (toString e) (isError := true)
      | .newInline comment =>
        log s!"tool comment: new inline comment on {state.upstream}#{n} \
          {comment.path}:{comment.line} ({comment.side})"
        try
          let result ← GitHub.createPrReviewComment state.pat state.upstream n
            comment.body comment.path comment.line comment.side
          log "tool comment: ok"
          return toolContent result
        catch e =>
          log s!"tool comment: error: {e}"
          return toolContent (toString e) (isError := true)
  | .getTaskInput =>
    log "tool get_task_input"
    match state.inputJson with
    | some j => return toolContent j.compress
    | none   => return toolContent "no input available" (isError := true)
  | .submitTaskOutput value =>
    log s!"tool submit_task_output: {value.compress.take 200}"
    match state.outputRef with
    | some ref =>
      ref.set (some value)
      return toolContent "output recorded"
    | none =>
      return toolContent "output submission not available for this task" (isError := true)
  | .project call =>
    let env : Project.Tools.Env :=
      { claimManager  := state.claimManager
      , allowedTools  := state.allowedTools
      , taskId        := state.taskId
      , agentBackend  := state.agentBackend
      , series        := state.series
      , projectId     := state.projectId
      , issueId       := state.issueId
      , enqueueMerger   := state.enqueueMerger
      , enqueueReviewer := state.enqueueReviewer }
    Project.Tools.evalProjectTool env call
  | .unknown name =>
    log s!"tool {name}: unknown"
    return toolContent s!"unknown tool: {name}" (isError := true)
  | .parseError msg =>
    log s!"tool call error: {msg}"
    return toolContent msg (isError := true)

/-- Evaluate a parsed JSON-RPC request. Returns `some` response, or `none` for notifications. -/
private def evalRequest (state : State) (req : Request) : IO (Option Json) := do
  match req with
  | .initialize id =>
    log "initialize"
    return some (jsonrpcResult id initializeResult)
  | .initialized =>
    log "initialized"
    return none
  | .toolsList id =>
    log "tools/list"
    return some (jsonrpcResult id (toolsList state))
  | .toolsCall id call =>
    let result ← evalToolCall state call
    return some (jsonrpcResult id result)
  | .unknown id method =>
    log s!"unknown method: {method}"
    return some (jsonrpcError id (-32601) s!"method not found: {method}")

-- TCP transport (raw JSON-RPC, newline-delimited)

private def awaitTcp (p : IO.Promise (Except IO.Error α)) : IO α := do
  let result ← IO.wait p.result!
  match result with
  | .error e => throw e
  | .ok v => return v

/--
Handle one TCP client connection as a JSON-RPC session.
Reads newline-delimited JSON messages, dispatches them, and writes responses.
A line buffer handles the case where a single TCP receive spans multiple messages
or a message is split across multiple receives.
-/
private def handleClient (state : State) (client : Socket) : IO Unit := do
  let buf ← IO.mkRef ""
  repeat do
    let data? ← awaitTcp (← client.recv? 65536)
    match data? with
    | none => return
    | some bytes =>
      buf.modify (· ++ String.fromUTF8! bytes)
      let lines := (← buf.get).splitOn "\n"
      -- All elements except the last are complete lines; the last may be partial.
      buf.set (lines.getLast?.getD "")
      for line in lines.dropLast do
        let trimmed := line.trimAscii.toString
        if trimmed.isEmpty then continue
        match Json.parse trimmed with
        | .error _ => pure ()
        | .ok msg =>
          match parseRequest msg with
          | none => pure ()
          | some req =>
            match ← evalRequest state req with
            | none => pure ()
            | some response =>
              let _ ← awaitTcp (← client.send #[(response.compress ++ "\n").toUTF8])

/-- Start the MCP server. Returns (port, shutdown action). -/
def start (state : State) : IO (UInt16 × IO Unit) := do
  let server ← Socket.new
  let addr := SocketAddress.v4 { addr := IPv4Addr.ofParts 127 0 0 1, port := 0 }
  server.bind addr
  server.listen 8
  let localAddr ← server.getSockName
  let port := match localAddr with
    | .v4 a => a.port | .v6 a => a.port
  let running ← IO.mkRef true
  let _acceptTask ← IO.asTask (prio := .dedicated) do
    while ← running.get do
      match ← IO.wait (← server.accept).result! with
      | .error _ => break
      | .ok client =>
        if !(← running.get) then break
        let _ ← IO.asTask (prio := .dedicated) do
          log "client connected"
          try handleClient state client
          catch _ => pure ()
          log "client disconnected"
  let shutdown : IO Unit := do
    running.set false
    try
      let dummy ← Socket.new
      let addr := SocketAddress.v4 { addr := IPv4Addr.ofParts 127 0 0 1, port }
      let _ ← dummy.connect addr
    catch _ => pure ()
  return (port, shutdown)

end Orchestra.Server
