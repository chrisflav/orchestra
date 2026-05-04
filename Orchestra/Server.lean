import Lean.Data.Json
import Std.Internal.UV.TCP
import Std.Net
import Orchestra.Config
import Orchestra.GitHub

open Lean (Json)
open Std.Net
open Std.Internal.UV.TCP

namespace Orchestra.Server

/-- Mutable state for the server, shared with request handlers. -/
structure State where
  upstream : String
  fork : String
  /-- Optional tools enabled for this run.
      Always-available tools (health, refresh_token, get_pr_comments) are never in this list.
      Currently the only optional tool is `"create_pr"`. -/
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
  /-- Email address for the `report` tool. -/
  email : Option String := none

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
    ("description", "Refresh the GitHub App installation token and reconfigure the gh CLI."),
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
    ("description", "Create a pull request on the upstream repository."),
    ("inputSchema", Json.mkObj [
      ("type", "object"),
      ("properties", Json.mkObj [
        ("title", Json.mkObj [("type", "string")]),
        ("body", Json.mkObj [("type", "string")]),
        ("head", Json.mkObj [
          ("type", "string"),
          ("description", "Branch name in the fork.")
        ]),
        ("base", Json.mkObj [("type", "string")])
      ]),
      ("required", .arr #["head"])
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
  ("report", Json.mkObj [
    ("name", "report"),
    ("description", "Send a private report via email to the configured address."),
    ("inputSchema", Json.mkObj [
      ("type", "object"),
      ("properties", Json.mkObj [
        ("subject", Json.mkObj [
          ("type", "string"),
          ("description", "Email subject line.")
        ]),
        ("body", Json.mkObj [
          ("type", "string"),
          ("description", "Email body text.")
        ])
      ]),
      ("required", .arr #["subject", "body"])
    ])
  ])
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

private def toolsList (state : State) : Json :=
  let optional := optionalToolDefs.filterMap fun entry =>
    if state.allowedTools.contains entry.1 then some entry.2 else none
  let io := ioToolDefs state.inputType state.outputType
  Json.mkObj [("tools", .arr (alwaysAvailableTools ++ optional.toArray ++ io))]

-- Types

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
  | createPr (title : String) (body : String) (head : String) (base : String)
  | getPrComments (prNumber : Nat) (unresolvedOnly : Bool) (excludeOutdated : Bool)
  | comment (action : CommentAction)
  | report (subject : String) (body : String)
  | getTaskInput
  | submitTaskOutput (value : Json)
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

private def parseToolCall (name : String) (args : Json) : ToolCall :=
  match name with
  | "health" => .health
  | "refresh_token" => .refreshToken
  | "create_pr" =>
    let title := args.getObjValAs? String "title" |>.toOption |>.getD "Agent PR"
    let body  := args.getObjValAs? String "body"  |>.toOption |>.getD ""
    let head  := args.getObjValAs? String "head"  |>.toOption |>.getD ""
    let base  := args.getObjValAs? String "base"  |>.toOption |>.getD "main"
    if head.isEmpty then .parseError "missing 'head' (branch name)"
    else .createPr title body head base
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
  | "report" =>
    let subject := args.getObjValAs? String "subject" |>.toOption |>.getD ""
    let body    := args.getObjValAs? String "body"    |>.toOption |>.getD ""
    if subject.isEmpty then .parseError "missing required 'subject' argument"
    else if body.isEmpty then .parseError "missing required 'body' argument"
    else .report subject body
  | "get_task_input" => .getTaskInput
  | "submit_task_output" =>
    match args.getObjVal? "value" with
    | .ok v  => .submitTaskOutput v
    | .error _ => .parseError "missing required 'value' argument"
  | _ => .unknown name

/-- Parse a JSON-RPC message into a typed `Request`.
    Returns `none` if the message has no method field. -/
private def parseRequest (msg : Json) : Option Request :=
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

private def evalToolCall (state : State) (call : ToolCall) : IO Json := do
  match call with
  | .health =>
    log "tool health"
    return toolContent "ok"
  | .refreshToken =>
    log "tool refresh_token: creating new installation token"
    try
      let jwt ← GitHub.createJWT state.appId state.privateKeyPath
      let token ← GitHub.createInstallationToken jwt state.installationId
      GitHub.setupGhAuth token
      log "tool refresh_token: ok"
      return toolContent token
    catch e =>
      log s!"tool refresh_token: error: {e}"
      return toolContent (toString e) (isError := true)
  | .createPr title body head base =>
    if !state.allowedTools.contains "create_pr" then
      log "tool create_pr: denied (not in allowed tools)"
      return toolContent "PR creation is not enabled for this task" (isError := true)
    if state.pat.isEmpty then
      log "tool create_pr: error: PAT not configured"
      return toolContent "github.pat not set in config" (isError := true)
    log s!"tool create_pr: {state.fork}:{head} -> {state.upstream} base={base} title={repr title}"
    let forkOwner := (state.fork.splitOn "/")[0]!
    try
      let result ← GitHub.createPullRequest state.pat state.upstream
        s!"{forkOwner}:{head}" base title body
      log s!"tool create_pr: ok: {result.trimAscii}"
      return toolContent result
    catch e =>
      log s!"tool create_pr: error: {e}"
      return toolContent (toString e) (isError := true)
  | .getPrComments prNumber unresolvedOnly excludeOutdated =>
    log s!"tool get_pr_comments: pr={prNumber} unresolved_only={unresolvedOnly} \
      exclude_outdated={excludeOutdated}"
    try
      let response ← GitHub.getPrReviewThreads state.upstream prNumber state.pat
      let threads :=
        response.getObjVal? "data"          |>.toOption
        |>.bind (·.getObjVal? "repository"  |>.toOption)
        |>.bind (·.getObjVal? "pullRequest" |>.toOption)
        |>.bind (·.getObjVal? "reviewThreads" |>.toOption)
        |>.bind (·.getObjVal? "nodes"       |>.toOption)
        |>.bind (·.getArr?                  |>.toOption)
        |>.getD #[]
      let mut lines : Array String := #[]
      let mut shown := 0
      let mut filtered := 0
      for thread in threads do
        let isResolved := thread.getObjValAs? Bool "isResolved" |>.toOption |>.getD false
        let isOutdated := thread.getObjValAs? Bool "isOutdated" |>.toOption |>.getD false
        if unresolvedOnly && isResolved then filtered := filtered + 1; continue
        if excludeOutdated && isOutdated then filtered := filtered + 1; continue
        shown := shown + 1
        let mut tags : Array String := #[]
        if !isResolved then tags := tags.push "unresolved"
        if isResolved  then tags := tags.push "resolved"
        if isOutdated  then tags := tags.push "outdated"
        let tagStr :=
          if tags.isEmpty then ""
          else s!" ({String.join (tags.toList.intersperse ", ")})"
        lines := lines.push s!"--- Thread {shown}{tagStr}"
        let comments :=
          thread.getObjVal? "comments" |>.toOption
          |>.bind (·.getObjVal? "nodes" |>.toOption)
          |>.bind (·.getArr?            |>.toOption)
          |>.getD #[]
        for comment in comments do
          let path   := comment.getObjValAs? String "path" |>.toOption |>.getD ""
          let body   := comment.getObjValAs? String "body" |>.toOption |>.getD ""
          let author := comment.getObjVal? "author" |>.toOption
            |>.bind (·.getObjValAs? String "login" |>.toOption)
            |>.getD "unknown"
          let lineStr := match comment.getObjValAs? Nat "line" |>.toOption with
            | some l => s!":{l}"
            | none   => ""
          lines := lines.push s!"  @{author} — {path}{lineStr}"
          for bodyLine in body.splitOn "\n" do
            lines := lines.push s!"    {bodyLine}"
        lines := lines.push ""
      let summary :=
        if filtered == 0 then s!"({shown} thread(s))"
        else s!"({shown} thread(s) shown, {filtered} filtered)"
      lines := lines.push summary
      log s!"tool get_pr_comments: ok: {shown} threads shown"
      return toolContent (String.join (lines.toList.intersperse "\n"))
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
          let result ← GitHub.replyToPrReviewComment state.pat state.upstream cid body
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
  | .report subject body =>
    if !state.allowedTools.contains "report" then
      log "tool report: denied (not in allowed tools)"
      return toolContent "report tool is not enabled for this task" (isError := true)
    match state.email with
    | none =>
      log "tool report: no email configured"
      return toolContent "no email address configured (set 'email' in config)" (isError := true)
    | some addr =>
      log s!"tool report: sending to {addr}, subject={repr subject}"
      try
        GitHub.sendEmail addr subject body
        log s!"tool report: ok"
        return toolContent s!"Report sent to {addr}"
      catch e =>
        log s!"tool report: error: {e}"
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
