import Lean.Data.Json
import Std.Internal.UV.TCP
import Std.Net
import Agent.GitHub

open Lean (Json)
open Std.Net
open Std.Internal.UV.TCP

namespace Agent.Server

/-- Mutable state for the server, shared with request handlers. -/
structure State where
  upstream : String
  fork : String
  allowPR : Bool
  appId : Nat
  privateKeyPath : String
  installationId : Nat
  pat : String

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

private def toolsList : Json :=
  Json.mkObj [("tools", .arr #[
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
    ]
    ,
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
  ])]

private def callTool (state : State) (name : String) (args : Json) : IO Json := do
  match name with
  | "health" =>
    log "tool health"
    return toolContent "ok"
  | "refresh_token" =>
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
  | "create_pr" =>
    if !state.allowPR then
      log "tool create_pr: denied (fork-only mode)"
      return toolContent "PR creation not allowed in fork-only mode" (isError := true)
    let title := args.getObjValAs? String "title" |>.toOption |>.getD "Agent PR"
    let body := args.getObjValAs? String "body" |>.toOption |>.getD ""
    let head := args.getObjValAs? String "head" |>.toOption |>.getD ""
    let base := args.getObjValAs? String "base" |>.toOption |>.getD "main"
    if head.isEmpty then
      log "tool create_pr: error: missing 'head'"
      return toolContent "missing 'head' (branch name)" (isError := true)
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
  | "get_pr_comments" =>
    let some prNumJson := args.getObjVal? "pr_number" |>.toOption
      | return toolContent "missing required argument: pr_number" (isError := true)
    let some prNumInt := prNumJson.getInt? |>.toOption
      | return toolContent "pr_number must be an integer" (isError := true)
    if prNumInt <= 0 then
      return toolContent "pr_number must be a positive integer" (isError := true)
    let prNumber := prNumInt.toNat
    let unresolvedOnly := args.getObjValAs? Bool "unresolved_only" |>.toOption |>.getD false
    let excludeOutdated := args.getObjValAs? Bool "exclude_outdated" |>.toOption |>.getD false
    log s!"tool get_pr_comments: pr={prNumber} unresolved_only={unresolvedOnly} \
      exclude_outdated={excludeOutdated}"
    try
      let response ← GitHub.getPrReviewThreads state.upstream prNumber state.pat
      let threads :=
        response.getObjVal? "data" |>.toOption
        |>.bind (·.getObjVal? "repository" |>.toOption)
        |>.bind (·.getObjVal? "pullRequest" |>.toOption)
        |>.bind (·.getObjVal? "reviewThreads" |>.toOption)
        |>.bind (·.getObjVal? "nodes" |>.toOption)
        |>.bind (·.getArr? |>.toOption)
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
        if isResolved then tags := tags.push "resolved"
        if isOutdated then tags := tags.push "outdated"
        let tagStr := if tags.isEmpty then "" else s!" ({String.join (tags.toList.intersperse ", ")})"
        lines := lines.push s!"--- Thread {shown}{tagStr}"
        let comments :=
          thread.getObjVal? "comments" |>.toOption
          |>.bind (·.getObjVal? "nodes" |>.toOption)
          |>.bind (·.getArr? |>.toOption)
          |>.getD #[]
        for comment in comments do
          let path   := comment.getObjValAs? String "path" |>.toOption |>.getD ""
          let body   := comment.getObjValAs? String "body" |>.toOption |>.getD ""
          let author := comment.getObjVal? "author" |>.toOption
            |>.bind (·.getObjValAs? String "login" |>.toOption)
            |>.getD "unknown"
          let lineStr := match comment.getObjValAs? Nat "line" |>.toOption with
            | some l => s!":{l}"
            | none => ""
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
  | _ =>
    log s!"tool {name}: unknown"
    return toolContent s!"unknown tool: {name}" (isError := true)

/-- Dispatch a JSON-RPC message. Returns Some response, or None for notifications. -/
private def handleMessage (state : State) (msg : Json) : IO (Option Json) := do
  let id := msg.getObjVal? "id" |>.toOption |>.getD .null
  let some method := msg.getObjValAs? String "method" |>.toOption | return none
  let params := msg.getObjVal? "params" |>.toOption |>.getD (Json.mkObj [])
  match method with
  | "initialize" =>
    log "initialize"
    return some (jsonrpcResult id initializeResult)
  | "notifications/initialized" =>
    log "initialized"
    return none
  | "tools/list" =>
    log "tools/list"
    return some (jsonrpcResult id toolsList)
  | "tools/call" =>
    let toolName := params.getObjValAs? String "name" |>.toOption |>.getD ""
    let toolArgs := params.getObjVal? "arguments" |>.toOption |>.getD (Json.mkObj [])
    let result ← callTool state toolName toolArgs
    return some (jsonrpcResult id result)
  | _ =>
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
          match ← handleMessage state msg with
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

end Agent.Server
