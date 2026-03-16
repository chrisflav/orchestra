import Lean.Data.Json
import Std.Internal.UV.TCP
import Std.Net
import Cli

open Lean (Json)
open Std.Net
open Std.Internal.UV.TCP
open Cli

-- MCP client over raw TCP

private def awaitTcp (p : IO.Promise (Except IO.Error α)) : IO α := do
  let result ← IO.wait p.result!
  match result with
  | .error e => throw e
  | .ok v => return v

structure McpClient where
  sock   : Socket
  buf    : IO.Ref String
  nextId : IO.Ref Nat

private def McpClient.connect (port : UInt16) : IO McpClient := do
  let sock ← Socket.new
  let addr := SocketAddress.v4 { addr := IPv4Addr.ofParts 127 0 0 1, port }
  let _ ← sock.connect addr
  return { sock, buf := ← IO.mkRef "", nextId := ← IO.mkRef 0 }

/-- Send a JSON-RPC message (newline-delimited). -/
private def McpClient.send (c : McpClient) (msg : Json) : IO Unit := do
  let _ ← awaitTcp (← c.sock.send #[(msg.compress ++ "\n").toUTF8])

/--
Read the next complete JSON-RPC message from the server.
Buffers partial TCP reads and splits on newlines.
-/
private def McpClient.recv (c : McpClient) : IO (Option Json) := do
  let found ← IO.mkRef (none : Option Json)
  let done ← IO.mkRef false
  while !(← done.get) do
    match (← c.buf.get).splitOn "\n" with
    | line :: rest@(_ :: _) =>
      c.buf.set (String.intercalate "\n" rest)
      let trimmed := line.trimAscii.toString
      if !trimmed.isEmpty then
        match Json.parse trimmed with
        | .ok j => found.set (some j); done.set true
        | .error _ => pure ()
    | _ =>
      match ← awaitTcp (← c.sock.recv? 65536) with
      | none => done.set true
      | some bytes => c.buf.modify (· ++ String.fromUTF8! bytes)
  return (← found.get)

/-- Send a request and return the matching response, skipping any notifications. -/
private def McpClient.request (c : McpClient) (method : String) (params : Json) : IO Json := do
  let id ← c.nextId.get
  c.nextId.modify (· + 1)
  c.send <| Json.mkObj [
    ("jsonrpc", "2.0"),
    ("id", .num ⟨id, 0⟩),
    ("method", .str method),
    ("params", params)
  ]
  let result ← IO.mkRef (Json.mkObj [])
  let done ← IO.mkRef false
  while !(← done.get) do
    match ← c.recv with
    | none => throw (.userError "connection closed waiting for response")
    | some resp =>
      match resp.getObjVal? "id" |>.toOption with
      | none => pure ()  -- notification, skip
      | some respId =>
        if respId == .num ⟨id, 0⟩ then
          result.set resp
          done.set true
  return (← result.get)

/-- Send a notification (no response expected). -/
private def McpClient.notify (c : McpClient) (method : String) (params : Json) : IO Unit :=
  c.send <| Json.mkObj [
    ("jsonrpc", "2.0"),
    ("method", .str method),
    ("params", params)
  ]

/-- Perform the MCP initialization handshake. -/
private def McpClient.handshake (c : McpClient) : IO Unit := do
  let _ ← c.request "initialize" <| Json.mkObj [
    ("protocolVersion", "2024-11-05"),
    ("capabilities", Json.mkObj []),
    ("clientInfo", Json.mkObj [("name", "agent-mcp-test"), ("version", "0.1.0")])
  ]
  c.notify "notifications/initialized" (Json.mkObj [])

/-- Call a tool and return the full JSON-RPC response. -/
private def McpClient.callTool (c : McpClient) (name : String) (args : Json) : IO Json :=
  c.request "tools/call" <| Json.mkObj [("name", .str name), ("arguments", args)]

-- Helpers

/-- Print the tool result: extract content[*].text if present, else pretty-print. -/
private def printResult (resp : Json) : IO Unit := do
  match resp.getObjVal? "result" |>.toOption with
  | none => IO.println resp.pretty
  | some result =>
    let contentItems := result.getObjVal? "content" |>.toOption
      |>.bind (·.getArr? |>.toOption)
    match contentItems with
    | none => IO.println result.pretty
    | some items =>
      for item in items do
        match item.getObjValAs? String "text" |>.toOption with
        | some text => IO.println text
        | none => IO.println item.pretty

-- Handler

private def handler (p : Parsed) : IO UInt32 := do
  let some portFlag := p.flag? "port"
    | throw (.userError "missing required flag: --port")
  let port  := (portFlag.as! Nat).toUInt16
  let tool  := p.positionalArg! "tool" |>.as! String
  let client ← McpClient.connect port
  client.handshake
  match tool with
  | "health" =>
    printResult (← client.callTool "health" (Json.mkObj []))
  | "refresh-token" =>
    printResult (← client.callTool "refresh_token" (Json.mkObj []))
  | "create-pr" =>
    let some headFlag := p.flag? "head"
      | throw (.userError "missing required flag: --head")
    let head  := headFlag.as! String
    let base  := p.flag? "base"  |>.map (·.as! String) |>.getD "main"
    let title := p.flag? "title" |>.map (·.as! String) |>.getD "Agent PR"
    let body  := p.flag? "body"  |>.map (·.as! String) |>.getD ""
    let args := Json.mkObj [
      ("head",  .str head),
      ("base",  .str base),
      ("title", .str title),
      ("body",  .str body)
    ]
    printResult (← client.callTool "create_pr" args)
  | "get-pr-comments" =>
    let some prFlag := p.flag? "pr_number"
      | throw (.userError "missing required flag: --pr_number")
    let prNumber := prFlag.as! Nat
    let args := Json.mkObj [
      ("pr_number",       .num ⟨(prNumber : Int), 0⟩),
      ("unresolved_only", p.hasFlag "unresolved_only"),
      ("exclude_outdated", p.hasFlag "exclude_outdated")
    ]
    printResult (← client.callTool "get_pr_comments" args)
  | _ =>
    throw (.userError
      s!"unknown tool '{tool}'; expected: health, refresh-token, create-pr, get-pr-comments")
  return 0

-- CLI definition

def mcpTestCmd : Cmd := `[Cli|
  «agent-mcp-test» VIA handler; ["0.1.0"]
  "Test client for the agent MCP server."

  FLAGS:
    p, port : Nat; "MCP server port"
    head : String; "Branch name in the fork (for create-pr)"
    base : String; "Base branch on the upstream (for create-pr, default: main)"
    title : String; "Pull request title (for create-pr, default: \"Agent PR\")"
    body : String; "Pull request body (for create-pr)"
    pr_number : Nat; "Pull request number (for get-pr-comments)"
    unresolved_only; "Only show unresolved conversations (for get-pr-comments)"
    exclude_outdated; "Exclude outdated comments (for get-pr-comments)"

  ARGS:
    "tool" : String; "Tool to call: health | refresh-token | create-pr"
]

def main (args : List String) : IO UInt32 :=
  mcpTestCmd.validate args
