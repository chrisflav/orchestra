import Lean.Data.Json
import Std.Internal.UV.TCP
import Std.Net
import Orchestra.Config
import Orchestra.Dirs
import Orchestra.Queue
import Orchestra.TaskStore
import Orchestra.Listener

open Lean (Json ToJson)
open Std.Net
open Std.Internal.UV.TCP

/-!
# Orchestra Web Dashboard

A minimal HTTP/1.1 server that serves a password-protected web dashboard showing
the live state of the queue, configured listeners, and task history with log output.

The HTML is generated directly from Lean string templates. Future work: migrate the
static parts of the site to a verso-based generator
(https://github.com/leanprover/verso) for richer markup and cross-linking.
-/

namespace Orchestra.Dashboard

-- TCP helper (mirrors the one in Orchestra.Server)

private def awaitTcp (p : IO.Promise (Except IO.Error α)) : IO α := do
  let result ← IO.wait p.result!
  match result with
  | .error e => throw e
  | .ok v    => return v

-- HTTP types

private structure HttpRequest where
  method  : String
  path    : String
  headers : Array (String × String)
  body    : String

private structure HttpResponse where
  status  : Nat
  headers : Array (String × String)
  body    : String

private def statusText : Nat → String
  | 200 => "OK"
  | 301 => "Moved Permanently"
  | 400 => "Bad Request"
  | 401 => "Unauthorized"
  | 404 => "Not Found"
  | n   => s!"Status {n}"

private def httpResponse (resp : HttpResponse) : String :=
  let statusLine := s!"HTTP/1.1 {resp.status} {statusText resp.status}\r\n"
  let contentLen := s!"Content-Length: {resp.body.utf8ByteSize}\r\n"
  let hdrs := resp.headers.map (fun (k, v) => s!"{k}: {v}\r\n") |>.toList
  statusLine ++ String.join hdrs ++ contentLen ++ "Connection: close\r\n\r\n" ++ resp.body

private def htmlResp (body : String) (status : Nat := 200) : HttpResponse :=
  { status, headers := #[("Content-Type", "text/html; charset=utf-8")], body }

private def notFound (msg : String) : HttpResponse :=
  htmlResp ("<html><body><h1>Not Found</h1><p>" ++ msg ++ "</p></body></html>") 404

private def unauthorizedResp : HttpResponse := {
  status  := 401
  headers := #[("Content-Type", "text/html; charset=utf-8"),
               ("WWW-Authenticate", "Basic realm=\"Orchestra Dashboard\"")]
  body    := "<html><body><h1>401 Unauthorized</h1></body></html>"
}

-- Base64 decode

private def b64Val (c : Char) : Option UInt8 :=
  let n := c.toNat
  if 'A'.toNat ≤ n && n ≤ 'Z'.toNat then some (n - 'A'.toNat).toUInt8
  else if 'a'.toNat ≤ n && n ≤ 'z'.toNat then some (n - 'a'.toNat + 26).toUInt8
  else if '0'.toNat ≤ n && n ≤ '9'.toNat then some (n - '0'.toNat + 52).toUInt8
  else if c == '+' then some 62
  else if c == '/' then some 63
  else none

private partial def decodeBase64 (s : String) : Option String := do
  let chars := s.toList.filter (· != '=')
  let rec go : List Char → List UInt8 → Option (List UInt8)
    | [], acc => some acc
    | [a], acc => do
        let va ← b64Val a
        some (acc ++ [va <<< 2])
    | [a, b], acc => do
        let va ← b64Val a; let vb ← b64Val b
        some (acc ++ [(va <<< 2) ||| (vb >>> 4)])
    | [a, b, c], acc => do
        let va ← b64Val a; let vb ← b64Val b; let vc ← b64Val c
        some (acc ++ [(va <<< 2) ||| (vb >>> 4), (vb <<< 4) ||| (vc >>> 2)])
    | a :: b :: c :: d :: rest, acc => do
        let va ← b64Val a; let vb ← b64Val b; let vc ← b64Val c; let vd ← b64Val d
        go rest (acc ++ [(va <<< 2) ||| (vb >>> 4),
                         (vb <<< 4) ||| (vc >>> 2),
                         (vc <<< 6) ||| vd])
  let bytes ← go chars []
  some (String.fromUTF8! ⟨bytes.toArray⟩)

private def checkBasicAuth (hdrs : Array (String × String)) (password : String) : Bool :=
  match hdrs.find? (fun (k, _) => k.toLower == "authorization") with
  | none => false
  | some (_, v) =>
    if !v.startsWith "Basic " then false
    else match decodeBase64 ((v.drop 6).toString) with
      | none => false
      | some decoded =>
        match decoded.splitOn ":" with
        | _ :: rest => String.intercalate ":" rest == password
        | _ => false

-- HTML helpers

private def esc (s : String) : String :=
  s.foldl (fun acc c => acc ++ match c with
    | '&'  => "&amp;" | '<' => "&lt;" | '>' => "&gt;"
    | '"'  => "&quot;" | '\'' => "&#39;"
    | c    => String.ofList [c]) ""

private def badge (s : String) : String :=
  let cls := match s with
    | "running"              => "r"
    | "pending"              => "p"
    | "done" | "completed"  => "d"
    | "failed"               => "f"
    | "cancelled"            => "c"
    | _                      => "o"
  s!"<span class=\"b b{cls}\">{esc s}</span>"

private def shell (title content : String) : String :=
  "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">" ++
  "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">" ++
  s!"<title>{esc title} — Orchestra</title>" ++
  "<style>" ++
  ":root{--bg:#0f1117;--sf:#1a1d24;--bd:#2a2d35;--tx:#e2e4e9;--mu:#8b8fa8;--ac:#5b8def}" ++
  "*{box-sizing:border-box;margin:0;padding:0}" ++
  "body{font-family:ui-monospace,monospace;background:var(--bg);color:var(--tx);font-size:14px;line-height:1.6}" ++
  "a{color:var(--ac);text-decoration:none}a:hover{text-decoration:underline}" ++
  "header{background:var(--sf);border-bottom:1px solid var(--bd);padding:12px 24px;display:flex;align-items:center;gap:24px}" ++
  "header .logo{font-weight:700;font-size:16px}" ++
  "nav{display:flex;gap:16px}nav a{color:var(--mu);font-size:13px}nav a:hover{color:var(--tx)}" ++
  "main{padding:24px;max-width:1200px;margin:0 auto}" ++
  "h1{font-size:18px;font-weight:600;margin-bottom:16px}" ++
  "h2{font-size:12px;font-weight:600;margin:24px 0 10px;color:var(--mu);text-transform:uppercase;letter-spacing:.06em}" ++
  ".card{background:var(--sf);border:1px solid var(--bd);border-radius:6px;overflow:hidden;margin-bottom:16px}" ++
  "table{width:100%;border-collapse:collapse}" ++
  "th{text-align:left;padding:8px 12px;font-size:11px;text-transform:uppercase;letter-spacing:.05em;color:var(--mu);border-bottom:1px solid var(--bd);font-weight:500}" ++
  "td{padding:8px 12px;border-bottom:1px solid var(--bd);vertical-align:top}tr:last-child td{border-bottom:none}" ++
  "tr:hover td{background:rgba(255,255,255,.02)}" ++
  ".b{display:inline-block;padding:2px 8px;border-radius:12px;font-size:11px;font-weight:600;text-transform:uppercase;letter-spacing:.03em}" ++
  ".br{background:rgba(58,154,92,.2);color:#6dcf96}.bp{background:rgba(232,168,56,.2);color:#f0c060}" ++
  ".bd{background:rgba(58,154,92,.15);color:#5abf80}.bf{background:rgba(232,80,80,.2);color:#f07070}" ++
  ".bc{background:rgba(139,143,168,.2);color:var(--mu)}.bo{background:rgba(91,141,239,.2);color:#7baaff}" ++
  ".m{font-size:12px;color:var(--mu)}.trunc{max-width:260px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}" ++
  ".pre{background:#0a0c10;padding:16px;font-size:12px;white-space:pre-wrap;word-break:break-all;max-height:600px;overflow-y:auto}" ++
  ".empty{padding:32px;text-align:center;color:var(--mu)}" ++
  ".note{font-size:11px;color:var(--mu);margin-top:8px}" ++
  ".stats{display:grid;grid-template-columns:repeat(auto-fit,minmax(120px,1fr));gap:12px;margin-bottom:24px}" ++
  ".st{background:var(--sf);border:1px solid var(--bd);border-radius:6px;padding:16px}" ++
  ".sv{font-size:26px;font-weight:700}.sl{font-size:11px;color:var(--mu);text-transform:uppercase;letter-spacing:.05em;margin-top:2px}" ++
  "</style></head><body>" ++
  "<header><span class=\"logo\">Orchestra</span><nav>" ++
  "<a href=\"/\">Overview</a><a href=\"/queue\">Queue</a>" ++
  "<a href=\"/listeners\">Listeners</a><a href=\"/tasks\">Tasks</a>" ++
  "</nav></header>" ++
  s!"<main>{content}</main>" ++
  "<script>setTimeout(()=>location.reload(),15000)</script>" ++
  "</body></html>"

-- Page generators

private def qStText : Queue.QueueStatus → String
  | .pending    => "pending"   | .running    => "running"
  | .done       => "done"      | .failed     => "failed"
  | .unfinished => "unfinished"| .cancelled  => "cancelled"

private def tStText : TaskStore.TaskStatus → String
  | .running    => "running"   | .completed  => "completed"
  | .failed     => "failed"    | .unfinished => "unfinished"
  | .cancelled  => "cancelled"

private def statBox (val : Nat) (lbl : String) : String :=
  s!"<div class=\"st\"><div class=\"sv\">{val}</div><div class=\"sl\">{lbl}</div></div>"

private def overviewPage : IO String := do
  let entries  ← Queue.loadAllEntries
  let tasks    ← TaskStore.loadAllTasks
  let lsCfgs   ← Listener.loadAllListenerConfigs
  let concerts ← Queue.loadAllConcertRuns
  let running := entries.filter (·.status == .running)
  let pending := entries.filter (·.status == .pending)
  let failed  := entries.filter (·.status == .failed)
  let rConcerts := concerts.filter (·.status == .running)
  let stats :=
    "<div class=\"stats\">" ++
    statBox running.size "Running" ++ statBox pending.size "Pending" ++
    statBox failed.size  "Failed"  ++ statBox rConcerts.size "Concerts" ++
    statBox lsCfgs.size  "Listeners" ++ statBox tasks.size "Total Tasks" ++
    "</div>"
  let active := running ++ pending
  let activeRows :=
    if active.isEmpty then
      "<tr><td colspan=\"6\" class=\"empty\">No active tasks</td></tr>"
    else
      String.join (active.toList.map fun e =>
        "<tr><td class=\"m\"><a href=\"/tasks/" ++ e.id ++ "\">" ++ esc e.id ++ "</a></td>" ++
        "<td>" ++ badge (qStText e.status) ++ "</td>" ++
        "<td class=\"m trunc\">" ++ esc e.fork.toString ++ "</td>" ++
        "<td class=\"m\">" ++ esc e.createdAt ++ "</td>" ++
        "<td>" ++ toString e.priority ++ "</td>" ++
        "<td class=\"m trunc\">" ++ esc (e.concertId.getD "") ++ "</td></tr>")
  let qTable :=
    "<div class=\"card\"><table><thead><tr>" ++
    "<th>ID</th><th>Status</th><th>Fork</th><th>Created</th><th>Pri</th><th>Concert</th>" ++
    "</tr></thead><tbody>" ++ activeRows ++ "</tbody></table></div>"
  let recentTasks := tasks.toList.take 10
  let recentRows :=
    if recentTasks.isEmpty then
      "<tr><td colspan=\"5\" class=\"empty\">No tasks yet</td></tr>"
    else
      String.join (recentTasks.map fun r =>
        let snip := (r.prompt.take 60).toString
        "<tr><td class=\"m\"><a href=\"/tasks/" ++ r.id ++ "\">" ++ esc r.id ++ "</a></td>" ++
        "<td>" ++ badge (tStText r.status) ++ "</td>" ++
        "<td class=\"m trunc\">" ++ esc r.fork.toString ++ "</td>" ++
        "<td class=\"m\">" ++ esc r.createdAt ++ "</td>" ++
        "<td class=\"trunc\">" ++ esc snip ++ "</td></tr>")
  let tTable :=
    "<div class=\"card\"><table><thead><tr>" ++
    "<th>ID</th><th>Status</th><th>Fork</th><th>Created</th><th>Prompt</th>" ++
    "</tr></thead><tbody>" ++ recentRows ++ "</tbody></table></div>"
  return shell "Overview"
    (stats ++ "<h2>Active Queue</h2>" ++ qTable ++
     "<h2>Recent Tasks</h2>" ++ tTable ++
     "<p class=\"note\">Refreshes every 15 s</p>")

private def queuePage : IO String := do
  let entries  ← Queue.loadAllEntries
  let concerts ← Queue.loadAllConcertRuns
  let cRows :=
    if concerts.isEmpty then "<tr><td colspan=\"5\" class=\"empty\">No concerts</td></tr>"
    else String.join (concerts.toList.map fun r =>
      let st := match r.status with
        | .running => "running" | .done => "done" | .failed => "failed" | .cancelled => "cancelled"
      "<tr><td class=\"m\">" ++ esc r.id ++ "</td>" ++
      "<td>" ++ badge st ++ "</td>" ++
      "<td class=\"m\">" ++ esc r.startedAt ++ "</td>" ++
      "<td class=\"m\">" ++ esc (r.finishedAt.getD "") ++ "</td>" ++
      "<td class=\"m trunc\">" ++ esc (r.name.getD (r.workflowFile.getD "")) ++ "</td></tr>")
  let cTable :=
    "<div class=\"card\"><table><thead><tr>" ++
    "<th>Concert ID</th><th>Status</th><th>Started</th><th>Finished</th><th>Name / File</th>" ++
    "</tr></thead><tbody>" ++ cRows ++ "</tbody></table></div>"
  let eRows :=
    if entries.isEmpty then "<tr><td colspan=\"7\" class=\"empty\">No entries</td></tr>"
    else String.join (entries.toList.map fun e =>
      "<tr><td class=\"m\"><a href=\"/tasks/" ++ e.id ++ "\">" ++ esc e.id ++ "</a></td>" ++
      "<td>" ++ badge (qStText e.status) ++ "</td>" ++
      "<td class=\"m trunc\">" ++ esc e.fork.toString ++ "</td>" ++
      "<td class=\"m\">" ++ esc e.createdAt ++ "</td>" ++
      "<td>" ++ toString e.priority ++ "</td>" ++
      "<td class=\"m\">" ++ esc (e.series.getD "") ++ "</td>" ++
      "<td class=\"m\">" ++ esc (e.concertId.getD "") ++ "</td></tr>")
  let eTable :=
    "<div class=\"card\"><table><thead><tr>" ++
    "<th>ID</th><th>Status</th><th>Fork</th><th>Created</th><th>Pri</th><th>Series</th><th>Concert</th>" ++
    "</tr></thead><tbody>" ++ eRows ++ "</tbody></table></div>"
  return shell "Queue"
    ("<h1>Queue</h1><h2>Concerts</h2>" ++ cTable ++ "<h2>Entries</h2>" ++ eTable)

private def listenersPage : IO String := do
  let cfgs ← Listener.loadAllListenerConfigs
  let rows ← cfgs.toList.mapM fun c => do
    let st ← Listener.loadListenerState c.name
    let last := if st.lastChecked.isEmpty then "never" else st.lastChecked
    let eb := if st.enabled then "<span class=\"b bd\">enabled</span>"
              else "<span class=\"b bc\">disabled</span>"
    return "<tr><td>" ++ esc c.name ++ "</td><td>" ++ eb ++ "</td>" ++
           "<td class=\"m\">" ++ toString c.intervalSeconds ++ "s</td>" ++
           "<td class=\"m\">" ++ esc last ++ "</td>" ++
           "<td>" ++ toString st.processedIds.size ++ "</td></tr>"
  let body :=
    if rows.isEmpty then "<tr><td colspan=\"5\" class=\"empty\">No listeners</td></tr>"
    else String.join rows
  return shell "Listeners"
    ("<h1>Listeners</h1><div class=\"card\"><table><thead><tr>" ++
     "<th>Name</th><th>Status</th><th>Interval</th><th>Last Checked</th><th>Events</th>" ++
     "</tr></thead><tbody>" ++ body ++ "</tbody></table></div>")

private def tasksPage : IO String := do
  let tasks ← TaskStore.loadAllTasks
  let shown := tasks.toList.take 50
  let rows :=
    if shown.isEmpty then "<tr><td colspan=\"6\" class=\"empty\">No tasks yet</td></tr>"
    else String.join (shown.map fun r =>
      let snip := (r.prompt.take 80).toString
      "<tr><td class=\"m\"><a href=\"/tasks/" ++ r.id ++ "\">" ++ esc r.id ++ "</a></td>" ++
      "<td>" ++ badge (tStText r.status) ++ "</td>" ++
      "<td class=\"m trunc\">" ++ esc r.fork.toString ++ "</td>" ++
      "<td class=\"m\">" ++ esc r.createdAt ++ "</td>" ++
      "<td class=\"m\">" ++ esc (r.series.getD "") ++ "</td>" ++
      "<td class=\"trunc\">" ++ esc snip ++ "</td></tr>")
  return shell "Tasks"
    ("<h1>Tasks</h1><div class=\"card\"><table><thead><tr>" ++
     "<th>ID</th><th>Status</th><th>Fork</th><th>Created</th><th>Series</th><th>Prompt</th>" ++
     "</tr></thead><tbody>" ++ rows ++ "</tbody></table></div>")

private def taskDetailPage (id : String) : IO (Option String) := do
  let record  ← TaskStore.loadTask id
  let entries ← Queue.loadAllEntries
  let qEntry  := entries.find? (·.id == id)
  let infoOpt : Option (Repository × String × String × String) :=
    match record with
    | some r => some (r.fork, tStText r.status, r.createdAt, r.prompt)
    | none   =>
      match qEntry with
      | some q => some (q.fork, qStText q.status, q.createdAt, q.prompt)
      | none   => none
  match infoOpt with
  | none => return none
  | some (fork, st, createdAt, prompt) =>
    let logPath := (← Dirs.dataBase) / "logs" / fork.toString / s!"{id}.log"
    let logContent ← if ← logPath.pathExists then IO.FS.readFile logPath else pure ""
    let logSec :=
      if logContent.isEmpty then "<p class=\"empty\" style=\"padding:16px\">No log file.</p>"
      else "<pre class=\"pre\">" ++ esc logContent ++ "</pre>"
    let details :=
      "<table>" ++
      "<tr><th>ID</th><td class=\"m\">" ++ esc id ++ "</td></tr>" ++
      "<tr><th>Status</th><td>" ++ badge st ++ "</td></tr>" ++
      "<tr><th>Fork</th><td class=\"m\">" ++ esc fork.toString ++ "</td></tr>" ++
      "<tr><th>Created</th><td class=\"m\">" ++ esc createdAt ++ "</td></tr>" ++
      "</table>"
    let content :=
      "<h1>Task <span class=\"m\">" ++ esc id ++ "</span></h1>" ++
      "<div class=\"card\">" ++ details ++ "</div>" ++
      "<h2>Prompt</h2><div class=\"card\"><pre class=\"pre\">" ++ esc prompt ++ "</pre></div>" ++
      "<h2>Log</h2><div class=\"card\">" ++ logSec ++ "</div>"
    return some (shell s!"Task {id}" content)

-- HTTP request parsing

private def parseRequest (raw : String) : Option HttpRequest :=
  match raw.splitOn "\r\n" with
  | [] => none
  | reqLine :: rest =>
    match reqLine.splitOn " " with
    | method :: path :: _ =>
      let rec parseHdrs (acc : Array (String × String)) : List String → Array (String × String)
        | [] | "" :: _ => acc
        | h :: t =>
          match h.splitOn ": " with
          | k :: vs => parseHdrs (acc.push (k, String.intercalate ": " vs)) t
          | _       => parseHdrs acc t
      let headers := parseHdrs #[] rest
      let body := match rest.dropWhile (· != "") with | _ :: b => String.intercalate "\r\n" b | _ => ""
      some { method, path, headers, body }
    | _ => none

private def queryParam (path key : String) : Option String :=
  match path.splitOn "?" with
  | [_, q] => q.splitOn "&" |>.findSome? fun p =>
      if p.startsWith (key ++ "=") then some ((p.drop (key.length + 1)).toString) else none
  | _ => none

private def pathOnly (path : String) : String :=
  match path.splitOn "?" with
  | p :: _ => p
  | _ => path

-- Route dispatch

private def dispatch (req : HttpRequest) (pw : String) : IO HttpResponse := do
  if !checkBasicAuth req.headers pw then return unauthorizedResp
  if req.method != "GET" then return { status := 400, headers := #[], body := "Bad Request" }
  let path := pathOnly req.path
  -- Exact route matches
  if path == "/" then return htmlResp (← overviewPage)
  if path == "/queue" then return htmlResp (← queuePage)
  if path == "/listeners" then return htmlResp (← listenersPage)
  if path == "/tasks" then
    match queryParam req.path "id" with
    | some id =>
      match ← taskDetailPage id with
      | some html => return htmlResp html
      | none => return notFound s!"Task not found: {esc id}"
    | none => return htmlResp (← tasksPage)
  -- Prefix routes: /tasks/<id>
  if path.startsWith "/tasks/" then
    let id := (path.drop 7).toString
    match ← taskDetailPage id with
    | some html => return htmlResp html
    | none => return notFound s!"Task not found: {esc id}"
  return notFound "Page not found."

-- TCP client handler

private def serveClient (client : Socket) (pw : String) : IO Unit := do
  let buf ← IO.mkRef ""
  let mut i := 0
  while i < 16 do
    i := i + 1
    let chunk? ← awaitTcp (← client.recv? 16384)
    match chunk? with
    | none => i := 16
    | some bytes =>
      buf.modify (· ++ String.fromUTF8! bytes)
      let cur ← buf.get
      -- Stop reading once we have the full headers
      if (cur.splitOn "\r\n\r\n").length > 1 then i := 16
  let raw ← buf.get
  let resp ← match parseRequest raw with
    | none => pure { status := 400, headers := #[], body := "Bad Request" }
    | some req => dispatch req pw
  let _ ← awaitTcp (← client.send #[httpResponse resp |>.toUTF8])

/-- Start the dashboard HTTP server on the given port (0 = auto-assign).
    Returns `(boundPort, shutdown)`. -/
def start (password : String) (port : UInt16 := 0) : IO (UInt16 × IO Unit) := do
  let server ← Socket.new
  let addr := SocketAddress.v4 { addr := IPv4Addr.ofParts 0 0 0 0, port }
  server.bind addr
  server.listen 32
  let boundPort := match ← server.getSockName with
    | .v4 a => a.port | .v6 a => a.port
  let running ← IO.mkRef true
  let _acceptTask ← IO.asTask (prio := .dedicated) do
    while ← running.get do
      match ← IO.wait (← server.accept).result! with
      | .error _ => break
      | .ok client =>
        if !(← running.get) then break
        let _ ← IO.asTask (prio := .dedicated) do
          try serveClient client password
          catch _ => pure ()
  let shutdown : IO Unit := do
    running.set false
    try
      let dummy ← Socket.new
      let dAddr := SocketAddress.v4 { addr := IPv4Addr.ofParts 127 0 0 1, port := boundPort }
      let _ ← dummy.connect dAddr
    catch _ => pure ()
  return (boundPort, shutdown)

end Orchestra.Dashboard
