import Lean.Data.Json
import Std.Internal.UV.TCP
import Std.Net
import Orchestra.Config
import Orchestra.Dirs
import Orchestra.Queue
import Orchestra.TaskStore
import Orchestra.Listener
import Orchestra.Dashboard.Site

open Lean (Json ToJson)
open Std.Net
open Std.Internal.UV.TCP

/-!
# Orchestra Web Dashboard

The dashboard is split into two independently-run pieces:

  * **`generate`** (in `Orchestra/Dashboard/Site.lean`) writes a fully static
    website (HTML/CSS/JS) into a target directory. The pages are authored as
    Verso `#doc (Page)` documents and emitted with Verso's Blog pipeline
    (`blogMain`), like `leanprover/verso-website`. The generated files can be
    served by any plain HTTP server — orchestra ships no front-end server.

  * **`serve`** (this module) runs a minimal HTTP/1.1 server (bound to
    `127.0.0.1`) exposing only the JSON API under `/api/...` and Server-Sent
    Event streams under `/sse/...`, with permissive CORS so the separately-served
    static site can fetch cross-origin. It serves no HTML.

Adding a page = a verso `#doc (Page)` shell (in `Dashboard/Pages/`) wired into
`versoSite` + a Lean `IO Json` builder (here) + a JS renderer keyed on
`data-page` (in `Dashboard/dashboard.js`).
-/

namespace Orchestra.Dashboard

-- API authentication
--
-- The server binds to `127.0.0.1` but replies with a wildcard `Access-Control-
-- Allow-Origin`, so any web page the user visits could otherwise `fetch` the
-- API cross-origin. A shared bearer token gates every `/api/...` and `/sse/...`
-- request. `generate` bakes the same token into the static site (see
-- `Dashboard/Site.lean`) and `dashboard.js` presents it on each request.

/-- The env var that overrides the persisted dashboard API token. -/
def tokenEnvVar : String := "ORCHESTRA_DASHBOARD_TOKEN"

/-- Where an auto-generated token is persisted so that `generate` and `serve`
    (typically separate invocations) agree on the same secret. -/
private def tokenFile : IO System.FilePath := do
  return (← Dirs.dataBase) / "dashboard.token"

private def hexDigits : Array Char := "0123456789abcdef".toList.toArray

private def toHex (b : UInt8) : String :=
  let n := b.toNat
  String.mk [hexDigits[n >>> 4]!, hexDigits[n &&& 0xf]!]

/-- Generate a fresh random token (60 hex chars / 30 bytes of entropy) sourced
    from `/dev/urandom`. -/
private def genToken : IO String := do
  let bytes ← IO.FS.withFile "/dev/urandom" .read fun h => h.read 30
  return String.join (bytes.toList.map toHex)

/-- Resolve the dashboard API token, in priority order:
    1. an explicit `flagToken` (e.g. `--token`),
    2. the `ORCHESTRA_DASHBOARD_TOKEN` env var,
    3. a token previously persisted under the data dir,
    4. otherwise a freshly generated token (persisted for reuse).

    Shared by `generate` (which bakes the token into the site) and `serve`
    (which enforces it), so both agree without the user wiring anything up. -/
def resolveToken (flagToken : Option String := none) : IO String := do
  if let some t := flagToken then
    if !t.trimAscii.isEmpty then return t.trimAscii.toString
  if let some t ← IO.getEnv tokenEnvVar then
    if !t.trimAscii.isEmpty then return t.trimAscii.toString
  let path ← tokenFile
  if ← path.pathExists then
    let t := (← IO.FS.readFile path).trimAscii.toString
    if !t.isEmpty then return t
  let t ← genToken
  IO.FS.createDirAll (← Dirs.dataBase)
  IO.FS.writeFile path (t ++ "\n")
  -- Best-effort: restrict the token file to the owner.
  try
    let _ ← IO.Process.run { cmd := "chmod", args := #["600", path.toString] }
  catch _ => pure ()
  return t

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
  | 200 => "OK"           | 204 => "No Content"
  | 400 => "Bad Request"  | 404 => "Not Found"
  | 401 => "Unauthorized" | 405 => "Method Not Allowed"
  | n   => s!"Status {n}"

/-- Permissive CORS headers so a separately-served static front-end can call the
    API/SSE endpoints from another origin. -/
private def corsHeaders : Array (String × String) :=
  #[("Access-Control-Allow-Origin", "*"),
    ("Access-Control-Allow-Methods", "GET, OPTIONS"),
    ("Access-Control-Allow-Headers", "Content-Type, Authorization")]

private def httpResponse (resp : HttpResponse) : String :=
  let statusLine := s!"HTTP/1.1 {resp.status} {statusText resp.status}\r\n"
  let contentLen := s!"Content-Length: {resp.body.utf8ByteSize}\r\n"
  let hdrs := resp.headers.map (fun (k, v) => s!"{k}: {v}\r\n") |>.toList
  statusLine ++ String.join hdrs ++ contentLen ++ "Connection: close\r\n\r\n" ++ resp.body

private def jsonResp (j : Json) (status : Nat := 200) : HttpResponse :=
  { status
    headers := #[("Content-Type", "application/json; charset=utf-8")] ++ corsHeaders
    body    := Json.compress j }

-- The static-site generator (`generate`, the theme, the page shells) lives in
-- `Orchestra/Dashboard/Site.lean`; this module contributes only the JSON API and
-- SSE server below.

-- View-model builders: each `/api/...` endpoint produces a `Json` value
-- tailored for the corresponding page renderer in `dashboard.js`.

private def qStText : Queue.QueueStatus → String
  | .pending    => "pending"   | .running    => "running"
  | .done       => "done"      | .failed     => "failed"
  | .unfinished => "unfinished"| .cancelled  => "cancelled"

private def tStText : TaskStore.TaskStatus → String
  | .running    => "running"   | .completed  => "completed"
  | .failed     => "failed"    | .unfinished => "unfinished"
  | .cancelled  => "cancelled"

private def cStText : Queue.ConcertStatus → String
  | .running   => "running"  | .done      => "done"
  | .failed    => "failed"   | .cancelled => "cancelled"

private def queueEntryJson (e : Queue.QueueEntry) : Json :=
  Json.mkObj [
    ("id",            e.id),
    ("status",        qStText e.status),
    ("fork",          e.fork.toString),
    ("createdAt",     e.createdAt),
    ("priority",      ToJson.toJson e.priority),
    ("series",        e.series.getD ""),
    ("concertId",     e.concertId.getD ""),
    ("concertStepKey", e.concertStepKey.getD ""),
    ("prompt",        e.prompt)
  ]

private def taskRecJson (r : TaskStore.TaskRecord) : Json :=
  Json.mkObj [
    ("id",        r.id),
    ("status",    tStText r.status),
    ("fork",      r.fork.toString),
    ("createdAt", r.createdAt),
    ("series",    r.series.getD ""),
    ("prompt",    r.prompt)
  ]

private def concertRunJson (r : Queue.ConcertRun) : Json :=
  Json.mkObj [
    ("id",           r.id),
    ("status",       cStText r.status),
    ("name",         r.name.getD ""),
    ("workflowFile", r.workflowFile.getD ""),
    ("startedAt",    r.startedAt),
    ("finishedAt",   r.finishedAt.getD "")
  ]

private def overviewApi : IO Json := do
  let entries  ← Queue.loadAllEntries
  let tasks    ← TaskStore.loadAllTasks
  let lsCfgs   ← Listener.loadAllListenerConfigs
  let concerts ← Queue.loadAllConcertRuns
  let running := entries.filter (·.status == .running)
  let pending := entries.filter (·.status == .pending)
  let failed  := entries.filter (·.status == .failed)
  let rConcerts := concerts.filter (·.status == .running)
  let active := running ++ pending
  let recent := tasks.toList.take 10
  return Json.mkObj [
    ("counts", Json.mkObj [
      ("running",    ToJson.toJson running.size),
      ("pending",    ToJson.toJson pending.size),
      ("failed",     ToJson.toJson failed.size),
      ("concerts",   ToJson.toJson rConcerts.size),
      ("listeners",  ToJson.toJson lsCfgs.size),
      ("totalTasks", ToJson.toJson tasks.size)
    ]),
    ("activeQueue", Json.arr (active.map queueEntryJson)),
    ("recentTasks", Json.arr (recent.map taskRecJson).toArray)
  ]

private def queueApi : IO Json := do
  let entries  ← Queue.loadAllEntries
  let concerts ← Queue.loadAllConcertRuns
  return Json.mkObj [
    ("concerts", Json.arr (concerts.map concertRunJson)),
    ("entries",  Json.arr (entries.map queueEntryJson))
  ]

private def concertsApi : IO Json := do
  let concerts ← Queue.loadAllConcertRuns
  return Json.mkObj [
    ("concerts", Json.arr (concerts.map concertRunJson))
  ]

private def concertDetailApi (id : String) : IO (Option Json) := do
  match ← Queue.loadConcertRun id with
  | none => return none
  | some r =>
    let entries ← Queue.loadAllEntries
    let steps := entries.filter (fun e => e.concertId == some id)
    return some (Json.mkObj [
      ("concert", concertRunJson r),
      ("steps",   Json.arr (steps.map queueEntryJson))
    ])

private def sourceSummary : Listener.SourceConfig → (String × String)
  | .githubIssues repos labels _ _    =>
      let r := String.intercalate ", " (repos.map (·.upstream.toString))
      ("github-issues",
        r ++ (if labels.isEmpty then "" else s!" [labels: {String.intercalate "," labels}]"))
  | .githubPrReviews repos labels _ _ =>
      let r := String.intercalate ", " (repos.map (·.upstream.toString))
      ("github-pr-reviews",
        r ++ (if labels.isEmpty then "" else s!" [labels: {String.intercalate "," labels}]"))
  | .githubComments repos labels _ _  =>
      let r := String.intercalate ", " (repos.map (·.upstream.toString))
      ("github-comments",
        r ++ (if labels.isEmpty then "" else s!" [labels: {String.intercalate "," labels}]"))
  | .shell cmd args                   => ("shell", s!"{cmd} {String.intercalate " " args}")
  | .projectDispatcher pid caps       =>
      let cs := String.intercalate ", " (caps.map (fun (n,c) => s!"{n}={c}"))
      ("project-dispatcher", s!"project={pid.value} caps=[{cs}]")
  | .githubLabelCount repos labels max kind =>
      let r := String.intercalate ", " (repos.map (·.upstream.toString))
      ("github-label-count", s!"{r} kind={kind} max={max} labels=[{String.intercalate "," labels}]")
  | .githubLabels repos labels kind _ =>
      let r := String.intercalate ", " (repos.map (·.upstream.toString))
      ("github-labels", s!"{r} kind={kind} labels=[{String.intercalate "," labels}]")

private def sourceExtras : Listener.SourceConfig → List (String × String)
  | .githubIssues _ _ trig authU
  | .githubPrReviews _ _ trig authU
  | .githubComments _ _ trig authU =>
      [("trigger", trig), ("authorized users", String.intercalate ", " authU)]
  | .githubLabels _ _ _ authU =>
      [("authorized users", String.intercalate ", " authU)]
  | _ => []

private def listenerSummaryJson (c : Listener.ListenerConfig) (st : Listener.ListenerState) : Json :=
  let (srcType, _) := sourceSummary c.source
  Json.mkObj [
    ("name",            c.name),
    ("enabled",         Json.bool st.enabled),
    ("sourceType",      srcType),
    ("intervalSeconds", ToJson.toJson c.intervalSeconds),
    ("lastChecked",     st.lastChecked),
    ("eventCount",      ToJson.toJson st.processedIds.size)
  ]

private def listenersApi : IO Json := do
  let cfgs ← Listener.loadAllListenerConfigs
  let rows : Array Json ← cfgs.mapM fun c => do
    let st ← Listener.loadListenerState c.name
    return listenerSummaryJson c st
  return Json.mkObj [("listeners", Json.arr rows)]

private def actionJson (a : Listener.ActionConfig) : Json :=
  Json.mkObj [
    ("mode",           ToJson.toJson a.mode),
    ("upstream",       a.upstream),
    ("fork",           a.fork),
    ("series",         a.series.getD ""),
    ("backend",        a.backend.getD ""),
    ("model",          a.model.getD ""),
    ("workflowPath",   a.workflowPath.getD ""),
    ("priority",       ToJson.toJson a.priority),
    ("promptTemplate", a.promptTemplate)
  ]

private def listenerDetailApi (name : String) : IO (Option Json) := do
  match ← Listener.loadListenerConfig name with
  | none => return none
  | some c =>
    let st ← Listener.loadListenerState c.name
    let (srcType, srcDetail) := sourceSummary c.source
    let extras := sourceExtras c.source
    let extrasJson : Array Json := (extras.filter (fun (_, v) => ! v.isEmpty)).map
      (fun (k, v) => (Json.arr #[Json.str k, Json.str v])) |>.toArray
    let recent := st.processedIds.toList.reverse.take 50
    return some (Json.mkObj [
      ("name",            c.name),
      ("enabled",         Json.bool st.enabled),
      ("intervalSeconds", ToJson.toJson c.intervalSeconds),
      ("lastChecked",     st.lastChecked),
      ("eventCount",      ToJson.toJson st.processedIds.size),
      ("sourceType",      srcType),
      ("sourceDetail",    srcDetail),
      ("sourceExtras",    Json.arr extrasJson),
      ("action",          actionJson c.action),
      ("recentEvents",    Json.arr (recent.map Json.str).toArray)
    ])

private def tasksApi : IO Json := do
  let tasks ← TaskStore.loadAllTasks
  return Json.mkObj [
    ("tasks", Json.arr ((tasks.toList.take 50).map taskRecJson).toArray)
  ]

/-- Parse the per-task structured JSONL log into an array of `Json` events.
    Returns an empty array if the file is missing. -/
private def loadTaskLog (fork : Repository) (id : String) : IO (Array Json) := do
  let path := (← Dirs.dataBase) / "logs" / fork.toString / s!"{id}.log"
  if !(← path.pathExists) then return #[]
  let raw ← IO.FS.readFile path
  let mut out : Array Json := #[]
  for line in raw.splitOn "\n" do
    if line.trimAscii.isEmpty then continue
    match Json.parse line with
    | .ok j    => out := out.push j
    | .error _ => out := out.push (Json.mkObj [("type", "unknown"), ("event_type", "parse_error")])
  return out

private def taskDetailApi (id : String) : IO (Option Json) := do
  let record  ← TaskStore.loadTask id
  let entries ← Queue.loadAllEntries
  let qEntry  := entries.find? (·.id == id)
  let infoOpt : Option (Repository × String × String × String) :=
    match record with
    | some r => some (r.fork, tStText r.status, r.createdAt, r.prompt)
    | none =>
      match qEntry with
      | some q => some (q.fork, qStText q.status, q.createdAt, q.prompt)
      | none   => none
  match infoOpt with
  | none => return none
  | some (fork, st, createdAt, prompt) =>
    let log ← loadTaskLog fork id
    return some (Json.mkObj [
      ("id",        id),
      ("status",    st),
      ("fork",      fork.toString),
      ("createdAt", createdAt),
      ("prompt",    prompt),
      ("log",       Json.arr log)
    ])

/-- Dispatch a `/api/...` or `/sse/...` kind string to the matching builder. -/
private def renderApi (kind : String) : IO (Option Json) := do
  if kind == "overview"  then return some (← overviewApi)
  if kind == "queue"     then return some (← queueApi)
  if kind == "concerts"  then return some (← concertsApi)
  if kind == "listeners" then return some (← listenersApi)
  if kind == "tasks"     then return some (← tasksApi)
  if kind.startsWith "concerts/" then
    return ← concertDetailApi (kind.drop "concerts/".length).toString
  if kind.startsWith "listeners/" then
    return ← listenerDetailApi (kind.drop "listeners/".length).toString
  if kind.startsWith "tasks/" then
    return ← taskDetailApi (kind.drop "tasks/".length).toString
  return none

/-- Encode a payload as a single SSE `message` event. -/
private def sseFrame (payload : String) : String :=
  let lines := payload.splitOn "\n"
  (lines.map (fun l => "data: " ++ l) |> String.intercalate "\n") ++ "\n\n"

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

private def pathOnly (path : String) : String :=
  match path.splitOn "?" with
  | p :: _ => p
  | _ => path

/-- Extract the API/SSE kind from a `/api/<kind>` (or `.json`-suffixed) path. -/
private def apiKind (path : String) : Option String :=
  if path.startsWith "/api/" then
    let raw := (path.drop "/api/".length).toString
    some (if raw.endsWith ".json" then (raw.take (raw.length - 5)).toString else raw)
  else none

-- Authentication: every request must present the shared token, either as an
-- `Authorization: Bearer <token>` header (used by `fetch`) or a `?token=<token>`
-- query parameter (needed for SSE, since `EventSource` cannot set headers).

/-- Look up a request header case-insensitively. -/
private def headerValue (req : HttpRequest) (name : String) : Option String :=
  (req.headers.find? (fun (k, _) => k.toLower == name.toLower)).map (·.2)

/-- The bearer token from an `Authorization: Bearer <token>` header, if present. -/
private def bearerToken (req : HttpRequest) : Option String :=
  match headerValue req "authorization" with
  | some v =>
    if v.startsWith "Bearer " then some (v.drop "Bearer ".length).trimAscii.toString else none
  | none   => none

/-- The value of a `?key=…` (or `&key=…`) query parameter in a raw path. Tokens
    are hex, so no percent-decoding is required. -/
private def queryParam (path : String) (key : String) : Option String :=
  match path.splitOn "?" with
  | _ :: q :: _ =>
    (q.splitOn "&").findSome? fun kv =>
      match kv.splitOn "=" with
      | k :: v :: _ => if k == key then some v else none
      | _           => none
  | _ => none

/-- Whether the request carries the expected token (header or query param). -/
private def authorized (token : String) (req : HttpRequest) : Bool :=
  let supplied := (bearerToken req).orElse (fun _ => queryParam req.path "token")
  supplied == some token

private def unauthorizedResp : HttpResponse :=
  jsonResp (Json.mkObj [("error", "unauthorized")]) 401

-- Route dispatch. Only `/api/...` (JSON) is handled here; `/sse/...` is handled
-- by `serveClient` before reaching this function. No HTML is served.

private def dispatch (token : String) (req : HttpRequest) : IO HttpResponse := do
  if req.method == "OPTIONS" then
    -- CORS preflight carries no credentials; never require auth for it.
    return { status := 204, headers := corsHeaders, body := "" }
  if req.method != "GET" then
    return jsonResp (Json.mkObj [("error", "method not allowed")]) 405
  unless authorized token req do
    return unauthorizedResp
  match apiKind (pathOnly req.path) with
  | some kind =>
    match ← renderApi kind with
    | some j => return jsonResp j
    | none   => return jsonResp (Json.mkObj [("error", "not found")]) 404
  | none => return jsonResp (Json.mkObj [("error", "not found")]) 404

-- SSE streaming handler

private def sseHeader : String :=
  "HTTP/1.1 200 OK\r\n" ++
  "Content-Type: text/event-stream\r\n" ++
  "Cache-Control: no-cache\r\n" ++
  "Connection: keep-alive\r\n" ++
  "Access-Control-Allow-Origin: *\r\n" ++
  "X-Accel-Buffering: no\r\n\r\n"

/-- Refresh interval for SSE pushes, in milliseconds. -/
private def sseIntervalMs : UInt32 := 2000

private partial def sseLoop (client : Socket) (kind : String) : IO Unit := do
  let jsonOpt ← try renderApi kind catch _ => pure none
  match jsonOpt with
  | none => return -- unknown kind or vanished resource; close stream
  | some j =>
    let ok ← try
      let _ ← awaitTcp (← client.send #[(sseFrame (Json.compress j)).toUTF8])
      pure true
    catch _ => pure false
    if !ok then return
    IO.sleep sseIntervalMs
    sseLoop client kind

private def serveSse (client : Socket) (kind : String) : IO Unit := do
  try
    let _ ← awaitTcp (← client.send #[sseHeader.toUTF8])
    sseLoop client kind
  catch _ => pure ()

-- TCP client handler

private def serveClient (token : String) (client : Socket) : IO Unit := do
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
      if (cur.splitOn "\r\n\r\n").length > 1 then i := 16
  let raw ← buf.get
  match parseRequest raw with
  | none =>
    let _ ← awaitTcp (← client.send #[httpResponse (jsonResp (Json.mkObj [("error", "bad request")]) 400) |>.toUTF8])
  | some req =>
    let path := pathOnly req.path
    if path.startsWith "/sse/" && req.method == "GET" then
      if authorized token req then
        serveSse client (path.drop "/sse/".length).toString
      else
        let _ ← awaitTcp (← client.send #[httpResponse unauthorizedResp |>.toUTF8])
    else
      let resp ← dispatch token req
      let _ ← awaitTcp (← client.send #[httpResponse resp |>.toUTF8])

/-- Start the dashboard JSON API + SSE backend, bound to `127.0.0.1` on the given
    port (`0` = auto-assign). Every request must present `token` (see
    `resolveToken`). Returns `(boundPort, shutdown)`. -/
def serve (token : String) (port : UInt16 := 8080) : IO (UInt16 × IO Unit) := do
  let server ← Socket.new
  let addr := SocketAddress.v4 { addr := IPv4Addr.ofParts 127 0 0 1, port }
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
          try serveClient token client
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
