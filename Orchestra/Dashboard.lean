import Lean.Data.Json
import Std.Internal.UV.TCP
import Std.Net
import Orchestra.Config
import Orchestra.Dirs
import Orchestra.Queue
import Orchestra.TaskStore
import Orchestra.Listener
import Verso.Output.Html
import Verso.Doc.Html
import Verso.Doc.Name
import Orchestra.Dashboard.About

open Lean (Json ToJson)
open Std.Net
open Std.Internal.UV.TCP
open Verso.Output (Html)
open Verso.Doc (Genre Part)

/-!
# Orchestra Web Dashboard

The dashboard is split into two independently-run pieces:

  * **`generate`** writes a fully static website (HTML/CSS/JS) into a target
    directory. The HTML page shells are authored with the verso `Html` DSL
    (`Verso.Output.Html`); each `<main>` carries `data-*` attributes telling the
    JS bundle which JSON endpoint to fetch and stream from. The generated files
    can be served by any plain HTTP server — orchestra ships no front-end server.

  * **`serve`** runs a minimal HTTP/1.1 server (bound to `127.0.0.1`) exposing
    only the JSON API under `/api/...` and Server-Sent Event streams under
    `/sse/...`, with permissive CORS so the separately-served static site can
    fetch cross-origin. It serves no HTML.

Adding a page = a verso shell (in `generate`) + a Lean `IO Json` builder + a JS
renderer keyed on `data-page` (in `Dashboard/dashboard.js`).
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
  | 200 => "OK"           | 204 => "No Content"
  | 400 => "Bad Request"  | 404 => "Not Found"
  | 405 => "Method Not Allowed"
  | n   => s!"Status {n}"

/-- Permissive CORS headers so a separately-served static front-end can call the
    API/SSE endpoints from another origin. -/
private def corsHeaders : Array (String × String) :=
  #[("Access-Control-Allow-Origin", "*"),
    ("Access-Control-Allow-Methods", "GET, OPTIONS"),
    ("Access-Control-Allow-Headers", "Content-Type")]

private def httpResponse (resp : HttpResponse) : String :=
  let statusLine := s!"HTTP/1.1 {resp.status} {statusText resp.status}\r\n"
  let contentLen := s!"Content-Length: {resp.body.utf8ByteSize}\r\n"
  let hdrs := resp.headers.map (fun (k, v) => s!"{k}: {v}\r\n") |>.toList
  statusLine ++ String.join hdrs ++ contentLen ++ "Connection: close\r\n\r\n" ++ resp.body

private def jsonResp (j : Json) (status : Nat := 200) : HttpResponse :=
  { status
    headers := #[("Content-Type", "application/json; charset=utf-8")] ++ corsHeaders
    body    := Json.compress j }

-- Pre-escape `&` in user-supplied strings before handing them to verso text
-- nodes. `Html.asString` escapes `<`/`>` but not `&`.
private def esc (s : String) : String := s.replace "&" "&amp;"

-- Static HTML shell generation.
-- Open the verso `Html` DSL in a section.

section
open Verso.Output.Html

private def loadingPlaceholder : Html := {{ <div class="loading"> "Loading…" </div> }}

/-- The single layout shared by every generated page.

    `apiBase` is the base URL of the JSON API backend (`dashboard serve`), baked
    into every page. `endpoint` is the API/SSE kind the page's JS should fetch
    (`"overview"`, `"queue"`, …); detail pages pass a prefix like `"tasks/"`
    together with `param` (`"id"`/`"name"`), whose value the JS reads from the
    page's query string and appends. Static pages (About) pass `endpoint = none`
    and a server-side-rendered `content`. -/
private def layout (apiBase title pageId : String) (endpoint param : Option String)
    (activeNav : String) (content : Html := loadingPlaceholder) : Html :=
  let epAttr    := endpoint.getD ""
  let paramAttr := param.getD ""
  let navItem (href label idTag : String) : Html :=
    if idTag == activeNav then
      {{ <a href=s!"{href}" class="active"> s!"{esc label}" </a> }}
    else
      {{ <a href=s!"{href}"> s!"{esc label}" </a> }}
  {{ <html lang="en">
    <head>
      <meta charset="utf-8"/>
      <meta name="viewport" content="width=device-width,initial-scale=1"/>
      <title> s!"{esc title} — Orchestra" </title>
      <link rel="stylesheet" href="dashboard.css"/>
    </head>
    <body>
      <header>
        <span class="logo"> "Orchestra" </span>
        <nav>
          {{ navItem "index.html"     "Overview"  "overview"  }}
          {{ navItem "queue.html"     "Queue"     "queue"     }}
          {{ navItem "concerts.html"  "Concerts"  "concerts"  }}
          {{ navItem "listeners.html" "Listeners" "listeners" }}
          {{ navItem "tasks.html"     "Tasks"     "tasks"     }}
          {{ navItem "about.html"     "About"     "about"     }}
        </nav>
      </header>
      <main data-page=s!"{pageId}" data-api-base=s!"{apiBase}"
            data-endpoint=s!"{epAttr}" data-param=s!"{paramAttr}">
        {{ content }}
      </main>
      <script src="dashboard.js"> "" </script>
    </body>
  </html> }}

-- Per-page verso shells. Each is a single `layout` call parameterised by the
-- JSON API base URL. List pages fetch `/api/<kind>`; detail pages append the
-- query-string value for `param` to the `<kind>/` prefix.

private def overviewShell      (apiBase : String) : Html := layout apiBase "Overview"  "overview"        (some "overview")   none        "overview"
private def queueShell         (apiBase : String) : Html := layout apiBase "Queue"     "queue"           (some "queue")      none        "queue"
private def concertsShell      (apiBase : String) : Html := layout apiBase "Concerts"  "concerts"        (some "concerts")   none        "concerts"
private def listenersShell     (apiBase : String) : Html := layout apiBase "Listeners" "listeners"       (some "listeners")  none        "listeners"
private def tasksShell         (apiBase : String) : Html := layout apiBase "Tasks"     "tasks"           (some "tasks")      none        "tasks"
private def taskDetailShell    (apiBase : String) : Html := layout apiBase "Task"      "task-detail"     (some "tasks/")     (some "id")   "tasks"
private def concertDetailShell (apiBase : String) : Html := layout apiBase "Concert"   "concert-detail"  (some "concerts/")  (some "id")   "concerts"
private def listenerDetailShell (apiBase : String) : Html := layout apiBase "Listener" "listener-detail" (some "listeners/") (some "name") "listeners"

-- About: prose authored as a verso document (`Orchestra/Dashboard/About.lean`)
-- and rendered to `Html` at generation time.

private def renderVersoPart (p : Part Genre.none) : Html :=
  let ctx : Verso.Doc.Html.HtmlT.Context Genre.none :=
    { options         := {}
      traverseContext := ()
      traverseState   := ()
      definitionIds   := {}
      linkTargets     := {}
      codeOptions     := {} }
  let act : Verso.Doc.Html.HtmlT Genre.none Id Html :=
    Verso.Doc.Html.ToHtml.toHtml p
  ((act ctx).run {}).fst

private def aboutContent : Html :=
  {{ <div class="prose"> {{ renderVersoPart (%doc Orchestra.Dashboard.About) }} </div> }}

private def aboutShell (apiBase : String) : Html :=
  layout apiBase "About" "about" none none "about" (content := aboutContent)

/-- Light theme stylesheet, written out by `generate`. -/
private def dashCss : String := include_str "Dashboard/dashboard.css"

/-- Front-end JS bundle, written out by `generate`. -/
private def dashJs : String := include_str "Dashboard/dashboard.js"

end -- section (HTML shell generation)

/-- Generate the complete static dashboard site into `targetDir`. The generated
    front-end fetches (and streams over SSE) from the JSON API at `apiBase` —
    the base URL of a running `orchestra dashboard serve`. -/
def generate (targetDir : System.FilePath) (apiBase : String) : IO Unit := do
  IO.FS.createDirAll targetDir
  let writeHtml (name : String) (h : Html) : IO Unit :=
    IO.FS.writeFile (targetDir / name) (Html.doctype ++ Html.asString h)
  writeHtml "index.html"     (overviewShell apiBase)
  writeHtml "queue.html"     (queueShell apiBase)
  writeHtml "concerts.html"  (concertsShell apiBase)
  writeHtml "listeners.html" (listenersShell apiBase)
  writeHtml "tasks.html"     (tasksShell apiBase)
  writeHtml "task.html"      (taskDetailShell apiBase)
  writeHtml "concert.html"   (concertDetailShell apiBase)
  writeHtml "listener.html"  (listenerDetailShell apiBase)
  writeHtml "about.html"     (aboutShell apiBase)
  IO.FS.writeFile (targetDir / "dashboard.css") dashCss
  IO.FS.writeFile (targetDir / "dashboard.js") dashJs

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

-- Route dispatch. Only `/api/...` (JSON) is handled here; `/sse/...` is handled
-- by `serveClient` before reaching this function. No HTML is served.

private def dispatch (req : HttpRequest) : IO HttpResponse := do
  if req.method == "OPTIONS" then
    return { status := 204, headers := corsHeaders, body := "" }
  if req.method != "GET" then
    return jsonResp (Json.mkObj [("error", "method not allowed")]) 405
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

private def serveClient (client : Socket) : IO Unit := do
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
      serveSse client (path.drop "/sse/".length).toString
    else
      let resp ← dispatch req
      let _ ← awaitTcp (← client.send #[httpResponse resp |>.toUTF8])

/-- Start the dashboard JSON API + SSE backend, bound to `127.0.0.1` on the given
    port (`0` = auto-assign). Returns `(boundPort, shutdown)`. -/
def serve (port : UInt16 := 8080) : IO (UInt16 × IO Unit) := do
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
          try serveClient client
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
