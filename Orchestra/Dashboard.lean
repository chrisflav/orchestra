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

A minimal HTTP/1.1 server that serves a password-protected web dashboard.

The Lean server is responsible for:
  * Generating the *static* per-page HTML shell using the verso `Html` DSL
    (`Verso.Output.Html`). The shell carries the chrome (header, nav, CSS) and
    a single `<main>` element annotated with `data-page` / `data-api` /
    `data-sse` attributes.
  * Exposing JSON endpoints under `/api/...` that return view models for each
    page.
  * Pushing the same JSON view models to the browser over Server-Sent Events
    under `/sse/...` so the dashboard stays live.

All rendering of dynamic content (tables, badges, log rows) happens in the
embedded JavaScript bundle below. Adding a page = a Verso shell function +
a Lean `IO Json` builder + a JS renderer keyed on `data-page`.
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

private def htmlResp (html : Html) (status : Nat := 200) : HttpResponse :=
  { status
    headers := #[("Content-Type", "text/html; charset=utf-8")]
    body    := Html.doctype ++ Html.asString html }

private def jsonResp (j : Json) (status : Nat := 200) : HttpResponse :=
  { status
    headers := #[("Content-Type", "application/json; charset=utf-8")]
    body    := Json.compress j }

-- Base64 decode (uses <<< / >>> operators; keep outside the verso DSL open)

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

private def esc (s : String) : String := s.replace "&" "&amp;"

-- HTML shell generation
-- Open the verso `Html` DSL in a section so the `<<<` / `>>>` attribute syntax
-- does not shadow the bit-shift operators used in `decodeBase64` above.

section
open Verso.Output.Html

private def notFound (msg : String) : HttpResponse :=
  htmlResp ({{ <html><body><h1>"Not Found"</h1><p> s!"{esc msg}" </p></body></html> }}) 404

private def unauthorizedResp : HttpResponse :=
  let base := htmlResp {{ <html><body><h1>"401 Unauthorized"</h1></body></html> }} 401
  { base with headers := base.headers.push ("WWW-Authenticate", "Basic realm=\"Orchestra Dashboard\"") }

/-- Light theme stylesheet, served inline by the layout. -/
private def dashCss : String := include_str "Dashboard/dashboard.css"

/-- Frontend JS bundle, served inline by the layout. Boots from the
    `data-page` / `data-api` / `data-sse` attributes on `<main>`: fetches the
    initial JSON state, paints it, then subscribes to the SSE stream for
    live updates. -/
private def dashJs : String := include_str "Dashboard/dashboard.js"

private def loadingPlaceholder : Html := {{ <div class="loading"> "Loading…" </div> }}

/-- The single layout shared by every page. Dynamic pages pass a `pageId`,
    API URL and SSE URL — the embedded JS bundle reads them from the `<main>`
    `data-*` attributes and renders the page on the client. Static pages
    (e.g. the Verso-rendered About prose) pass `api = none` / `sse = none`
    and a server-side-rendered `content`; the JS bundle bails out because
    `data-api` is empty. -/
private def layout (title : String) (pageId : String) (api : Option String)
    (sse : Option String) (activeNav : String)
    (content : Html := loadingPlaceholder) : Html :=
  let apiAttr := api.getD ""
  let sseAttr := sse.getD ""
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
      <style> {{ Html.text false dashCss }} </style>
    </head>
    <body>
      <header>
        <span class="logo"> "Orchestra" </span>
        <nav>
          {{ navItem "/"          "Overview"  "overview"  }}
          {{ navItem "/queue"     "Queue"     "queue"     }}
          {{ navItem "/concerts"  "Concerts"  "concerts"  }}
          {{ navItem "/listeners" "Listeners" "listeners" }}
          {{ navItem "/tasks"     "Tasks"     "tasks"     }}
          {{ navItem "/about"     "About"     "about"     }}
        </nav>
      </header>
      <main data-page=s!"{pageId}" data-api=s!"{apiAttr}" data-sse=s!"{sseAttr}">
        {{ content }}
      </main>
      <script> {{ Html.text false dashJs }} </script>
    </body>
  </html> }}

-- Per-page Verso shells. Each is a single `layout` call.

private def overviewShell : Html :=
  layout "Overview" "overview" (some "/api/overview") (some "/sse/overview") "overview"

private def queueShell : Html :=
  layout "Queue" "queue" (some "/api/queue") (some "/sse/queue") "queue"

private def concertsShell : Html :=
  layout "Concerts" "concerts" (some "/api/concerts") (some "/sse/concerts") "concerts"

private def concertDetailShell (id : String) : Html :=
  layout s!"Concert {id}" "concert-detail"
    (some s!"/api/concerts/{id}") (some s!"/sse/concerts/{id}") "concerts"

private def listenersShell : Html :=
  layout "Listeners" "listeners" (some "/api/listeners") (some "/sse/listeners") "listeners"

private def listenerDetailShell (name : String) : Html :=
  layout s!"Listener {name}" "listener-detail"
    (some s!"/api/listeners/{name}") (some s!"/sse/listeners/{name}") "listeners"

private def tasksShell : Html :=
  layout "Tasks" "tasks" (some "/api/tasks") (some "/sse/tasks") "tasks"

private def taskDetailShell (id : String) : Html :=
  layout s!"Task {id}" "task-detail"
    (some s!"/api/tasks/{id}") (some s!"/sse/tasks/{id}") "tasks"

-- About: prose authored as a Verso document (`Orchestra/Dashboard/About.lean`)
-- and rendered to `Html` at request time using `Verso.Doc.Html.Part.toHtml`.

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

private def aboutShell : Html :=
  layout "About" "about" none none "about" (content := aboutContent)

end -- section (HTML shell generation)

-- View-model builders: each `/api/...` endpoint produces a `Json` value
-- tailored for the corresponding page renderer in `dashJs`.

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

private def queryParam (path key : String) : Option String :=
  match path.splitOn "?" with
  | [_, q] => q.splitOn "&" |>.findSome? fun p =>
      if p.startsWith (key ++ "=") then some ((p.drop (key.length + 1)).toString) else none
  | _ => none

private def pathOnly (path : String) : String :=
  match path.splitOn "?" with
  | p :: _ => p
  | _ => path

-- Route dispatch.
-- `/`, `/queue`, ... return static Verso-rendered shells.
-- `/api/...` return JSON view models.
-- `/sse/...` is handled by `serveClient` before reaching this function.

private def dispatch (req : HttpRequest) (pw : String) : IO HttpResponse := do
  if !checkBasicAuth req.headers pw then return unauthorizedResp
  if req.method != "GET" then return { status := 400, headers := #[], body := "Bad Request" }
  let path := pathOnly req.path
  -- JSON API
  if path.startsWith "/api/" then
    let raw := (path.drop "/api/".length).toString
    let kind := if raw.endsWith ".json" then (raw.take (raw.length - 5)).toString else raw
    match ← renderApi kind with
    | some j => return jsonResp j
    | none   => return jsonResp (Json.mkObj [("error", "not found")]) 404
  -- Static shells (JS fetches the matching /api/... after load)
  if path == "/about"     then return htmlResp aboutShell
  if path == "/"          then return htmlResp overviewShell
  if path == "/queue"     then return htmlResp queueShell
  if path == "/concerts"  then return htmlResp concertsShell
  if path.startsWith "/concerts/" then
    let id := (path.drop "/concerts/".length).toString
    return htmlResp (concertDetailShell id)
  if path == "/listeners" then return htmlResp listenersShell
  if path.startsWith "/listeners/" then
    let name := (path.drop "/listeners/".length).toString
    return htmlResp (listenerDetailShell name)
  if path == "/tasks" then
    match queryParam req.path "id" with
    | some id => return htmlResp (taskDetailShell id)
    | none    => return htmlResp tasksShell
  if path.startsWith "/tasks/" then
    let id := (path.drop "/tasks/".length).toString
    return htmlResp (taskDetailShell id)
  return notFound "Page not found."

-- SSE streaming handler

private def sseHeader : String :=
  "HTTP/1.1 200 OK\r\n" ++
  "Content-Type: text/event-stream\r\n" ++
  "Cache-Control: no-cache\r\n" ++
  "Connection: keep-alive\r\n" ++
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
      if (cur.splitOn "\r\n\r\n").length > 1 then i := 16
  let raw ← buf.get
  match parseRequest raw with
  | none =>
    let resp : HttpResponse := { status := 400, headers := #[], body := "Bad Request" }
    let _ ← awaitTcp (← client.send #[httpResponse resp |>.toUTF8])
  | some req =>
    let path := pathOnly req.path
    if path.startsWith "/sse/" then
      if !checkBasicAuth req.headers pw then
        let _ ← awaitTcp (← client.send #[httpResponse unauthorizedResp |>.toUTF8])
      else
        serveSse client (path.drop "/sse/".length).toString
    else
      let resp ← dispatch req pw
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
