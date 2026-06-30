import Lean.Data.Json
import Std.Internal.UV.TCP
import Std.Net
import Orchestra.Config
import Orchestra.Dirs
import Orchestra.Queue
import Orchestra.TaskStore
import Orchestra.Listener
import Verso.Output.Html

open Lean (Json ToJson)
open Std.Net
open Std.Internal.UV.TCP
open Verso.Output (Html)

/-!
# Orchestra Web Dashboard

A minimal HTTP/1.1 server that serves a password-protected web dashboard showing
the live state of the queue, configured listeners, and task history with log output.

HTML is generated using the verso `Html` DSL (`Verso.Output.Html`).
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

-- Pre-escape `&` in user-supplied strings before handing them to verso text nodes.
-- `Html.asString` for `Html.text true` escapes `<` and `>` but not `&`; we handle
-- `&` here so that the combined output is fully HTML-safe.
private def esc (s : String) : String := s.replace "&" "&amp;"

-- HTML generation
-- Open the verso Html DSL in a section so that the <<< / >>> attribute syntax
-- does not shadow the bit-shift operators used in decodeBase64 above.

section
open Verso.Output.Html

private def notFound (msg : String) : HttpResponse :=
  htmlResp ({{ <html><body><h1>"Not Found"</h1><p> s!"{esc msg}" </p></body></html> }}) 404

private def unauthorizedResp : HttpResponse :=
  let base := htmlResp {{ <html><body><h1>"401 Unauthorized"</h1></body></html> }} 401
  { base with headers := base.headers.push ("WWW-Authenticate", "Basic realm=\"Orchestra Dashboard\"") }

private def badge (s : String) : Html :=
  let cls := match s with
    | "running"             => "r"
    | "pending"             => "p"
    | "done" | "completed" => "d"
    | "failed"              => "f"
    | "cancelled"           => "c"
    | _                     => "o"
  {{ <span class=s!"b b{cls}"> s!"{s}" </span> }}

private def dashCss : String :=
  ":root{--bg:#f7f8fa;--sf:#ffffff;--bd:#e4e7ec;--tx:#1f2937;--mu:#6b7280;--ac:#2563eb;--code:#f3f4f6}" ++
  "*{box-sizing:border-box;margin:0;padding:0}" ++
  "body{font-family:system-ui,-apple-system,'Segoe UI',sans-serif;background:var(--bg);color:var(--tx);font-size:14px;line-height:1.55}" ++
  "code,pre,.mono{font-family:ui-monospace,SFMono-Regular,Menlo,monospace}" ++
  "a{color:var(--ac);text-decoration:none}a:hover{text-decoration:underline}" ++
  "header{background:var(--sf);border-bottom:1px solid var(--bd);padding:12px 24px;display:flex;align-items:center;gap:24px}" ++
  "header .logo{font-weight:700;font-size:16px;color:var(--tx)}" ++
  "nav{display:flex;gap:16px}nav a{color:var(--mu);font-size:13px}nav a:hover{color:var(--tx)}" ++
  "main{padding:24px;max-width:1200px;margin:0 auto}" ++
  "h1{font-size:20px;font-weight:600;margin-bottom:16px}" ++
  "h2{font-size:12px;font-weight:600;margin:24px 0 10px;color:var(--mu);text-transform:uppercase;letter-spacing:.06em}" ++
  ".card{background:var(--sf);border:1px solid var(--bd);border-radius:8px;overflow:hidden;margin-bottom:16px;box-shadow:0 1px 2px rgba(0,0,0,.03)}" ++
  "table{width:100%;border-collapse:collapse}" ++
  "th{text-align:left;padding:10px 12px;font-size:11px;text-transform:uppercase;letter-spacing:.05em;color:var(--mu);border-bottom:1px solid var(--bd);font-weight:600;background:#fafbfc}" ++
  "td{padding:10px 12px;border-bottom:1px solid var(--bd);vertical-align:top}tr:last-child td{border-bottom:none}" ++
  "tr:hover td{background:#fafbfc}" ++
  ".b{display:inline-block;padding:2px 8px;border-radius:12px;font-size:11px;font-weight:600;text-transform:uppercase;letter-spacing:.03em}" ++
  ".br{background:#dcfce7;color:#15803d}.bp{background:#fef3c7;color:#a16207}" ++
  ".bd{background:#dcfce7;color:#166534}.bf{background:#fee2e2;color:#b91c1c}" ++
  ".bc{background:#e5e7eb;color:#4b5563}.bo{background:#dbeafe;color:#1d4ed8}" ++
  ".m{font-size:12px;color:var(--mu)}.mono{font-family:ui-monospace,monospace;font-size:12px}" ++
  ".trunc{max-width:260px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}" ++
  ".pre{background:var(--code);padding:16px;font-size:12px;font-family:ui-monospace,monospace;color:var(--tx);" ++
       "white-space:pre-wrap;word-break:break-word;max-height:600px;overflow-y:auto;border-radius:0}" ++
  ".empty{padding:32px;text-align:center;color:var(--mu)}" ++
  ".note{font-size:11px;color:var(--mu);margin-top:8px}" ++
  ".stats{display:grid;grid-template-columns:repeat(auto-fit,minmax(140px,1fr));gap:12px;margin-bottom:24px}" ++
  ".st{background:var(--sf);border:1px solid var(--bd);border-radius:8px;padding:16px;box-shadow:0 1px 2px rgba(0,0,0,.03)}" ++
  ".sv{font-size:26px;font-weight:700;color:var(--tx)}" ++
  ".sl{font-size:11px;color:var(--mu);text-transform:uppercase;letter-spacing:.05em;margin-top:2px}" ++
  -- Log renderer
  ".log{background:var(--sf);padding:0;font-size:13px;max-height:760px;overflow-y:auto}" ++
  ".le{display:grid;grid-template-columns:90px 1fr;gap:12px;padding:10px 14px;border-bottom:1px solid var(--bd);align-items:start}" ++
  ".le:last-child{border-bottom:none}" ++
  ".lk{font-size:10px;font-weight:700;text-transform:uppercase;letter-spacing:.06em;padding:3px 8px;border-radius:4px;text-align:center;width:fit-content}" ++
  ".lk-init{background:#dbeafe;color:#1d4ed8}.lk-system{background:#e5e7eb;color:#374151}" ++
  ".lk-think{background:#ede9fe;color:#6d28d9}.lk-tool{background:#fef3c7;color:#92400e}" ++
  ".lk-text{background:#dcfce7;color:#166534}.lk-out{background:#f3f4f6;color:#374151}" ++
  ".lk-result{background:#dcfce7;color:#166534}.lk-err{background:#fee2e2;color:#b91c1c}" ++
  ".lk-unk{background:#e5e7eb;color:#6b7280}" ++
  ".lbody{min-width:0;font-family:ui-monospace,monospace;font-size:12.5px;line-height:1.5;color:var(--tx);word-break:break-word}" ++
  ".lbody pre{background:var(--code);padding:8px 10px;border-radius:4px;white-space:pre-wrap;word-break:break-word;font-size:12px;margin-top:4px;max-height:300px;overflow-y:auto}" ++
  ".lbody .meta{color:var(--mu);font-size:11px;margin-top:2px}" ++
  ".lbody .toolname{font-weight:600;color:#92400e}" ++
  ".lbody .thinking{font-style:italic;color:#6d28d9}" ++
  ".lbody .err{color:#b91c1c;font-weight:600}" ++
  ".lraw{padding:8px 14px;font-family:ui-monospace,monospace;font-size:11px;color:var(--mu);border-bottom:1px solid var(--bd)}"

-- JS that subscribes to an SSE endpoint and replaces the inner HTML of `<main>`
-- with the received fragment. The browser reuses the page's cached Basic Auth
-- credentials for the EventSource request to the same origin.
private def sseScript (endpoint : String) : String :=
  "var es=new EventSource('" ++ endpoint ++ "');" ++
  "es.onmessage=function(e){document.querySelector('main').innerHTML=e.data;};" ++
  "window.addEventListener('beforeunload',function(){es.close();});"

private def shell (title : String) (sseEndpoint : Option String) (content : Html) : Html :=
  let script := match sseEndpoint with
    | none => ""
    | some p => sseScript p
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
          <a href="/"> "Overview" </a>
          <a href="/queue"> "Queue" </a>
          <a href="/concerts"> "Concerts" </a>
          <a href="/listeners"> "Listeners" </a>
          <a href="/tasks"> "Tasks" </a>
        </nav>
      </header>
      <main> {{ content }} </main>
      <script> {{ Html.text false script }} </script>
    </body>
  </html> }}

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

private def statBox (val : Nat) (lbl : String) : Html :=
  {{ <div class="st">
    <div class="sv"> s!"{val}" </div>
    <div class="sl"> s!"{lbl}" </div>
  </div> }}

private def overviewContent : IO Html := do
  let entries  ← Queue.loadAllEntries
  let tasks    ← TaskStore.loadAllTasks
  let lsCfgs   ← Listener.loadAllListenerConfigs
  let concerts ← Queue.loadAllConcertRuns
  let running := entries.filter (·.status == .running)
  let pending := entries.filter (·.status == .pending)
  let failed  := entries.filter (·.status == .failed)
  let rConcerts := concerts.filter (·.status == .running)
  let stats : Html :=
    {{ <div class="stats">
      {{ statBox running.size "Running" }}
      {{ statBox pending.size "Pending" }}
      {{ statBox failed.size "Failed" }}
      {{ statBox rConcerts.size "Concerts" }}
      {{ statBox lsCfgs.size "Listeners" }}
      {{ statBox tasks.size "Total Tasks" }}
    </div> }}
  let active := running ++ pending
  let activeRows : Html :=
    if active.isEmpty then
      {{ <tr><td colspan="6" class="empty"> "No active tasks" </td></tr> }}
    else
      Html.fromList (active.toList.map fun e =>
        {{ <tr>
          <td class="m"> <a href=s!"/tasks/{e.id}"> s!"{esc e.id}" </a> </td>
          <td> {{ badge (qStText e.status) }} </td>
          <td class="m trunc"> s!"{esc e.fork.toString}" </td>
          <td class="m"> s!"{e.createdAt}" </td>
          <td> s!"{e.priority}" </td>
          <td class="m trunc"> s!"{esc (e.concertId.getD "")}" </td>
        </tr> }})
  let qTable : Html :=
    {{ <div class="card">
      <table>
        <thead>
          <tr>
            <th> "ID" </th> <th> "Status" </th> <th> "Fork" </th>
            <th> "Created" </th> <th> "Pri" </th> <th> "Concert" </th>
          </tr>
        </thead>
        <tbody> {{ activeRows }} </tbody>
      </table>
    </div> }}
  let recentTasks := tasks.toList.take 10
  let recentRows : Html :=
    if recentTasks.isEmpty then
      {{ <tr><td colspan="5" class="empty"> "No tasks yet" </td></tr> }}
    else
      Html.fromList (recentTasks.map fun r =>
        let snip : String := (r.prompt.take 60).toString
        {{ <tr>
          <td class="m"> <a href=s!"/tasks/{r.id}"> s!"{esc r.id}" </a> </td>
          <td> {{ badge (tStText r.status) }} </td>
          <td class="m trunc"> s!"{esc r.fork.toString}" </td>
          <td class="m"> s!"{r.createdAt}" </td>
          <td class="trunc"> s!"{esc snip}" </td>
        </tr> }})
  let tTable : Html :=
    {{ <div class="card">
      <table>
        <thead>
          <tr>
            <th> "ID" </th> <th> "Status" </th> <th> "Fork" </th>
            <th> "Created" </th> <th> "Prompt" </th>
          </tr>
        </thead>
        <tbody> {{ recentRows }} </tbody>
      </table>
    </div> }}
  return {{
    {{ stats }}
    <h2> "Active Queue" </h2>
    {{ qTable }}
    <h2> "Recent Tasks" </h2>
    {{ tTable }}
  }}

private def overviewPage : IO Html := do
  return shell "Overview" (some "/sse/overview") (← overviewContent)

private def queueContent : IO Html := do
  let entries  ← Queue.loadAllEntries
  let concerts ← Queue.loadAllConcertRuns
  let cRows : Html :=
    if concerts.isEmpty then
      {{ <tr><td colspan="5" class="empty"> "No concerts" </td></tr> }}
    else
      Html.fromList (concerts.toList.map fun r =>
        {{ <tr>
          <td class="mono"> <a href=s!"/concerts/{r.id}"> s!"{esc r.id}" </a> </td>
          <td> {{ badge (cStText r.status) }} </td>
          <td class="m"> s!"{r.startedAt}" </td>
          <td class="m"> s!"{esc (r.finishedAt.getD "")}" </td>
          <td class="m trunc"> s!"{esc (r.name.getD (r.workflowFile.getD ""))}" </td>
        </tr> }})
  let cTable : Html :=
    {{ <div class="card">
      <table>
        <thead>
          <tr>
            <th> "Concert ID" </th> <th> "Status" </th>
            <th> "Started" </th> <th> "Finished" </th> <th> "Name / File" </th>
          </tr>
        </thead>
        <tbody> {{ cRows }} </tbody>
      </table>
    </div> }}
  let eRows : Html :=
    if entries.isEmpty then
      {{ <tr><td colspan="7" class="empty"> "No entries" </td></tr> }}
    else
      Html.fromList (entries.toList.map fun e =>
        {{ <tr>
          <td class="m"> <a href=s!"/tasks/{e.id}"> s!"{esc e.id}" </a> </td>
          <td> {{ badge (qStText e.status) }} </td>
          <td class="m trunc"> s!"{esc e.fork.toString}" </td>
          <td class="m"> s!"{e.createdAt}" </td>
          <td> s!"{e.priority}" </td>
          <td class="m"> s!"{esc (e.series.getD "")}" </td>
          <td class="m"> s!"{esc (e.concertId.getD "")}" </td>
        </tr> }})
  let eTable : Html :=
    {{ <div class="card">
      <table>
        <thead>
          <tr>
            <th> "ID" </th> <th> "Status" </th> <th> "Fork" </th>
            <th> "Created" </th> <th> "Pri" </th> <th> "Series" </th> <th> "Concert" </th>
          </tr>
        </thead>
        <tbody> {{ eRows }} </tbody>
      </table>
    </div> }}
  return {{
    <h1> "Queue" </h1>
    <h2> "Concerts" </h2>
    {{ cTable }}
    <h2> "Entries" </h2>
    {{ eTable }}
  }}

private def queuePage : IO Html := do
  return shell "Queue" (some "/sse/queue") (← queueContent)

-- Concerts

private def concertsContent : IO Html := do
  let concerts ← Queue.loadAllConcertRuns
  let rows : Html :=
    if concerts.isEmpty then
      {{ <tr><td colspan="6" class="empty"> "No concerts" </td></tr> }}
    else
      Html.fromList (concerts.toList.map fun r =>
        {{ <tr>
          <td class="mono"> <a href=s!"/concerts/{r.id}"> s!"{esc r.id}" </a> </td>
          <td> {{ badge (cStText r.status) }} </td>
          <td class="m"> s!"{esc (r.name.getD "")}" </td>
          <td class="m trunc"> s!"{esc (r.workflowFile.getD "")}" </td>
          <td class="m"> s!"{r.startedAt}" </td>
          <td class="m"> s!"{esc (r.finishedAt.getD "")}" </td>
        </tr> }})
  return {{
    <h1> "Concerts" </h1>
    <div class="card">
      <table>
        <thead>
          <tr>
            <th> "ID" </th> <th> "Status" </th> <th> "Name" </th>
            <th> "Workflow" </th> <th> "Started" </th> <th> "Finished" </th>
          </tr>
        </thead>
        <tbody> {{ rows }} </tbody>
      </table>
    </div>
  }}

private def concertsPage : IO Html := do
  return shell "Concerts" (some "/sse/concerts") (← concertsContent)

private def concertDetailContent (id : String) : IO (Option Html) := do
  match ← Queue.loadConcertRun id with
  | none => return none
  | some r =>
    let entries ← Queue.loadAllEntries
    let steps := entries.filter (fun e => e.concertId == some id)
    let stepRows : Html :=
      if steps.isEmpty then
        {{ <tr><td colspan="5" class="empty"> "No steps" </td></tr> }}
      else
        Html.fromList (steps.toList.map fun e =>
          {{ <tr>
            <td class="mono"> <a href=s!"/tasks/{e.id}"> s!"{esc e.id}" </a> </td>
            <td> {{ badge (qStText e.status) }} </td>
            <td class="m trunc"> s!"{esc e.fork.toString}" </td>
            <td class="m"> s!"{e.createdAt}" </td>
            <td class="m trunc"> s!"{esc (e.concertStepKey.getD "")}" </td>
          </tr> }})
    let details : Html :=
      {{ <table>
        <tr><th> "ID" </th>       <td class="mono"> s!"{esc r.id}" </td></tr>
        <tr><th> "Status" </th>   <td> {{ badge (cStText r.status) }} </td></tr>
        <tr><th> "Name" </th>     <td> s!"{esc (r.name.getD "—")}" </td></tr>
        <tr><th> "Workflow" </th> <td class="mono"> s!"{esc (r.workflowFile.getD "—")}" </td></tr>
        <tr><th> "Started" </th>  <td class="m"> s!"{r.startedAt}" </td></tr>
        <tr><th> "Finished" </th> <td class="m"> s!"{esc (r.finishedAt.getD "—")}" </td></tr>
      </table> }}
    return some {{
      <h1> "Concert " <span class="m mono"> s!"{esc id}" </span> </h1>
      <div class="card"> {{ details }} </div>
      <h2> "Steps" </h2>
      <div class="card">
        <table>
          <thead>
            <tr>
              <th> "Task" </th> <th> "Status" </th> <th> "Fork" </th>
              <th> "Created" </th> <th> "Step Key" </th>
            </tr>
          </thead>
          <tbody> {{ stepRows }} </tbody>
        </table>
      </div>
    }}

private def concertDetailPage (id : String) : IO (Option Html) := do
  match ← concertDetailContent id with
  | none => return none
  | some content => return some (shell s!"Concert {id}" (some s!"/sse/concerts/{id}") content)

-- Listener detail

private def sourceSummary : Listener.SourceConfig → (String × String)
  | .githubIssues repos labels _ _    =>
      let r := String.intercalate ", " (repos.map (·.upstream.toString))
      ("github-issues", s!"{r}" ++ (if labels.isEmpty then "" else s!" [labels: {String.intercalate "," labels}]"))
  | .githubPrReviews repos labels _ _ =>
      let r := String.intercalate ", " (repos.map (·.upstream.toString))
      ("github-pr-reviews", s!"{r}" ++ (if labels.isEmpty then "" else s!" [labels: {String.intercalate "," labels}]"))
  | .githubComments repos labels _ _  =>
      let r := String.intercalate ", " (repos.map (·.upstream.toString))
      ("github-comments", s!"{r}" ++ (if labels.isEmpty then "" else s!" [labels: {String.intercalate "," labels}]"))
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

private def listenersContent : IO Html := do
  let cfgs ← Listener.loadAllListenerConfigs
  let rows : List Html ← cfgs.toList.mapM fun c => do
    let st ← Listener.loadListenerState c.name
    let last := if st.lastChecked.isEmpty then "never" else st.lastChecked
    let eb : Html :=
      if st.enabled then {{ <span class="b bd"> "enabled" </span> }}
      else {{ <span class="b bc"> "disabled" </span> }}
    let (srcType, _) := sourceSummary c.source
    return {{ <tr>
      <td> <a href=s!"/listeners/{c.name}"> s!"{esc c.name}" </a> </td>
      <td> {{ eb }} </td>
      <td class="m"> s!"{esc srcType}" </td>
      <td class="m"> s!"{c.intervalSeconds}s" </td>
      <td class="m"> s!"{last}" </td>
      <td> s!"{st.processedIds.size}" </td>
    </tr> }}
  let tableBody : Html :=
    if rows.isEmpty then
      {{ <tr><td colspan="6" class="empty"> "No listeners" </td></tr> }}
    else Html.fromList rows
  return {{
    <h1> "Listeners" </h1>
    <div class="card">
      <table>
        <thead>
          <tr>
            <th> "Name" </th> <th> "Status" </th> <th> "Source" </th>
            <th> "Interval" </th> <th> "Last Checked" </th> <th> "Events" </th>
          </tr>
        </thead>
        <tbody> {{ tableBody }} </tbody>
      </table>
    </div>
  }}

private def listenersPage : IO Html := do
  return shell "Listeners" (some "/sse/listeners") (← listenersContent)

private def listenerDetailContent (name : String) : IO (Option Html) := do
  match ← Listener.loadListenerConfig name with
  | none => return none
  | some c =>
    let st ← Listener.loadListenerState c.name
    let last := if st.lastChecked.isEmpty then "never" else st.lastChecked
    let (srcType, srcDetail) := sourceSummary c.source
    let extras := sourceExtras c.source
    let extraRows : Html := Html.fromList (extras.filterMap fun (k, v) =>
      if v.isEmpty then none
      else some {{ <tr><th> s!"{esc k}" </th><td class="m"> s!"{esc v}" </td></tr> }})
    let eb : Html :=
      if st.enabled then {{ <span class="b bd"> "enabled" </span> }}
      else {{ <span class="b bc"> "disabled" </span> }}
    let cfg : Html :=
      {{ <table>
        <tr><th> "Name" </th>     <td> s!"{esc c.name}" </td></tr>
        <tr><th> "Status" </th>   <td> {{ eb }} </td></tr>
        <tr><th> "Interval" </th> <td class="m"> s!"{c.intervalSeconds}s" </td></tr>
        <tr><th> "Last Checked" </th> <td class="m"> s!"{last}" </td></tr>
        <tr><th> "Events Seen" </th>  <td> s!"{st.processedIds.size}" </td></tr>
        <tr><th> "Source Type" </th>  <td class="mono"> s!"{esc srcType}" </td></tr>
        <tr><th> "Source" </th>       <td class="m"> s!"{esc srcDetail}" </td></tr>
        {{ extraRows }}
      </table> }}
    let act : Html :=
      {{ <table>
        <tr><th> "Mode" </th>     <td class="mono"> s!"{esc (Lean.ToJson.toJson c.action.mode).compress}" </td></tr>
        <tr><th> "Upstream" </th> <td class="m"> s!"{esc c.action.upstream}" </td></tr>
        <tr><th> "Fork" </th>     <td class="m"> s!"{esc c.action.fork}" </td></tr>
        <tr><th> "Series" </th>   <td class="m"> s!"{esc (c.action.series.getD "—")}" </td></tr>
        <tr><th> "Backend" </th>  <td class="m"> s!"{esc (c.action.backend.getD "—")}" </td></tr>
        <tr><th> "Model" </th>    <td class="m"> s!"{esc (c.action.model.getD "—")}" </td></tr>
        <tr><th> "Workflow" </th> <td class="mono"> s!"{esc (c.action.workflowPath.getD "—")}" </td></tr>
        <tr><th> "Priority" </th> <td> s!"{c.action.priority}" </td></tr>
      </table> }}
    let recent := st.processedIds.toList.reverse.take 50
    let evRows : Html :=
      if recent.isEmpty then
        {{ <tr><td class="empty"> "No events processed yet" </td></tr> }}
      else
        Html.fromList (recent.map fun ev =>
          {{ <tr><td class="mono"> s!"{esc ev}" </td></tr> }})
    return some {{
      <h1> "Listener " <span class="m"> s!"{esc name}" </span> </h1>
      <h2> "Configuration" </h2>
      <div class="card"> {{ cfg }} </div>
      <h2> "Action" </h2>
      <div class="card">
        {{ act }}
        <div style="padding:12px">
          <div class="meta" style="font-size:11px;color:var(--mu);margin-bottom:4px"> "PROMPT TEMPLATE" </div>
          <pre class="pre"> s!"{esc c.action.promptTemplate}" </pre>
        </div>
      </div>
      <h2> s!"Processed Event IDs ({st.processedIds.size})" </h2>
      <div class="card">
        <table><tbody> {{ evRows }} </tbody></table>
      </div>
    }}

private def listenerDetailPage (name : String) : IO (Option Html) := do
  match ← listenerDetailContent name with
  | none => return none
  | some content =>
    return some (shell s!"Listener {name}" (some s!"/sse/listeners/{name}") content)

private def tasksContent : IO Html := do
  let tasks ← TaskStore.loadAllTasks
  let shown := tasks.toList.take 50
  let rows : Html :=
    if shown.isEmpty then
      {{ <tr><td colspan="6" class="empty"> "No tasks yet" </td></tr> }}
    else
      Html.fromList (shown.map fun r =>
        let snip : String := (r.prompt.take 80).toString
        {{ <tr>
          <td class="m"> <a href=s!"/tasks/{r.id}"> s!"{esc r.id}" </a> </td>
          <td> {{ badge (tStText r.status) }} </td>
          <td class="m trunc"> s!"{esc r.fork.toString}" </td>
          <td class="m"> s!"{r.createdAt}" </td>
          <td class="m"> s!"{esc (r.series.getD "")}" </td>
          <td class="trunc"> s!"{esc snip}" </td>
        </tr> }})
  return {{
    <h1> "Tasks" </h1>
    <div class="card">
      <table>
        <thead>
          <tr>
            <th> "ID" </th> <th> "Status" </th> <th> "Fork" </th>
            <th> "Created" </th> <th> "Series" </th> <th> "Prompt" </th>
          </tr>
        </thead>
        <tbody> {{ rows }} </tbody>
      </table>
    </div>
  }}

private def tasksPage : IO Html := do
  return shell "Tasks" (some "/sse/tasks") (← tasksContent)

-- Log rendering: parse each JSONL line produced by `Sandbox.launchAgent` and
-- render it as a structured row instead of dumping raw JSON. Lines that fail
-- to parse fall back to a small monospaced raw row.

private def jStr? (j : Json) (k : String) : Option String :=
  match j.getObjValAs? String k with | .ok s => some s | _ => none

private def jStrD (j : Json) (k : String) (d := "") : String :=
  (jStr? j k).getD d

private def jNat? (j : Json) (k : String) : Option Nat :=
  j.getObjValAs? Nat k |>.toOption

private def truncStr (s : String) (n : Nat) : String :=
  if s.length ≤ n then s
  else String.ofList (s.toList.take n) ++ "…"

private def renderToolInput (input : Json) : Html :=
  let cmd := jStrD input "command"
  let fp  := jStrD input "file_path"
  let pat := jStrD input "pattern"
  let desc := jStrD input "description"
  if !cmd.isEmpty then
    {{ <div>
      {{ if desc.isEmpty then ({{ "" }} : Html)
         else {{ <div class="meta"> s!"{esc desc}" </div> }} }}
      <pre> s!"$ {esc cmd}" </pre>
    </div> }}
  else if !fp.isEmpty then
    {{ <div class="meta"> s!"{esc fp}" </div> }}
  else if !pat.isEmpty then
    {{ <div class="meta"> s!"pattern: {esc pat}" </div> }}
  else if !desc.isEmpty then
    {{ <div class="meta"> s!"{esc desc}" </div> }}
  else
    let raw := truncStr (Json.compress input) 280
    {{ <pre> s!"{esc raw}" </pre> }}

private def renderLogEvent (j : Json) : Html :=
  match jStrD j "type" with
  | "init" =>
    let sid   := jStrD j "session_id"
    let model := jStrD j "model"
    let sidShort := truncStr sid 12
    {{ <div class="le">
      <span class="lk lk-init"> "init" </span>
      <div class="lbody"> s!"model: {esc model}"
        <div class="meta"> s!"session {esc sidShort}" </div>
      </div>
    </div> }}
  | "system" =>
    {{ <div class="le">
      <span class="lk lk-system"> "system" </span>
      <div class="lbody"> s!"{esc (jStrD j "subtype")}" </div>
    </div> }}
  | "assistant" =>
    let item := (j.getObjVal? "item").toOption.getD (Json.mkObj [])
    match jStrD item "type" with
    | "thinking" =>
      {{ <div class="le">
        <span class="lk lk-think"> "thinking" </span>
        <div class="lbody thinking"> s!"{esc (jStrD item "text")}" </div>
      </div> }}
    | "tool_use" =>
      let name := jStrD item "name"
      let input := (item.getObjVal? "input").toOption.getD (Json.mkObj [])
      {{ <div class="le">
        <span class="lk lk-tool"> "tool" </span>
        <div class="lbody">
          <span class="toolname"> s!"{esc name}" </span>
          {{ renderToolInput input }}
        </div>
      </div> }}
    | "text" =>
      {{ <div class="le">
        <span class="lk lk-text"> "text" </span>
        <div class="lbody"> s!"{esc (jStrD item "text")}" </div>
      </div> }}
    | other =>
      {{ <div class="le">
        <span class="lk lk-unk"> s!"{esc other}" </span>
        <div class="lbody"> s!"{esc (truncStr (Json.compress item) 240)}" </div>
      </div> }}
  | "tool_result" =>
    let stdout := jStrD j "stdout"
    let stderr := jStrD j "stderr"
    {{ <div class="le">
      <span class="lk lk-out"> "output" </span>
      <div class="lbody">
        {{ if stdout.isEmpty then ({{ "" }} : Html)
           else {{ <pre> s!"{esc stdout}" </pre> }} }}
        {{ if stderr.isEmpty then ({{ "" }} : Html)
           else {{ <div>
             <div class="meta err"> "stderr" </div>
             <pre> s!"{esc stderr}" </pre>
           </div> }} }}
      </div>
    </div> }}
  | "result" =>
    let sub := jStrD j "subtype"
    let res := jStrD j "result"
    let isErr := sub.startsWith "error"
    let kindCls := if isErr then "lk lk-err" else "lk lk-result"
    let turns := match jNat? j "num_turns"   with | some n => s!"{n} turns" | none => ""
    let dur   := match jNat? j "duration_ms" with | some n => s!"{n / 1000}s" | none => ""
    let cost := match j.getObjVal? "total_cost_usd" |>.toOption with
      | some v => s!"${v.compress}"
      | none   => ""
    let metaStr := [turns, dur, cost].filter (! ·.isEmpty) |> String.intercalate " · "
    {{ <div class="le">
      <span class=s!"{kindCls}"> s!"{esc sub}" </span>
      <div class="lbody">
        {{ if res.isEmpty then ({{ "" }} : Html)
           else {{ <pre> s!"{esc res}" </pre> }} }}
        {{ if metaStr.isEmpty then ({{ "" }} : Html)
           else {{ <div class="meta"> s!"{esc metaStr}" </div> }} }}
      </div>
    </div> }}
  | "unknown" =>
    {{ <div class="le">
      <span class="lk lk-unk"> "unknown" </span>
      <div class="lbody"> s!"{esc (jStrD j "event_type")}" </div>
    </div> }}
  | other =>
    {{ <div class="le">
      <span class="lk lk-unk"> s!"{esc other}" </span>
      <div class="lbody"> s!"{esc (truncStr (Json.compress j) 240)}" </div>
    </div> }}

private def renderLog (content : String) : Html :=
  let lines := content.splitOn "\n" |>.filter (! ·.trimAscii.isEmpty)
  if lines.isEmpty then
    {{ <p class="empty" style="padding:16px"> "No log file." </p> }}
  else
    let rows : List Html := lines.map fun line =>
      match Json.parse line with
      | .ok j    => renderLogEvent j
      | .error _ => {{ <div class="lraw"> s!"{esc (truncStr line 400)}" </div> }}
    {{ <div class="log"> {{ Html.fromList rows }} </div> }}

private def taskDetailContent (id : String) : IO (Option Html) := do
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
    let logSec : Html := renderLog logContent
    let details : Html :=
      {{ <table>
        <tr><th> "ID" </th>      <td class="m"> s!"{esc id}" </td></tr>
        <tr><th> "Status" </th>  <td> {{ badge st }} </td></tr>
        <tr><th> "Fork" </th>    <td class="m"> s!"{esc fork.toString}" </td></tr>
        <tr><th> "Created" </th> <td class="m"> s!"{createdAt}" </td></tr>
      </table> }}
    let content : Html :=
      {{ <h1> "Task " <span class="m"> s!"{esc id}" </span> </h1>
        <div class="card"> {{ details }} </div>
        <h2> "Prompt" </h2>
        <div class="card"> <pre class="pre"> s!"{esc prompt}" </pre> </div>
        <h2> "Log" </h2>
        <div class="card"> {{ logSec }} </div> }}
    return some content

private def taskDetailPage (id : String) : IO (Option Html) := do
  match ← taskDetailContent id with
  | none => return none
  | some content => return some (shell s!"Task {id}" (some s!"/sse/tasks/{id}") content)

/-- Render the current dynamic fragment for an SSE endpoint kind.
    Returns `none` if the kind is unknown or the referenced resource is missing.
    The fragment is the same verso `Html` value that the corresponding page
    wraps in `shell` — rendered to a string via `Html.asString`. -/
private def renderFragment (kind : String) : IO (Option String) := do
  let toStr (h : Html) : String := Html.asString h (breakLines := false)
  if kind == "overview"  then return some (toStr (← overviewContent))
  if kind == "queue"     then return some (toStr (← queueContent))
  if kind == "listeners" then return some (toStr (← listenersContent))
  if kind == "concerts"  then return some (toStr (← concertsContent))
  if kind == "tasks"     then return some (toStr (← tasksContent))
  if kind.startsWith "concerts/" then
    let id := (kind.drop "concerts/".length).toString
    match ← concertDetailContent id with
    | none   => return none
    | some h => return some (toStr h)
  if kind.startsWith "listeners/" then
    let name := (kind.drop "listeners/".length).toString
    match ← listenerDetailContent name with
    | none   => return none
    | some h => return some (toStr h)
  if kind.startsWith "tasks/" then
    let id := (kind.drop "tasks/".length).toString
    match ← taskDetailContent id with
    | none   => return none
    | some h => return some (toStr h)
  return none

end -- HTML generation section

/-- Encode a payload as a single SSE `message` event. Each `\n` in the payload
    becomes a separate `data:` line per the SSE spec; the browser rejoins them
    with `\n` in `event.data`. The frame is terminated by a blank line. -/
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

-- Route dispatch

private def dispatch (req : HttpRequest) (pw : String) : IO HttpResponse := do
  if !checkBasicAuth req.headers pw then return unauthorizedResp
  if req.method != "GET" then return { status := 400, headers := #[], body := "Bad Request" }
  let path := pathOnly req.path
  if path == "/" then return htmlResp (← overviewPage)
  if path == "/queue" then return htmlResp (← queuePage)
  if path == "/concerts" then return htmlResp (← concertsPage)
  if path.startsWith "/concerts/" then
    let id := (path.drop "/concerts/".length).toString
    match ← concertDetailPage id with
    | some html => return htmlResp html
    | none => return notFound s!"Concert not found: {id}"
  if path == "/listeners" then return htmlResp (← listenersPage)
  if path.startsWith "/listeners/" then
    let name := (path.drop "/listeners/".length).toString
    match ← listenerDetailPage name with
    | some html => return htmlResp html
    | none => return notFound s!"Listener not found: {name}"
  if path == "/tasks" then
    match queryParam req.path "id" with
    | some id =>
      match ← taskDetailPage id with
      | some html => return htmlResp html
      | none => return notFound s!"Task not found: {id}"
    | none => return htmlResp (← tasksPage)
  if path.startsWith "/tasks/" then
    let id := (path.drop 7).toString
    match ← taskDetailPage id with
    | some html => return htmlResp html
    | none => return notFound s!"Task not found: {id}"
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
  let payloadOpt ← try renderFragment kind catch _ => pure none
  match payloadOpt with
  | none => return -- unknown kind or vanished resource; close stream
  | some payload =>
    let ok ← try
      let _ ← awaitTcp (← client.send #[(sseFrame payload).toUTF8])
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
