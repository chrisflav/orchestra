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

/-- Light theme stylesheet. Inlined into every shell. -/
private def dashCss : String :=
  ":root{--bg:#f7f8fa;--sf:#ffffff;--bd:#e4e7ec;--tx:#1f2937;--mu:#6b7280;--ac:#2563eb;--code:#f3f4f6}" ++
  "*{box-sizing:border-box;margin:0;padding:0}" ++
  "body{font-family:system-ui,-apple-system,'Segoe UI',sans-serif;background:var(--bg);color:var(--tx);font-size:14px;line-height:1.55}" ++
  "code,pre,.mono{font-family:ui-monospace,SFMono-Regular,Menlo,monospace}" ++
  "a{color:var(--ac);text-decoration:none}a:hover{text-decoration:underline}" ++
  "header{background:var(--sf);border-bottom:1px solid var(--bd);padding:12px 24px;display:flex;align-items:center;gap:24px}" ++
  "header .logo{font-weight:700;font-size:16px;color:var(--tx)}" ++
  "nav{display:flex;gap:16px}nav a{color:var(--mu);font-size:13px;padding:4px 0}" ++
  "nav a:hover{color:var(--tx)}nav a.active{color:var(--tx);border-bottom:2px solid var(--ac)}" ++
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
       "white-space:pre-wrap;word-break:break-word;max-height:600px;overflow-y:auto}" ++
  ".empty{padding:32px;text-align:center;color:var(--mu)}" ++
  ".loading{padding:48px;text-align:center;color:var(--mu);font-size:13px}" ++
  ".note{font-size:11px;color:var(--mu);margin-top:8px}" ++
  ".stats{display:grid;grid-template-columns:repeat(auto-fit,minmax(140px,1fr));gap:12px;margin-bottom:24px}" ++
  ".st{background:var(--sf);border:1px solid var(--bd);border-radius:8px;padding:16px;box-shadow:0 1px 2px rgba(0,0,0,.03)}" ++
  ".sv{font-size:26px;font-weight:700;color:var(--tx)}" ++
  ".sl{font-size:11px;color:var(--mu);text-transform:uppercase;letter-spacing:.05em;margin-top:2px}" ++
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
  ".kv{font-size:11px;color:var(--mu);margin-bottom:4px;text-transform:uppercase;letter-spacing:.05em}"

/-- Frontend JS bundle. Renders all dynamic content. Boots from the `data-page`
    / `data-api` / `data-sse` attributes on `<main>`: fetches the initial JSON
    state, paints it, then subscribes to the SSE stream for live updates.
    All HTML strings are built with single-quoted attributes so the Lean
    string literal stays free of internal `"` characters. -/
private def dashJs : String :=
"(function(){
'use strict';

function E(s){
  return String(s==null?'':s)
    .replace(/&/g,'&amp;')
    .replace(/</g,'&lt;')
    .replace(/>/g,'&gt;');
}

function trunc(s,n){
  s = String(s||'');
  return s.length<=n ? s : s.slice(0,n)+'…';
}

var BADGE_CLS = {running:'br',pending:'bp',done:'bd',completed:'bd',failed:'bf',cancelled:'bc',unfinished:'bc'};
function badge(s){
  var cls = BADGE_CLS[s] || 'bo';
  return `<span class='b ${cls}'>${E(s)}</span>`;
}

function table(headers, rows, emptyMsg){
  var c = headers.length;
  var body = rows.length===0
    ? `<tr><td colspan='${c}' class='empty'>${E(emptyMsg)}</td></tr>`
    : rows.join('');
  var ths = headers.map(function(h){return `<th>${E(h)}</th>`;}).join('');
  return `<div class='card'><table><thead><tr>${ths}</tr></thead><tbody>${body}</tbody></table></div>`;
}

function kvTable(rows){
  var trs = rows.map(function(r){
    var k=r[0], v=r[1], cls=r[2]||'m';
    return `<tr><th>${E(k)}</th><td class='${cls}'>${v}</td></tr>`;
  }).join('');
  return `<div class='card'><table>${trs}</table></div>`;
}

function statBox(value,label){
  return `<div class='st'><div class='sv'>${E(value)}</div><div class='sl'>${E(label)}</div></div>`;
}

function link(href,label,cls){
  return `<a href='${E(href)}' class='${cls||''}'>${E(label)}</a>`;
}

// -------- Log rendering --------

function logKind(label,cls,body){
  return `<div class='le'><span class='lk ${cls}'>${E(label)}</span><div class='lbody'>${body}</div></div>`;
}

function renderToolInput(input){
  input = input || {};
  var cmd = input.command || '';
  var fp = input.file_path || input.filePath || '';
  var pat = input.pattern || '';
  var desc = input.description || '';
  if (cmd){
    return (desc ? `<div class='meta'>${E(desc)}</div>` : '') + `<pre>$ ${E(cmd)}</pre>`;
  }
  if (fp)  return `<div class='meta'>${E(fp)}</div>`;
  if (pat) return `<div class='meta'>pattern: ${E(pat)}</div>`;
  if (desc) return `<div class='meta'>${E(desc)}</div>`;
  return `<pre>${E(trunc(JSON.stringify(input),280))}</pre>`;
}

function renderLogEvent(j){
  switch (j.type){
    case 'init':
      return logKind('init','lk-init',
        `model: ${E(j.model)}<div class='meta'>session ${E(trunc(j.session_id||'',12))}</div>`);
    case 'system':
      return logKind('system','lk-system', E(j.subtype||''));
    case 'assistant': {
      var it = j.item || {};
      if (it.type==='thinking')
        return logKind('thinking','lk-think', `<div class='thinking'>${E(it.text)}</div>`);
      if (it.type==='tool_use')
        return logKind('tool','lk-tool',
          `<span class='toolname'>${E(it.name)}</span>${renderToolInput(it.input||{})}`);
      if (it.type==='text')
        return logKind('text','lk-text', E(it.text));
      return logKind(it.type||'unknown','lk-unk', E(trunc(JSON.stringify(it),240)));
    }
    case 'tool_result': {
      var so = j.stdout||'';
      var se = j.stderr||'';
      var body = (so ? `<pre>${E(so)}</pre>` : '') +
                 (se ? `<div class='meta err'>stderr</div><pre>${E(se)}</pre>` : '');
      return logKind('output','lk-out', body);
    }
    case 'result': {
      var sub = j.subtype||'';
      var cls = (typeof sub==='string' && sub.indexOf('error')===0) ? 'lk-err' : 'lk-result';
      var parts = [];
      if (j.num_turns!=null) parts.push(`${j.num_turns} turns`);
      if (j.duration_ms!=null) parts.push(`${Math.floor(j.duration_ms/1000)}s`);
      if (j.total_cost_usd!=null) parts.push(`$${j.total_cost_usd}`);
      var meta = parts.join(' · ');
      var body = (j.result ? `<pre>${E(j.result)}</pre>` : '') +
                 (meta ? `<div class='meta'>${E(meta)}</div>` : '');
      return logKind(sub, cls, body);
    }
    case 'unknown':
      return logKind('unknown','lk-unk', E(j.event_type||''));
    default:
      return logKind(j.type||'unknown','lk-unk', E(trunc(JSON.stringify(j),240)));
  }
}

function renderLog(events){
  if (!events || events.length===0)
    return `<p class='empty' style='padding:16px'>No log file.</p>`;
  return `<div class='log'>${events.map(renderLogEvent).join('')}</div>`;
}

// -------- Page renderers --------

var pages = {};

pages.overview = function(d){
  var c = d.counts;
  var labels = [['running','Running'],['pending','Pending'],['failed','Failed'],
                ['concerts','Concerts'],['listeners','Listeners'],['totalTasks','Total Tasks']];
  var stats = `<div class='stats'>${
    labels.map(function(p){return statBox(c[p[0]], p[1]);}).join('')
  }</div>`;
  var qRows = d.activeQueue.map(function(e){
    return `<tr><td class='mono'>${link('/tasks/'+e.id, e.id)}</td>
      <td>${badge(e.status)}</td>
      <td class='m trunc'>${E(e.fork)}</td>
      <td class='m'>${E(e.createdAt)}</td>
      <td>${E(e.priority)}</td>
      <td class='m trunc'>${E(e.concertId||'')}</td></tr>`;
  });
  var qTable = table(['ID','Status','Fork','Created','Pri','Concert'], qRows, 'No active tasks');
  var tRows = d.recentTasks.map(function(r){
    return `<tr><td class='mono'>${link('/tasks/'+r.id, r.id)}</td>
      <td>${badge(r.status)}</td>
      <td class='m trunc'>${E(r.fork)}</td>
      <td class='m'>${E(r.createdAt)}</td>
      <td class='trunc'>${E(trunc(r.prompt,60))}</td></tr>`;
  });
  var tTable = table(['ID','Status','Fork','Created','Prompt'], tRows, 'No tasks yet');
  return `<h1>Overview</h1>${stats}<h2>Active Queue</h2>${qTable}<h2>Recent Tasks</h2>${tTable}`;
};

pages.queue = function(d){
  var cRows = d.concerts.map(function(r){
    return `<tr><td class='mono'>${link('/concerts/'+r.id, r.id)}</td>
      <td>${badge(r.status)}</td>
      <td class='m'>${E(r.startedAt)}</td>
      <td class='m'>${E(r.finishedAt||'')}</td>
      <td class='m trunc'>${E(r.name||r.workflowFile||'')}</td></tr>`;
  });
  var cTable = table(['Concert ID','Status','Started','Finished','Name / File'], cRows, 'No concerts');
  var eRows = d.entries.map(function(e){
    return `<tr><td class='mono'>${link('/tasks/'+e.id, e.id)}</td>
      <td>${badge(e.status)}</td>
      <td class='m trunc'>${E(e.fork)}</td>
      <td class='m'>${E(e.createdAt)}</td>
      <td>${E(e.priority)}</td>
      <td class='m'>${E(e.series||'')}</td>
      <td class='m'>${E(e.concertId||'')}</td></tr>`;
  });
  var eTable = table(['ID','Status','Fork','Created','Pri','Series','Concert'], eRows, 'No entries');
  return `<h1>Queue</h1><h2>Concerts</h2>${cTable}<h2>Entries</h2>${eTable}`;
};

pages.concerts = function(d){
  var rows = d.concerts.map(function(r){
    return `<tr><td class='mono'>${link('/concerts/'+r.id, r.id)}</td>
      <td>${badge(r.status)}</td>
      <td class='m'>${E(r.name||'')}</td>
      <td class='m trunc'>${E(r.workflowFile||'')}</td>
      <td class='m'>${E(r.startedAt)}</td>
      <td class='m'>${E(r.finishedAt||'')}</td></tr>`;
  });
  var t = table(['ID','Status','Name','Workflow','Started','Finished'], rows, 'No concerts');
  return `<h1>Concerts</h1>${t}`;
};

pages['concert-detail'] = function(d){
  if (!d || d.error) return `<h1>Concert not found</h1>`;
  var r = d.concert;
  var details = kvTable([
    ['ID', E(r.id), 'mono'],
    ['Status', badge(r.status), ''],
    ['Name', E(r.name||'—'), ''],
    ['Workflow', E(r.workflowFile||'—'), 'mono'],
    ['Started', E(r.startedAt), 'm'],
    ['Finished', E(r.finishedAt||'—'), 'm']
  ]);
  var stepRows = d.steps.map(function(s){
    return `<tr><td class='mono'>${link('/tasks/'+s.id, s.id)}</td>
      <td>${badge(s.status)}</td>
      <td class='m trunc'>${E(s.fork)}</td>
      <td class='m'>${E(s.createdAt)}</td>
      <td class='m trunc'>${E(s.concertStepKey||'')}</td></tr>`;
  });
  var steps = table(['Task','Status','Fork','Created','Step Key'], stepRows, 'No steps');
  return `<h1>Concert <span class='m mono'>${E(r.id)}</span></h1>${details}<h2>Steps</h2>${steps}`;
};

pages.listeners = function(d){
  var rows = d.listeners.map(function(l){
    var statusBadge = `<span class='b ${l.enabled?'bd':'bc'}'>${l.enabled?'enabled':'disabled'}</span>`;
    return `<tr><td>${link('/listeners/'+l.name, l.name)}</td>
      <td>${statusBadge}</td>
      <td class='m'>${E(l.sourceType)}</td>
      <td class='m'>${E(l.intervalSeconds)}s</td>
      <td class='m'>${E(l.lastChecked||'never')}</td>
      <td>${E(l.eventCount)}</td></tr>`;
  });
  var t = table(['Name','Status','Source','Interval','Last Checked','Events'], rows, 'No listeners');
  return `<h1>Listeners</h1>${t}`;
};

pages['listener-detail'] = function(d){
  if (!d || d.error) return `<h1>Listener not found</h1>`;
  var statusBadge = `<span class='b ${d.enabled?'bd':'bc'}'>${d.enabled?'enabled':'disabled'}</span>`;
  var cfgRows = [
    ['Name', E(d.name), ''],
    ['Status', statusBadge, ''],
    ['Interval', `${E(d.intervalSeconds)}s`, 'm'],
    ['Last Checked', E(d.lastChecked||'never'), 'm'],
    ['Events Seen', E(d.eventCount), ''],
    ['Source Type', E(d.sourceType), 'mono'],
    ['Source', E(d.sourceDetail), 'm']
  ];
  (d.sourceExtras||[]).forEach(function(p){ cfgRows.push([E(p[0]), E(p[1]), 'm']); });
  var cfg = kvTable(cfgRows);
  var act = kvTable([
    ['Mode', E(d.action.mode), 'mono'],
    ['Upstream', E(d.action.upstream||''), 'm'],
    ['Fork', E(d.action.fork||''), 'm'],
    ['Series', E(d.action.series||'—'), 'm'],
    ['Backend', E(d.action.backend||'—'), 'm'],
    ['Model', E(d.action.model||'—'), 'm'],
    ['Workflow', E(d.action.workflowPath||'—'), 'mono'],
    ['Priority', E(d.action.priority), '']
  ]);
  var tmpl = `<div style='padding:12px'><div class='kv'>PROMPT TEMPLATE</div><pre class='pre'>${E(d.action.promptTemplate)}</pre></div>`;
  var evRows = (d.recentEvents||[]).map(function(ev){return `<tr><td class='mono'>${E(ev)}</td></tr>`;});
  var evTable = `<div class='card'><table><tbody>${
    evRows.length===0 ? `<tr><td class='empty'>No events processed yet</td></tr>` : evRows.join('')
  }</tbody></table></div>`;
  return `<h1>Listener <span class='m'>${E(d.name)}</span></h1>
    <h2>Configuration</h2>${cfg}
    <h2>Action</h2><div class='card'>${act}${tmpl}</div>
    <h2>Processed Event IDs (${E(d.eventCount)})</h2>${evTable}`;
};

pages.tasks = function(d){
  var rows = d.tasks.map(function(r){
    return `<tr><td class='mono'>${link('/tasks/'+r.id, r.id)}</td>
      <td>${badge(r.status)}</td>
      <td class='m trunc'>${E(r.fork)}</td>
      <td class='m'>${E(r.createdAt)}</td>
      <td class='m'>${E(r.series||'')}</td>
      <td class='trunc'>${E(trunc(r.prompt,80))}</td></tr>`;
  });
  var t = table(['ID','Status','Fork','Created','Series','Prompt'], rows, 'No tasks yet');
  return `<h1>Tasks</h1>${t}`;
};

pages['task-detail'] = function(d){
  if (!d || d.error) return `<h1>Task not found</h1>`;
  var details = kvTable([
    ['ID', E(d.id), 'mono'],
    ['Status', badge(d.status), ''],
    ['Fork', E(d.fork), 'm'],
    ['Created', E(d.createdAt), 'm']
  ]);
  var prompt = `<div class='card'><pre class='pre'>${E(d.prompt)}</pre></div>`;
  var logHtml = renderLog(d.log);
  return `<h1>Task <span class='m'>${E(d.id)}</span></h1>${details}
    <h2>Prompt</h2>${prompt}
    <h2>Log</h2><div class='card'>${logHtml}</div>`;
};

// -------- Boot + live updates --------

function render(target, page, data){
  var fn = pages[page];
  if (!fn) { target.innerHTML = `<p class='empty'>Unknown page: ${E(page)}</p>`; return; }
  target.innerHTML = fn(data);
}

function fetchAndRender(target, page, url){
  return fetch(url, { credentials: 'same-origin' })
    .then(function(r){ return r.ok ? r.json() : { error: 'fetch failed (' + r.status + ')' }; })
    .then(function(data){ render(target, page, data); })
    .catch(function(e){ target.innerHTML = `<p class='empty'>Error: ${E(e.message)}</p>`; });
}

function subscribeSse(target, page, url){
  var es = new EventSource(url);
  es.onmessage = function(ev){
    try { render(target, page, JSON.parse(ev.data)); } catch (_) {}
  };
  window.addEventListener('beforeunload', function(){ es.close(); });
}

document.addEventListener('DOMContentLoaded', function(){
  var main = document.querySelector('main[data-page]');
  if (!main) return;
  var page = main.dataset.page;
  var api  = main.dataset.api;
  var sse  = main.dataset.sse;
  if (!api) return;
  fetchAndRender(main, page, api).then(function(){
    if (sse) subscribeSse(main, page, sse);
  });
});

})();"

/-- The single layout `Template` shared by every page. Each page is a thin
    call to `layout` that passes its identifier (used by the JS bundle to pick
    a renderer) plus optional API / SSE endpoints. -/
private def layout (title : String) (pageId : String) (api : Option String)
    (sse : Option String) (activeNav : String) : Html :=
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
        </nav>
      </header>
      <main data-page=s!"{pageId}" data-api=s!"{apiAttr}" data-sse=s!"{sseAttr}">
        <div class="loading"> "Loading…" </div>
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
