// Orchestra dashboard front-end.
// Renders every dynamic page from JSON fetched from the corresponding /api/...
// endpoint, then keeps it live by replacing the inner HTML on each SSE message.
// The page kind and endpoint are read from data-* attributes on the shell
// element (a wrapper div emitted by the Verso page). The JSON API base URL is
// read from window.ORCHESTRA_API_BASE, injected into each page's <head> by the
// Verso theme at generation time.

(function () {
  "use strict";

  // ---- helpers ----------------------------------------------------------

  function E(s) {
    return String(s == null ? "" : s)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function trunc(s, n) {
    s = String(s || "");
    return s.length <= n ? s : s.slice(0, n) + "…";
  }

  const BADGE_CLS = {
    running: "br", pending: "bp",
    done: "bd", completed: "bd",
    failed: "bf", rejected: "bf",
    cancelled: "bc", unfinished: "bc", abandoned: "bc", blocked: "bc",
    // issue statuses
    open: "bo", claimed: "bp", in_review: "br",
  };
  function badge(s) {
    const cls = BADGE_CLS[s] || "bo";
    return `<span class="b ${cls}">${E(s)}</span>`;
  }

  function table(headers, rows, emptyMsg) {
    const c = headers.length;
    const body = rows.length === 0
      ? `<tr><td colspan="${c}" class="empty">${E(emptyMsg)}</td></tr>`
      : rows.join("");
    const ths = headers.map((h) => `<th>${E(h)}</th>`).join("");
    return `<div class="card"><table><thead><tr>${ths}</tr></thead><tbody>${body}</tbody></table></div>`;
  }

  function kvTable(rows) {
    const trs = rows.map(([k, v, cls]) =>
      `<tr><th>${E(k)}</th><td class="${cls || "m"}">${v}</td></tr>`
    ).join("");
    return `<div class="card"><table>${trs}</table></div>`;
  }

  function statBox(value, label) {
    return `<div class="st"><div class="sv">${E(value)}</div><div class="sl">${E(label)}</div></div>`;
  }

  function link(href, label) {
    return `<a href="${E(href)}">${E(label)}</a>`;
  }

  // ---- log rendering ----------------------------------------------------

  function logKind(label, cls, body) {
    return `<div class="le"><span class="lk ${cls}">${E(label)}</span><div class="lbody">${body}</div></div>`;
  }

  function renderToolInput(input) {
    input = input || {};
    const cmd  = input.command || "";
    const fp   = input.file_path || input.filePath || "";
    const pat  = input.pattern || "";
    const desc = input.description || "";
    if (cmd) {
      return (desc ? `<div class="meta">${E(desc)}</div>` : "") +
             `<pre>$ ${E(cmd)}</pre>`;
    }
    if (fp)   return `<div class="meta">${E(fp)}</div>`;
    if (pat)  return `<div class="meta">pattern: ${E(pat)}</div>`;
    if (desc) return `<div class="meta">${E(desc)}</div>`;
    return `<pre>${E(trunc(JSON.stringify(input), 280))}</pre>`;
  }

  function renderLogEvent(j) {
    switch (j.type) {
      case "init":
        return logKind("init", "lk-init",
          `model: ${E(j.model)}<div class="meta">session ${E(trunc(j.session_id || "", 12))}</div>`);
      case "system":
        return logKind("system", "lk-system", E(j.subtype || ""));
      case "assistant": {
        const it = j.item || {};
        if (it.type === "thinking")
          return logKind("thinking", "lk-think",
            `<div class="thinking">${E(it.text)}</div>`);
        if (it.type === "tool_use")
          return logKind("tool", "lk-tool",
            `<span class="toolname">${E(it.name)}</span>${renderToolInput(it.input || {})}`);
        if (it.type === "text")
          return logKind("text", "lk-text", E(it.text));
        return logKind(it.type || "unknown", "lk-unk",
          E(trunc(JSON.stringify(it), 240)));
      }
      case "tool_result": {
        const so = j.stdout || "";
        const se = j.stderr || "";
        const body =
          (so ? `<pre>${E(so)}</pre>` : "") +
          (se ? `<div class="meta err">stderr</div><pre>${E(se)}</pre>` : "");
        return logKind("output", "lk-out", body);
      }
      case "result": {
        const sub = j.subtype || "";
        const cls = (typeof sub === "string" && sub.indexOf("error") === 0)
          ? "lk-err" : "lk-result";
        const parts = [];
        if (j.num_turns != null)     parts.push(`${j.num_turns} turns`);
        if (j.duration_ms != null)   parts.push(`${Math.floor(j.duration_ms / 1000)}s`);
        if (j.total_cost_usd != null) parts.push(`$${j.total_cost_usd}`);
        const meta = parts.join(" · ");
        const body =
          (j.result ? `<pre>${E(j.result)}</pre>` : "") +
          (meta    ? `<div class="meta">${E(meta)}</div>` : "");
        return logKind(sub, cls, body);
      }
      case "unknown":
        return logKind("unknown", "lk-unk", E(j.event_type || ""));
      default:
        return logKind(j.type || "unknown", "lk-unk",
          E(trunc(JSON.stringify(j), 240)));
    }
  }

  function renderLog(events) {
    if (!events || events.length === 0)
      return `<p class="empty" style="padding:16px">No log file.</p>`;
    return `<div class="log">${events.map(renderLogEvent).join("")}</div>`;
  }

  // ---- page renderers ---------------------------------------------------

  const pages = {};

  pages.overview = (d) => {
    const c = d.counts;
    const labels = [
      ["running", "Running"], ["pending", "Pending"], ["failed", "Failed"],
      ["concerts", "Concerts"], ["listeners", "Listeners"], ["totalTasks", "Total Tasks"],
    ];
    const stats = `<div class="stats">${
      labels.map(([k, l]) => statBox(c[k], l)).join("")
    }</div>`;
    const qRows = d.activeQueue.map((e) => `
      <tr>
        <td class="mono">${link("task/?id=" +e.id, e.id)}</td>
        <td>${badge(e.status)}</td>
        <td class="m trunc">${E(e.fork)}</td>
        <td class="m">${E(e.createdAt)}</td>
        <td>${E(e.priority)}</td>
        <td class="m trunc">${E(e.concertId || "")}</td>
      </tr>`);
    const qTable = table(
      ["ID", "Status", "Fork", "Created", "Pri", "Concert"],
      qRows, "No active tasks");
    const tRows = d.recentTasks.map((r) => `
      <tr>
        <td class="mono">${link("task/?id=" +r.id, r.id)}</td>
        <td>${badge(r.status)}</td>
        <td class="m trunc">${E(r.fork)}</td>
        <td class="m">${E(r.createdAt)}</td>
        <td class="trunc">${E(trunc(r.prompt, 60))}</td>
      </tr>`);
    const tTable = table(
      ["ID", "Status", "Fork", "Created", "Prompt"],
      tRows, "No tasks yet");
    return `<h1>Overview</h1>${stats}<h2>Active Queue</h2>${qTable}<h2>Recent Tasks</h2>${tTable}`;
  };

  pages.queue = (d) => {
    const cRows = d.concerts.map((r) => `
      <tr>
        <td class="mono">${link("concert/?id=" +r.id, r.id)}</td>
        <td>${badge(r.status)}</td>
        <td class="m">${E(r.startedAt)}</td>
        <td class="m">${E(r.finishedAt || "")}</td>
        <td class="m trunc">${E(r.name || r.workflowFile || "")}</td>
      </tr>`);
    const cTable = table(
      ["Concert ID", "Status", "Started", "Finished", "Name / File"],
      cRows, "No concerts");
    const eRows = d.entries.map((e) => `
      <tr>
        <td class="mono">${link("task/?id=" +e.id, e.id)}</td>
        <td>${badge(e.status)}</td>
        <td class="m trunc">${E(e.fork)}</td>
        <td class="m">${E(e.createdAt)}</td>
        <td>${E(e.priority)}</td>
        <td class="m">${E(e.series || "")}</td>
        <td class="m">${E(e.concertId || "")}</td>
      </tr>`);
    const eTable = table(
      ["ID", "Status", "Fork", "Created", "Pri", "Series", "Concert"],
      eRows, "No entries");
    return `<h1>Queue</h1><h2>Concerts</h2>${cTable}<h2>Entries</h2>${eTable}`;
  };

  pages.concerts = (d) => {
    const rows = d.concerts.map((r) => `
      <tr>
        <td class="mono">${link("concert/?id=" +r.id, r.id)}</td>
        <td>${badge(r.status)}</td>
        <td class="m">${E(r.name || "")}</td>
        <td class="m trunc">${E(r.workflowFile || "")}</td>
        <td class="m">${E(r.startedAt)}</td>
        <td class="m">${E(r.finishedAt || "")}</td>
      </tr>`);
    const t = table(
      ["ID", "Status", "Name", "Workflow", "Started", "Finished"],
      rows, "No concerts");
    return `<h1>Concerts</h1>${t}`;
  };

  pages["concert-detail"] = (d) => {
    if (!d || d.error) return `<h1>Concert not found</h1>`;
    const r = d.concert;
    const details = kvTable([
      ["ID",       E(r.id), "mono"],
      ["Status",   badge(r.status), ""],
      ["Name",     E(r.name || "—"), ""],
      ["Workflow", E(r.workflowFile || "—"), "mono"],
      ["Started",  E(r.startedAt), "m"],
      ["Finished", E(r.finishedAt || "—"), "m"],
    ]);
    const stepRows = d.steps.map((s) => `
      <tr>
        <td class="mono">${link("task/?id=" +s.id, s.id)}</td>
        <td>${badge(s.status)}</td>
        <td class="m trunc">${E(s.fork)}</td>
        <td class="m">${E(s.createdAt)}</td>
        <td class="m trunc">${E(s.concertStepKey || "")}</td>
      </tr>`);
    const steps = table(
      ["Task", "Status", "Fork", "Created", "Step Key"],
      stepRows, "No steps");
    return `<h1>Concert <span class="m mono">${E(r.id)}</span></h1>${details}<h2>Steps</h2>${steps}`;
  };

  pages.listeners = (d) => {
    const rows = d.listeners.map((l) => {
      const sb = `<span class="b ${l.enabled ? "bd" : "bc"}">${l.enabled ? "enabled" : "disabled"}</span>`;
      return `
        <tr>
          <td>${link("listener/?name=" +l.name, l.name)}</td>
          <td>${sb}</td>
          <td class="m">${E(l.sourceType)}</td>
          <td class="m">${E(l.intervalSeconds)}s</td>
          <td class="m">${E(l.lastChecked || "never")}</td>
          <td>${E(l.eventCount)}</td>
        </tr>`;
    });
    const t = table(
      ["Name", "Status", "Source", "Interval", "Last Checked", "Events"],
      rows, "No listeners");
    return `<h1>Listeners</h1>${t}`;
  };

  pages["listener-detail"] = (d) => {
    if (!d || d.error) return `<h1>Listener not found</h1>`;
    const sb = `<span class="b ${d.enabled ? "bd" : "bc"}">${d.enabled ? "enabled" : "disabled"}</span>`;
    const cfgRows = [
      ["Name",         E(d.name), ""],
      ["Status",       sb, ""],
      ["Interval",     `${E(d.intervalSeconds)}s`, "m"],
      ["Last Checked", E(d.lastChecked || "never"), "m"],
      ["Events Seen",  E(d.eventCount), ""],
      ["Source Type",  E(d.sourceType), "mono"],
      ["Source",       E(d.sourceDetail), "m"],
    ];
    (d.sourceExtras || []).forEach(([k, v]) => cfgRows.push([E(k), E(v), "m"]));
    const cfg = kvTable(cfgRows);
    const act = kvTable([
      ["Mode",     E(d.action.mode), "mono"],
      ["Upstream", E(d.action.upstream || ""), "m"],
      ["Fork",     E(d.action.fork || ""), "m"],
      ["Series",   E(d.action.series || "—"), "m"],
      ["Backend",  E(d.action.backend || "—"), "m"],
      ["Model",    E(d.action.model || "—"), "m"],
      ["Workflow", E(d.action.workflowPath || "—"), "mono"],
      ["Priority", E(d.action.priority), ""],
    ]);
    const tmpl = `<div style="padding:12px"><div class="kv">Prompt Template</div><pre class="pre">${E(d.action.promptTemplate)}</pre></div>`;
    const evRows = (d.recentEvents || []).map((ev) => `<tr><td class="mono">${E(ev)}</td></tr>`);
    const evTable = `<div class="card"><table><tbody>${
      evRows.length === 0
        ? `<tr><td class="empty">No events processed yet</td></tr>`
        : evRows.join("")
    }</tbody></table></div>`;
    return `
      <h1>Listener <span class="m">${E(d.name)}</span></h1>
      <h2>Configuration</h2>${cfg}
      <h2>Action</h2><div class="card">${act}${tmpl}</div>
      <h2>Processed Event IDs (${E(d.eventCount)})</h2>${evTable}`;
  };

  pages.tasks = (d) => {
    const rows = d.tasks.map((r) => `
      <tr>
        <td class="mono">${link("task/?id=" +r.id, r.id)}</td>
        <td>${badge(r.status)}</td>
        <td class="m trunc">${E(r.fork)}</td>
        <td class="m">${E(r.createdAt)}</td>
        <td class="m">${E(r.series || "")}</td>
        <td class="trunc">${E(trunc(r.prompt, 80))}</td>
      </tr>`);
    const t = table(
      ["ID", "Status", "Fork", "Created", "Series", "Prompt"],
      rows, "No tasks yet");
    return `<h1>Tasks</h1>${t}`;
  };

  pages["task-detail"] = (d) => {
    if (!d || d.error) return `<h1>Task not found</h1>`;
    const details = kvTable([
      ["ID",      E(d.id), "mono"],
      ["Status",  badge(d.status), ""],
      ["Fork",    E(d.fork), "m"],
      ["Created", E(d.createdAt), "m"],
    ]);
    const prompt = `<div class="card"><pre class="pre">${E(d.prompt)}</pre></div>`;
    const logHtml = renderLog(d.log);
    return `
      <h1>Task <span class="m">${E(d.id)}</span></h1>${details}
      <h2>Prompt</h2>${prompt}
      <h2>Log</h2><div class="card">${logHtml}</div>`;
  };

  pages.projects = (d) => {
    const rows = (d.projects || []).map((p) => {
      const c = p.counts || {};
      const active = (c.open || 0) + (c.claimed || 0) + (c.inReview || 0);
      return `
        <tr>
          <td>${link("project/?id=" +p.id, p.name || p.id)}</td>
          <td class="m mono trunc">${E(p.id)}</td>
          <td>${E(p.issueCount)}</td>
          <td>${active}</td>
          <td>${E(c.blocked || 0)}</td>
          <td>${E(c.completed || 0)}</td>
          <td class="m trunc">${E(p.defaultTarget || "—")}</td>
        </tr>`;
    });
    const t = table(
      ["Project", "ID", "Issues", "Active", "Blocked", "Completed", "Default Target"],
      rows, "No projects");
    return `<h1>Projects</h1>${t}`;
  };

  // ---- dependency graph -------------------------------------------------

  // Fill colour per issue status (keeps the graph readable at a glance).
  const NODE_FILL = {
    open: "#dbeafe", claimed: "#fef3c7", in_review: "#dcfce7",
    blocked: "#e5e7eb", completed: "#dcfce7",
    abandoned: "#e5e7eb", rejected: "#fee2e2",
  };
  const NODE_STROKE = {
    open: "#1d4ed8", claimed: "#a16207", in_review: "#15803d",
    blocked: "#4b5563", completed: "#166534",
    abandoned: "#9ca3af", rejected: "#b91c1c",
  };

  // Assign each node a layer = longest dependency chain reaching it, so that a
  // node always sits to the right of everything it depends on. Cycles (which
  // the model permits but the dispatcher never queues) are broken by ignoring
  // back-edges left over after a topological pass.
  function layerNodes(ids, edges) {
    const adj = new Map(), indeg = new Map();
    ids.forEach((id) => { adj.set(id, []); indeg.set(id, 0); });
    edges.forEach((e) => {
      if (adj.has(e.from) && indeg.has(e.to)) {
        adj.get(e.from).push(e.to);
        indeg.set(e.to, indeg.get(e.to) + 1);
      }
    });
    const deg = new Map(indeg);
    const queue = ids.filter((id) => deg.get(id) === 0);
    const order = [];
    while (queue.length) {
      const u = queue.shift();
      order.push(u);
      adj.get(u).forEach((v) => { deg.set(v, deg.get(v) - 1); if (deg.get(v) === 0) queue.push(v); });
    }
    ids.forEach((id) => { if (!order.includes(id)) order.push(id); }); // leftover cycle nodes
    const layer = new Map(ids.map((id) => [id, 0]));
    order.forEach((u) => adj.get(u).forEach((v) => {
      if (layer.get(v) < layer.get(u) + 1) layer.set(v, layer.get(u) + 1);
    }));
    return layer;
  }

  function depGraph(issues) {
    if (!issues || issues.length === 0) {
      return `<div class="card"><p class="empty">No issues in this project.</p></div>`;
    }
    const byId = new Map(issues.map((i) => [i.id, i]));
    // Dependency edges (solid): dep -> issue, i.e. dep must finish first.
    // Parent edges (dashed): parent -> child decomposition.
    const edges = [];
    issues.forEach((i) => {
      (i.dependencies || []).forEach((dep) => {
        if (byId.has(dep)) edges.push({ from: dep, to: i.id, kind: "dep" });
      });
      if (i.parentId && byId.has(i.parentId)) {
        edges.push({ from: i.parentId, to: i.id, kind: "parent" });
      }
    });

    const ids = issues.map((i) => i.id);
    const layer = layerNodes(ids, edges);
    // Bucket nodes by layer, preserving input order within a layer.
    const cols = new Map();
    ids.forEach((id) => {
      const l = layer.get(id);
      if (!cols.has(l)) cols.set(l, []);
      cols.get(l).push(id);
    });

    const COLW = 210, ROWH = 78, NW = 168, NH = 52, PAD = 20;
    const pos = new Map();
    let maxRow = 0;
    cols.forEach((col, l) => {
      col.forEach((id, r) => {
        pos.set(id, { x: PAD + l * COLW, y: PAD + r * ROWH });
        if (r > maxRow) maxRow = r;
      });
    });
    const maxLayer = Math.max(0, ...Array.from(cols.keys()));
    const width = PAD * 2 + maxLayer * COLW + NW;
    const height = PAD * 2 + maxRow * ROWH + NH;

    // Edges first so nodes paint on top.
    const edgeSvg = edges.map((e) => {
      const a = pos.get(e.from), b = pos.get(e.to);
      if (!a || !b) return "";
      const x1 = a.x + NW, y1 = a.y + NH / 2;
      const x2 = b.x, y2 = b.y + NH / 2;
      const mx = (x1 + x2) / 2;
      const dashed = e.kind === "parent";
      return `<path d="M${x1},${y1} C${mx},${y1} ${mx},${y2} ${x2},${y2}"
        fill="none" stroke="${dashed ? "#c084fc" : "#94a3b8"}" stroke-width="1.5"
        ${dashed ? 'stroke-dasharray="4 3"' : ""} marker-end="url(#arrow)"/>`;
    }).join("");

    const nodeSvg = ids.map((id) => {
      const i = byId.get(id), p = pos.get(id);
      const fill = NODE_FILL[i.status] || "#f3f4f6";
      const stroke = NODE_STROKE[i.status] || "#9ca3af";
      const short = E(String(id).slice(0, 7));
      const title = E(trunc(i.title || "", 26));
      const claim = i.claimedBy ? ` · ${E(i.claimedBy)}` : "";
      return `<g class="gnode">
        <title>${E(i.title || "")} — ${E(i.status)}${i.claimedBy ? " (claimed by " + E(i.claimedBy) + ")" : ""}</title>
        <rect x="${p.x}" y="${p.y}" width="${NW}" height="${NH}" rx="8"
          fill="${fill}" stroke="${stroke}" stroke-width="1.5"/>
        <text x="${p.x + 10}" y="${p.y + 20}" class="gid">${short}${claim}</text>
        <text x="${p.x + 10}" y="${p.y + 38}" class="gtitle">${title}</text>
      </g>`;
    }).join("");

    const legend = `<div class="glegend">
      <span><i class="lseg" style="background:#94a3b8"></i>depends on</span>
      <span><i class="lseg dash" style="background:#c084fc"></i>parent → child</span>
    </div>`;

    return `${legend}<div class="graph">
      <svg width="${width}" height="${height}" viewBox="0 0 ${width} ${height}"
           xmlns="http://www.w3.org/2000/svg">
        <defs>
          <marker id="arrow" markerWidth="8" markerHeight="8" refX="7" refY="4"
                  orient="auto" markerUnits="userSpaceOnUse">
            <path d="M0,0 L8,4 L0,8 z" fill="#94a3b8"/>
          </marker>
        </defs>
        ${edgeSvg}${nodeSvg}
      </svg></div>`;
  }

  pages["project-detail"] = (d) => {
    if (!d || d.error) return `<h1>Project not found</h1>`;
    const p = d.project, c = p.counts || {};
    const meta = kvTable([
      ["ID",             E(p.id), "mono"],
      ["Name",           E(p.name || "—"), ""],
      ["Description",    E(p.description || "—"), ""],
      ["Created",        E(p.createdAt), "m"],
      ["Default Target", E(p.defaultTarget || "—"), "m"],
      ["Issues",         E(p.issueCount), ""],
    ]);
    const stats = `<div class="stats">${[
      ["open", "Open"], ["claimed", "Claimed"], ["inReview", "In Review"],
      ["blocked", "Blocked"], ["completed", "Completed"], ["rejected", "Rejected"],
    ].map(([k, l]) => statBox(c[k] || 0, l)).join("")}</div>`;
    const rows = (d.issues || []).map((i) => `
      <tr>
        <td class="mono">${E(String(i.id).slice(0, 8))}</td>
        <td>${badge(i.status)}</td>
        <td class="trunc">${E(i.title)}</td>
        <td class="m mono trunc">${E(i.parentId || "")}</td>
        <td class="m">${(i.dependencies || []).length}</td>
        <td class="m">${E(i.claimedBy || "")}</td>
        <td class="m">${E(i.prCount)}</td>
      </tr>`);
    const t = table(
      ["Issue", "Status", "Title", "Parent", "Deps", "Claimed By", "PRs"],
      rows, "No issues");
    return `
      <h1>Project <span class="m">${E(p.name || p.id)}</span></h1>${meta}
      ${stats}
      <h2>Dependency Graph</h2>${depGraph(d.issues)}
      <h2>Issues</h2>${t}`;
  };

  // ---- boot + live updates ---------------------------------------------

  // Snapshot every scrollable region inside the page (the task-detail log,
  // prompt and per-event `<pre>` blocks) **and** the document scroll, so a
  // full innerHTML replace doesn't reset where the user is reading. If they
  // were pinned to the bottom of the log, keep them pinned so new events
  // follow.
  //
  // Indexing is done by `(className, ordinal-within-class)`, which is
  // resilient to extra inner `<pre>` elements appearing between renders
  // (a new tool result event would otherwise shift positional indices).
  const SCROLL_SELECTOR = ".log, .lbody pre, .pre";

  function snapshotScrolls(root) {
    const out = new Map();
    const counts = new Map();
    root.querySelectorAll(SCROLL_SELECTOR).forEach((el) => {
      const key = el.className || "_pre";
      const n = counts.get(key) || 0;
      counts.set(key, n + 1);
      if (el.scrollTop > 0 || el.scrollHeight > el.clientHeight) {
        const atBottom = (el.scrollHeight - el.scrollTop - el.clientHeight) < 4;
        out.set(`${key}:${n}`, { top: el.scrollTop, atBottom });
      }
    });
    return { winY: window.scrollY, els: out };
  }

  function restoreScrolls(root, snap) {
    // Force layout so scrollHeight is computed on the freshly-inserted nodes
    // before we try to set scrollTop. Without this some browsers clamp
    // scrollTop to 0 because layout hasn't decided that overflow exists yet.
    void root.offsetHeight;
    const counts = new Map();
    root.querySelectorAll(SCROLL_SELECTOR).forEach((el) => {
      const key = el.className || "_pre";
      const n = counts.get(key) || 0;
      counts.set(key, n + 1);
      const s = snap.els.get(`${key}:${n}`);
      if (!s) return;
      el.scrollTop = s.atBottom ? el.scrollHeight : s.top;
    });
    // Restore document scroll. Replacing innerHTML on <main> briefly
    // collapses its layout, and the browser can scroll the viewport back
    // toward the top while it rebuilds.
    if (snap.winY > 0) window.scrollTo(0, snap.winY);
  }

  const lastSig = new WeakMap();

  function render(target, page, data) {
    const fn = pages[page];
    if (!fn) {
      target.innerHTML = `<p class="empty">Unknown page: ${E(page)}</p>`;
      return;
    }
    // Skip if the payload hasn't changed since the last render: avoids
    // re-laying out the page (and thrashing scroll positions) when SSE
    // delivers identical data on consecutive ticks.
    let sig;
    try { sig = JSON.stringify(data); } catch (_) { sig = null; }
    if (sig != null && lastSig.get(target) === sig) return;
    if (sig != null) lastSig.set(target, sig);

    const snap = snapshotScrolls(target);
    target.innerHTML = fn(data);
    restoreScrolls(target, snap);
  }

  // API token. The backend requires it on every request, but it is NOT baked
  // into these static pages: the user supplies it at runtime, and it is kept in
  // localStorage for subsequent visits. It is sent as a bearer header on fetch
  // requests and — since EventSource cannot set custom headers — as a ?token=
  // query param on SSE streams.
  const TOKEN_KEY = "orchestraApiToken";
  const getToken = () => window.localStorage.getItem(TOKEN_KEY) || "";
  const setToken = (t) =>
    t ? window.localStorage.setItem(TOKEN_KEY, t)
      : window.localStorage.removeItem(TOKEN_KEY);

  // Prompt the user for the token; returns the new token, or "" if cancelled.
  function promptToken(message) {
    const t = window.prompt(
      message || "Enter the Orchestra dashboard API token:", getToken());
    if (t === null) return "";
    setToken(t.trim());
    return getToken();
  }

  function withToken(url) {
    const t = getToken();
    if (!t) return url;
    return url + (url.indexOf("?") >= 0 ? "&" : "?") + "token=" + encodeURIComponent(t);
  }

  function fetchAndRender(target, page, url) {
    function attempt(reprompted) {
      const t = getToken();
      const opts = { credentials: "same-origin" };
      if (t) opts.headers = { Authorization: "Bearer " + t };
      return fetch(url, opts).then((r) => {
        if (r.status === 401 && !reprompted) {
          setToken("");
          const msg = t
            ? "The API token was rejected. Enter the Orchestra dashboard API token:"
            : "Authentication required. Enter the Orchestra dashboard API token:";
          if (promptToken(msg)) return attempt(true);
          return { error: "unauthorized — a valid API token is required" };
        }
        return r.ok ? r.json() : { error: `fetch failed (${r.status})` };
      });
    }
    return attempt(false)
      .then((data) => render(target, page, data))
      .catch((e) => {
        target.innerHTML = `<p class="empty">Error: ${E(e.message)}</p>`;
      });
  }

  function subscribeSse(target, page, url) {
    if (!getToken()) return; // no token yet; fetch path prompts for it
    const es = new EventSource(withToken(url));
    es.onmessage = (ev) => {
      try { render(target, page, JSON.parse(ev.data)); } catch (_) { /* ignore */ }
    };
    window.addEventListener("beforeunload", () => es.close());
  }

  // Build the absolute API/SSE URLs for this page. The JSON API base comes from
  // window.ORCHESTRA_API_BASE (injected by the Verso theme). The rest come from
  // the shell wrapper's data-* attributes:
  //   data-page       renderer key, e.g. "overview" or "task-detail"
  //   data-endpoint   endpoint kind, e.g. "overview" or "tasks/" for details
  //   data-param      query-string key ("id"/"name") whose value is appended
  //                   to data-endpoint for detail pages (task/?id=…)
  // Pages without a data-endpoint (e.g. the static About page) are left alone.
  document.addEventListener("DOMContentLoaded", () => {
    const main = document.querySelector("[data-page]");
    if (!main) return;
    const page = main.dataset.page;
    const base = String(window.ORCHESTRA_API_BASE || "").replace(/\/+$/, "");
    let endpoint = main.dataset.endpoint || "";
    const param = main.dataset.param || "";
    if (!endpoint) return; // static page, nothing to fetch
    if (param) {
      const v = new URLSearchParams(location.search).get(param);
      if (!v) {
        main.innerHTML = `<p class="empty">Missing ?${E(param)}= parameter.</p>`;
        return;
      }
      endpoint += encodeURIComponent(v);
    }
    const api = base + "/api/" + endpoint;
    const sse = base + "/sse/" + endpoint;
    fetchAndRender(main, page, api).then(() => subscribeSse(main, page, sse));
  });
})();
