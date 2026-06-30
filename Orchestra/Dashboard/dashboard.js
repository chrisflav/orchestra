// Orchestra dashboard front-end.
// Renders every dynamic page from JSON fetched from the corresponding /api/...
// endpoint, then keeps it live by replacing the inner HTML on each SSE message.
// The page kind, API URL and SSE URL are read from data-* attributes on <main>
// emitted by the Verso shell.

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
    failed: "bf",
    cancelled: "bc", unfinished: "bc",
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
        <td class="mono">${link("/tasks/" + e.id, e.id)}</td>
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
        <td class="mono">${link("/tasks/" + r.id, r.id)}</td>
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
        <td class="mono">${link("/concerts/" + r.id, r.id)}</td>
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
        <td class="mono">${link("/tasks/" + e.id, e.id)}</td>
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
        <td class="mono">${link("/concerts/" + r.id, r.id)}</td>
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
        <td class="mono">${link("/tasks/" + s.id, s.id)}</td>
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
          <td>${link("/listeners/" + l.name, l.name)}</td>
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
        <td class="mono">${link("/tasks/" + r.id, r.id)}</td>
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

  // ---- boot + live updates ---------------------------------------------

  function render(target, page, data) {
    const fn = pages[page];
    if (!fn) {
      target.innerHTML = `<p class="empty">Unknown page: ${E(page)}</p>`;
      return;
    }
    target.innerHTML = fn(data);
  }

  function fetchAndRender(target, page, url) {
    return fetch(url, { credentials: "same-origin" })
      .then((r) => r.ok ? r.json() : { error: `fetch failed (${r.status})` })
      .then((data) => render(target, page, data))
      .catch((e) => {
        target.innerHTML = `<p class="empty">Error: ${E(e.message)}</p>`;
      });
  }

  function subscribeSse(target, page, url) {
    const es = new EventSource(url);
    es.onmessage = (ev) => {
      try { render(target, page, JSON.parse(ev.data)); } catch (_) { /* ignore */ }
    };
    window.addEventListener("beforeunload", () => es.close());
  }

  document.addEventListener("DOMContentLoaded", () => {
    const main = document.querySelector("main[data-page]");
    if (!main) return;
    const page = main.dataset.page;
    const api  = main.dataset.api;
    const sse  = main.dataset.sse;
    if (!api) return;
    fetchAndRender(main, page, api).then(() => {
      if (sse) subscribeSse(main, page, sse);
    });
  });
})();
