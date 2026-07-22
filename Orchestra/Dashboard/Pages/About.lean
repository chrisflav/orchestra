import VersoBlog

/-!
About page for the Orchestra dashboard, authored as a Verso `Page` document and
rendered to HTML by `blogMain`. Lives in its own module because the `#doc`
command takes over the file parser until end-of-file.
-/

open Verso Genre Blog

#doc (Page) "About" =>

Orchestra is a small daemon that schedules and runs _AI agent tasks_ against
GitHub repositories. Each task gets its own sandboxed checkout, an MCP server
that exposes a curated set of tools, and a structured event log captured to
disk.

# Concepts

A *task* is a single run of an agent against a fork of an upstream repository.
The task's _prompt_, _backend_, _budget_ and _tools_ are recorded when it is
enqueued; its stream of thinking, tool calls and outputs is logged as JSON
events and rendered on the task detail page.

A *listener* polls an external source — GitHub issues, comments, PR reviews,
shell commands, project dispatchers — on a fixed interval and enqueues a task
for every new event that matches its trigger.

A *concert* is a workflow that orchestrates several tasks in a defined
order, suspending and resuming between steps. Each step in a concert shows
up as its own queue entry with a back-reference to the parent run.

An *auth source* is one credential an agent backend can run on. A subscription
runs several usage limits at once — a rolling session window, a weekly total,
and weekly limits scoped to a single model family — so availability is a
question about a source _and a model_, not about the source alone. The Auth
page shows every configured source with the limits last reported for it, which
is where to look when the queue is full but nothing is running.

# Live updates

This dashboard is a fully static website. Its pages are authored as Verso
documents and generated ahead of time with Verso's site pipeline
(`orchestra dashboard generate`), so they can be served by any HTTP server —
including `orchestra dashboard serve --site`, which serves them next to the API
so that one port answers both. Each page fetches its data as JSON from the
`/api/...` endpoints of that backend. The same JSON is streamed over
Server-Sent Events at `/sse/...` every two seconds so the page stays current
without a full reload.
