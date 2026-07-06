import Verso.Doc
import Verso.Doc.Concrete

/-!
About page for the Orchestra dashboard, authored as a Verso document and
rendered to HTML at request time. Lives in its own module because the `#doc`
command takes over the file parser until end-of-file.
-/

open Verso.Doc.Concrete

#doc (Verso.Doc.Genre.none) "About Orchestra" =>

Orchestra is a small daemon that schedules and runs *AI agent tasks* against
GitHub repositories. Each task gets its own sandboxed checkout, an MCP server
that exposes a curated set of tools, and a structured event log captured to
disk.

# Concepts

A **task** is a single run of an agent against a fork of an upstream repository.
The task's *prompt*, *backend*, *budget* and *tools* are recorded when it is
enqueued; its stream of thinking, tool calls and outputs is logged as JSON
events and rendered on the task detail page.

A **listener** polls an external source — GitHub issues, comments, PR reviews,
shell commands, project dispatchers — on a fixed interval and enqueues a task
for every new event that matches its trigger.

A **concert** is a workflow that orchestrates several tasks in a defined
order, suspending and resuming between steps. Each step in a concert shows
up as its own queue entry with a back-reference to the parent run.

# Live updates

This dashboard is a fully static website — its page chrome is generated ahead of
time with Verso (`orchestra dashboard generate`) and can be served by any HTTP
server. Each page fetches its data as JSON from the `/api/...` endpoints of the
backend started with `orchestra dashboard serve`. The same JSON is streamed over
Server-Sent Events at `/sse/...` every two seconds so the page stays current
without a full reload.
