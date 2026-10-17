# remote interactive sessions

A **session** is a conversation with an agent, hosted by the daemon and reachable over the HTTP
API. A CLI, the dashboard and a mobile app are all clients of the same endpoints; nothing in the
server is specific to any of them.

This is a third shape alongside the two orchestra already had. A **queued task** is one prompt in
and one run out, with multi-turn available only as `--continues` / `orchestra resume` — a new run
per turn, minutes apart. **`orchestra interactive`** is a real conversation, in the same sandbox
with the same credentials and MCP tools, but strictly local: it spawns the vendor TUI with
inherited stdio, parses nothing, logs nothing, and is unreachable from anywhere but the terminal
on the daemon's host. A session is the conversation without the terminal.

## the shape of it

```
  orchestra chat  ┐
  dashboard       ├─ HTTPS ─► orchestrad dashboard ─┬─ writes ─► daemon.sock ─┐
  mobile app      ┘                                 │                         │
                                                    └─ reads ──► <data>/orchestra.db
                                                                        ▲     │
                                                 orchestrad queue ──────┘     │
                                                 (session manager) ◄──────────┘
                                                        │
                                      landrun sandbox ── agent process
                                                        │  (one process, many turns)
                                                        └─ MCP server, clone slot, GitHub token
```

Two processes, one database. `orchestrad dashboard` and `orchestrad queue` are separate
containers in the compose deployment sharing `/data`, and that split dictates the design:
**writes travel over the daemon's control socket**, because only the daemon holds a live process
handle, and **reads are queries**, so they answer identically whether the API and the daemon are
one process or two.

There is no new transport. Server-Sent Events already carry the session cookie, survive proxies
and work on mobile; `Std.Http` is an HTTP/1.1 server with no WebSocket upgrade, and a socket
would buy nothing over the POST-in / SSE-out pair.

## what a session holds

For its whole lifetime, not per turn:

| held | acquired by | released by |
|---|---|---|
| a clone slot, reserved | `Repo.ensureCloned`, `Repo.ensureSlot` | teardown, in a `finally` |
| a GitHub App installation token | `GitHub.createInstallationToken` | expiry, refreshed through the MCP tool |
| the MCP server, which holds the PAT | `Server.start` | its own `shutdown` |
| a resolved authentication source | `Usage.resolveLabel`, `Usage.markUsed` | — |
| the agent process | `Sandbox.launchStreaming` | kill, on end or crash |

Reserving the clone slot for the session — in the same table the queue claims from — is what
stops a queued task taking the slot and resetting the working tree in the middle of a
conversation. A session is repository-bound like every other unit of work here: it takes an
`upstream`/`fork` pair and gets the sandbox, the credentials and the tools a task would get.

States are `starting → idle → running → idle → … → ended | failed`. A turn begins when a message
is written to the agent's stdin and ends when its stream emits a result.

## the API

Seven routes, under the same rules as every other route: `Authorization: Bearer` or the session
cookie, and `Content-Type: application/json` on everything that is not a `GET`.

| method | path | body | success | errors |
|---|---|---|---|---|
| `GET` | `/api/v1/interactive` | — | collection of sessions | `400` bad paging |
| `POST` | `/api/v1/interactive` | `{upstream,fork,backend?,model?,budget?,tools?,systemPrompt?,resumeFrom?}` | `201` + the session | `400` malformed or unsupported backend; `409` daemon unreachable, or the session cap is reached |
| `GET` | `/api/v1/interactive/{id}` | — | the session | `404` |
| `DELETE` | `/api/v1/interactive/{id}` | — | `204` | `404`; `409` daemon unreachable |
| `GET` | `/api/v1/interactive/{id}/events` | — | collection of transcript events, `?after=<seq>` | `404`; `400` bad parameter |
| `POST` | `/api/v1/interactive/{id}/messages` | `{"text": "…"}` | `202` + `{seq}` | `404`; `400` empty text; `409` not idle, ended, or daemon unreachable |
| `POST` | `/api/v1/interactive/{id}/interrupt` | `{}` | `200` + `{id}` | `404`; `409` no turn running, or daemon unreachable |

Both reads are also Server-Sent Events at the same path under `/sse/v1/`, as every read in this
API is.

`409` covering "the daemon is not answering" is what `POST /api/v1/queue/{id}/cancel` already
does, and for the same reason: it is a statement about the daemon rather than about the server
answering.

### the transcript stream

`/sse/v1/interactive/{id}/events?after=<seq>` is the one stream that cannot re-send its whole
payload on every tick — a transcript grows, and re-sending it in full is quadratic in the length
of the conversation. It carries a cursor instead: each frame holds only what follows the last one
sent, with `id: <seq>` on every frame, so a browser's `EventSource` reconnect sends
`Last-Event-ID` and resumes exactly where it dropped. `?after=` is the same thing for clients
that are not browsers. It ticks faster than the dashboard streams, because chat latency is felt.

## where a session is kept

Two tables of `<data>/orchestra.db`, written by the daemon and read by the API (see
[storage.md](storage.md)):

| table | contents |
|---|---|
| `interactive_session` | the session record, one row, rewritten on every state change |
| `interactive_event` | the transcript, one row per event, keyed `(session_id, seq)` |

A monotone `seq` is what makes the transcript cheap to tail from another process with a cursor,
and what makes a dropped stream lossless to resume: `readEvents id after atMost` is `WHERE
session_id = id AND seq > after ORDER BY seq LIMIT atMost`, so a poll that finds nothing new
costs an index probe rather than the whole conversation.

Both writes are single statements, so there is no half-written record and no torn line for a
reader to step over — which is what the write-and-rename and the tolerant tail decoding of the
files this replaced were for. That tolerance survives only in the one-time import of a legacy
`events.jsonl`, which is the one place a line torn by a killed daemon can still turn up.

A transcript row stores the whole event as JSON in one `doc` column rather than a column per
field, so a reader hands the document straight back to the client and a field a newer orchestra
writes is not lost on the way through.

### the session record

Instants are RFC 3339 UTC in a `...At` field, durations are integer seconds in a `...Seconds`
field, and absent is `null` — the conventions the rest of the API holds to.

```json
{
  "id": "i20260821T100412-a3f1",
  "status": "starting|idle|running|ended|failed",
  "createdAt": "2026-08-21T10:04:12Z",
  "lastActivityAt": "2026-08-21T10:11:03Z",
  "endedAt": null,
  "upstream": "owner/repo",
  "fork": "your-org/repo",
  "backend": "claude",
  "model": null,
  "budget": 20.0,
  "slot": 0,
  "agentSessionId": "b7c1…",
  "turnCount": 3,
  "costUsd": 0.42,
  "lastEventSeq": 128,
  "title": "why does the queue stall when…",
  "error": null
}
```

`agentSessionId` is the agent CLI's own session id — the one `--resume` takes. Persisting it is
what makes a dead session recoverable. `title` is the first user message, truncated, so a list of
sessions reads as a list of conversations rather than of ids.

### the transcript

One envelope per event, `seq` monotone from 1, stored whole in the row's `doc` column:

```json
{"seq":12,"occurredAt":"…","kind":"user","text":"add a test for the retry path"}
{"seq":13,"occurredAt":"…","kind":"turnStarted","turn":3}
{"seq":14,"occurredAt":"…","kind":"agent","event":{"type":"assistant","item":{"type":"text","text":"…"}}}
{"seq":15,"occurredAt":"…","kind":"turnEnded","turn":3,"subtype":"success","costUsd":0.02,"durationSeconds":12}
{"seq":16,"occurredAt":"…","kind":"notice","level":"error","message":"the agent exited unexpectedly"}
```

`kind: "agent"` wraps a `StreamFormat.Event` unchanged, so a client that can already render a
task log can render a transcript. The envelope adds what the agent's own stream cannot say: user
turns, turn boundaries, and the daemon's notices — a crash, a usage limit, an interrupt, an idle
reap.

## driving the agent

One process per session, spawned once inside the sandbox and fed turns over stdin, rather than
one process per turn. For Claude Code that is:

```
claude --print --input-format stream-json --output-format stream-json --verbose \
       --session-id <uuid> --replay-user-messages \
       --dangerously-skip-permissions --mcp-config <path> --max-budget-usd <n> [--model …]
```

`--session-id` **assigns** the session id rather than scraping it from the stream, so the daemon
knows what to `--resume` even if the process dies before it says anything.

The transcript has one writer for each kind of line and they do not overlap: the daemon records
the turn it sends, because it is the thing that sent it, and the agent's own events arrive on the
stream. (`--replay-user-messages`, which echoes stdin turns back on stdout, would buy a second
copy of every turn and a second writer racing the first, so it is deliberately not passed.)

`--max-budget-usd` bounds the **whole session**, not a turn; a conversation therefore wants a
larger default than a one-shot task's. Exhausting it arrives as a result subtype the session
manager turns into a notice and an `ended` session, rather than a silent stall.

Only backends whose CLI can read turns from stdin can host a session. A backend that cannot says
so at creation — a `400` naming it — rather than silently falling back to something slower than
the caller asked for.

## when things go wrong

| event | what happens |
|---|---|
| the agent process crashes | a notice carrying the exit code, session `failed`, transcript kept, everything released |
| a usage limit mid-turn | recorded against the resolved source, a notice, session `ended` |
| the budget is exhausted | a notice, session `ended` — said distinctly from a crash |
| the daemon restarts | the processes are gone; every non-terminal session in the database is reconciled to `ended` at startup. A dead session never reads as a live one |
| reviving a session | not resurrection: a new session with `resumeFrom` inherits the repository and settings and `--resume`s the old agent session |
| the daemon is down, the API up | writes are `409` naming the daemon; reads keep working, straight off the database |
| a session sits idle too long | a notice, session `ended`, the slot released |

A session pins a clone slot and an agent process, so two limits bound them — a cap on concurrent
sessions and an idle timeout — configured like `parallel` and `parallel_per_repo` are. These are
capacity controls, not access controls: an abandoned browser tab should not hold a slot forever.

## clients

`orchestra chat` starts a session and attaches, re-attaches to one by id, lists them, and ends
one. It is a plain HTTP/SSE client — it resolves its URL and credential exactly as
`orchestra config` does — and renders events with the same formatter `orchestra run` prints, so a
chat looks like a run that answers back. Detaching is not ending: closing the client leaves the
session running, and ending it is a separate word.

The dashboard adds a page over the same routes. A native mobile app needs nothing built for it:
a bearer token and SSE are the whole contract. Two things it should know — the API is plain HTTP
behind one password, so put TLS in front of it, and no CORS headers are served, so a
*browser-based* client on another origin needs a proxy where a native app does not.

## a note on what this changes

`POST /api/v1/interactive` is the first route in this API that starts an agent. Everything before
it read state or edited configuration, and the documentation said plainly that nothing here
enqueues work — that being the daemon's control socket, which is not on the network. This adds a
way to run an agent, with a repository, credentials and tools, to a surface gated by the one
shared secret that already grants every read and every write. No separate credential and no
additional gate: the same secret, the same cookie, the same content-type rule as every other
write.

## residents

A **resident** is an identity with one session that never ends — the design is
[`design/resident-sessions.md`](design/resident-sessions.md); what is built so far is its phases 1
and 2 (the Zulip feed and watches).
An identity becomes one with a `resident` block in its `identity.json`:

```json
{
  "name": "secretary",
  "kleis_spec": "secretary",
  "resident": {
    "budget_per_day": 20,
    "process_idle_seconds": 1800,
    "pod_idle_seconds": 21600,
    "batch_window_seconds": 3,
    "model": "claude-opus-5-5",
    "feeds": [
      { "kind": "zulip", "site": "zulip.merten.dev",
        "notify": ["dm", "mention"],
        "allow_senders": ["christian@flavigny.de", "*@merten.dev", "id:8"],
        "watch_senders": "any" }
    ]
  }
}
```

Every field is optional and falls back to the `residents` block of `config.json`: `enabled`
(`true`), `budget_per_day` (`100`, USD), `process_idle_seconds` (`1800`), `pod_idle_seconds`
(`21600`), `batch_window_seconds` (`3`), `timezone` (`"UTC"`, for the budget day), `max_awake`
(`4`, residents with a process at once, counted apart from `interactive.max_sessions`),
`max_turns_per_hour` (`60`) and `volume_size` (the backend's own size when absent). The block
may carry `tools`, `agent` (the backend) and `feeds` (below); it may not carry `upstream` or
`fork`. A resident is reached through its inbox: its feeds, the chat, and listeners with
`deliver_to`.

**One session, by name, never ended.** At start-up the daemon creates each resident's session
asleep (id `resident-<name>`, no process), with no repository: a scratch workspace kept across
wakes (`<data>/workspaces/sessions/<id>` on landrun, its task volume on Kubernetes), no clone
slot, no repository-scoped tools, and the repository-less form of the kleis task facts. Its
kleis token is derived from the identity's source at every cold wake. What ends or fails another
session puts a resident to sleep instead — a dead process, a turn that could not be written, a
failed wake — and the next delivery wakes it, with a backoff; three failures in a row pause it
(`paused: failed`) until an operator resumes it. `DELETE /api/v1/interactive/{id}` is refused for
it. Only `orchestra resident reset` ends its conversation, by starting a new one in the same
session: the memory directory and the inbox carry over.

**The inbox.** Everything a resident is handed goes through `resident_inbox`: a person's message
(source `human`), a listener event (`listener:<name>`). Items are unique per
`(resident, source, event_id)`, so a source that replays delivers once. The daemon's delivery
loop, once a second, waits `batch_window_seconds` from the oldest ready item (not at all when a
person is waiting), wakes the resident if needed, renders every ready item into **one turn** and
marks them `sent_in` it; the turn's `result` marks them `confirmed`. A turn that never got a
result leaves its items ready, and the next turn carries them again headed "these may have
reached you already; your previous turn was interrupted". A message posted to a resident's
session (`POST /interactive/{id}/messages` or `/residents/{name}/messages`) is queued rather than
refused while it is busy: the answer is the turn's `seq` once it is sent, or `queued` with the
inbox item and why.

**The day's budget** is enforced by the process: every agent process a resident starts gets
`--max-budget-usd` = what is left of today's `budget_per_day` (spend is recorded per turn in
`resident_spend`). A process whose budget day has rolled over is restarted before its next turn,
to pick up the new allowance. A spent day pauses the resident (`paused: budget:<day>`); its inbox
keeps filling and the next day it is woken with everything that arrived.

**Asleep: warm, then cold.** After `process_idle_seconds` idle the agent process is stopped and
everything else kept — the environment (on Kubernetes, the pod), the MCP server, the credentials:
`warm`, and the next turn is a process start (`--resume`). After `pod_idle_seconds` the
environment goes too: `cold` (`dormant` on the record), and the next turn is a full wake. A warm
resident whose kleis token expires within two hours is woken cold instead, for a new one. On
Kubernetes a resident's pod has no `deadline_seconds`, and its task volume — keyed by its session
— is labelled `orchestra.dev/resident` and exempt from `retention_days`. A volume lost anyway (or
a backend that keeps none) starts a fresh conversation in the same session, said in the
transcript and at the head of the next turn. A daemon stop puts residents to sleep cold.

**`deliver_to`.** A listener action may name `"deliver_to": "<resident>"` instead of queueing a
task: each event's rendered `prompt_template` becomes an inbox item from `listener:<name>`, keyed
by the event's id, and is marked untrusted unless the action says `"trusted": true`.

**Feeds.** Each entry of `feeds` is a Zulip event queue the daemon holds open for the resident: a
listener named `resident-<name>-<feed>` (the feed's `name`, default `zulip`) with a `zulip-feed`
source and a `deliver_to` action, on the dashboard and switched with `orchestra listener
enable|disable` like any other. A listener file may declare one too (`"source": {"type":
"zulip-feed", "site": …}`, with `deliver_to`). Instead of polling, the daemon long-polls it, through
kleis with the **resident's own key**: a token derived from the identity's source, narrowed to
`zulip: only read` and `services: zulip`, task id `resident:<name>/feed:<feed>`, living 4h and
derived again half an hour before it expires, revoked when the feed stops; its revocation id is on
the listener's state so a daemon that died holding it has it revoked by the next. Requests go to
`https://<site>` through the proxy at kleis's `url` (not the pods' `proxy`), the token on curl's
config stdin, with kleis's CA. The feed reads `users/me`, registers a queue for messages
(`all_public_streams`), catches up from its checkpoint — the last message id it saw, on the
listener's state — with one `GET /messages` per term (`is:dm`, `is:mentioned`, the bot's name in
public streams, one per watch), then long-polls `GET /events` (`--max-time 120`), again at once;
an expired queue is registered again. A feed with no checkpoint starts at the newest message.

A message becomes an inbox item (source `zulip:<site>`, event id its message id) when it is not the
bot's own and: it is a DM, or mentions the bot (`mentioned`, `@**<bot name>**` in a public stream
it is not subscribed to, wildcards if `notify` has `wildcard`), from a sender in `allow_senders`;
or it is in a watched topic, from a sender `watch_senders` admits (`"any"` or a list). An entry of
`allow_senders` is an address pattern (`*` matches anything), matched against the address Zulip
shows and, where the server lets the bot see it, the real one (`delivery_email`) — or a user id,
`id:8`, for a server that hides addresses. Items are `trusted` when the sender is in
`allow_senders`; the turn lists them under "New on Zulip (<site>)", marking the others "[not from
an allowed sender]". A failure backs off up to five minutes and is shown on the listener
(`feed.error`, `feed.connection`, `feed.checkpoint`) and on `GET /api/v1/residents/{name}`.

**Watches.** A resident's MCP server, and only a resident's, offers `watch(feed?, key, until?,
note?)` — `key` is `{stream, topic?}`, `until` an instant or a span (`12h`, `3d`) — `unwatch(feed?,
key)` and `list_watches(feed?)`. `watch` asks the server, with the feed's own token, which streams
the bot can see, and refuses a private stream it is not subscribed to. Watches are kept in
`resident_watch`, read on every event and for the catch-up, and lapse on their own at `until`.

**Control.** `orchestra resident list|show|inbox|pause|resume|reset <name>` and
`orchestra resident send <name> <text…>` go through the API (`/api/v1/residents…`), as
`orchestra chat` does; `orchestra chat --resident <name>` attaches to its session.
