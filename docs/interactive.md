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
                                                    └─ reads ──► <data>/interactive/<id>/
                                                                        ▲     │
                                                 orchestrad queue ──────┘     │
                                                 (session manager) ◄──────────┘
                                                        │
                                      landrun sandbox ── agent process
                                                        │  (one process, many turns)
                                                        └─ MCP server, clone slot, GitHub token
```

Two processes, one filesystem. `orchestrad dashboard` and `orchestrad queue` are separate
containers in the compose deployment, and that split dictates the design: **writes travel over
the daemon's control socket**, because only the daemon holds a live process handle, and **reads
come off disk**, so they answer identically whether the API and the daemon are one process or
two.

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

## on disk

`<data>/interactive/<id>/`, written by the daemon and read by the API:

| file | contents |
|---|---|
| `session.json` | the session record, rewritten on every state change |
| `events.jsonl` | the transcript, appended and flushed per event |

Append-only JSONL with a monotone `seq` is what makes the transcript cheap to tail from another
process with a cursor, and what makes a dropped stream lossless to resume. A reader walks it
backwards and stops at the cursor, so a poll that finds nothing new costs one line rather than
the whole conversation.

Two details of the format are there to bound what a crash costs. The newline goes *before* each
record rather than after it, so a daemon killed mid-write leaves a fragment the next append
cannot splice itself onto — one lost event instead of two. And the file is decoded tolerantly at
the tail: a kill in the middle of a multi-byte character would otherwise make the whole
conversation unreadable rather than costing it one line.

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

One envelope per line, `seq` monotone from 1:

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
| the daemon restarts | the processes are gone; every non-terminal session on disk is reconciled to `ended` at startup. A dead session never reads as a live one |
| reviving a session | not resurrection: a new session with `resumeFrom` inherits the repository and settings and `--resume`s the old agent session |
| the daemon is down, the API up | writes are `409` naming the daemon; reads keep working off disk |
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
