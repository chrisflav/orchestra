# Resident sessions: agents that are always available

*Status: proposal, revised once after review (changes at the end). All three phases (§14) are
implemented — `docs/interactive.md` § residents says how they work. §15.1 and §15.2 were verified:
§15.1 with a finding that changed §7.3 (a stub that sources the env file, not the env file itself),
§15.2 with a Zulip long poll held open through kleis, answered by a heartbeat after ~46s.*

A **resident** is an identity with one session that never ends. It is not always running: when it
has had nothing to do for a while it is put to sleep — first its process, later its pod — and the
next thing that arrives for it wakes it, with its conversation intact. Things arrive through
**feeds**, long-lived connections orchestra holds on the resident's behalf; the first is Zulip. A
message for the resident lands in its **inbox**, and orchestra hands the inbox to the agent as a
turn.

The example throughout is the **secretary**: an identity with a Zulip bot on zulip.merten.dev
(its kleis spec allows read, send and react). When somebody writes to the bot, mentions it, or
posts in a topic it was asked to watch, the secretary answers within seconds — waking first if it
was asleep.

Nothing here is specific to the secretary or to Zulip: a resident is any identity with a
`resident` block, and a feed is any source that can say "here is the next thing, and its id".

## Terms

| term | meaning |
| --- | --- |
| **resident** | An identity whose `identity.json` has a `resident` block; it owns exactly one resident session. |
| **resident session** | An interactive session (`docs/interactive.md`) with a stable name (the identity's), no repository, and no end. |
| **feed** | A long-lived event source the daemon runs for a resident: a Zulip event queue first. |
| **inbox** | The resident's persistent queue of things to hand the agent: feed events and human messages. |
| **watch** | A topic (or other feed-defined key) the resident asked to be told about. |
| **asleep** | No agent process. **Warm**: asleep, pod kept. **Cold**: asleep, no pod. |
| **wake** | Starting the agent process again on the same conversation (`--resume`), as `Interactive.wake` does today. |
| **cycle** | Stopping the process between turns and waking it again at once — to rotate its kleis token or reset its budget. |

---

## 1. What exists, and what is missing

Interactive sessions already provide most of the life of a resident (`docs/interactive.md`,
`Orchestra/Interactive.lean`):

* a stable session id, record and transcript in the database, seen alike by the CLI, the dashboard
  and a phone;
* **dormancy**: `Manager.reap` (`Interactive.lean:879-912`) puts a session to sleep after
  `idle_timeout_seconds` (default 1800) and releases its process, slot, MCP server and pod;
* **wake** (`:719-755`): `send` on a dormant session starts a new process with `--resume`; on
  Kubernetes this needs a task volume (`carriesAgentState`, `Kubernetes.lean:1703`), which carries
  `$HOME` — and with it the conversation — from one pod to the next;
* identities (`AGENTS.md`, memory, the kleis source) applied to sessions.

What a resident needs and sessions do not have:

1. a session **without a repository** (`SessionSpec` requires `upstream`/`fork`,
   `Store.lean:221-223`; wake reserves a clone slot, `:737`);
2. **one session per identity, by name**, created by configuration, never ended by anything but an
   operator — today a crashed process (`:897-899`), a failed write to it (`:822-825`) and an
   exhausted budget (`:198-204`) all end or fail a session, and wake accepts only `dormant`
   (`:725`);
3. an **inbox**: `send` refuses a turn while one is running (`claimTurn`, `:766-780`);
4. **feeds**: listeners poll on intervals and can only queue *tasks*; nothing routes an event into a
   session;
5. **watches** the agent can set;
6. **token lifetime** beyond one kleis token;
7. **daily budgets** instead of one lifetime budget (default $20);
8. **storage that does not expire** (`retention_days`, default 14);
9. **wakes fast enough** on Kubernetes, where waking today means a new pod;
10. dashboard **control** of residents.

## 2. Declaring a resident

```json
// identities/secretary/identity.json
{
  "name": "secretary",
  "description": "Christian's secretary",
  "kleis_spec": "secretary",
  "resident": {
    "budget_per_day": 100,
    "process_idle_seconds": 1800,
    "pod_idle_seconds": 21600,
    "batch_window_seconds": 3,
    "model": "claude-opus-5-5",
    "feeds": [
      { "kind": "zulip", "site": "zulip.merten.dev",
        "notify": ["dm", "mention"],
        "allow_senders": ["christian@flavigny.de", "*@merten.dev"],
        "watch_senders": "any" }
    ]
  }
}
```

* `budget_per_day` — USD; default from `config.json` `residents.budget_per_day`, itself default
  **100** (§8).
* `process_idle_seconds`, `pod_idle_seconds` — when it falls asleep, warm then cold (§9).
* `batch_window_seconds` — §4.
* `feeds` — §5. A resident with no feed is still useful: people and `deliver_to` listener actions
  (§5.5) wake it.
* `model`, `tools`, `agent` as for a session; `upstream`/`fork` may not be set; the kleis source is
  the identity's, as for any task.

`config.json` gains `residents`: `enabled` (default `true`), defaults for the fields above,
`timezone` (for the budget day), and `max_awake` (default 4) — how many residents may have a
process at once, counted apart from `interactive.max_sessions`. A resident with pending work when
`max_awake` is reached waits for a place; the one asleep longest is never evicted for it, but the
one idle longest is put to sleep first (§10).

## 3. The resident session

### 3.1 One per identity, by name

The session store gains a nullable `resident` column with a unique index. At daemon start, for each
resident in configuration without a row, the daemon creates its session **asleep** — a record, no
process — so it exists, can be looked up by name, and is woken by its first turn. A resident
removed from configuration is left asleep and its feeds stop. `orchestra chat --resident secretary`
and the dashboard address it by name.

### 3.2 Never ended

For a resident session — and only for one — the paths that end or fail a session today put it to
sleep instead:

| today | for a resident |
| --- | --- |
| process dead at reap → `failed` | asleep; the next delivery wakes it (with backoff, §10) |
| `sendLine` fails → `failed` | asleep; the turn is redelivered (§4.2) |
| budget exhausted → `ended` | **paused: budget** until the next budget day (§8) |
| idle → `dormant` | asleep (warm, then cold, §9) |

Wake accepts `asleep` and `paused` (once the reason is gone). A resident ends only by an operator:
`orchestra resident reset secretary` ends the conversation and starts a fresh one asleep — the
identity's memory directory carries over, the conversation does not. Its inbox and its **watches**
(§6) carry over too: a watch is standing configuration the resident chose — "tell me about #ops >
deploys until the release" — not part of the conversation, and a reset is usually about the
conversation going wrong; the new conversation's first turn says that its watches are as they were
(`list_watches`, `unwatch`).

### 3.3 Without a repository

`SessionSpec.upstream`/`fork` become optional. A session without them gets the scratch workspace a
repository-less task gets today, reserves no clone slot, and is offered no repository-scoped tools.
Its kleis task facts are the repository-less form (`task_id`, `task_identity`, no fork or
upstream). On Kubernetes its task volume (§9) holds the workspace and `$HOME`.

### 3.4 Lifecycle

```
          ┌────────────────── inbox ready ──────────────────┐
          ▼                                                 │
cold ──pod+wake──►┐                                         │
warm ──wake──────►├► idle ──deliver──► running ──result──► idle ──process_idle──► warm ──pod_idle──► cold
                  │    ▲                                    │
                  │    └──────────── inbox ready ───────────┘
                  └◄── cycle (token, budget day): stop process, wake at once, pod kept
paused: budget ── next budget day ──► asleep (and woken if the inbox is ready)
```

What drives it is new: the **inbox** (§4). Whenever the inbox has something ready and the session is
not mid-turn, the daemon delivers — waking it first if asleep.

## 4. The inbox

### 4.1 Storage

```
resident_inbox(
  id            bigserial,
  resident      text,
  source        text,         -- "zulip:<site>", "human", "listener:<name>"
  event_id      text,         -- e.g. the Zulip message id
  trusted       boolean,      -- from an allowed sender (§12)
  received_at   timestamptz,
  payload       jsonb,        -- what the turn template renders
  sent_in       bigint null,  -- the turn it was written into
  confirmed     boolean       -- that turn produced a result
)
unique (resident, source, event_id)
```

The unique key deduplicates: a feed that reconnects and replays inserts each message once.

### 4.2 Delivery

One delivery loop per resident:

1. Wait until an item is **ready** (unsent, or sent in a turn that was never confirmed) and the
   session is not mid-turn.
2. **Batch**: wait `batch_window_seconds` from the oldest ready item. A human message from the chat
   UI goes at once and carries any ready feed items with it.
3. Check the budget day (§8) and the token (§7); cycle if needed.
4. Wake if asleep.
5. Render every ready item into **one turn**; write it; mark the items `sent_in` that turn.
6. On the turn's `result`, mark them `confirmed`. If the process dies or the write fails first, the
   items stay ready and are delivered again in the next turn, headed "these may have reached you
   already; your previous turn was interrupted".

Human messages go through the inbox too (source `human`), so a person writing while the resident is
busy is queued rather than refused. The API keeps returning the turn's event seq once it is sent, so
a client can stream it as today; while it waits it returns the inbox item id.

### 4.3 The turn

```
New on Zulip (zulip.merten.dev):
- DM from Alice <alice@merten.dev>, message 4711, 09:14:
  > Can you move my 3pm to Thursday?
- [not from an allowed sender] #ops > deploys, message 4712 (you are watching this topic), from Bob:
  > castor is back up
Reply in the same place with the zulip client (your token card says how).
```

Items from senders outside `allow_senders` are marked as such (§12). What the resident does is its
`AGENTS.md`'s business.

## 5. Feeds

### 5.1 Built on listeners

A feed is a **listener** of a new kind: it keeps the listener machinery — its state row, its rate
limit, its place on the dashboard, its enable switch — but instead of polling on `interval_seconds`
its source **long-polls**: the listener fiber calls it again as soon as it returns. Its events go to
the resident's inbox through `deliver_to` (§5.5) rather than becoming tasks. Feeds named in a
`resident` block are created as such listeners, owned by the resident; their checkpoint is the
listener state.

### 5.2 Zulip, through the resident's own key

Every Zulip connection a resident's feed makes goes **through kleis, with the resident's
account** — the same key the agent's own requests spend. The daemon never holds a Zulip
credential:

* The daemon derives a kleis token from the identity's source (`POST /.kleis/v1/derive`) with
  **two narrowings**: `zulip: only read` (the `read` group already includes the event queue), and
  `services: zulip` — refusing every other service the spec reaches. The second is a small kleis
  addition (§13). `task_id` is `resident:secretary/feed:zulip`; the token lives a few hours and is
  re-derived before it expires; at shutdown it is revoked, and at start any recorded in the previous
  run's state are revoked too.
* It speaks to `https://<site>` through the kleis proxy (`--proxy`, the token on curl's config stdin,
  never argv) with kleis's CA bundle (`Kleis.caBundle`), at kleis's `url` host — the address the
  daemon reaches kleis at, not the pods' `proxy`. The long poll uses `--max-time 120`.

The protocol is Zulip's real-time event queue:

1. `GET /api/v1/users/me` — the bot's own user id, for loop prevention.
2. `POST /api/v1/register` with `event_types=["message"]`, `all_public_streams=true`,
   `apply_markdown=false`. The answer's `last_event_id` and `max_message_id` mark where the queue
   starts.
3. **Catch up** from the checkpoint (the last message id delivered) to `max_message_id`: one
   `GET /api/v1/messages` per term — `is:dm`, `is:mentioned`, and one per watch
   (`channel:X topic:Y`) — because Zulip narrows combine their terms with AND. Insert, deduplicated by
   the unique key.
4. **Long-poll** `GET /api/v1/events?queue_id=…&last_event_id=…` (Zulip holds it up to ~90s and
   answers at once when a message arrives). On `BAD_EVENT_QUEUE_ID`, go back to 2.

Registering before catching up means nothing sent while reconnecting is missed; the overlap is
deduplicated.

A message becomes an inbox item when it is **not from the bot** and:

* `dm` — a direct message to the bot, from a sender in `allow_senders`;
* `mention` — the bot is mentioned (`flags` has `mentioned`; `wildcard_mentioned` if `notify`
  includes `wildcard`), from a sender in `allow_senders`. Zulip reports a mention only where the bot
  is **subscribed**; in a public stream it is not subscribed to, a mention reaches it only if the
  feed also matches `@**<bot name>**` in the content, which it does;
* it is in a **watched** topic (§6), from a sender `watch_senders` admits (`"any"`, or a list).

`allow_senders` decides who can wake the resident by asking for it; `watch_senders` who can reach it
through a topic it chose to watch. Either way the item records `trusted` = sender in
`allow_senders`.

### 5.3 kleis and the long poll

kleis relays a response as it arrives and has no upstream read timeout; its 30s limit is on a
client sending its request head, and its per-connection request cap does not apply to one curl per
poll. A 90s `GET /events` therefore passes through. This is to be confirmed end to end before
phase 2 is built on it (§15).

### 5.4 Later feeds

The notifications aggregator (`/api/sync` long-poll), mail (IMAP IDLE behind kleis-mail) and a
webhook endpoint fit the same shape. None is in this proposal.

### 5.5 Listeners into an inbox

Any listener action may name `"deliver_to": "<resident>"` instead of queueing a task: its event
becomes an inbox item (source `listener:<name>`, the listener's event id, `trusted` as the action
says). Every existing source — GitHub, taxis, shell — can reach a resident with no new code.

## 6. Watches

| MCP tool | effect |
| --- | --- |
| `watch(feed, key, until?, note?)` | add a watch; for Zulip `key` is `{stream, topic?}` |
| `unwatch(feed, key)` | remove it |
| `list_watches(feed?)` | what it is watching, with notes and expiry |

Watches live in `resident_watch(resident, feed, key jsonb, note, created_at, until)`; the feed reads
them on every event and for catch-up. `until` lets "until the release is out" expire on its own.

`watch` checks visibility first, through the feed's own token: a `GET /api/v1/streams` (or
`get_stream_id`) as the bot. A private stream the bot is not subscribed to is refused with that
reason — subscribing the bot stays a person's job, since the spec does not allow it.

Who may ask for a watch is the resident's to judge: its `AGENTS.md` says to take watch requests only
from trusted senders, and the turn marks which items are trusted. The tool itself cannot know who
asked.

## 7. kleis tokens in a session that does not end

A kleis token lives at most the spec's `max_ttl` (a day; orchestra's default ttl is 12h). Two things
in a running resident hold one:

* **the agent's own process** — its model API traffic goes through the proxy too
  (`HTTPS_PROXY` covers the whole process, and kleis checks the token at every CONNECT), from the
  process environment, which cannot be changed from outside;
* **the commands it runs** — each Bash command inherits that environment.

So:

1. **Every wake is a rotation.** The daemon derives a new token, fetches setup, and starts the
   process with them; the card in the system prompt is current.
2. **Between turns, cycle before expiry.** Before each delivery the daemon checks the token: with
   less than `token_refresh_margin` (default 2h) left, it cycles first — stop the process, wake at
   once on the warm pod (§9). The old token is revoked after the new process is up.
3. **`refresh_kleis_token`, an MCP tool, for a long turn.** It derives a new token, writes the proxy
   settings into the session's **env file**, returns the new expiry and card, and marks the session
   to be cycled after this turn. The env file is per session, in a directory granted at launch (on
   landrun under the session's own data directory, not the shared `$HOME`; on Kubernetes written into
   the pod with `Session.putHomeFile`), and the agent's command shell is pointed at it with Claude
   Code's `CLAUDE_ENV_FILE` — **indirectly** (§15.1): `CLAUDE_ENV_FILE` names a *stub*, written
   before each process starts and never changed while it runs, that sources the env file
   (`if [ -r <env> ]; then . <env>; fi`). The CLI keeps the stub's content and replays it before
   each Bash command, and the `.` reads the env file afresh every time. The next command uses the
   new token; the agent's own process keeps its token until the cycle, which is why the margin is
   hours. The token the tool replaced is revoked at that cycle, and the env file is emptied first —
   never left naming a revoked token.

   *As built:* the env file sets only the four proxy variables (the rest of a launch's environment
   does not change with the token, and re-exporting it would undo the rule that the agent's own
   variables win over a setup's). Every process — not only every wake — gets a token of its own,
   and a warm sleep revokes all the session holds. The tool is offered only to residents behind
   kleis; queued tasks are not offered it, since their token outlives any task's deadline at the
   default ttl and nothing would cycle them.

With a 12h ttl and a 2h margin, only a single turn longer than 2h needs the tool; it exists for
that, and for a command refused with "token expired".

## 8. Budget

* **A day's budget** (`budget_per_day`, default **$100**), rolling over at midnight in
  `residents.timezone`. Spend is recorded per turn from the `result` cost in
  `resident_spend(resident, day, usd)`.
* **Enforced by the process cap.** Each process is started with `--max-budget-usd` = what is left of
  today's budget. Claude Code stops itself at the cap, so the day's budget is a hard limit, not a
  check between turns. At the budget day's rollover a running resident is cycled to pick up the new
  allowance.
* **Exhausted**: the resident is **paused: budget** (§3.2); its inbox keeps filling; the dashboard
  shows it; at the next day it is woken with everything that arrived.
* There is no per-turn budget: Claude Code has none, and the day's cap bounds a runaway turn.

## 9. Asleep: warm, then cold

On Kubernetes, waking from no pod means scheduling one, running its init containers and starting
the agent — tens of seconds, not seconds. So sleep has two stages:

* after `process_idle_seconds` (default 1800): **warm** — the agent process stops; the pod, the task
  volume mount and the MCP relay stay. A wake from warm is a process start (`--resume`): seconds.
  *As built:* a warm sleep also revokes the session's kleis tokens (the next process gets its
  own, §7) and copies the memory directory back from the pod (`Session.syncBack`), so what the
  resident learned reaches the daemon's disk at each sleep rather than only when the pod goes.
* after `pod_idle_seconds` (default 21600): **cold** — the pod is deleted too. A wake from cold is a
  pod start; the first reply takes that long.

Cycles (§7, §8) happen on a warm pod. On landrun there is no pod, and warm and cold are the same.

The resident's task volume is keyed by the resident's name and **exempt from `retention_days`**. If
it is lost anyway, the next wake starts a fresh conversation in the same session, says so in the
first turn ("your previous conversation was lost; your memory directory is intact"), and records it
in the transcript. Its size is `residents.volume_size`. Conversation growth is handled by the
agent's own compaction; the memory directory is the durable part.

## 10. Restarts, failures and limits

* **Daemon stop**: residents go to sleep (as sessions do today), cold on Kubernetes (pods are swept
  at restart); feeds stop; their tokens are revoked. **Start**: tokens recorded by the previous run
  are revoked; feeds reconnect and catch up (§5.2); residents with ready items are woken.
* **Feed failure** (Zulip down, kleis refusing): backoff up to 5 minutes; the error on the
  dashboard; the inbox untouched.
* **Wake failure**: backoff; after three in a row the resident is **paused: failed** until an
  operator resumes it. The count, the backoff and the turns counted against `max_turns_per_hour`
  are kept in `resident_runtime` (a row per resident, not columns on the session record, which is
  written whole from the manager's copy), so a restart does not reset them.
* **Limits**: `max_awake` residents with a process at once (§2); `max_turns_per_hour` (default 60)
  against loops with another bot; every turn's cost against the day.

## 11. Control and visibility

* `orchestra resident list|show|pause|resume|reset|inbox <name>`.
* Dashboard: each resident's state (running / idle / warm / cold / paused and why), inbox depth, last
  event, today's spend against budget, its feeds' connection state, its watches; and the chat view,
  where a person talks to it directly (through the inbox).
* Every turn names the inbox items it carried, so the transcript shows what woke it.

## 12. Safety

A resident reads text from people it does not know and can send messages: the pair prompt injection
needs. The design bounds it in layers:

* **Who can wake it**: `allow_senders` for DMs and mentions; `watch_senders` for watched topics.
* **What it is told about trust**: every item is marked trusted or not in the turn, and `AGENTS.md`
  says to take instructions only from trusted items.
* **What it can do**: its kleis spec, not what a message asks. The secretary's spec allows read,
  send and react on one Zulip server; a deployment that wants less can bound sending with the Zulip
  extension's `stream` field (`allow send_message where stream in [...]`).
* **What the feed can do**: its token is narrowed to reading Zulip and to Zulip alone (§5.2).
* **Loops**: the bot's own messages are never items; turns per hour are capped.
* **Spend**: the day's budget is a hard cap (§8).

## 13. Changes outside orchestra

* **kleis**: a narrowing line `services: <name>, …` that refuses every service not named
  (`reject if request_service($s), ![…].contains($s)`), so an issuer can confine a token to some of
  the services its spec reaches. Small: `Kleis.Narrow` and its tests.
* **Nothing else**: the Zulip extension's `read` group (which includes the event queue) and
  `send_message`/`react` are deployed (kleis `e4de27c`, on castor).

## 14. Phasing

1. **Resident sessions**: the `resident` block, sessions without a repository, one per identity by
   name, never ended (§3.2), the inbox with confirmed delivery, human turns through it, `deliver_to`,
   the day's budget as a process cap, warm and cold sleep, the storage exemption,
   `orchestra resident …`. Usable with no feed.
2. **The Zulip feed** as a long-poll listener through kleis with the resident's key; the kleis
   `services:` narrowing; watches and their tools.
3. **Token cycling and `refresh_kleis_token`**; dashboard controls.

## 15. Open questions, and what to verify first

1. **Verified**, with a finding (Claude Code 2.1.280, headless `--print --input-format stream-json
   --output-format stream-json`, as orchestra launches it; two turns, each running
   `echo $KLEIS_PROBE` through the Bash tool, the file rewritten from `one` to `two` between them;
   driven through a scripted stand-in for the Messages API, `ANTHROPIC_BASE_URL`):

   | mechanism | turn 1 | turn 2 |
   | --- | --- | --- |
   | `CLAUDE_ENV_FILE` → a file rewritten mid-session | one | **one** |
   | `CLAUDE_ENV_FILE` → a stub that sources a file rewritten mid-session | one | **two** |
   | `env` in a `--settings <file>` file rewritten mid-session | one | **one** |
   | `env` in `$HOME/.claude/settings.json` rewritten mid-session | one | **two** |

   So the CLI reads `CLAUDE_ENV_FILE`'s *content* once and replays it before each command; a file
   it sources is read again each time. The user settings file is watched and its `env` applied,
   but it is `$HOME`'s — shared by every session on landrun — so the stub is what §7.3 uses. The
   same held with `SHELL` (and `CLAUDE_CODE_SHELL`) pointing at zsh 5.9: the CLI ran its commands
   under bash regardless, which is the case `BASH_ENV` was dropped for; the stub is plain POSIX
   `.`, so it does not depend on which. An end-to-end run (a resident on the local backend, the
   real CLI, a real kleis) confirmed it: after `refresh_kleis_token`, the next command in the same
   process carried the new token.
2. **Verify** a 90s Zulip `GET /events` through kleisd end to end, from the daemon's network.
3. **Several Zulip servers**: one feed per site (proposed), or one per Zulip account the spec reaches?
4. **Owner notifications**: the dashboard reports *paused: budget* and *paused: failed*; should they
   also reach a person — which would need a send-scoped token of its own?

---

## What the review changed

An adversarial review against the orchestra and kleis sources found three blockers and ten major
issues in the first draft. Changes:

* **Token refresh** (§7): `BASH_ENV` dropped — it is ignored under zsh, landrun shares one `$HOME`,
  and Landlock grants are fixed at launch. A per-session env file in a granted directory, pointed at
  through `CLAUDE_ENV_FILE` (to verify); the tool also schedules a cycle, and the replaced token is
  revoked at that cycle, with the file removed — not at the end of the turn, which would have left
  the file naming a revoked token.
* **Never ended** (§3.2): the crash, failed-write and budget paths that end or fail a session today
  are listed and changed for residents.
* **Senders** (§5.2): `allow_senders` gates only DMs and mentions; watched topics have
  `watch_senders`; every item is marked trusted. The first draft's example contradicted its own
  filter.
* **Budget** (§8): per-turn budgets dropped (Claude Code cannot enforce them); the day's budget is
  the process's `--max-budget-usd`, cycled at rollover — a hard cap instead of a check between turns.
  Owner notification "through the feed" dropped: the feed cannot send.
* **Reconnect** (§5.2): register first, then catch up per term (Zulip narrows AND their terms), so
  nothing is lost between the two. Mentions in unsubscribed streams are matched by content.
* **Feed token** (§5.2, §13): a narrowing confines only the service it names, so the feed token also
  gets `services: zulip`, a small kleis addition.
* **Delivery** (§4.2): sent, then confirmed on the turn's result; an interrupted turn's items are
  redelivered with a note.
* **Latency** (§9): two-stage sleep, warm (process stopped, pod kept) then cold, so a wake and a
  cycle are seconds; only a cold wake pays for a pod.
* **Feeds on listeners** (§5.1): a long-poll listener kind with `deliver_to`, rather than a new
  abstraction — listeners already have state, rate limits and a dashboard.
* **Smaller points**: the bot's own id from `users/me`; `max_awake` separate from `max_sessions`;
  tokens of a crashed run revoked at start; the repository-less form of the kleis task facts; the
  daemon reaching kleis at `url`, not the pods' `proxy`; `--max-time 120` for the long poll.

One finding was not taken: it said the Zulip extension and narrowings are not on kleis master. They
are (kleis#3, `e4de27c`, deployed); the reviewer read a stale local checkout.
