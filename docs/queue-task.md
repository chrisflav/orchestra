# queueing tasks from inside a task

An agent can put work on orchestra's queue itself, through the `queue_task` MCP tool, and bind a
taxis issue to it — optionally claiming that issue so no dispatcher hands it to anyone else in
the meantime.

This is how one agent hands off work rather than doing it: a planner that decomposes an epic and
queues an implementor per leaf, a reviewer that queues a second opinion it cannot give itself, a
maintainer that queues the boring half of a job on a cheaper model.

## the policy is the switch

Every other tool an agent holds does one bounded thing. `create_pr` opens a pull request against
the repository the task already has; `claim_issue` locks an issue that already exists. `queue_task`
hands the agent a whole task to configure — which backend runs it, on which model, holding which
tools, against which repository — and each of those is a way to reach further than the task
itself was ever granted.

So the tool has no fixed capability. Everything it may do is written down in a `spawn_policy`,
and **a task with no policy is not offered the tool at all**. There is deliberately no permission
label for it: naming `queue_task` in a `tools` list grants nothing, because a label would be a
second way to switch on a tool whose whole safety is that somebody wrote down its bounds.

## where a policy is written

`spawn_policy` sits beside the rest of the task's shape, at whichever level the task is
described:

| level | file | applies to |
|---|---|---|
| task file | the task object in `tasks.json` | that task |
| queue entry | `spawn_policy` on the entry | that entry's run |
| listener | `action.spawn_policy` | every task the listener queues |
| role | `spawn_policy` on the role document | every task dispatched for that role, whether the dispatcher spawned it or `orchestra spawn` did |

A listener that names a `workflow_path` starts a concert instead of queueing a task, and never
reads its `action` block's policy. The workflow YAML has no `spawn_policy` key of its own, so a
concert step cannot carry one either.

The two dispatchers (`project-dispatcher`, `label-dispatcher`) build their entries from the
**role**, not from the listener's `action` block, so a dispatched role takes its policy from its
own file. That is also the right granularity for it: "a planner may queue implementors" is a
statement about planners, not about the listener that happens to dispatch them.

## what a policy says

```json
{
  "spawn_policy": {
    "backends": ["claude", "codex"],
    "models": ["opus", "haiku"],
    "tools": ["create_pr", "work_issues"],
    "repos": ["acme/widgets", "acme/docs"],
    "max_tasks": 5,
    "allow_pre_claim": true,
    "max_budget": 6.0,
    "priority": 20,
    "read_only": false
  }
}
```

Three of the four choices follow one rule:

> **Omitted means "the same as mine".** A value the agent names has to appear in the policy's
> list for that field; an empty list means nothing may be named, so the field is inherited and
> the agent has no say at all.

That holds for `backend`, `model` and `repo` alike, which is why an empty policy —
`"spawn_policy": {}` — is still a useful one: it lets a task queue more of *itself*, with the
same backend, model and repository, and the tools it already holds, and nothing else.

- **`backends`** / **`models`** — what may be named instead of inheriting. Models are not checked
  against the backend: orchestra holds no per-backend model list, and one written into a config
  would go stale on the vendor's schedule rather than the operator's. A model the backend does
  not know fails when the task runs, the same way it does when a role names one.
- **`repos`** — upstream repositories, as `owner/name`. The fork is never named here; it is
  resolved when the task is queued by the same rule every other dispatch path uses (the target
  itself when the GitHub App can push to it, a fork in `default_organization` otherwise), and it
  is resolved even when the repository was inherited rather than named, so one rule decides where
  a task pushes no matter how it got there. A repository the App can neither push to nor fork is
  refused, and nothing is queued.
- **`tools`** — by the same names a role's `permissions` use, and checked against the same
  vocabulary (`create_pr`, `comment`, `label_issue`, `manage_issues`, `work_issues`,
  `review_issues`) wherever a policy is validated (see below), so a policy cannot *list*
  something a role could not be given. What the spawning task already holds is grantable without
  being listed — naming what you already hold reaches nothing new, though it does mean a task
  itself granted `merge_pr` or `create_repository`, neither of which a role may have, can pass
  them on. The list **may** name tools the spawning task does not hold,
  and that is the point rather than an oversight: a read-only planner queueing an implementor
  that can open a pull request is the case the tool exists for, and the operator writing the
  policy is the one deciding it.
- **`max_tasks`** — how many tasks one task may queue over its whole run, counted over the queue
  rather than in memory, so a daemon restart mid-run cannot hand an agent its allowance a second
  time. Terminal entries count too: the ceiling is on how much work a task may create, not on how
  much of it is still running. Defaults to **1**.
- **`allow_pre_claim`** — whether the tool may claim the issue it binds. Off by default: a claim
  takes an issue out of every dispatcher's candidate set until the task holding it ends, which is
  exactly what you want when the queued task is the one that should do the work, and exactly what
  you do not want by accident.
- **`max_budget`** — a ceiling in USD, and the budget a queued task gets when the agent names
  none. Both halves matter: without the default, a policy capping spend at 2.0 would still hand
  an unbudgeted task orchestra's own default of 4.0. A budget over the ceiling is refused with
  the ceiling in it rather than silently lowered — a task queued at a quarter of the budget its
  prompt assumes stops halfway and looks like a failure. With no `max_budget` set the agent may
  not name a budget at all: this field is closed like every other one, so that the ceiling on how
  many tasks may be queued does not sit beside an unbounded amount each of them may spend.
- **`priority`** — the agent has no say: a priority is only meaningful against every other entry
  in the queue, and is a way to starve all of them.
- **`read_only`** — the agent has no say here either, but unlike `priority` this one *inherits*
  when the policy omits it. Otherwise an empty policy would hand a read-only reviewer a
  read-write child, which is the single way "queue a copy of itself" could grant more than the
  queueing task had. Set it explicitly for the case the tool exists for: a read-only planner
  queueing an implementor that has to write.

## what the agent calls

```json
{
  "prompt": "Implement the parser described in issue 412 and open a pull request.",
  "backend": "claude",
  "model": "opus",
  "repo": "acme/widgets",
  "tools": ["create_pr", "work_issues"],
  "budget": 4.0,
  "issue_id": 412,
  "pre_claim": true
}
```

Only `prompt` is required; everything else is inherited when omitted. `tools` distinguishes
absent (inherit the spawning task's set) from `[]` (a task with no optional tools at all), which
is a thing worth being able to ask for.

**`max_budget` is a ceiling *and* a default; `tools` is a ceiling only.** An agent that omits
`tools` gets what the queueing task holds, not the policy's whole list — so a planner holding
only `manage_issues`, under a policy permitting `create_pr`, queues an implementor that cannot
open a pull request unless it asks for one. Say so in the prompt template if the queued task
needs the tools the policy went to the trouble of permitting.

A named `issue_id` must be **at or below the spawning task's own issue or project** — the same
subtree every other write is confined to. Binding an issue is what decides which work the queued
agent is pointed at, and pre-claiming it writes to taxis; either outside that subtree would be a
way to reach past every other write's bound. The queued task is held to the *queueing* task's
scope rather than re-deriving its own from the issue it was bound to, so a task cannot create an
agent that writes where it cannot.

It must also be **unclaimed**, whoever holds it — including the task making the call. A task's
id is written over the claim on the issue it is bound to when it starts, and the claim is
released when it ends; binding a claimed issue would therefore take the claim off its holder and
hand the issue back to the open pool underneath an agent still working it. Release it first if
it is yours to release.

Refusals carry the list the agent may pick from, which is the point at which it needs it. The
tool's own description deliberately does not list the allowed sets: they are per task, the
description is not, and one that named them would be wrong for every task but the one it was
written against.

## where a policy is checked

A policy's `tools` are checked against the vocabulary a role's `permissions` are checked against,
and `max_tasks: 0` is refused, when a role or listener is written **through the API** — which is
what `orchestra role`/`listener` edits and the dashboard use. A role, listener, task file or queue
entry written straight to disk skips that check: an unknown tool name is then silently ignored
when the queued task runs, and `max_tasks: 0` becomes a tool that is offered and refuses every
call. A file that fails to parse at all is reported on stderr and the role or listener is skipped.

`allow_pre_claim` is checked against nothing, because there is nothing to check it against:
taking a claim is otherwise reached through `claim_issue`, which needs `work_issues`, and a
policy may set `allow_pre_claim` on a task holding neither. That is deliberate — the queued task
is the one that will do the work — but it is a capability this field grants on its own.

## the fan-out is one level deep

A task queued through `queue_task` never carries a policy of its own, so it cannot queue tasks in
turn. That is what bounds the fan-out to `max_tasks` rather than `max_tasks` to the power of
however many generations the agent felt like — and it is a rule rather than a depth counter
because a counter has to be got right in every path that builds a queue entry, while "the field
is never set" only has to be got right where entries are built from a spawn.

## following it afterwards

An entry queued this way records the task that queued it as `spawned_by`, which is both the
counter behind `max_tasks` and the provenance shown in the dashboard's queue rows: without it, a
task that appeared out of a running agent's turn is indistinguishable from one a person added.

## a worked example: a planner that queues implementors

`~/.config/orchestra/roles/planner.json`:

```json
{
  "name": "planner",
  "permissions": ["manage_issues"],
  "read_only": true,
  "prompt_template": "Project {{project_name}}. Decompose the open work into leaves, then queue one implementor per leaf with queue_task, binding and claiming its issue and passing tools: [\"create_pr\", \"work_issues\"].",
  "dispatch": { "trigger": "idle", "max": 1 },
  "spawn_policy": {
    "tools": ["create_pr", "work_issues"],
    "max_tasks": 5,
    "allow_pre_claim": true,
    "max_budget": 8.0,
    "read_only": false
  }
}
```

The planner is read-only and holds only `manage_issues`. The tasks it queues *may* be granted
`create_pr` and `work_issues` — which is why the template tells the agent to ask for them; omit
them and the implementor inherits `manage_issues` and cannot open a pull request. They run on the
planner's own backend, model and repository, are budgeted at 8.0 apiece (named or not, since the
ceiling is also the default), and there are at most five of them per planner run. `read_only` is
not set, so it inherits — the planner is read-only and its implementors would be too, which is
why a policy like this one usually sets `"read_only": false` once it has real work to do. Each is
bound to its issue and claimed on the spot, so the dispatcher does not offer the same issue to
anyone else on its next tick.
