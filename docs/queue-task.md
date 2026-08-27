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
| role | `spawn_policy` on the role document | every task dispatched for that role |
| workflow step | `spawn_policy` on the step's task spec | that step |

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
- **`tools`** — by the same names a role's `permissions` use, and validated against the same
  vocabulary (`create_pr`, `comment`, `label_issue`, `manage_issues`, `work_issues`,
  `review_issues`), so `queue_task` cannot hand a task something a role could not be given.
  What the spawning task already holds is grantable without being listed — naming what you
  already hold reaches nothing new. The list **may** name tools the spawning task does not hold,
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
  prompt assumes stops halfway and looks like a failure.
- **`priority`** / **`read_only`** — the agent has no say in either. A queue-wide priority is a
  way to starve every other task, and read-only is the sandbox's answer rather than the prompt's.

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

A named `issue_id` must be **at or below the spawning task's own issue or project** — the same
subtree every other write is confined to. Binding an issue is what decides which work the queued
agent is pointed at, and pre-claiming it writes to taxis; either outside that subtree would be a
way to reach past every other write's bound.

Refusals carry the list the agent may pick from, which is the point at which it needs it. The
tool's own description deliberately does not list the allowed sets: they are per task, the
description is not, and one that named them would be wrong for every task but the one it was
written against.

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
  "prompt_template": "Project {{project_name}}. Decompose the open work into leaves, then queue one implementor per leaf with queue_task, binding and claiming its issue.",
  "dispatch": { "trigger": "idle", "max": 1 },
  "spawn_policy": {
    "tools": ["create_pr", "work_issues"],
    "max_tasks": 5,
    "allow_pre_claim": true,
    "max_budget": 8.0
  }
}
```

The planner is read-only and holds only `manage_issues`; the tasks it queues get `create_pr` and
`work_issues`, run on the planner's own backend, model and repository, are budgeted at 8.0
apiece, and there are at most five of them per planner run. Each is bound to its issue and
claimed on the spot, so the dispatcher does not offer the same issue to anyone else on its next
tick.
