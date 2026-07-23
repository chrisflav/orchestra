# orchestra

orchestra is a daemon that runs agentic coding tasks. Listeners watch issues, comments, pull
request reviews and an issue tracker, turn what they find into tasks, and the daemon runs each one
in a sandbox with credentials the agent never gets to hold.

## the moving parts

Three pieces carry everything else.

- A **task** is the unit of work and the only thing that ever runs an agent: a repository pair, a
  prompt, a set of tools, a budget → [task files](#task-files)
- The **queue** is where tasks wait for the daemon that runs them, by priority and in parallel →
  [queue mode](#queue-mode)
- **Listeners** poll an event source and enqueue a task whenever something matches →
  [listeners](#listeners)

```
 task file      workflow       listener event      role dispatcher
      \             \                /                  /
       ─────────────► queue (priority, parallel slots) ◄──────
                             │
                             ▼
                        task runner
                             │
          clone + GitHub App token + per-repo hooks
                             │
                             ▼
              landrun sandbox ──── agent backend
                             │           │
                             └► MCP server ◄┘
                              (GitHub + issues)
```

## what it does

- **Sandboxed runs.** Every agent runs under landrun: write access to its own clone and `/tmp`,
  read+execute on the toolchain, outbound TCP to HTTPS and the MCP port, nothing else. A task can
  mount its clone read-only, which is what review tasks use.
- **Credentials the agent never sees.** A GitHub App installation token is minted per task and
  handed to `gh` for git transport only. The personal access token used for upstream pull
  requests, reviews and comments stays in the MCP server process.
- **Four agent backends** — `claude` (Claude Code), `vibe` (mistral-vibe), `opencode` and `pi` —
  plus two built-in agent-less backends: `merger`, which lands an approved pull request, and
  `triage`, which applies and removes labels deterministically.
- **GitHub actions as MCP tools.** Creating pull requests, comments, reviews with inline
  annotations, and reading review threads are tools, not `gh` invocations. Each task is granted a
  subset of them.
- **A queue daemon** with priorities, parallel execution backed by per-repository clone slots,
  re-enqueueing of unfinished entries, and a graceful drain on `SIGTERM`.
- **Usage-limit awareness.** Every Claude subscription limit is tracked — session, weekly, and
  the weekly limits scoped to one model family — so work is routed to an account that can still
  run it instead of into a wall. Several accounts can back one listener, used in order or
  balanced across → [usage limits](#usage-limits)
- **Listeners** that poll GitHub issues, comments, pull request reviews, labels, or an arbitrary
  shell command, and enqueue a task — or a whole workflow — when something matches.
- **An autonomous project pipeline.** Projects and issues live in a
  [taxis](https://github.com/chrisflav/taxis) tracker; *roles* are task templates a dispatcher
  spawns as work appears, with claim locks so two agents never pick up the same issue.
- **Concert workflows**: YAML multi-step programs with typed step outputs, loops and conditionals.
- **Per-repository hooks** for setup, validation and teardown, with an automatic retry loop when
  validation fails.
- **Recorded history.** Every run is stored, can be grouped into a named series, and resumed with
  a follow-up prompt.

## prerequisites

It is recommended to run all of orchestra inside a virtual machine or container. Two ready-made
environments provide the full set of tools: a NixOS incus image (see the
[container section](#container)) and a Docker image running the queue daemon (see the
[docker section](#docker)).

Before starting you will need to create a GitHub App with a private key, installed on the organization owning the fork. Download the private key.

You will also want a personal access token with repo scope. The two are not alternatives: every
task mints an installation token from the App and gives it to `gh` for cloning and pushing, so
without the App no task runs at all, while the PAT covers what an installation token cannot —
pull requests against the *upstream* repository, issue comments, PR reviews, and the triage
backend.

Inside the container (or VM), the following must be available (all installed
automatically when using the provided container image):

- [Lean 4 / Lake](https://leanprover.github.io/lean4/doc/setup.html) to build
  the tool
- [landrun](https://github.com/Zouuup/landrun) for sandboxing
- at least one agent CLI, installed and authenticated: `claude` (Claude Code), `vibe`
  (mistral-vibe), `opencode`, or `pi`
- `gh` (GitHub CLI) for repository operations
- [pi-mcp-adapter](https://github.com/nicobailon/pi-mcp-adapter) — **Optional**
  MCP OAuth support for the **Pi agent** specifically (not needed for the other
  backends). Tested with the MCP tools defined in this project. Install in
  the container with `pi install npm:pi-mcp-adapter`.

In addition, the private key from the GitHub App must be included as a file in the container.

## building

```
lake build orchestra
```

The resulting binary is at `.lake/build/bin/orchestra`.

## configuration

Create `~/.config/orchestra/config.json`:

```json
{
  "github_app": {
    "app_id": 12345,
    "private_key_path": "/path/to/private-key.pem",
    "installation_id": 67890
  },
  "github": {
    "pat": "github_pat_..."
  },
  "plugin_dirs": [],
  "claude_token": "...",
  "authorized_users": ["<GitHub_username>"],
  "default_organization": "my-org",
  "queue": {
    "parallel": 4,
    "parallel_per_repo": 2
  }
}
```

`default_organization` is the organisation under which orchestra may create repositories. It is
used by every project/role-based task that pushes (the automatic dispatcher and `orchestra spawn`):
a task targets a repository its pull requests must land in, and the agent works on a *fork* it can
push to. When the GitHub App already has write access to the target, the fork is the target itself
and nothing is created; when it does not, the target is forked into `default_organization` and that
fork is what the agent pushes to. If the App cannot push to a target and `default_organization` is
unset, the task is skipped rather than dispatched at a repository it cannot push to — so set this
whenever the dispatcher works on repositories the App is not directly installed on. The App must be
installed on `default_organization` with permission to create repositories.

**The target must be readable by the App.** A task's token is minted for the *fork's* installation,
and that token is what fetches the upstream. Public targets are fine. A private target the App is
not installed on cannot be fetched — and cannot be forked in the first place — so such a task is
skipped, with the reason on stderr under `[fork]`.

Two roles never fork. The **merger** merges with `gh pr merge` as the GitHub App, which requires
write access to the pull request's own repository, and a fork cannot supply that; when the App
cannot push to a pull request's repository — or the check stays inconclusive — no merger is queued
and `decide_issue approve` reports that the pull request has to be merged by hand. The
**auto-reviewer** is read-only and pushes nothing, so it works in the pull request's own repository
and needs no writable fork at all.

`installation_id` is optional; if omitted it is looked up automatically.
`pat` is a personal access token used to create pull requests to the upstream
repository. The agent itself never sees this token.

`claude_token` is an optional long-lived Claude OAuth token. Claude login
sessions lapse frequently; a stable token avoids repeated re-authentication.
Obtain one by running `claude setup-token` and copy the token value here. When
set, it is passed to the agent as the `CLAUDE_CODE_OAUTH_TOKEN` environment
variable.

`additional_sandbox_paths` grants every agent launched by this instance extra filesystem access on
top of what its backend already needs — `rox`/`ro`/`rw` for absolute paths, `home_rox`/`home_rw`/
`home_rwx` for `$HOME`-relative ones. It is the usual fix when a repository's build needs a shared
cache directory:

```json
{
  "additional_sandbox_paths": {
    "home_rw": [".cache/mybuildtool"]
  }
}
```

TCP ports beyond HTTPS and the MCP server are opened per backend, via `extra_ports` on that
backend's entry in the `agents` array — for a local model server, for instance.

The `queue` block sets how many tasks the daemon runs at once. Both keys default
to `1`, which is the serial behaviour, and `orchestra queue start --parallel N` /
`--parallel-per-repo N` override them for a single run.

`parallel_per_repo` is capped separately because concurrent tasks on the same
repository each need their own clone — a *slot* — so that two agents can create
the same branch name without git refusing. Raising it costs one working tree per
slot; the git history itself is hardlinked from a shared cache clone, so it is
the checkout and its build output that take the space, not the objects. Run

```sh
orchestra prepare <upstream> <fork> --slots 2
```

with `--slots` matching `parallel_per_repo`, so each slot's init hook is paid up
front rather than charged to whichever task lands there first.

Two backends always run exclusively regardless of these settings: `pi` and
`opencode` keep per-run state at a fixed path under `$HOME`, so a second
concurrent run would read the first one's MCP configuration. They start only
while the daemon is otherwise idle.

### files and directories

orchestra separates configuration from state:

| Path | Contents |
| --- | --- |
| `$XDG_CONFIG_HOME/orchestra/` (or `~/.config/orchestra/`) | `config.json`, `secrets.json`, `prompts/`, `listeners/`, `roles/`, `skills/` |
| `$XDG_DATA_HOME/orchestra/` (or `~/.local/share/orchestra/`) | clones, task records, queue entries, logs, per-project role overrides |

Installations predating this split keep everything in `~/.agent/`; that layout is still read, with
a deprecation warning. `orchestra migrate` moves it into place.

### secrets

Values that should not sit in `config.json` — or that are shared between it and the listener
configs — can live in `secrets.json` next to it:

```json
{
  "github_pat": "github_pat_...",
  "taxis_token": "..."
}
```

Every `{{key}}` occurrence in `config.json` and in listener configs is replaced with the
corresponding value before the file is parsed, so `"pat": "{{github_pat}}"` works in either.
The file is optional; when absent, nothing is substituted.

## authentication sources

The `agents` array in `config.json` lets you configure multiple named
authentication sources per backend. Each source carries either an OAuth token
or an API key:

```json
{
  "github_app": { "..." : "..." },
  "agents": [
    {
      "name": "claude",
      "auth_sources": [
        { "label": "work", "oauth_token": "sk-ant-oat-..." },
        { "label": "personal", "api_key": "sk-ant-api-..." }
      ],
      "default_auth_source": "work"
    },
    {
      "name": "vibe",
      "auth_sources": [
        { "label": "main", "api_key": "mistral-..." }
      ]
    }
  ]
}
```

Each authentication source object has the following fields:

- `label` — unique name within the backend, used to reference the source
- `oauth_token` — an OAuth token (sets `CLAUDE_CODE_OAUTH_TOKEN` for the
  claude backend)
- `api_key` — an API key (sets `ANTHROPIC_API_KEY` for claude,
  `MISTRAL_API_KEY` for vibe)
- `base_url` — optional base URL used with `api_key` (sets
  `ANTHROPIC_BASE_URL` for claude)

Exactly one of `oauth_token` or `api_key` must be present per source.

The `default_auth_source` field selects which source is used when a task does
not specify one. When omitted and only one source is configured, that source
is selected automatically.

To select a specific source in a task, set the `auth_source` field:

```json
{
  "upstream": "owner/repo",
  "fork": "org/repo",
  "mode": "pr",
  "prompt": "Fix the bug.",
  "auth_source": "personal"
}
```

The same `auth_source` field is available on queue entries and listener
actions.

The legacy flat fields (`claude_token`, `anthropic_api_key`,
`anthropic_base_url`, `anthropic_auth_token`) still work when no `agents`
array is present, so existing configurations remain valid.

### using several sources, and failing over between them

A task, queue entry or listener action can name *several* candidate sources
instead of one, and say how to choose between them:

```json
{
  "upstream": "owner/repo",
  "fork": "org/repo",
  "mode": "pr",
  "prompt": "Fix the bug.",
  "auth_sources": ["work", "personal"],
  "auth_mode": "ordered"
}
```

- `ordered` (default) — use the sources in the order listed, falling through
  to the next when one is out of quota. Burn the subscription first, then the
  API key.
- `distribute` — spread work across every source that is not limited,
  preferring the least-consumed one. Two accounts with the same plan end up
  roughly level rather than one being exhausted before the other is touched.
  Between sources at the same utilisation it round-robins, so a burst of
  parallel claims fans out instead of landing on one account.

Which source ran a task is recorded on its queue entry. Under `distribute` the
choice is made and stamped while the daemon holds its claim lock, so parallel
workers cannot all read the same pre-dispatch state and pick the same account.

`auth_sources` takes precedence over `auth_source`; either can be omitted, in
which case `default_auth_source` applies as before.

**Which source a task runs on is decided when it starts, not when it is
queued.** An entry can sit in the queue for hours, and the account that was
free when a listener created it may be exhausted by the time a worker picks it
up. The daemon resolves the list at claim time and records the winner on the
entry.

### usage limits

Orchestra tracks how much of each subscription is left, so it can route around
a limit instead of running into it. For OAuth sources it polls the same
endpoint Claude Code itself uses, which reports every limit at once: the
rolling session window, the weekly total, and weekly limits scoped to a single
model family.

```
$ orchestra usage --select --model claude-opus-4-8
claude:
  work [oauth]: BLOCKED (weekly_scoped limit for Opus at 100%, resets in 16h 22m)
    session: 13% [normal], resets in 1h 51m
    weekly_all: 77% [warning], resets in 16h 22m
    weekly_scoped (Opus): 100% [critical], resets in 16h 22m
  personal [api-key]: available
    no subscription limits to report (API-key sources are billed per token)
  → would select: personal (ordered)
```

Two details matter in practice:

- **Model-scoped limits only close one model family.** An exhausted weekly-Opus
  window leaves Sonnet work on the same account perfectly runnable, so
  availability is a question about *(source, model)* — not about the account.
  Pass `--model` to ask about a specific one.
- **A limited source does not cancel anything.** Queue entries that would land
  on it stay pending and run when the window resets; entries on any other
  source, backend or model family keep going. Earlier versions cancelled every
  pending entry sharing a backend, which threw away work that was about to
  become runnable.

Limits are learned two ways, and the two cover each other: the poll sees a
limit coming and knows the exact reset time, while a run that comes back
rate-limited is recorded immediately, before anything else can be dispatched to
that source. Both write to `<data>/usage/<backend>/<label>.json`, which is
shared across processes — a limit `orchestra run` discovers in a terminal stops
the daemon from dispatching to that source too.

API-key sources have no subscription window to poll (they bill per token
against an organisation), so they are always considered available until a run
proves otherwise.

Polling is an account-metadata call, not an inference call: **it costs no input
or output tokens and consumes none of the quota it reports.** It is metered as
*requests*, though, and the budget is small: about five of them before the
endpoint answers `429` with a `retry-after` of five minutes, so roughly one
request per minute per token — shared by everything that polls.

There is therefore one poller, and everything else reads what it wrote. The
daemon refreshes each source every five minutes; dispatch goes to the network
only if the stored numbers are older than that, which while the daemon is up
they never are. A poll that fails backs off before anything retries it — for as
long as the server's `retry-after` asks after a `429`, a minute otherwise —
because a failed poll leaves the source stale, and a stale source with no
backoff is re-polled by every claim decision until the budget is gone.
`orchestra usage` reuses fresh data too; pass `--refresh` to force a poll or
`--cached` to make none. While a backoff is in effect the command says so, and
dispatch falls back to the last known limits — an unreachable endpoint is never
treated as an exhausted account.

`orchestra usage` flags: `--backend` to narrow to one backend, `--model` to
judge model-scoped limits, `--cached` to skip polling, `--select` to also show
which source a task queued now would be dispatched to, and `--auth_mode` to
simulate `distribute` instead of `ordered`.

System prompts can be placed in `~/.config/orchestra/prompts/`. The file
`~/.config/orchestra/prompts/default.md` is loaded automatically; named prompts can be
referenced via the `system_prompt` field in a task file.

## task files

Tasks are described in a JSON file:

```json
{
  "tasks": [
    {
      "upstream": "owner/repo",
      "fork": "your-org/fork-repo",
      "tools": ["create_pr"],
      "prompt": "Implement feature X and open a pull request.",
      "budget": 8.0
    }
  ]
}
```

Fields:

- `upstream` — upstream repository in `owner/repo` format
- `fork` — fork repository the agent has write access to
- `prompt` — instruction sent to the agent
- `tools` — optional tools granted to this task on top of the always-available ones; see
  [MCP tools](#mcp-tools)
- `mode` — legacy shorthand for `tools`, kept for compatibility: `"fork"` grants nothing, `"pr"`
  grants `create_pr`. Ignored when `tools` is present
- `backend` — `"claude"` (default), `"vibe"`, `"opencode"`, `"pi"`, or one of the agent-less
  backends `"merger"` / `"triage"`
- `model` — optional model override passed to the agent
- `agent` — optional sub-agent name passed to the backend
- `auth_source` — label of the authentication source to use
- `auth_sources` — candidate authentication sources, tried per `auth_mode`; takes precedence over
  `auth_source` → [authentication sources](#using-several-sources-and-failing-over-between-them)
- `auth_mode` — `"ordered"` (default) or `"distribute"`
- `system_prompt` — optional name of a file in `~/.config/orchestra/prompts/` (without
  `.md`); defaults to `default.md` if present
- `budget` — maximum spend in USD (default `4.0`)
- `read_only` — mount the clone read-only; used by review tasks
- `memory` — which memory directories the agent may persist to: `"none"`, `"global"`,
  `"project"`, or `"both"` (default)
- `priority` — queue priority, higher runs first (default `10`)
- `series` — name of the series this run belongs to
- `issue_number` — GitHub issue or PR the task was launched from; enables the `comment` tool
- `pr_labels` — labels applied to every pull request `create_pr` opens during the task, created on
  the target repository if missing
- `triage_add_labels` / `triage_remove_labels` — labels the `triage` backend applies
- `project_id` / `issue_id` / `role` — set by the project subsystem; see
  [projects, issues and roles](#projects-issues-and-roles)

## sandboxing

Agents are confined with landrun, which uses the Linux Landlock LSM — no container or VM is
involved, so this works inside the Docker image as well. A task's agent gets:

- read+write+execute on its own clone (read+execute only when `read_only` is set) and read+write
  on `/tmp`
- read+execute on the toolchain paths its backend declares, and read+write on the `$HOME`
  subdirectories that backend needs for its own configuration and state
- read+execute on plugin directories, read+write on the memory directories permitted by `memory`
- outbound TCP to port 443 and to the local MCP server port, plus any `extra_ports` configured for
  the backend
- `GH_TOKEN` (the installation token) and the selected authentication source's key in the
  environment — nothing else is inherited beyond `SHELL`, `PATH`, `HOME`, `USER` and `TERM`

`orchestra run --debug` prints the exact landrun invocation before executing it, which is the
quickest way to find out why an agent cannot see a path. A path that does not exist cannot be
granted — orchestra warns about missing `$HOME`-relative paths rather than letting the agent hang.

## MCP tools

The agent has access to the following tools via the built-in MCP server. `health`, `refresh_token`,
and `get_pr_comments` are always available; `create_pr` and `comment` must be enabled explicitly by
adding them to the `tools` list in the task configuration.

- `health` — check that the MCP server is running
- `refresh_token` — refresh the GitHub App installation token
- `get_pr_comments` — fetch review threads for a pull request
- `create_pr` — create a pull request on the upstream repository
- `comment` — post a comment on the issue or pull request the task was launched from.
  Supports four modes:
  - **regular comment**: provide only `body`
  - **PR review**: provide `body` and set `review: true`; optionally include `inline_comments`
  - **reply to inline comment**: provide `body` and `reply_to_comment_id`
  - **new inline comment**: provide `body`, `path`, and `line`

  The `comment` tool requires the task to carry an `issue_number` (set automatically by the
  listener when triggered from an issue or pull request).

Three more tool groups — `manage_issues`, `work_issues`, `review_issues` — are available when a
task carries them in its `tools` list, backing orchestra's project/issue/claim workflow (a taxis
issue tracker instance, not local files — see [`examples/projects/README.md`](examples/projects/README.md)
for the full concept mapping, the `taxis` config section, and a CLI/tool cheat sheet).

## skills

`skills/` is a Claude plugin directory loaded into every agent by default. It carries two skills:

- **`orchestra-pull-requests`** — opening PRs, commenting, reading review feedback.
- **`orchestra-taxis-issues`** — claiming, splitting, attaching PRs, and managing tracker issues.

Both exist mainly to state one rule the agent cannot infer: **`gh` must never be used for pull
requests or issues.** Everything goes through the MCP tools, which select the right credential
(PAT vs GitHub App token), record what the task did, and enforce the per-task permission groups.
`gh` is authenticated for git transport only. Plain `git` is fine.

They also draw the distinction between **taxis** issues (the tracker; what agents claim and work)
and **GitHub** issues (the thread a task was launched from). The ids look alike and nothing stops
you passing one where the other belongs.

Install by copying into the config directory, from where orchestra picks it up automatically:

```
cp -r skills $XDG_CONFIG_HOME/orchestra/skills      # or ~/.config/orchestra/skills
```

The Docker image bundles them and seeds `/config/orchestra/skills` on first start, so nothing is
needed there. Local edits survive restarts; delete the directory to get the shipped copy back.

Anything in `plugin_dirs` in `config.json` is loaded as well — the skills directory is prepended,
not a replacement.

## running tasks

```
orchestra run tasks.json
```

Run only one task from the file (0-based index):

```
orchestra run --task 0 tasks.json
```

Continue a previous agent session:

```
orchestra run --task 0 --continues <task-id> tasks.json
```

Group runs into a named series for later resumption:

```
orchestra run --series my-series tasks.json
```

To sit in front of the agent yourself, with the same clone, credentials and sandbox a task would
get, use its interactive TUI:

```
orchestra interactive --upstream owner/repo --fork your-org/fork
```

## concert workflows

Workflows are YAML files that describe a multi-step agent program. Steps run
sequentially; later steps can receive typed outputs from earlier ones. The
workflow is compiled to a _Concert_ program and evaluated step by step.

Pass a `.yaml` file directly to `run`:

```
orchestra run workflow.yaml
```

Pass initial variable values with `--vars`:

```
orchestra run --vars '{"difficulty": 5}' workflow.yaml
```

See `docs/workflow.md` for the full workflow DSL reference and
`examples/concerts/` for ready-made examples:

- `examples/concerts/sequence.yaml` — plan / implement / review pipeline
- `examples/concerts/loop.yaml` — for-each loop with typed outputs
- `examples/concerts/conditionals.yaml` — conditional exit on difficulty score

## task history

```
orchestra tasks
orchestra task <id>
orchestra series
orchestra tag <id> <series>    # append a finished task to a series
```

## resuming a series

```
orchestra resume my-series --prompt "Now add tests."
```

This picks up the repository and settings from the latest run in the series and
resumes the agent session from where it left off.

## queue mode

Start a daemon that picks up tasks from a queue:

```
orchestra queue start
```

Add tasks to the queue:

```
orchestra queue add tasks.json
orchestra queue add --resume my-series --prompt "Next step."
```

Add a workflow (concert) to the queue:

```
orchestra queue add workflow.yaml
orchestra queue add --vars '{"key": "value"}' workflow.yaml
```

Inspect and control the daemon:

```
orchestra queue                # show queued entries
orchestra queue status         # daemon status and running tasks
orchestra queue cancel         # cancel the running task, keep the daemon going
orchestra queue shutdown       # stop after the current task (--force cancels it)
```

Re-enqueue unfinished or cancelled entries:

```
orchestra queue retry
orchestra queue retry --series my-series
```

Unfinished entries resume the partial agent session they died in; cancelled ones start over. On
`SIGTERM` the daemon stops accepting work and drains what is in flight, so a `docker compose down`
does not lose a running task.

## per-repository configuration

A repository can provide a `.orchestra/` directory with optional hooks and a config
file:

- `.orchestra/init.sh` — run once after cloning
- `.orchestra/before.sh` — run before each agent launch
- `.orchestra/validation.sh` — run after each agent launch; non-zero exit triggers
  a retry
- `.orchestra/after.sh` — run after the validation loop completes
- `.orchestra/config.json` — validation settings:

```json
{
  "validation": {
    "max_retries": 3,
    "retry_prompt": "Validation failed. Please fix the issues."
  }
}
```

The failing script's output is available to the retry prompt as `{{validation_output}}`.

## listeners

Listeners poll event sources and automatically enqueue tasks. Listener configs
are JSON files placed in `~/.config/orchestra/listeners/`.

Example — respond to issue comments containing a trigger word:

```json
{
  "name": "issue-comments",
  "source": {
    "type": "github-comments",
    "repos": [
      {"upstream": "upstream-org/upstream-repo", "fork": "your-org/upstream-repo"}
    ],
    "trigger": "@orchestra",
    "authorized_users": ["alice", "bob"]
  },
  "action": {
    "upstream": "{{upstream}}",
    "fork": "{{fork}}",
    "mode": "fork",
    "prompt_template": "A comment has been left on issue/PR #{{issue_number}}.\n\nAuthor: {{author}}\nURL: {{url}}\n\n{{body}}\n\nPlease read the comment and take the appropriate action.",
    "series": "issue-{{issue_number}}"
  },
  "interval_seconds": 120
}
```

Fields:

- `source.type` — one of `"github-issues"`, `"github-comments"`, `"github-pr-reviews"`,
  `"github-labels"`, `"github-label-count"`, `"shell"`, or the two auto-dispatchers
  `"project-dispatcher"` and `"label-dispatcher"` (documented in
  [`examples/projects/README.md`](examples/projects/README.md) — the first works on one project,
  the second on every issue carrying a given label, wherever it lives)
- `source.repos` — list of `{"upstream": "...", "fork": "..."}` pairs
- `source.trigger` — only events whose body contains this string are processed
- `source.authorized_users` — list of GitHub logins that may trigger the listener; empty means allow everyone
- `action.prompt_template` — template rendered with event variables (e.g. `{{upstream}}`, `{{fork}}`, `{{issue_number}}`, `{{body}}`, `{{author}}`)
- `action.workflow_path` — path to a `.yaml` workflow file; when set the listener starts a concert instead of enqueueing a single task
- `action.auth_sources` / `action.auth_mode` — candidate authentication sources for the tasks this
  listener queues, and how to pick among them. A listener that fires repeatedly is the case
  several accounts exist for: it keeps producing runnable work after one account's weekly window
  closes → [usage limits](#usage-limits)

To trigger a multi-step workflow from a listener, replace `prompt_template` with `workflow_path`:

```json
{
  "name": "issue-workflow",
  "source": {
    "type": "github-issues",
    "repos": [{"upstream": "owner/repo", "fork": "your-org/fork"}],
    "trigger": "@orchestra",
    "authorized_users": ["alice"]
  },
  "action": {
    "upstream": "{{upstream}}",
    "fork": "{{fork}}",
    "mode": "fork",
    "workflow_path": "/path/to/workflow.yaml"
  },
  "interval_seconds": 120
}
```

Listeners are picked up by the running daemon; they can be inspected and switched on or off
without restarting it:

```
orchestra listener list
orchestra listener enable <name>
orchestra listener disable <name>
```

See `examples/listeners/` for further listener examples.

## projects, issues and roles

This is the autonomous end of orchestra. A **project** is a repository-independent unit owning a
tree of **issues**; tasks attach to issues, worker agents *claim* them, reviewer agents approve or
reject, and the built-in `merger` backend lands the pull request. All of it is stored in a
[taxis](https://github.com/chrisflav/taxis) instance configured under `taxis` in `config.json` —
issues created in the taxis UI behave exactly like ones created through the tools.

A **role** (`<config>/roles/<name>.json`) is a reusable task template: backend, prompt template,
tool permissions, and a dispatch policy. `examples/projects/roles/` ships `implementor`,
`reviewer`, `planner` and `maintainer`. A `project-dispatcher` or `label-dispatcher` listener
spawns them as work appears, up to per-role caps, taking a claim lock so no two agents land on the
same issue.

```sh
orchestra project create "API v2" --default-repo myorg/api --default-branch main
orchestra issue add <project-id> --title "Rewrite the auth handler"
orchestra issue list <project-id> --status open
orchestra issue show <issue-id>
orchestra roles list <project-id>
orchestra spawn implementor <project-id> --issue <issue-id>
orchestra project health <project-id>          # find claims whose task is gone
```

[`examples/projects/README.md`](examples/projects/README.md) is the full reference: how orchestra
concepts map onto taxis, the `taxis` config block, role templates and their prompt variables, the
dispatch triggers, and how the two dispatchers differ.

## dashboard

A read-only web view of everything above: the queue and concert runs, listeners and when they last
checked, task history with the full structured log of each run, projects with their issue
dependency graph, and every configured authentication source with the usage limits last reported
for it. Pages stream updates over Server-Sent Events, so they stay current without a reload.

The UI is a React/TypeScript app under [`web/`](web/), built by Vite; the backend is the Lean
server behind `orchestra dashboard`, which answers the JSON API, its SSE streams, and — with
`--site` — the built front-end, all on one port:

```sh
cd web && npm ci && npm run build && cd ..
orchestra dashboard --site web/dist --port 8080
```

`web/dist` is a build artifact and is not tracked, so `npm run build` has to run before
`--site` has anything to point at. During front-end work `npm run dev` is the better loop: it
serves the app with hot reload and proxies `/api` and `/sse` through to a `orchestra dashboard`
running on 8080, which keeps the app same-origin in development exactly as it is in production.

Access is gated by a password. The login screen exchanges it for an `HttpOnly`,
`SameSite=Strict` session cookie, so the secret is never held in `localStorage` and never rides
in a URL — including on the SSE streams, which authenticate with the same cookie. The password
comes from `--password`, `$ORCHESTRA_DASHBOARD_PASSWORD`, or one generated on first run and
persisted to `<data>/dashboard.secret`; a generated one is printed at start-up. Scripts can skip
the login and send `Authorization: Bearer <password>` instead:

```sh
curl -H "Authorization: Bearer $(cat ~/.local/share/orchestra/dashboard.secret)" \
     http://127.0.0.1:8080/api/v1/overview
```

The server binds loopback unless `--host` says otherwise — it is plain HTTP behind that
password, so anything wider wants TLS in front, plus `--secure-cookie` so the session cookie is
only ever sent over HTTPS. The docker image builds the front-end in a Node stage and runs the
server as [its own container](docker/README.md#dashboard).

The **Auth** page is the one to open when the queue has pending work but nothing is running: it
names the limit that is binding on each source and when it lifts — the same data as
[`orchestra usage`](#usage-limits), read from the usage store rather than polled, so opening the
page costs nothing.

### the API

The web UI is one client of the API, not the reason it has the shape it has. Anything that
speaks HTTP can read the same data, and the server describes itself:

```sh
curl http://127.0.0.1:8080/api/openapi.json     # needs no credential
```

That document is embedded in the binary, so it describes the server answering rather than some
checkout, and a test fails the build if a route and the spec disagree.

Reads live under `/api/v1/`, and every one of them is also a Server-Sent Events stream at the
same path under `/sse/v1/` — identical payload, pushed when it changes and not otherwise, which
is what makes it cheap to sit on:

```sh
curl -N -H "Authorization: Bearer $PASSWORD" http://127.0.0.1:8080/sse/v1/overview
```

Four conventions hold everywhere:

- **Instants** are RFC 3339 UTC in a `...At` field. Never a rendered phrase — `"3m ago"` cannot
  be compared or thresholded, and it is only useful if you read English.
- **Durations** are integer seconds, in a `...Seconds` field.
- **Absent** is `null`. `""` means present and empty, which for a name is a different fact.
- **Collections** answer in one envelope — `items`, `total`, `limit`, `offset` — and take
  `limit`, `offset`, and, where they are ordered by time, `since`. `total` counts matches
  before the window, so "50 of 812" needs no second request. A parameter that is malformed, or
  that a collection cannot honour, is a `400` rather than a shrug.

```sh
# the twenty most recent tasks since yesterday
curl -H "Authorization: Bearer $PASSWORD" \
     "http://127.0.0.1:8080/api/v1/tasks?limit=20&since=2026-07-22T00:00:00Z"
```

The API is read-only. Nothing here enqueues, cancels or reconfigures anything: the only routes
that are not `GET` are the two that open and close a session.

## other commands

```
orchestra prepare <upstream> <fork>   # clone the fork and configure remotes
orchestra cleanup                     # remove all cloned repositories
orchestra cleanup list                # list clones and their task slots
orchestra mcp <upstream> <fork>       # start the MCP server standalone
orchestra usage                       # usage limits of every configured auth source
orchestra dashboard --site web/dist   # the web dashboard (API, SSE and UI on one port)
orchestra migrate                     # move ~/.agent/ to the XDG directories
```

## container

The `container/` directory contains a NixOS image definition for incus. It
installs all required tools (`claude-code`, `elan`, `gh`, `landrun`,
`mistral-vibe`, and others) and creates an `orchestra` user.

To build the container image you need distrobuilder and incus:

- distrobuilder: https://linuxcontainers.org/distrobuilder/docs/latest/
- incus: https://linuxcontainers.org/incus/docs/main/installing/

Build the image (from the `container/` subdirectory):

```
distrobuilder build-incus nixos.yaml
```

Import and start the container:

```
incus image import incus.tar.xz rootfs.squashfs --alias orchestra
incus launch orchestra my-orchestra --config security.nesting=true
incus exec my-orchestra -- nixos-rebuild switch
```

The last command installs the software in the container. To login as the `orchestra`
user:

```
incus exec my-orchestra -- su orchestra
```

## docker

`docker/` packages the queue daemon and the same dependency set as an image, for hosts without
incus. See [`docker/README.md`](docker/README.md) for the details.

```
cd docker
cp .env.example .env      # fill in at least ORCHESTRA_TAXIS_URL and a token
docker compose up --build
```

Two containers come up off the one image: the daemon, and the [dashboard](#dashboard) on
<http://127.0.0.1:8080> (`docker compose logs dashboard` prints the password it asks for). They are
separate because the daemon drains in-flight tasks for up to half an hour on every stop, and a
read-only web view should not be unavailable for that long.

The daemon container runs `orchestra queue start`; override the command for one-off subcommands
against the same volumes:

```
docker compose run --rm orchestra project list
```

Config and state live in gitignored host directories — `docker/config/`, `docker/data/` and
`docker/secrets/` — created on first `up`, so `config/orchestra/config.json` can be edited
directly and a GitHub App key just gets dropped in `secrets/`.

It expects an existing taxis instance rather than starting one — `ORCHESTRA_TAXIS_URL` must be
reachable from inside the container, so not `localhost`. Agents are sandboxed with landrun
(Landlock), which works under Docker's default seccomp profile; the entrypoint probes it at
start-up and `docker/README.md` covers what to check if that warning appears.
