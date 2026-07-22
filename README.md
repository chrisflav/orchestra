# orchestra

A CLI tool for managing and sandboxing coding agents. It clones repositories,
authenticates via a GitHub App, runs an agent inside a landrun sandbox, and
exposes GitHub actions (creating pull requests, posting comments) to the agent
through a built-in MCP server.

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
- `claude` (Claude Code CLI) and/or `vibe` (mistral-vibe) installed and
  authenticated
- `gh` (GitHub CLI) for repository operations
- [pi-mcp-adapter](https://github.com/nicobailon/pi-mcp-adapter) — **Optional**
  MCP OAuth support for the **Pi agent** specifically (not needed for Claude Code
  or vibe agents). Tested with the MCP tools defined in this project. Install in
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
      "mode": "pr",
      "prompt": "Implement feature X and open a pull request."
    }
  ]
}
```

Fields:

- `upstream` — upstream repository in `owner/repo` format
- `fork` — fork repository the agent has write access to
- `mode` — `"fork"` (work on the fork only) or `"pr"` (allow opening pull
  requests to upstream)
- `prompt` — instruction sent to the agent
- `agent` — optional sub-agent name passed to the backend
- `system_prompt` — optional name of a file in `~/.config/orchestra/prompts/` (without
  `.md`); defaults to `default.md` if present
- `backend` — `"claude"` (default) or `"vibe"`
- `model` — optional model override passed to the agent

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

Show the queue:

```
orchestra queue
```

Re-enqueue unfinished or cancelled entries:

```
orchestra queue retry
orchestra queue retry --series my-series
```

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

See `examples/listeners/` for further listener examples.

## other commands

```
orchestra prepare <upstream> <fork>   # clone the fork and configure remotes
orchestra cleanup                     # remove all cloned repositories
orchestra mcp <upstream> <fork>       # start the MCP server standalone
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

The container runs `orchestra queue start`; override the command for one-off subcommands against
the same volumes:

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
