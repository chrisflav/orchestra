# Projects

A **project** is a repository-independent organisational unit owning a tree
of issues. Tasks attach to issues; worker agents claim issues; reviewer
agents approve or reject; a built-in `merger` backend lands the PR.

Project/issue/claim data is backed by a [taxis](https://github.com/chrisflav/taxis) issue tracker
instance — there is no on-disk project/issue storage anymore. See "Configuring taxis" below to
point orchestra at one; until that's done, every `orchestra project`/`orchestra issue` command and
the `manage_issues`/`work_issues`/`review_issues` MCP tools fail with a "taxis is not configured"
error.

## How orchestra concepts map onto taxis

| Orchestra concept | taxis representation |
| --- | --- |
| Project | A taxis issue carrying the `t-project` label. `Project.name`/`.description` are the issue's `title`/`description`. |
| Issue | A taxis issue whose `parent` is the project's issue (root issues) or another orchestra issue (sub-issues created by `split_issue`). |
| Issue status (`open`/`claimed`/`in_review`/`blocked`/`completed`/`abandoned`/`rejected`) | taxis `state` (`open`/`closed`/`completed`) plus at most one status label (`o-claimed`, `o-in-review`, `o-blocked`, `o-rejected`); `open` state with none of those labels is `open`, `closed` is `abandoned`. |
| Dependencies | taxis's native issue-dependency graph — no separate plumbing. |
| Claim (which task currently holds an issue) | A `session`-kind artifact on the issue (`{task_id, agent, series?, claimed_at}`) — "claimed" means the issue has one. |
| Attached PR | A `github-pr`-kind artifact on the issue. |
| `RepoTarget` override / reviewer template | Not a native taxis field — encoded as a JSON blob in a trailing ` ```orchestra-meta ` fenced block appended to the issue's description, stripped before a human sees it. |

`o-claimed`/`o-in-review`/`o-blocked`/`o-rejected`/`t-project` are created automatically (if
missing) the first time they're needed, so nothing needs to be pre-provisioned on the taxis side
beyond the instance itself and an actor with permission to manage labels (an admin — see
"Configuring taxis").

CLI cheat sheet:

```sh
# Create a multi-org project (no default target — every issue must specify)
orchestra project create "Mono-repo cleanup"

# Create a project bound to one repo
orchestra project create "API v2" --default-repo myorg/api --default-branch main

# List
orchestra project list
orchestra issue list <project-id>
orchestra issue list <project-id> --status open

# Add issues
orchestra issue add <project-id> --title "Top-level task"
orchestra issue add <project-id> --title "Subtask" --parent <issue-id>
orchestra issue add <project-id> --title "Cross-org" \
  --target-repo other/repo --target-branch dev

orchestra issue show <issue-id>
orchestra issue close <issue-id>      # marks abandoned (review flow handles completed)
```

`<project-id>`/`<issue-id>` are taxis's own numeric issue ids (a project is itself a taxis issue,
so `orchestra issue show <project-id>` also works, showing it like any other issue).

Tool permission groups (set via the task config's `tools` list):

- `manage_issues` — plan agents: list / create / update issues + sub-issues.
- `work_issues` — worker agents: list open issues, claim, attach PR, release, split into sub-issues.
- `review_issues` — reviewer agents: list in-review, approve / reject.

## Configuring taxis

Add a `taxis` section to `config.json` (`$XDG_CONFIG_HOME/orchestra/config.json`, falling back to
`~/.config/orchestra/config.json`, with a legacy `~/.agent/config.json` fallback if that's what you
already have — see `Orchestra.Dirs`):

```json
{
  "taxis": {
    "url": "http://localhost:8080",
    "token": "{{taxis_token}}"
  }
}
```

`token` supports the same `{{secret}}` substitution as the rest of `config.json` — put the real
value in `secrets.json` (`$XDG_CONFIG_HOME/orchestra/secrets.json`) rather than committing it.

The token must be an API token (`POST /api/me/tokens` while authenticated in the taxis UI, or
`GET|POST /actors/:id/tokens` as an admin to mint one for a bot actor — see taxis's own README) —
not a session-login token. Its actor needs **admin** rights on the taxis instance: creating the
`t-project`/`o-claimed`/`o-in-review`/`o-blocked`/`o-rejected` labels on first use requires it.

Without a `taxis` section, `orchestra project`/`orchestra issue`/`manage_issues`/`work_issues`/
`review_issues` all fail with a clear "taxis is not configured" error; every other orchestra
feature (task running, listeners, concerts, ...) is unaffected.

### Projects created before the taxis migration

There is no automatic import. Projects and issues written by an older orchestra still sit under
`<data>/projects/` as `<project-id>.json` and `<project-id>/issues/*.json`, and are simply not
read anymore — nothing deletes them, but nothing finds them either. Recreate the ones you still
care about with `orchestra project add` / `orchestra issue add` against your taxis instance.

That directory is not dead, though: **roles stay file-based** and are still read from
`<data>/projects/<project-id>/roles/` — where `<project-id>` is now the taxis issue id of the
project. If you are recreating a pre-migration project, move its `roles/` directory across to
the new id.

## Roles

A *role* is a reusable task template — backend, prompt template, the
permission set the agent gets, and an optional auto-dispatch policy.
Role names are user-defined; only the dispatcher's set of triggers is
fixed (`has_open_issues` | `has_in_review_issues` | `idle`).

Roles are the one part of this subsystem that's still plain files, not taxis-backed (project
overrides global by name):

```
$XDG_CONFIG_HOME/orchestra/roles/<name>.json                        -- global
$XDG_DATA_HOME/orchestra/projects/<project-id>/roles/<name>.json     -- per-project override
```

(`~/.config/orchestra`/`~/.local/share/orchestra` if the `XDG_*` variables aren't set; see
`Orchestra.Dirs`.)

Examples in `roles/`: `implementor.json`, `reviewer.json`, `planner.json`.
Each ships with `dispatch.max: 0` so auto-spawn is opt-in — set caps in a
dispatcher listener config (see `examples/listeners/auto-dispatcher.json`)
to enable.

Manual spawning:

```sh
orchestra roles list <project-id>                       # see what's available
orchestra spawn <role-name> <project-id> [--prompt ...]  # ad-hoc spawn
orchestra spawn implementor <project-id> --issue <iid>   # pre-claims via daemon
```

Auto-dispatch listener:

```json
{
  "source": {
    "type": "project-dispatcher",
    "project_id": 42,
    "caps": { "implementor": 2, "reviewer": 1, "planner": 1 }
  },
  "interval_seconds": 30
}
```

Each tick the dispatcher counts queue entries `(status ∈ {pending, running}) ∧ projectId == this ∧ role == X`, and if `count < cap` and the role's trigger holds, enqueues exactly one new entry per role.

### Project-independent dispatch

`label-dispatcher` does the same thing without being tied to one project: it works on **every**
taxis issue carrying a given label, wherever in the tracker it lives.

```json
{
  "source": {
    "type": "label-dispatcher",
    "label": "agent-ready",
    "caps": { "implementor": 2, "reviewer": 1 }
  },
  "interval_seconds": 30
}
```

Because these issues have no orchestra project behind them, there is no `defaultTarget` to
inherit. The target is read off taxis artifacts instead, walking from the issue up its parent
chain and taking the **nearest** ancestor carrying each:

| What | Artifact | Field |
| --- | --- | --- |
| Repository | `repository` | `url`, parsed for `owner/repo` |
| Branch | `github-branch` | `branch` |

So a project-level issue can carry the `repository` artifact once and every issue beneath it
inherits it, while a single sub-issue can pin its own branch by carrying its own `github-branch`.

Both are required. An issue carrying the label but missing either is **skipped with a warning**
naming which half is missing — dispatching an agent at a guessed repository is worse than not
dispatching:

```
[dispatcher] issue 57 is labelled 'agent-ready' but has no repository artifact
  on it or any ancestor; skipping
```

Two further differences from `project-dispatcher`:

- **Only global roles apply** (`<config>/roles/`). The issues can span projects, so no single
  project's `roles/` directory takes precedence.
- **Caps are scoped to the labelled set** — they bound concurrent work on labelled issues rather
  than colliding with per-project dispatchers using the same role names.

The ancestor carrying the `repository` artifact stands in for the project: it fills the queue
entry's `project_id` and supplies `{{project_name}}` when the role prompt is rendered. Issues
carrying the `t-project` label are skipped even if labelled, since they are containers rather
than units of work.
