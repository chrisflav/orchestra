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
| Issue status (`open`/`claimed`/`completed`/`abandoned`) | taxis `state` (`open`/`closed`/`completed`) plus the single `o-claimed` label; `open` without it is `open`, `closed` is `abandoned`. Three conditions have no status of their own and are read from the data: **decomposed** (has open children), **awaiting review** (open with an unmerged pull request attached), and **rejected** (latest review comment asks for changes). |
| Dependencies | taxis's native issue-dependency graph — no separate plumbing. A dependency holds its dependent back only while it is still open; completed *or* abandoned releases it. |
| Claim (which task currently holds an issue) | A `session`-kind artifact on the issue (`{task_id, agent, series?, claimed_at}`) — "claimed" means the issue has one. |
| Attached PR | A `github-pr`-kind artifact on the issue. |
| `RepoTarget` override / reviewer template | Not a native taxis field — encoded as a JSON blob in a trailing ` ```orchestra-meta ` fenced block appended to the issue's description, stripped before a human sees it. |

`o-claimed`/`t-project` are created automatically (if
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
- `review_issues` — reviewer agents: list issues awaiting review, approve / complete / reject.

A reviewer is queued for **any open issue with an unmerged pull request attached**, including one
with children — a container can have a PR of its own. Merge state comes from GitHub, so an issue
whose PR was merged by hand stops being queued too. `approve` lands the PR and leaves the issue
open; `complete` is the separate decision that the issue's work is done.

Reviews are recorded on the taxis issue's comment thread: `decide_issue` posts its `notes` there
automatically as a native review comment carrying the approve / request-changes verdict, and `comment_issue` / `list_issue_comments` cover the rest. Reading is available to
all three groups and writing to `work_issues`/`review_issues`, so a rejected issue carries its
reasoning forward to whoever picks it up next. These are taxis comments, distinct from the
`comment` tool, which posts to the GitHub issue or PR a task was launched from.

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
`t-project`/`o-claimed` labels on first use requires it.

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
fixed (`has_open_issues` | `has_in_review_issues` | `idle` | `always`).

Roles are the one part of this subsystem that's still plain files, not taxis-backed (project
overrides global by name):

```
$XDG_CONFIG_HOME/orchestra/roles/<name>.json                        -- global
$XDG_DATA_HOME/orchestra/projects/<project-id>/roles/<name>.json     -- per-project override
```

(`~/.config/orchestra`/`~/.local/share/orchestra` if the `XDG_*` variables aren't set; see
`Orchestra.Dirs`.)

Prompt templates support `{{project_id}}`, `{{project_name}}`, `{{instructions}}`, and — when the
role is dispatched onto an issue — `{{issue_id}}`, `{{issue_title}}`, `{{issue_description}}`,
`{{issue_comments}}`, `{{target_repo}}`, `{{target_branch}}`, `{{pr_number}}`, `{{pr_branch}}`,
`{{pr_repo}}`. Unrecognised placeholders pass through; an absent value renders empty.

`{{issue_description}}` and `{{issue_comments}}` are the two worth putting in every worker
template. They are the only way an agent sees what an issue asks for and what a reviewer said
about it — `get_issue` and `list_issue_comments` render both, but a worker has to know to call
them, and a rejection lives nowhere else now that there is no rejected status.

Examples in `roles/`: `implementor.json`, `reviewer.json`, `planner.json`,
`maintainer.json`. Each ships with `dispatch.max: 0` so auto-spawn is opt-in — set caps in a
dispatcher listener config (see `examples/listeners/auto-dispatcher.json`)
to enable.

### The `always` trigger, and roles that wear every hat

The first three triggers hand the agent its job: `has_open_issues` binds an issue to work,
`has_in_review_issues` binds one to review, `idle` fires only when there is nothing of either.
`always` does none of that — it spawns whenever the role is under its cap, bound to no issue, and
lets the agent decide what the project needs next. `maintainer.json` is the example: it holds
`manage_issues` + `work_issues` + `review_issues`, so one agent can plan, implement and review.

Two consequences worth understanding before you enable one:

- **It claims for itself.** `pre_claim` is meaningless on an `always` role (there is no issue at
  spawn time to claim), so the agent must call `claim_issue` before working anything and must
  honour an `already_claimed` answer by picking something else. That call routes through the
  daemon's process-wide claim manager, the same mutex the dispatcher's pre-claims take, so two
  maintainers cannot end up on one issue. The template says so explicitly; keep that in yours.
  Whatever the agent claims and does not release is released for it when the task ends.
- **It bypasses `splitForDispatch`.** That is the mechanism keeping a worker and a reviewer off
  the same issue in one tick, and it only works on issues the dispatcher binds. An `always` role
  binds nothing, so if you run one *alongside* `has_open_issues`/`has_in_review_issues` roles,
  the maintainer can claim an issue a worker was dispatched onto in the same tick — the claim
  arbitrates, but one of the two spawns is then wasted. Simplest configuration is to let the
  maintainer own the project by itself:

```json
{
  "source": {
    "type": "project-dispatcher",
    "project_id": 42,
    "caps": { "maintainer": 3 }
  },
  "interval_seconds": 30
}
```

With `caps` above 1 you get several maintainers working the project concurrently; the claim
protocol is what keeps them off each other's issues.

Because the role is unbound, the project **must** have a `default_target` — there is no issue to
resolve a repository and branch from, and a role whose target cannot be resolved is silently
never dispatched.

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

It says what it checked and what it decided, for every role named in `caps`:

```
[dispatcher] project 42: 18 issues, 3 workable, 1 awaiting review; roles available: implementor, reviewer, planner
[dispatcher]   role 'implementor' (has_open_issues): DISPATCH, bound to issue 57
[dispatcher]   role 'reviewer' (has_in_review_issues): skip: 1 active, cap 1
[dispatcher]   role 'planner' (idle): skip: not idle (3 workable, 1 awaiting review)
```

A role named in `caps` that no role file defines, or one whose cap is zero, is reported too — those
are the two cases that otherwise look identical to "nothing was due".

### Project-independent dispatch

`label-dispatcher` does the same thing without being tied to one project: it works on **every**
taxis issue in scope for a given label, wherever in the tracker it lives.

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
  than colliding with per-project dispatchers using the same role names. Unbound (`always`) roles
  are capped per labelled root instead; see below.

The ancestor carrying the `repository` artifact stands in for the project: it fills the queue
entry's `project_id` and supplies `{{project_name}}` when the role prompt is rendered.

Two rules decide what gets dispatched:

- **The label is inherited.** An issue is in scope if it *or any transitive ancestor* carries the
  trigger label, so labelling a project once opts its whole subtree in rather than needing the
  label repeated on every issue.
- **Only workable issues are dispatched.** An issue can be worked on when it is open, has no
  open children, and has no open dependencies. Children still open mean it has been decomposed —
  the children are the work. Open dependencies mean it is waiting on something else. Both
  conditions count *open* only: a completed or abandoned child or dependency releases it, since
  abandoning is the decision that the work will not happen and stranding everything downstream
  would be worse.

Together these give the usual flow: label the project, a planner decomposes it, implementors pick
up the leaves. A labelled issue with no children yet is itself a leaf, so it dispatches until it
gains children, at which point the children take over — and when they are all finished it becomes
available again.

There is no "blocked" status. Being decomposed is read off the tree, so nothing has to remember
to set a flag when children appear or clear it when they finish, and it cannot disagree with the
actual hierarchy. Issues created directly in the taxis UI behave the same as ones created through
the tools.

### `always` roles here: one per labelled root

Both rules above are about picking an *issue* to bind, which an `always` role has none of. It is
placed per **labelled root** instead — every still-open issue carrying the trigger label
*directly*, rather than inheriting it. Each root gets its own set of maintainers:

```json
{
  "source": {
    "type": "label-dispatcher",
    "label": "agent-ready",
    "caps": { "maintainer": 2 }
  },
  "interval_seconds": 30
}
```

Three labelled roots and `"maintainer": 2` means up to six maintainers, two per root, each owning
its own subtree.

Four things differ from a bound role in this dispatcher:

- **The root is the project**, and it is the *labelled* issue — not the ancestor carrying the
  `repository` artifact, which is what a bound role's `project_id` points at. The two are often
  the same issue but need not be, and the labelled one is what scopes a maintainer's planning.
  It supplies `{{project_name}}`, the `project_id` on the queue entry, and — via its own
  artifacts — the target the entry is built against, so a root still needs a resolvable
  `repository` + `github-branch` like everything else.
- **Caps are counted per root**, over entries with a matching `project_id` and no `issue_id`.
  Filling one root's cap leaves the others alone.
- **Workability does not apply.** Nothing is pre-selected for a maintainer, so leaf-ness and
  dependencies do not gate it; it sees the whole subtree through its tools and decides. It is
  dispatched while its root is open, *including when the subtree has no open issues at all* —
  that is the case where you want it planning new work, and no other role in this dispatcher can
  create issues.
- **Nothing is pre-claimed.** `pre_claim` is ignored (there is no issue at spawn time), and the
  agent claims what it works through `claim_issue`, which takes the daemon's claim mutex. That
  is what keeps two maintainers on one root — or a maintainer and an implementor — off the same
  issue.

If the label exists but no *open* issue carries it directly, unbound roles have no root to scope
to and are reported rather than silently skipped:

```
[dispatcher] label 'agent-ready' has unbound roles configured (maintainer) but no open issue
  carries the label directly, so there is no root to scope them to; not dispatched
```

`idle` roles still cannot run in a label dispatcher — they bind nothing *and* have no root to
stand in for it. Use a `project-dispatcher`, or `always`.

Carrying `t-project` does not exempt an issue from dispatch. Trackers apply that label broadly,
including to leaves that are perfectly good units of work, so it cannot be used to tell containers
from work — having children is what does that.

### What an agent may modify

`manage_issues` writes are confined to the agent's own project subtree. The root of that subtree
is the nearest ancestor — the agent's own issue counts — that **anchors a project**: it carries
`t-project`, or it has both a `repository` and a `github-branch` artifact. Those are the same
markers the dispatcher uses to resolve a target, so "the project this work belongs to" means the
same thing on both sides.

That one anchor rule decides two separate things, and they agree by construction: which subtree
an agent may write to, and which issue is reported as an issue's *project* whenever a tool or the
CLI is handed a bare issue id. A tree assembled purely from artifacts — no `t-project` anywhere,
which is the normal shape under a label-dispatcher — is a first-class project on both counts.

Nearest wins. A sub-issue carrying its own `repository` + `github-branch` becomes the project for
everything beneath it, in preference to a `t-project` further up. That is deliberate: those two
artifacts are what say "work below here targets a different repository", and an agent under them
should inherit that target rather than the one belonging to the tree above.

`create_issue` may therefore place a new issue under the root or under anything beneath it, and
`update_issue` may only touch issues in that range. An attempt to reach outside is refused naming
both the issue and the root. If nothing up the chain qualifies, the agent's own issue becomes the
root, so it can still create children under itself rather than being locked out.

One exception, for roles dispatched without an issue: their project id is taken as the root
verbatim rather than re-derived. A label-dispatched maintainer is scoped to the *labelled* issue,
which typically anchors nothing itself — the repository artifact sits on an ancestor — so
re-deriving would silently widen its write access to that ancestor's whole subtree, including
siblings nobody labelled.

Reads are not scoped: `list_issues` and `get_issue` still see the whole tracker, which is useful
for context and cannot damage anything.
