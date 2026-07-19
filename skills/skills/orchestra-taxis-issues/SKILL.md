---
name: orchestra-taxis-issues
description: Work with taxis issues from inside an orchestra task — claim one, split it, attach a PR, release it, or create and update issues in the tracker. Use whenever you need to find what to work on, record progress against an issue, or restructure a project's issue tree. Taxis issues are not GitHub issues; `gh` cannot see them.
---

# Taxis issues

Orchestra's projects, issues, and claims live in a **taxis** tracker, not on GitHub. They are
reached only through orchestra's MCP tools.

## Taxis issues vs GitHub issues

These are two different systems and confusing them is the most common mistake here.

| | Taxis issue | GitHub issue |
| --- | --- | --- |
| Identified by | A taxis issue id (integer, tracker-wide) | An issue number (integer, per repository) |
| Lives in | The taxis instance | github.com |
| Reached with | `claim_issue`, `get_issue`, `create_issue`, … | `comment`, `get_pr_comments` |
| Is | The unit of work you are assigned | The thread a task was launched from |

`gh` cannot see taxis issues at all, and the taxis tools cannot see GitHub issues. Never pass an
id from one into a tool belonging to the other — the numbers look alike and nothing will stop
you. And never use `gh issue` for either: GitHub issues go through `comment` (see the
`orchestra-pull-requests` skill).

## Never use `gh` or curl for tracker work

There is no `gh` equivalent for taxis, and hand-rolled HTTP against the taxis API is not a
substitute for these tools. The tools apply the claim protocol, the status mapping, and the
write-scoping rules described below; a raw API call silently skips all three.

## Finding and claiming work

Most tasks are dispatched already bound to an issue — the prompt names the issue id and its
description. When you need to look for work yourself:

```
list_open_issues(project_id: 42)          open, unclaimed issues
claim_issue(issue_id: 57)                 take one; returns the target repo and branch
```

An issue may be held by one task at a time. `claim_issue` fails with `already_claimed` if
somebody else holds it — pick another rather than trying to force it. Claiming is what marks the
issue as being worked; do not start work on an issue you have not claimed.

When finished, or when abandoning:

```
release_claim(issue_id: 57, reason: "...")
```

## Recording a pull request

Once the PR is open (via `create_pr` — see the `orchestra-pull-requests` skill):

```
attach_pr(issue_id: 57, repo: "owner/name", number: 123, branch: "my-branch")
```

Attaching is what puts the issue in front of a reviewer: the dispatcher queues one for any open
issue carrying a pull request that is not yet merged. Opening a PR without attaching it leaves the
issue looking unstarted and nobody will review it.

You must hold the claim on the issue to attach to it.

## When an issue is too big

```
split_issue(parent_id: 57, reason: "...", children: [{title, description}, ...])
```

The children become open and pickable, and your claim on the parent is released. Prefer this over
half-doing a large issue. You must hold the claim on the parent.

A parent with open children is not dispatched — the children are the work. It becomes available
again on its own once they are all completed or abandoned. There is no "blocked" status to set or
clear; being decomposed is read from the tree.

## Creating and updating issues

```
list_issues(project_id: 42, status: "open", parent_id: 57)
get_issue(issue_id: 57)                    full detail: children, attached PRs, dependencies
create_issue(project_id: 42, title: "...", description: "...", parent_id: 57)
update_issue(issue_id: 57, title/description/status/target_repo/target_branch/dependency_ids)
```

Ids are **integers**, taxis's own convention — `42`, not `"42"`.

Put the actual task in the `description`. It is what a worker agent is shown; a title alone leaves
it guessing.

`dependency_ids` lists issues that must be finished before this one is dispatched. A dependency
blocks only while it is still **open** — completing or abandoning it unblocks the dependent, so
abandoning work does not strand everything downstream. Use this for real ordering constraints
rather than encoding them in prose.

### Writes are confined to your project subtree

`create_issue` and `update_issue` may only touch issues at or below **your** project root — the
nearest ancestor of your issue that carries `t-project` or has both a `repository` and a
`github-branch` artifact. Reaching outside is refused, naming the issue and the root.

So you can create children under your root or anything beneath it, and update anything in that
range. You cannot retitle another project's issues. Reads are not restricted: `list_issues` and
`get_issue` see the whole tracker, which is fine for context.

## Comments — where review feedback lives

Every taxis issue has a comment thread. It is the record of *why* something was decided, and the
only thing the next agent to pick the issue up will see.

```
list_issue_comments(issue_id: 57)          read the thread
comment_issue(issue_id: 57, body: "...")   add to it
```

**Read the thread before reworking an issue that came back to open.** There is no "rejected"
status — a rejection *is* the request-changes review on the thread, and it is the only record of
what was wrong. The merger records validation failures the same way. Without reading it you will
not know what to fix and are likely to resubmit the same work.

`get_issue` also includes the thread, so if you are already fetching detail you have it. Role
prompts can carry it directly too, via `{{issue_comments}}` — if your prompt already shows the
discussion, you do not need to fetch it again.

These are comments on the **taxis** issue. Do not confuse them with `comment`, which posts to the
GitHub issue or pull request the task was launched from — a different system, see the
`orchestra-pull-requests` skill.

## Reviewing

```
list_issues_in_review(project_id: 42)
decide_issue(issue_id: 57, decision: "approve" | "complete" | "reject", notes: "...")
```

| Decision | Effect |
| --- | --- |
| `approve` | Enqueues the merger for the latest attached PR. The PR lands; **the issue stays open.** |
| `complete` | Marks the issue finished. Merges nothing. |
| `reject` | Returns the issue to open, with your notes recorded as a request-changes review. |

Approving is a verdict on the *pull request*, not on the issue: one PR is rarely the whole issue,
and an issue may collect several. So approve when the code should land, and `complete` separately
once the issue's actual goal is met — which may be after several approved PRs, or immediately
after one.

Judge the PR against what its issue asked for, which `get_issue` will show you.

`decide_issue` records your `notes` as a **review comment** on the issue automatically, carrying
the verdict, so it renders as a review in taxis and a rejection always leaves a trace the next
worker can read. Put the actual reasoning in `notes` — "rejected" on its
own tells them nothing. Use `comment_issue` for anything beyond the verdict, such as replying to a
worker's question or leaving guidance without deciding yet.

## Which tools you have

Tools are gated per task by permission group:

- `manage_issues` — `list_projects`, `list_issues`, `get_issue`, `create_issue`, `update_issue`
- `work_issues` — `list_open_issues`, `claim_issue`, `release_claim`, `attach_pr`, `split_issue`
- `review_issues` — `list_issues_in_review`, `decide_issue`

`list_issue_comments` comes with any of the three, and `comment_issue` with `work_issues` or
`review_issues` — the thread is shared ground between whoever reviews and whoever reworks.

A refusal saying the task is not authorized for a group is deliberate, not a bug. Report what you
needed; do not look for another route to it.
