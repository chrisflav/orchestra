---
name: orchestra-pull-requests
description: Open pull requests, comment on them, and read review feedback from inside an orchestra task. Use whenever you are about to create a PR, reply to review comments, or comment on the GitHub issue or PR a task was launched from. Also read this before reaching for `gh` — `gh pr`, `gh issue`, and `gh api` must not be used for these operations.
---

# Pull requests and GitHub comments

Everything that touches a pull request or a GitHub issue goes through orchestra's MCP tools.

## Never use `gh` for this

Do **not** run `gh pr create`, `gh pr comment`, `gh pr review`, `gh issue create`,
`gh issue comment`, `gh api`, or any other `gh` command that reads or writes pull requests and
issues. Do not use `curl` against `api.github.com` either.

`gh` is authenticated in the sandbox for **git transport only** — cloning, fetching, pushing.
Using it for PRs or issues bypasses the credential the task was given:

- `create_pr` targeting upstream authenticates with the configured PAT; targeting the fork mints
  a fresh GitHub App installation token. `gh` has neither selected for the repository you mean.
- The MCP tools record what a task did, so the queue entry, the taxis issue, and the PR stay in
  agreement. A `gh` call is invisible to all of that.
- Permission groups gate these tools per task. A task without `create_pr` is not supposed to open
  one; reaching for `gh` to get around a refusal defeats the point.

If a tool you need is missing or refuses, that is the answer — report it. Do not route around it.

Plain `git` is fine and expected: branch, commit, push. It is only the PR/issue *API* surface
that belongs to the MCP tools.

## Opening a pull request

Commit and push your branch with `git`, then:

```
create_pr(head: "my-branch", title: "...", body: "...")
```

- `head` (required) — the branch name in the fork.
- `base` — target branch; defaults to the repository's default.
- `target` — `"upstream"` (default) opens the PR on the upstream repository, cross-repo, using
  the PAT. `"fork"` opens it on the fork with a GitHub App token and needs no PAT.

Push the branch first. `create_pr` does not push for you, and a PR cannot be opened for a branch
GitHub has never seen.

## Commenting

```
comment(body: "...")                                  regular comment
comment(body: "...", review: true)                    PR review (COMMENT event)
comment(body: "...", review: true, inline_comments: [{path, line, body, side}])
comment(body: "...", reply_to_comment_id: 123)        reply to an inline review comment
comment(body: "...", path: "src/x.lean", line: 42)    new inline comment on a line
```

`review`, `reply_to_comment_id`, and `path`/`line` are mutually exclusive — pick one mode.

`comment` posts to the issue or PR **the task was launched from**, which the task carries as its
`issue_number`. It cannot post to an arbitrary issue, and there is no argument to redirect it. A
task launched without one cannot comment at all.

## Reading review feedback

```
get_pr_comments(pr_number: 123)
get_pr_comments(pr_number: 123, unresolved_only: true, exclude_outdated: true)
```

Use the filters when addressing feedback — resolved and outdated threads are usually noise.

## GitHub issues are not taxis issues

`issue_number` here is a **GitHub** issue or PR number, scoped to a repository, and it is only
ever the one the task was launched from.

The issues you *claim and work on* are **taxis** issues, identified by taxis issue ids, and they
live in a different system entirely with different tools. See the `orchestra-taxis-issues` skill.
The two never mix: never pass a taxis issue id to `comment` or `get_pr_comments`, and never pass
a GitHub issue number to `attach_pr`, `claim_issue`, or anything else in that group.

After opening a PR for a taxis issue you are working, attach it with `attach_pr` — that is what
moves the taxis issue into review. `create_pr` alone does not.
