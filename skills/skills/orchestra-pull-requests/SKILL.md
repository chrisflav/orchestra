---
name: orchestra-pull-requests
description: Open pull requests, comment on them, label and merge them, and read review feedback from inside an orchestra task. Use whenever you are about to create a PR, reply to review comments, or comment on the GitHub issue or PR a task was launched from — and before reaching for `gh`, which is right in one setup and wrong in the other.
---

# Pull requests and GitHub comments

There are two ways an orchestra task reaches GitHub, and which one applies to you is decided by
your tool list, not by you:

- **Your MCP server offers `create_pr`, `comment` or `get_pr_comments`** — use those tools, as
  described under *With orchestra's tools* below, and do not use `gh` for pull requests or issues.
- **It offers none of them, and `HTTPS_PROXY` is set in your environment** — GitHub is reached
  through a proxy that holds the credentials, and you use `gh api` and `git` yourself. See
  *Through the proxy* first; the sections after it describe the tools and do not apply.

# Through the proxy

Every request you make to github.com and api.github.com goes through a proxy that decides, per
request, whether this task may make it, and attaches the credential if so. You never see a GitHub
token, and `GH_TOKEN` in your environment is a placeholder. Your system prompt names the fork
you push to, the upstream you open pull requests on, and the issue or pull request the task was
launched from.

What decides what you may do is the proxy, so a refusal is final: it answers `403` with the check
that failed quoted in the body. Report it; do not look for another way to make the same request.

## Use `gh api`, not `gh pr` or `gh issue`

`gh pr create`, `gh pr merge`, `gh pr review`, `gh issue comment` and most other `gh pr`/`gh issue`
commands go through GitHub's GraphQL API as *mutations*, and the proxy refuses every GraphQL
mutation. Use the REST endpoints through `gh api`, which the proxy can read. GraphQL *queries*
(`gh api graphql -f query='query { … }'`, and read-only commands like `gh pr view`) are fine.

Plain `git` — clone, fetch, push — works against the real URLs as usual.

## Opening a pull request

Push your branch to the fork with `git push`, then, against the upstream:

```
gh api repos/UPSTREAM_OWNER/UPSTREAM_REPO/pulls \
  -f head=FORK_OWNER:my-branch -f base=main -f title='…' -f body='…'
```

The head must be a branch of your fork, written `FORK_OWNER:branch`. To open it on the fork
itself instead, post to `repos/FORK_OWNER/FORK_REPO/pulls` with `-f head=my-branch`. Only a task
granted `create_pr` may do either.

If your system prompt lists pull request labels, add them once the pull request exists — the
number is in the response's `number`:

```
gh api repos/UPSTREAM_OWNER/UPSTREAM_REPO/issues/NUMBER/labels -f 'labels[]=LABEL'
```

A label the repository does not have yet is created with
`gh api repos/UPSTREAM_OWNER/UPSTREAM_REPO/labels -f name=LABEL -f color=ededed` first. Other
labels are refused.

## Commenting

Only on the issue or pull request the task was launched from, and only with `comment` granted:

```
gh api repos/O/R/issues/N/comments -f body='…'                    a comment
gh api repos/O/R/pulls/N/reviews -f event=COMMENT -f body='…'      a review (COMMENT only)
gh api repos/O/R/pulls/N/comments/COMMENT_ID/replies -f body='…'   reply to an inline comment
gh api repos/O/R/pulls/N/comments -f body='…' -f commit_id=SHA \
  -f path=src/x.lean -F line=42 -f side=RIGHT                      a new inline comment
```

A review with inline comments takes a JSON body: write it to a file and pass `--input file.json`.

## Reading review feedback

```
gh api repos/O/R/pulls/N/comments --paginate          inline comments
gh api repos/O/R/issues/N/comments --paginate         the conversation
gh api graphql -f query='query { repository(owner: "O", name: "R") { pullRequest(number: N) {
  reviewThreads(first: 100) { nodes { isResolved isOutdated
    comments(first: 50) { nodes { databaseId path line body author { login } } } } } } } }'
```

The GraphQL form is the one that says which threads are resolved or outdated.

## Labelling and merging

With `label_issue`, any issue or pull request on the upstream:
`gh api repos/O/R/issues/N/labels -f 'labels[]=t-bug'`, and
`gh api -X DELETE repos/O/R/issues/N/labels/needs-triage` to remove one.

With `merge_pr`: `gh api -X PUT repos/O/R/pulls/N/merge -f merge_method=squash`. A merge GitHub
refuses — conflicting, blocked by branch protection, already merged — says why in its response;
report that rather than retrying.

# With orchestra's tools

Everything that touches a pull request or a GitHub issue goes through orchestra's MCP tools.

## Never use `gh` for this

Do **not** run `gh pr create`, `gh pr merge`, `gh pr comment`, `gh pr review`, `gh issue create`,
`gh issue comment`, `gh issue edit`, `gh label`, `gh api`, or any other `gh` command that reads or
writes pull requests and issues. Do not use `curl` against `api.github.com` either.

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

## Merging a pull request

```
merge_pr(pr_number: 123)
merge_pr(pr_number: 123, merge_method: "rebase", delete_branch: false)
```

- `pr_number` (required) — the pull request on the **upstream** repository.
- `merge_method` — `"squash"` (default), `"merge"`, or `"rebase"`.
- `delete_branch` — delete the head branch afterwards; defaults to true.

Most tasks are not granted this tool, and a refusal means this task is not the one that decides
whether the pull request lands. When a merge is refused because the pull request is already
merged, closed, still a draft, conflicting, or blocked by branch protection, the tool says which:
report that back instead of calling again, since none of those clear up by retrying.

## Labelling (triage)

```
label_issue(issue_number: 42, add: ["t-bug"])
label_issue(issue_number: 42, add: ["t-bug"], remove: ["needs-triage"])
```

- `issue_number` (required) — an issue **or** pull request on the upstream repository; they share
  one numbering. This is the one tool here that is not restricted to the issue the task was
  launched from.
- `add` / `remove` — lists of label names. Give at least one label between them.

Only labels the repository already defines can be applied. An unknown name is refused, and the
refusal lists the labels that do exist — pick one of those rather than asking again; the tool
will not create a label for you. Spelling is matched case-insensitively, so `T-Bug` finds
`t-bug`.

Adding a label the issue already has, or removing one it does not, is reported and skipped
rather than failing, so a repeated call is harmless.

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
