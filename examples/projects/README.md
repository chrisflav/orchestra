# Project example layout

A **project** is a repository-independent organisational unit owning a tree
of issues. Tasks attach to issues; worker agents claim issues; reviewer
agents approve or reject; a built-in `merger` backend lands the PR.

This directory shows the on-disk shape under `~/.agent/projects/`. You
normally never hand-edit these files — use `orchestra project` /
`orchestra issue` or the MCP tools (`manage_issues`, `work_issues`,
`review_issues`) — but the layout is documented here for inspection and
backup.

```
~/.agent/projects/
  <project-id>.json              # Project record
  <project-id>/
    issues/<issue-id>.json       # Issue record (parentId, attached PRs, status, …)
    claims/<issue-id>.json       # Per-issue lock; presence == "claimed"
```

Files in this directory:

- `sample-project.json` — a project with a default target plus an optional
  reviewer template (F1 auto-enqueue).
- `sample-issue-root.json` — a top-level issue.
- `sample-issue-child.json` — a sub-issue (`parent_id` set).

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

Tool permission groups (set via the task config's `tools` list):

- `manage_issues` — plan agents: list / create / update issues + sub-issues.
- `work_issues` — worker agents: list open issues, claim, attach PR, release.
- `review_issues` — reviewer agents: list in-review, approve / reject.
