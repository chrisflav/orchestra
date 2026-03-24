# orchestra

A CLI tool for managing and sandboxing coding agents. It clones repositories,
authenticates via a GitHub App, runs an agent inside a landrun sandbox, and
optionally creates pull requests to an upstream repository through a built-in
MCP server.

## prerequisites

It is recommended to run all of orchestra inside a virtual machine or container. The
[container section](#container) describes a ready-made NixOS incus image that
provides the full environment.

Before starting you will need to create a GitHub App with a private key, installed on the organization owning the fork. Download the private key.

Inside the container (or VM), the following must be available (all installed
automatically when using the provided container image):

- [Lean 4 / Lake](https://leanprover.github.io/lean4/doc/setup.html) to build
  the tool
- [landrun](https://github.com/Zouuup/landrun) for sandboxing
- `claude` (Claude Code CLI) and/or `vibe` (mistral-vibe) installed and
  authenticated
- `gh` (GitHub CLI) for repository operations

In addition, the private key from the GitHub App must be included as a file in the container.

## building

```
lake build orchestra
```

The resulting binary is at `.lake/build/bin/orchestra`.

## configuration

Create `~/.agent/config.json`:

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
  "claude_token": "..."
}
```

`installation_id` is optional; if omitted it is looked up automatically.
`pat` is a personal access token used to create pull requests to the upstream
repository. The agent itself never sees this token.

`claude_token` is an optional long-lived Claude OAuth token. Claude login
sessions lapse frequently; a stable token avoids repeated re-authentication.
Obtain one by running `claude setup-token` and copy the token value here. When
set, it is passed to the agent as the `CLAUDE_CODE_OAUTH_TOKEN` environment
variable.

System prompts can be placed in `~/.agent/prompts/`. The file
`~/.agent/prompts/default.md` is loaded automatically; named prompts can be
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
- `system_prompt` — optional name of a file in `~/.agent/prompts/` (without
  `.md`); defaults to `default.md` if present
- `backend` — `"claude"` (default) or `"vibe"`
- `model` — optional model override passed to the agent

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

A repository can provide a `.agent/` directory with optional hooks and a config
file:

- `.agent/init.sh` — run once after cloning
- `.agent/before.sh` — run before each agent launch
- `.agent/validation.sh` — run after each agent launch; non-zero exit triggers
  a retry
- `.agent/after.sh` — run after the validation loop completes
- `.agent/config.json` — validation settings:

```json
{
  "validation": {
    "max_retries": 3,
    "retry_prompt": "Validation failed. Please fix the issues."
  }
}
```

## other commands

```
orchestra prepare <upstream> <fork>   # clone the fork and configure remotes
orchestra cleanup                     # remove all cloned repositories
orchestra mcp <upstream> <fork>       # start the MCP server standalone
```

## container

The `container/` directory contains a NixOS image definition for incus. It
installs all required tools (`claude-code`, `elan`, `gh`, `landrun`,
`mistral-vibe`, and others) and creates an `orchestra` user with SSH access.

Before building, edit `container/nixos.yaml` and replace `"your public key
here"` with your actual SSH public key.

To build the container image you need distrobuilder and incus:

- distrobuilder: https://linuxcontainers.org/distrobuilder/docs/latest/
- incus: https://linuxcontainers.org/incus/docs/main/installing/

Build the image (from the `container/` subdirectory):

```
distrobuilder build-incus nixos.yaml
```

Import and start the container:

```
incus image import incus.tar.xz rootfs.squashfs  --alias orchestra
incus launch orchestra my-orchestra --config security.nesting=true
incus shell my-orchestra
# nixos-rebuild switch
```

The last command installs the software in the container.
