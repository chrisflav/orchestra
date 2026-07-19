# Running orchestra in Docker

Packages the queue daemon (`orchestra queue start`) with everything it shells out to: `landrun`,
`git`, `gh`, `openssl`, the Lean toolchain, and the Claude Code CLI.

This is the Docker equivalent of `container/configuration.nix`, which packages the same thing for
NixOS/Incus. Both install the same dependency set; keep them in sync.

## Quick start

```sh
cd docker
cp .env.example .env      # fill in at least ORCHESTRA_TAXIS_URL and a token
docker compose up --build
```

The first build takes a while — it fetches the Lean toolchain and compiles orchestra and its
dependencies from source.

## What is in the image

| Component | Why |
| --- | --- |
| `orchestra` | The binary itself; entrypoint runs `queue start`. |
| `landrun` | Every agent launch goes through it (`Orchestra/Sandbox.lean`). No bypass exists. |
| `git`, `gh` | Cloning, branching, opening and merging PRs. |
| `openssl` | Signs the GitHub App JWT (`Orchestra/GitHub.lean`). |
| `elan` + `build-essential` | Agent tasks build the Lean projects they work on, so the toolchain is needed at runtime, not just to build orchestra. |
| `jq` | Used by the entrypoint to render `config.json`. |
| Claude Code CLI | The default agent backend (`role.backend.getD "claude"`). |

Only the Claude backend is installed. `opencode`, `pi` and `vibe` exist in `Orchestra/Agents/`
and in the Nix container but are not in this image; add them to the Dockerfile if you use them.

## Configuration

Three host directories are bind-mounted, all gitignored and created on first `up`:

| Host | Container | Holds |
| --- | --- | --- |
| `docker/config/` | `/config` | `orchestra/config.json`, `listeners/`, `roles/`, `prompts/`, `secrets.json` |
| `docker/data/` | `/data` | Task history, queue state, cloned repos, per-project roles |
| `docker/secrets/` | `/secrets` (read-only) | GitHub App private key |

Plain directories rather than named volumes, so you can read and edit everything without
`docker cp`. Paths follow XDG (`Orchestra/Dirs.lean`), which appends `orchestra` to each root —
hence `config/orchestra/config.json`.

The entrypoint writes `config/orchestra/config.json` from environment variables on first start,
covering taxis, GitHub auth and Claude auth. **Once that file exists it is used as-is and the
environment is ignored** — so edit it in place for anything the environment can't express
(listeners, roles, agent auth blocks, `additional_sandbox_paths`), and restart. To regenerate
from the environment instead, delete the file.

Docker creates the bind-mount directories as root, which would leave them unwritable by the
unprivileged user agents run as. The entrypoint therefore starts as root, takes ownership of
`/config` and `/data`, and immediately re-execs itself as `orchestra` via `setpriv` — the daemon
itself never runs as root. Ownership is only rewritten when it is actually wrong, so restarts
don't walk every cloned repository under `data/`.

### GitHub App private key

Put the downloaded `.pem` in `docker/secrets/` and point at it *inside* the container:

```sh
ORCHESTRA_GITHUB_APP_ID=123456
ORCHESTRA_GITHUB_PRIVATE_KEY_PATH=/secrets/github-app.pem
```

`ORCHESTRA_SECRETS_DIR` overrides the host side if you keep keys elsewhere.

Note the compose file mounts the secrets *directory*, not the key file. Bind-mounting a file that
doesn't exist on the host makes Docker silently create a **directory** in its place, which
surfaces much later as a confusing openssl error; mounting the containing directory avoids that
entirely. The entrypoint still checks the key path and fails fast if it is a directory.

Not needed when using `ORCHESTRA_GITHUB_PAT` — leave the key path empty and no key is read.

## Reaching taxis

taxis is expected to be running already; this compose file does not start one. `localhost` inside
the container is the container, so a taxis on the host needs one of:

```yaml
# 1. host gateway
services:
  orchestra:
    extra_hosts:
      - "host.docker.internal:host-gateway"
# then ORCHESTRA_TAXIS_URL=http://host.docker.internal:8080
```

```yaml
# 2. shared network with taxis's own compose project
services:
  orchestra:
    networks: [taxis_default]
networks:
  taxis_default:
    external: true
# then ORCHESTRA_TAXIS_URL=http://taxis:8080
```

The token must belong to a taxis **admin** actor: orchestra creates the `t-project` and `o-*`
labels on first use, and label creation is admin-only.

## Landlock

Every agent launch goes through `landrun`, which uses Landlock, and there is no bypass — if
landrun cannot build a sandbox, every dispatched task fails. The entrypoint probes this at
start-up and warns if it can't.

**No seccomp or AppArmor relaxation is needed.** This was verified against Docker 29 with the
default profiles: the `landlock_*` syscalls are allowlisted, and a sandbox built inside the
container works. If you find otherwise on an older Docker, check in this order before reaching
for `seccomp=unconfined` — it is a much bigger hammer than the problem needs, and it relaxes the
*outer* sandbox while landrun is the inner one:

```sh
# 1. Does the host kernel have Landlock at all?
grep landlock /sys/kernel/security/lsm

# 2. Can landrun build a sandbox in the container?
docker compose run --rm --entrypoint sh orchestra \
  -c 'landrun --rox /usr --rox /bin --rox /lib --rox /lib64 -- /bin/true && echo OK'
```

Note that `landrun --help` is *not* a valid check — it never reaches the Landlock syscalls, so it
succeeds even where sandboxing is broken. Likewise `--ro` grants read without execute, so probing
with `--ro` and then executing a binary fails for reasons that have nothing to do with Landlock.
Use `--rox` as above.

If step 2 fails while step 1 succeeds, the narrow fix is a seccomp profile that allowlists
`landlock_create_ruleset`, `landlock_add_rule` and `landlock_restrict_self` on top of Docker's
default.

## Running other subcommands

The image's `CMD` is `queue start`; override it for one-off commands against the same volumes:

```sh
docker compose run --rm orchestra project list
docker compose run --rm orchestra queue status
```
