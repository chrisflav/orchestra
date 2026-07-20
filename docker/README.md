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

### Parallelism

The container runs `queue start`, which is serial by default. To run tasks concurrently, add a
`queue` block to `config/orchestra/config.json` after first start and restart the container:

```json
"queue": { "parallel": 4, "parallel_per_repo": 2 }
```

`parallel_per_repo` gives each concurrent task on one repository its own clone, so size
`docker/data/` accordingly — the git objects are hardlinked from a shared cache clone, but each
slot carries its own checkout and its own build output, and the build output usually dominates.
Warm them up front, matching `--slots` to `parallel_per_repo`, or the first task to reach each
new slot pays that repository's init hook:

```sh
docker compose run --rm orchestra prepare <upstream> <fork> --slots 2
```

Docker creates the bind-mount directories as root, which would leave them unwritable by the
unprivileged user agents run as. The entrypoint therefore starts as root, takes ownership of
`/config` and `/data`, and immediately re-execs itself as `orchestra` via `setpriv` — the daemon
itself never runs as root. Ownership is only rewritten when it is actually wrong, so restarts
don't walk every cloned repository under `data/`.

### GitHub credentials

A working setup wants **both** a GitHub App and a PAT. They are not alternatives:

| Credential | Covers | Without it |
| --- | --- | --- |
| GitHub App | Every task mints an installation token from it and hands that to `gh` for cloning and pushing. | No task runs at all — it fails before the agent starts. |
| PAT (repo scope) | Pull requests against the **upstream** repo, issue comments, PR reviews, the triage backend. | Those specific operations fail; tasks targeting the fork still work. |

The App is what authenticates the machinery; the PAT covers what an installation token cannot
reach. Put the downloaded `.pem` in `docker/secrets/` and point at it *inside* the container:

```sh
ORCHESTRA_GITHUB_APP_ID=123456
ORCHESTRA_GITHUB_PRIVATE_KEY_PATH=/secrets/github-app.pem
ORCHESTRA_GITHUB_PAT=ghp_...
```

`ORCHESTRA_SECRETS_DIR` overrides the host side if you keep keys elsewhere.

Note the compose file mounts the secrets *directory*, not the key file. Bind-mounting a file that
doesn't exist on the host makes Docker silently create a **directory** in its place, which
surfaces much later as a confusing openssl error; mounting the containing directory avoids that
entirely. The entrypoint still checks the key path and fails fast if it is a directory.

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

## Troubleshooting

### Tasks fail with "error connecting to api.github.com"

Symptom: a queue entry fails with `sh failed (exit 1): ... error connecting to api.github.com`,
yet `curl https://api.github.com` from inside the same container returns 200.

`gh` is a Go program, and Go's DNS resolver is stricter than glibc's. On a compose network
`/etc/resolv.conf` points at Docker's embedded resolver (`127.0.0.11`), which forwards to the
host's upstreams. If one of those answers badly, Go reports `server misbehaving` and gives up,
while `curl` and `getent` fall through to another and succeed — so the usual connectivity checks
all pass and only `gh` breaks. This does not happen on Docker's default bridge network, which
skips the embedded resolver, so `docker run` can also look fine while compose fails.

Confirm it:

```sh
docker compose run --rm -e GH_DEBUG=api --entrypoint sh orchestra \
  -c 'echo bogus | gh auth login --with-token'
```

A line like `lookup api.github.com on 127.0.0.11:53: server misbehaving` is this problem. (A
`HTTP 401: Bad credentials` reply instead means DNS is fine and the token is the issue.)

Fix by naming an upstream resolver explicitly in `.env`, then recreating:

```sh
ORCHESTRA_DNS=1.1.1.1        # or your own resolver, if you need internal names
```

```sh
docker compose up -d --force-recreate
```

Leaving `ORCHESTRA_DNS` empty (the default) changes nothing. Only one address is supported —
compose interpolates the variable as a single scalar, so a comma-separated list becomes one
invalid entry. One working resolver is enough; edit `docker-compose.yaml` directly if you want a
fallback list.

Container names still resolve either way: the embedded resolver answers those itself and only
forwards external queries.

## Running other subcommands

The image's `CMD` is `queue start`; override it for one-off commands against the same volumes:

```sh
docker compose run --rm orchestra project list
docker compose run --rm orchestra queue status
```
