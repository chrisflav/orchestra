# Running orchestra in Docker

Packages the queue daemon (`orchestra queue start`) with everything it shells out to: `landrun`,
`git`, `gh`, `openssl`, the Lean toolchain, and the Claude Code CLI. A second container off the
same image serves the web dashboard.

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

Two containers come up: `orchestra` (the queue daemon) and `dashboard` (the web view, on
<http://127.0.0.1:8080> — see [Dashboard](#dashboard) for the token it asks for).

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
| `/opt/orchestra/dashboard-site` | The dashboard's static pages, generated at build time by the binary itself and served by the `dashboard` container. |

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

## Dashboard

The `dashboard` service runs `orchestra dashboard serve` off the same image and the same
`/config` and `/data` mounts, published on `127.0.0.1:8080` by default:

```sh
docker compose logs dashboard    # prints the API token
open http://127.0.0.1:8080
```

The page asks for that token once and keeps it in `localStorage`; appending `?token=<token>` to
the URL supplies it without the prompt (the page then strips it from the address bar). Leaving
`ORCHESTRA_DASHBOARD_TOKEN` empty generates one on first start and persists it to
`data/orchestra/dashboard.token`, so it survives restarts — set the variable instead if you would
rather choose it.

What it shows: the queue and concert runs, listeners and their last check, task history with the
full structured log of each run, projects with their issue dependency graph, and **Auth** — every
configured authentication source with the usage limits last reported for it. That last page is
the one to open when the queue has pending work but nothing is running: it names the limit that
is binding and when it lifts. It reads the usage store the daemon refreshes rather than polling
Anthropic itself, and each row says how long ago it was polled.

A few deliberate choices:

- **A separate container, not a thread in the daemon.** The daemon drains for up to
  `stop_grace_period` on every stop, and a read-only web view should not be unavailable for half
  an hour exactly when someone is trying to see why a task is stuck. The dashboard dispatches
  nothing; it reads the state files the daemon writes.
- **One port for HTML and JSON.** The image bakes the generated static site in at
  `/opt/orchestra/dashboard-site` and `--site` serves it next to the API, so there is no second
  web server and no cross-origin hop. The pages themselves carry no data — each is an empty
  shell that fetches from `/api/…`, which is what the token gates.
- **Published on loopback.** The API is plain HTTP behind a bearer token. Put TLS in front of it
  before setting `ORCHESTRA_DASHBOARD_BIND` to anything wider:

  ```sh
  ORCHESTRA_DASHBOARD_BIND=0.0.0.0     # only behind a reverse proxy
  ORCHESTRA_DASHBOARD_PORT=8080
  ```

To run without it, `docker compose up orchestra` starts the daemon alone.

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

## Stopping

```sh
docker compose stop
```

This sends SIGTERM to the daemon, which stops claiming new work, lets the tasks already running
finish, and then exits — the same drain as `orchestra queue shutdown`. It returns as soon as the
last in-flight task lands, so the wait is only as long as the work outstanding. The dashboard
container holds nothing and stops immediately; `docker compose stop dashboard` takes it down on
its own if you want to keep the daemon running.

Send it again while the drain is in progress (`docker compose stop` a second time, or Ctrl-C twice
if you started it in the foreground) to cancel the running tasks instead, equivalent to
`orchestra queue shutdown --force`.

Docker's default 10s grace period is far too short for this — it would SIGKILL the daemon
mid-task and leave the entry stuck in `running`. `docker-compose.yaml` therefore sets
`stop_grace_period` to 30 minutes; override it with `ORCHESTRA_STOP_GRACE_PERIOD` if your tasks
routinely run longer.

`orchestra queue shutdown` itself is not the way to stop the container. It drains correctly, but
the daemon is PID 1, so its exit stops the container and `restart: unless-stopped` immediately
starts a new one. `docker compose stop` marks the container as deliberately stopped and does not.

## Running other subcommands

The image's `CMD` is `queue start`; override it for one-off commands against the same volumes:

```sh
docker compose run --rm orchestra project list
docker compose run --rm orchestra queue status
```
