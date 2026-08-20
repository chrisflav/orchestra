# the execution model

Every agent orchestra runs goes through one function — `Sandbox.launchAgent` — and that function
used to do three separate jobs at once: work out what the agent needs, turn that into `landrun`
flags, and supervise the process it spawned. The first and third are the same whatever runs the
agent. The second is the only part that knows about Landlock, and it is the part that has to
change to run agents anywhere else.

This document describes the split those three jobs were pulled apart into, and what it takes to
add a backend — a container runtime, a Kubernetes cluster, a machine over SSH.

```
  TaskRunner ────► Sandbox.launchAgent
                        │
                        │ builds
                        ▼
                    RunSpec ─────────► Exec.Backend ────► Handle
             what the run needs      how it is run     what is running
                                            │
                                     landrun │ local │ (yours)
                        ◄───────────────────┘
                supervision: stream parsing, logs,
                cancellation, usage-limit detection
```

## the three pieces

### `RunSpec` — what the run needs

`Orchestra/Exec/Spec.lean`. A command, its arguments, a working directory, the paths it may touch,
the ports it may use, and the environment it starts with. It names no mechanism: landrun renders
it as Landlock flags, a Kubernetes backend would render the same fields as a pod spec — volume
mounts, a `NetworkPolicy`, `env` entries — and neither vocabulary leaks into the other.

Two fields are shaped by remote execution rather than by landrun, and are worth reading before
writing a backend:

- **`PathGrant.scope`** is `.absolute` or `.home`. A home-scoped grant means "wherever *this
  agent* keeps its state", which is a different directory when the agent runs somewhere else.
  Backends resolve it themselves — `Landrun.usable` against the daemon's `$HOME`, a pod backend
  against the image's.
- **`envPassthrough`** carries variable *names*, never values. `PATH` and `HOME` mean what they
  mean where the agent runs, and copying this machine's values into a container is how an agent
  ends up with a `PATH` full of binaries that are not there.

`PathGrant.required` marks a grant the run is expected to break without: the backend's own
`$HOME` paths, the checkout, `/tmp`. Backends that can only grant paths which exist say so on the
way out for those, and stay quiet about the rest — `/nix` on a machine without Nix is not news.

### `Exec.Backend` — how it is run

`Orchestra/Exec/Backend.lean`. Five fields:

| field | what it does |
| --- | --- |
| `name` | what `execution.backend` selects it by |
| `mcpEndpoint` | where the agent should reach the MCP server started on this machine's loopback |
| `preflight` | one check that this backend can run here, before a task depends on it |
| `describe` | the run as a command a person can read, for `--debug` |
| `start` | start the run, return a `Handle` |

### `Handle` — what is running

Stdout and stderr to read (when the spec asked for `.piped`), a `wait` for the exit code, and a
`kill` for cancellation. Nothing about it says "process": a pod backend satisfies it with a
followed log stream, a wait on the pod's phase, and a delete.

Everything above the handle — parsing the agent's event stream, writing the task log, honouring
the cancel token, deciding whether the run hit a usage limit — stays in `Sandbox` and is written
once, for all backends. That is most of the code, and none of it had to move.

## what ships

- **`landrun`** (default) — Landlock on this machine, which is what orchestra has always done.
  `Orchestra/Exec/Landrun.lean`.
- **`local`** — a plain child process, with no confinement at all. For machines where Landlock is
  unavailable. It says what it is on every launch, and it means it: an agent under it can read and
  write everything the daemon can, orchestra's own configuration and credentials included.

Selected in `config.json`:

```json
{
  "execution": {
    "backend": "landrun"
  }
}
```

`execution.options` is an arbitrary JSON object handed to the backend uninterpreted, so a backend
can grow settings of its own — a namespace, an image, a service account — without the core
configuration type learning about any of them.

## adding a backend

1. Write `Orchestra/Exec/<Name>.lean` with a `backend : Backend`.
2. Add it to `Exec.backends` in `Orchestra/Exec.lean`. That is the whole registration: every
   launch path resolves through `Exec.resolve`.
3. Give `preflight` a real check. It runs once per task, before the clone and the token, and it is
   what turns "every attempt fails with `could not execute external process`" into one line naming
   what is missing.
4. Make `describe` reproduce what you actually do. `--debug` is the first thing anyone reaches for
   when an agent cannot see a path.
5. Test the rendering, not the running. `Landrun.argv` is a pure function from a spec to an
   argument vector for exactly this reason — see `OrchestraTest/ExecTest.lean`. A wrong grant is a
   security bug, and finding it should not need a cluster.

## what a Kubernetes backend would look like

The mapping itself is mechanical:

| `RunSpec` | pod |
| --- | --- |
| `command` + `args` | container `command` / `args` |
| `workdir` | `workingDir`, on the volume the checkout lives in |
| `grants` | `volumeMounts` (`readOnly` for `.ro`/`.rox`), plus whatever the image already carries |
| `ports` | a `NetworkPolicy` egress rule per `connect` port |
| `env` | `env` entries, or a `Secret` for the ones that are credentials |
| `envPassthrough` | resolved from the *image*, not from the daemon |
| `stdio := .piped` | `kubectl logs -f` / the pod-log API |
| `Handle.wait` | watch the pod until `Succeeded`/`Failed`, read the container's exit code |
| `Handle.kill` | delete the pod |
| `mcpEndpoint` | the daemon's service address, not loopback |

Three things are *not* mechanical, and each is a decision rather than a translation. They are
listed here because the interface above deliberately does not pretend to have answered them.

**The workspace.** Today `TaskRunner` clones the repository on the daemon's own disk — a *slot*,
one per concurrent task on a repository (`Repo.ensureSlot`) — and grants the agent that path. A
pod cannot see it. Either the pod materialises its own checkout (an init container cloning with
the installation token, which is the natural fit and makes clone slots unnecessary for that
backend), or the checkout is placed on a volume the pod mounts. The first is cleaner and the
second is closer to what exists; either way the workspace step needs the same treatment the
sandbox just got — an interface with the host implementation behind it — before this backend can
run. It is the largest remaining piece of work.

**Reaching the MCP server.** `Server.start` binds `127.0.0.1` and speaks an unauthenticated
protocol, which is safe exactly because only a process on this machine can connect. It holds the
PAT that opens pull requests and posts comments — the token the sandbox exists to keep away from
the agent — so exposing it to a cluster network is not a matter of changing the bind address. It
needs a per-task credential the pod is given and the server checks. `Backend.mcpEndpoint` is the
seam where the address is decided (and `AgentDef.setupMcp` takes a host and port rather than a
port for the same reason), but the authentication is still to be built.

**Getting the agent's configuration into the pod.** `AgentDef.setupMcp` writes files on the
daemon's filesystem — `/tmp/agent-mcp-*.json` for Claude, `~/.pi/agent/mcp.json` for pi — and
returns a path that lands in the agent's command line. A pod sees none of them. The fix that keeps
the hook honest is for `setupMcp` to return file *contents* keyed by their destination path, and
for `RunSpec` to carry them, so that a host backend writes them to disk and a pod backend
projects them as a `ConfigMap` or `Secret`. That change touches all four agent backends and was
left out until something needs it.

Two smaller notes. `AgentDef.parallelSafe` is about a CLI's process-global state on one machine —
a fixed port, a config file under `$HOME` — and a backend that gives every run its own filesystem
makes it moot for that backend, which the queue does not know yet. And usage limits, retries,
validation hooks and the whole result-parsing path are backend-independent by construction: they
read what the agent said, not where it ran.
