# the execution model

Every agent orchestra runs goes through one function — `Sandbox.launchAgent` — and that function
used to do three separate jobs at once: work out what the agent needs, turn that into `landrun`
flags, and supervise the process it spawned. The first and third are the same whatever runs the
agent. The second is the only part that knows about Landlock, and it is the part that has to
change to run agents anywhere else.

This document describes the split those three jobs were pulled apart into, and what it takes to
add a backend — a container runtime, a Kubernetes cluster, a machine over SSH.

```
  TaskRunner ─── opens ──► Exec.Session ◄─── landrun │ local │ kubernetes
      │                    the environment for one task
      │                      │        │         │
      │  init.sh, before.sh ─┘        │         └─ validation.sh, after.sh
      │  (Session.runScript)          │            (Session.runScript)
      │                               │
      └─► Sandbox.launchAgent ── RunSpec ──► Session.start ──► Handle
                    │                                            │
                    └──── supervision: stream parsing, logs, ─────┘
                          cancellation, usage-limit detection
```

## the three pieces

### `RunSpec` — what the run needs

`Orchestra/Exec/Spec.lean`. A command, its arguments, a working directory, the paths it may touch,
the ports it may use, and the environment it starts with. It names no mechanism: landrun renders
it as Landlock flags, the Kubernetes backend renders the same fields as a pod spec — volume
mounts, a `Secret`, `env` entries — and neither vocabulary leaks into the other.

Three fields are shaped by running the agent somewhere else rather than by landrun, and are worth
reading before writing a backend:

- **`PathGrant.scope`** is `.absolute` or `.home`. A home-scoped grant means "wherever *this
  agent* keeps its state", which is a different directory when the agent runs somewhere else.
  Backends resolve it themselves — `Landrun.usable` against the daemon's `$HOME`, a pod backend
  against the image's.
- **`envPassthrough`** carries variable *names*, never values. `PATH` and `HOME` mean what they
  mean where the agent runs, and copying this machine's values into a container is how an agent
  ends up with a `PATH` full of binaries that are not there.
- **`PathGrant.from_`** says who supplies what is at a path: the environment (`/usr`, `/etc`, the
  agent's own `$HOME` — the image has them) or orchestra (the checkout, plugin directories, memory
  directories — they exist on the daemon's disk and nowhere else). A backend that runs the agent
  elsewhere has to carry the second kind there, and carry the writable ones back.

`PathGrant.required` marks a grant the run is expected to break without: the backend's own
`$HOME` paths, the checkout, `/tmp`. Backends that can only grant paths which exist say so on the
way out for those, and stay quiet about the rest — `/nix` on a machine without Nix is not news.

### `Exec.Session` — where the task happens

`Orchestra/Exec/Backend.lean`. A task is not one command. It is `init.sh`, then `before.sh`, then
the agent, then `validation.sh`, then the agent again if that failed, and `after.sh` at the end —
and all of it has to happen in one place, on one copy of the checkout. A session is opened once per
task, everything runs through it, and it is closed when the task ends, however it ended.

| field | what it does |
| --- | --- |
| `start` | run the agent, return a `Handle` |
| `runScript` | run one of the repository's own scripts, where the agent works |
| `close` | bring back what has to come back, release what was held |
| `describe` | the run as a command a person can read, for `--debug` |
| `mcpEndpoint` | where the agent should reach the MCP server |
| `freshEnvironment` | whether every task starts from one with nothing a previous task installed |
| `carriesAgentState` | whether a conversation an earlier task started can be resumed in this one |
| `id` | what to call this environment in a log line |

`carriesAgentState` is the other question a session answers about itself: whether a conversation
the agent started in an *earlier* task can be resumed in this one. That is a file under the agent's
own `$HOME`, so it is always true on a machine, where every task shares one home, and true of a
container backend only when the home outlives the pod. A task that continues another one is
refused by the task runner when its environment cannot have the conversation, rather than being run
with a follow-up prompt and no memory of what it follows.

`freshEnvironment` is what `init.sh` is measured against. The hook is meant to run once — install a
toolchain, warm a cache — and it records that it has by writing a marker into the checkout, which
is right when the environment is a machine that keeps what it was given and wrong when the
environment is new each time and the checkout is the one thing carried into it. Saying so makes
`RepoConfig.runInitIfNeeded` run the hook every task instead of trusting the marker.

A session is opened with a `SessionSpec`, which also carries the repository and the image it asked
for in its own `.orchestra/config.json`: what a task needs installed varies by repository, and a
backend that runs tasks in an image of some kind is the one that has to decide which.

For landrun and local there is nothing to open: the checkout is already where the daemon put it,
and `runScript` is `bash` on this machine, exactly as the task runner did it before sessions
existed. For a backend that runs the agent elsewhere the session is the whole problem — and the
reason `validation.sh` gets asked its question about the tree the agent actually worked on, rather
than about a copy on a machine with no toolchain.

### `Exec.Backend` — how it is run

`Orchestra/Exec/Backend.lean`. Five fields:

| field | what it does |
| --- | --- |
| `name` | what `execution.backend` selects it by |
| `exposure` | where the MCP server must listen for this backend's agents to reach it |
| `mcpEndpoint` | rewrites that endpoint into the one the agent can actually reach |
| `preflight` | one check that this backend can run here, before a task depends on it |
| `openSession` | open an environment for one task |

`exposure` is the one field a backend cannot get away with leaving at its default if it moves the
agent. `.loopback` means the server is reachable only from this machine, which is the whole of the
access control it needs. `.network` binds wider — and mints a one-run token in the same breath
(`Exec.mcpBinding`), because the server hands out the PAT's authority to whatever can talk to it.
Neither half can be set without the other.

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
- **`kubernetes`** — one pod per task, on a cluster reached through `kubectl`. The agent is off this
  machine entirely; the checkout travels to it and back, and the repository's scripts run there
  too. `Orchestra/Exec/Kubernetes.lean`, and [docs/kubernetes.md](kubernetes.md) for running it.

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

1. Write `Orchestra/Exec/<Name>.lean` with a `factory : BackendFactory` — a name, a one-line
   summary, and `make`, which builds the backend from `execution.options` or says which key is
   wrong. A backend that needs no settings ignores the argument. Its `openSession` returns the
   session everything else goes through; if the agent runs on this machine, `hostRunScript` is the
   `runScript` you want, since that is what the task runner always did.
2. Add it to `Exec.factories` in `Orchestra/Exec.lean`. That is the whole registration: every
   launch path resolves through `Exec.resolve`.
3. Give `preflight` a real check. It runs once per task, before the clone and the token, and it is
   what turns "every attempt fails with `could not execute external process`" into one line naming
   what is missing.
4. Make `describe` reproduce what you actually do. `--debug` is the first thing anyone reaches for
   when an agent cannot see a path.
5. Say where the MCP server has to listen (`exposure`) and where the agent should look for it
   (`mcpEndpoint`). Getting this wrong is the quiet failure: the agent starts, finds no tools, and
   does the task without them.
6. Test the rendering, not the running. `Landrun.argv` and `Kubernetes.podManifest` are pure
   functions from a spec to what gets sent, for exactly this reason — see
   `OrchestraTest/ExecTest.lean` and `OrchestraTest/KubernetesTest.lean`. A wrong grant is a
   security bug, and finding it should not need a Landlock kernel or a cluster. What genuinely
   cannot be tested that way — the shell that runs inside a pod — is written to take its paths as
   parameters so it can be run against a directory in `/tmp` instead.

## how the Kubernetes backend uses this

The mapping is mechanical, which is the point of the interface:

| orchestra | pod |
| --- | --- |
| `SessionSpec.workdir` | the container's `workingDir`, on the volume the checkout is staged into |
| `SessionSpec.grants` marked `.orchestra` | `emptyDir` volumes, filled from the daemon's disk when the session opens |
| `SessionSpec.grants` marked `.environment` | nothing: `/usr`, `/etc` and the agent's home come from the image |
| `SessionSpec.image` / `.repo` | which image the pod runs, and the label it carries |
| `Session.start` | `kubectl exec` — with `-i -t` when the spec asked for the daemon's own terminal |
| `Session.runScript` | `kubectl exec ... bash <script>`, in the same pod |
| `Session.close` | copy the checkout back, delete the pod |
| `RunSpec.env` | a file written into the pod and sourced, never a command line or a pod spec |
| `RunSpec.envPassthrough` | nothing: `PATH` and `HOME` are resolved in the image, which is why they travel by name |
| `Handle.kill` | delete the pod |
| `exposure` | `.network`, so the MCP server binds where a pod can reach it and mints a token |
| `mcpEndpoint` | rewrites the host to the daemon's cluster address, keeping port and token |

Three things were decisions rather than translations.

**Memory is a grant like any other.** Orchestra's memory directories are `.orchestra`-provenance
paths marked writable, so they are carried into the environment with the checkout and merged back
out of it — the merge being what lets several tasks hold a copy at once. Nothing in the memory
system knows about pods; it knows about directories, and the session puts them where the agent is.

**The workspace travels, and the session is what carries it.** The daemon keeps doing everything
except running things — it clones, mints the tokens, parses the agent's output — so the pod starts
from the checkout the daemon prepared and hands it back changed. The alternative, a pod that clones
for itself, leaves the daemon holding a tree nothing wrote to. What travels is exactly the grants
marked `.orchestra`: the checkout, plugin directories, memory directories. `Provenance` exists for
this and for nothing else — landrun ignores it entirely.

**Reaching the MCP server needed authentication, not just an address.** The server holds the PAT
that opens pull requests and posts comments, and it speaks an unauthenticated line protocol, which
is safe precisely as long as only this machine can connect. `Exec.Exposure` is how a backend says
that no longer holds: `.network` makes `Server.start` bind wider *and* makes `Exec.mcpBinding` mint
a one-task token, so neither can happen without the other. The agent sends it as its first line —
`nc` has no way to do that, so `McpEndpoint.stdioCommand` wraps it in a shell that sends the token
and gets out of the way. A connection that presents anything else is closed rather than asked
again.

**A `kubectl exec` connection can die without the process at the far end dying with it**, and the
daemon cannot tell that from the agent having exited. Left alone, the retry after a failed
validation would put a second agent in the same checkout as the first. So each agent records its
process id in the pod and kills whatever the last one left behind before starting.

What is still true of every backend: usage limits, retries, the validation loop's own logic and the
whole result-parsing path are backend-independent by construction. They read what the agent said,
not where it ran. The one place that is not yet true is `AgentDef.parallelSafe`, which is about a
CLI's process-global state on one machine — a fixed port, a config file under `$HOME` — and which a
backend that gives every task its own filesystem makes moot without the queue knowing it.
