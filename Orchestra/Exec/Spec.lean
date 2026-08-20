/-!
# What a run asks for, said without saying how it is confined

`RunSpec` is the whole of what orchestra needs from whatever executes an agent: a command, a
working directory, the filesystem it may touch, the ports it may open, and the environment it
starts with. It names no mechanism. `landrun` renders it as Landlock flags; a Kubernetes backend
would render the same fields as a pod spec — mounts, a `NetworkPolicy`, `env` entries — without
orchestra learning anything about pods.

The split matters because the three things `Sandbox.launchAgent` used to do at once come apart
along it: *what the agent needs* (here), *how that is enforced* (`Orchestra.Exec.Backend` and its
implementations), and *how the run is supervised* — streams parsed, cancellation honoured, logs
written — which is the same work whatever runs the process, and stays in `Sandbox`.

See `docs/execution.md` for how a new backend is added.
-/

namespace Orchestra.Exec

/-- What a run may do with one path.

    The four cases are what agents actually need, which is why they are not a set of independent
    read/write/execute bits: read-without-execute is the right grant for configuration, and
    write-without-execute for scratch space, but write-without-read is not a thing any agent
    wants and would only be a way to spell a mistake. -/
inductive Access where
  /-- Read, no execute. Configuration, certificates, `/etc`. -/
  | ro
  /-- Read and execute. Binaries, libraries, toolchains that are only run, never installed. -/
  | rox
  /-- Read and write, no execute. Scratch space, caches, agent state directories. -/
  | rw
  /-- Read, write and execute. Toolchain managers, which install binaries and then run them. -/
  | rwx
deriving Repr, BEq, Inhabited, DecidableEq

/-- Where a grant's path is rooted. -/
inductive Scope where
  /-- An absolute path, valid as written on whatever filesystem the run sees. -/
  | absolute
  /-- Relative to the *run's* home directory, not orchestra's.

      They are the same directory for a backend that runs the agent on this machine, and they are
      not for one that runs it elsewhere: `.claude` means "wherever this agent keeps its state",
      and only the backend knows where that is. Resolving it here would bake this machine's
      `$HOME` into a path that will be interpreted somewhere else entirely. -/
  | home
deriving Repr, BEq, Inhabited, DecidableEq

/-- One path the run may touch, and how. -/
structure PathGrant where
  /-- The path, interpreted according to `scope`. -/
  path : String
  /-- What the run may do with it. -/
  access : Access
  /-- How to read `path`. -/
  scope : Scope := .absolute
  /-- Whether the run is expected to break without this path.

      A backend that can only grant paths that exist has to drop the rest, and the two kinds of
      missing path want opposite treatment: `/nix` on a non-Nix machine is absent because that
      machine has no Nix, and saying so every launch is noise, while a missing `~/.claude` is a
      broken image — the agent cannot create it either, since home itself is never granted, so it
      typically reports that it cannot write its configuration and then hangs. Backends warn for
      the second kind and stay quiet about the first. -/
  required : Bool := false
deriving Repr, BEq, Inhabited

/-- The ports a run may use. Everything not named here is denied. -/
structure Ports where
  /-- Ports the run may connect out to. -/
  connect : Array UInt16 := #[]
  /-- Ports the run may listen on. -/
  bind : Array UInt16 := #[]
deriving Repr, BEq, Inhabited

/-- Where the agent should reach the MCP server this task started.

    A port alone was enough while every backend ran the agent on the same machine and loopback was
    the answer by construction. It stops being enough the moment the agent runs anywhere else, and
    the failure is silent in the worst way: the agent starts, finds no tools, and does the task
    with its bare hands. The host travels with the port so that a backend which moves the agent
    has somewhere to say so — see `Backend.mcpEndpoint`. -/
structure McpEndpoint where
  /-- Host the agent connects to. `127.0.0.1` for anything running on this machine. -/
  host : String
  /-- Port the MCP server was started on. -/
  port : UInt16
deriving Repr, BEq, Inhabited

/-- How the run's standard streams are wired. -/
inductive Stdio where
  /-- Captured, for orchestra to parse and log. What every queued task uses. -/
  | piped
  /-- Handed to the terminal orchestra was started from, for `orchestra interactive`. -/
  | inherit
deriving Repr, BEq, Inhabited, DecidableEq

/-- Everything one agent run needs, in terms no execution backend is privileged by.

    Built by `Sandbox.launchAgent` from the task's parameters and its agent backend's declared
    needs; consumed by an `Orchestra.Exec.Backend`. -/
structure RunSpec where
  /-- The executable to run, resolved on the run's `PATH`, not orchestra's. -/
  command : String
  /-- Its arguments, already built by the agent backend. -/
  args : Array String := #[]
  /-- The directory the command starts in: the repository checkout. -/
  workdir : System.FilePath
  /-- Every path the run may touch. Order is preserved so that a rendered command line is stable
      enough to diff between two runs. -/
  grants : Array PathGrant := #[]
  /-- The ports it may use. -/
  ports : Ports := {}
  /-- Environment variables set explicitly for the run. -/
  env : Array (String × String) := #[]
  /-- Names of environment variables to carry over from orchestra's own environment. Kept apart
      from `env` because the *value* is not orchestra's to state: `PATH` and `HOME` mean what they
      mean wherever the agent ends up, and a backend that runs it elsewhere resolves them there
      rather than copying this machine's. -/
  envPassthrough : Array String := #[]
  /-- How the standard streams are wired. -/
  stdio : Stdio := .piped
  /-- What to call this run in a log line. Not passed to the command; used by backends that have
      to name the thing they create (a container, a pod) and by `--debug` output. -/
  label : String := "orchestra-agent"
deriving Inhabited

namespace PathGrant

/-- `g` with a `.home` path resolved against `home`. Absolute grants are returned unchanged.

    Kept pure and separate from any filesystem check so that the two questions a backend asks —
    *where is this* and *is it there* — stay answerable one at a time. -/
def resolve (home : String) (g : PathGrant) : PathGrant :=
  match g.scope with
  | .absolute => g
  | .home     => { g with path := (System.FilePath.mk home / System.FilePath.mk g.path).toString
                        , scope := .absolute }

end PathGrant

namespace RunSpec

/-- Every grant, with `.home` paths resolved against `home`. -/
def resolveGrants (home : String) (spec : RunSpec) : RunSpec :=
  { spec with grants := spec.grants.map (PathGrant.resolve home) }

end RunSpec

end Orchestra.Exec
