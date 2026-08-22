import Orchestra.Exec.Spec
import Lean.Data.Json

/-!
# The execution backend interface

An `Exec.Backend` is everything orchestra needs from "the thing that runs an agent": it turns a
`RunSpec` into something running, and hands back a handle over which the run can be read, waited
on and killed. Two ship today — `landrun`, which is what orchestra has always done, and `local`,
which does the same without confinement — and the interface is deliberately the smallest one that
also fits a backend running the agent on another machine.

`Handle` is the whole of what supervision needs, and nothing about it says "process". A backend
that launches a pod satisfies it with the pod's log stream, a wait that watches the pod's phase,
and a kill that deletes it.
-/

namespace Orchestra.Exec

/-- A run in progress, whatever is running it.

    The fields are functions rather than data because a remote backend answers them with API
    calls, not by reading a struct it already has. -/
structure Handle where
  /-- The run's stdout, when the spec asked for `.piped`; `none` under `.inherit`, where it went
      straight to orchestra's own terminal and there is nothing to read. -/
  stdout : Option IO.FS.Handle
  /-- The run's stderr, on the same terms as `stdout`. -/
  stderr : Option IO.FS.Handle
  /-- Block until the run finishes, and return its exit code. Called once. -/
  wait : IO UInt32
  /-- Stop the run now, as a cancellation. Best-effort and safe to call on a run that has already
      finished: cancellation races completion by nature, and every caller here would otherwise
      have to guess which one won. -/
  kill : IO Unit
  /-- What this run is called where the backend runs it — a pid, a container id, a pod name. For
      logs and for a human trying to find it by hand. -/
  id : String

/-- Where the MCP server has to listen for this backend's agents to reach it.

    This is the backend's answer to a question only it can answer, and the reason it is asked
    before the server starts: `Server.start` binds what this says, and a task whose agent runs off
    this machine gets a per-run token minted along with it. -/
inductive Exposure where
  /-- The agent runs here, so loopback is reachable and no one else is. No token. -/
  | loopback
  /-- The agent runs elsewhere: bind this address, and mint a token the agent must present. -/
  | network (bindHost : String)
deriving Repr, BEq, Inhabited

/-- One way of executing agents.

    Everything a backend needs to know about the task is in the `RunSpec` it is handed;
    everything orchestra needs to know about the backend is here. -/
structure Backend where
  /-- The name this backend is selected by in `execution.backend`. -/
  name : String
  /-- Where the MCP server must listen for this backend's agents. -/
  exposure : Exposure := .loopback
  /-- Rewrite the endpoint orchestra would hand the agent — this machine's loopback, plus the
      token if one was minted — into the one the agent can actually reach.

      The default is that endpoint unchanged, which is right for any backend that runs the agent
      here. A backend that runs it elsewhere replaces the host: the agent's MCP client is
      configured from what this returns, and getting it wrong is silent — the agent starts, finds
      no tools, and does the task without them. See `docs/execution.md`. -/
  mcpEndpoint : McpEndpoint → IO McpEndpoint := pure
  /-- Check that this backend can actually run something here, before a task depends on it.

      Called once per task, ahead of the clone and the token, so that a machine without `landrun`
      says so in one line instead of failing every attempt with `could not execute external
      process` and burning the retry budget on it. -/
  preflight : IO (Except String Unit) := pure (.ok ())
  /-- The run as a human-readable command, for `--debug`. What it prints should be close enough to
      what the backend does that a person can paste it and see the same thing happen. -/
  describe : RunSpec → IO String
  /-- Start the run. -/
  start : RunSpec → IO Handle

namespace Handle

/-- Kill a local process, and say nothing if it is already gone.

    Shelling out to `kill` rather than signalling directly: Lean's process API offers no kill, and
    this is the one place orchestra needs it. -/
def killPid (pid : UInt32) : IO Unit := do
  try
    let killer ← IO.Process.spawn {
      cmd := "kill"
      args := #["-9", toString pid]
      stdin := .null, stdout := .null, stderr := .null
    }
    let _ ← killer.wait
  catch _ => pure ()

/-- A handle over a local child process whose streams orchestra reads. -/
def ofPipedChild (child : IO.Process.Child { stdin := .null, stdout := .piped, stderr := .piped })
    : Handle :=
  { stdout := some child.stdout
    stderr := some child.stderr
    wait   := child.wait
    kill   := killPid child.pid
    id     := s!"pid {child.pid}" }

/-- A handle over a local child process that was given orchestra's own terminal. -/
def ofInheritChild
    (child : IO.Process.Child { stdin := .inherit, stdout := .inherit, stderr := .inherit })
    : Handle :=
  { stdout := none
    stderr := none
    wait   := child.wait
    kill   := killPid child.pid
    id     := s!"pid {child.pid}" }

end Handle

/-- How a backend is built from the `execution.options` object in `config.json`.

    A registry of these rather than of `Backend`s directly, because a backend that runs agents
    somewhere else needs to be told where: a namespace, an image, an address. Settings that belong
    to one backend are read by that backend, and a bad one is an `.error` naming the key rather
    than a launch that fails later for a reason nobody can trace to a config file. -/
structure BackendFactory where
  /-- The name this backend is selected by in `execution.backend`. -/
  name : String
  /-- One line for the error a person sees when they name a backend that does not exist. -/
  summary : String
  /-- Build the backend from `execution.options`. -/
  make : Lean.Json → Except String Backend

/-- `bytes` random bytes as lowercase hex, for names and secrets that must be unguessable.

    Falls back to the monotonic clock if `/dev/urandom` cannot be read, which keeps a name unique
    but not unpredictable — fine for a pod name, and the reason a caller minting a credential
    should care that this is best-effort. -/
def randomHex (bytes : Nat) : IO String := do
  let digits := "0123456789abcdef".toList
  try
    let h ← IO.FS.Handle.mk "/dev/urandom" .read
    let bs ← h.read bytes.toUSize
    let mut out := ""
    for b in bs.toList do
      out := out.push digits[b.toNat / 16]! |>.push digits[b.toNat % 16]!
    if out.isEmpty then throw (IO.userError "no entropy") else return out
  catch _ =>
    return toString (← IO.monoNanosNow)

/-- This machine's `$HOME`, or `""` when it is unset.

    `""` rather than an error: a home-scoped grant then resolves to a path that does not exist,
    which the host backends already drop with a warning, and that is a better failure than
    refusing to launch. -/
def hostHome : IO String :=
  return (← IO.getEnv "HOME").getD ""

end Orchestra.Exec
