import Orchestra.Config
import Orchestra.Exec.Backend
import Orchestra.Exec.Landrun
import Orchestra.Exec.Local
import Orchestra.Exec.Kubernetes

/-!
# Choosing an execution backend

The registry of backends and the one function that turns a piece of configuration into a usable
one. Everything that launches an agent — the task runner, `orchestra interactive` — goes through
`resolve`, so a backend becomes available to all of them by being added to `factories`.
-/

namespace Orchestra.Exec

/-- Every backend that ships, in the order they are offered to a person who named one that does
    not exist. -/
def factories : Array BackendFactory :=
  #[Landrun.factory, Local.factory, Kubernetes.factory]

/-- The factory called `name`, if there is one. -/
def factoryOf? (name : String) : Option BackendFactory :=
  factories.find? (·.name == name)

/-- The backend `cfg` selects, built from its options and checked (`Backend.preflight`) before it
    is handed back.

    All three failures — an unknown name, options that cannot be read, a backend that cannot run
    here — report as `.error` rather than throwing, because they are all configuration mistakes
    with the same right answer at the call site: fail this task, say why in one line, and leave
    the daemon running for the tasks that follow. -/
def resolve (cfg : ExecutionConfig) : IO (Except String Backend) := do
  match factoryOf? cfg.backend with
  | none =>
    let known := String.intercalate "\n" (factories.toList.map fun f => s!"  {f.name} — {f.summary}")
    return .error s!"unknown execution backend '{cfg.backend}' in config.json \
(execution.backend). Known backends:\n{known}"
  | some f =>
    match f.make cfg.options with
    | .error e => return .error s!"execution backend '{f.name}' is misconfigured: {e}"
    | .ok b =>
      match ← b.preflight with
      | .ok ()   => return .ok b
      | .error e => return .error s!"execution backend '{b.name}' is not usable here: {e}"

/-- Where the MCP server should listen for `backend`'s agents — address and, if it matters, which
    ports it may use — and the token it should demand.

    Decided together because they are the same decision. Loopback is reachable only by something
    already on this machine, which is the whole of the access control it needs, and the port can be
    whatever the kernel hands out because nothing else has to find it. Anywhere else, the socket is
    reachable by whatever else is on that network, and the server holds the PAT — so a run that is
    exposed at all is exposed with a secret the agent has to present, and on a port whoever has to
    route to it can be told about in advance.

    The token is minted here, once per task, and never written anywhere but the agent's own MCP
    configuration inside its sandbox. `randomSecret` and not `randomHex`: this is the only thing
    standing between a socket on a cluster network and the PAT behind it, so a machine that cannot
    produce entropy has to fail the task rather than get a guessable one. -/
def mcpBinding (backend : Backend) : IO (String × Option (UInt16 × UInt16) × Option String) := do
  match backend.exposure with
  | .loopback            => return ("127.0.0.1", none, none)
  | .network host ports  => return (host, ports, some (← randomSecret 24))

end Orchestra.Exec
