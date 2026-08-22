import Orchestra.Exec.Backend

/-!
# The local backend: no confinement at all

Runs the agent as an ordinary child process of the daemon, with the daemon's own filesystem access
and network. The `RunSpec`'s grants and ports are *ignored* — not approximated — because there is
nothing here to enforce them with.

It exists for two reasons. It is the escape hatch for a machine where Landlock is unavailable (a
non-Linux kernel, a container without the syscalls) and the alternative is not running at all. And
it is the second implementation of `Backend`, which is what keeps the interface honest: anything
`landrun` needs that is not in `RunSpec` shows up here as a field nobody can supply.

It is never the default, and it says what it is on every launch. An agent under it can read and
write anything the daemon can — including orchestra's own configuration, which holds the GitHub
App key and every API token the sandbox exists to keep away from it.
-/

namespace Orchestra.Exec.Local

open Orchestra.Exec

/-- The command `--debug` prints. Grants and ports are left out because they do not apply; what
    runs is exactly this. -/
def describe (spec : RunSpec) : IO String := do
  let env := String.intercalate " " (spec.env.toList.map fun (k, v) => shellEscape s!"{k}={v}")
  let cmd := String.intercalate " "
    ((#[spec.command] ++ spec.args).toList.map shellEscape)
  return s!"[debug] cd {shellEscape spec.workdir.toString} && {env} {cmd}"

/-- Say once per launch what this backend does not do.

    In `preflight` rather than at startup so that it is repeated for every task: this is the
    warning that should be hard to stop noticing. -/
def preflight : IO (Except String Unit) := do
  IO.eprintln "  [sandbox] warning: the 'local' execution backend runs the agent unconfined — it \
can read and write everything this daemon can, orchestra's own configuration and credentials \
included. Use it only where Landlock is unavailable."
  return .ok ()

/-- Start `spec` as a plain child process.

    Passthrough variables need no work: a child inherits the environment, and `env` here only adds
    to it. -/
def start (spec : RunSpec) : IO Handle := do
  let env := spec.env.map fun (k, v) => (k, some v)
  match spec.stdio with
  | .inherit =>
    let child ← IO.Process.spawn {
      cmd := spec.command, args := spec.args, cwd := spec.workdir, env
      stdin := .inherit, stdout := .inherit, stderr := .inherit
    }
    return Handle.ofInheritChild child
  | .piped =>
    let child ← IO.Process.spawn {
      cmd := spec.command, args := spec.args, cwd := spec.workdir, env
      stdin := .null, stdout := .piped, stderr := .piped
    }
    return Handle.ofPipedChild child

/-- The environment a task gets here: this machine, with nothing between the agent and it. -/
def session : Session where
  id := "this machine (unconfined)"
  mcpEndpoint := pure
  describe := describe
  start := start
  runScript := hostRunScript
  close := pure ()

/-- Unconfined local execution as an execution backend.

    Reads nothing from `execution.options`: there is nothing to configure about doing nothing. -/
def backend : Backend where
  name := "local"
  preflight := preflight
  openSession _ := pure session

def factory : BackendFactory where
  name := "local"
  summary := "an ordinary child process, with no confinement at all"
  make _ := .ok backend

end Orchestra.Exec.Local
