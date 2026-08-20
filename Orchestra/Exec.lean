import Orchestra.Config
import Orchestra.Exec.Backend
import Orchestra.Exec.Landrun
import Orchestra.Exec.Local

/-!
# Choosing an execution backend

The registry of backends and the one function that turns a piece of configuration into a usable
one. Everything that launches an agent — the task runner, `orchestra interactive` — goes through
`resolve`, so a backend becomes available to all of them by being added to `ofName?`.
-/

namespace Orchestra.Exec

/-- Every backend that ships, in the order they are offered to a person who named one that does
    not exist. -/
def backends : Array Backend := #[Landrun.backend, Local.backend]

/-- The backend called `name`, if there is one. -/
def ofName? (name : String) : Option Backend :=
  backends.find? (·.name == name)

/-- The backend `cfg` selects, checked (`Backend.preflight`) before it is handed back.

    Both halves report as `.error` rather than throwing, because both are configuration mistakes
    with the same right answer at the call site: fail this task, say why in one line, and leave
    the daemon running for the tasks that follow. -/
def resolve (cfg : ExecutionConfig) : IO (Except String Backend) := do
  match ofName? cfg.backend with
  | none =>
    let known := String.intercalate ", " (backends.toList.map (·.name))
    return .error s!"unknown execution backend '{cfg.backend}' in config.json \
(execution.backend); known backends are: {known}"
  | some b =>
    match ← b.preflight with
    | .ok ()   => return .ok b
    | .error e => return .error s!"execution backend '{b.name}' is not usable here: {e}"

end Orchestra.Exec
