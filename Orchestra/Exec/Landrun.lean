import Orchestra.Exec.Backend

/-!
# The landrun backend

Confines the agent with [landrun](https://github.com/Zouuup/landrun), which builds a Landlock
ruleset around a single process on this machine. This is what orchestra has always done, and it
stays the default: no container, no VM, no daemon in between — the kernel refuses what the ruleset
does not name.

The rendering is pure (`argv`), and everything that has to touch the filesystem — resolving
home-relative grants, dropping paths that do not exist here — happens once in `usable` before it.
That split is what makes the flags a run gets testable without a Landlock kernel to run them on.
-/

namespace Orchestra.Exec.Landrun

open Orchestra.Exec

/-- The landrun flag granting `access`. -/
def flagOf : Access → String
  | .ro  => "--ro"
  | .rox => "--rox"
  | .rw  => "--rw"
  | .rwx => "--rwx"

/-- The `landrun` argument vector for `spec`, whose grants are expected to be resolved already
    (see `usable`): grants first, then ports, then environment, then `--` and the command.

    Landlock rules are order-independent, so the grouping is for the human reading `--debug`
    output — a run's filesystem in one block instead of interleaved with its ports. -/
def argv (spec : RunSpec) : Array String := Id.run do
  let mut args : Array String := #[]
  for g in spec.grants do
    args := args.push (flagOf g.access) |>.push g.path
  for p in spec.ports.connect do
    args := args.push "--connect-tcp" |>.push (toString p)
  for p in spec.ports.bind do
    args := args.push "--bind-tcp" |>.push (toString p)
  for (k, v) in spec.env do
    args := args.push "--env" |>.push s!"{k}={v}"
  for name in spec.envPassthrough do
    args := args.push "--env" |>.push name
  args := args.push "--"
  args := args.push spec.command
  return args ++ spec.args

/-- `spec` with its grants resolved against this machine's `$HOME` and cut down to the paths that
    are actually here.

    Landlock can only attach a rule to a path that exists, so a missing one has to go. Required
    grants say so on the way out: the agent cannot create the directory either — `$HOME` itself is
    never granted — so it will start, fail to write its own configuration, and usually hang, with
    nothing in the log pointing at the cause. -/
def usable (spec : RunSpec) (warn : Bool := true) : IO RunSpec := do
  let resolved := spec.resolveGrants (← hostHome)
  let mut kept : Array PathGrant := #[]
  for g in resolved.grants do
    if ← System.FilePath.pathExists (System.FilePath.mk g.path) then
      kept := kept.push g
    else if g.required && warn then
      IO.eprintln s!"  [sandbox] warning: {g.path} does not exist, so the agent gets no access \
to it and cannot create it — the agent may hang on startup. Create it and retry."
  return { resolved with grants := kept }

/-- The command `--debug` prints: what a person would type to reproduce this run by hand. -/
def describe (spec : RunSpec) : IO String := do
  -- Quietly: `start` is about to resolve the same grants and say what is missing, and printing
  -- each warning twice under `--debug` is how a person learns to skim them.
  let spec ← usable spec (warn := false)
  let rendered := String.intercalate " " ((argv spec).toList.map shellEscape)
  return s!"[debug] cd {shellEscape spec.workdir.toString} && landrun {rendered}"

/-- Check that `landrun` is on `PATH` and can be executed.

    Presence only. That it can build a ruleset *here* depends on the kernel's Landlock support and
    is probed once at container start (`docker/entrypoint.sh`); what this catches is the other
    failure, where the binary is simply absent and every launch dies as `could not execute
    external process 'landrun'` — a message that names neither landrun's absence nor the task. -/
def preflight : IO (Except String Unit) := do
  try
    let _ ← IO.Process.output { cmd := "landrun", args := #["--help"] }
    return .ok ()
  catch _ =>
    return .error "landrun is not on PATH. Install it (see the prerequisites in README.md), or \
select another execution backend with \"execution\": {\"backend\": \"local\"} in config.json."

/-- Start `spec` under landrun. -/
def start (spec : RunSpec) : IO Handle := do
  let spec ← usable spec
  let args := argv spec
  match spec.stdio with
  | .inherit =>
    let child ← IO.Process.spawn {
      cmd := "landrun", args, cwd := spec.workdir
      stdin := .inherit, stdout := .inherit, stderr := .inherit
    }
    return Handle.ofInheritChild child
  | .piped =>
    let child ← IO.Process.spawn {
      cmd := "landrun", args, cwd := spec.workdir
      stdin := .null, stdout := .piped, stderr := .piped
    }
    return Handle.ofPipedChild child

/-- landrun as an execution backend. -/
def backend : Backend where
  name := "landrun"
  preflight := preflight
  describe := describe
  start := start

end Orchestra.Exec.Landrun
