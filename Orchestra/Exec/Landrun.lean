import Orchestra.Exec.Backend
import Orchestra.Dirs

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

/-- `path` with its `.` and `..` components resolved and repeated slashes collapsed, without
    looking at the filesystem: `/a//b/./c/../d/` is `/a/b/d`. A `..` at the root stays at the root,
    which is what the kernel does with it too.

    Lexical only, so it is the right answer for a path that does not exist and possibly the wrong
    one for a path through a symlink (`/a/link/..` is wherever the link points, plus `..`). That is
    why `canonicalize` asks the filesystem first and falls back to this. -/
def normalizeLexical (path : String) : String := Id.run do
  let mut stack : Array String := #[]
  for c in path.splitOn "/" do
    if c.isEmpty || c == "." then continue
    if c == ".." then stack := stack.pop
    else stack := stack.push c
  return "/" ++ String.intercalate "/" stack.toList

/-- Whether `inner` is `outer` or lies below it, by whole path components, after lexical
    normalization of both (`normalizeLexical`): `/a/b/../c` lies within `/a`, `/ab` does not. -/
def pathWithin (inner outer : String) : Bool :=
  let i := normalizeLexical inner
  let o := normalizeLexical outer
  i == o || o == "/" || i.startsWith (o ++ "/")

/-- `path` as the kernel would resolve it: every symlink followed, every `.` and `..` resolved.

    The guard has to compare what landrun will *open*, not what the grant spells. landrun follows
    symlinks, so a grant of `~/data-link` pointing at the data directory, or of
    `~/.local/bin/../share/orchestra`, is a grant of the data directory whatever its spelling says,
    and a comparison of spellings would let both through.

    A path that does not exist (yet) cannot be asked about, so its longest existing prefix is
    resolved and the rest appended lexically. Such a grant is dropped by `usable` anyway — landrun
    can only attach a rule to something that exists — but the guarded directories themselves may
    not exist on a fresh install, and still have to compare equal to their own resolved form. -/
def canonicalize (path : String) : IO String := do
  try
    return (← IO.FS.realPath (System.FilePath.mk path)).toString
  catch _ =>
    let comps := (normalizeLexical path).splitOn "/" |>.filter (!·.isEmpty) |>.toArray
    -- Longest existing prefix first; `/` always exists, so the loop always returns.
    for k in (List.range (comps.size + 1)).reverse do
      let pre := "/" ++ String.intercalate "/" (comps.extract 0 k).toList
      let rest := comps.extract k comps.size
      if ← System.FilePath.pathExists (System.FilePath.mk pre) then
        let base ← try pure (← IO.FS.realPath (System.FilePath.mk pre)).toString
          catch _ => pure pre
        return normalizeLexical (String.intercalate "/" (base :: rest.toList))
    return normalizeLexical path

/-- Whether a grant whose canonical path is `canon` would reach one of `guarded` (also canonical).

    For the environment's own grants — the agent backend's lists, `additional_sandbox_paths` —
    reaching means being one of those directories, lying inside one, or containing one. For a
    grant orchestra makes itself (`Provenance.orchestra`) only the last two count: the task's
    checkout, its memories and its skills live *inside* these directories by design, but a
    `plugin_dirs` entry that is `~/.local`, or the data directory itself, is not a plugin and
    must not ride through on orchestra's provenance. -/
def reachesGuarded (g : PathGrant) (canon : String) (guarded : Array String) : Bool :=
  guarded.any fun d =>
    pathWithin d canon || (g.from_ != .orchestra && pathWithin canon d)

/-- `grants`, each paired with its canonical path (`canonicalize`), without any that would reach
    one of `guarded`, and the ones taken out; see `withoutGuarded`. -/
def withoutGuardedCanonical (grants : Array (PathGrant × String)) (guarded : Array String) :
    Array PathGrant × Array PathGrant :=
  let (kept, refused) := grants.partition fun (g, canon) => !reachesGuarded g canon guarded
  (kept.map (·.1), refused.map (·.1))

/-- `grants` without any that would reach into one of `guarded` — orchestra's own data and
    configuration directories — and the ones taken out, for the caller to say so.

    The data directory holds every slot and cache clone of every repository (another
    repository's code, private ones included), every task's log, the queue's control socket, the
    dashboard's password (`dashboard.secret`) and, in a local install, the database; the
    configuration directory holds `secrets.json`, the API clients, the GitHub App's settings and
    the daemon's control-socket token. An agent has no business reading any of it. A grant is
    taken out when it is one of those directories, lies inside one, or contains one — `~/.local`,
    `~/.config`, `/home` — since a Landlock rule on a directory covers everything below it, and
    Landlock has no way to carve an exception out of a rule once granted. Better an agent that is
    missing a path, which says so, than one that can read the operator's secrets.

    What orchestra grants of its own (`Provenance.orchestra`) may lie inside them: the task's own
    checkout, its plugins and its memories live there by design, and are exactly what the task is
    meant to reach. It may not *contain* one (`reachesGuarded`).

    Pure, and lexical: `..` is resolved, symlinks are not. `usable` compares the canonical paths
    instead (`canonicalize`), which is what landrun will actually open. -/
def withoutGuarded (grants : Array PathGrant) (guarded : Array String) :
    Array PathGrant × Array PathGrant :=
  withoutGuardedCanonical (grants.map fun g => (g, normalizeLexical g.path))
    (guarded.map normalizeLexical)

/-- The directories no agent grant may reach (`withoutGuarded`): orchestra's data and
    configuration directories as this process resolves them. -/
def guardedDirs : IO (Array String) := do
  return #[(← Dirs.dataBase).toString, (← Dirs.orchestraConfigDir).toString,
           (← Dirs.configBase).toString]

/-- `ports` without a listening grant on any of `reserved`, and the ports taken out.

    A port an agent may bind is a port it may answer on, and an agent answering on the port the
    dashboard listens on while the dashboard is down is one the CLI will hand the bearer password
    to (`Client.defaultBaseUrl`). Connecting is left alone: talking to the dashboard needs that
    password, and the agent does not have it. -/
def withoutReservedBinds (ports : Ports) (reserved : Array UInt16) : Ports × Array UInt16 :=
  let (kept, refused) := ports.bind.partition (!reserved.contains ·)
  ({ ports with bind := kept }, refused)

/-- `spec` with its grants resolved against this machine's `$HOME` and cut down to the paths that
    are actually here.

    Landlock can only attach a rule to a path that exists, so a missing one has to go. Required
    grants say so on the way out: the agent cannot create the directory either — `$HOME` itself is
    never granted — so it will start, fail to write its own configuration, and usually hang, with
    nothing in the log pointing at the cause. -/
def usable (spec : RunSpec) (warn : Bool := true) : IO RunSpec := do
  let resolved := spec.resolveGrants (← hostHome)
  let guarded ← (← guardedDirs).mapM canonicalize
  let canonical ← resolved.grants.mapM fun g => return (g, ← canonicalize g.path)
  let (allowed, refused) := withoutGuardedCanonical canonical guarded
  if warn then
    for g in refused do
      IO.eprintln s!"  [sandbox] warning: not granting {g.path}: it is or contains orchestra's \
own data or configuration directory, which holds other repositories' clones, task logs and \
credentials. Grant a narrower path instead."
  let (ports, refusedPorts) := withoutReservedBinds resolved.ports (← orchestraPorts)
  if warn then
    for p in refusedPorts do
      IO.eprintln s!"  [sandbox] warning: not letting the agent listen on port {p}: orchestra or \
its dashboard uses it, and whatever answers there is handed the dashboard password. Pick \
another port in extra_ports."
  let mut kept : Array PathGrant := #[]
  for g in allowed do
    if ← System.FilePath.pathExists (System.FilePath.mk g.path) then
      kept := kept.push g
    else if g.required && warn then
      IO.eprintln s!"  [sandbox] warning: {g.path} does not exist, so the agent gets no access \
to it and cannot create it — the agent may hang on startup. Create it and retry."
  return { resolved with grants := kept, ports }

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
  | .stream =>
    let child ← IO.Process.spawn {
      cmd := "landrun", args, cwd := spec.workdir
      stdin := .piped, stdout := .piped, stderr := .piped
    }
    Handle.ofStreamChild child

/-- The environment a task gets here: this machine.

    There is nothing to open and nothing to close — the checkout is already where the daemon put
    it, and every launch builds its own ruleset. The repository's scripts run outside the sandbox,
    as they always have: they are the operator's own code, not the agent's, and they set up and
    check a working tree the agent is not running in at the time. -/
def session : Session where
  id := "this machine"
  mcpEndpoint := pure
  describe := describe
  start := start
  runScript := hostRunScript
  close := pure ()

/-- landrun as an execution backend.

    Reads nothing from `execution.options`: what landrun grants comes from the agent backend and
    from `additional_sandbox_paths`, both of which predate this and stay where they are. -/
def backend : Backend where
  name := "landrun"
  preflight := preflight
  openSession spec := do
    spec.noteImageIgnored "landrun"
    pure session

def factory : BackendFactory where
  name := "landrun"
  summary := "Landlock confinement on this machine (the default)"
  make _ := .ok backend

end Orchestra.Exec.Landrun
