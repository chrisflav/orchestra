import Orchestra.Exec.Backend
import Orchestra.Exec.Scope
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
def usable (spec : RunSpec) (warn : Bool := true) (provided : Array String := #[]) :
    IO RunSpec := do
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
    -- A path the session provides itself (`HomeLayout`) is there whether or not the host has it.
    if provided.contains (normalizeLexical g.path) ||
        (← System.FilePath.pathExists (System.FilePath.mk g.path)) then
      kept := kept.push g
    else if g.required && warn then
      IO.eprintln s!"  [sandbox] warning: {g.path} does not exist, so the agent gets no access \
to it and cannot create it — the agent may hang on startup. Create it and retry."
  return { resolved with grants := kept, ports }

/-! ## What landrun does not confine, and what is put around it

Landlock, as landrun 0.1.15 uses it on a 6.12 kernel (ABI 6), restricts the filesystem and TCP
ports. It does **not** mediate:

* `connect(2)` on a *pathname* Unix socket. A socket file is reachable without any grant on the
  directory it is in: the systemd user bus (whose `systemd-run --user` runs anything, unconfined),
  `/run/docker.sock` (root, for a user in the `docker` group), incus's, the gpg and ssh agents,
  tmux's, orchestra's own control socket.
* abstract Unix sockets (`@/tmp/.X11-unix/X0`: keystrokes into the operator's session) and
  signals to processes outside the sandbox. Landlock can scope both since ABI 6; landrun does not
  ask it to.
* anything under `/tmp` the grants include, which is shared with every other process of the user.

All of it was verified on this backend's development host from inside a sandbox with exactly the
claude backend's grants. So a landrun run is wrapped, from the outside in:

1. **Landlock scopes** (`Exec.Scope`): this binary, re-executed as `__landlock-scope`, restricts
   itself to the abstract-socket and signal scopes and executes the rest. Every process below it
   inherits them.
2. **A mount namespace** (`bwrap`), built from the grants and nothing else (`bwrapArgs`). A path no
   grant names does not exist in it, so neither does any socket under it; `/run` is cut down to
   what name resolution needs; `/tmp` is the session's own; there is a fresh `/proc` in a PID
   namespace of its own, so no process outside is visible, and all of the agent's die with it.
   Per-session copies stand in for the home paths whose content something outside the sandbox
   later executes or trusts (`HomeLayout`): `~/.cache`, `~/.elan`, `~/.claude`, `~/.claude.json`.
3. **landrun**, inside it, with the same grants as always.

Without bwrap (or without the scopes, below Linux 6.12) only step 3 is left, and that confines
neither sockets nor signals. The backend then refuses to start a task unless the operator says, in
`execution.options.unconfined_sockets_ok`, that they know; it then says so on every session. The
recommendation for such a host is a dedicated user for the agents: one with no `docker` or `incus`
group, no login session (so no user bus, no agents), and nothing of the operator's in its home. -/

/-- How the landrun backend is configured, from `execution.options`. -/
structure Options where
  /-- Run without the mount namespace or the scopes when they are unavailable, accepting that the
      agent can then connect to any Unix socket and signal any process of this user. Off by
      default: such a run is not confined in the sense this backend promises. -/
  unconfinedSocketsOk : Bool := false
  /-- Subdirectories of the operator's `~/.cache` a session may read through, with its writes kept
      to itself (an overlay): a warm cache without sharing one. Only build caches belong here —
      `~/.cache` also holds password stores, backups, mail and browser profiles. -/
  sharedCaches : Array String := #["mathlib"]
  /-- Where sessions' private areas are made (`Scratch`); the system's temporary directory when
      unset. -/
  scratchDir : Option String := none
  /-- TCP ports a repository's script may connect to: the scripts run in the sandbox too
      (`runScriptIn`). -/
  scriptPorts : Array UInt16 := #[80, 443]
deriving Repr, Inhabited

/-- Read `Options` from `execution.options`. -/
def Options.parse (j : Lean.Json) : Except String Options := do
  let d : Options := {}
  if j matches .null then return d
  let some _ := j.getObj?.toOption
    | throw "execution.options must be an object for the landrun backend"
  let unconfinedSocketsOk ← match j.getObjVal? "unconfined_sockets_ok" with
    | .error _ => pure d.unconfinedSocketsOk
    | .ok v => match v.getBool? with
      | .ok b => pure b
      | .error _ => throw "execution.options.unconfined_sockets_ok must be true or false"
  let sharedCaches ← match j.getObjVal? "shared_caches" with
    | .error _ => pure d.sharedCaches
    | .ok v => match (Lean.FromJson.fromJson? v : Except String (Array String)) with
      | .ok cs =>
        for c in cs do
          if c.isEmpty || c.contains '/' || c == "." || c == ".." then
            throw s!"execution.options.shared_caches: '{c}' is not a directory name under ~/.cache"
        pure cs
      | .error _ => throw "execution.options.shared_caches must be a list of directory names"
  let scratchDir := (j.getObjValAs? String "scratch_dir").toOption
  let scriptPorts ← match j.getObjVal? "script_ports" with
    | .error _ => pure d.scriptPorts
    | .ok v => match (Lean.FromJson.fromJson? v : Except String (Array Nat)) with
      | .ok ps =>
        if ps.any (· ≥ 65536) then throw "execution.options.script_ports: not a port"
        pure (ps.map UInt16.ofNat)
      | .error _ => throw "execution.options.script_ports must be a list of port numbers"
  return { unconfinedSocketsOk, sharedCaches, scratchDir, scriptPorts }

/-- What confines a run on this machine (see the section above). -/
inductive Containment where
  /-- The mount namespace (`bwrap`, its absolute path) around landrun, all inside the scopes. -/
  | namespaced (bwrap : String)
  /-- landrun alone, possibly inside the scopes: the operator opted in
      (`Options.unconfinedSocketsOk`). -/
  | landrunOnly (hasScopes : Bool)
deriving Repr, BEq, Inhabited

/-- `cmd` resolved on `PATH`, or `none`. -/
def which (cmd : String) : IO (Option String) := do
  if cmd.contains '/' then
    return if ← System.FilePath.pathExists cmd then some cmd else none
  for dir in ((← IO.getEnv "PATH").getD "").splitOn ":" do
    if dir.isEmpty then continue
    let p := System.FilePath.mk dir / cmd
    if ← p.pathExists then return some p.toString
  return none

/-- Whether `bwrap` can build a namespace here: it can be missing, or present and refused by the
    kernel or a container's seccomp profile (unprivileged user namespaces). -/
def bwrapWorks (bwrap : String) : IO Bool := do
  try
    let out ← IO.Process.output {
      cmd := bwrap
      args := #["--ro-bind", "/", "/", "--unshare-pid", "--die-with-parent", "--", "/bin/true"] }
    return out.exitCode == 0
  catch _ => return false

/-- The containment a machine with `bwrap` (a working one, or none) and Landlock scopes (or not)
    allows under `opts`, or why it allows none. -/
def chooseContainment (bwrap : Option String) (hasScopes : Bool) (opts : Options) :
    Except String Containment :=
  match bwrap, hasScopes with
  | some b, true => .ok (.namespaced b)
  | _, _ =>
    let missing := (if bwrap.isNone then ["a working bwrap (bubblewrap)"] else []) ++
      (if hasScopes then [] else ["Landlock scopes (ABI 6, Linux 6.12)"])
    let what := String.intercalate " and " missing
    if opts.unconfinedSocketsOk then .ok (.landrunOnly hasScopes)
    else .error s!"landrun cannot confine agents on this machine: there is no {what}. Without \
them an agent can connect to any Unix socket (the user's systemd bus, docker, ssh and gpg agents, \
orchestra's own control socket) and signal any process of this user, and Landlock alone does not \
stop it. Install bubblewrap and allow unprivileged user namespaces, or run the agents as a \
dedicated user with no docker/incus group and no login session and set \
\"execution\": \{\"options\": \{\"unconfined_sockets_ok\": true}} to accept the rest. See \
\"what landrun does and does not confine\" in docs/execution.md."

/-- What this machine can confine a run with, or why it refuses to run one at all
    (`chooseContainment`, with this machine's `bwrap` and kernel). -/
def detectContainment (opts : Options) : IO (Except String Containment) := do
  let hasScopes ← Scope.supported
  let bwrap ← match ← which "bwrap" with
    | some b => if ← bwrapWorks b then pure (some b) else pure none
    | none => pure none
  return chooseContainment bwrap hasScopes opts

/-- The loud line a session without the namespace prints, every time. -/
def unconfinedWarning (hasScopes : Bool) : String :=
  s!"  [sandbox] WARNING: running agents under landrun alone \
(execution.options.unconfined_sockets_ok). An agent can connect to every Unix socket of this \
machine — the user's systemd bus, docker, ssh and gpg agents — and share /tmp with this user\
{if hasScopes then "" else ", and signal any of this user's processes and reach abstract sockets"}. \
Run agents as a dedicated user with no docker/incus group and no login session."

/-! ### The session's private area -/

/-- A session's private area: its `/tmp`, and its copies of the home paths in `HomeLayout`. Made
    when the session opens and removed when it closes. -/
structure Scratch where
  root : System.FilePath
deriving Repr, Inhabited

namespace Scratch
def tmp (s : Scratch) : System.FilePath := s.root / "tmp"
/-- Laid out like `$HOME`: `home/.cache`, `home/.claude`, … -/
def home (s : Scratch) : System.FilePath := s.root / "home"
/-- Upper and work directories of the overlays, one pair per overlaid path. -/
def overlay (s : Scratch) (name : String) : System.FilePath × System.FilePath :=
  (s.root / "overlay" / name / "upper", s.root / "overlay" / name / "work")
end Scratch

/-- Where an agent's conversations are kept across sessions: a directory of orchestra's own, never
    the operator's `~/.claude/projects`, so a continuation can `--resume` what an earlier task said
    without any agent writing to the operator's Claude Code state. -/
def claudeProjectsDir : IO System.FilePath :=
  return (← Dirs.dataBase) / "agent-state" / "claude" / "projects"

private def ownerOnly : IO.AccessRight := { read := true, write := true, execution := true }

/-- `~/.claude.json` as a session may start from: the operator's, without `mcpServers` (commands
    the agent would start) and `projects` (per-directory MCP servers and allowed tools). `{}` when
    there is none, or it is not a JSON object. -/
def sanitizedClaudeJson (content : String) : String :=
  match Lean.Json.parse content with
  | .ok (.obj kvs) =>
    (Lean.Json.obj (kvs.erase "mcpServers" |>.erase "projects")).compress
  | _ => "{}"

/-- Make a session's private area under `opts.scratchDir`: an owner-only directory with a random
    name, holding the session's `/tmp` and its copies of `~/.cache`, `~/.claude`, `~/.claude.json`
    and `~/.config/claude`, and the overlay directories for `~/.elan` and `opts.sharedCaches`.

    `~/.claude` starts with nothing but `.credentials.json`, copied, so an agent whose CLI logs in
    with it can; its conversations go to `claudeProjectsDir`. `~/.claude.json` starts as
    `sanitizedClaudeJson`. Nothing an agent writes to either reaches the operator's: hooks,
    `mcpServers`, skills or a `CLAUDE.md` planted there used to be run by the operator's own,
    unsandboxed Claude Code sessions. -/
def Scratch.create (opts : Options) : IO Scratch := do
  let base := System.FilePath.mk (opts.scratchDir.getD
    ((← IO.getEnv "TMPDIR").filter (!·.isEmpty) |>.getD "/tmp"))
  IO.FS.createDirAll base
  let root := base / s!"orchestra-sandbox-{← randomHex 8}"
  IO.FS.createDir root
  IO.setAccessRights root { user := ownerOnly }
  let s : Scratch := { root }
  let home ← hostHome
  IO.FS.createDirAll s.tmp
  IO.FS.createDirAll (s.home / ".cache")
  IO.FS.createDirAll (s.home / ".claude" / "projects")
  IO.FS.createDirAll (s.home / ".config" / "claude")
  IO.FS.createDirAll (s.home / ".elan")
  for c in opts.sharedCaches do
    IO.FS.createDirAll (s.home / ".cache" / c)
  for name in #["elan"] ++ opts.sharedCaches.map (s!"cache-{·}") do
    let (u, w) := s.overlay name
    IO.FS.createDirAll u
    IO.FS.createDirAll w
  let creds := System.FilePath.mk home / ".claude" / ".credentials.json"
  if ← creds.pathExists then
    let dst := s.home / ".claude" / ".credentials.json"
    IO.FS.writeFile dst ""
    IO.setAccessRights dst { user := { read := true, write := true } }
    IO.FS.writeFile dst (← IO.FS.readFile creds)
  let json := System.FilePath.mk home / ".claude.json"
  let content ← if ← json.pathExists then IO.FS.readFile json else pure "{}"
  IO.FS.writeFile (s.home / ".claude.json") (sanitizedClaudeJson content)
  IO.FS.createDirAll (← claudeProjectsDir)
  return s

/-- Remove the area, first carrying a refreshed `.credentials.json` back: Claude Code rotates its
    OAuth tokens, and a session that refreshed the operator's login with a copy would otherwise
    leave the operator holding a refresh token that no longer works. Only that file, only when it
    is still a JSON object with an `claudeAiOauth` member: it is data the CLI reads, not anything it
    runs. -/
def Scratch.remove (s : Scratch) : IO Unit := do
  try
    let copy := s.home / ".claude" / ".credentials.json"
    let orig := System.FilePath.mk (← hostHome) / ".claude" / ".credentials.json"
    if (← copy.pathExists) && (← orig.pathExists) then
      let fresh ← IO.FS.readFile copy
      if fresh != (← IO.FS.readFile orig) then
        if let .ok j := Lean.Json.parse fresh then
          if (j.getObjVal? "claudeAiOauth").isOk then
            let tmp := System.FilePath.mk s!"{orig}.orchestra-new"
            IO.FS.writeFile tmp ""
            IO.setAccessRights tmp { user := { read := true, write := true } }
            IO.FS.writeFile tmp fresh
            IO.FS.rename tmp orig
  catch e => IO.eprintln s!"  [sandbox] warning: could not carry Claude's refreshed login back: {e}"
  -- The overlays' work directories hold a directory overlayfs made unreadable to its owner.
  discard <| IO.Process.output { cmd := "chmod", args := #["-R", "u+rwx", s.root.toString] }
  try IO.FS.removeDirAll s.root catch _ => pure ()

/-! ### The mount namespace -/

/-- The entries of `/run` a run may see when a grant names `/run` as a whole: name resolution
    (`resolv.conf` points into `systemd/resolve` on most systemd hosts, `nscd`'s socket answers
    lookups), and NixOS's system profile and drivers. Everything else under `/run` is a daemon's
    socket or a user's session — `/run/user/<uid>` (the user bus, gpg, ssh, pipewire),
    `/run/docker.sock`, `/run/dbus`, `/run/containerd`, `/run/libvirt`, `/run/podman`, `/run/incus`
    — and stays out of the namespace. -/
def runAllowlist : Array String :=
  #["/run/systemd/resolve", "/run/resolvconf", "/run/NetworkManager/resolv.conf",
    "/run/NetworkManager/no-stub-resolv.conf", "/run/nscd", "/run/current-system",
    "/run/opengl-driver", "/run/booted-system"]

/-- The `/run` entries a run sees for a grant of `/run`: `runAllowlist`, plus wherever
    `/etc/resolv.conf` really is when that is under `/run` and the list does not name it — a host
    whose resolver configuration lives somewhere unexpected keeps name resolution. -/
def runEntries : IO (Array String) := do
  let mut out := #[]
  for r in runAllowlist do
    if ← System.FilePath.pathExists r then out := out.push r
  let resolv ← try (some <$> IO.FS.realPath "/etc/resolv.conf") catch _ => pure none
  if let some real := resolv then
    let r := real.toString
    if r.startsWith "/run/" && !out.any (pathWithin r ·) then out := out.push r
  return out

/-- How a session provides one of the home paths it keeps a copy of. -/
inductive HomeLayout where
  /-- A directory of the session's own, empty or prepared by `Scratch.create`. -/
  | own (rel : String)
  /-- The operator's directory, read through an overlay whose writes stay in the session. -/
  | overlay (rel : String) (name : String)
deriving Repr, BEq

/-- The home paths a session keeps a copy of, and how. These are the ones something outside the
    sandbox later executes or trusts: `~/.elan/bin` is on the operator's `PATH` and the toolchains
    under it run every build; `~/.cache` is where build tools look for things to run again (and
    also where the operator keeps password stores and backups); `~/.claude` and `~/.claude.json`
    are read by the operator's own Claude Code. -/
def homeLayout : Array HomeLayout :=
  #[.own ".cache", .overlay ".elan" "elan", .own ".claude", .own ".claude.json",
    .own ".config/claude"]

/-- `path` split into components. -/
private def components (path : String) : List String :=
  (path.splitOn "/").filter (!·.isEmpty)

/-- The `bwrap` arguments that build `spec`'s namespace: every grant bound at its own path (with
    the access landrun will enforce anyway), and nothing else. `spec`'s grants are resolved and
    usable already. `extraBinds` are read-only binds the launch itself needs inside (landrun's
    binary); `scriptDir`, when given, is bound read-only over `<workdir>/<name>`, for a repository
    script that must see the daemon's copy where the checkout has its own.

    Order matters to bwrap — a mount over a directory hides what an earlier one put below it — so
    binds go shallowest first. -/
def bwrapArgs (spec : RunSpec) (scratch : Scratch) (opts : Options) (home : String)
    (extraBinds : Array String := #[]) (scriptDir : Option (System.FilePath × String) := none) :
    IO (Array String) := do
  let mut binds : Array (String × Array String) := #[]
  let homeOf (rel : String) := (System.FilePath.mk home / rel).toString
  for g in spec.grants do
    let p := normalizeLexical g.path
    if p == "/tmp" || p == "/proc" || p == "/dev" || p.startsWith "/dev/" then continue
    if p == "/run" then
      -- The directory itself always, so landrun finds what it was told to grant.
      binds := binds.push ("/run", #["--dir", "/run"])
      for r in ← runEntries do binds := binds.push (r, #["--ro-bind", r, r])
      continue
    match homeLayout.find? fun l => match l with
        | .own rel | .overlay rel _ => p == homeOf rel with
    | some (.own rel) =>
      let src := (scratch.home / rel).toString
      binds := binds.push (p, #["--bind", src, p])
      if rel == ".cache" then
        for c in opts.sharedCaches do
          let lower := System.FilePath.mk home / ".cache" / c
          if ← lower.isDir then
            let (u, w) := scratch.overlay s!"cache-{c}"
            binds := binds.push (s!"{p}/{c}",
              #["--overlay-src", lower.toString, "--overlay", u.toString, w.toString, s!"{p}/{c}"])
      if rel == ".claude" then
        let shared := (← claudeProjectsDir).toString
        binds := binds.push (s!"{p}/projects", #["--bind", shared, s!"{p}/projects"])
    | some (.overlay rel name) =>
      if ← System.FilePath.isDir p then
        let (u, w) := scratch.overlay name
        binds := binds.push (p, #["--overlay-src", p, "--overlay", u.toString, w.toString, p])
      else
        -- Nothing on the host to read through: an empty directory of the session's own.
        binds := binds.push (p, #["--bind", (scratch.home / rel).toString, p])
    | none =>
      let flag := match g.access with
        | .ro | .rox => "--ro-bind"
        | .rw | .rwx => "--bind"
      binds := binds.push (p, #[flag, g.path, p])
  for b in extraBinds do
    binds := binds.push (b, #["--ro-bind", b, b])
  if let some (dir, name) := scriptDir then
    let dest := (spec.workdir / name).toString
    binds := binds.push (dest, #["--ro-bind", dir.toString, dest])
  -- Shallowest first, then in the order they were granted; the sort is stable.
  let sorted := binds.insertionSort fun a b => (components a.1).length < (components b.1).length
  let mut args : Array String := #["--unshare-pid", "--die-with-parent", "--proc", "/proc",
    "--dev", "/dev", "--bind", scratch.tmp.toString, "/tmp"]
  -- A run that owns no terminal gets its own session, so it cannot push input into one (TIOCSTI).
  -- An interactive run keeps the terminal it was handed: it is the person's.
  if spec.stdio != .inherit then args := args.push "--new-session"
  for (_, a) in sorted do args := args ++ a
  return args.push "--chdir" |>.push spec.workdir.toString

/-- `spec` as the namespace sees it: `/tmp` and the copied home paths are what they are inside,
    so nothing changes for landrun — except that a grant of a file under the host's `/tmp` (an
    agent's MCP configuration, vibe's home) is bound into the private one at the same path, which
    `bwrapArgs` does for every grant, and needs no change here. -/
def namespacedArgv (bwrap landrun : String) (bwrapArgv : Array String) (spec : RunSpec) :
    Array String :=
  #[bwrap] ++ bwrapArgv ++ #["--", landrun] ++ argv spec

/-- The whole command line of a run under `containment`, as `(command, args)`, from the outside in:
    the scopes, the namespace, landrun. -/
def commandFor (containment : Containment) (spec : RunSpec) (scratch : Scratch)
    (opts : Options) : IO (String × Array String) := do
  let landrun := (← which "landrun").getD "landrun"
  let self := (← IO.appPath).toString
  let scope (rest : Array String) : String × Array String :=
    (self, #[Scope.subcommand, "--"] ++ rest)
  match containment with
  | .namespaced bwrap =>
    let b ← bwrapArgs spec scratch opts (← hostHome) (extraBinds := #[landrun])
    return scope (namespacedArgv bwrap landrun b spec)
  | .landrunOnly hasScopes =>
    let rest := #[landrun] ++ argv spec
    return if hasScopes then scope rest else (landrun, argv spec)

/-- `spec` for a session without the namespace (`Containment.landrunOnly`): the copied home paths
    cannot be mounted in place, so they are pointed at by environment instead — `XDG_CACHE_HOME` at
    the session's cache, `CLAUDE_CONFIG_DIR` at its Claude directory (which holds its
    `.claude.json`), `TMPDIR` at its `/tmp` — and the operator's `~/.cache`, `~/.claude`,
    `~/.claude.json` and `~/.config/claude` are not granted at all. `~/.elan` is granted read and
    execute only: there is nowhere to put a session's own toolchains, so a toolchain has to be
    installed beforehand. -/
def withoutNamespace (spec : RunSpec) (scratch : Scratch) (home : String) : IO RunSpec := do
  let homeOf (rel : String) := (System.FilePath.mk home / rel).toString
  let replaced := [".cache", ".claude", ".claude.json", ".config/claude"].map homeOf
  let grants := (spec.grants.filter fun g => !replaced.contains (normalizeLexical g.path)).map
    fun g => if normalizeLexical g.path == homeOf ".elan" then { g with access := .rox } else g
  -- `CLAUDE_CONFIG_DIR` holds `.claude.json` itself when it is set.
  let claudeDir := scratch.home / ".claude"
  try IO.FS.rename (scratch.home / ".claude.json") (claudeDir / ".claude.json") catch _ => pure ()
  let own : Array PathGrant := #[
    { path := (scratch.home / ".cache").toString, access := .rwx, from_ := .orchestra },
    { path := claudeDir.toString, access := .rw, from_ := .orchestra },
    { path := (← claudeProjectsDir).toString, access := .rw, from_ := .orchestra },
    { path := scratch.tmp.toString, access := .rwx, from_ := .orchestra }]
  -- The conversations: a link from the session's directory to the shared one. Made once per
  -- session; a later launch finds the link already there, and must not follow it into the shared
  -- directory to "replace" it.
  let link := claudeDir / "projects"
  match ← link.symlinkMetadata.toBaseIO with
  | .ok md =>
    if md.type == .dir then
      IO.FS.removeDirAll link
      discard <| IO.Process.output { cmd := "ln", args := #["-s", (← claudeProjectsDir).toString,
        link.toString] }
  | .error _ =>
    discard <| IO.Process.output { cmd := "ln", args := #["-s", (← claudeProjectsDir).toString,
      link.toString] }
  return { spec with
    grants := grants ++ own
    env := spec.env ++ #[("XDG_CACHE_HOME", (scratch.home / ".cache").toString),
                         ("CLAUDE_CONFIG_DIR", claudeDir.toString),
                         ("TMPDIR", scratch.tmp.toString)] }

/-! ### Running -/

/-- Spawn `cmd args` in `workdir` with `stdio`, as a `Handle`. -/
def spawnWith (cmd : String) (args : Array String) (workdir : System.FilePath) (stdio : Stdio) :
    IO Handle := do
  match stdio with
  | .inherit =>
    let child ← IO.Process.spawn {
      cmd, args, cwd := workdir, stdin := .inherit, stdout := .inherit, stderr := .inherit }
    return Handle.ofInheritChild child
  | .piped =>
    let child ← IO.Process.spawn {
      cmd, args, cwd := workdir, stdin := .null, stdout := .piped, stderr := .piped }
    return Handle.ofPipedChild child
  | .stream =>
    let child ← IO.Process.spawn {
      cmd, args, cwd := workdir, stdin := .piped, stdout := .piped, stderr := .piped }
    Handle.ofStreamChild child

/-- `spec` resolved (`usable`) and fitted to `containment`: under the namespace as it is, without
    it with the copied home paths pointed at by environment (`withoutNamespace`). -/
def prepared (containment : Containment) (scratch : Scratch) (spec : RunSpec)
    (warn : Bool := true) : IO RunSpec := do
  match containment with
  | .namespaced _ =>
    let home ← hostHome
    let provided := homeLayout.map fun l => match l with
      | .own rel | .overlay rel _ => normalizeLexical (System.FilePath.mk home / rel).toString
    usable spec warn (provided := provided)
  | .landrunOnly _ => withoutNamespace (← usable spec warn) scratch (← hostHome)

/-- The command `--debug` prints: what a person would type to reproduce this run by hand. -/
def describeIn (containment : Containment) (scratch : Scratch) (opts : Options) (spec : RunSpec) :
    IO String := do
  -- Quietly: `start` is about to resolve the same grants and say what is missing, and printing
  -- each warning twice under `--debug` is how a person learns to skim them.
  let spec ← prepared containment scratch spec (warn := false)
  let (cmd, args) ← commandFor containment spec scratch opts
  let rendered := String.intercalate " " ((#[cmd] ++ args).toList.map shellEscape)
  return s!"[debug] cd {shellEscape spec.workdir.toString} && {rendered}"

/-- Start `spec` under `containment`. -/
def startIn (containment : Containment) (scratch : Scratch) (opts : Options) (spec : RunSpec) :
    IO Handle := do
  let spec ← prepared containment scratch spec
  let (cmd, args) ← commandFor containment spec scratch opts
  spawnWith cmd args spec.workdir spec.stdio

/-- What a repository's script is granted on top of the session's own grants: the system paths
    every agent backend declares, git's configuration to read, and the toolchain and cache paths a
    build uses (which the namespace replaces with the session's copies). A merger's or `prepare`'s
    session is opened with nothing but the checkout, and a script needs a shell to run in. -/
def scriptBaseGrants : Array PathGrant :=
  (#["/usr", "/lib", "/lib64", "/bin", "/sbin", "/nix"].map fun p =>
      ({ path := p, access := .rox } : PathGrant)) ++
  (#["/etc", "/run", "/dev", "/proc", "/sys"].map fun p => ({ path := p, access := .ro } : PathGrant)) ++
  #[{ path := "/dev/null", access := .rw }, { path := "/tmp", access := .rw },
    { path := ".local/bin", access := .rox, scope := .home },
    { path := ".gitconfig", access := .ro, scope := .home },
    { path := ".config/git", access := .ro, scope := .home },
    { path := ".elan", access := .rwx, scope := .home },
    { path := ".cache", access := .rwx, scope := .home }]

/-- `grants` with one grant per path: the first spelling, with the widest access asked for. -/
def dedupGrants (grants : Array PathGrant) : Array PathGrant := Id.run do
  let rank : Access → Nat
    | .ro => 0 | .rox => 1 | .rw => 2 | .rwx => 3
  let widest (a b : Access) : Access :=
    if rank a ≥ rank b then
      (if (a == .rw && b == .rox) || (a == .rox && b == .rw) then .rwx else a)
    else (if (b == .rw && a == .rox) then .rwx else b)
  let mut out : Array PathGrant := #[]
  for g in grants do
    match out.findIdx? (fun h => h.path == g.path && h.scope == g.scope) with
    | some i =>
      let h := out[i]!
      out := out.set! i { h with access := widest h.access g.access
                                , from_ := if g.from_ == .orchestra then .orchestra else h.from_ }
    | none => out := out.push g
  return out

/-- Run one of the repository's scripts in the sandbox, like the agent: same grants, same
    namespace, same scopes. The scripts are not the agent's, but the tree they work on is —
    `validation.sh` builds what the agent wrote, `lake build` runs the agent's lakefile, and an
    `init.sh` warming a cache runs whatever a previous agent left in `.lake`. Run on the host, each
    of those was the agent's code running unconfined.

    The script is the daemon's copy (`ScriptSpec.trusted`): bound read-only over the checkout's
    `.orchestra/` under the namespace, so `$0` and its siblings are where the script expects them;
    run by its path in the copy without it. Its environment is the one the agent gets minus the
    agent's credentials: `PATH`, `HOME`, `USER`, `SHELL`, `TERM` and the locale. -/
def runScriptIn (containment : Containment) (scratch : Scratch) (opts : Options)
    (sessionGrants : Array PathGrant) (script : ScriptSpec) : IO ScriptResult := do
  let fileName := (System.FilePath.mk script.path).fileName.getD script.path
  let scriptDirName := (System.FilePath.mk script.path).parent.bind (·.fileName)
  let trustedDir := script.trusted
  let grants := dedupGrants (scriptBaseGrants ++ sessionGrants ++
    (match containment, trustedDir with
     | .landrunOnly _, some d => #[{ path := d.toString, access := .rox, from_ := .orchestra }]
     | _, _ => #[]))
  let path := match containment, trustedDir with
    | .landrunOnly _, some d => (d / fileName).toString
    | _, _ => script.path
  let spec : RunSpec := {
    command := "bash", args := #[path], workdir := script.workdir, grants
    ports := { connect := opts.scriptPorts }
    envPassthrough := #["PATH", "HOME", "USER", "SHELL", "TERM", "LANG", "LC_ALL", "TZ"]
    stdio := if script.stdio == .inherit then .inherit else .piped
    label := "orchestra-script" }
  let spec ← prepared containment scratch spec
  let (cmd, args) ← match containment, trustedDir, scriptDirName with
    | .namespaced bwrap, some d, some name => do
      let landrun := (← which "landrun").getD "landrun"
      let b ← bwrapArgs spec scratch opts (← hostHome) (extraBinds := #[landrun])
        (scriptDir := some (d, name))
      pure ((← IO.appPath).toString,
        #[Scope.subcommand, "--"] ++ namespacedArgv bwrap landrun b spec)
    | _, _, _ => commandFor containment spec scratch opts
  match script.stdio with
  | .inherit =>
    let child ← IO.Process.spawn {
      cmd, args, cwd := script.workdir, stdin := .null, stdout := .inherit, stderr := .inherit }
    return { exitCode := ← child.wait }
  | .piped | .stream =>
    let child ← IO.Process.spawn {
      cmd, args, cwd := script.workdir, stdin := .null, stdout := .piped, stderr := .piped }
    let stderrTask ← IO.asTask (prio := .dedicated) child.stderr.readToEnd
    let stdout ← child.stdout.readToEnd
    let stderr ← IO.ofExcept (← IO.wait stderrTask)
    let exitCode ← child.wait
    return { exitCode, output := (stdout ++ stderr).trimAscii.toString }

/-- Check that `landrun` is on `PATH` and can be executed, and that this machine can confine a run
    the way this backend promises (`detectContainment`).

    Presence only, for landrun: that it can build a ruleset *here* depends on the kernel's Landlock
    support and is probed once at container start (`docker/entrypoint.sh`); what this catches is
    the other failure, where the binary is simply absent and every launch dies as `could not
    execute external process 'landrun'` — a message that names neither landrun's absence nor the
    task. -/
def preflight (opts : Options) : IO (Except String Unit) := do
  try
    let _ ← IO.Process.output { cmd := "landrun", args := #["--help"] }
  catch _ =>
    return .error "landrun is not on PATH. Install it (see the prerequisites in README.md), or \
select another execution backend with \"execution\": {\"backend\": \"local\"} in config.json."
  match ← detectContainment opts with
  | .ok _ => return .ok ()
  | .error e => return .error e

/-- The environment a task gets here: this machine, inside the containment `detectContainment`
    chose, with a private area of its own (`Scratch`) that `close` removes.

    `sessionGrants` are what the session was opened with (`SessionSpec.grants`): the agent's own,
    for a task. The repository's scripts run with them (`runScriptIn`). -/
def mkSession (containment : Containment) (scratch : Scratch) (opts : Options)
    (sessionGrants : Array PathGrant) : Session where
  id := match containment with
    | .namespaced _ => "this machine (landrun in a namespace)"
    | .landrunOnly _ => "this machine (landrun only)"
  mcpEndpoint := pure
  describe := describeIn containment scratch opts
  start := startIn containment scratch opts
  runScript := runScriptIn containment scratch opts sessionGrants
  close := scratch.remove

/-- Open a session: decide the containment (refusing, as `preflight` does, when there is none the
    operator accepted), make the private area, and say loudly when the run will not be namespaced. -/
def openSessionWith (opts : Options) (spec : SessionSpec) : IO Session := do
  let containment ← match ← detectContainment opts with
    | .ok c => pure c
    | .error e => throw (.userError e)
  if let .landrunOnly hasScopes := containment then IO.eprintln (unconfinedWarning hasScopes)
  let scratch ← Scratch.create opts
  return mkSession containment scratch opts spec.grants

/-- A session for a caller that was handed no session: what `Sandbox.launchAgent` falls back to.
    Each run gets a private area of its own, made when it starts and removed when it is waited
    for. -/
def session : Session where
  id := "this machine"
  mcpEndpoint := pure
  describe spec := do
    let c ← match ← detectContainment {} with
      | .ok c => pure c
      | .error e => throw (.userError e)
    let scratch ← Scratch.create {}
    try describeIn c scratch {} spec finally scratch.remove
  start spec := do
    let c ← match ← detectContainment {} with
      | .ok c => pure c
      | .error e => throw (.userError e)
    let scratch ← Scratch.create {}
    let h ← startIn c scratch {} spec
    return { h with wait := do
      let code ← h.wait
      scratch.remove
      return code }
  runScript script := do
    let c ← match ← detectContainment {} with
      | .ok c => pure c
      | .error e => throw (.userError e)
    let scratch ← Scratch.create {}
    try runScriptIn c scratch {} #[] script finally scratch.remove
  close := pure ()

/-- landrun as an execution backend.

    What landrun grants comes from the agent backend and from `additional_sandbox_paths`;
    `execution.options` says how the run is contained around it (`Options`). -/
def backend (opts : Options) : Backend where
  name := "landrun"
  preflight := preflight opts
  openSession := openSessionWith opts

def factory : BackendFactory where
  name := "landrun"
  summary := "Landlock confinement on this machine, in a mount namespace (the default)"
  make j := return backend (← Options.parse j)

end Orchestra.Exec.Landrun
