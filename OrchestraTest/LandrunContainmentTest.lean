import OrchestraTest.TestM
import Orchestra

open Lean (Json)
open Orchestra
open Orchestra.Exec

/-!
# What is put around landrun

Landlock as landrun uses it confines the filesystem and TCP ports and nothing else: not `connect`
on a Unix socket, not abstract sockets, not signals, not a shared `/tmp`. The landrun backend
therefore runs it inside Landlock's scopes and a mount namespace built from the grants
(`Landrun.bwrapArgs`), with per-session copies of the home paths that something outside the
sandbox executes or trusts. Pinned here:

* the namespace's shape, without running anything: only granted paths are bound, `/run` is cut
  down to name resolution, `/tmp` is the session's, `~/.cache`, `~/.claude` and `~/.claude.json`
  are the session's own and `~/.elan` an overlay;
* without bwrap or the scopes the backend refuses unless the operator opted in, and then points
  the agent at its own cache and Claude directory by environment instead;
* and, with landrun and bwrap installed, for real: from inside a session with exactly the claude
  backend's grants, a stand-in socket the grants do not cover, a stand-in abstract socket and a
  process outside are unreachable, `/tmp` is empty of the host's files, and what the agent writes
  to `~/.claude` and `~/.elan` never reaches the host. The same for a repository script, which
  also runs the daemon's copy of itself.
-/

namespace OrchestraTest.LandrunContainment

private def home : String := "/home/u"

private def scratch : Landrun.Scratch := { root := System.FilePath.mk "/scratch/s1" }

private def argsFor (grants : Array PathGrant) (stdio : Stdio := .piped) : IO (Array String) :=
  Landrun.bwrapArgs { command := "true", workdir := System.FilePath.mk "/work", grants, stdio }
    scratch {} home

/-- The `(flag, source, destination)` binds in `args`. -/
private def bindsOf (args : Array String) : Array (String × String × String) := Id.run do
  let mut out := #[]
  let mut i := 0
  while i + 2 < args.size do
    let a := args[i]!
    if a == "--bind" || a == "--ro-bind" then
      out := out.push (a, args[i+1]!, args[i+2]!)
      i := i + 3
    else i := i + 1
  return out

@[test]
def onlyWhatIsGrantedIsMounted : Test := do
  let args ← argsFor #[{ path := "/usr", access := .rox }, { path := "/work", access := .rwx },
    { path := "/tmp", access := .rw }, { path := "/proc", access := .ro },
    { path := "/tmp/agent-mcp-1.json", access := .ro }]
  let binds := bindsOf args
  TestM.assert (binds.contains ("--ro-bind", "/usr", "/usr")) "a read grant is a read-only bind"
  TestM.assert (binds.contains ("--bind", "/work", "/work")) "a write grant is a writable bind"
  TestM.assert (binds.contains ("--bind", "/scratch/s1/tmp", "/tmp")) "/tmp is the session's own"
  TestM.assert (binds.contains ("--ro-bind", "/tmp/agent-mcp-1.json", "/tmp/agent-mcp-1.json"))
    "a granted file under the host's /tmp is carried into the private one"
  TestM.assert (!binds.any (·.2.2 == "/home")) "nothing ungranted is bound"
  for flag in ["--unshare-pid", "--die-with-parent", "--new-session"] do
    TestM.assert (args.contains flag) s!"{flag}"
  TestM.assert (match args.findIdx? (· == "--proc") with
    | some i => args[i+1]? == some "/proc" | none => false) "a fresh /proc"
  let tmpAt := args.findIdx? (· == "/scratch/s1/tmp")
  let mcpAt := args.findIdx? (· == "/tmp/agent-mcp-1.json")
  TestM.assert (match tmpAt, mcpAt with | some a, some b => a < b | _, _ => false)
    "the private /tmp is mounted before what goes into it"

@[test]
def anInteractiveRunKeepsItsTerminal : Test := do
  let args ← argsFor #[{ path := "/usr", access := .rox }] (stdio := .inherit)
  TestM.assert (!args.contains "--new-session") "the person's terminal stays theirs"

@[test]
def runIsCutDownToNameResolution : Test := do
  let binds := (bindsOf (← argsFor #[{ path := "/run", access := .ro }])).filter (·.2.2 != "/tmp")
  TestM.assert (!binds.any (·.2.2 == "/run")) "/run is not bound whole"
  for b in binds do
    TestM.assert ((← Landrun.runEntries).contains b.2.2) s!"{b.2.2} is on the allowlist"
  for hidden in ["/run/user", "/run/docker.sock", "/run/dbus", "/run/containerd"] do
    TestM.assert (!Landrun.runAllowlist.any (Landrun.pathWithin · hidden) &&
                  !Landrun.runAllowlist.any (Landrun.pathWithin hidden ·)) s!"{hidden} stays out"

@[test]
def theHomePathsOutsideTrustsAreTheSessionsOwn : Test := do
  let g (rel : String) (access : Access) : PathGrant := { path := s!"{home}/{rel}", access }
  let args ← argsFor #[g ".claude" .rw, g ".claude.json" .rw, g ".cache" .rwx, g ".gitconfig" .ro]
  let binds := bindsOf args
  TestM.assert (binds.contains ("--bind", "/scratch/s1/home/.claude", s!"{home}/.claude"))
    "~/.claude is the session's"
  TestM.assert (binds.contains ("--bind", "/scratch/s1/home/.claude.json", s!"{home}/.claude.json"))
    "and so is ~/.claude.json"
  TestM.assert (binds.contains ("--bind", "/scratch/s1/home/.cache", s!"{home}/.cache"))
    "and ~/.cache"
  TestM.assert (binds.any fun (_, _, dst) => dst == s!"{home}/.claude/projects")
    "conversations go to orchestra's own directory, for --resume"
  TestM.assert (binds.contains ("--ro-bind", s!"{home}/.gitconfig", s!"{home}/.gitconfig"))
    "anything else granted is the host's"
  TestM.assert (!binds.any fun (_, src, _) => src == s!"{home}/.claude" || src == s!"{home}/.cache")
    "the operator's own are never mounted"

@[test]
def claudesConfigStartsWithoutCommandsToRun : Test := do
  let j := Landrun.sanitizedClaudeJson
    "{\"oauthAccount\": {\"x\": 1}, \"mcpServers\": {\"evil\": {}}, \"projects\": {\"/a\": {}}}"
  TestM.assert (j.contains "oauthAccount") "the account is kept"
  TestM.assert (!j.contains "mcpServers" && !j.contains "projects")
    "MCP servers and per-project settings are dropped"
  TestM.assertEqual (Landrun.sanitizedClaudeJson "not json") "{}"

@[test]
def withoutTheNamespaceTheBackendRefusesUnlessToldOtherwise : Test := do
  match Landrun.chooseContainment none true {} with
  | .error e => TestM.assert (e.contains "unconfined_sockets_ok") "the refusal names the opt-in"
  | .ok _ => TestM.fail "no bwrap and no opt-in: refused"
  match Landrun.chooseContainment (some "/usr/bin/bwrap") false {} with
  | .error e => TestM.assert (e.contains "ABI 6") "no scopes: refused, saying why"
  | .ok _ => TestM.fail "no scopes and no opt-in: refused"
  TestM.assert (Landrun.chooseContainment none true { unconfinedSocketsOk := true }
    matches .ok (.landrunOnly true)) "with the opt-in, landrun alone"
  TestM.assert (match Landrun.chooseContainment (some "/b") true {} with
    | .ok (.namespaced b) => b == "/b" | _ => false) "with both, the namespace"

@[test]
def optionsAreReadStrictly : Test := do
  TestM.assert ((Landrun.Options.parse Json.null) matches .ok _) "no options: defaults"
  match Landrun.Options.parse (Json.mkObj [("unconfined_sockets_ok", true),
      ("shared_caches", Json.arr #["mathlib", "uv"]), ("script_ports", Json.arr #[443])]) with
  | .ok o =>
    TestM.assert o.unconfinedSocketsOk "the opt-in"
    TestM.assertEqual o.sharedCaches #["mathlib", "uv"]
    TestM.assertEqual o.scriptPorts #[443]
  | .error e => TestM.fail e
  TestM.assert ((Landrun.Options.parse (Json.mkObj [("unconfined_sockets_ok", "yes")]))
    matches .error _) "not a boolean: refused"
  TestM.assert ((Landrun.Options.parse (Json.mkObj [("shared_caches", Json.arr #["../gopass"])]))
    matches .error _) "a shared cache must be a name under ~/.cache"

@[test]
def withoutTheNamespaceTheCopiesArePointedAtByEnvironment : Test := do
  let r ← do
    let root := System.FilePath.mk "/tmp" / s!"orchestra-noNs-{← IO.monoNanosNow}"
    let s : Landrun.Scratch := { root }
    IO.FS.createDirAll (s.home / ".claude" / "projects")
    IO.FS.writeFile (s.home / ".claude.json") "{}"
    let g (rel : String) (access : Access) : PathGrant := { path := s!"{home}/{rel}", access }
    try
      let base : RunSpec := {
        command := "true", workdir := System.FilePath.mk "/work"
        grants := #[g ".claude" .rw, g ".claude.json" .rw, g ".cache" .rwx, g ".elan" .rwx] }
      let spec ← Orchestra.withTempDataNoDb (root / "data") do
        let shared ← Landrun.claudeProjectsDir
        IO.FS.createDirAll shared
        let spec ← Landrun.withoutNamespace base s home
        -- A second launch in the same session must leave the shared conversations alone.
        let keep := shared / "keep"
        IO.FS.writeFile keep "x"
        discard <| Landrun.withoutNamespace base s home
        unless ← keep.pathExists do
          throw (.userError "a second launch emptied the shared conversations")
        pure spec
      pure (spec, s)
    finally try IO.FS.removeDirAll root catch _ => pure ()
  let (spec, s) := r
  let paths := spec.grants.map (·.path)
  TestM.assert (!paths.contains s!"{home}/.claude" && !paths.contains s!"{home}/.cache")
    "the operator's are not granted"
  TestM.assert (spec.grants.any fun g => g.path == s!"{home}/.elan" && g.access == .rox)
    "the toolchains are read-only"
  TestM.assert (spec.env.contains ("CLAUDE_CONFIG_DIR", (s.home / ".claude").toString))
    "Claude is pointed at the session's directory"
  TestM.assert (spec.env.contains ("XDG_CACHE_HOME", (s.home / ".cache").toString))
    "and caches at the session's"

/-! ## For real -/

/-- What the stand-in agent does, as a Python script: try each way out and print what happened. -/
private def probeScript : String := "
import os, socket, sys
sock, abstract, pid, marker, probe = sys.argv[1:6]
def conn(p):
    try:
        s = socket.socket(socket.AF_UNIX); s.connect(p); return 'CONNECTED'
    except Exception as e: return 'denied'
print('socket', conn(sock))
print('abstract', conn('\\0' + abstract))
print('docker', conn('/run/docker.sock'))
print('userbus', conn('/run/user/%d/bus' % os.getuid()))
try:
    os.kill(int(pid), 0); print('signal ALLOWED')
except Exception: print('signal denied')
print('tmp', 'LEAKED' if os.path.exists('/tmp/' + marker) else 'private')
home = os.path.expanduser('~')
try:
    print('creds', open(os.path.join(os.path.expanduser('~'), '.claude', '.credentials.json')).read())
except Exception: print('creds unreadable')
try:
    cj = open(os.path.join(os.path.expanduser('~'), '.claude.json')).read()
    print('claudejson', 'HAS-MCP' if 'mcpServers' in cj else 'clean')
except Exception: print('claudejson unreadable')
try:
    print('sharedcache', open(os.path.join(os.path.expanduser('~'), '.cache', 'mathlib', 'warm')).read())
    open(os.path.join(os.path.expanduser('~'), '.cache', 'mathlib', probe), 'w').write('x')
except Exception as e: print('sharedcache', e)
for d in ['.claude', '.elan']:
    try:
        open(os.path.join(home, d, probe), 'w').write('x'); print(d, 'written')
    except Exception as e: print(d, 'refused')
"

/-- Start a stand-in Unix socket server at `path` (`abstract`: in the abstract namespace) that
    answers every connection. -/
private def standInServer (path : String) (abstract : Bool) :
    IO (IO.Process.Child { stdin := .null, stdout := .null, stderr := .null }) :=
  IO.Process.spawn { cmd := "python3", stdin := .null, stdout := .null, stderr := .null, args := #[
    "-I", "-c", s!"import socket\ns = socket.socket(socket.AF_UNIX)\n\
s.bind({if abstract then "'\\0'+" else ""}'{path}')\ns.listen(8)\n\
while True:\n    c, _ = s.accept(); c.sendall(b'hi'); c.close()\n"] }

@[test]
def nothingOutsideIsReachableFromASession : Test := do
  let some _ ← Landrun.which "landrun" | TestM.skip "landrun is not installed"; return
  let some bwrap ← Landrun.which "bwrap" | TestM.skip "bwrap is not installed"; return
  unless ← Landrun.bwrapWorks bwrap do TestM.skip "bwrap cannot build a namespace here"; return
  unless ← Scope.supported do TestM.skip "no Landlock scopes on this kernel"; return
  let stamp ← IO.monoNanosNow
  let root := System.FilePath.mk "/tmp" / s!"orchestra-contain-{stamp}"
  let hidden := root / "hidden"
  let work := root / "work"
  let probeName := s!"orchestra-probe-{stamp}"
  -- A stand-in home, never the operator's: its own `.claude` with a fake login, `.claude.json`,
  -- `.elan` and `.cache`.
  let fakeHome := root / "home"
  for d in [".claude", ".elan/bin", ".cache/mathlib", ".local/bin"] do
    IO.FS.createDirAll (fakeHome / d)
  IO.FS.writeFile (fakeHome / ".claude" / ".credentials.json") "{\"claudeAiOauth\": \"fake\"}"
  IO.FS.writeFile (fakeHome / ".claude.json") "{\"mcpServers\": {\"x\": {}}}"
  IO.FS.writeFile (fakeHome / ".cache" / "mathlib" / "warm") "WARM"
  let hostProbes := [fakeHome / ".claude" / probeName, fakeHome / ".elan" / probeName,
                     fakeHome / ".cache" / "mathlib" / probeName]
  IO.FS.createDirAll hidden
  IO.FS.createDirAll (work / ".orchestra")
  IO.FS.writeFile (root / probeName) "the host's"
  IO.FS.writeFile (work / "probe.py") probeScript
  let sockPath := (hidden / "s.sock").toString
  let abstract := s!"orchestra-test-{stamp}"
  let server ← standInServer sockPath false
  let abstractServer ← standInServer abstract true
  let outside ← IO.Process.spawn {
    cmd := "sleep", args := #["600"], stdin := .null, stdout := .null, stderr := .null }
  -- Orchestra's data directory is a stand-in too: the session keeps conversations there.
  let previousData ← Dirs.dataBaseOverride.get
  Dirs.setDataBaseOverride (some (root / "data"))
  let result ← try
      IO.sleep 300
      let tmpMarker := probeName
      -- The marker under the host's /tmp, which a private /tmp must not show.
      IO.FS.writeFile (System.FilePath.mk "/tmp" / tmpMarker) "host"
      let grants := Sandbox.grantsFor AgentDef.claude.sandboxPaths {} work false #[] #[]
      hostHomeOverride.set (some fakeHome.toString)
      let session ← (Landrun.backend {}).openSession { workdir := work, grants }
      let out ← try
          let h ← session.start {
            command := "python3"
            args := #["-I", (work / "probe.py").toString, sockPath, abstract,
                      toString outside.pid, tmpMarker, probeName]
            workdir := work, grants, envPassthrough := #["PATH"]
            env := #[("HOME", fakeHome.toString)] }
          let stdout ← match h.stdout with
            | some o => o.readToEnd
            | none => pure ""
          let _ ← h.wait
          -- And a repository script: the daemon's copy, bound over the checkout's own.
          let copy := root / "copy"
          IO.FS.createDirAll copy
          IO.FS.writeFile (copy / "validation.sh") s!"echo COPY; python3 -I {work / "probe.py"} \
{sockPath} {abstract} {outside.pid} {tmpMarker} {probeName}-s\n"
          IO.FS.writeFile (work / ".orchestra" / "validation.sh") "echo TREE\n"
          let r ← session.runScript {
            path := (work / ".orchestra" / "validation.sh").toString
            trusted := some copy, workdir := work, stdio := .piped }
          pure (stdout, r.output)
        finally session.close
      let leaked ← (hostProbes ++ hostProbes.map fun p => System.FilePath.mk s!"{p}-s").filterM
        (·.pathExists)
      pure (out.1, out.2, leaked)
    finally
      hostHomeOverride.set none
      Dirs.setDataBaseOverride previousData
      for c in [server, abstractServer, outside] do
        try c.kill catch _ => pure ()
        discard c.wait
      try IO.FS.removeFile (System.FilePath.mk "/tmp" / probeName) catch _ => pure ()
      try IO.FS.removeDirAll root catch _ => pure ()
  let (agent, script, leaked) := result
  for (label, out) in [("agent", agent), ("script", script)] do
    for line in ["socket denied", "abstract denied", "docker denied", "userbus denied",
                 "signal denied", "tmp private"] do
      TestM.assert (out.contains line) s!"{label}: {line} (got: {out})"
  TestM.assert (agent.contains "creds {\"claudeAiOauth\": \"fake\"}")
    "the session's ~/.claude holds a copy of the login"
  TestM.assert (agent.contains "claudejson clean") "and ~/.claude.json no MCP servers"
  TestM.assert (agent.contains "sharedcache WARM") "a shared cache is read through"
  TestM.assert (agent.contains ".claude written") "the agent can write its own ~/.claude"
  TestM.assert (agent.contains ".elan written") "and install into ~/.elan"
  TestM.assertEqual leaked [] (msg := "neither write reaches the (stand-in) host home")
  TestM.assert (script.startsWith "COPY") "the script run is the daemon's copy, not the tree's"

end OrchestraTest.LandrunContainment
