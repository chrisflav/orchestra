import OrchestraTest.TestM
import Orchestra

open Lean (Json)
open Orchestra
open Orchestra.Exec

/-!
# The execution abstraction

What a task is allowed to do is now said once, as a `RunSpec`, and rendered by whichever
execution backend runs it. Both halves are checked here without launching anything, which is the
whole reason the rendering was made pure: a wrong grant is a security bug, and finding it should
not require a Landlock kernel, a GitHub App and an agent CLI.

Three things are worth pinning down.

* The **spec** a task gets: the repository is writable exactly when the task is not `read_only`,
  and the paths an agent backend declares are held to a higher standard than the ones an operator
  adds in `additional_sandbox_paths` — see `Sandbox.grantsFor`.
* The **rendering** into landrun flags, access by access. `--ro` where `--rox` was meant is a run
  that cannot execute its toolchain; `--rwx` where `--rox` was meant is a review task that can
  rewrite the repository it was only supposed to read.
* That the MCP server's **address** reaches the agent's configuration intact. It is a host and a
  port precisely so a backend that runs the agent elsewhere can point it somewhere else, and a
  backend that quietly ignores the host fails in the worst way available: the agent starts, finds
  no tools, and does the task without them.
-/

namespace OrchestraTest.Exec

/-! ## Rendering a spec as landrun flags -/

private def sampleSpec : RunSpec := {
  command := "claude"
  args    := #["-p", "do it"]
  workdir := System.FilePath.mk "/work/repo"
  grants  := #[{ path := "/work/repo", access := .rwx, required := true },
               { path := "/usr", access := .rox }]
  ports   := { connect := #[8080, 443], bind := #[11434] }
  env     := #[("GH_TOKEN", "t")]
  envPassthrough := #["PATH"]
}

@[test]
def landrunRendersEachAccessAsItsOwnFlag : Test := do
  TestM.assertEqual (Landrun.flagOf .ro)  "--ro"  (msg := "read-only")
  TestM.assertEqual (Landrun.flagOf .rox) "--rox" (msg := "read + execute")
  TestM.assertEqual (Landrun.flagOf .rw)  "--rw"  (msg := "read + write")
  TestM.assertEqual (Landrun.flagOf .rwx) "--rwx" (msg := "read + write + execute")

@[test]
def landrunArgvIsGrantsThenPortsThenEnvThenCommand : Test := do
  TestM.assertEqual (Landrun.argv sampleSpec)
    #["--rwx", "/work/repo", "--rox", "/usr",
      "--connect-tcp", "8080", "--connect-tcp", "443", "--bind-tcp", "11434",
      "--env", "GH_TOKEN=t", "--env", "PATH",
      "--", "claude", "-p", "do it"]
    (msg := "landrun argv")

@[test]
def passthroughVariablesTravelByNameNotByValue : Test := do
  -- `--env PATH` tells landrun to carry the variable over; `--env PATH=...` would pin this
  -- machine's value onto a run that may resolve it somewhere else entirely.
  let argv := Landrun.argv sampleSpec
  TestM.assert (argv.contains "PATH") "PATH is passed by name"
  TestM.assert (!argv.any (·.startsWith "PATH=")) "PATH is not passed by value"

/-! ## Home-relative grants -/

@[test]
def homeGrantsResolveAgainstTheRunsHome : Test := do
  let g : PathGrant := { path := ".claude", access := .rw, scope := .home }
  let resolved := PathGrant.resolve "/home/agent" g
  TestM.assertEqual resolved.path "/home/agent/.claude" (msg := "resolved home path")
  TestM.assert (resolved.scope == .absolute) "a resolved grant is absolute"
  TestM.assertEqual resolved.access .rw (msg := "resolution does not change the access")

@[test]
def absoluteGrantsAreLeftAlone : Test := do
  let g : PathGrant := { path := "/nix", access := .rox }
  TestM.assertEqual (PathGrant.resolve "/home/agent" g).path "/nix"
    (msg := "an absolute grant is not rooted at home")

/-! ## The spec a task gets -/

private def backendPaths : SandboxPaths :=
  { rox := ["/usr"], ro := ["/etc"], rw := ["/dev/null"]
  , homeRox := [".local/bin"], homeRw := [".claude"], homeRwx := [".elan"]
  , extraPorts := [11434] }

private def operatorPaths : SandboxPaths :=
  { rw := ["/srv/shared"], homeRw := [".cache/uv"], extraPorts := [4000] }

private def summary (g : PathGrant) : String × Access × Scope × Bool :=
  (g.path, g.access, g.scope, g.required)

private def grants (readOnly : Bool) : Array (String × Access × Scope × Bool) :=
  (Sandbox.grantsFor backendPaths operatorPaths (System.FilePath.mk "/work/repo") readOnly
    #["/opt/plugins"] #["/var/memories"]).map summary

@[test]
def theRepositoryIsWritableUnlessTheTaskIsReadOnly : Test := do
  TestM.assertEqual (grants false)[0]! ("/work/repo", .rwx, .absolute, true)
    (msg := "a normal task may write its checkout")
  TestM.assertEqual (grants true)[0]! ("/work/repo", .rox, .absolute, true)
    (msg := "a read-only task may not")

@[test]
def everyRunGetsScratchSpace : Test := do
  TestM.assert ((grants false).contains ("/tmp", .rw, .absolute, true))
    "/tmp is granted read-write"

@[test]
def declaredPathsKeepTheirAccessAndScope : Test := do
  let g := grants false
  TestM.assert (g.contains ("/usr", .rox, .absolute, false)) "backend rox path"
  TestM.assert (g.contains ("/etc", .ro, .absolute, false)) "backend ro path"
  TestM.assert (g.contains (".local/bin", .rox, .home, false)) "backend home rox path"
  TestM.assert (g.contains ("/srv/shared", .rw, .absolute, false)) "operator rw path"
  TestM.assert (g.contains ("/opt/plugins", .rox, .absolute, false)) "plugins are read and run"
  TestM.assert (g.contains ("/var/memories", .rw, .absolute, false)) "memories are written back"

@[test]
def onlyTheBackendsOwnHomePathsAreRequired : Test := do
  -- A missing `~/.claude` means the machine cannot run this agent at all: it has no way to create
  -- the directory, since home itself is never granted, so it starts and hangs. A missing path
  -- from `additional_sandbox_paths` is an operator granting access to something that may or may
  -- not be there, and warning about it every launch would be noise.
  let g := grants false
  TestM.assert (g.contains (".claude", .rw, .home, true)) "the backend's home rw path is required"
  TestM.assert (g.contains (".elan", .rwx, .home, true)) "the backend's home rwx path is required"
  TestM.assert (g.contains (".cache/uv", .rw, .home, false)) "an operator's home path is advisory"

@[test]
def mcpAndHttpsAreOutboundOnly : Test := do
  let ports := Sandbox.portsFor backendPaths operatorPaths { host := "127.0.0.1", port := 8080 }
    #[9999]
  TestM.assertEqual ports.connect #[8080, 443, 11434, 4000, 9999] (msg := "outbound ports")
  -- Only the ports someone asked for are listenable: a local service the agent starts and then
  -- talks to. It has no business listening on orchestra's MCP port or on 443.
  TestM.assertEqual ports.bind #[11434, 4000, 9999] (msg := "listenable ports")

/-! ## Choosing a backend -/

@[test]
def theDefaultBackendIsLandrun : Test := do
  let cfg : ExecutionConfig := {}
  TestM.assertEqual cfg.backend "landrun" (msg := "default execution backend")
  match Orchestra.Exec.factoryOf? cfg.backend with
  | some f => TestM.assertEqual f.name "landrun" (msg := "and it resolves to itself")
  | none   => TestM.fail "the default backend is not registered"

@[test]
def executionConfigIsReadFromJson : Test := do
  match Json.parse "{\"backend\": \"local\", \"options\": {\"namespace\": \"agents\"}}" with
  | .error e => TestM.fail s!"could not parse: {e}"
  | .ok j =>
    match (Lean.FromJson.fromJson? j : Except String ExecutionConfig) with
    | .error e => TestM.fail s!"could not read execution config: {e}"
    | .ok cfg =>
      TestM.assertEqual cfg.backend "local" (msg := "backend name")
      -- Backend-specific settings are carried through uninterpreted, so a backend can grow one
      -- without the core configuration type learning about it.
      TestM.assertEqual (cfg.options.getObjValAs? String "namespace" |>.toOption) (some "agents")
        (msg := "options are passed through")

@[test]
def anUnknownBackendNamesTheOnesThatExist : Test := do
  match ← Orchestra.Exec.resolve { backend := "podman" } with
  | .ok _ => TestM.fail "an unknown backend was accepted"
  | .error e =>
    TestM.assert (AgentDef.containsCI e "podman") "the error names what was asked for"
    TestM.assert (AgentDef.containsCI e "landrun" && AgentDef.containsCI e "local"
                  && AgentDef.containsCI e "kubernetes")
      "and lists the backends that do exist, with a line on each"

/-! ## Running something

The one test here that actually launches a process. It goes through the `local` backend because
that is the one that needs no kernel feature, no image and no cluster — but what it exercises is
the plumbing every backend shares: a spec becomes a run, the run's output can be read, its
environment arrived, and waiting on it yields its exit code. -/

@[test]
def theLocalBackendRunsWhatTheSpecSays : Test := do
  let spec : RunSpec := {
    command := "/bin/sh"
    args    := #["-c", "echo \"$GREETING\""]
    workdir := System.FilePath.mk "/tmp"
    env     := #[("GREETING", "hello from the spec")]
  }
  let handle ← Orchestra.Exec.Local.session.start spec
  let line ← match handle.stdout with
    | some out => out.getLine
    | none     => pure ""
  let exitCode ← handle.wait
  TestM.assertEqual line.trimAscii.toString "hello from the spec"
    (msg := "the command ran, in its environment, and its output came back")
  TestM.assertEqual exitCode 0 (msg := "exit code")

/-! ## The MCP address reaches the agent -/

@[test]
def claudeConnectsToTheEndpointItIsGiven : Test := do
  -- Claude stands in for the four backends: `pi` and `opencode` write their MCP configuration to
  -- a fixed path under `$HOME` — the very thing that makes them `parallelSafe := false` — and
  -- `vibe` injects its entry into the user's own `~/.vibe/config.toml`, so exercising any of
  -- them here would write to the config of whoever is running the suite.
  let mcp : McpEndpoint := { host := "orchestra.internal", port := 9999 }
  let (configPath, _, files) ← AgentDef.claude.setupMcp mcp none none
  let contents ← IO.FS.readFile (System.FilePath.mk configPath)
  AgentDef.claude.cleanup configPath
  TestM.assert (AgentDef.containsCI contents "orchestra.internal") "the host reaches the config"
  TestM.assert (AgentDef.containsCI contents "9999") "so does the port"
  -- The right address in the file is no use if the file is not where the agent is. `setupMcp`
  -- writes to *this* machine's `/tmp`, so it has to declare what it wrote as orchestra's to
  -- carry — otherwise a backend that runs the agent elsewhere launches it with `--mcp-config`
  -- pointing at nothing, and it does the task with no tools and no complaint.
  TestM.assert (files.any fun g => g.path == configPath && g.from_ == .orchestra)
    "the config it wrote is declared as a path orchestra has to carry"

/-! ## What the agent's environment starts with -/

@[test]
def anAbsentInstallationTokenIsUnsetRatherThanEmpty : Test := do
  -- A task with no GitHub App installation behind it runs with `ghToken = ""`. `gh` reads an
  -- empty `GH_TOKEN` as a credential it must use and fails every call with an authentication
  -- error, where an unset one lets it fall back to whatever else is configured.
  let withToken := Sandbox.envFor "ghs_realtoken" #[] #[]
  TestM.assert (withToken.any fun (k, v) => k == "GH_TOKEN" && v == "ghs_realtoken")
    "a token that exists is exported"
  let without := Sandbox.envFor "" #[] #[]
  TestM.assert (!without.any fun (k, _) => k == "GH_TOKEN")
    "an empty token is not exported at all"
  TestM.assert (without.any fun (k, _) => k == "CLAUDE_CODE_DISABLE_AUTO_MEMORY")
    "the rest of the environment is unaffected"

/-! ## The MCP host is not a shell -/

@[test]
def mcpHostIsRefusedUnlessItIsAHost : Test := do
  -- `stdioCommand` interpolates the host into an `sh -c` pipeline, and `vibe` renders that
  -- pipeline into TOML. Anything that is not spelled like a hostname is refused where the key can
  -- be named, rather than reaching either.
  for good in ["127.0.0.1", "orchestra.internal", "orchestra-mcp.default.svc", "::1"] do
    TestM.assert (McpEndpoint.validHost? good |>.toOption |>.isSome) s!"'{good}' is a host"
  for bad in ["", "host; rm -rf /", "host with spaces", "ho\"st", "$(id)", "host`id`"] do
    TestM.assert (McpEndpoint.validHost? bad |>.toOption |>.isNone) s!"'{bad}' is not a host"

@[test]
def theTokenTransportEscapesWhatItInterpolates : Test := do
  -- Belt and braces: even reaching `stdioCommand` directly, the host lands as one word.
  let (cmd, args) := McpEndpoint.stdioCommand
    { host := "orchestra.internal", port := 4711, token := some "deadbeef" }
  TestM.assertEqual cmd "bash" (msg := "a shell, because `nc` cannot send the token first")
  let script := (args.getD 1 "")
  TestM.assert (AgentDef.containsCI script "deadbeef") "the token is in the script"
  TestM.assert (AgentDef.containsCI script "orchestra.internal") "and so is the host"
  TestM.assert (AgentDef.containsCI script "4711") "and the port"

/-! ## Nothing reaches orchestra's own directories

An agent under landrun must not be able to read orchestra's data directory (every clone of every
repository, every task's log, the queue's socket, `dashboard.secret`) or its configuration
directory (`secrets.json`, the API clients). Both live under the home by default, below
`~/.local/share` and `~/.config`, so the agent backends' home grants are held to paths that miss
them, and landrun refuses any grant that would reach them whatever asked for it. -/

private def home : String := "/home/u"
private def dataDir : String := "/home/u/.local/share/orchestra"
private def configDir : String := "/home/u/.config/orchestra"

/-- The grants a task on `agent` gets, resolved against `home`, with the task's own checkout and
    memories where they really are: inside the data directory. -/
private def resolvedFor (agent : AgentDef) : Array PathGrant :=
  (Sandbox.grantsFor agent.sandboxPaths {} (System.FilePath.mk s!"{dataDir}/repos/acme/widgets-0")
    false #[] #[s!"{dataDir}/memories/acme-widgets"]).map (·.resolve home)

private def reaches (g : PathGrant) (dir : String) : Bool :=
  Landrun.pathWithin g.path dir || Landrun.pathWithin dir g.path

@[test]
def noAgentBackendAsksForOrchestrasDirectories : Test := do
  for (name, agent) in [("claude", AgentDef.claude), ("vibe", AgentDef.vibe), ("pi", AgentDef.pi),
                         ("opencode", AgentDef.opencode)] do
    let (_, refused) := Landrun.withoutGuarded (resolvedFor agent) #[dataDir, configDir]
    TestM.assertEqual (refused.map (·.path)) #[]
      (msg := s!"{name}: nothing it declares reaches the data or configuration directory")
    let leaks := (resolvedFor agent).filter fun g =>
      g.from_ != .orchestra &&
        (reaches g dataDir || reaches g configDir || Landrun.pathWithin "/home" g.path)
    TestM.assertEqual (leaks.map (·.path)) #[]
      (msg := s!"{name}: no grant of its own covers them, nor all of /home")

@[test]
def theAgentStillGetsWhatItRuns : Test := do
  let paths := (resolvedFor AgentDef.claude).map (·.path)
  TestM.assert (paths.contains "/home/u/.local/bin") "the CLI's launcher"
  TestM.assert (paths.contains "/home/u/.local/share/claude") "and the versions it points at"
  TestM.assert (paths.contains "/home/u/.claude") "and its own state"
  TestM.assert (paths.contains s!"{dataDir}/repos/acme/widgets-0") "the task's own checkout"
  TestM.assert (paths.contains s!"{dataDir}/memories/acme-widgets") "and its memories"
  let (kept, _) := Landrun.withoutGuarded (resolvedFor AgentDef.claude) #[dataDir, configDir]
  TestM.assert ((kept.map (·.path)).contains s!"{dataDir}/repos/acme/widgets-0")
    "what orchestra grants of its own survives the guard"

@[test]
def noAgentWritesGitsConfigOrReadsGhs : Test := do
  for (name, agent) in [("claude", AgentDef.claude), ("vibe", AgentDef.vibe), ("pi", AgentDef.pi),
                         ("opencode", AgentDef.opencode)] do
    let gs := resolvedFor agent
    TestM.assert (!gs.any fun g => g.path.startsWith "/home/u/.config/gh")
      s!"{name}: the operator's gh login is not granted"
    let writable := gs.filter fun g =>
      (g.access == .rw || g.access == .rwx) &&
        (g.path == "/home/u/.gitconfig" || Landrun.pathWithin g.path "/home/u/.config/git")
    TestM.assertEqual (writable.map (·.path)) #[]
      (msg := s!"{name}: git's global configuration is read-only")

@[test]
def aGrantReachingOrchestrasDirectoriesIsRefused : Test := do
  let g (path : String) : PathGrant := { path, access := .ro }
  let (kept, refused) := Landrun.withoutGuarded
    #[g "/home/u/.local", g "/home", g "/", g "/home/u/.config", g s!"{dataDir}/logs",
      g configDir, g "/home/u/.local/bin", g "/home/u/.config/gh", g "/home/u/.local/share/claude",
      g "/home/u/.local/share/orchestra-other", g "/usr"]
    #[dataDir, configDir]
  TestM.assertEqual (refused.map (·.path))
    #["/home/u/.local", "/home", "/", "/home/u/.config", s!"{dataDir}/logs", configDir]
    (msg := "a directory containing them, one inside them, or one of them")
  TestM.assertEqual (kept.map (·.path))
    #["/home/u/.local/bin", "/home/u/.config/gh", "/home/u/.local/share/claude",
      "/home/u/.local/share/orchestra-other", "/usr"]
    (msg := "a sibling is not inside, whatever its name starts with")

@[test]
def usableDropsWhatReachesTheDataDirectory : Test := do
  let kept ← Orchestra.withTempData "sandbox-guard" do
    let data := (← Dirs.dataBase).toString
    let parent := (System.FilePath.mk data).parent.map (·.toString) |>.getD "/"
    IO.FS.createDirAll (System.FilePath.mk data / "logs")
    let spec : RunSpec :=
      { command := "true", workdir := System.FilePath.mk "/tmp"
      , grants := #[{ path := parent, access := .ro }, { path := s!"{data}/logs", access := .ro },
                    { path := "/usr", access := .rox }] }
    let spec ← Landrun.usable spec (warn := false)
    return (spec.grants.map (·.path), parent, data)
  let (paths, parent, data) := kept
  TestM.assert (!paths.contains parent) "the directory holding the data directory is not granted"
  TestM.assert (!paths.contains s!"{data}/logs") "nor anything inside it"
  TestM.assert (paths.contains "/usr") "the rest is"

/-! ## The guard compares what landrun opens, not what the grant spells

landrun follows symlinks and the kernel resolves `..`, so a grant is compared with the guarded
directories after both are resolved (`Landrun.canonicalize`). And orchestra's own provenance lets a
grant lie *inside* the data directory, never contain it. -/

@[test]
def dotDotDoesNotWalkPastTheGuard : Test := do
  TestM.assertEqual (Landrun.normalizeLexical "/a//b/./c/../d/") "/a/b/d"
  TestM.assertEqual (Landrun.normalizeLexical "/../..") "/" (msg := "`..` at the root stays there")
  let g : PathGrant := { path := "/home/u/.local/bin/../share/orchestra", access := .ro }
  let (_, refused) := Landrun.withoutGuarded #[g] #[dataDir, configDir]
  TestM.assertEqual (refused.map (·.path)) #[g.path]
    (msg := "a path that walks back up into the data directory is the data directory")
  let g2 : PathGrant := { path := "/home/u/.config/orchestra/../gh", access := .ro }
  let (kept, _) := Landrun.withoutGuarded #[g2] #[dataDir, configDir]
  TestM.assertEqual (kept.map (·.path)) #[g2.path] (msg := "one that walks out of it is not")

@[test]
def orchestrasProvenanceDoesNotCoverAContainingDirectory : Test := do
  let plugin (path : String) : PathGrant := { path, access := .rox, from_ := .orchestra }
  let (kept, refused) := Landrun.withoutGuarded
    #[plugin "/home/u/.local", plugin dataDir, plugin s!"{dataDir}/repos/acme/widgets-0",
      plugin s!"{configDir}/skills"]
    #[dataDir, configDir]
  TestM.assertEqual (refused.map (·.path)) #["/home/u/.local", dataDir]
    (msg := "a plugin directory that is or contains the data directory is refused")
  TestM.assertEqual (kept.map (·.path))
    #[s!"{dataDir}/repos/acme/widgets-0", s!"{configDir}/skills"]
    (msg := "the checkout and the skills inside them are orchestra's to grant")

/-- Run `act` with `Dirs.dataBase` pointed at a fresh directory under `/tmp`, removed afterwards.
    No database: these tests only need the directory. -/
private def withScratchData (label : String) (act : System.FilePath → IO α) : IO α := do
  let root := System.FilePath.mk "/tmp" / s!"orchestra-{label}-{← IO.monoNanosNow}"
  let data := root / "data"
  IO.FS.createDirAll data
  let previous ← Dirs.dataBaseOverride.get
  Dirs.setDataBaseOverride (some data)
  try act root
  finally
    Dirs.setDataBaseOverride previous
    try IO.FS.removeDirAll root catch _ => pure ()

@[test]
def aSymlinkToTheDataDirectoryIsTheDataDirectory : Test := do
  let (paths, link, sneaky) ← withScratchData "guard-symlink" fun root => do
    let link := root / "innocent"
    discard <| IO.Process.output { cmd := "ln", args := #["-s", (root / "data").toString,
      link.toString] }
    IO.FS.createDirAll (root / "data" / "logs")
    IO.FS.createDirAll (root / "elsewhere")
    let sneaky := (root / "elsewhere" / ".." / "data" / "logs").toString
    let spec : RunSpec :=
      { command := "true", workdir := System.FilePath.mk "/tmp"
      , grants := #[{ path := link.toString, access := .ro }, { path := sneaky, access := .ro },
                    { path := (root / "elsewhere").toString, access := .ro }] }
    let spec ← Landrun.usable spec (warn := false)
    return (spec.grants.map (·.path), link.toString, sneaky)
  TestM.assert (!paths.contains link) "a symlink to the data directory is refused"
  TestM.assert (!paths.contains sneaky) "so is a path that walks into it with `..`"
  TestM.assert (paths.any (·.endsWith "elsewhere")) "a sibling is kept"

@[test]
def canonicalizeResolvesWhatDoesNotExistAgainstWhatDoes : Test := do
  let out ← withScratchData "guard-canon" fun root => do
    let link := root / "link"
    discard <| IO.Process.output { cmd := "ln", args := #["-s", (root / "data").toString,
      link.toString] }
    let resolvedRoot ← IO.FS.realPath root
    return (← Landrun.canonicalize (link / "not-yet" / ".." / "later").toString,
            (resolvedRoot / "data" / "later").toString)
  TestM.assertEqual out.1 out.2
    (msg := "the existing prefix is resolved through the symlink and the rest appended")

/-! ## No agent listens where orchestra answers -/

@[test]
def noAgentBindsAPortOrchestraUses : Test := do
  let ports : Ports := { connect := #[4711, 443, 8080, 11434], bind := #[8080, 11434] }
  let (kept, refused) := Landrun.withoutReservedBinds ports #[8080]
  TestM.assertEqual kept.bind #[11434] (msg := "the dashboard's port is not bound")
  TestM.assertEqual refused #[8080] (msg := "and the caller is told which")
  TestM.assertEqual kept.connect ports.connect (msg := "connecting is left alone")
  let spec ← Landrun.usable
    { command := "true", workdir := System.FilePath.mk "/tmp", ports } (warn := false)
  TestM.assert (!spec.ports.bind.contains 8080) "usable takes the default dashboard port out"
  TestM.assert (spec.ports.bind.contains 11434) "and keeps the rest"

@[test]
def anExtraPortThatIsTheMcpServersIsNeverBound : Test := do
  let mcp : McpEndpoint := { host := "127.0.0.1", port := 40000 }
  let p := Sandbox.portsFor {} {} mcp #[40000, 11434]
  TestM.assert (p.connect.contains 40000) "the MCP server is connected to"
  TestM.assertEqual p.bind #[11434] (msg := "and never listened on")

@[test]
def theCliPortIsReadFromALoopbackUrl : Test := do
  TestM.assertEqual (loopbackPort? "http://127.0.0.1:9090/api") (some 9090)
  TestM.assertEqual (loopbackPort? "http://localhost") (some 80)
  TestM.assertEqual (loopbackPort? "https://orchestra.example:8443") none
    (msg := "a remote server is not this machine's port")
  TestM.assertEqual (loopbackPort? "http://10.0.0.1:8080") none

end OrchestraTest.Exec
