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
  , homeRox := [".local"], homeRw := [".claude"], homeRwx := [".elan"]
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
  TestM.assert (g.contains (".local", .rox, .home, false)) "backend home rox path"
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
  let (configPath, _) ← AgentDef.claude.setupMcp mcp none none
  let contents ← IO.FS.readFile (System.FilePath.mk configPath)
  AgentDef.claude.cleanup configPath
  TestM.assert (AgentDef.containsCI contents "orchestra.internal") "the host reaches the config"
  TestM.assert (AgentDef.containsCI contents "9999") "so does the port"

end OrchestraTest.Exec
