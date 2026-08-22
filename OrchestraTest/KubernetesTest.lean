import OrchestraTest.TestM
import Orchestra

open Lean (Json)
open Orchestra
open Orchestra.Exec

/-!
# The Kubernetes execution backend

A pod spec is this backend's ruleset, exactly as an argument vector is landrun's, and it is
checked here the same way: by rendering it, without a cluster to send it to. What the tests are
holding onto is that the pod says what the run asked for — the agent's own command, the checkout
mounted where the daemon has it, the environment nowhere but in a `Secret` — and that the things
the daemon has to carry to the cluster are exactly the things no image could have.

The parts that cannot be checked without a cluster are the pod's *lifecycle*: staging the checkout
in over `kubectl exec`, the two streams, the exit code, copying the workspace back. Those are IO
against a real API server; what stands in for a test of them is that each has one job and says so.
-/

namespace OrchestraTest.Kubernetes

open Orchestra.Exec.Kubernetes

private def options (extra : List (String × Json) := []) : Json :=
  Json.mkObj ([("image", .str "ghcr.io/example/agent:1"),
               ("mcp_host", .str "orchestra.orchestra.svc.cluster.local")] ++ extra)

private def config (extra : List (String × Json) := []) : Config :=
  match Config.fromJson (options extra) with
  | .ok c    => c
  | .error _ => { image := "unparsed", mcpHost := "unparsed" }

private def sampleSpec : RunSpec := {
  command := "claude"
  args    := #["-p", "fix the bug"]
  workdir := System.FilePath.mk "/var/lib/orchestra/work/acme-widgets-slot0"
  grants  := #[
    { path := "/var/lib/orchestra/work/acme-widgets-slot0", access := .rwx, required := true
    , from_ := .orchestra },
    { path := "/tmp", access := .rw, required := true },
    { path := "/usr", access := .rox },
    { path := ".claude", access := .rw, scope := .home, required := true },
    { path := "/opt/orchestra/plugins", access := .rox, from_ := .orchestra },
    { path := "/var/lib/orchestra/memories/acme", access := .rw, from_ := .orchestra }]
  ports   := { connect := #[8080, 443] }
  env     := #[("GH_TOKEN", "ghs_averysecrettoken"), ("CLAUDE_CODE_DISABLE_AUTO_MEMORY", "1")]
  envPassthrough := #["PATH", "HOME"]
}

private def staged : Array StagedPath := stagedPaths (config) "/home/daemon" sampleSpec

private def manifest : Json :=
  podManifest (config) sampleSpec "orchestra-abc123" "orchestra-abc123-env" staged

private def container (name : String) : Option Json :=
  match manifest.getObjVal? "spec" |>.toOption |>.bind (·.getObjVal? "containers" |>.toOption) with
  | some (.arr cs) => cs.find? fun c => (c.getObjValAs? String "name" |>.toOption) == some name
  | _              => none

private def strings (j : Option Json) : List String :=
  match j with
  | some (.arr a) => a.toList.filterMap fun v => match v with | .str s => some s | _ => none
  | _             => []

/-! ## Configuration -/

@[test]
def theTwoSettingsThatCannotBeGuessedAreRequired : Test := do
  -- An image that does not exist would be a pod that never starts, which is at least loud. An
  -- unset `mcp_host` is the quiet one: the pod starts, the agent finds no tools, and it does the
  -- task by hand — so both are refused before a task can depend on them.
  match Config.fromJson (Json.mkObj [("mcp_host", .str "h")]) with
  | .ok _    => TestM.fail "a config with no image was accepted"
  | .error e => TestM.assert (AgentDef.containsCI e "image") "the error names the missing image"
  match Config.fromJson (Json.mkObj [("image", .str "i")]) with
  | .ok _    => TestM.fail "a config with no mcp_host was accepted"
  | .error e =>
    TestM.assert (AgentDef.containsCI e "mcp_host") "the error names the missing mcp_host"
    TestM.assert (AgentDef.containsCI e "no tools") "and says what it costs to leave it out"

@[test]
def defaultsAreTheOnesAClusterUsuallyWants : Test := do
  let c := config
  TestM.assertEqual c.ns "default" (msg := "namespace")
  TestM.assertEqual c.kubectl "kubectl" (msg := "kubectl binary")
  TestM.assertEqual c.mcpBind "0.0.0.0" (msg := "the daemon binds where a pod can reach it")
  TestM.assert c.syncBack "the checkout comes back by default"
  TestM.assertEqual c.homePath "/home/agent" (msg := "home")

@[test]
def clusterSpecificSettingsAreReadThrough : Test := do
  let c := config [("namespace", .str "orchestra"),
                   ("service_account", .str "orchestra-runner"),
                   ("sync_back", .bool false),
                   ("excludes", .arr #[.str ".lake"]),
                   ("image_pull_secrets", .arr #[.str "ghcr"])]
  TestM.assertEqual c.ns "orchestra" (msg := "namespace")
  TestM.assertEqual c.serviceAccount (some "orchestra-runner") (msg := "service account")
  TestM.assert (!c.syncBack) "sync_back can be turned off"
  TestM.assertEqual (excludeArgs c) #["--exclude", ".lake"] (msg := "excludes become tar flags")
  TestM.assertEqual c.imagePullSecrets #["ghcr"] (msg := "pull secrets")

/-! ## What the daemon has to carry -/

@[test]
def onlyOrchestrasOwnPathsAreStaged : Test := do
  -- `/usr` and `~/.claude` come from the image; the checkout, the plugins and the memories exist
  -- on the daemon's disk and nowhere else.
  TestM.assertEqual (staged.map (·.hostPath))
    #["/var/lib/orchestra/work/acme-widgets-slot0", "/opt/orchestra/plugins",
      "/var/lib/orchestra/memories/acme"]
    (msg := "staged paths")

@[test]
def stagedPathsKeepTheirAbsolutePosition : Test := do
  -- Mounted where the daemon has them, so every log line, prompt and error message that names the
  -- checkout is as true inside the pod as outside it.
  TestM.assert (staged.all fun st => st.hostPath == st.podPath)
    "an absolute path means the same thing on both sides"

@[test]
def whatComesBackIsWhatCouldBeWritten : Test := do
  let byPath (p : String) : Option StagedPath := staged.find? (·.hostPath == p)
  match byPath "/var/lib/orchestra/work/acme-widgets-slot0" with
  | none    => TestM.fail "the checkout is not staged"
  | some st =>
    TestM.assert st.writable "the checkout is writable, so it has to come back"
    TestM.assert st.isWorkspace "and it is the one path replaced wholesale rather than merged"
  match byPath "/opt/orchestra/plugins" with
  | none    => TestM.fail "plugins are not staged"
  | some st => TestM.assert (!st.writable) "a plugin directory is read-only, so nothing comes back"
  match byPath "/var/lib/orchestra/memories/acme" with
  | none    => TestM.fail "memories are not staged"
  | some st =>
    TestM.assert st.writable "memories are written by the agent"
    TestM.assert (!st.isWorkspace) "but merged back, since other tasks share them"

/-! ## The pod -/

@[test]
def thePodRunsTheAgentTheSpecNamed : Test := do
  match container "agent" with
  | none   => TestM.fail "there is no agent container"
  | some c =>
    let cmd := strings (c.getObjVal? "command" |>.toOption)
    TestM.assertEqual (cmd.take 2) ["/bin/sh", "-c"] (msg := "the wrapper is a shell script")
    -- Everything after the script and the shell's `$0` is the agent's own argv, unchanged.
    TestM.assertEqual (cmd.drop 4) ["claude", "-p", "fix the bug"] (msg := "the agent's argv")
    TestM.assertEqual (c.getObjValAs? String "workingDir" |>.toOption)
      (some "/var/lib/orchestra/work/acme-widgets-slot0") (msg := "it starts in the checkout")
    TestM.assertEqual (c.getObjValAs? String "image" |>.toOption)
      (some "ghcr.io/example/agent:1") (msg := "image")

@[test]
def theWrapperWaitsToBeToldToStart : Test := do
  -- The workspace is copied in after the container is running, because there is no earlier moment
  -- to copy it at; the go-file is what keeps the agent from starting before it arrives.
  TestM.assert (AgentDef.containsCI agentScript "/orchestra/go") "it waits for the go file"
  TestM.assert (AgentDef.containsCI agentScript "2>/orchestra/stderr")
    "and sends its stderr to a file, so the pod log stays a clean event stream"
  TestM.assert (AgentDef.containsCI agentScript "/orchestra/exit") "and records its exit code"

@[test]
def aSecondContainerOutlivesTheAgent : Test := do
  -- `kubectl exec` needs a running container, and the agent's is gone at exactly the moment there
  -- is something to copy back out of it.
  match container "workspace" with
  | none   => TestM.fail "there is no workspace container"
  | some c =>
    TestM.assert (AgentDef.containsCI sidecarScript "/orchestra/release")
      "it stays alive until the daemon releases it"
    let mounts := c.getObjVal? "volumeMounts" |>.toOption
    match mounts with
    | some (.arr ms) =>
      let paths := ms.toList.filterMap (·.getObjValAs? String "mountPath" |>.toOption)
      TestM.assert (paths.contains "/var/lib/orchestra/work/acme-widgets-slot0")
        "and it can see the checkout, or it could not copy it"
      TestM.assert (paths.contains "/orchestra") "and the control directory"
    | _ => TestM.fail "the workspace container mounts nothing"

@[test]
def theCheckoutIsMountedWhereTheDaemonHasIt : Test := do
  match container "agent" with
  | none   => TestM.fail "there is no agent container"
  | some c =>
    match c.getObjVal? "volumeMounts" |>.toOption with
    | some (.arr ms) =>
      let paths := ms.toList.filterMap (·.getObjValAs? String "mountPath" |>.toOption)
      TestM.assert (paths.contains "/var/lib/orchestra/work/acme-widgets-slot0") "the checkout"
      TestM.assert (paths.contains "/opt/orchestra/plugins") "the plugin directory"
      TestM.assert (paths.contains "/home/agent") "a writable home"
      TestM.assert (paths.contains "/orchestra") "the control directory"
    | _ => TestM.fail "the agent container mounts nothing"

@[test]
def thePodIsNotAllowedToOutliveTheRun : Test := do
  match manifest.getObjVal? "spec" |>.toOption with
  | none => TestM.fail "the manifest has no spec"
  | some spec =>
    TestM.assertEqual (spec.getObjValAs? String "restartPolicy" |>.toOption) (some "Never")
      (msg := "a failed agent is a failed run, not a retry the daemon cannot see")
    -- The backstop for a daemon that dies mid-run: without it the pod would hold a token and a
    -- checkout until someone noticed.
    TestM.assertEqual (spec.getObjValAs? Nat "activeDeadlineSeconds" |>.toOption) (some 14400)
      (msg := "the cluster kills the pod eventually, whatever the daemon is doing")

/-! ## The shell the pod actually runs

The two scripts below are the only real logic this backend runs inside the cluster, and both are
load-bearing in a way a rendering test cannot reach: one decides when the agent starts and what
its exit code was, the other decides whether the daemon ever stops waiting. They take the control
directory as a parameter precisely so they can be run here, by the same `/bin/sh`, against a
directory in `/tmp`. -/

private def scratchDir : IO System.FilePath := do
  let dir := System.FilePath.mk s!"/tmp/orchestra-k8s-test-{← Exec.randomHex 6}"
  IO.FS.createDirAll dir
  return dir

@[test]
def theWrapperHoldsTheAgentUntilTheWorkspaceArrives : Test := do
  let dir ← scratchDir
  let child ← IO.Process.spawn {
    cmd := "/bin/sh"
    args := #["-c", agentScript dir.toString, "orchestra-agent",
              "/bin/sh", "-c", "echo hello; echo trouble 1>&2; exit 3"]
    stdin := .null, stdout := .piped, stderr := .piped }
  -- Nothing runs until the go-file appears: this is what makes it safe to copy a repository into
  -- a container that is already running.
  IO.sleep 400
  TestM.assert (!(← (dir / "exit").pathExists)) "the agent has not run yet"
  IO.FS.writeFile (dir / "go") ""
  let line ← child.stdout.getLine
  let exitCode ← child.wait
  TestM.assertEqual line.trimAscii.toString "hello" (msg := "stdout is the pod's log")
  TestM.assertEqual exitCode 3 (msg := "the container exits with the agent's code")
  -- stderr goes to a file rather than the log, because a pod's log merges the two and orchestra
  -- reads them for different things — events on one, a usage limit on the other.
  TestM.assertEqual (← IO.FS.readFile (dir / "stderr")).trimAscii.toString "trouble"
    (msg := "stderr was captured separately")
  TestM.assertEqual (← IO.FS.readFile (dir / "exit")).trimAscii.toString "3"
    (msg := "and the exit code is left where the sidecar can still be asked for it")
  try IO.FS.removeDirAll dir catch _ => pure ()

@[test]
def theStderrStreamEndsWhenTheAgentDoes : Test := do
  -- The hang this rules out: the daemon drains both streams before it collects an exit code, so a
  -- `tail -f` that outlived the run would stall the task for as long as the pod's deadline.
  let dir ← scratchDir
  IO.FS.writeFile (dir / "stderr") "first line
"
  let child ← IO.Process.spawn {
    cmd := "/bin/sh", args := #["-c", stderrScript dir.toString]
    stdin := .null, stdout := .piped, stderr := .piped }
  let line ← child.stdout.getLine
  TestM.assertEqual line.trimAscii.toString "first line" (msg := "it streams what is there")
  IO.FS.writeFile (dir / "exit") "0
"
  let exitCode ← child.wait
  TestM.assertEqual exitCode 0 (msg := "and ends itself once the agent has recorded its exit")
  try IO.FS.removeDirAll dir catch _ => pure ()

@[test]
def homeIsAPodLocalScratchUnlessAClaimIsNamed : Test := do
  let volumes (j : Json) : List Json :=
    match j.getObjVal? "spec" |>.toOption |>.bind (·.getObjVal? "volumes" |>.toOption) with
    | some (.arr vs) => vs.toList
    | _              => []
  let home (j : Json) : Option Json :=
    (volumes j).find? fun v => (v.getObjValAs? String "name" |>.toOption) == some "home"
  match home manifest with
  | none   => TestM.fail "the pod has no home volume"
  | some v =>
    TestM.assert (AgentDef.containsCI v.compress "emptyDir")
      "by default the agent's state lives and dies with the pod"
  -- With a claim it survives, which is what makes a validation retry able to `--resume` the
  -- session the first attempt started: that session is a file under the agent's home.
  let withClaim := podManifest (config [("home_claim", .str "orchestra-agent-home")])
    sampleSpec "orchestra-abc123" "orchestra-abc123-env" staged
  match home withClaim with
  | none   => TestM.fail "the pod has no home volume"
  | some v =>
    TestM.assert (AgentDef.containsCI v.compress "orchestra-agent-home")
      "a configured claim becomes the agent's home"
    TestM.assert (!AgentDef.containsCI v.compress "emptyDir") "and replaces the scratch volume"

/-! ## Credentials -/

@[test]
def theEnvironmentTravelsInASecretAndNotInThePod : Test := do
  -- A pod's spec is readable by anything that can list pods in the namespace, and the environment
  -- is where the installation token and the agent's API key are.
  TestM.assert (!AgentDef.containsCI manifest.compress "ghs_averysecrettoken")
    "no credential appears in the pod manifest"
  match container "agent" with
  | none   => TestM.fail "there is no agent container"
  | some c =>
    TestM.assert (AgentDef.containsCI c.compress "orchestra-abc123-env")
      "the agent's environment comes from the run's secret"
  let secret := secretManifest (config) sampleSpec "orchestra-abc123-env" "orchestra-abc123"
  TestM.assertEqual
    (secret.getObjVal? "stringData" |>.toOption |>.bind
      (·.getObjValAs? String "GH_TOKEN" |>.toOption))
    (some "ghs_averysecrettoken") (msg := "the secret carries it instead")
  TestM.assert (AgentDef.containsCI secret.compress "orchestra.dev/run")
    "and it is labelled with the run, so an orphan can be found and deleted"

/-! ## Reaching the daemon from the cluster -/

@[test]
def theBackendMovesTheEndpointAndKeepsTheToken : Test := do
  match Kubernetes.factory.make (options) with
  | .error e => TestM.fail s!"a valid config was rejected: {e}"
  | .ok b =>
    TestM.assert (b.exposure == .network "0.0.0.0")
      "the daemon has to listen where a pod can reach it"
    let reached ← b.mcpEndpoint { host := "127.0.0.1", port := 8080, token := some "s3cret" }
    TestM.assertEqual reached.host "orchestra.orchestra.svc.cluster.local"
      (msg := "the agent is told the daemon's cluster address")
    TestM.assertEqual reached.port 8080 (msg := "on the port the server actually bound")
    TestM.assertEqual reached.token (some "s3cret") (msg := "carrying the token it must present")

@[test]
def anExposedServerAlwaysGetsAToken : Test := do
  let (bind, token) ← Exec.mcpBinding { name := "test", exposure := .network "0.0.0.0"
                                      , describe := fun _ => pure "", start := fun _ =>
                                          throw (IO.userError "not started") }
  TestM.assertEqual bind "0.0.0.0" (msg := "bind address")
  match token with
  | none   => TestM.fail "a server reachable over a network was left unauthenticated"
  | some t => TestM.assert (t.length ≥ 32) "and the token is long enough to be worth having"
  let (loopbackBind, loopbackToken) ← Exec.mcpBinding Landrun.backend
  TestM.assertEqual loopbackBind "127.0.0.1" (msg := "a local run still binds loopback")
  TestM.assertEqual loopbackToken none
    (msg := "and needs no token: only this machine can connect at all")

@[test]
def theClientSendsItsTokenBeforeAnythingElse : Test := do
  -- `nc` has no way to put a line on the connection before the agent's own traffic, so a token
  -- turns the transport into a shell that sends it and then gets out of the way.
  let (cmd, args) := McpEndpoint.stdioCommand { host := "10.0.0.1", port := 4000 }
  TestM.assertEqual cmd "nc" (msg := "an unauthenticated endpoint is still plain nc")
  TestM.assertEqual args #["10.0.0.1", "4000"] (msg := "with the host and port")
  let (cmd', args') := McpEndpoint.stdioCommand
    { host := "orchestra.svc", port := 4000, token := some "abc123" }
  TestM.assertEqual cmd' "sh" (msg := "an authenticated one goes through a shell")
  let script := args'[1]!
  TestM.assert (AgentDef.containsCI script "echo abc123") "which sends the token first"
  TestM.assert (AgentDef.containsCI script "nc orchestra.svc 4000") "and then the connection"
  TestM.assert (!AgentDef.containsCI script "\\n")
    "with no backslash escape, which TOML and JSON would read differently"

private def testServerState (token : Option String) : Server.State :=
  { upstream := { owner := "acme", name := "widgets" }
    fork := { owner := "acme", name := "widgets" }
    allowedTools := []
    appId := 0, privateKeyPath := "", installationId := 0, pat := ""
    authToken := token }

/-- Ask an MCP server for its tool list the way an agent would: through the command
    `McpEndpoint.stdioCommand` produces, token and all. Returns whatever the server said, or `""`
    when it said nothing.

    Both sides go through files and the answer is collected after a fixed wait, rather than by
    reading until end-of-file. A client that is refused is *supposed* to be told nothing, and
    "nothing, ever" is not a thing a blocking read can distinguish from "not yet". -/
private def askOverStdio (endpoint : McpEndpoint) : IO String := do
  let tag ← Exec.randomHex 6
  let inPath  := s!"/tmp/orchestra-mcp-{tag}.in"
  let outPath := s!"/tmp/orchestra-mcp-{tag}.out"
  IO.FS.writeFile inPath "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}\n"
  IO.FS.writeFile outPath ""
  let (cmd, args) := endpoint.stdioCommand
  let rendered := String.intercalate " " ((#[cmd] ++ args).toList.map Exec.shellEscape)
  let child ← IO.Process.spawn {
    cmd := "/bin/sh"
    args := #["-c", s!"{rendered} < {inPath} > {outPath} 2>/dev/null"]
    stdin := .null, stdout := .null, stderr := .null }
  IO.sleep 1500
  Exec.Handle.killPid child.pid
  let _ ← child.wait
  let out ← IO.FS.readFile (System.FilePath.mk outPath)
  try IO.FS.removeFile (System.FilePath.mk inPath) catch _ => pure ()
  try IO.FS.removeFile (System.FilePath.mk outPath) catch _ => pure ()
  return out

@[test]
def anAgentWithTheTokenIsServedAndOneWithoutIsNot : Test := do
  -- The whole remote path, minus the network: a server that had to listen off loopback, the
  -- transport `stdioCommand` builds for it, and the token the two agree on. `nc` is doing here
  -- exactly what it does inside a pod.
  if !(← System.FilePath.pathExists (System.FilePath.mk "/usr/bin/nc")) then
    TestM.skip "nc is not installed, and it is what an agent's MCP transport is made of"
    return ()
  let token := "s3cret-" ++ (← Exec.randomHex 8)
  let (port, shutdown) ← Server.start (testServerState (some token)) (bindHost := "127.0.0.1")
  let served ← askOverStdio { host := "127.0.0.1", port, token := some token }
  let refused ← askOverStdio { host := "127.0.0.1", port, token := some "not-the-token" }
  let silent ← askOverStdio { host := "127.0.0.1", port }
  shutdown
  TestM.assert (AgentDef.containsCI served "tools") "an agent holding the token gets its tools"
  TestM.assertEqual refused "" (msg := "one with the wrong token is hung up on, not retried")
  TestM.assertEqual silent "" (msg := "and so is one that never presents a token at all")

@[test]
def aLoopbackServerIsUnchanged : Test := do
  -- No token configured, no line expected: the protocol for a local run is exactly what it was
  -- before any of this existed.
  if !(← System.FilePath.pathExists (System.FilePath.mk "/usr/bin/nc")) then
    TestM.skip "nc is not installed"
    return ()
  let (port, shutdown) ← Server.start (testServerState none)
  let served ← askOverStdio { host := "127.0.0.1", port }
  shutdown
  TestM.assert (AgentDef.containsCI served "tools") "a plain nc client is served as before"

@[test]
def theServerBindsWhatItIsTold : Test := do
  TestM.assert (Server.parseIPv4? "0.0.0.0" |>.isSome) "0.0.0.0 parses"
  TestM.assert (Server.parseIPv4? "127.0.0.1" |>.isSome) "loopback parses"
  TestM.assert (Server.parseIPv4? "10.1.2.3" |>.isSome) "a private address parses"
  -- A name would have to be resolved before a socket could be bound to it, so it is not accepted
  -- silently: `Server.start` falls back to loopback and says so.
  TestM.assert (Server.parseIPv4? "orchestra.svc.cluster.local" |>.isNone) "a hostname does not"
  TestM.assert (Server.parseIPv4? "1.2.3.999" |>.isNone) "nor does an out-of-range quad"

/-! ## Selecting the backend -/

@[test]
def aMisconfiguredBackendFailsBeforeAnyClusterIsContacted : Test := do
  match ← Exec.resolve { backend := "kubernetes", options := Json.mkObj [] } with
  | .ok _    => TestM.fail "a kubernetes backend with no image was accepted"
  | .error e =>
    TestM.assert (AgentDef.containsCI e "misconfigured") "it reads as a configuration error"
    TestM.assert (AgentDef.containsCI e "image") "and names the key to fix"

@[test]
def theBackendListNamesKubernetes : Test := do
  match Exec.factoryOf? "kubernetes" with
  | none   => TestM.fail "kubernetes is not registered"
  | some f => TestM.assert (!f.summary.isEmpty) "and it says what it is in the unknown-name error"

end OrchestraTest.Kubernetes
