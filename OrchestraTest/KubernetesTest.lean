import OrchestraTest.TestM
import Orchestra

open Lean (Json)
open Orchestra
open Orchestra.Exec

/-!
# The Kubernetes execution backend

A pod spec is this backend's ruleset, exactly as an argument vector is landrun's, and it is checked
here the same way: by rendering it, without a cluster to send it to. What the tests hold onto is
that the pod says what the task asked for — the checkout mounted where the daemon has it, a
container that stays up so everything the task consists of can run in it, no credential anywhere a
cluster reader could find one — and that the shell each command runs under does what it claims,
which is checked by running it.

What cannot be checked without a cluster is `kubectl` itself: creating the pod, the copies in and
out, the exec streams. Those are IO against a real API server; what stands in for a test of them is
that each has one job and says so.
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

/-- What a task's session is opened with: the checkout it runs on, and every path it was granted.
    The grants are what `Sandbox.grantsFor` produces for a task with plugins and memories. -/
private def sampleSession : SessionSpec := {
  workdir := System.FilePath.mk "/var/lib/orchestra/work/acme-widgets-slot0"
  label   := "t-1234"
  grants  := #[
    { path := "/var/lib/orchestra/work/acme-widgets-slot0", access := .rwx, required := true
    , from_ := .orchestra },
    { path := "/tmp", access := .rw, required := true },
    { path := "/usr", access := .rox },
    { path := ".claude", access := .rw, scope := .home, required := true },
    { path := "/opt/orchestra/plugins", access := .rox, from_ := .orchestra },
    { path := "/var/lib/orchestra/memories/acme", access := .rw, from_ := .orchestra }]
}

private def staged : Array StagedPath := stagedPaths (config) "/home/daemon" sampleSession

private def imageOf (cfg : Config) (spec : SessionSpec) : String :=
  match imageFor cfg spec with | .ok i => i | .error _ => "<refused>"

private def manifest : Json :=
  podManifest (config) sampleSession "orchestra-abc123" (imageOf (config) sampleSession) staged

private def container : Option Json :=
  match manifest.getObjVal? "spec" |>.toOption |>.bind (·.getObjVal? "containers" |>.toOption) with
  | some (.arr cs) => cs[0]?
  | _              => none

private def mountPaths : List String :=
  match container.bind (·.getObjVal? "volumeMounts" |>.toOption) with
  | some (.arr ms) => ms.toList.filterMap (·.getObjValAs? String "mountPath" |>.toOption)
  | _              => []

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
def aMemoryDirectoryComesBackWhateverSyncBackSays : Test := do
  -- `sync_back` is about the checkout: an operator turns it off because the agent pushes its work
  -- and nothing local reads the tree afterwards. That is not a reason to throw away what the agent
  -- learned, and a memory that does not outlive its pod is not a memory. The staging plan is the
  -- same either way; what differs is what `close` copies, which the two flags below select.
  let memories := staged.find? (·.hostPath == "/var/lib/orchestra/memories/acme")
  let checkout := staged.find? (·.isWorkspace)
  match memories, checkout with
  | some m, some c =>
    TestM.assert (m.writable && !m.isWorkspace)
      "a memory directory is writable and is not the checkout — which is what close keys on"
    TestM.assert c.isWorkspace "and the checkout is the one path sync_back speaks for"
  | _, _ => TestM.fail "the staging plan is missing the checkout or the memories"

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
def thePodIsAPlaceToRunThingsRatherThanOneCommand : Test := do
  -- The task is `init.sh`, `before.sh`, the agent, `validation.sh`, the agent again, `after.sh`.
  -- The container has to be there for all of it, so it runs nothing of its own and everything is
  -- `kubectl exec`ed into it.
  match container with
  | none   => TestM.fail "the pod has no container"
  | some c =>
    let cmd : List String := match c.getObjVal? "command" |>.toOption with
      | some (.arr a) => a.toList.filterMap fun v =>
          match v with | Json.str s => some s | _ => none
      | _             => []
    TestM.assertEqual (cmd.take 2) ["/bin/sh", "-c"] (msg := "it runs a shell")
    TestM.assert (AgentDef.containsCI (cmd.getD 2 "") "sleep") "that does nothing but wait"
    TestM.assertEqual (c.getObjValAs? String "workingDir" |>.toOption)
      (some "/var/lib/orchestra/work/acme-widgets-slot0") (msg := "rooted in the checkout")
    TestM.assertEqual (c.getObjValAs? String "image" |>.toOption)
      (some "ghcr.io/example/agent:1") (msg := "image")

@[test]
def theCheckoutIsMountedWhereTheDaemonHasIt : Test := do
  TestM.assert (mountPaths.contains "/var/lib/orchestra/work/acme-widgets-slot0") "the checkout"
  TestM.assert (mountPaths.contains "/opt/orchestra/plugins") "the plugin directory"
  TestM.assert (mountPaths.contains "/var/lib/orchestra/memories/acme") "the memories"
  TestM.assert (mountPaths.contains "/home/agent") "a writable home"
  TestM.assert (mountPaths.contains "/orchestra") "and orchestra's own control directory"

@[test]
def theAgentsHomeIsPodLocalAndLastsTheTask : Test := do
  -- Everything the agent CLI writes about itself — its session above all, which a validation retry
  -- resumes — lives here. It survives every command in the task because the pod does, and nothing
  -- of it survives the task.
  let volumes := match manifest.getObjVal? "spec" |>.toOption
                       |>.bind (·.getObjVal? "volumes" |>.toOption) with
    | some (.arr vs) => vs.toList
    | _              => []
  match volumes.find? fun v => (v.getObjValAs? String "name" |>.toOption) == some "home" with
  | none   => TestM.fail "the pod has no home volume"
  | some v => TestM.assert (AgentDef.containsCI v.compress "emptyDir") "home is pod-local scratch"
  match container.bind (·.getObjVal? "env" |>.toOption) with
  | some (.arr env) =>
    TestM.assert (env.any fun e => (e.getObjValAs? String "value" |>.toOption) == some "/home/agent")
      "and HOME points at it, since the image's idea of home is not orchestra's"
  | _ => TestM.fail "the container has no environment"

@[test]
def thePodIsNotAllowedToOutliveTheTask : Test := do
  match manifest.getObjVal? "spec" |>.toOption with
  | none => TestM.fail "the manifest has no spec"
  | some spec =>
    TestM.assertEqual (spec.getObjValAs? String "restartPolicy" |>.toOption) (some "Never")
      (msg := "nothing restarts behind the daemon's back")
    -- The backstop for a daemon that dies mid-task: without it the pod would hold a checkout until
    -- someone noticed.
    TestM.assertEqual (spec.getObjValAs? Nat "activeDeadlineSeconds" |>.toOption) (some 14400)
      (msg := "the cluster ends it eventually, whatever the daemon is doing")

/-! ## Which image a task runs in

What a task needs installed — a Lean toolchain, a JDK, a browser, a database client — is a property
of the repository, so one image for the whole daemon is not an answer. Three sources, in the order
a conflict between them should be settled. -/

private def repoAsked : SessionSpec :=
  { sampleSession with repo := some "acme/widgets", image := some "ghcr.io/acme/widgets-ci:2" }

@[test]
def aRepositorySaysWhatItNeedsInstalled : Test := do
  -- The repository already writes this down for its own CI, and it is the thing that knows.
  TestM.assertEqual (imageOf (config) repoAsked) "ghcr.io/acme/widgets-ci:2"
    (msg := "the repository's own image is used")
  TestM.assertEqual (imageOf (config) sampleSession) "ghcr.io/example/agent:1"
    (msg := "and the configured default covers everything that has not said otherwise")

@[test]
def anOperatorCanPinARepositoryAndCanRefuseTheWholeIdea : Test := do
  -- A pin beats what the repository asked for: it is the choice someone made deliberately for that
  -- repository, and nothing inside the repository should be able to talk them out of it.
  let pinned := config [("images", Json.mkObj [("acme/widgets", .str "ghcr.io/ops/pinned:1")])]
  TestM.assertEqual (imageOf pinned repoAsked) "ghcr.io/ops/pinned:1"
    (msg := "the operator's pin wins")
  TestM.assertEqual (imageOf pinned { sampleSession with repo := some "acme/other" })
    "ghcr.io/example/agent:1" (msg := "and applies to the repository it names, not the rest")
  -- Or the whole mechanism can be turned off, and every task runs in what the configuration says.
  let fixed := config [("allow_repo_image", .bool false)]
  TestM.assertEqual (imageOf fixed repoAsked) "ghcr.io/example/agent:1"
    (msg := "a repository cannot choose when it is not allowed to")

@[test]
def whatARepositoryNamesIsCheckedBeforeItBecomesAManifest : Test := do
  -- This is the one image reference that did not come from the daemon's own configuration: it is
  -- read out of a file in the repository, so a name with a space or a quote in it is refused here
  -- rather than turned into a pod spec.
  TestM.assert (validImageRef "ghcr.io/acme/widgets-ci:2") "an ordinary reference is fine"
  TestM.assert (validImageRef "registry.internal:5000/team/img@sha256:abc123")
    "so is a port and a digest"
  TestM.assert (!validImageRef "") "an empty one is not"
  TestM.assert (!validImageRef "ghcr.io/acme/img:1 --privileged") "nor is one with a space in it"
  TestM.assert (!validImageRef "img\";echo") "nor one carrying a quote"
  let malformed : SessionSpec :=
    { sampleSession with repo := some "acme/widgets", image := some "not a reference" }
  match imageFor (config) malformed with
  | .ok _    => TestM.fail "a malformed reference was accepted"
  | .error e =>
    -- Refused, not quietly replaced by the default: a repository that asked for a JDK image and
    -- silently got one without would fail its validation script for a reason nothing points at.
    TestM.assert (AgentDef.containsCI e "not a usable image reference") "and says why"

@[test]
def anOperatorCanAllowTheChoiceWithoutAllowingTheRegistry : Test := do
  -- The middle ground between "any image a repository names" and "no repository chooses": a
  -- namespace that pulls from a scanned mirror lets repositories pick within it.
  let allowlisted := config [("allowed_image_prefixes", .arr #[.str "ghcr.io/acme/"])]
  TestM.assertEqual (imageOf allowlisted repoAsked) "ghcr.io/acme/widgets-ci:2"
    (msg := "a repository may pick inside the allowed prefixes")
  let outside : SessionSpec :=
    { sampleSession with
      repo := some "acme/widgets", image := some "docker.io/library/ubuntu:24.04" }
  match imageFor allowlisted outside with
  | .ok _    => TestM.fail "an image outside the allowed prefixes was accepted"
  | .error e =>
    TestM.assert (AgentDef.containsCI e "ghcr.io/acme/") "the error names what is allowed"
    TestM.assert (AgentDef.containsCI e "images") "and how to pin it instead"
  -- The operator's own two answers are never checked against it; they are the operator's.
  TestM.assertEqual (imageOf allowlisted sampleSession) "ghcr.io/example/agent:1"
    (msg := "the configured default is exempt")

@[test]
def thePullPolicyIsTheClustersUnlessItIsSet : Test := do
  -- Unset means Kubernetes' own rule: `Always` for `:latest` or no tag, `IfNotPresent` otherwise.
  -- Which is the wrong one for a floating tag that is not `:latest` — a `:main` rebuilt nightly is
  -- otherwise served from whatever each node cached.
  TestM.assert (!AgentDef.containsCI manifest.compress "imagePullPolicy")
    "nothing is said by default"
  let always := config [("image_pull_policy", .str "Always")]
  let m := podManifest always sampleSession "orchestra-abc123" (imageOf always sampleSession) staged
  TestM.assert (AgentDef.containsCI m.compress "\"imagePullPolicy\":\"Always\"")
    "and what is set reaches the container"
  -- A typo here would otherwise be a pod the API server rejects, once per task.
  match ← (Kubernetes.preflight (config [("image_pull_policy", .str "always")])) with
  | .ok _    => TestM.fail "an invalid pull policy was accepted"
  | .error e => TestM.assert (AgentDef.containsCI e "IfNotPresent") "the error names the valid ones"

@[test]
def theImageReachesThePodAndTheLogLine : Test := do
  let m := podManifest (config) repoAsked "orchestra-abc123" (imageOf (config) repoAsked) staged
  TestM.assert (AgentDef.containsCI m.compress "ghcr.io/acme/widgets-ci:2")
    "the container runs the image the repository asked for"
  -- The repository is on the pod too, so a running task can be found from what the dashboard
  -- shows. A label value cannot hold a `/`.
  TestM.assert (AgentDef.containsCI m.compress "acme.widgets") "and the pod is labelled with it"

@[test]
def aFreshEnvironmentRunsTheInitHookEveryTime : Test := do
  -- `init.sh` is what installs the toolchain, and it records that it has run in a marker inside
  -- the checkout — which is precisely the thing carried into a pod that has nothing installed. The
  -- marker would then skip the install in an environment that needs it.
  match Kubernetes.factory.make (options) with
  | .error e => TestM.fail s!"a valid config was rejected: {e}"
  | .ok _ =>
    -- The backend says its environments are new each task; `RepoConfig.runInitIfNeeded` reads that.
    TestM.assert (!Landrun.session.freshEnvironment)
      "a machine keeps what a previous task installed, so the marker means what it says"
    TestM.assert (!Local.session.freshEnvironment) "and so does this one"

@[test]
def apersistentHomeIsWhatMakesInitCheap : Test := do
  -- Every task starts from a new pod, so a hook that installs a toolchain pays in full each time
  -- unless what it installs survives — and `~/.elan`, `~/.cargo` and `~/.cache` are all under
  -- `$HOME`.
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
      "by default it is scratch, and every task installs from nothing"
  let cachedCfg := config [("home_claim", .str "orchestra-agent-home")]
  let cached := podManifest cachedCfg sampleSession "orchestra-abc123"
    (imageOf cachedCfg sampleSession) staged
  match home cached with
  | none   => TestM.fail "the pod has no home volume"
  | some v =>
    TestM.assert (AgentDef.containsCI v.compress "orchestra-agent-home")
      "a configured claim becomes the agent's home"
    TestM.assert (!AgentDef.containsCI v.compress "emptyDir") "and replaces the scratch volume"

@[test]
def aConversationOnlyOutlivesThePodIfHomeDoes : Test := do
  -- Within a task the pod is reused for every command, so the retry after a failed validation
  -- resumes what the first attempt started. Across tasks — `continues_from`, a series — the pod is
  -- gone, and the conversation is a file the agent CLI wrote under its `$HOME`.
  match Kubernetes.factory.make (options) with
  | .error e => TestM.fail s!"a valid config was rejected: {e}"
  | .ok _    => pure ()
  let scratch := config
  let kept    := config [("home_claim", .str "orchestra-agent-home")]
  TestM.assert (!scratch.homeClaim.isSome)
    "with a scratch home, nothing the agent wrote about itself outlives the task"
  TestM.assert kept.homeClaim.isSome
    "with a claim, home outlives the pod and an earlier task's conversation is still there"
  -- The backends that run on this machine always can: every task shares one home.
  TestM.assert Landrun.session.carriesAgentState "a landrun session carries the agent's state"
  TestM.assert Local.session.carriesAgentState "and so does an unconfined local one"

/-! ## Running something in the pod -/

@[test]
def credentialsAreNeitherInThePodNorOnACommandLine : Test := do
  -- A pod's spec is readable by anything that can list pods, and a command line shows up in the
  -- cluster's audit log and in `/proc` inside the pod. The environment goes in a file instead,
  -- copied in over the same channel as the checkout.
  TestM.assert (!AgentDef.containsCI manifest.compress "ghs_")
    "the pod carries no environment of its own beyond HOME"
  let env := envFileContents #[("GH_TOKEN", "ghs_averysecrettoken"), ("EMPTY", "")]
  TestM.assert (AgentDef.containsCI env "export GH_TOKEN=") "the file exports what was asked for"
  TestM.assert (AgentDef.containsCI env "ghs_averysecrettoken") "with the value"
  let args := execArgs (config) "orchestra-abc123" false "script" "claude" #["-p", "do it"]
  TestM.assert (!args.any fun a => AgentDef.containsCI a "ghs_")
    "and no credential reaches the command line"

@[test]
def anInteractiveRunAsksForATerminal : Test := do
  -- This is what makes `orchestra interactive` work on a cluster: the same exec, with a TTY, and
  -- the daemon's own streams handed through.
  let plain := execArgs (config) "pod-1" false "script" "claude" #["-p", "x"]
  let tty   := execArgs (config) "pod-1" true  "script" "claude" #[]
  TestM.assert (!plain.contains "-t") "a queued run has no terminal to give"
  TestM.assert (tty.contains "-t" && tty.contains "-i") "an interactive one asks for both"
  -- The agent's own argv survives intact in either case, after the `--` that ends kubectl's flags.
  TestM.assertEqual (plain.toList.drop (plain.toList.length - 3)) ["claude", "-p", "x"]
    (msg := "the command and its arguments")
  TestM.assert (plain.contains "--") "separated from kubectl's own flags"

@[test]
def theRunnerScriptSourcesTheEnvironmentAndBecomesTheCommand : Test := do
  -- Run for real, by the same `/bin/sh` that runs it in the pod: an environment file it sources, a
  -- directory it has to change to, and a command it must `exec` rather than wrap — so that what
  -- the container kills is the agent.
  let dir := System.FilePath.mk s!"/tmp/orchestra-k8s-test-{← Exec.randomHex 6}"
  IO.FS.createDirAll (dir / "work")
  let envFile := dir / "env"
  IO.FS.writeFile envFile (envFileContents #[("GREETING", "hello from the env file")])
  let child ← IO.Process.spawn {
    cmd := "/bin/sh"
    args := #["-c", runnerScript envFile.toString (dir / "work").toString, "orchestra",
              "/bin/sh", "-c", "echo \"$GREETING from $(pwd)\"; exit 7"]
    stdin := .null, stdout := .piped, stderr := .piped }
  let line ← child.stdout.getLine
  let exitCode ← child.wait
  try IO.FS.removeDirAll dir catch _ => pure ()
  TestM.assert (AgentDef.containsCI line "hello from the env file")
    "the environment file was sourced"
  TestM.assert (AgentDef.containsCI line (dir / "work").toString)
    "and the command ran in the checkout"
  TestM.assertEqual exitCode 7 (msg := "its exit code is the one the caller sees")

@[test]
def anAgentShootsWhateverTheLastOneLeftBehind : Test := do
  -- A `kubectl exec` connection can die without the process at the far end dying with it, and the
  -- daemon cannot tell that from the agent having exited. Without this, the retry after a failed
  -- validation would put a second agent in the same checkout as the first.
  let script := runnerScript "/orchestra/env-1" "/work" (guard := true)
  TestM.assert (AgentDef.containsCI script "kill -9") "the previous agent is killed"
  TestM.assert (AgentDef.containsCI script agentPidPath) "by the id it recorded"
  TestM.assert (AgentDef.containsCI script "echo $$") "and this one records its own"
  -- Only the agent: a hook or the validation script killing the agent would be a bug of its own.
  let plain := runnerScript "/orchestra/env-2" "/work"
  TestM.assert (!AgentDef.containsCI plain "kill") "the repository's own scripts kill nothing"

@[test]
def anEnvironmentValueCannotEndItsOwnAssignment : Test := do
  -- The values here are a GitHub token and an API key, but also a system prompt and a model name;
  -- one of them containing a quote must not turn into a second command.
  let env := envFileContents #[("EVIL", "'; touch /tmp/orchestra-pwned; echo '")]
  let dir := System.FilePath.mk s!"/tmp/orchestra-k8s-test-{← Exec.randomHex 6}"
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "env") env
  let child ← IO.Process.spawn {
    cmd := "/bin/sh"
    args := #["-c", runnerScript (dir / "env").toString dir.toString, "orchestra",
              "/bin/sh", "-c", "printf '%s' \"$EVIL\""]
    stdin := .null, stdout := .piped, stderr := .piped }
  let out ← child.stdout.readToEnd
  let _ ← child.wait
  try IO.FS.removeDirAll dir catch _ => pure ()
  TestM.assertEqual out "'; touch /tmp/orchestra-pwned; echo '"
    (msg := "the value arrives as itself")
  TestM.assert (!(← System.FilePath.pathExists (System.FilePath.mk "/tmp/orchestra-pwned")))
    "and nothing in it ran"

/-! ## Reaching the daemon from the cluster -/

@[test]
def theBackendMovesTheEndpointAndKeepsTheToken : Test := do
  match Kubernetes.factory.make (options) with
  | .error e => TestM.fail s!"a valid config was rejected: {e}"
  | .ok b =>
    TestM.assert (b.exposure == .network "0.0.0.0" none)
      "the daemon has to listen where a pod can reach it"
    let reached ← b.mcpEndpoint { host := "127.0.0.1", port := 8080, token := some "s3cret" }
    TestM.assertEqual reached.host "orchestra.orchestra.svc.cluster.local"
      (msg := "the agent is told the daemon's cluster address")
    TestM.assertEqual reached.port 8080 (msg := "on the port the server actually bound")
    TestM.assertEqual reached.token (some "s3cret") (msg := "carrying the token it must present")

@[test]
def anExposedServerAlwaysGetsAToken : Test := do
  let (bind, _ports, token) ← Exec.mcpBinding { name := "test"
                                              , exposure := .network "0.0.0.0" none
                                              , openSession := fun _ =>
                                                  throw (IO.userError "not opened") }
  TestM.assertEqual bind "0.0.0.0" (msg := "bind address")
  match token with
  | none   => TestM.fail "a server reachable over a network was left unauthenticated"
  | some t => TestM.assert (t.length ≥ 32) "and the token is long enough to be worth having"
  let (loopbackBind, loopbackPorts, loopbackToken) ← Exec.mcpBinding Landrun.backend
  TestM.assertEqual loopbackBind "127.0.0.1" (msg := "a local run still binds loopback")
  TestM.assertEqual loopbackToken none
    (msg := "and needs no token: only this machine can connect at all")
  TestM.assertEqual loopbackPorts none
    (msg := "nor a port anything outside has to be told about in advance")

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
  { repo := some { upstream := { owner := "acme", name := "widgets" }
                 , fork := { owner := "acme", name := "widgets" } }
    allowedTools := []
    appId := 0, privateKeyPath := "", installationId := some 0, pat := ""
    authToken := token }

private def await (p : IO.Promise (Except IO.Error α)) : IO α := do
  match ← IO.wait p.result! with
  | .error e => throw e
  | .ok v    => return v

/-- Open one connection to the MCP server on `port`, send `lines`, and return what came back —
    `""` if the server hung up without saying anything.

    Blocking on the answer is safe here precisely because of the property being tested: a client is
    either served or disconnected, so something always arrives. An agent reaches the same socket
    through `nc` (see `theClientSendsItsTokenBeforeAnythingElse` for the command that puts the
    token on it); a socket is used here because `nc` does not exit when the far end hangs up while
    its own stdin is still open, which would leave a test waiting on a pipe forever. -/
private def ask (port : UInt16) (lines : List String) : IO String := do
  let sock ← Std.Internal.UV.TCP.Socket.new
  let addr := Std.Net.SocketAddress.v4 { addr := Std.Net.IPv4Addr.ofParts 127 0 0 1, port }
  await (← sock.connect addr)
  for l in lines do
    await (← sock.send #[(l ++ "\n").toUTF8])
  let answer ← match ← await (← sock.recv? 65536) with
    | none       => pure ""
    | some bytes => pure (String.fromUTF8! bytes)
  -- Closed from this side too, so the server's handler stops waiting on a connection nobody is
  -- going to say anything else on.
  try await (← sock.shutdown) catch _ => pure ()
  return answer

private def toolsList : String := "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\"}"

@[test]
def anAgentWithTheTokenIsServedAndOneWithoutIsNot : Test := do
  -- The server as a pod would find it: listening for something that is not on this machine, and
  -- letting in only what presents the secret minted with it.
  let token := "s3cret-" ++ (← Exec.randomHex 8)
  let (port, shutdown) ← Server.start (testServerState (some token)) (bindHost := "127.0.0.1")
  let served ← ask port [token, toolsList]
  let wrong  ← ask port ["not-the-token", toolsList]
  let silent ← ask port [toolsList]
  shutdown
  TestM.assert (AgentDef.containsCI served "tools") "an agent holding the token gets its tools"
  TestM.assertEqual wrong "" (msg := "one with the wrong token is hung up on, not asked again")
  TestM.assertEqual silent ""
    (msg := "and so is one that starts talking without presenting a token at all")

@[test]
def aLoopbackServerIsUnchanged : Test := do
  -- No token configured, no line expected: for a local run the protocol is exactly what it was
  -- before any of this existed, which is what keeps `nc <host> <port>` a working transport.
  let (port, shutdown) ← Server.start (testServerState none)
  let served ← ask port [toolsList]
  shutdown
  TestM.assert (AgentDef.containsCI served "tools") "a client that sends no token is served"

@[test]
def aDaemonOutsideTheClusterCanBeGivenPortsToLiveOn : Test := do
  -- The point of a range: something between the pods and the daemon — a firewall rule, a
  -- port-forward, a tunnel — has to be told the port before the task that uses it exists, and an
  -- ephemeral one cannot be told to anybody.
  let c := config [("mcp_ports", .arr #[.num 31000, .num 31009])]
  TestM.assertEqual c.mcpPorts (some (31000, 31009)) (msg := "the range is read")
  match Kubernetes.factory.make (options [("mcp_ports", .arr #[.num 31000, .num 31009])]) with
  | .error e => TestM.fail s!"a valid config was rejected: {e}"
  | .ok b =>
    let (_, ports, _) ← Exec.mcpBinding b
    TestM.assertEqual ports (some (31000, 31009)) (msg := "and reaches the server that binds it")
  -- Nonsense is ignored rather than obeyed: a backwards or out-of-range pair would otherwise
  -- become a server that binds nothing and a task that fails for an unrelated-looking reason.
  TestM.assertEqual (config [("mcp_ports", .arr #[.num 900, .num 100])]).mcpPorts none
    (msg := "a backwards range is not a range")
  TestM.assertEqual (config [("mcp_ports", .arr #[.num 1])]).mcpPorts none
    (msg := "nor is a single number")

@[test]
def theServerListensInsideTheRangeItWasGiven : Test := do
  -- Started for real: this is the property a firewall rule depends on.
  let (port, shutdown) ← Server.start (testServerState none) (bindHost := "127.0.0.1")
    (portRange := some (39120, 39129))
  shutdown
  TestM.assert (port ≥ 39120 && port ≤ 39129) s!"the server listened on {port}, inside the range"
  -- A second server takes the next port, which is what makes the range's width the number of
  -- tasks that can run at once.
  let (first, shutFirst) ← Server.start (testServerState none) (bindHost := "127.0.0.1")
    (portRange := some (39130, 39131))
  let (second, shutSecond) ← Server.start (testServerState none) (bindHost := "127.0.0.1")
    (portRange := some (39130, 39131))
  shutFirst
  shutSecond
  TestM.assert (first != second) "two tasks at once do not land on the same port"

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
