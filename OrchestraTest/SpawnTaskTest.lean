import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Project
open Orchestra.Project.Tools

namespace OrchestraTest.SpawnTask

/-! Everything the `queue_task` tool decides before it touches anything: parsing the call, and
    `SpawnPolicy.resolve`, which is the whole of the policy. Both are pure, so none of this needs
    taxis, GitHub or a queue — which is the point of keeping the decision out of the enqueuing
    (`TaskRunner.enqueueTaskImpl`, where the parts that must talk to something live). -/

private def parent : SpawnContext :=
  { backend := some "claude"
  , model   := some "opus"
  , repo    := some { owner := "acme", name := "widgets" }
  , tools   := ["work_issues", "comment"]
  , projectId := some ⟨7⟩ }

private def ask (prompt : String := "do the thing") : SpawnRequest := { prompt }

/-- A policy that widens nothing: the agent may name none of the four, so every field of the
    task it queues is inherited. -/
private def widensNothing : SpawnPolicy := {}

private def repoOf (s : String) : Repository :=
  match Repository.parse s with
  | .ok r    => r
  | .error _ => { owner := "?", name := "?" }

/-! ## Inheritance: an empty policy still queues a copy of the task -/

@[test]
def emptyPolicyInheritsEverything : Test := do
  match widensNothing.resolve parent (ask) with
  | .error e => TestM.fail s!"an empty policy should still queue a copy: {e}"
  | .ok r =>
    TestM.assertEqual r.backend (some "claude") (msg := "backend inherited")
    TestM.assertEqual r.model   (some "opus")   (msg := "model inherited")
    TestM.assertEqual (r.repo.map (·.toString)) (some "acme/widgets")
      (msg := "repository inherited")
    TestM.assertEqual r.tools ["work_issues", "comment"] (msg := "tools inherited")
    TestM.assertEqual r.projectId (some (⟨7⟩ : Taxis.IssueId)) (msg := "project inherited")

@[test]
def repolessParentQueuesRepolessChild : Test := do
  match widensNothing.resolve { parent with repo := none } (ask) with
  | .error e => TestM.fail s!"a repository-independent task should queue one too: {e}"
  | .ok r => TestM.assert r.repo.isNone "no repository to inherit, so none queued"

/-! ## The lists widen; they do not restrict what is inherited -/

@[test]
def namingAnUnlistedBackendIsRefused : Test := do
  let p : SpawnPolicy := { backends := ["claude", "codex"] }
  match p.resolve parent { ask with backend := some "vibe" } with
  | .ok _ => TestM.fail "'vibe' is not in the policy and should have been refused"
  | .error e =>
    TestM.assert (e.containsSubstr "claude, codex")
      s!"the refusal should name what may be picked instead, got: {e}"

@[test]
def namingAListedBackendIsAllowed : Test := do
  let p : SpawnPolicy := { backends := ["claude", "codex"] }
  match p.resolve parent { ask with backend := some "codex" } with
  | .error e => TestM.fail s!"'codex' is listed and should be allowed: {e}"
  | .ok r    => TestM.assertEqual r.backend (some "codex") (msg := "backend named")

/-- An empty list is not "no opinion": it is "you may name nothing", and the refusal has to say
    so rather than listing an empty set of alternatives. -/
@[test]
def emptyListMeansNoChoiceAtAll : Test := do
  match widensNothing.resolve parent { ask with model := some "haiku" } with
  | .ok _ => TestM.fail "no models are listed, so naming one should be refused"
  | .error e =>
    TestM.assert (e.containsSubstr "only inherit its own")
      s!"the refusal should say the task may only inherit, got: {e}"

@[test]
def unlistedRepositoryIsRefused : Test := do
  let p : SpawnPolicy := { repos := [repoOf "acme/docs"] }
  match p.resolve parent { ask with repo := some (repoOf "acme/secrets") } with
  | .ok _ => TestM.fail "acme/secrets is not listed and should have been refused"
  | .error e =>
    TestM.assert (e.containsSubstr "acme/docs")
      s!"the refusal should name the repositories allowed, got: {e}"

@[test]
def listedRepositoryIsAllowed : Test := do
  let p : SpawnPolicy := { repos := [repoOf "acme/docs"] }
  match p.resolve parent { ask with repo := some (repoOf "acme/docs") } with
  | .error e => TestM.fail s!"acme/docs is listed and should be allowed: {e}"
  | .ok r    => TestM.assertEqual (r.repo.map (·.toString)) (some "acme/docs")
                  (msg := "repository named")

/-! ## Tools

The policy may grant more than the spawning task holds — a read-only planner queueing an
implementor is the case the tool exists for — and what the task already holds is grantable
without being listed. -/

@[test]
def policyMayGrantMoreThanTheSpawnerHolds : Test := do
  let p : SpawnPolicy := { tools := ["create_pr"] }
  match p.resolve parent { ask with tools := some ["create_pr"] } with
  | .error e => TestM.fail s!"the policy lists create_pr, so it may be granted: {e}"
  | .ok r    => TestM.assertEqual r.tools ["create_pr"] (msg := "granted from the policy")

@[test]
def ownToolsAreGrantableUnlisted : Test := do
  match widensNothing.resolve parent { ask with tools := some ["work_issues"] } with
  | .error e => TestM.fail s!"naming a tool the task already holds reaches nothing new: {e}"
  | .ok r    => TestM.assertEqual r.tools ["work_issues"] (msg := "own tool re-granted")

@[test]
def toolsBeyondBothAreRefused : Test := do
  let p : SpawnPolicy := { tools := ["create_pr"] }
  match p.resolve parent { ask with tools := some ["create_pr", "manage_issues"] } with
  | .ok _ => TestM.fail "manage_issues is neither listed nor held, so it should be refused"
  | .error e =>
    TestM.assert (e.containsSubstr "manage_issues")
      s!"the refusal should name the tool it turned down, got: {e}"

/-- `none` (inherit) and `some []` (grant nothing) are different answers, and a task queued
    deliberately toolless must not fall back to the spawner's set. -/
@[test]
def emptyToolsArrayIsNotAbsent : Test := do
  match widensNothing.resolve parent { ask with tools := some [] } with
  | .error e => TestM.fail s!"asking for no tools is a legitimate request: {e}"
  | .ok r    => TestM.assert r.tools.isEmpty "no tools granted"

/-! ## Budget -/

@[test]
def budgetCeilingIsAlsoTheDefault : Test := do
  let p : SpawnPolicy := { maxBudget := some 2.0 }
  match p.resolve parent (ask) with
  | .error e => TestM.fail s!"an unnamed budget should take the ceiling: {e}"
  | .ok r    => TestM.assert (r.budget == some 2.0)
                  s!"expected the ceiling as the default, got {repr r.budget}"

@[test]
def budgetOverTheCeilingIsRefused : Test := do
  let p : SpawnPolicy := { maxBudget := some 2.0 }
  match p.resolve parent { ask with budget := some 8.0 } with
  | .ok _ => TestM.fail "8.0 is over the 2.0 ceiling and should be refused"
  | .error e =>
    TestM.assert (e.containsSubstr "2.0") s!"the refusal should carry the ceiling, got: {e}"

@[test]
def budgetUnderTheCeilingIsKept : Test := do
  let p : SpawnPolicy := { maxBudget := some 2.0 }
  match p.resolve parent { ask with budget := some 1.5 } with
  | .error e => TestM.fail s!"1.5 is under the ceiling: {e}"
  | .ok r    => TestM.assert (r.budget == some 1.5) "budget kept as asked"

/-! ## Pre-claim -/

@[test]
def preClaimNeedsThePolicyToAllowIt : Test := do
  match widensNothing.resolve parent { ask with issueId := some ⟨11⟩, preClaim := true } with
  | .ok _ => TestM.fail "allow_pre_claim is off, so claiming should be refused"
  | .error e =>
    TestM.assert (e.containsSubstr "allow_pre_claim")
      s!"the refusal should name the field that would enable it, got: {e}"

@[test]
def preClaimNeedsAnIssue : Test := do
  let p : SpawnPolicy := { allowPreClaim := true }
  match p.resolve parent { ask with preClaim := true } with
  | .ok _ => TestM.fail "there is nothing to claim without an issue"
  | .error e =>
    TestM.assert (e.containsSubstr "issue_id") s!"the refusal should say what is missing: {e}"

@[test]
def preClaimAllowedWithAnIssue : Test := do
  let p : SpawnPolicy := { allowPreClaim := true }
  match p.resolve parent { ask with issueId := some ⟨11⟩, preClaim := true } with
  | .error e => TestM.fail s!"the policy allows it and an issue was named: {e}"
  | .ok r =>
    TestM.assert r.preClaim "pre-claim carried through"
    TestM.assertEqual r.issueId (some (⟨11⟩ : Taxis.IssueId)) (msg := "issue bound")

/-! ## What the agent never chooses -/

@[test]
def priorityAndReadOnlyComeFromThePolicy : Test := do
  let p : SpawnPolicy := { priority := 50, readOnly := true }
  match p.resolve parent (ask) with
  | .error e => TestM.fail s!"resolve: {e}"
  | .ok r =>
    TestM.assertEqual r.priority 50 (msg := "priority from the policy")
    TestM.assert r.readOnly "read_only from the policy"

@[test]
def maxTasksIsCarriedToTheEnqueuer : Test := do
  let p : SpawnPolicy := { maxTasks := 4 }
  match p.resolve parent (ask) with
  | .error e => TestM.fail s!"resolve: {e}"
  | .ok r    => TestM.assertEqual r.maxTasks 4 (msg := "ceiling carried for the refusal message")

@[test]
def blankPromptIsRefused : Test := do
  match widensNothing.resolve parent { ask with prompt := "   " } with
  | .ok _ => TestM.fail "a queued task with a blank prompt runs an agent against nothing"
  | .error e => TestM.assert (e.containsSubstr "prompt") s!"names the empty field: {e}"

/-! ## Config: policies are read strictly, and validated against the role vocabulary -/

@[test]
def policyRoundTrips : Test := do
  let p : SpawnPolicy :=
    { backends := ["claude"], models := ["opus"], tools := ["create_pr"]
    , repos := [repoOf "acme/widgets"], maxTasks := 3, allowPreClaim := true
    , maxBudget := some 2.5, priority := 40, readOnly := true }
  match FromJson.fromJson? (ToJson.toJson p) (α := SpawnPolicy) with
  | .error e => TestM.fail s!"round-trip: {e}"
  | .ok got =>
    TestM.assertEqual got.backends ["claude"] (msg := "backends")
    TestM.assertEqual got.models ["opus"] (msg := "models")
    TestM.assertEqual got.tools ["create_pr"] (msg := "tools")
    TestM.assertEqual (got.repos.map (·.toString)) ["acme/widgets"] (msg := "repos")
    TestM.assertEqual got.maxTasks 3 (msg := "max_tasks")
    TestM.assert got.allowPreClaim "allow_pre_claim"
    TestM.assert (got.maxBudget == some 2.5) "max_budget"
    TestM.assertEqual got.priority 40 (msg := "priority")
    TestM.assert got.readOnly "read_only"

/-- A misspelled list must not read as the empty one: empty means "the agent may name nothing",
    so swallowing the error would silently do the opposite of what the file says. -/
@[test]
def malformedListIsAnError : Test := do
  match Json.parse "{\"backends\": \"claude\"}" >>= FromJson.fromJson? (α := SpawnPolicy) with
  | .ok _    => TestM.fail "a bare string where a list belongs should not parse as no list"
  | .error _ => TestM.assert true "reported"

@[test]
def absentPolicyIsNotAnError : Test := do
  match parseSpawnPolicy? (Json.mkObj [("prompt", "x")]) with
  | .error e => TestM.fail s!"most tasks have no policy: {e}"
  | .ok p    => TestM.assert p.isNone "absent reads as none"

/-- `Option`'s own `ToJson` writes `null` for a policy-less task, so a document orchestra wrote
    has to read back rather than fail. -/
@[test]
def nullPolicyReadsAsAbsent : Test := do
  match parseSpawnPolicy? (Json.mkObj [("spawn_policy", Json.null)]) with
  | .error e => TestM.fail s!"an explicit null is how 'not set' is serialised: {e}"
  | .ok p    => TestM.assert p.isNone "null reads as none"

@[test]
def unreadablePolicyIsAnError : Test := do
  match parseSpawnPolicy? (Json.mkObj [("spawn_policy", Json.mkObj [("repos", Json.str "acme")])]) with
  | .ok _    => TestM.fail "an unreadable policy must not be swallowed as no policy"
  | .error _ => TestM.assert true "reported"

@[test]
def validateRejectsUnknownTools : Test := do
  let p : SpawnPolicy := { tools := ["work_issues", "rm_rf"] }
  match p.validate Role.knownPermissions with
  | .ok _    => TestM.fail "'rm_rf' is not a tool a role may be granted"
  | .error e => TestM.assert (e.containsSubstr "rm_rf") s!"names the offending tool: {e}"

@[test]
def validateRejectsZeroMaxTasks : Test := do
  match ({ maxTasks := 0 } : SpawnPolicy).validate Role.knownPermissions with
  | .ok _    => TestM.fail "max_tasks 0 refuses every call and is an off switch in disguise"
  | .error e => TestM.assert (e.containsSubstr "max_tasks") s!"names the field: {e}"

@[test]
def validateAcceptsAKnownSet : Test := do
  let p : SpawnPolicy := { tools := ["create_pr", "work_issues"], maxBudget := some 1.0 }
  match p.validate Role.knownPermissions with
  | .ok _    => TestM.assert true "accepted"
  | .error e => TestM.fail s!"every tool here is one a role may hold: {e}"

/-! ## Parsing the tool call -/

@[test]
def parseQueueTaskMinimal : Test := do
  match tryParseToolCall "queue_task" (Json.mkObj [("prompt", "fix the flake")]) with
  | some (.ok (.queueTask r)) =>
    TestM.assertEqual r.prompt "fix the flake" (msg := "prompt")
    TestM.assert r.backend.isNone "backend absent means inherit"
    TestM.assert r.tools.isNone "tools absent means inherit"
    TestM.assert (!r.preClaim) "pre_claim defaults off"
  | other => TestM.fail s!"unexpected parse: {repr other}"

@[test]
def parseQueueTaskFull : Test := do
  let args := Json.mkObj
    [ ("prompt", "implement it")
    , ("backend", "codex")
    , ("model", "opus")
    , ("repo", "acme/widgets")
    , ("tools", Json.arr #["create_pr", "work_issues"])
    , ("budget", Json.num 3)
    , ("issue_id", Json.num 42)
    , ("pre_claim", Json.bool true) ]
  match tryParseToolCall "queue_task" args with
  | some (.ok (.queueTask r)) =>
    TestM.assertEqual r.backend (some "codex") (msg := "backend")
    TestM.assertEqual r.model (some "opus") (msg := "model")
    TestM.assertEqual (r.repo.map (·.toString)) (some "acme/widgets") (msg := "repo")
    TestM.assertEqual r.tools (some ["create_pr", "work_issues"]) (msg := "tools")
    TestM.assertEqual r.issueId (some (⟨42⟩ : Taxis.IssueId)) (msg := "issue_id")
    TestM.assert r.preClaim "pre_claim"
  | other => TestM.fail s!"unexpected parse: {repr other}"

/-- Absent means "inherit mine", so a mistyped field read as absent would queue the task onto
    this task's backend while the agent believed it had chosen another. -/
@[test]
def parseQueueTaskRejectsMistypedBackend : Test := do
  let args := Json.mkObj [("prompt", "x"), ("backend", Json.num 5)]
  match tryParseToolCall "queue_task" args with
  | some (.error _) => TestM.assert true "reported"
  | other => TestM.fail s!"expected an error, got: {repr other}"

@[test]
def parseQueueTaskRejectsBadRepo : Test := do
  let args := Json.mkObj [("prompt", "x"), ("repo", "not-a-repo")]
  match tryParseToolCall "queue_task" args with
  | some (.error _) => TestM.assert true "reported"
  | other => TestM.fail s!"expected an error, got: {repr other}"

@[test]
def parseQueueTaskNeedsAPrompt : Test := do
  match tryParseToolCall "queue_task" (Json.mkObj [("backend", "codex")]) with
  | some (.error _) => TestM.assert true "reported"
  | other => TestM.fail s!"expected an error, got: {repr other}"

/-! ## The tool is offered exactly when it can be answered -/

private def baseState (policy : Option SpawnPolicy) (hook : Bool) : Orchestra.Server.State :=
  { repo := some { upstream := repoOf "acme/widgets", fork := repoOf "acme/widgets" }
  , allowedTools := ["work_issues"]
  , appId := 1, privateKeyPath := "/dev/null", installationId := none, pat := ""
  , spawnPolicy := policy
  , enqueueTask := if hook then some (fun _ _ => return .ok "queued") else none }

private def listsQueueTask (s : Orchestra.Server.State) : Bool :=
  (Orchestra.Server.toolsList s).compress.containsSubstr "\"queue_task\""

@[test]
def queueTaskListedOnlyWithAPolicy : Test := do
  TestM.assert (!listsQueueTask (baseState none true))
    "a task with no policy must not be offered the tool"
  TestM.assert (listsQueueTask (baseState (some {}) true))
    "a task with a policy and a queue to write to is offered it"

/-- Listed but unanswerable is the one state worth ruling out: outside the daemon there is no
    queue, and a tool that is always refused is worse than one that is absent. -/
@[test]
def queueTaskNotListedWithoutTheHook : Test := do
  TestM.assert (!listsQueueTask (baseState (some {}) false))
    "no queue to write to, so the tool is not offered"

/-- The policy is the only switch: naming it in `allowedTools` grants nothing. -/
@[test]
def permissionLabelDoesNotEnableIt : Test := do
  let s := { baseState none true with allowedTools := ["work_issues", "queue_task"] }
  TestM.assert (!listsQueueTask s) "a label is not a policy and must not turn the tool on"

end OrchestraTest.SpawnTask
