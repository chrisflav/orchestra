import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra

namespace OrchestraTest.RepolessTask

/-!
# Repository-independent tasks

Covers the pieces that make a task runnable without a repository: the JSON round-trip that had
to stay backwards compatible, the claim path that must not hand such a task a clone slot, and
the manager dispatcher that spawns them.

The sharp edge guarded here is `Config.getOptObjValAs?`. Making `upstream`/`fork` optional
meant relaxing three `FromJson` instances, and the obvious relaxation (`.toOption`) would have
turned a *malformed* repository string from "entry rejected" into "entry silently runs with no
clone and no credentials". Absent and unparseable must stay distinguishable.
-/

private def repo : Repository := { owner := "o", name := "r" }

private def withRepo : Queue.QueueEntry :=
  { id := "0001", createdAt := "2026-01-01T00:00:00Z"
  , upstream := some repo, fork := some repo, mode := some .pr
  , prompt := "work" }

private def withoutRepo : Queue.QueueEntry :=
  { id := "0002", createdAt := "2026-01-01T00:00:00Z"
  , prompt := "survey the tracker", role := some "manager"
  , tools := some ["manage_all_issues"] }

/-- `Repository` has no `DecidableEq`, so comparisons go through the canonical string. -/
private def repoStr (r : Option Repository) : Option String := r.map (·.toString)

private def modeStr : Option TaskMode → String
  | some .fork => "fork"
  | some .pr   => "pr"
  | none       => "-"

-- JSON round-trip

@[test]
def queueEntryRoundTripsWithARepository : Test := do
  match FromJson.fromJson? (ToJson.toJson withRepo) (α := Queue.QueueEntry) with
  | .error e => TestM.fail s!"QueueEntry round-trip: {e}"
  | .ok got =>
    TestM.assertEqual (repoStr got.upstream) (some "o/r") (msg := "upstream survives")
    TestM.assertEqual (repoStr got.fork) (some "o/r") (msg := "fork survives")
    TestM.assertEqual (modeStr got.mode) "pr" (msg := "and so does mode")

@[test]
def queueEntryRoundTripsWithoutARepository : Test := do
  match FromJson.fromJson? (ToJson.toJson withoutRepo) (α := Queue.QueueEntry) with
  | .error e => TestM.fail s!"QueueEntry round-trip: {e}"
  | .ok got =>
    TestM.assertEqual (repoStr got.upstream) none (msg := "no upstream")
    TestM.assertEqual (repoStr got.fork) none (msg := "no fork")
    TestM.assertEqual (modeStr got.mode) "-" (msg := "and no mode to derive repo tools from")
    TestM.assertEqual got.role (some "manager") (msg := "the rest of the entry is intact")

@[test]
def queueEntryOmitsRepositoryKeysWhenAbsent : Test := do
  -- Not cosmetic: an emitted `"upstream": null` would fail to parse on the way back in.
  let s := (ToJson.toJson withoutRepo).compress
  TestM.assert (!s.containsSubstr "\"upstream\"") "upstream must be omitted when none"
  TestM.assert (!s.containsSubstr "\"fork\"") "fork must be omitted when none"
  TestM.assert (!s.containsSubstr "\"mode\"") "mode must be omitted when none"

@[test]
def queueEntryDecodesLegacyJsonWithARepository : Test := do
  -- Every entry already on disk carries all three keys and must keep working.
  let j := Json.mkObj
    [ ("id", "0004"), ("created_at", "2026-01-01T00:00:00Z")
    , ("status", "pending"), ("prompt", "x")
    , ("upstream", "o/r"), ("fork", "o/r"), ("mode", "pr") ]
  match FromJson.fromJson? j (α := Queue.QueueEntry) with
  | .error e => TestM.fail s!"legacy QueueEntry: {e}"
  | .ok got => TestM.assertEqual (repoStr got.fork) (some "o/r") (msg := "legacy entry decodes")

@[test]
def queueEntryMalformedRepositoryStillFailsToParse : Test := do
  -- The regression: `"o"` has no slash, so `Repository.parse` rejects it. That must stay a
  -- parse failure (the entry is dropped) rather than silently decoding to `none`, which would
  -- promote a typo into a credential-less task.
  let j := Json.mkObj
    [ ("id", "0003"), ("created_at", "2026-01-01T00:00:00Z")
    , ("status", "pending"), ("prompt", "x")
    , ("upstream", "o"), ("fork", "o/r"), ("mode", "pr") ]
  match FromJson.fromJson? j (α := Queue.QueueEntry) with
  | .error _ => TestM.assert true ""
  | .ok _ => TestM.fail "a malformed upstream must not decode to a repository-less entry"

@[test]
def taskRecordRoundTripsWithoutARepository : Test := do
  let r : TaskStore.TaskRecord :=
    { id := "t-1", createdAt := "2026-01-01T00:00:00Z", prompt := "survey" }
  match FromJson.fromJson? (ToJson.toJson r) (α := TaskStore.TaskRecord) with
  | .error e => TestM.fail s!"TaskRecord round-trip: {e}"
  | .ok got =>
    TestM.assertEqual (repoStr got.fork) none (msg := "no fork on the record either")

-- Claim path

private def ctx (parallelLimit perRepoLimit : Nat)
    (occupied : List (String × Array Nat) := []) (total : Nat := 0) : Queue.ClaimContext :=
  { occupiedSlots := Std.HashMap.ofList occupied
    total, exclusiveActive := false, parallelLimit, perRepoLimit
    parallelSafe := TaskRunner.backendIsParallelSafe }

private def noOccupants : Repository → Nat → IO (Option String) := fun _ _ => pure none

@[test]
def claimDecisionRepolessEntryTakesNoSlot : Test := do
  match ← Queue.claimDecision (ctx 4 1) #[withoutRepo] noOccupants with
  | some c =>
    TestM.assertEqual c.slot none (msg := "no clone to run in, so no slot")
    TestM.assertEqual c.entry.id "0002" (msg := "the repo-less entry was claimed")
  | none => TestM.fail "expected a claim"

@[test]
def claimDecisionRepolessEntryIgnoresThePerRepoLimit : Test := do
  -- Repository `o/r` is at its per-repo limit of 1, which blocks the entry bound to it. The
  -- repo-less entry occupies none of those slots, so it must still be admitted.
  match ← Queue.claimDecision (ctx 4 1 [("o/r", #[0])] 1) #[withRepo, withoutRepo] noOccupants with
  | some c =>
    TestM.assertEqual c.entry.id "0002"
      (msg := "the repo-bound entry is blocked; the repo-less one is not")
  | none => TestM.fail "expected the repo-less entry to be claimable"

@[test]
def claimDecisionRepolessEntryStillRespectsParallelLimit : Test := do
  -- The one bound it does obey: it occupies a worker like anything else.
  match ← Queue.claimDecision (ctx 2 1 [] 2) #[withoutRepo] noOccupants with
  | none   => TestM.assert true ""
  | some _ => TestM.fail "at the global parallel limit, nothing starts"

-- Roles

private def managerRole : Project.Role :=
  { name := "manager", permissions := ["manage_all_issues"]
  , promptTemplate := "survey", repoless := true
  , dispatch := some { trigger := .always } }

@[test]
def roleRepolessRoundTrips : Test := do
  match FromJson.fromJson? (ToJson.toJson managerRole) (α := Project.Role) with
  | .error e => TestM.fail s!"Role round-trip: {e}"
  | .ok r => TestM.assertEqual r.repoless true (msg := "repoless survives the round trip")

@[test]
def roleRepolessDefaultsToFalse : Test := do
  -- Every existing role file must keep parsing byte-for-byte.
  let j := Json.mkObj
    [ ("name", "implementor"), ("permissions", Json.arr #["work_issues"])
    , ("prompt_template", "do it") ]
  match FromJson.fromJson? j (α := Project.Role) with
  | .error e => TestM.fail s!"legacy Role: {e}"
  | .ok r => TestM.assertEqual r.repoless false (msg := "absent means repository-bound")

@[test]
def roleOmitsRepolessWhenFalse : Test := do
  let plain : Project.Role :=
    { name := "implementor", permissions := ["work_issues"], promptTemplate := "do it" }
  TestM.assert (!(ToJson.toJson plain).compress.containsSubstr "\"repoless\"")
    "repoless must be omitted when false, so existing role files round-trip unchanged"

-- Manager dispatcher

@[test]
def dispatcherAlwaysRoleSpawnsWithNoIssues : Test := do
  -- The manager dispatcher hands `dispatcherDecisions` empty issue sets; only `always` fires.
  let input : Listener.DispatcherInput :=
    { activeByRole := {}, issues := #[], reviewable := #[]
    , caps := [("manager", 1)], roles := #[managerRole] }
  let spawns := Listener.dispatcherTick input
  TestM.assertEqual spawns.size 1 (msg := "one manager spawned")
  match spawns[0]? with
  | some s => TestM.assertEqual (s.issueId.map (·.toString)) none (msg := "bound to no issue")
  | none   => TestM.fail "expected a spawn"

@[test]
def dispatcherManagerAtCapDoesNotRespawn : Test := do
  let input : Listener.DispatcherInput :=
    { activeByRole := Std.HashMap.ofList [("manager", 1)]
    , issues := #[], reviewable := #[]
    , caps := [("manager", 1)], roles := #[managerRole] }
  TestM.assertEqual (Listener.dispatcherTick input).size 0
    (msg := "the cap is what paces the manager between ticks")

@[test]
def splitCapsByBindingManagerIsUnbound : Test := do
  -- What the manager dispatcher uses to reject triggers that need an issue set.
  let planner : Project.Role :=
    { name := "planner", permissions := [], promptTemplate := ""
    , dispatch := some { trigger := .idle } }
  let (bound, unbound) :=
    Listener.splitCapsByBinding #[managerRole, planner] [("manager", 1), ("planner", 1)]
  TestM.assertEqual (bound.map (·.1)) ["planner"] (msg := "idle needs an issue set")
  TestM.assertEqual (unbound.map (·.1)) ["manager"] (msg := "always does not")

@[test]
def buildManagerEntryCarriesNoRepositoryOrProject : Test := do
  let e ← Listener.buildManagerEntry managerRole
  TestM.assertEqual (repoStr e.upstream) none (msg := "no upstream")
  TestM.assertEqual (repoStr e.fork) none (msg := "no fork")
  TestM.assertEqual (modeStr e.mode) "-" (msg := "no mode")
  TestM.assertEqual (e.projectId.map (·.val)) none
    (msg := "no project — its absence is what the dispatcher counts")
  TestM.assertEqual (e.issueId.map (·.val)) none (msg := "and no issue")
  TestM.assertEqual e.role (some "manager") (msg := "tagged with its role for the tally")
  TestM.assertEqual e.tools (some ["manage_all_issues"]) (msg := "tracker-wide issue writes")

@[test]
def sourceConfigManagerDispatcherRoundTrips : Test := do
  let src := Listener.SourceConfig.managerDispatcher [("manager", 1)]
  match FromJson.fromJson? (ToJson.toJson src) (α := Listener.SourceConfig) with
  | .error e => TestM.fail s!"SourceConfig round-trip: {e}"
  | .ok (.managerDispatcher caps) =>
    TestM.assertEqual caps [("manager", 1)] (msg := "caps survive the round trip")
  | .ok _ => TestM.fail "parsed as the wrong source type"

-- Permission expansion

@[test]
def expandPermsManageAllImpliesManage : Test := do
  TestM.assertEqual (Project.Tools.expandPerms ["manage_all_issues"])
    ["manage_issues", "manage_all_issues"]
    (msg := "the same toolset is advertised; only the write scope differs")

@[test]
def expandPermsLeavesOtherPermissionsAlone : Test := do
  TestM.assertEqual (Project.Tools.expandPerms ["work_issues"]) ["work_issues"]
    (msg := "no implication to add")
  TestM.assertEqual (Project.Tools.expandPerms ["manage_issues", "manage_all_issues"])
    ["manage_issues", "manage_all_issues"]
    (msg := "already present, so not duplicated")
