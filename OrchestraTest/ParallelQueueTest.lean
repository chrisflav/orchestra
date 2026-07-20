import OrchestraTest.TestM
import Orchestra.Queue
import Orchestra.Repo
import Orchestra.TaskRunner

open Orchestra

/-!
# Parallel queue mode

Covers the pieces of `--parallel` / `--parallel-per-repo` that are pure enough to test
without a daemon: slot selection, per-repo admission, backend exclusivity, and the id
generator the whole scheme relies on for not colliding across workers.
-/

-- Queue.chooseSlot

@[test]
def chooseSlot_freshTakesLowestFreeSlot : Test := do
  match Queue.chooseSlot #[] 3 none with
  | some (slot, reuse) =>
    TestM.assertEqual slot 0 (msg := "lowest free slot")
    TestM.assertEqual reuse false (msg := "a fresh task resets its tree")
  | none => TestM.fail "expected a slot"

@[test]
def chooseSlot_skipsOccupiedSlots : Test := do
  match Queue.chooseSlot #[0, 2] 4 none with
  | some (slot, _) => TestM.assertEqual slot 1 (msg := "lowest free index, not lowest overall")
  | none => TestM.fail "expected a slot"

@[test]
def chooseSlot_noneWhenRepoIsFull : Test := do
  TestM.assertEqual (Queue.chooseSlot #[0, 1] 2 none) none
    (msg := "a repo at its per-repo limit admits nothing")

@[test]
def chooseSlot_continuationReturnsToItsOwnSlot : Test := do
  -- Slot 1 is free and is where the predecessor ran, even though slot 0 is also free:
  -- the tree the resumed agent expects lives in 1.
  match Queue.chooseSlot #[] 3 (some 1) with
  | some (slot, reuse) =>
    TestM.assertEqual slot 1 (msg := "predecessor's slot")
    TestM.assertEqual reuse true (msg := "continuation keeps the working tree")
  | none => TestM.fail "expected a slot"

@[test]
def chooseSlot_continuationWaitsWhenItsSlotIsBusy : Test := do
  -- Slot 0 is free, but taking it would hand the resumed agent a wiped tree. Better to wait.
  TestM.assertEqual (Queue.chooseSlot #[1] 3 (some 1)) none
    (msg := "a continuation waits for its own slot rather than taking a different one")

@[test]
def chooseSlot_continuationFallsBackWhenLimitShrank : Test := do
  -- Daemon restarted with --parallel-per-repo 2, but the predecessor ran in slot 5.
  match Queue.chooseSlot #[] 2 (some 5) with
  | some (slot, reuse) =>
    TestM.assertEqual slot 0 (msg := "falls back to a reachable slot")
    TestM.assertEqual reuse false (msg := "and resets, since the wanted tree is unreachable")
  | none => TestM.fail "expected a slot"

@[test]
def chooseSlot_serialModeAlwaysUsesSlotZero : Test := do
  match Queue.chooseSlot #[] 1 none with
  | some (slot, _) => TestM.assertEqual slot 0 (msg := "default --parallel-per-repo 1")
  | none => TestM.fail "expected a slot"

-- Queue.pendingCandidates

private def mkEntry (id : String) (fork : String) (priority : Nat := 10)
    (status : Queue.QueueStatus := .pending) : Queue.QueueEntry :=
  let repo : Repository := { owner := fork, name := "r" }
  { id, createdAt := "2026-01-01T00:00:00Z", status
  , upstream := some repo, fork := some repo, mode := some .pr, prompt := "", priority }

@[test]
def pendingCandidates_ordersByPriorityThenAge : Test := do
  let all := #[ mkEntry "0003" "a" (priority := 10)
              , mkEntry "0001" "b" (priority := 10)
              , mkEntry "0002" "c" (priority := 50) ]
  let got := (Queue.pendingCandidates all ∅ 1).map (·.id)
  -- Highest priority first; within a priority the smaller (older) id first.
  TestM.assertEqual got.toList ["0002", "0001", "0003"] (msg := "claim order")

@[test]
def pendingCandidates_excludesNonPending : Test := do
  let all := #[ mkEntry "0001" "a" (status := .running)
              , mkEntry "0002" "b" (status := .done)
              , mkEntry "0003" "c" (status := .pending) ]
  let got := (Queue.pendingCandidates all ∅ 1).map (·.id)
  TestM.assertEqual got.toList ["0003"] (msg := "only pending entries are claimable")

@[test]
def pendingCandidates_respectsPerRepoLimit : Test := do
  let all := #[mkEntry "0001" "busy", mkEntry "0002" "free"]
  let counts : Std.HashMap String Nat := (∅ : Std.HashMap String Nat).insert "busy/r" 1
  -- At limit 1 the busy repo is excluded; at limit 2 it is admitted again.
  TestM.assertEqual ((Queue.pendingCandidates all counts 1).map (·.id)).toList ["0002"]
    (msg := "repo at its limit is skipped")
  TestM.assertEqual ((Queue.pendingCandidates all counts 2).map (·.id)).toList ["0001", "0002"]
    (msg := "raising the limit admits it")

-- Queue.claimDecision

/-- A context with nothing running and generous limits; tests override what they exercise. -/
private def ctx (parallelLimit perRepoLimit : Nat)
    (occupied : List (String × Array Nat) := []) (total : Nat := 0)
    (exclusiveActive : Bool := false) : Queue.ClaimContext :=
  { occupiedSlots := Std.HashMap.ofList occupied
    total, exclusiveActive, parallelLimit, perRepoLimit
    parallelSafe := TaskRunner.backendIsParallelSafe }

/-- No slot anywhere has a recorded occupant. -/
private def noOccupants : Repository → Nat → IO (Option String) := fun _ _ => pure none

/-- Slot `slot` of any repository holds the tree `owner` left behind. -/
private def occupantIs (slot : Nat) (owner : String) : Repository → Nat → IO (Option String) :=
  fun _ s => pure (if s == slot then some owner else none)

@[test]
def claimDecision_stopsAtTheGlobalLimit : Test := do
  let all := #[mkEntry "0001" "a"]
  let got ← Queue.claimDecision (ctx 2 1 (total := 2)) all noOccupants
  TestM.assertEqual (got.map (·.entry.id)) none (msg := "nothing starts once --parallel is full")

@[test]
def claimDecision_nothingStartsBesideAnExclusiveTask : Test := do
  let all := #[mkEntry "0001" "a"]
  let got ← Queue.claimDecision (ctx 4 1 (total := 1) (exclusiveActive := true)) all noOccupants
  TestM.assertEqual (got.map (·.entry.id)) none (msg := "an exclusive backend holds the daemon")

@[test]
def claimDecision_exclusiveBackendIsSkippedWhileBusy : Test := do
  -- The `pi` entry is older and would win on ordering, but it needs an idle daemon. Skipping
  -- past it — rather than giving up for the round — is what keeps repo `b` moving.
  let all := #[ { mkEntry "0001" "a" with backend := some "pi" }
              , mkEntry "0002" "b" ]
  let got ← Queue.claimDecision (ctx 4 1 (occupied := [("a/r", #[])]) (total := 1)) all noOccupants
  TestM.assertEqual (got.map (·.entry.id)) (some "0002")
    (msg := "the blocked exclusive entry does not stall the queue behind it")

@[test]
def claimDecision_exclusiveBackendStartsWhenIdle : Test := do
  let all := #[{ mkEntry "0001" "a" with backend := some "pi" }]
  let got ← Queue.claimDecision (ctx 4 1) all noOccupants
  TestM.assertEqual (got.map (·.entry.id)) (some "0001") (msg := "idle daemon admits it")

@[test]
def claimDecision_continuationResumesItsPredecessorsTree : Test := do
  let pred := { mkEntry "0001" "a" (status := .done) with
                taskId := some "t1", slot := some 1 }
  let cont := { mkEntry "0002" "a" with continuesFrom := some "t1" }
  let got ← Queue.claimDecision (ctx 4 3) #[pred, cont] (occupantIs 1 "0001")
  match got with
  | some c =>
    TestM.assertEqual c.slot (some 1) (msg := "back to the predecessor's slot")
    TestM.assertEqual c.resumeFrom (some "0001") (msg := "and keeps its working tree")
  | none => TestM.fail "expected a claim"

@[test]
def claimDecision_continuationResetsWhenAnotherTaskTookItsSlot : Test := do
  -- The regression this guards: slot 1 is *free*, and the predecessor recorded slot 1, but an
  -- unrelated task has since occupied and reset it. Resuming onto that tree would hand the
  -- agent someone else's branch and edits while its restored session describes work that is
  -- gone — worse than the clean checkout it gets instead.
  let pred := { mkEntry "0001" "a" (status := .done) with
                taskId := some "t1", slot := some 1 }
  let cont := { mkEntry "0002" "a" with continuesFrom := some "t1" }
  let got ← Queue.claimDecision (ctx 4 3) #[pred, cont] (occupantIs 1 "0009")
  match got with
  | some c =>
    TestM.assertEqual c.resumeFrom none (msg := "the tree is not the predecessor's, so reset")
    TestM.assertEqual c.slot (some 0) (msg := "and there is no reason to wait for slot 1")
  | none => TestM.fail "expected a claim, not a wait"

@[test]
def claimDecision_continuationWaitsWhenItsSlotIsBusy : Test := do
  -- Slot 0 is free, but the tree the resumed agent needs is in slot 1, which is running.
  let pred := { mkEntry "0001" "a" (status := .done) with
                taskId := some "t1", slot := some 1 }
  let cont := { mkEntry "0002" "a" with continuesFrom := some "t1" }
  let got ← Queue.claimDecision (ctx 4 3 (occupied := [("a/r", #[1])]) (total := 1))
    #[pred, cont] (occupantIs 1 "0001")
  TestM.assertEqual (got.map (·.entry.id)) none (msg := "waits for its own workspace")

@[test]
def claimDecision_blockedContinuationDoesNotStallOtherRepos : Test := do
  let pred := { mkEntry "0001" "a" (status := .done) with
                taskId := some "t1", slot := some 1 }
  let cont := { mkEntry "0002" "a" (priority := 90) with continuesFrom := some "t1" }
  let other := mkEntry "0003" "b" (priority := 10)
  let got ← Queue.claimDecision (ctx 4 3 (occupied := [("a/r", #[1])]) (total := 1))
    #[pred, cont, other] (occupantIs 1 "0001")
  TestM.assertEqual (got.map (·.entry.id)) (some "0003")
    (msg := "a higher-priority entry waiting on its slot is skipped, not blocking")

@[test]
def claimDecision_continuationWithNoRecordedSlotJustResets : Test := do
  -- Entries queued before slots existed carry no `slot`, so there is no workspace to return
  -- to. Starting cleanly is all that is left; the daemon logs a note.
  let pred := { mkEntry "0001" "a" (status := .done) with taskId := some "t1" }
  let cont := { mkEntry "0002" "a" with continuesFrom := some "t1" }
  let got ← Queue.claimDecision (ctx 4 3) #[pred, cont] noOccupants
  match got with
  | some c => TestM.assertEqual c.resumeFrom none (msg := "no predecessor slot, no resume")
  | none   => TestM.fail "expected a claim"

-- QueueConfig: the `queue` block in config.json

private def parseQueueConfig (s : String) : Except String QueueConfig := do
  Lean.fromJson? (← Lean.Json.parse s)

@[test]
def queueConfig_defaultsToSerial : Test := do
  match parseQueueConfig "{}" with
  | .ok c =>
    TestM.assertEqual c.parallel 1 (msg := "an empty queue block is the old serial behaviour")
    TestM.assertEqual c.parallelPerRepo 1 (msg := "per-repo too")
  | .error e => TestM.fail s!"expected a parse, got {e}"

@[test]
def queueConfig_readsBothKeys : Test := do
  match parseQueueConfig "{\"parallel\": 4, \"parallel_per_repo\": 2}" with
  | .ok c =>
    TestM.assertEqual c.parallel 4 (msg := "parallel")
    TestM.assertEqual c.parallelPerRepo 2 (msg := "parallel_per_repo")
  | .error e => TestM.fail s!"expected a parse, got {e}"

@[test]
def queueConfig_keysAreIndependentlyOptional : Test := do
  -- Setting only the global limit must not silently reset the per-repo one, and vice versa.
  match parseQueueConfig "{\"parallel\": 6}" with
  | .ok c =>
    TestM.assertEqual c.parallel 6 (msg := "the key that was set")
    TestM.assertEqual c.parallelPerRepo 1 (msg := "the one that was not keeps its default")
  | .error e => TestM.fail s!"expected a parse, got {e}"

@[test]
def queueConfig_zeroClampsToOne : Test := do
  -- A daemon with zero workers would poll forever and run nothing, which reads as a hang.
  match parseQueueConfig "{\"parallel\": 0, \"parallel_per_repo\": 0}" with
  | .ok c =>
    TestM.assertEqual c.parallel 1 (msg := "parallel clamps up")
    TestM.assertEqual c.parallelPerRepo 1 (msg := "parallel_per_repo clamps up")
  | .error e => TestM.fail s!"expected a parse, got {e}"

@[test]
def appConfig_withoutQueueBlockStillParses : Test := do
  -- Every existing config.json predates this block; none of them may start failing to load.
  let json := "{\"github_app\": {\"app_id\": 1, \"private_key_path\": \"/k.pem\"}}"
  match (Lean.Json.parse json >>= fun j => (Lean.fromJson? j : Except String AppConfig)) with
  | .ok c => TestM.assertEqual c.queue.parallel 1 (msg := "absent queue block is the default")
  | .error e => TestM.fail s!"expected a parse, got {e}"

-- Repo.slotBaseName

@[test]
def slotBaseName_parsesSlotDirectories : Test := do
  TestM.assertEqual (Repo.slotBaseName "mathlib4-slot-2") (some "mathlib4") (msg := "plain")
  TestM.assertEqual (Repo.slotBaseName "mathlib4-slot-13") (some "mathlib4") (msg := "two digits")
  TestM.assertEqual (Repo.slotBaseName "mathlib4") none (msg := "the cache clone is not a slot")
  -- A repository whose own name contains the separator: only a trailing numeric index counts.
  TestM.assertEqual (Repo.slotBaseName "my-slot-repo") none
    (msg := "a repo genuinely named my-slot-repo is not slot 'repo' of 'my'")
  TestM.assertEqual (Repo.slotBaseName "my-slot-repo-slot-3") (some "my-slot-repo")
    (msg := "and its own slots still parse")
  TestM.assertEqual (Repo.slotBaseName "repo-slot-") none (msg := "no index")
  TestM.assertEqual (Repo.slotBaseName "repo-slot-x") none (msg := "non-numeric index")

-- Backend exclusivity

@[test]
def backendParallelSafety : Test := do
  TestM.assertEqual (TaskRunner.backendIsParallelSafe none) true
    (msg := "the default backend (claude) is parallel-safe")
  TestM.assertEqual (TaskRunner.backendIsParallelSafe (some "claude")) true
    (msg := "claude")
  TestM.assertEqual (TaskRunner.backendIsParallelSafe (some "vibe")) true
    (msg := "vibe uses a per-run VIBE_HOME")
  -- Both of these keep per-run state at a fixed global path; see their AgentDef comments.
  TestM.assertEqual (TaskRunner.backendIsParallelSafe (some "pi")) false
    (msg := "pi overwrites ~/.pi/agent/mcp.json")
  TestM.assertEqual (TaskRunner.backendIsParallelSafe (some "opencode")) false
    (msg := "opencode hard-codes port 4096")
  -- Non-agent backends never launch a sandbox, so they are safe regardless of the fallback.
  TestM.assertEqual (TaskRunner.backendIsParallelSafe (some "merger")) true
    (msg := "merger")
  TestM.assertEqual (TaskRunner.backendIsParallelSafe (some "triage")) true
    (msg := "triage")

-- uniqueToken: the identifier every per-task file path is derived from

@[test]
def uniqueToken_isUniqueWithinANanosecond : Test := do
  -- The point of the counter: a bare nanosecond clock hands two workers the same value, and
  -- task records, log files and temp dirs are all named from it.
  let mut seen : Std.HashSet String := ∅
  for _ in List.range 2000 do
    seen := seen.insert (← uniqueToken)
  TestM.assertEqual seen.size 2000 (msg := "2000 tokens, no collisions")

@[test]
def uniqueToken_hasFixedWidth : Test := do
  let a ← uniqueToken
  let b ← uniqueToken
  TestM.assertEqual a.length 20 (msg := "16 digits of nanos + 4 hex of counter")
  TestM.assertEqual a.length b.length (msg := "width is stable")

@[test]
def uniqueToken_sortsChronologically : Test := do
  -- The queue sorts entries by id string and picks the oldest at a given priority, so
  -- lexicographic order has to agree with issue order.
  let a ← uniqueToken
  let b ← uniqueToken
  let c ← uniqueToken
  TestM.assert (a < b && b < c) (msg := s!"expected increasing ids, got {a}, {b}, {c}")
