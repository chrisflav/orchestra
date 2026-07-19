import OrchestraTest.TestM
import Orchestra.Queue
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
  , upstream := repo, fork := repo, mode := .pr, prompt := "", priority }

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
