import OrchestraTest.TestM
import Orchestra

open Orchestra

/-!
# Picking up what a restart interrupted

A deploy drains the daemon and then kills it; a task still running loses its agent. On a backend
that keeps workspaces the new daemon queues a continuation of each such task (`Queue.resumeEntryFor`)
instead of leaving it `unfinished` for somebody to retry from scratch. What is pinned here is what
that continuation carries and what it deliberately does not, which interrupted entries qualify at
all, and the guard that stops a task that keeps getting killed from being resumed forever.
-/

namespace OrchestraTest.RestartResume

private def withTempData (act : IO α) : IO α := Orchestra.withTempData "resume" act

/-- An entry the startup sweep just turned `unfinished`, with every field a continuation might
    carry set to something recognisable. -/
private def interrupted : Queue.QueueEntry := {
  id := "q-1", createdAt := "2026-10-10T08:00:00Z", status := .unfinished
  repo := none
  prompt := "fix the flaky test"
  prependPrompt := some "house-rules"
  goal := some "CI is green"
  backend := some "claude", model := some "opus", agent := some "worker"
  systemPrompt := some "careful"
  series := some "flaky", configPath := some "/etc/orchestra/config.json"
  budget := some 12.0, priority := 7
  identity := some "houyi"
  authSource := some "acct-a", resolvedAuthSource := some "acct-a"
  authSources := ["acct-a", "acct-b"], authMode := some .distribute
  tools := some ["comment"], readOnly := true
  taskId := some "t-1", slot := some 2
  issueNumber := some 42, role := some "fixer"
  prLabels := ["bot"]
  spawnedBy := some "t-0"
  continuesFrom := some "t-0"
}

private def record (sessionId : Option String := some "sess-1")
    (status : TaskStore.TaskStatus := .unfinished) : TaskStore.TaskRecord := {
  id := "t-1", createdAt := "2026-10-10T08:00:00Z", status, sessionId
  repo := none, prompt := "fix the flaky test"
}

@[test]
def aResumeIsTheSameWorkContinuingTheDeadRun : Test := do
  let some e := Queue.resumeEntryFor interrupted (record) 0 "q-2" "2026-10-10T09:00:00Z"
    | TestM.fail "an interrupted run with a session should be resumed"
  TestM.assertEqual e.id "q-2"
  TestM.assert (e.status == .pending) (msg := "queued, not running")
  TestM.assertEqual e.continuesFrom (some "t-1") (msg := "continues the dead run, not its predecessor")
  TestM.assert (Queue.isRestartResumePrompt e.prompt) (msg := "carries the restart prompt")
  TestM.assertEqual e.goal (some "CI is green")
  TestM.assertEqual e.backend (some "claude")
  TestM.assertEqual e.model (some "opus")
  TestM.assertEqual e.agent (some "worker")
  TestM.assertEqual e.systemPrompt (some "careful")
  TestM.assertEqual e.series (some "flaky")
  TestM.assertEqual e.configPath (some "/etc/orchestra/config.json")
  TestM.assert (e.budget == some 12.0) (msg := "budget carried")
  TestM.assertEqual e.priority 7
  TestM.assertEqual e.identity (some "houyi")
  TestM.assertEqual e.authSources ["acct-a", "acct-b"]
  TestM.assert (e.authMode == some .distribute) (msg := "auth mode carried")
  TestM.assertEqual e.tools (some ["comment"])
  TestM.assertEqual e.readOnly true
  TestM.assertEqual e.issueNumber (some 42)
  TestM.assertEqual e.role (some "fixer")
  TestM.assertEqual e.prLabels ["bot"]

@[test]
def aResumeDrawsItsAccountAfreshAndLeavesTheDeadRunsStateBehind : Test := do
  let some e := Queue.resumeEntryFor interrupted (record) 0 "q-2" "2026-10-10T09:00:00Z"
    | TestM.fail "expected a resume"
  TestM.assertEqual e.authSource none (msg := "not pinned to the account the dead run drew")
  TestM.assertEqual e.resolvedAuthSource none (msg := "resolved again at claim")
  TestM.assertEqual e.prependPrompt none (msg := "already in the conversation")
  TestM.assertEqual e.spawnedBy none (msg := "spends no spawner's allowance")
  TestM.assertEqual e.taskId none (msg := "the new run has no task yet")
  TestM.assertEqual e.slot none (msg := "nor a slot")

@[test]
def onlyAResumableRunIsResumed : Test := do
  let at_ := "2026-10-10T09:00:00Z"
  TestM.assert (Queue.resumeEntryFor interrupted (record (sessionId := none)) 0 "q-2" at_).isNone
    (msg := "no session id: nothing to resume, a retry starts over")
  TestM.assert (Queue.resumeEntryFor { interrupted with status := .done } (record) 0 "q-2" at_).isNone
    (msg := "an entry that landed is not resumed")
  TestM.assert (Queue.resumeEntryFor interrupted (record (status := .completed)) 0 "q-2" at_).isNone
    (msg := "a run that landed is not resumed")
  TestM.assert (Queue.resumeEntryFor { interrupted with taskId := some "t-other" } (record) 0 "q-2" at_).isNone
    (msg := "the record has to be the entry's own run")
  TestM.assert (Queue.resumeEntryFor { interrupted with concertStepKey := some "k" } (record) 0 "q-2" at_).isNone
    (msg := "a concert step's fiber died with the daemon")

@[test]
def resumesInARowAreCapped : Test := do
  let at_ := "2026-10-10T09:00:00Z"
  for run in [0:Queue.maxRestartResumes] do
    TestM.assert (Queue.resumeEntryFor interrupted (record) run "q-2" at_).isSome
      (msg := s!"resumed after {run} resumes in a row")
  TestM.assert (Queue.resumeEntryFor interrupted (record) Queue.maxRestartResumes "q-2" at_).isNone
    (msg := "not after the cap")

@[test]
def onlyTheUnbrokenRunOfResumesAtTheHeadCounts : Test := do
  let r := Queue.restartResumePrompt
  TestM.assertEqual (Queue.restartResumeRun []) 0
  TestM.assertEqual (Queue.restartResumeRun ["fix it"]) 0 (msg := "a task nobody resumed")
  TestM.assertEqual (Queue.restartResumeRun [r, r, "fix it"]) 2
  TestM.assertEqual (Queue.restartResumeRun [r, "please go on", r, r, "fix it"]) 1
    (msg := "a continuation somebody asked for resets the count")

@[test]
def theChainIsReadThroughTheTaskRecords : Test := do
  let r := Queue.restartResumePrompt
  let prompts ← withTempData do
    TaskStore.saveTask { record with id := "t-a", prompt := "fix it", continuesFrom := none }
    TaskStore.saveTask { record with id := "t-b", prompt := r, continuesFrom := some "t-a" }
    TaskStore.saveTask { record with id := "t-c", prompt := r, continuesFrom := some "t-b" }
    Queue.chainPrompts "t-c"
  TestM.assertEqual prompts [r, r, "fix it"] (msg := "newest first, back to the start")
  TestM.assertEqual (Queue.restartResumeRun prompts) 2

/-- End to end over a real store: the sweep answers what it swept, and only the entry whose run
    named its conversation gets a continuation. -/
@[test]
def theSweptEntriesWithASessionAreResumed : Test := do
  let (swept, queued, pending) ← withTempData do
    Queue.saveEntry { interrupted with id := "q-live", status := .running, taskId := some "t-1" }
    Queue.saveEntry { interrupted with id := "q-early", status := .running, taskId := some "t-2" }
    Queue.saveEntry { interrupted with id := "q-old", status := .unfinished, taskId := some "t-3" }
    TaskStore.saveTask (record (status := .running))
    TaskStore.saveTask { record (sessionId := none) (status := .running) with id := "t-2" }
    TaskStore.saveTask { record with id := "t-3" }
    let swept ← Queue.markStaleRunningAsUnfinished
    let _ ← Queue.reconcileStaleTaskRecords
    let queued ← Queue.resumeInterrupted swept
    let pending := (← Queue.loadAllEntries).filter (·.status == .pending)
    pure (swept, queued, pending)
  TestM.assertEqual ((swept.map (·.id)).qsort (· < ·)) #["q-early", "q-live"]
    (msg := "the sweep answers exactly the entries it turned unfinished")
  TestM.assertEqual queued 1 (msg := "one of them had a session")
  TestM.assertEqual (pending.map (·.continuesFrom)) #[some "t-1"]
    (msg := "the continuation is of the run that named its conversation")

-- `queue.resume_after_restart`

private def parseQueueConfig (s : String) : Except String QueueConfig := do
  Lean.fromJson? (← Lean.Json.parse s)

@[test]
def resumingIsOnByDefaultAndCanBeTurnedOff : Test := do
  match parseQueueConfig "{}", parseQueueConfig "{\"resume_after_restart\": false, \"parallel\": 3}" with
  | .ok d, .ok off =>
    TestM.assertEqual d.resumeAfterRestart true (msg := "on unless said otherwise")
    TestM.assertEqual off.resumeAfterRestart false (msg := "off when said")
    TestM.assertEqual off.parallel 3 (msg := "the other keys are unaffected")
  | _, _ => TestM.fail "expected both to parse"

end OrchestraTest.RestartResume
