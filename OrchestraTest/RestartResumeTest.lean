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
  spawnPolicy := some {}
}

private def record (sessionId : Option String := some "sess-1")
    (status : TaskStore.TaskStatus := .unfinished) : TaskStore.TaskRecord := {
  id := "t-1", createdAt := "2026-10-10T08:00:00Z", status, sessionId
  repo := none, prompt := "fix the flaky test"
}


private def facts : Queue.ResumeFacts := { hasSession := true }

private def resume (e : Queue.QueueEntry := interrupted) (r : TaskStore.TaskRecord := record)
    (f : Queue.ResumeFacts := facts) : Except String Queue.QueueEntry :=
  Queue.resumeEntryFor e r f "q-2" "2026-10-10T09:00:00Z"

private def refused (x : Except String Queue.QueueEntry) : Bool :=
  match x with | .ok _ => false | .error _ => true

@[test]
def aResumeIsTheSameWorkContinuingTheDeadRun : Test := do
  let .ok e := resume | TestM.fail "an interrupted run with a session should be resumed"
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
  TestM.assertEqual e.authSource (some "acct-a") (msg := "what was asked for is still asked for")
  TestM.assertEqual e.authSources ["acct-a", "acct-b"]
  TestM.assert (e.authMode == some .distribute) (msg := "auth mode carried")
  TestM.assertEqual e.tools (some ["comment"])
  TestM.assertEqual e.readOnly true
  TestM.assertEqual e.issueNumber (some 42)
  TestM.assertEqual e.role (some "fixer")
  TestM.assertEqual e.prLabels ["bot"]
  TestM.assert e.spawnPolicy.isSome (msg := "an orchestrating agent keeps queue_task")

@[test]
def aResumeLeavesTheDeadRunsOwnStateBehind : Test := do
  let .ok e := resume | TestM.fail "expected a resume"
  TestM.assertEqual e.resolvedAuthSource none (msg := "the daemon's pick is made again at claim")
  TestM.assertEqual e.prependPrompt none (msg := "already in the conversation")
  TestM.assertEqual e.spawnedBy none (msg := "spends no spawner's allowance")
  TestM.assertEqual e.taskId none (msg := "the new run has no task yet")
  TestM.assertEqual e.slot none (msg := "nor a slot")

@[test]
def onlyAResumableRunIsResumed : Test := do
  TestM.assert (refused (resume (f := { hasSession := false })))
    (msg := "no conversation: nothing to resume, a retry starts over")
  TestM.assert (refused (resume (e := { interrupted with status := .done })))
    (msg := "an entry that landed is not resumed")
  TestM.assert (refused (resume (r := record (status := .completed))))
    (msg := "a run that landed is not resumed")
  TestM.assert (refused (resume (e := { interrupted with taskId := some "t-other" })))
    (msg := "the record has to be the entry's own run")
  TestM.assert (refused (resume (e := { interrupted with concertStepKey := some "k" })))
    (msg := "a concert step's fiber died with the daemon")
  TestM.assert (refused (resume (f := { facts with continued := true })))
    (msg := "something already carries the task on")
  TestM.assert (refused (resume (f := { facts with recent := false })))
    (msg := "a run from days ago is left for a person")
  TestM.assert (refused (resume (f := { facts with sameExecution := false })))
    (msg := "an entry on another execution backend was not reclaimed here")

@[test]
def resumesInARowAreCapped : Test := do
  for run in [0:Queue.maxRestartResumes] do
    TestM.assert (!refused (resume (f := { facts with run })))
      (msg := s!"resumed after {run} resumes in a row")
  TestM.assert (refused (resume (f := { facts with run := Queue.maxRestartResumes })))
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

/-- A resume killed before its agent said anything added nothing to the conversation: the one it
    was resuming is still the one to pick up. A run somebody queued that has none is a fresh start. -/
@[test]
def aResumeKilledEarlyFallsBackToTheConversationItWasResuming : Test := do
  let r := Queue.restartResumePrompt
  let (viaResume, ownSession, handQueued) ← withTempData do
    TaskStore.saveTask { record with id := "t-a", sessionId := some "sess-a", continuesFrom := none }
    TaskStore.saveTask { record with id := "t-b", prompt := r, sessionId := none, continuesFrom := some "t-a" }
    TaskStore.saveTask { record with id := "t-h", prompt := "go on", sessionId := none, continuesFrom := some "t-a" }
    pure (← Queue.sessionFor "t-b", ← Queue.sessionFor "t-a", ← Queue.sessionFor "t-h")
  TestM.assertEqual viaResume (some "sess-a") (msg := "a resume without a session resumes its predecessor's")
  TestM.assertEqual ownSession (some "sess-a") (msg := "a run with a session resumes its own")
  TestM.assertEqual handQueued none (msg := "a hand-queued run without one has nothing to resume")

/-- `orchestra queue retry` leaves out an unfinished run something carries on — but only a
    continuation that is waiting, running or done counts; a cancelled or failed one carried
    nothing on. -/
@[test]
def retrySkipsOnlyWhatALiveContinuationCarriesOn : Test := do
  let e (id : String) (status : Queue.QueueStatus) (taskId cont : Option String)
      (series : Option String := none) : Queue.QueueEntry :=
    { id, createdAt := s!"2026-10-10T08:00:0{id.length}Z", status, taskId, continuesFrom := cont
      repo := none, prompt := "p", series }
  let all := #[
    e "u1" .unfinished (some "t1") none,  e "c1" .pending   none (some "t1"),
    e "u2" .unfinished (some "t2") none,  e "c2" .cancelled none (some "t2"),
    e "u3" .unfinished (some "t3") none,  e "c3" .failed    none (some "t3"),
    e "u4" .unfinished (some "t4") none,  e "c4" .done      (some "t5") (some "t4"),
    e "u6" .unfinished (some "t6") none (series := some "s")]
  let ids := (Queue.retryCandidates all).map (·.id)
  TestM.assert (!ids.contains "u1") (msg := "a pending continuation carries it on")
  TestM.assert (!ids.contains "u4") (msg := "so does one that landed")
  TestM.assert (ids.contains "u2") (msg := "a cancelled one does not")
  TestM.assert (ids.contains "u3") (msg := "nor does a failed one")
  TestM.assert (ids.contains "c2") (msg := "the cancelled continuation is itself retried, as before")
  TestM.assertEqual ((Queue.retryCandidates all (some "s")).map (·.id)) ["u6"]
    (msg := "the series filter still applies")

/-- End to end over a real store: the sweep records what it swept, and the resume decides from
    that record and the database — so it works the same for a daemon that died in between. -/
@[test]
def theSweptEntriesWithASessionAreResumed : Test := do
  let now ← Usage.nowEpoch
  let fresh ← TaskStore.currentIso8601
  let (swept, queued, pending, again, left) ← withTempData do
    Queue.saveEntry { interrupted with id := "q-live", status := .running, taskId := some "t-1", configPath := none }
    Queue.saveEntry { interrupted with id := "q-early", status := .running, taskId := some "t-2", configPath := none }
    Queue.saveEntry { interrupted with id := "q-old", status := .unfinished, taskId := some "t-3", configPath := none }
    Queue.saveEntry { interrupted with id := "q-ancient", status := .running, taskId := some "t-4", configPath := none }
    TaskStore.saveTask { record (status := .running) with createdAt := fresh }
    TaskStore.saveTask { record (sessionId := none) (status := .running) with id := "t-2", createdAt := fresh }
    TaskStore.saveTask { record with id := "t-3", createdAt := fresh }
    TaskStore.saveTask { record (status := .running) with id := "t-4", createdAt := "2020-01-01T00:00:00Z" }
    let swept ← Queue.markStaleRunningAsUnfinished
    let _ ← Queue.reconcileStaleTaskRecords
    -- As if the daemon had died here and another started: the swept list is all that is passed on.
    let queued ← Queue.resumeInterrupted {} now
    let pending := (← Queue.loadAllEntries).filter (·.status == .pending)
    -- A second pass finds nothing to do: the list is cleared, and a resume already queued would
    -- count as a live continuation anyway.
    let again ← Queue.resumeInterrupted {} now
    pure (swept, queued, pending, again, ← Queue.loadInterrupted)
  TestM.assertEqual ((swept.map (·.id)).qsort (· < ·)) #["q-ancient", "q-early", "q-live"]
    (msg := "the sweep answers exactly the entries it turned unfinished")
  TestM.assertEqual queued 1 (msg := "one of them had a session and is recent")
  TestM.assertEqual (pending.map (·.continuesFrom)) #[some "t-1"]
    (msg := "the continuation is of the run that named its conversation; q-old was not swept")
  TestM.assertEqual again 0 (msg := "nothing is queued twice")
  TestM.assertEqual left #[] (msg := "the list is cleared once decided")

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

-- What a starting daemon may remove (`Daemon.isLeftoverStatus`)

@[test]
def onlyWhatThisDatabaseKnowsAndHasStoppedIsALeftover : Test := do
  TestM.assert (Daemon.isLeftoverStatus (some .unfinished) none) (msg := "a swept run")
  TestM.assert (Daemon.isLeftoverStatus (some .completed) none) (msg := "a run that landed")
  TestM.assert (!Daemon.isLeftoverStatus (some .running) none)
    (msg := "a run still going — an orchestra run in the foreground — is not")
  TestM.assert (!Daemon.isLeftoverStatus none none)
    (msg := "an id this database does not know is another daemon's")
  TestM.assert (Daemon.isLeftoverStatus none (some .dormant)) (msg := "a session put to sleep")
  TestM.assert (Daemon.isLeftoverStatus none (some .ended)) (msg := "a session that ended")
  TestM.assert (!Daemon.isLeftoverStatus none (some .running)) (msg := "a live session is not")

end OrchestraTest.RestartResume
