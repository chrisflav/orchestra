import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra

/-!
# Repository-independent tasks

A task without an `upstream`/`fork` pair runs in the sandbox with no checkout: an empty scratch
workspace instead of a clone slot, and none of the tools that act on a repository. It is the
shape meta-work takes — coordinating issues across projects on the taxis tracker, maintenance
that belongs to no one repository.

Three things have to hold, and all three are checked here without a network or a filesystem:

* absence is *only* ever the whole pair. Half of one is a config with a line missing, and
  reading it as "no repository" would send a task written to open a pull request into an empty
  directory;
* absence survives every hop — task file, queue entry, task record — so a daemon restart does
  not turn a repository-independent entry into an unparseable one, or into somebody's repo; and
* a repository-independent task cannot reach a repository anyway: the MCP server refuses the
  repository-scoped tools whatever the task file asked for.

The workspace pooling itself is checked through `Queue.claimDecision`, which is where a
repository-independent entry meets `--parallel-per-repo`.
-/

namespace OrchestraTest.Repoless

private def repo : RepoPair :=
  { upstream := { owner := "up", name := "widget" }, fork := { owner := "me", name := "widget" } }

/-! ## Reading a repository pair -/

@[test]
def bothKeysAbsentIsARepositoryIndependentTask : Test := do
  let j := Json.mkObj [("prompt", "tidy the tracker")]
  match (FromJson.fromJson? j : Except String Task) with
  | .error e => TestM.fail s!"a task without repositories should parse: {e}"
  | .ok t =>
    TestM.assert t.ioTask.repo.isNone (msg := "no upstream/fork ⇒ no repository")
    -- `mode` had to become optional along with this: a task with no repository has no meaningful
    -- answer for it, and the documented task-file example never carried one either.
    TestM.assert (t.ioTask.mode matches .fork) (msg := "an absent mode grants nothing")

@[test]
def bothKeysPresentIsRead : Test := do
  let j := Json.mkObj [("upstream", "up/widget"), ("fork", "me/widget"), ("prompt", "x")]
  match (FromJson.fromJson? j : Except String Task) with
  | .error e => TestM.fail s!"expected a repository pair: {e}"
  | .ok t    =>
    TestM.assertEqual (t.ioTask.repo.map (·.upstream.toString)) (some "up/widget")
      (msg := "the upstream is read")
    TestM.assertEqual (t.ioTask.repo.map (·.fork.toString)) (some "me/widget")
      (msg := "the fork is read")

@[test]
def halfAPairIsRejected : Test := do
  -- Loudly, in both directions. The silent reading is "no repository", which is the one answer
  -- that cannot be right for a config that named a repository.
  for (name, j) in [("upstream alone", Json.mkObj [("upstream", "up/widget"), ("prompt", "x")]),
                    ("fork alone",     Json.mkObj [("fork", "me/widget"),     ("prompt", "x")])] do
    match (FromJson.fromJson? j : Except String Task) with
    | .ok _    => TestM.fail s!"{name}: half a repository pair was accepted"
    | .error e =>
      TestM.assert ((e.splitOn "both repositories").length == 2)
        (msg := s!"{name}: the error should say a task names both or neither, got {e}")

@[test]
def aMalformedModeIsStillRejected : Test := do
  -- `mode` became optional so that a task with no repository need not answer for it. Optional is
  -- not the same as ignored: `"PR"` read as `fork` would leave a task that was written to open a
  -- pull request holding no tools, and the deprecation warning does not fire for `fork`, so
  -- nothing on the way would say why.
  for bad in [Json.str "PR", Json.str "", Json.num 1, Json.null] do
    let j := Json.mkObj [("upstream", "up/widget"), ("fork", "me/widget"), ("prompt", "x"),
                         ("mode", bad)]
    match (FromJson.fromJson? j : Except String Task) with
    | .ok t    => TestM.fail s!"'mode': {bad.compress} was accepted as {repr t.ioTask.mode}"
    | .error _ => TestM.assert true

/-! ## Surviving the hops -/

private def entryWithout : Queue.QueueEntry :=
  { id := "e1", createdAt := "2026-01-01T00:00:00Z", repo := none, prompt := "tidy the tracker" }

private def recordWithout : TaskStore.TaskRecord :=
  { id := "t1", createdAt := "2026-01-01T00:00:00Z", repo := none, prompt := "tidy the tracker" }

@[test]
def queueEntryRoundTripsWithoutARepository : Test := do
  let j := ToJson.toJson entryWithout
  TestM.assert (j.getObjVal? "upstream" |>.toOption |>.isNone)
    (msg := "no repository ⇒ no 'upstream' key, rather than a null one")
  TestM.assert (j.getObjVal? "fork" |>.toOption |>.isNone) (msg := "…and no 'fork' key")
  match (FromJson.fromJson? j : Except String Queue.QueueEntry) with
  | .error e => TestM.fail s!"queue entry should decode: {e}"
  | .ok e    => TestM.assert e.repo.isNone (msg := "queue entry stays repository-independent")

@[test]
def taskRecordRoundTripsWithoutARepository : Test := do
  let j := ToJson.toJson recordWithout
  match (FromJson.fromJson? j : Except String TaskStore.TaskRecord) with
  | .error e => TestM.fail s!"task record should decode: {e}"
  | .ok r    => TestM.assert r.repo.isNone (msg := "task record stays repository-independent")

@[test]
def aRepositoryStillRoundTrips : Test := do
  -- The pair is written as two keys, exactly as before, so an entry queued by an older build
  -- still reads and a newer one still reads on an older build.
  let j := ToJson.toJson { entryWithout with repo := some repo }
  TestM.assertEqual (j.getObjValAs? String "upstream" |>.toOption) (some "up/widget")
    (msg := "'upstream' is still written as owner/repo")
  match (FromJson.fromJson? j : Except String Queue.QueueEntry) with
  | .error e => TestM.fail s!"queue entry should decode: {e}"
  | .ok e    => TestM.assertEqual (e.repo.map (·.fork.toString)) (some "me/widget")
                                  (msg := "the pair survives the round trip")

/-! ## Slot pooling -/

@[test]
def repositoryIndependentEntriesShareOnePool : Test := do
  let a := { entryWithout with id := "0001" }
  let b := { entryWithout with id := "0002" }
  TestM.assertEqual a.slotKey b.slotKey (msg := "every repository-independent entry, one pool")
  TestM.assert (!a.slotKey.contains '/')
    (msg := "the pool key carries no '/', so no owner/repo can collide with it")
  let withRepo := { entryWithout with repo := some repo }
  TestM.assertEqual withRepo.slotKey "me/widget" (msg := "a repository pools under its fork")

@[test]
def theWorkspacePoolIsBoundedLikeARepository : Test := do
  -- `--parallel-per-repo 1` holds the second repository-independent entry back, exactly as it
  -- would a second task on one repository: they share a workspace pool of that size.
  let all := #[{ entryWithout with id := "0001", status := .running }
             , { entryWithout with id := "0002" }]
  let ctx : Queue.ClaimContext :=
    { occupiedSlots := Std.HashMap.ofList [(entryWithout.slotKey, #[0])]
      total := 1, exclusiveActive := false, parallelLimit := 4, perRepoLimit := 1
      parallelSafe := fun _ => true }
  let got ← Queue.claimDecision ctx all (fun _ _ => pure none)
  TestM.assertEqual (got.map (·.entry.id)) none
    (msg := "the workspace pool is full, so nothing else starts in it")
  let ctx' := { ctx with perRepoLimit := 2 }
  let got' ← Queue.claimDecision ctx' all (fun _ _ => pure none)
  TestM.assertEqual (got'.map (·.entry.id)) (some "0002")
    (msg := "a second slot lets the waiting entry start")
  TestM.assertEqual (got'.map (·.slot)) (some 1) (msg := "…in the free slot, not the busy one")

@[test]
def aContinuationAsksForTheWorkspaceItsPredecessorLeft : Test := do
  -- Same rule as a clone slot: the agent's restored session describes files it wrote, so it goes
  -- back to the directory holding them — and only when that directory still holds them. Here the
  -- occupant is consulted with `none` for the repository, which is the repository-independent
  -- pool's spelling.
  let pred := { entryWithout with
                id := "0001", taskId := some "task-1", slot := some 3, status := .done }
  let cont := { entryWithout with id := "0002", continuesFrom := some "task-1" }
  let ctx : Queue.ClaimContext :=
    { occupiedSlots := Std.HashMap.ofList [], total := 0, exclusiveActive := false
      parallelLimit := 4, perRepoLimit := 4, parallelSafe := fun _ => true }
  let occupant : Option Repository → Nat → IO (Option String) :=
    fun fork slot => pure (if fork.isNone && slot == 3 then some "0001" else none)
  let got ← Queue.claimDecision ctx #[pred, cont] occupant
  TestM.assertEqual (got.map (·.slot)) (some 3) (msg := "back to the predecessor's workspace")
  TestM.assertEqual (got.bind (·.resumeFrom)) (some "0001") (msg := "and it is kept, not emptied")

/-! ## The scratch workspace -/

@[test]
def aWorkspaceIsEmptiedBetweenTasksAndKeptForAContinuation : Test := do
  -- The whole point of the occupant marker, on the path that has no slot record to consult: an
  -- unrelated run finds the directory and empties it, and the run continuing the task that left
  -- it gets the files back. Exercised through the ad-hoc workspace because that is the one
  -- `orchestra run` and `orchestra interactive` share.
  let scratch ← Repo.ensureAdhocWorkspace (occupant := some "task-1")
  IO.FS.writeFile (scratch / "notes.md") "half-finished"
  -- A continuation of `task-1` keeps what `task-1` wrote.
  let resumed ← Repo.ensureAdhocWorkspace (occupant := some "task-2") (resumeFrom := some "task-1")
  TestM.assert (← (resumed / "notes.md").pathExists)
    (msg := "a continuation finds the files its session refers to")
  -- An unrelated run does not.
  let fresh ← Repo.ensureAdhocWorkspace (occupant := some "task-3")
  TestM.assert (!(← (fresh / "notes.md").pathExists))
    (msg := "an unrelated task starts in an empty directory")
  -- Nor does a continuation whose predecessor's files are gone: `task-3` holds it now.
  IO.FS.writeFile (fresh / "other.md") "someone else's"
  let stale ← Repo.ensureAdhocWorkspace (occupant := some "task-4") (resumeFrom := some "task-1")
  TestM.assert (!(← (stale / "other.md").pathExists))
    (msg := "a continuation is not resumed onto a tree its predecessor never left")
  IO.FS.removeDirAll stale

/-! ## Listeners -/

@[test]
def aListenerWithNoRepositoriesQueuesATaskWithout : Test := do
  -- A `shell` source supplies no `{{upstream}}`/`{{fork}}`, so an action that names neither has
  -- none to give. Before, that reached `Repository.parse ""` and threw.
  let action : Listener.ActionConfig := { promptTemplate := "{{output}}" }
  let entry ← Listener.buildQueueEntry action [("output", "three projects are blocked")]
  TestM.assert entry.repo.isNone (msg := "no repository named anywhere ⇒ none on the entry")
  TestM.assertEqual entry.prompt "three projects are blocked" (msg := "the prompt still renders")

@[test]
def aListenerStillTakesTheRepositoriesItsEventCarries : Test := do
  -- The fallback path every GitHub source uses: the action names no repositories itself, and the
  -- event's variables supply them. This must keep working, or the change above would quietly
  -- turn every such listener repository-independent.
  let action : Listener.ActionConfig := { promptTemplate := "x" }
  let entry ← Listener.buildQueueEntry action [("upstream", "up/widget"), ("fork", "me/widget")]
  TestM.assertEqual (entry.repo.map (·.fork.toString)) (some "me/widget")
    (msg := "the event's fork is used")
  TestM.assertEqual (entry.repo.map (·.upstream.toString)) (some "up/widget")
    (msg := "…and its upstream")

/-! ## Tools -/

@[test]
def theRepositoryScopedToolsAreRefusedWithoutOne : Test := do
  -- `TaskRunner` drops these from the task's tool list and `tools/list` hides whatever survives
  -- that, so this is the backstop: a tool that is merely unlisted is not one an agent cannot
  -- name. Granting them here is deliberate — it is the hand-written task file this guards.
  let state : Server.State :=
    { repo := none, allowedTools := Server.repoScopedTools ++ ["get_pr_comments"]
    , appId := 0, privateKeyPath := "", installationId := none, pat := "pat"
    , issueNumber := some 7 }
  let calls : List (String × Server.ToolCall) :=
    [ ("create_pr",       .createPr "t" "b" "branch" "master" .upstream)
    , ("merge_pr",        .mergePr 7 .squash true)
    , ("label_issue",     .labelIssue 7 ["bug"] [])
    , ("comment",         .comment (.issue "hello"))
    , ("get_pr_comments", .getPrComments 7 false false) ]
  for (name, call) in calls do
    let result ← Server.evalToolCall state call
    TestM.assert (result.getObjValAs? Bool "isError" |>.toOption |>.getD false)
      (msg := s!"{name} should be refused when the task has no repository")

@[test]
def refreshTokenIsRefusedWithoutAnInstallation : Test := do
  -- A repository-independent task with no `installation_id` and no `default_organization` has no
  -- installation to mint from. Saying so beats signing a JWT for an installation of `0`.
  let state : Server.State :=
    { repo := none, allowedTools := [], appId := 0, privateKeyPath := ""
    , installationId := none, pat := "" }
  let result ← Server.evalToolCall state .refreshToken
  TestM.assert (result.getObjValAs? Bool "isError" |>.toOption |>.getD false)
    (msg := "refresh_token has no installation to mint from")

@[test]
def theTaskToolsStillWorkWithoutARepository : Test := do
  -- The point of the whole thing: what is left is what such a task is for.
  let ref ← IO.mkRef (none : Option Json)
  let state : Server.State :=
    { repo := none, allowedTools := [], appId := 0, privateKeyPath := ""
    , installationId := none, pat := "", outputRef := some ref
    , outputType := .string }
  let health ← Server.evalToolCall state .health
  TestM.assert (!(health.getObjValAs? Bool "isError" |>.toOption |>.getD false))
    (msg := "health answers without a repository")
  let submitted ← Server.evalToolCall state (.submitTaskOutput (Json.str "done"))
  TestM.assert (!(submitted.getObjValAs? Bool "isError" |>.toOption |>.getD false))
    (msg := "submit_task_output answers without a repository")
  TestM.assertEqual ((← ref.get).map (·.compress)) (some "\"done\"")
                    (msg := "and the output is recorded")

end OrchestraTest.Repoless
