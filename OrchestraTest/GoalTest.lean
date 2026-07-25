import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra

/-!
# Goal support

An issue's taxis `goal` is the condition a task launched for it is held to. Two things have to
hold for that to be worth anything, and both are checked here without touching the network:

* the goal survives every hop between the spawn site and the sandbox — queue entry, task record,
  task file — so a daemon restart or a continuation doesn't quietly drop the bar; and
* the goal is the issue's goal *alone*. The prompt the same spawn builds is the role template
  with the issue body and its comment thread rendered into it; handing that to the judge instead
  would ask it to decide whether a description had been "met".
-/

namespace OrchestraTest.Goal

private def repo : Repository := { owner := "o", name := "r" }

private def entryWithGoal (goal : Option String) : Queue.QueueEntry :=
  { id := "e1", createdAt := "2026-01-01T00:00:00Z"
  , upstream := repo, fork := repo, mode := .pr
  , prompt := "do the thing", goal }

private def recordWithGoal (goal : Option String) : TaskStore.TaskRecord :=
  { id := "t1", createdAt := "2026-01-01T00:00:00Z"
  , upstream := repo, fork := repo, mode := .pr
  , prompt := "do the thing", goal }

/-! ## Serialization round-trips -/

@[test]
def queueEntryRoundTripsGoal : Test := do
  let goal := "`lake build` and `lake test` both pass on the branch"
  match (FromJson.fromJson? (ToJson.toJson (entryWithGoal (some goal))) : Except String Queue.QueueEntry) with
  | .ok e => TestM.assertEqual e.goal (some goal) (msg := "queue entry goal")
  | .error err => TestM.fail s!"queue entry did not parse back: {err}"

@[test]
def queueEntryOmitsAbsentGoal : Test := do
  let j := ToJson.toJson (entryWithGoal none)
  TestM.assert (j.getObjVal? "goal" |>.toOption |>.isNone) "no goal key when there is no goal"
  match (FromJson.fromJson? j : Except String Queue.QueueEntry) with
  | .ok e => TestM.assertEqual e.goal none (msg := "absent goal reads back as none")
  | .error err => TestM.fail s!"queue entry did not parse back: {err}"

@[test]
def taskRecordRoundTripsGoal : Test := do
  let goal := "the migration is applied and the old table is gone"
  match (FromJson.fromJson? (ToJson.toJson (recordWithGoal (some goal))) : Except String TaskStore.TaskRecord) with
  | .ok r => TestM.assertEqual r.goal (some goal) (msg := "task record goal")
  | .error err => TestM.fail s!"task record did not parse back: {err}"

@[test]
def taskFileGoalIsRead : Test := do
  -- A task file may set a goal explicitly, without any issue behind it.
  let goal := "every doctest in the README runs"
  let j := Json.mkObj
    [ ("upstream", "o/r"), ("fork", "o/r"), ("mode", "pr")
    , ("prompt", "do the thing")
    , ("goal", Json.str goal) ]
  match (FromJson.fromJson? j : Except String Task) with
  | .ok t => TestM.assertEqual t.ioTask.goal (some goal) (msg := "task file goal")
  | .error err => TestM.fail s!"task did not parse: {err}"

@[test]
def taskFileWithoutGoalHasNone : Test := do
  let j := Json.mkObj
    [ ("upstream", "o/r"), ("fork", "o/r"), ("mode", "pr"), ("prompt", "do the thing") ]
  match (FromJson.fromJson? j : Except String Task) with
  | .ok t => TestM.assertEqual t.ioTask.goal none (msg := "no goal without the field")
  | .error err => TestM.fail s!"task did not parse: {err}"

/-! ## The goal is the issue's goal, not the prompt

`Project.Issue` values are plain structures, so this needs no taxis instance: it exercises the
mapping (`goalFor`) and the prompt builder (`render`/`renderVarsFor`) that the two spawn paths
share, on the same issue. -/

private def project : Project.Project :=
  { id := ⟨1⟩, name := "orchestra", createdAt := "2026-01-01T00:00:00Z"
  , defaultTarget := some { repo, branch := "master" } }

private def issue : Project.Issue :=
  { id := ⟨2⟩, projectId := ⟨1⟩
  , title := "Goal support"
  , description := "Taxis issues have a goal field. The goal of an issue, if set, should be a \
condition to check if an issue is completed or not. Modern coding agents support /goal commands: \
they work and don't stop before the goal is complete, with a second agent evaluating whether the \
goal is complete."
  , goal := "orchestra passes a task's goal to the agent backend"
  , createdAt := "2026-01-01T00:00:00Z", updatedAt := "2026-01-01T00:00:00Z" }

private def roleTemplate : String :=
  "You are working on {{issue_title}} (#{{issue_id}}) in {{project_name}}.\n\n\
{{issue_description}}\n\n{{issue_comments}}\n\nOpen a PR against {{target_branch}}."

@[test]
def goalForIsTheIssueGoalAlone : Test := do
  TestM.assertEqual (Project.goalFor (some issue)) (some issue.goal)
    (msg := "the issue's own goal, verbatim")

@[test]
def goalForIsNoneWithoutAnIssue : Test := do
  TestM.assertEqual (Project.goalFor none) none (msg := "no issue, no goal")

@[test]
def goalForIsNoneWhenUnset : Test := do
  TestM.assertEqual (Project.goalFor (some { issue with goal := "" })) none
    (msg := "an unset goal is no goal, not an empty condition")

@[test]
def goalIsNotTheRenderedPrompt : Test := do
  let vars := Project.renderVarsFor project (some issue) ""
    (comments := some "reviewer: this needs a test")
  let prompt := Project.render roleTemplate vars
  let some goal := Project.goalFor (some issue)
    | TestM.fail "expected a goal"
  TestM.assert (goal != prompt) "the goal is not the prompt"
  TestM.assert (!goal.containsSubstr "Taxis issues have a goal field")
    "the goal does not carry the issue description"
  TestM.assert (!goal.containsSubstr "reviewer: this needs a test")
    "the goal does not carry the comment thread"
  TestM.assert (goal.length * 4 < prompt.length)
    "the goal is a condition, not the whole assembled prompt"
  -- The prompt is still the full briefing; keeping the goal short must not have thinned it.
  TestM.assert (prompt.containsSubstr "Taxis issues have a goal field")
    "the prompt still carries the issue description"

/-! ## Backend invocation -/

@[test]
def claudeGoalArgsCarryTheConditionVerbatim : Test := do
  let goal := "`lake test` passes"
  let some args := AgentDef.claude.goalArgs goal
    | TestM.fail "claude supports goals"
  TestM.assert (args.contains "--settings") "the goal travels as a settings argument"
  TestM.assert (args.any (·.containsSubstr "Stop")) "installed as a Stop hook"
  let payload := args[1]?.getD ""
  match Json.parse payload with
  | .error err => TestM.fail s!"settings payload is not JSON: {err}"
  | .ok j =>
    let hook := j.getObjVal? "hooks" |>.toOption
      |>.bind (·.getObjVal? "Stop" |>.toOption)
      |>.bind (·.getArr? |>.toOption) |>.bind (·[0]?)
      |>.bind (·.getObjVal? "hooks" |>.toOption)
      |>.bind (·.getArr? |>.toOption) |>.bind (·[0]?)
    let kind := hook.bind (·.getObjValAs? String "type" |>.toOption)
    let condition := hook.bind (·.getObjValAs? String "prompt" |>.toOption)
    TestM.assertEqual kind (some "prompt") (msg := "an LLM-judged hook, not a shell command")
    TestM.assertEqual condition (some goal) (msg := "the condition, unmodified")

@[test]
def backendsWithoutGoalSupportSayNone : Test := do
  -- `none` is what makes `Sandbox.launchAgent` warn and run without a goal instead of handing
  -- the CLI a flag it has never heard of.
  for (name, agent) in [("vibe", AgentDef.vibe), ("opencode", AgentDef.opencode),
                        ("pi", AgentDef.pi)] do
    TestM.assert (agent.goalArgs "anything" |>.isNone) s!"{name} declines the goal cleanly"
