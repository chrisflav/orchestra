import OrchestraTest.TestM
import Orchestra.WorkflowParser

open Orchestra
open Orchestra.Workflow

@[test]
def sequenceYamlParse : Test := do
  let yaml ← IO.FS.readFile "examples/concerts/sequence.yaml"
  match WorkflowProgram.parseYaml yaml with
  | .error e => TestM.fail s!"parse failed: {e}"
  | .ok prog =>
    TestM.assertEqual prog.name "sequence" "name"
    TestM.assert prog.description.isSome "description present"
    TestM.assertEqual prog.variables.length 0 "no variables"
    TestM.assertEqual prog.steps.length 3 "step count"
    match prog.steps with
    | [plan, implement, review] =>
      TestM.assertEqual plan.name "plan" "plan name"
      TestM.assert plan.«for».isNone "plan: no for"
      TestM.assert plan.guard.isNone "plan: no guard"
      match plan.action with
      | .task spec =>
        TestM.assertEqual spec.agent (some "claude") "plan: agent"
        TestM.assertEqual spec.model (some "sonnet") "plan: model"
        TestM.assert spec.readOnly "plan: read-only"
        TestM.assertEqual spec.output.length 0 "plan: no outputs"
      | _ => TestM.fail "plan: expected task action"
      TestM.assertEqual implement.name "implement" "implement name"
      match implement.action with
      | .task spec =>
        TestM.assert (!spec.readOnly) "implement: not read-only"
        TestM.assertEqual spec.output.length 0 "implement: no outputs"
      | _ => TestM.fail "implement: expected task action"
      TestM.assertEqual review.name "review" "review name"
      match review.action with
      | .task spec => TestM.assert spec.readOnly "review: read-only"
      | _ => TestM.fail "review: expected task action"
    | _ => TestM.fail "expected exactly 3 steps"

@[test]
def conditionalsYamlParse : Test := do
  let yaml ← IO.FS.readFile "examples/concerts/conditionals.yaml"
  match WorkflowProgram.parseYaml yaml with
  | .error e => TestM.fail s!"parse failed: {e}"
  | .ok prog =>
    TestM.assertEqual prog.name "conditionals" "name"
    TestM.assertEqual prog.steps.length 3 "step count"
    match prog.steps with
    | [evaluate, checkDiff, implement] =>
      TestM.assertEqual evaluate.name "evaluate-difficulty" "evaluate name"
      TestM.assert evaluate.guard.isNone "evaluate: no guard"
      match evaluate.action with
      | .task spec =>
        TestM.assertEqual spec.output.length 1 "evaluate: output count"
        TestM.assertEqual (spec.output.map (·.name)) ["difficulty"] "evaluate: output names"
        TestM.assertEqual spec.context (some "evaluation") "evaluate: context"
      | _ => TestM.fail "evaluate: expected task action"
      TestM.assertEqual checkDiff.name "check-difficulty" "check name"
      TestM.assert checkDiff.guard.isSome "check: has guard"
      match checkDiff.action with
      | .flow .exit => TestM.assert true "check: flow exit"
      | _ => TestM.fail "check: expected flow exit"
      TestM.assertEqual implement.name "implement" "implement name"
      TestM.assert implement.guard.isNone "implement: no guard"
      match implement.action with
      | .task spec => TestM.assertEqual spec.output.length 0 "implement: no outputs"
      | _ => TestM.fail "implement: expected task action"
    | _ => TestM.fail "expected exactly 3 steps"

@[test]
def loopYamlParse : Test := do
  let yaml ← IO.FS.readFile "examples/concerts/loop.yaml"
  match WorkflowProgram.parseYaml yaml with
  | .error e => TestM.fail s!"parse failed: {e}"
  | .ok prog =>
    TestM.assertEqual prog.name "joke-machine" "name"
    TestM.assertEqual prog.variables.length 1 "variable count"
    TestM.assertEqual (prog.variables.map (·.1)) ["total-joke-score"] "variable names"
    TestM.assertEqual prog.steps.length 3 "step count"
    match prog.steps with
    | [chooseAnimals, joke, evaluateJokes] =>
      TestM.assertEqual chooseAnimals.name "choose-animals" "choose-animals name"
      TestM.assert chooseAnimals.«for».isNone "choose-animals: no for"
      match chooseAnimals.action with
      | .task spec =>
        TestM.assertEqual spec.model (some "opus") "choose-animals: model"
        TestM.assert spec.readOnly "choose-animals: read-only"
        TestM.assertEqual (spec.output.map (·.name)) ["animals"] "choose-animals: output names"
      | _ => TestM.fail "choose-animals: expected task action"
      TestM.assertEqual joke.name "joke" "joke name"
      TestM.assert joke.«for».isSome "joke: has for"
      TestM.assert joke.guard.isNone "joke: no guard"
      match joke.«for» with
      | some fc =>
        TestM.assertEqual fc.loopVar "animal" "joke: loop var"
        match fc.source with
        | .stepOutput step out =>
          TestM.assertEqual step "choose-animals" "joke: source step"
          TestM.assertEqual out "animals" "joke: source output"
        | _ => TestM.fail "joke: expected stepOutput source"
      | none => TestM.fail "joke: for clause missing"
      match joke.action with
      | .task spec =>
        TestM.assertEqual spec.input.length 1 "joke: input count"
        TestM.assertEqual (spec.output.map (·.name)) ["joke"] "joke: output names"
      | _ => TestM.fail "joke: expected task action"
      TestM.assertEqual evaluateJokes.name "evaluate-jokes" "evaluate-jokes name"
      match evaluateJokes.action with
      | .task spec =>
        TestM.assertEqual spec.input.length 2 "evaluate-jokes: input count"
        TestM.assertEqual spec.output.length 1 "evaluate-jokes: output count"
        match spec.output with
        | [out] =>
          TestM.assertEqual out.name "summary" "evaluate-jokes: output name"
          TestM.assertEqual out.writeTo (some "total-joke-score") "evaluate-jokes: write_to"
        | _ => TestM.fail "evaluate-jokes: expected single output"
      | _ => TestM.fail "evaluate-jokes: expected task action"
    | _ => TestM.fail "expected exactly 3 steps"
