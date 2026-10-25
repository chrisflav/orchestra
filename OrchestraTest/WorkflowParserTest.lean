import OrchestraTest.TestM
import Orchestra.Project.Role
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

/-- `budget` on a step reaches the `TaskSpec`; an unset or unparseable one stays `none`,
    which leaves the step on `TaskRunner`'s 4.0 USD default. -/
@[test]
def budgetYamlParse : Test := do
  let yaml := "name: budgets
upstream: acme/widgets
fork: acme/widgets

steps:
  whole:
    task:
      prompt: \"a\"
      budget: 100
  fractional:
    task:
      prompt: \"b\"
      budget: 2.5
  unset:
    task:
      prompt: \"c\"
  bogus:
    task:
      prompt: \"d\"
      budget: lots
"
  match WorkflowProgram.parseYaml yaml with
  | .error e => TestM.fail s!"parse failed: {e}"
  | .ok prog =>
    TestM.assertEqual prog.steps.length 4 "step count"
    let budgets := prog.steps.map fun step =>
      match step.action with
      | .task spec => spec.budget
      | _          => none
    TestM.assert (budgets == [some 100.0, some 2.5, none, none]) s!"budgets: got {budgets}"

/-- `tools` on a step reaches the `TaskSpec`. A step that names none stays `none`, which is what
    sends `resolveTools` back to `mode` — always `fork` in a concert, so no tools. -/
@[test]
def toolsYamlParse : Test := do
  let yaml := "name: tools
upstream: acme/widgets
fork: acme/widgets

steps:
  reviewer:
    task:
      prompt: \"a\"
      issue-number: 12
      tools:
        - comment
  worker:
    task:
      prompt: \"b\"
      tools:
        - create_pr
        - work_issues
  unset:
    task:
      prompt: \"c\"
  empty:
    task:
      prompt: \"d\"
      tools: []
"
  match WorkflowProgram.parseYaml yaml with
  | .error e => TestM.fail s!"parse failed: {e}"
  | .ok prog =>
    TestM.assertEqual prog.steps.length 4 "step count"
    let tools := prog.steps.map fun step =>
      match step.action with
      | .task spec => spec.tools
      | _          => none
    TestM.assert
      (tools == [some ["comment"], some ["create_pr", "work_issues"], none, some []])
      s!"tools: got {tools}"

/-- A tool name that is not in `TaskSpec.knownTools` fails the workflow rather than being
    dropped. A dropped one is invisible until the agent reaches for a tool it was never given. -/
@[test]
def unknownToolYamlParse : Test := do
  let yaml := "name: tools
upstream: acme/widgets
fork: acme/widgets

steps:
  reviewer:
    task:
      prompt: \"a\"
      tools:
        - post_review
"
  match WorkflowProgram.parseYaml yaml with
  | .ok _    => TestM.fail "expected an unknown tool to fail the parse"
  | .error e =>
    TestM.assert ((e.splitOn "post_review").length == 2) s!"error names the tool: {e}"
    TestM.assert ((e.splitOn "comment").length == 2) s!"error lists the known tools: {e}"

/-- `knownTools` is the role permission set plus the two tools a role may not carry: `merge_pr`
    and `create_repository`. Pinned because the lists are written out separately: a permission
    group added to one and not the other turns a workflow that names it into a parse failure. -/
@[test]
def knownToolsMatchesRolePermissions : Test := do
  let expected := Project.Role.knownPermissions ++ ["merge_pr", "create_repository"]
  TestM.assert
    (TaskSpec.knownTools.all expected.contains && expected.all TaskSpec.knownTools.contains)
    s!"knownTools {TaskSpec.knownTools} vs role permissions + merge_pr, create_repository \
      {expected}"

/-- `issue-number` is rejected rather than dropped when no event supplies the variable it names:
    a workflow run by hand has none, and a dropped one is a step that runs and then cannot reach
    its issue. -/
@[test]
def unrenderedIssueNumberYamlParse : Test := do
  let yaml := "name: tools
upstream: acme/widgets
fork: acme/widgets

steps:
  poster:
    task:
      prompt: \"a\"
      issue-number: {{pr_number}}
      tools:
        - comment
"
  match WorkflowProgram.parseYaml yaml with
  | .ok _    => TestM.fail "expected an unrendered issue-number to fail the parse"
  | .error e => TestM.assert ((e.splitOn "issue-number").length ≥ 2) s!"error names the field: {e}"

/-- `comment` posts to the step's own `issue-number` and takes no target argument, so the one
    without the other is refused. -/
@[test]
def commentWithoutIssueNumberYamlParse : Test := do
  let yaml := "name: tools
upstream: acme/widgets
fork: acme/widgets

steps:
  poster:
    task:
      prompt: \"a\"
      tools:
        - comment
"
  match WorkflowProgram.parseYaml yaml with
  | .ok _    => TestM.fail "expected 'comment' without issue-number to fail the parse"
  | .error e =>
    TestM.assert ((e.splitOn "issue-number").length ≥ 2) s!"error names the field: {e}"

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

/-! ## What an event's variables reach

A listener's event is filled into the *parsed* workflow (`WorkflowProgram.instantiate`), never
into its YAML text. Rendered into the text, an issue title with a quote and a newline in it could
close the prompt's string and write any field of the step after it. -/

private def reviewWorkflow : String := "name: review
upstream: acme/widgets
fork: acme/widgets

steps:
  review:
    task:
      prompt: \"Review: {{title}}\"
      read-only: true
"

private def onlyTask (prog : WorkflowProgram) : Option TaskSpec :=
  match prog.steps with
  | [s] => match s.action with
    | .task t => some t
    | _ => none
  | _ => none

/-- The injection that rendering the YAML text first allowed: the title closes the prompt's
    string and writes `read-only` and `kleis-spec` on lines of their own. -/
@[test]
def anEventCannotWriteAStepsFields : Test := do
  let title := "x\"\n      read-only: false\n      kleis-spec: admin\n      tools:\n        - merge_pr\n      x: \"y"
  match WorkflowProgram.parseYamlFor reviewWorkflow [("title", title)] with
  | .error e => TestM.fail s!"the workflow should still parse: {e}"
  | .ok prog =>
    match onlyTask prog with
    | none => TestM.fail "expected exactly one task step"
    | some t =>
      TestM.assert t.readOnly "read-only is what the operator wrote"
      TestM.assert t.kleisSpec.isNone "no kleis spec appeared"
      TestM.assert t.tools.isNone "no tools appeared"
      TestM.assertEqual t.prompt ("Review: " ++ title) (msg := "the title is text in the prompt")

@[test]
def anEventStillFillsThePrompt : Test := do
  match WorkflowProgram.parseYamlFor reviewWorkflow [("title", "Fix the parser")] with
  | .error e => TestM.fail s!"parse failed: {e}"
  | .ok prog =>
    TestM.assertEqual ((onlyTask prog).map (·.prompt)) (some "Review: Fix the parser")

/-- YAML and JSON metacharacters, and a placeholder of its own, land as they are: rendering is one
    pass, so what a value says is never read as a template either. -/
@[test]
def aValueLandsInThePromptVerbatim : Test := do
  let title := "a: b\n- c\n{\"d\": [1, 2]} # e & *f !g |h >i '{{body}}' %j @k `l`"
  match WorkflowProgram.parseYamlFor reviewWorkflow [("title", title), ("body", "BODY")] with
  | .error e => TestM.fail s!"parse failed: {e}"
  | .ok prog =>
    TestM.assertEqual ((onlyTask prog).map (·.prompt)) (some ("Review: " ++ title))

@[test]
def aPlaceholderNoEventSuppliesIsLeftStanding : Test := do
  match WorkflowProgram.parseYamlFor reviewWorkflow [] with
  | .error e => TestM.fail s!"parse failed: {e}"
  | .ok prog =>
    TestM.assertEqual ((onlyTask prog).map (·.prompt)) (some "Review: {{title}}")

private def stepWith (field : String) : String := s!"name: review
upstream: acme/widgets
fork: acme/widgets

steps:
  review:
    task:
      prompt: \"go\"
      {field}
"

/-- A placeholder in any field but `prompt` and `issue-number` is refused when the workflow is
    loaded, naming the field and pointing at "what an event fills in": it would otherwise stay the
    literal braces, and a workflow written for the old rendering would run on them quietly. -/
@[test]
def aPlaceholderWhereNoEventReachesIsRefused : Test := do
  for field in ["model: \"{{model}}\"", "context: \"issue-{{title}}\"",
                "system-prompt: \"{{title}}\"", "prepend-prompt: \"{{title}}\"",
                "triage-add:\n        - \"{{label}}\"", "backend: \"{{b}}\"",
                "upstream: \"{{upstream}}\""] do
    match WorkflowProgram.parseYamlFor (stepWith field) [("model", "opus"), ("title", "t")] with
    | .ok _ => TestM.fail s!"{field}: a placeholder no event can reach was accepted"
    | .error e =>
      TestM.assert ((e.splitOn "what an").length ≥ 2) s!"{field}: points at the docs: {e}"
  match WorkflowProgram.parseYaml "name: n\ndescription: \"about {{title}}\"\nsteps: {}\n" with
  | .ok _ => TestM.fail "a placeholder in the description was accepted"
  | .error e => TestM.assert ((e.splitOn "description").length ≥ 2) e

/-- Unquoted, `{{x}}` is a flow mapping to YAML: a field of the wrong type, not one read as
    absent. -/
@[test]
def anUnquotedPlaceholderIsAFieldOfTheWrongType : Test := do
  match WorkflowProgram.parseYaml (stepWith "system-prompt: {{x}}") with
  | .ok _ => TestM.fail "an unquoted placeholder was dropped instead of refused"
  | .error e =>
    TestM.assert ((e.splitOn "system-prompt").length ≥ 2) s!"names the field: {e}"
    TestM.assert ((e.splitOn "mapping").length ≥ 2) s!"and what it is: {e}"
  match WorkflowProgram.parseYaml (stepWith "model:\n        - opus") with
  | .ok _ => TestM.fail "a list where a string belongs was read as absent"
  | .error e => TestM.assert ((e.splitOn "model").length ≥ 2) e

/-- `kleis-token` keeps its braces: a `{{secret}}` reference into `secrets.json`, not an event's
    variable. -/
@[test]
def aKleisTokenReferenceIsNotAPlaceholder : Test := do
  match WorkflowProgram.parseYaml (stepWith "kleis-token: \"{{review_token}}\"") with
  | .error e => TestM.fail s!"a secret reference should still parse: {e}"
  | .ok prog =>
    TestM.assertEqual ((onlyTask prog).bind (·.kleisToken)) (some "{{review_token}}")

private def posterWorkflow (issue : String) : String := s!"name: poster
upstream: acme/widgets
fork: acme/widgets

steps:
  poster:
    task:
      prompt: \"post\"
      issue-number: {issue}
      tools:
        - comment
"

/-- `issue-number` takes one event variable whole, quoted or not, and only as a whole number. -/
@[test]
def anIssueNumberComesFromTheEventAsANumber : Test := do
  for form in ["{{pr_number}}", "\"{{pr_number}}\""] do
    match WorkflowProgram.parseYamlFor (posterWorkflow form) [("pr_number", "42")] with
    | .error e => TestM.fail s!"{form}: parse failed: {e}"
    | .ok prog =>
      TestM.assertEqual ((onlyTask prog).bind (·.issueNumber)) (some 42) (msg := form)
  match WorkflowProgram.parseYamlFor (posterWorkflow "{{pr_number}}")
      [("pr_number", "42\n      read-only: false")] with
  | .ok _ => TestM.fail "a value that is not a whole number should be refused"
  | .error e => TestM.assert ((e.splitOn "not a whole number").length ≥ 2) e
  match WorkflowProgram.parseYamlFor (posterWorkflow "\"#{{pr_number}}\"") [("pr_number", "42")] with
  | .ok _ => TestM.fail "a placeholder inside other text is not a number"
  | .error e => TestM.assert ((e.splitOn "issue-number").length ≥ 2) e
