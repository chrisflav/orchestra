import Orchestra.Workflow
import Yaml

open Yaml (Node)

namespace Orchestra.Workflow

private def orError {α : Type} (o : Option α) (msg : String) : Except String α :=
  match o with
  | some a => .ok a
  | none   => .error msg

private def nodeAsString : Node → Except String String
  | .scalar _ _ v => .ok v
  | n              => .error s!"expected scalar, got {repr n}"

private def nodeAsMapping : Node → Except String (Array (Node × Node))
  | .mapping _ _ pairs => .ok pairs
  | n                  => .error s!"expected mapping, got {repr n}"

private def nodeAsSeq : Node → Except String (Array Node)
  | .sequence _ _ items => .ok items
  | n                   => .error s!"expected sequence, got {repr n}"

private def mappingLookup (pairs : Array (Node × Node)) (key : String) : Option Node :=
  pairs.findSome? fun (k, v) =>
    match k with
    | .scalar _ _ s => if s == key then some v else none
    | _             => none

private def strTrim (s : String) : String := s.trimAscii.toString

/-- YAML scalars reach us as strings, so a budget written `budget: 100` or `budget: 100.0`
    is parsed as JSON to get the float. Anything unparseable is dropped, which leaves the
    step on the 4.0 USD default rather than failing the whole workflow. -/
private def parseBudget (s : String) : Option Float :=
  match Lean.Json.parse (strTrim s) with
  | .ok (.num n) => some n.toFloat
  | _            => none

private partial def parseResultType (s : String) : Except String ResultType :=
  let s' := strTrim s
  if s'.startsWith "list " then
    parseResultType (s'.drop 5).toString |>.map .list
  else
    match s' with
    | "string" => .ok .string
    | "int"    => .ok .int
    | "nat"    => .ok .nat
    | "bool"   => .ok .bool
    | "unit"   => .ok .unit
    | other    => .error s!"unknown type '{other}'"

private def parseVarRef (s : String) : VarRef :=
  match s.splitOn "." with
  | [step, out] => .stepOutput step out
  | _           => .global s

private def parseIntLit (s : String) : Except String Int :=
  match s.toInt? with
  | some n => .ok n
  | none   => .error s!"expected integer, got '{s}'"

private def parseCondTokens : List String → Except String Cond
  | "!" :: rest            => parseCondTokens rest |>.map .not
  | [v, "in", xs]          => .ok (.mem (parseVarRef v) (parseVarRef xs))
  | [v, "not", "in", xs]   => .ok (.not (.mem (parseVarRef v) (parseVarRef xs)))
  | [v, ">", n]            => parseIntLit n |>.map (.gt (parseVarRef v))
  | [v, "<", n]            => parseIntLit n |>.map (.lt (parseVarRef v))
  | [v, "==", n]           => parseIntLit n |>.map (.eq (parseVarRef v))
  | [v]                    => .ok (.ref (parseVarRef v))
  | toks                   => .error s!"cannot parse condition: '{" ".intercalate toks}'"

private def parseCond (node : Node) : Except String Cond := do
  let s      ← nodeAsString node
  let tokens := s.splitOn " " |>.filter (· != "")
  parseCondTokens tokens

private def parseForClause (node : Node) : Except String ForClause := do
  let pairs ← nodeAsMapping node
  if h : 0 < pairs.size then
    let (keyNode, valNode) := pairs[0]
    let loopVar ← nodeAsString keyNode
    let src     ← nodeAsString valNode
    return { loopVar, source := parseVarRef src }
  else
    .error "for clause must have at least one entry"

private def parseOutputSpec (name : String) (node : Node) : Except String OutputSpec := do
  let pairs   ← nodeAsMapping node
  let typeStr ← nodeAsString (← orError (mappingLookup pairs "type") s!"output '{name}' missing 'type'")
  let type    ← parseResultType typeStr
  let writeTo := (mappingLookup pairs "write_to").bind (nodeAsString · |>.toOption)
  return { name, type, writeTo }

private def parseTaskSpec (node : Node) : Except String TaskSpec := do
  let pairs    ← nodeAsMapping node
  let prompt   ← nodeAsString (← orError (mappingLookup pairs "prompt") "task missing 'prompt'")
  let agent    := (mappingLookup pairs "agent").bind   (nodeAsString · |>.toOption)
  let model    := (mappingLookup pairs "model").bind   (nodeAsString · |>.toOption)
  let budget   := (mappingLookup pairs "budget").bind  (nodeAsString · |>.toOption)
                  |>.bind parseBudget
  let context  := (mappingLookup pairs "context").bind (nodeAsString · |>.toOption)
  let readOnly :=
    (mappingLookup pairs "read-only").bind (nodeAsString · |>.toOption)
    |>.map (· == "true") |>.getD false
  let upstream      := (mappingLookup pairs "upstream").bind       (nodeAsString · |>.toOption)
                       |>.map (Repository.parse · |>.toOption) |>.join
  let fork          := (mappingLookup pairs "fork").bind           (nodeAsString · |>.toOption)
                       |>.map (Repository.parse · |>.toOption) |>.join
  let systemPrompt  := (mappingLookup pairs "system-prompt").bind  (nodeAsString · |>.toOption)
  let prependPrompt := (mappingLookup pairs "prepend-prompt").bind (nodeAsString · |>.toOption)
  let backend       := (mappingLookup pairs "backend").bind        (nodeAsString · |>.toOption)
  -- A workflow's YAML is template-rendered before it is parsed, and an unknown `{{...}}` is left
  -- standing. Dropping what does not parse would turn `issue-number: {{pr_number}}` against a
  -- listener that exports no `pr_number` into a step that runs to completion and then cannot
  -- reach the issue it was written for.
  let issueNumber ← match mappingLookup pairs "issue-number" with
    | none      => pure none
    | some node =>
        -- Not `nodeAsString`'s error: an unrendered `{{issue_number}}` is a nested flow mapping
        -- to YAML, so the failure that actually shows up here reads "expected scalar, got
        -- mapping" and names neither the field nor the placeholder that caused it.
        match nodeAsString node with
        | .error _  => .error "issue-number must be a number; the value is not even a scalar, \
                               which is what an unrendered '{{...}}' placeholder looks like to \
                               the YAML parser"
        | .ok raw   =>
          match (strTrim raw).toNat? with
          | some n => .ok (some n)
          | none   => .error s!"issue-number must be a number, got '{strTrim raw}'"
  let tools ← match mappingLookup pairs "tools" with
    | none      => pure none
    | some node => do
        let items ← nodeAsSeq node
        let names ← items.toList.mapM fun item => return strTrim (← nodeAsString item)
        match names.find? (!TaskSpec.knownTools.contains ·) with
        | some bad => .error s!"unknown tool '{bad}'; known tools are \
                               {", ".intercalate TaskSpec.knownTools}"
        | none     => pure (some names)
  -- `comment` posts to the issue the task was launched from and nothing else, and a step is only
  -- ever launched from the `issue-number` it names. Granting one without the other is a step that
  -- writes a whole report and then has nowhere to put it.
  if (tools.getD []).contains "comment" && issueNumber.isNone then
    .error "the 'comment' tool needs 'issue-number' on the same step: it posts to that issue or \
            pull request and takes no target of its own"
  let triageAddLabels ← match mappingLookup pairs "triage-add" with
    | none      => pure []
    | some node => do
        let items ← nodeAsSeq node
        items.toList.mapM (nodeAsString ·)
  let triageRemoveLabels ← match mappingLookup pairs "triage-remove" with
    | none      => pure []
    | some node => do
        let items ← nodeAsSeq node
        items.toList.mapM (nodeAsString ·)
  let input ← match mappingLookup pairs "input" with
    | none      => pure []
    | some iNode => do
        let items ← nodeAsSeq iNode
        items.toList.mapM fun item => do
          return parseVarRef (← nodeAsString item)
  let output ← match mappingLookup pairs "output" with
    | none       => pure []
    | some oNode => do
        let outPairs ← nodeAsMapping oNode
        outPairs.toList.mapM fun (k, v) => do
          let name ← nodeAsString k
          parseOutputSpec name v
  return { agent, model, budget, tools, prompt, readOnly, input, output, context, upstream, fork,
           systemPrompt, prependPrompt, backend, issueNumber, triageAddLabels, triageRemoveLabels }

private def parseWriteAction (node : Node) : Except String StepAction := do
  let s ← nodeAsString node
  if let [var, _] := s.splitOn " += " then
    return .write (strTrim var) .increment
  else if let [var, _] := s.splitOn " -= " then
    return .write (strTrim var) .decrement
  else if let [var, rhs] := s.splitOn " = " then
    return .write (strTrim var) (.assign (parseVarRef (strTrim rhs)))
  else
    .error s!"cannot parse write operation: '{s}'"

private def parseFlowControl (node : Node) : Except String FlowControl := do
  let s ← nodeAsString node
  match strTrim s with
  | "exit"     => .ok .exit
  | "break"    => .ok .doBreak
  | "continue" => .ok .doContinue
  | other      => .error s!"unknown flow control: '{other}'"

private def parseStep (name : String) (node : Node) : Except String Step := do
  let pairs  ← nodeAsMapping node
  let for_   ← (mappingLookup pairs "for").mapM parseForClause
  let guard  ← (mappingLookup pairs "if").mapM parseCond
  let action ←
    if let some taskNode := mappingLookup pairs "task" then
      .task <$> parseTaskSpec taskNode
    else if let some writeNode := mappingLookup pairs "write" then
      parseWriteAction writeNode
    else if let some flowNode := mappingLookup pairs "flow" then
      .flow <$> parseFlowControl flowNode
    else
      .error s!"step '{name}' has no action (expected 'task', 'write', or 'flow')"
  return { name, «for» := for_, guard, action }

private def parseVariables (node : Node) : Except String (List (String × ResultType)) := do
  let pairs ← nodeAsMapping node
  pairs.toList.mapM fun (k, v) => do
    let name    ← nodeAsString k
    let vPairs  ← nodeAsMapping v
    let typeStr ← nodeAsString (← orError (mappingLookup vPairs "type")
                                  s!"variable '{name}' missing 'type'")
    let type    ← parseResultType typeStr
    return (name, type)

/-- Parse a YAML string into a `WorkflowProgram`. -/
def WorkflowProgram.parseYaml (input : String) : Except String WorkflowProgram := do
  let stream ← match Yaml.lYamlStream.run (Yaml.ensureTrailingNewline input) with
    | .ok _ s    => .ok s
    | .error _ e => .error s!"YAML parse error: {e}"
  let doc   ← orError stream.documents[0]? "empty YAML document"
  let root  ← orError doc.root "YAML document has no root node"
  let pairs ← nodeAsMapping root
  let name     ← nodeAsString (← orError (mappingLookup pairs "name") "missing 'name'")
  let description := (mappingLookup pairs "description").bind (nodeAsString · |>.toOption)
  let upstream := (mappingLookup pairs "upstream").bind (nodeAsString · |>.toOption)
                  |>.bind (Repository.parse · |>.toOption)
  let fork     := (mappingLookup pairs "fork").bind     (nodeAsString · |>.toOption)
                  |>.bind (Repository.parse · |>.toOption)
  let variables ← match mappingLookup pairs "variables" with
    | none      => pure []
    | some node => parseVariables node
  let steps ← match mappingLookup pairs "steps" with
    | none      => pure []
    | some node => do
        let stepPairs ← nodeAsMapping node
        stepPairs.toList.mapM fun (k, v) => do
          let stepName ← nodeAsString k
          parseStep stepName v
  return { name, description, upstream, fork, variables, steps }

end Orchestra.Workflow
