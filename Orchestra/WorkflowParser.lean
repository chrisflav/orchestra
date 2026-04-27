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
  let context  := (mappingLookup pairs "context").bind (nodeAsString · |>.toOption)
  let readOnly :=
    (mappingLookup pairs "read-only").bind (nodeAsString · |>.toOption)
    |>.map (· == "true") |>.getD false
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
  return { agent, model, prompt, readOnly, input, output, context }

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
  let name  ← nodeAsString (← orError (mappingLookup pairs "name") "missing 'name'")
  let description := (mappingLookup pairs "description").bind (nodeAsString · |>.toOption)
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
  return { name, description, variables, steps }

end Orchestra.Workflow
