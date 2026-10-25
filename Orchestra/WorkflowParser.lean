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

private def nodeKind : Node → String
  | .scalar .. => "string"
  | .sequence .. => "list"
  | .mapping .. => "mapping"
  | .alias .. => "alias"

/-- An optional string field, strictly: absent is `none`, and anything but a scalar is an error
    naming the field, rather than the field silently read as absent. -/
private def optStr (pairs : Array (Node × Node)) (key : String) : Except String (Option String) :=
  match mappingLookup pairs key with
  | none => .ok none
  | some (.scalar _ _ v) => .ok (some v)
  | some n => .error s!"'{key}' must be a string, not a {nodeKind n}"

/-- Where an event's variables may stand: a task step's `prompt` and `issue-number`, the two
    fields `WorkflowProgram.instantiate` fills in. A step's `kleis-token` is a `{{secret}}`
    reference into `secrets.json`, never an event's variable, and keeps its braces too. -/
private def placeholderAllowedAt : List String → Bool
  | ["steps", _, "task", "prompt"] => true
  | ["steps", _, "task", "issue-number"] => true
  | ["steps", _, "task", "kleis-token"] => true
  | _ => false

private def whatAnEventFillsIn : String :=
  "an event's variables reach only a task step's 'prompt' and 'issue-number' (see \"what an \
event fills in\" in docs/workflow.md)"

/-- Refuse a `{{` anywhere but where `placeholderAllowedAt` allows one, before anything is read.

    Everywhere else a placeholder used to be rendered into the YAML text with the event, and is now
    left as the literal braces (`WorkflowProgram.instantiate`). A workflow written for the old
    behaviour would therefore run with `{{title}}` as its context label or its system prompt's
    name, and say nothing; refusing it at load time says which field and why. An unquoted `{{x}}`
    is a flow mapping to YAML, so a mapping keyed by a mapping is refused here too, as the field of
    the wrong type it is. -/
private partial def refusePlaceholders (path : List String) (node : Node) : Except String Unit := do
  if placeholderAllowedAt path then return
  let field := if path.isEmpty then "the document" else s!"'{".".intercalate path}'"
  match node with
  | .scalar _ _ v =>
    if (v.splitOn "{{").length > 1 then
      .error s!"{field} contains '\{\{', but {whatAnEventFillsIn}; write it without a placeholder"
  | .sequence _ _ items =>
    for (item, i) in items.zipIdx do refusePlaceholders (path ++ [toString i]) item
  | .mapping _ _ pairs =>
    for (k, v) in pairs do
      match k with
      | .scalar _ _ key =>
        if (key.splitOn "{{").length > 1 then
          .error s!"{field} has a key containing '\{\{', but {whatAnEventFillsIn}"
        refusePlaceholders (path ++ [key]) v
      | _ =>
        .error s!"{field} is a mapping where a string belongs: YAML reads an unquoted \
'\{\{…}}' as one, and {whatAnEventFillsIn}"
  | .alias _ => pure ()

/-- The variable a `{{name}}` placeholder names, when `node` is exactly one: quoted, a scalar
    `"{{name}}"`; unquoted, what YAML reads `{{name}}` as — a flow mapping whose one key is a flow
    mapping whose one key is `name`. -/
private def placeholderOf? : Node → Option String
  | .scalar _ _ v =>
    let s := strTrim v
    if s.startsWith "{{" && s.endsWith "}}" && s.length > 4 then
      let name := strTrim ((s.drop 2).dropEnd 2).toString
      if !name.isEmpty && name.all (fun c => c.isAlphanum || c == '_' || c == '-' || c == '.')
      then some name else none
    else none
  | .mapping _ _ #[(.mapping _ _ #[(.scalar _ _ name, _)], _)] =>
    let name := strTrim name
    if !name.isEmpty && name.all (fun c => c.isAlphanum || c == '_' || c == '-' || c == '.')
    then some name else none
  | _ => none

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
  -- Strict about their type: a list or a mapping where a string belongs is an error naming the
  -- field, not a field read as absent. What a string field may *say* is unchanged.
  let agent    ← optStr pairs "agent"
  let model    ← optStr pairs "model"
  let budget   := (← optStr pairs "budget").bind parseBudget
  let context  ← optStr pairs "context"
  let readOnly := (← optStr pairs "read-only").map (· == "true") |>.getD false
  let upstream      := (← optStr pairs "upstream").bind (Repository.parse · |>.toOption)
  let fork          := (← optStr pairs "fork").bind (Repository.parse · |>.toOption)
  let systemPrompt  ← optStr pairs "system-prompt"
  let prependPrompt ← optStr pairs "prepend-prompt"
  let backend       ← optStr pairs "backend"
  let identity      ← optStr pairs "identity"
  -- Strict, unlike the fields above: each decides what the step's kleis token reaches, and a
  -- dropped one would run the step under whatever the next level names.
  let kleisSpec ← match mappingLookup pairs "kleis-spec" with
    | none => pure none
    | some n =>
      let s := strTrim (← nodeAsString n)
      match kleisSpecNameError? s with
      | some e => .error s!"kleis-spec: {e}"
      | none   => pure (some s)
  let kleisToken ← match mappingLookup pairs "kleis-token" with
    | none => pure none
    | some n =>
      match nodeAsString n with
      | .error _ => .error "kleis-token must be a quoted reference to a secret, such as \
                           \"{{my_token}}\"; unquoted, YAML reads the braces as a mapping"
      | .ok raw =>
        let s := strTrim raw
        match kleisTokenRefError? s with
        | some e => .error e
        | none   => pure (some s)
  -- Strict too: an image dropped as unreadable runs the step without the tools it needs.
  let executionImage ← match mappingLookup pairs "execution-image" with
    | none => pure none
    | some n =>
      let s := strTrim (← nodeAsString n)
      match Exec.imageRefError? s with
      | some e => .error s!"execution-image: {e}"
      | none   => pure (some s)
  let kleisNarrow ← match mappingLookup pairs "kleis-narrow" with
    | none      => pure []
    | some node => do
        let items ← nodeAsSeq node
        items.toList.mapM fun item => return strTrim (← nodeAsString item)
  -- A number, or one event variable that names it (`issue-number: {{pr_number}}`), which
  -- `WorkflowProgram.instantiate` resolves once the event is known — after this parse, so that
  -- nothing an event says is ever read as YAML. Anything else is refused rather than dropped: a
  -- dropped one is a step that runs to completion and then cannot reach its issue.
  let (issueNumber, issueNumberVar) ← match mappingLookup pairs "issue-number" with
    | none      => pure (none, none)
    | some node =>
        match placeholderOf? node with
        | some v => pure (none, some v)
        | none =>
          -- Not `nodeAsString`'s error: a `{{...}}` that is not one whole placeholder is a nested
          -- flow mapping to YAML, so the failure that would show up reads "expected scalar, got
          -- mapping" and names neither the field nor the placeholder that caused it.
          match nodeAsString node with
          | .error _  => .error "issue-number must be a number or one '{{variable}}'; the value \
                                 is not even a scalar"
          | .ok raw   =>
            match (strTrim raw).toNat? with
            | some n => .ok (some n, none)
            | none   => .error s!"issue-number must be a number or one '\{\{variable}}', got \
'{strTrim raw}'"
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
  if (tools.getD []).contains "comment" && issueNumber.isNone && issueNumberVar.isNone then
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
           systemPrompt, prependPrompt, backend, issueNumber, triageAddLabels, triageRemoveLabels,
           identity, kleisToken, kleisSpec, kleisNarrow, issueNumberVar, executionImage }

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

/-- Parse a YAML string into a `WorkflowProgram`, with every placeholder still in it: what an
    event fills in is `WorkflowProgram.instantiate`'s, after this. -/
def WorkflowProgram.parseTemplate (input : String) : Except String WorkflowProgram := do
  let stream ← match Yaml.lYamlStream.run (Yaml.ensureTrailingNewline input) with
    | .ok _ s    => .ok s
    | .error _ e => .error s!"YAML parse error: {e}"
  let doc   ← orError stream.documents[0]? "empty YAML document"
  let root  ← orError doc.root "YAML document has no root node"
  -- Before anything is read, so a placeholder where no event can reach is the error a reader sees.
  refusePlaceholders [] root
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

/-- Parse a workflow an event started, and fill in what the event supplies (`instantiate`). The
    event's variables are never part of the text that is parsed: see `instantiate` for what they
    reach, and why only that. -/
def WorkflowProgram.parseYamlFor (input : String) (vars : List (String × String)) :
    Except String WorkflowProgram :=
  WorkflowProgram.parseTemplate input >>= (·.instantiate vars)

/-- Parse a workflow run by hand, with no event: prompts keep their placeholders, and a step whose
    `issue-number` was to come from an event is refused. -/
def WorkflowProgram.parseYaml (input : String) : Except String WorkflowProgram :=
  WorkflowProgram.parseYamlFor input []

end Orchestra.Workflow
