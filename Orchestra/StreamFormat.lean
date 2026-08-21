import Lean.Data.Json

open Lean (Json ToJson)

namespace Orchestra.StreamFormat

-- Helpers

private def truncate (s : String) (n : Nat := 150) : String :=
  let s := s.replace "\n" " "
  if s.length ≤ n then s
  else String.ofList (s.toList.take n) ++ "..."

private def jStr (j : Json) (key : String) : String :=
  j.getObjValAs? String key |>.toOption |>.getD ""

private def jVal (j : Json) (key : String) : Option Json :=
  j.getObjVal? key |>.toOption

private def jArr (j : Json) (key : String) : Option (Array Json) :=
  match j.getObjVal? key with
  | .ok (.arr a) => some a
  | _ => none

-- Types

/-- A content item within an assistant message.

    `toolUse` carries the provider's own id for the call when the backend reports one. It is
    what pairs a call with the result that answers it: a turn can have several calls in flight,
    and without the id a reader can only guess which `toolResult` belongs to which. `none` for
    a backend that does not report one, which is why it is optional rather than empty. -/
inductive ContentItem where
  | thinking (text : String)
  | toolUse (name : String) (input : Json) (toolUseId : Option String)
  | text (text : String)

/-- The subtype of a result event emitted by the agent. -/
inductive ResultSubtype where
  | success
  | errorMaxBudgetUsd
  | error (msg : String)
  | unknown (raw : String)
deriving BEq, Repr

/-- A parsed stream-json event. -/
inductive Event where
  | init (sessionId : String) (model : String)
  | system (subtype : String)
  | assistant (item : ContentItem)
  | toolResult (stdout : String) (stderr : String) (toolUseId : Option String)
  | result (subtype : ResultSubtype) (numTurns : Option Nat) (durationMs : Option Nat)
            (costUsd : Option Json) (res : String)
  /-- A `rate_limit_event` from the agent's stream. Its payload is undocumented and has changed
      shape between CLI releases, so only the reset timestamp is lifted out — and only if one is
      present anywhere in it. Never displayed; it exists so the usage monitor can learn a real
      reset time instead of guessing one. -/
  | rateLimit (resetsAt : Option String)
  | unknown (type : String)

-- Serialisation

private def resultSubtypeStr : ResultSubtype → String
  | .success           => "success"
  | .errorMaxBudgetUsd => "error_max_budget_usd"
  | .error _           => "error"
  | .unknown raw       => raw

instance : ToJson ResultSubtype where
  toJson sub := Json.str (resultSubtypeStr sub)

/-- An optional string as a field: present as itself, absent as `null`. Absent is `null` rather
    than an omitted key, so a reader never has to tell "no id" from "this version did not say". -/
private def optStr : Option String → Json
  | some s => Json.str s
  | none   => Json.null

instance : ToJson ContentItem where
  toJson
    | .thinking t    => Json.mkObj [("type", "thinking"), ("text", t)]
    | .toolUse n inp id =>
      Json.mkObj [("type", "tool_use"), ("name", n), ("input", inp),
                  ("tool_use_id", optStr id)]
    | .text t        => Json.mkObj [("type", "text"), ("text", t)]

instance : ToJson Event where
  toJson
    | .init sid model =>
      Json.mkObj [("type", "init"), ("session_id", sid), ("model", model)]
    | .system sub =>
      Json.mkObj [("type", "system"), ("subtype", sub)]
    | .assistant ci =>
      Json.mkObj [("type", "assistant"), ("item", ToJson.toJson ci)]
    | .toolResult stdout stderr id =>
      Json.mkObj [("type", "tool_result"), ("stdout", stdout), ("stderr", stderr),
                  ("tool_use_id", optStr id)]
    | .result sub numTurns durationMs costUsd res =>
      let fields : List (String × Json) := [
        ("type",    "result"),
        ("subtype", ToJson.toJson sub),
        ("result",  res)
      ]
      let fields := match numTurns  with | some n => fields ++ [("num_turns",   ToJson.toJson n)]   | none => fields
      let fields := match durationMs with | some n => fields ++ [("duration_ms", ToJson.toJson n)]   | none => fields
      let fields := match costUsd   with | some v => fields ++ [("total_cost_usd", v)] | none => fields
      Json.mkObj fields
    | .rateLimit resetsAt =>
      let fields : List (String × Json) := [("type", "rate_limit")]
      let fields := match resetsAt with
        | some r => fields ++ [("resets_at", Json.str r)]
        | none   => fields
      Json.mkObj fields
    | .unknown t =>
      Json.mkObj [("type", "unknown"), ("event_type", t)]

-- Parsing

private def jStr? (j : Json) (key : String) : Option String :=
  match j.getObjValAs? String key with
  | .ok s => if s.isEmpty then none else some s
  | _     => none

private def parseContentItem (item : Json) : Option ContentItem :=
  match jStr item "type" with
  | "thinking" =>
    let t := jStr item "thinking"
    if t.isEmpty then none else some (.thinking t)
  | "tool_use" =>
    some (.toolUse (jStr item "name") (jVal item "input" |>.getD (Json.mkObj []))
                   (jStr? item "id"))
  | "text" =>
    let t := jStr item "text"
    if t.isEmpty then none else some (.text t)
  | _ => none

/-- Pull the value of a `"key": "value"` pair out of a raw JSON line.

    Deliberately textual rather than structural. It is used on `rate_limit_event`, whose payload
    is undocumented and whose nesting has moved between CLI releases; a scan for the key finds it
    wherever it currently sits, where a path-based lookup would silently return nothing the next
    time it moves. Nothing depends on the result being present, so a false negative costs only a
    less precise reset time. -/
def extractStringField (s key : String) : Option String := do
  let parts := s.splitOn ("\"" ++ key ++ "\"")
  let after ← (parts.drop 1).head?
  let rest := after.toList.dropWhile fun c => c == ' ' || c == ':'
  guard (rest.head? == some '"')
  let val := (rest.drop 1).takeWhile (· != '"')
  guard !val.isEmpty
  return String.ofList val

/-- The `tool_result` content block of a `user` line, if it has one.

    This is where the id that pairs a result with its call lives — `message.content[i]
    .tool_use_id`. It is emphatically not at the top level of the line, and not inside
    `tool_use_result`, which is the raw tool return value and differs per tool. -/
private def toolResultBlock (json : Json) : Option Json := do
  let msg ← jVal json "message"
  let items ← jArr msg "content"
  items.find? fun b => jStr b "type" == "tool_result"

/-- What a `tool_result` block says, as text. Its `content` is either a bare string or a list of
    text blocks. -/
private def blockText (block : Json) : String :=
  match jVal block "content" with
  | some (.str s)   => s
  | some (.arr its) =>
    String.intercalate "\n" <| its.toList.filterMap fun i =>
      if jStr i "type" == "text" then
        let t := jStr i "text"
        if t.isEmpty then none else some t
      else none
  | _ => ""

/-- The line types that carry exactly one event. Split out so that `parseEvents` below reads as
    the dispatch it is: the two interesting cases are the ones that do not answer one event. -/
private def parseSingle (line : String) (json : Json) : Option Event :=
  match jStr json "type" with
  | "system" =>
    let sub := jStr json "subtype"
    if sub == "init" then
      some (.init (jStr json "session_id") (jStr json "model"))
    else
      some (.system sub)
  | "user" =>
    -- A `user` line is a tool result only when it carries `tool_use_result`. Without that key
    -- it is an ordinary user message, and the CLI emits plenty of those: feedback from a `Stop`
    -- hook (which is exactly what `AgentDef.goalArgs` installs), "continue from where you left
    -- off" after an interruption, and other synthetic injections. Treating those as tool
    -- results puts a result into the log for a call that never happened. The CLI discriminates
    -- the same way — `type === "user" && toolUseResult === undefined` is not a result to it
    -- either.
    match jVal json "tool_use_result" with
    | none => none
    | some tr =>
      -- Only Bash reports `stdout`/`stderr` here; every other tool's answer is in the
      -- `tool_result` content block, as a string or as text blocks. Reading just the two
      -- streams rendered every `Read` and `Edit` as an empty `[output]` — which says the tool
      -- returned nothing, when it returned something this parser could not see.
      let block  := toolResultBlock json
      let stdout := jStr tr "stdout"
      let stderr := jStr tr "stderr"
      let stdout :=
        if stdout.isEmpty && stderr.isEmpty then (block.map blockText).getD "" else stdout
      -- What is still kept is a result that really is empty: "the command produced no output"
      -- is a fact a reader needs, and dropped, a tool call reads as one that never returned.
      some (.toolResult stdout stderr (block.bind (jStr? · "tool_use_id")))
  | "result" =>
    let isError := json.getObjValAs? Bool "is_error" |>.toOption |>.getD false
    let res := jStr json "result"
    let sub :=
      if isError then ResultSubtype.error res
      else match jStr json "subtype" with
        | "success"              => ResultSubtype.success
        | "error_max_budget_usd" => ResultSubtype.errorMaxBudgetUsd
        | raw                    => ResultSubtype.unknown raw
    some (.result
      sub
      (json.getObjValAs? Nat "num_turns" |>.toOption)
      (json.getObjValAs? Nat "duration_ms" |>.toOption)
      (jVal json "total_cost_usd")
      res)
  | "rate_limit_event" =>
    some (.rateLimit ((extractStringField line "resets_at").orElse fun _ =>
      extractStringField line "resetsAt"))
  | other => some (.unknown other)

/-- Parse a stream-json event line into the typed events it carries.

    An array rather than an `Option`, because one line is not one event. An assistant message
    carries a *list* of content items, and a turn that thinks, then says what it is about to do,
    then calls a tool is all three of those on one line. Keeping only the last — which is what
    this did before — rendered that turn as the tool call alone, with the reasoning and the
    narration dropped on the floor. In a log that was a lossy summary; in a transcript someone
    reads as a conversation, it is the conversation missing.

    An empty array is a line with nothing to show: one that does not parse, or an assistant
    message whose every item was empty. -/
def parseEvents (line : String) : Array Event := Id.run do
  let some json := (Json.parse line.trimAscii.toString).toOption | return #[]
  if jStr json "type" == "assistant" then
    let some msg := jVal json "message" | return #[]
    let some items := jArr msg "content" | return #[]
    return items.filterMap fun item => (parseContentItem item).map Event.assistant
  match parseSingle line json with
  | some e => return #[e]
  | none   => return #[]

/-- One event, or none, in the array `AgentDef.parseOutputLine` answers in. For the backends
    that really do emit exactly one event per line. -/
def one : Option Event → Array Event
  | some e => #[e]
  | none   => #[]

/-- The first event a line carries, or `none` for a line that carries none.

    For the callers that know their line is one event — every backend but Claude emits exactly
    one per line — and for tests, which assert about a single known line. -/
def parseEvent (line : String) : Option Event :=
  (parseEvents line)[0]?

-- Formatting

private def formatContentItem : ContentItem → String
  | .thinking t => s!"[thinking] {truncate t}"
  | .toolUse name input _ =>
    let desc := jStr input "description"
    let cmd := jStr input "command"
    let fp := jStr input "file_path"
    let fpAlt := jStr input "filePath"
    let fpFinal := if !fp.isEmpty then fp else fpAlt
    let pat := jStr input "pattern"
    let detail :=
      if !cmd.isEmpty then
        let header := if !desc.isEmpty then s!"{desc}\n" else ""
        s!"{header}  > {truncate cmd}"
      else if !fpFinal.isEmpty then fpFinal
      else if !pat.isEmpty then s!"pattern: {pat}"
      else desc
    s!"[tool] {name}: {detail}"
  | .text t => s!"[text] {truncate t 300}"

/-- Format a typed `Event` as a human-readable string. -/
def format : Event → String
  | .init sid model =>
    let sidShort := if sid.length > 8 then String.ofList (sid.toList.take 8) ++ "..." else sid
    s!"[init] session={sidShort} model={model}"
  | .system sub => s!"[system] {sub}"
  | .assistant ci => formatContentItem ci
  | .toolResult stdout stderr _ =>
    let outPart :=
      if stdout.isEmpty then ""
      else
        let lines := stdout.splitOn "\n"
        if lines.length > 5 then
          let preview := String.intercalate "\n  " (lines.take 3)
          s!"\n  {preview}\n  ... ({lines.length} lines)"
        else
          s!"\n  {String.intercalate "\n  " lines}"
    let errPart := if stderr.isEmpty then "" else s!"\n  stderr: {truncate stderr}"
    s!"[output]{outPart}{errPart}"
  | .result sub numTurns durationMs costUsd res =>
    let turns := match numTurns with | some n => s!" | {n} turns" | none => ""
    let dur := match durationMs with | some ms => s!" | {ms / 1000}s" | none => ""
    let cost := match costUsd with | some v => s!" | ${v.compress}" | none => ""
    let resPart := match sub with
      | .error msg => s!"\n{truncate msg 300}"
      | _ => if res.isEmpty then "" else s!"\n{truncate res 300}"
    s!"[done] {resultSubtypeStr sub}{turns}{dur}{cost}{resPart}"
  | .rateLimit resetsAt =>
    match resetsAt with
    | some r => s!"[rate-limit] resets at {r}"
    | none   => "[rate-limit]"
  | .unknown t => s!"[{t}]"

/-- Parse and format a single stream-json event line for human-readable display.
    Returns `none` if the event should be suppressed. -/
def formatEvent (line : String) : Option String :=
  parseEvent line |>.map format

end Orchestra.StreamFormat
