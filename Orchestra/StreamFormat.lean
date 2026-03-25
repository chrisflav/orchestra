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

/-- A content item within an assistant message. -/
inductive ContentItem where
  | thinking (text : String)
  | toolUse (name : String) (input : Json)
  | text (text : String)

/-- The subtype of a result event emitted by the agent. -/
inductive ResultSubtype where
  | success
  | errorMaxBudgetUsd
  | unknown (raw : String)
deriving BEq, Repr

/-- A parsed stream-json event. -/
inductive Event where
  | init (sessionId : String) (model : String)
  | system (subtype : String)
  | assistant (item : ContentItem)
  | toolResult (stdout : String) (stderr : String)
  | result (subtype : ResultSubtype) (numTurns : Option Nat) (durationMs : Option Nat)
            (costUsd : Option Json) (res : String)
  | unknown (type : String)

-- Serialisation

private def resultSubtypeStr : ResultSubtype → String
  | .success          => "success"
  | .errorMaxBudgetUsd => "error_max_budget_usd"
  | .unknown raw      => raw

instance : ToJson ResultSubtype where
  toJson sub := Json.str (resultSubtypeStr sub)

instance : ToJson ContentItem where
  toJson
    | .thinking t    => Json.mkObj [("type", "thinking"), ("text", t)]
    | .toolUse n inp => Json.mkObj [("type", "tool_use"), ("name", n), ("input", inp)]
    | .text t        => Json.mkObj [("type", "text"), ("text", t)]

instance : ToJson Event where
  toJson
    | .init sid model =>
      Json.mkObj [("type", "init"), ("session_id", sid), ("model", model)]
    | .system sub =>
      Json.mkObj [("type", "system"), ("subtype", sub)]
    | .assistant ci =>
      Json.mkObj [("type", "assistant"), ("item", ToJson.toJson ci)]
    | .toolResult stdout stderr =>
      Json.mkObj [("type", "tool_result"), ("stdout", stdout), ("stderr", stderr)]
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
    | .unknown t =>
      Json.mkObj [("type", "unknown"), ("event_type", t)]

-- Parsing

private def parseContentItem (item : Json) : Option ContentItem :=
  match jStr item "type" with
  | "thinking" =>
    let t := jStr item "thinking"
    if t.isEmpty then none else some (.thinking t)
  | "tool_use" =>
    some (.toolUse (jStr item "name") (jVal item "input" |>.getD (Json.mkObj [])))
  | "text" =>
    let t := jStr item "text"
    if t.isEmpty then none else some (.text t)
  | _ => none

/-- Parse a stream-json event line into a typed `Event`.
    Returns `none` for suppressed events (rate limits, empty tool output). -/
def parseEvent (line : String) : Option Event := do
  let json ← (Json.parse line.trimAscii.toString).toOption
  match jStr json "type" with
  | "system" =>
    let sub := jStr json "subtype"
    if sub == "init" then
      some (.init (jStr json "session_id") (jStr json "model"))
    else
      some (.system sub)
  | "assistant" =>
    let msg ← jVal json "message"
    let items ← jArr msg "content"
    let item ← items.back?
    let ci ← parseContentItem item
    some (.assistant ci)
  | "user" =>
    let tr ← jVal json "tool_use_result"
    let stdout := jStr tr "stdout"
    let stderr := jStr tr "stderr"
    if stdout.isEmpty && stderr.isEmpty then none
    else some (.toolResult stdout stderr)
  | "result" =>
    let sub := match jStr json "subtype" with
      | "success"              => ResultSubtype.success
      | "error_max_budget_usd" => ResultSubtype.errorMaxBudgetUsd
      | raw                    => ResultSubtype.unknown raw
    some (.result
      sub
      (json.getObjValAs? Nat "num_turns" |>.toOption)
      (json.getObjValAs? Nat "duration_ms" |>.toOption)
      (jVal json "total_cost_usd")
      (jStr json "result"))
  | "rate_limit_event" => none
  | other => some (.unknown other)

-- Formatting

private def formatContentItem : ContentItem → String
  | .thinking t => s!"[thinking] {truncate t}"
  | .toolUse name input =>
    let desc := jStr input "description"
    let cmd := jStr input "command"
    let fp := jStr input "file_path"
    let pat := jStr input "pattern"
    let detail :=
      if !cmd.isEmpty then
        let header := if !desc.isEmpty then s!"{desc}\n" else ""
        s!"{header}  > {truncate cmd}"
      else if !fp.isEmpty then fp
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
  | .toolResult stdout stderr =>
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
    let resPart := if res.isEmpty then "" else s!"\n{truncate res 300}"
    s!"[done] {resultSubtypeStr sub}{turns}{dur}{cost}{resPart}"
  | .unknown t => s!"[{t}]"

/-- Parse and format a single stream-json event line for human-readable display.
    Returns `none` if the event should be suppressed. -/
def formatEvent (line : String) : Option String :=
  parseEvent line |>.map format

end Orchestra.StreamFormat
