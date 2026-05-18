import Yaml
import Lean.Data.Json

open Yaml (Node ScalarStyle Tag)
open Lean (Json)

namespace Orchestra.YamlConfig

/-- Convert a YAML node to a JSON value.
    - Quoted/literal/folded scalars → always `Json.str`
    - Plain scalars with an explicit `!!str` tag → `Json.str`
    - Plain empty scalar or `~`/`null` → `Json.null`
    - Other plain scalars → type-coerced via `Json.parse` (handles true/false/numbers);
      falls back to `Json.str` if parsing fails.
    - Sequences → `Json.arr`
    - Mappings → `Json.obj` (non-scalar keys are silently dropped)
    - Aliases → `Json.null` (resolution not supported in this context) -/
partial def yamlNodeToJson : Node → Json
  | .scalar props style value =>
    let forceString := style != .plain || props.tag == some Tag.str
    if forceString then .str value
    else if value.isEmpty || value == "~" || value == "null" then .null
    else
      match Lean.Json.parse value with
      | .ok j  => j
      | .error _ => .str value
  | .sequence _ _ items => .arr (items.map yamlNodeToJson)
  | .mapping _ _ pairs =>
    let kvs := (pairs.filterMap fun (k, v) =>
      match k with
      | .scalar _ _ s => some (s, yamlNodeToJson v)
      | _ => none).toList
    Lean.Json.mkObj kvs
  | .alias _ => .null

/-- Parse a YAML text string, returning the first document as a `Json` value.
    Returns `.null` for an empty document. -/
def parseYamlToJson (text : String) : Except String Json := do
  let stream ← match Yaml.lYamlStream.run (Yaml.ensureTrailingNewline text) with
    | .ok _ s    => .ok s
    | .error _ e => .error s!"YAML parse error: {e}"
  let doc ← match stream.documents[0]? with
    | some d => .ok d
    | none   => .error "empty YAML document"
  return match doc.root with
  | none      => .null
  | some node => yamlNodeToJson node

end Orchestra.YamlConfig
