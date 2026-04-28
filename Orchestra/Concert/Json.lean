import Orchestra.Concert.Basic

open Lean (Json FromJson ToJson)

namespace Orchestra.Concert

-- Unit has no meaningful JSON value; any JSON round-trips as ()
private instance : FromJson Unit where
  fromJson? _ := .ok ()

-- Unit serializes as JSON null
private instance : ToJson Unit where
  toJson _ := .null

instance {i o : ResultType} : ToJson (IOTask i o) where
  toJson t := Json.mkObj
    [ ("upstream",      .str t.upstream)
    , ("fork",          .str t.fork)
    , ("mode",          ToJson.toJson t.mode)
    , ("prompt",        .str t.prompt)
    , ("agent",         ToJson.toJson t.agent)
    , ("system_prompt", ToJson.toJson t.systemPrompt)
    , ("backend",       ToJson.toJson t.backend)
    , ("model",         ToJson.toJson t.model)
    , ("budget",        ToJson.toJson t.budget)
    , ("memory",        ToJson.toJson t.memory)
    , ("auth_source",   ToJson.toJson t.authSource)
    , ("tools",         ToJson.toJson t.tools)
    , ("read_only",     .bool t.readOnly)
    , ("series",        ToJson.toJson t.series)
    , ("input_type",    ToJson.toJson i)
    , ("output_type",   ToJson.toJson o)
    ]

instance {i o : ResultType} : FromJson (IOTask i o) where
  fromJson? j := do
    let upstream     ← j.getObjValAs? String "upstream"
    let fork         ← j.getObjValAs? String "fork"
    let mode         ← j.getObjValAs? TaskMode "mode"
    let prompt       ← j.getObjValAs? String "prompt"
    let agent        := j.getObjValAs? String "agent" |>.toOption
    let systemPrompt := j.getObjValAs? String "system_prompt" |>.toOption
    let backend      := j.getObjValAs? String "backend" |>.toOption
    let model        := j.getObjValAs? String "model" |>.toOption
    let budget       := j.getObjValAs? Float "budget" |>.toOption
    let memory       := j.getObjValAs? MemoryMode "memory" |>.toOption |>.getD .both
    let authSource   := j.getObjValAs? String "auth_source" |>.toOption
    let tools        := j.getObjValAs? (List String) "tools" |>.toOption
    let readOnly     := j.getObjValAs? Bool "read_only" |>.toOption |>.getD false
    let series       := j.getObjValAs? String "series" |>.toOption
    return { upstream, fork, mode, prompt, agent, systemPrompt, backend, model,
             budget, memory, authSource, tools, readOnly, series }

/-- Serialize a `Concert α` to JSON.
    For `run` nodes, the task spec is computed from the current input and the continuation is
    serialized by applying it to the default output value — faithful for unit-output tasks. -/
private partial def concertToJson {α : Type} [ToJson α] : Concert α → Json
  | .pure a    => Json.mkObj [("type", "pure"), ("value", ToJson.toJson a)]
  | .abort     => Json.mkObj [("type", "abort")]
  | .op (.run o t input) k =>
      Json.mkObj [ ("type",        "run")
                 , ("output_type", ToJson.toJson o)
                 , ("task",        ToJson.toJson (t.toIOTask input))
                 , ("then",        concertToJson (k default))
                 ]
  | .op (.while cond body) k =>
      Json.mkObj [ ("type",  "while")
                 , ("cond",  concertToJson cond)
                 , ("body",  concertToJson body)
                 , ("then",  concertToJson (k ()))
                 ]

instance {α : Type} [ToJson α] : ToJson (Concert α) where
  toJson := concertToJson

-- `Concert α : Type 1`, so `Except String (Concert α) : Type 1`.  Using `do` in
-- `concertFromJson` would require mixing `Except.{0,1}` with `Except.{0,0}` results
-- (e.g. `getObjValAs? String "type" : Except.{0,0}`). We use explicit pattern matching
-- instead to keep universe levels uniform.

/-- Deserialize a `Concert α` from JSON.
    `run` nodes are reconstructed with a unit input and a continuation that ignores the
    output — faithful for unit-input/output tasks. -/
private partial def concertFromJson {α : Type} [FromJson α] [Inhabited α]
    (j : Json) : Except String (Concert α) :=
  match j.getObjValAs? String "type" with
  | .error e    => .error e
  | .ok "pure"  =>
      .ok (.pure ((j.getObjValAs? α "value").toOption.getD default))
  | .ok "abort" => .ok .abort
  | .ok "run"   =>
      match j.getObjValAs? ResultType "output_type",
            j.getObjVal? "task",
            j.getObjVal? "then" with
      | .ok o, .ok taskJ, .ok thnJ =>
          match FromJson.fromJson? (α := IOTask .unit o) taskJ,
                concertFromJson (α := α) thnJ with
          | .ok spec, .ok rest =>
              .ok (.op (.run o { toIOTask := fun _ => spec } ()) (fun _ => rest))
          | .error e, _ => .error e
          | _, .error e => .error e
      | .error e, _, _ => .error e
      | _, .error e, _ => .error e
      | _, _, .error e => .error e
  | .ok "while" =>
      match j.getObjVal? "cond",
            j.getObjVal? "body",
            j.getObjVal? "then" with
      | .ok condJ, .ok bodyJ, .ok thnJ =>
          match concertFromJson (α := Bool) condJ,
                concertFromJson (α := Unit) bodyJ,
                concertFromJson (α := α) thnJ with
          | .ok cond, .ok body, .ok rest =>
              .ok (.op (.while cond body) (fun _ => rest))
          | .error e, _, _ => .error e
          | _, .error e, _ => .error e
          | _, _, .error e => .error e
      | .error e, _, _ => .error e
      | _, .error e, _ => .error e
      | _, _, .error e => .error e
  | .ok t => .error s!"unknown Concert node type: {t}"

instance {α : Type} [FromJson α] [Inhabited α] : FromJson (Concert α) where
  fromJson? := concertFromJson

end Orchestra.Concert
