import Orchestra.Config
import Lean.Data.Json

open Lean (Json)

namespace Orchestra.Gemini

/-- Default Gemini model when none is specified in the task. -/
def defaultModel : String := "gemini-2.0-flash"

private def runCmd (cmd : String) (args : Array String)
    (input : Option String := none) : IO String := do
  let child ← IO.Process.spawn {
    cmd, args
    stdin  := if input.isSome then .piped else .null
    stdout := .piped
    stderr := .piped
  }
  if let some s := input then
    let (stdinHandle, child') ← child.takeStdin
    stdinHandle.putStr s
    stdinHandle.flush
    let stdout ← child'.stdout.readToEnd
    let stderr ← child'.stderr.readToEnd
    let code ← child'.wait
    if code != 0 then
      throw (.userError s!"{cmd} failed (exit {code}):\n{stderr}")
    return stdout.trimAscii.toString
  let stdout ← child.stdout.readToEnd
  let stderr ← child.stderr.readToEnd
  let code ← child.wait
  if code != 0 then
    throw (.userError s!"{cmd} failed (exit {code}):\n{stderr}")
  return stdout.trimAscii.toString

/-- Query the Gemini generateContent API with the given prompt and return the response text.
    The request body is piped to curl via stdin to safely handle large prompts and special chars.
    An optional system prompt is passed as a `system_instruction` if provided. -/
def queryGemini (apiKey : String) (model : String) (prompt : String)
    (systemPrompt : Option String := none) : IO String := do
  let contentsPart := Json.arr #[Json.mkObj [
    ("role", .str "user"),
    ("parts", Json.arr #[Json.mkObj [("text", .str prompt)]])
  ]]
  let fields : List (String × Json) :=
    match systemPrompt with
    | none    => [("contents", contentsPart)]
    | some sp =>
      [ ("system_instruction", Json.mkObj
          [("parts", Json.arr #[Json.mkObj [("text", .str sp)]])])
      , ("contents", contentsPart) ]
  let requestBody := Json.mkObj fields
  let url :=
    s!"https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent?key={apiKey}"
  let response ← runCmd "curl"
    #["-s", "-f", "-X", "POST",
      "-H", "Content-Type: application/json",
      "--data-binary", "@-",
      url]
    (input := some requestBody.compress)
  match Json.parse response with
  | .error e =>
    throw (.userError s!"Gemini response parse error: {e}\nRaw: {response}")
  | .ok j =>
    if let .ok errObj := j.getObjVal? "error" then
      let msg := errObj.getObjValAs? String "message" |>.toOption |>.getD j.compress
      throw (.userError s!"Gemini API error: {msg}")
    let .ok (.arr candidates) := j.getObjVal? "candidates"
      | throw (.userError s!"Gemini response missing 'candidates': {j.compress}")
    let some candidate := candidates[0]?
      | throw (.userError "Gemini response has empty 'candidates' array")
    let .ok content := candidate.getObjVal? "content"
      | throw (.userError s!"Gemini candidate missing 'content': {candidate.compress}")
    let .ok (.arr parts) := content.getObjVal? "parts"
      | throw (.userError s!"Gemini content missing 'parts': {content.compress}")
    let some part := parts[0]?
      | throw (.userError "Gemini content has empty 'parts' array")
    let .ok text := part.getObjValAs? String "text"
      | throw (.userError s!"Gemini part missing 'text': {part.compress}")
    return text

/-- Resolve the Gemini API key from the application configuration.
    Looks up the "gemini" backend in `appConfig.agentAuthConfigs` and extracts the API key,
    following the same label-resolution rules as other backends:
    explicit auth_source label > default_auth_source > sole configured source. -/
def resolveApiKey (appConfig : AppConfig) (requestedLabel : Option String) : IO String := do
  let some agentAuth := appConfig.agentAuthConfigs.find? (fun c => c.name == "gemini")
    | throw (.userError
        "No auth sources configured for the 'gemini' backend. \
         Add an entry with name 'gemini' and an 'api_key' auth source to the \
         'agents' array in your orchestra config.")
  let label ← match requestedLabel with
    | some l => pure l
    | none   =>
      match agentAuth.defaultAuthSource with
      | some d => pure d
      | none   =>
        if agentAuth.authSources.size == 1 then
          pure agentAuth.authSources[0]!.label
        else
          throw (.userError
            "Multiple auth sources configured for the 'gemini' backend. \
             Specify one via the 'auth_source' field in the task.")
  let some src := agentAuth.authSources.find? (fun s => s.label == label)
    | throw (.userError
        s!"Auth source '{label}' not found for the 'gemini' backend. \
           Available: {", ".intercalate (agentAuth.authSources.toList.map (·.label))}")
  match src.kind with
  | .apiKey key _ => return key
  | .oauthToken _ =>
    throw (.userError
      s!"Auth source '{label}' for the 'gemini' backend uses an OAuth token, \
         but Gemini requires an 'api_key' auth source.")

end Orchestra.Gemini
