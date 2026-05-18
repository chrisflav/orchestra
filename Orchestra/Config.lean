import Lean.Data.Json
import Orchestra.Dirs
import Orchestra.Project.Id

open Lean (Json FromJson ToJson)

namespace Orchestra
open Orchestra.Project (ProjectId IssueId)

/-- A GitHub repository identified by its owner and name. -/
structure Repository where
  owner : String
  name  : String
deriving BEq, Repr, Inhabited

/-- Parse `"owner/repo"` into a `Repository`. -/
def Repository.parse (s : String) : Except String Repository :=
  match s.splitOn "/" with
  | [owner, repo] => .ok { owner, name := repo }
  | _ => .error s!"invalid repository format '{s}', expected 'owner/repo'"

/-- Return the canonical `"owner/repo"` string. -/
def Repository.toString (r : Repository) : String := s!"{r.owner}/{r.name}"

instance : ToString Repository where
  toString r := r.toString

instance : ToJson Repository where
  toJson r := Json.str r.toString

instance : FromJson Repository where
  fromJson? j := do
    let s ← FromJson.fromJson? (α := String) j
    Repository.parse s |>.mapError id

inductive TaskMode where
  | fork
  | pr
deriving Repr, Inhabited

instance : FromJson TaskMode where
  fromJson?
    | .str "fork" => .ok .fork
    | .str "pr" => .ok .pr
    | j => .error s!"expected \"fork\" or \"pr\", got {j}"

instance : ToJson TaskMode where
  toJson
    | .fork => "fork"
    | .pr => "pr"

/-- Controls which memory directories are made available to the agent.
    - `none`    – no memory directories
    - `global`  – global memory only (`<data>/memory/`)
    - `project` – per-project memory only (`<data>/memory/<project>/`)
    - `both`    – global and per-project memory (default) -/
inductive MemoryMode where
  | none
  | global
  | project
  | both
deriving Repr, Inhabited

instance : FromJson MemoryMode where
  fromJson?
    | .str "none"    => .ok .none
    | .str "global"  => .ok .global
    | .str "project" => .ok .project
    | .str "both"    => .ok .both
    | j => .error s!"expected \"none\", \"global\", \"project\", or \"both\", got {j}"

instance : ToJson MemoryMode where
  toJson
    | .none    => "none"
    | .global  => "global"
    | .project => "project"
    | .both    => "both"

/-- The set of types that tasks may use as input or output. -/
inductive ResultType where
  | string
  | int
  | nat
  | bool
  | list (t : ResultType)
  | unit
  | mapping (fields : List (String × ResultType))
  deriving Repr, BEq, Inhabited

/-- The Lean `Type` corresponding to a `ResultType`. -/
abbrev ResultType.Type : ResultType → Type
  | .string      => String
  | .unit        => Unit
  | .bool        => Bool
  | .nat         => Nat
  | .int         => Int
  | .list t      => List t.Type
  | .mapping _   => Json

instance (t : ResultType) : Inhabited t.Type :=
  match t with
  | .string    => ⟨""⟩
  | .unit      => ⟨()⟩
  | .bool      => ⟨false⟩
  | .nat       => ⟨0⟩
  | .int       => ⟨0⟩
  | .list _    => ⟨[]⟩
  | .mapping _ => ⟨.null⟩

private partial def resultTypeFromJson : Json → Except String ResultType
  | .str "string" => .ok .string
  | .str "int"    => .ok .int
  | .str "nat"    => .ok .nat
  | .str "bool"   => .ok .bool
  | .str "unit"   => .ok .unit
  | j =>
      match j.getObjVal? "list" |>.toOption with
      | some inner => resultTypeFromJson inner |>.map .list
      | none       =>
        match j.getObjVal? "mapping" |>.toOption with
        | some arr => do
            let pairs ← arr.getArr?
            let fields ← pairs.toList.mapM fun p => do
              let key ← p.getObjValAs? String "key"
              let typ ← resultTypeFromJson (← p.getObjVal? "type")
              return (key, typ)
            return .mapping fields
        | none => .error s!"expected ResultType, got {j.compress}"

instance : FromJson ResultType where
  fromJson? := resultTypeFromJson

private partial def resultTypeToJson : ResultType → Json
  | .string      => "string"
  | .int         => "int"
  | .nat         => "nat"
  | .bool        => "bool"
  | .unit        => "unit"
  | .list t      => Json.mkObj [("list", resultTypeToJson t)]
  | .mapping fs  =>
      let arr := fs.map fun (k, t) => Json.mkObj [("key", k), ("type", resultTypeToJson t)]
      Json.mkObj [("mapping", .arr arr.toArray)]

instance : ToJson ResultType where
  toJson := resultTypeToJson

/-- Human-readable description of a value of the given result type, for use in tool descriptions. -/
partial def ResultType.toDescription : ResultType → String
  | .string      => "a JSON string"
  | .int         => "a JSON integer (may be negative)"
  | .nat         => "a JSON non-negative integer"
  | .bool        => "a JSON boolean"
  | .unit        => "null"
  | .list t      => s!"a JSON array where each element is {t.toDescription}"
  | .mapping fs  =>
      let fields := fs.map fun (k, t) => "\"" ++ k ++ "\": " ++ t.toDescription
      "a JSON object with fields: {" ++ String.intercalate ", " fields ++ "}"

/-- JSON Schema describing values of the given result type. -/
partial def ResultType.toJsonSchema : ResultType → Json
  | .string     => Json.mkObj [("type", "string")]
  | .int        => Json.mkObj [("type", "integer")]
  | .nat        => Json.mkObj [("type", "integer")]
  | .bool       => Json.mkObj [("type", "boolean")]
  | .unit       => Json.mkObj [("type", "null")]
  | .list t     => Json.mkObj [("type", "array"), ("items", t.toJsonSchema)]
  | .mapping fs =>
      let props := Json.mkObj (fs.map fun (k, t) => (k, t.toJsonSchema))
      let req   := Json.arr (fs.map (fun (k, _) => Json.str k) |>.toArray)
      Json.mkObj [("type", "object"), ("properties", props), ("required", req)]

/-- Serialize a value of the Lean type corresponding to `t` into JSON. -/
partial def ResultType.valueToJson : (t : ResultType) → t.Type → Json
  | .string, s    => ToJson.toJson s
  | .int, i       => ToJson.toJson i
  | .nat, n       => ToJson.toJson n
  | .bool, b      => ToJson.toJson b
  | .unit, ()     => .null
  | .list t, l    => .arr (l.map (ResultType.valueToJson t) |>.toArray)
  | .mapping _, j => j

/-- Deserialize a JSON value into the Lean type corresponding to `t`. -/
partial def ResultType.valueFromJson : (t : ResultType) → Json → Except String t.Type
  | .string, j    => FromJson.fromJson? j
  | .int, j       => FromJson.fromJson? j
  | .nat, j       => FromJson.fromJson? j
  | .bool, j      => FromJson.fromJson? j
  | .unit, _      => .ok ()
  | .list t, j    => do
      let arr ← j.getArr?
      arr.toList.mapM (ResultType.valueFromJson t)
  | .mapping fs, j => do
      let _ ← j.getObj?
      for (key, t) in fs do
        let v ← j.getObjVal? key
        let _ ← ResultType.valueFromJson t v
      return j

/-- A typed task with phantom input type `i` and output type `o`. -/
structure IOTask (i o : ResultType) where
  upstream : Repository
  fork : Repository
  /-- Legacy mode field (deprecated). Use `tools` instead.
      If `tools` is absent, this field is used to derive the allowed tools:
      - `fork` → no tools
      - `pr`   → `["create_pr"]` -/
  mode : TaskMode
  prompt : String
  agent : Option String := none
  systemPrompt : Option String := none
  prependPrompt : Option String := none
  /-- Agent backend to use: "claude" (default) or "vibe". -/
  backend : Option String := none
  /-- Model override passed to the agent (e.g. "sonnet", "devstral-small"). -/
  model : Option String := none
  /-- Maximum spend in USD. Defaults to 4.0 if not set. -/
  budget : Option Float := none
  /-- Which memory directories to make available to the agent. Defaults to `both`. -/
  memory : MemoryMode := .both
  /-- Label of the authentication source to use for this task.
      Must match a label in the agent's `auth_sources` config. -/
  authSource : Option String := none
  /-- Optional tools to enable beyond the always-available ones (health, refresh_token,
      get_pr_comments). Currently the only optional tool is `"create_pr"`.
      When absent, the allowed tools are derived from `mode` for backwards compatibility. -/
  tools : Option (List String) := none
  /-- If true, the project folder is mounted read-only in the sandbox.
      Useful for tasks that should only read the codebase (e.g. review tasks). -/
  readOnly : Bool := false
  /-- Optional series name for grouping tasks in a sequence. -/
  series : Option String := none
  /-- Priority of this task. Natural number; higher = more important.
      Defaults to 10 if not set. -/
  priority : Nat := 10
  /-- Issue or PR number this task was launched from.
      When set, enables the `comment` tool to post to that issue/PR. -/
  issueNumber : Option Nat := none
  /-- Orchestra project this task belongs to (optional).
      Distinct from `issueNumber`, which is a GitHub issue number. -/
  projectId : Option ProjectId := none
  /-- Orchestra issue this task is working on (optional).
      Set by `claim_issue`; release on terminal status flips it back to `.open`. -/
  issueId : Option IssueId := none
  /-- Optional role name this task was spawned for (e.g. "implementor"). Used
      by the project-dispatcher to count active per-role tasks unambiguously,
      avoiding fragile `tools` list comparisons. -/
  role : Option String := none
deriving Repr, Inhabited

/-- The kind of authentication for an agent backend. -/
inductive AuthKind where
  /-- An OAuth token. -/
  | oauthToken (token : String)
  /-- An API key with an optional base URL. -/
  | apiKey (key : String) (baseUrl : Option String := none)
deriving Repr, Inhabited

instance : FromJson AuthKind where
  fromJson? j := do
    -- Determine the kind from the fields present in the JSON object
    if let .ok token := j.getObjValAs? String "oauth_token" then
      return .oauthToken token
    if let .ok key := j.getObjValAs? String "api_key" then
      let baseUrl := j.getObjValAs? String "base_url" |>.toOption
      return .apiKey key baseUrl
    .error "expected \"oauth_token\" or \"api_key\" field"

instance : ToJson AuthKind where
  toJson
    | .oauthToken token => Json.mkObj [("oauth_token", token)]
    | .apiKey key baseUrl =>
      let fields : List (String × Json) := [("api_key", key)]
      let fields := match baseUrl with
        | some url => fields ++ [("base_url", Json.str url)]
        | none => fields
      Json.mkObj fields

/-- A single authentication source for an agent backend. -/
structure AuthSource where
  /-- Unique label identifying this source within its agent backend. -/
  label : String
  /-- The authentication kind and its credentials. -/
  kind  : AuthKind
deriving Repr, Inhabited

instance : FromJson AuthSource where
  fromJson? j := do
    let label ← j.getObjValAs? String "label"
    let kind ← @FromJson.fromJson? AuthKind _ j
    return { label, kind }

/-- Authentication source configuration for one agent backend. -/
structure AgentAuthConfig where
  /-- Backend name (e.g., `"claude"`, `"vibe"`). -/
  name : String
  /-- Available authentication sources for this backend. Labels must be unique. -/
  authSources : Array AuthSource := #[]
  /-- Label of the default authentication source.
      If absent and exactly one source is configured, that source is used automatically. -/
  defaultAuthSource : Option String := none
  /-- Additional TCP ports the agent is allowed to connect to inside the sandbox.
      Appended to the ports the agent backend already opens (MCP server port + 443). -/
  extraPorts : Array Nat := #[]
deriving Repr, Inhabited

instance : FromJson AgentAuthConfig where
  fromJson? j := do
    let name             ← j.getObjValAs? String "name"
    let authSources       := j.getObjValAs? (Array AuthSource) "auth_sources" |>.toOption |>.getD #[]
    let defaultAuthSource := j.getObjValAs? String "default_auth_source" |>.toOption
    let extraPorts        := j.getObjValAs? (Array Nat) "extra_ports" |>.toOption |>.getD #[]
    return { name, authSources, defaultAuthSource, extraPorts }

structure Task where
  /-- The input type of this task. -/
  i : ResultType
  /-- The output type of this task. -/
  o : ResultType
  /-- The task configuration. -/
  ioTask : IOTask i o
deriving Repr, Inhabited

instance : FromJson Task where
  fromJson? j := do
    let i          := j.getObjValAs? ResultType "input_type"  |>.toOption |>.getD .unit
    let o          := j.getObjValAs? ResultType "output_type" |>.toOption |>.getD .unit
    let upstream   ← j.getObjValAs? Repository "upstream"
    let fork       ← j.getObjValAs? Repository "fork"
    let mode       ← j.getObjValAs? TaskMode "mode"
    let prompt     ← j.getObjValAs? String "prompt"
    let agent      := j.getObjValAs? String "agent"          |>.toOption
    let systemPrompt := j.getObjValAs? String "system_prompt" |>.toOption
    let prependPrompt := j.getObjValAs? String "prepend_prompt" |>.toOption
    let backend    := j.getObjValAs? String "backend"        |>.toOption
    let model      := j.getObjValAs? String "model"          |>.toOption
    let budget     := j.getObjValAs? Float "budget"          |>.toOption
    let memory     := j.getObjValAs? MemoryMode "memory"     |>.toOption |>.getD .both
    let authSource := j.getObjValAs? String "auth_source"    |>.toOption
    let tools      := j.getObjValAs? (List String) "tools"   |>.toOption
    let readOnly   := j.getObjValAs? Bool "read_only"        |>.toOption |>.getD false
    let series      := j.getObjValAs? String "series"          |>.toOption
    let priority    := j.getObjValAs? Nat "priority"           |>.toOption |>.getD 10
    let issueNumber := j.getObjValAs? Nat "issue_number" |>.toOption
    let projectId   := j.getObjValAs? ProjectId "project_id" |>.toOption
    let issueId     := j.getObjValAs? IssueId   "issue_id"   |>.toOption
    let role        := j.getObjValAs? String    "role"       |>.toOption
    return { i, o, ioTask := { upstream, fork, mode, prompt, agent, systemPrompt, prependPrompt, backend, model,
                                budget, memory, authSource, tools, readOnly, series, priority,
                                issueNumber, projectId, issueId, role } }

/-- Filesystem paths to expose inside the landrun sandbox. -/
structure SandboxPaths where
  /-- Absolute paths needing read+execute (binaries/libraries). -/
  rox : List String := []
  /-- Absolute paths needing read-only access. -/
  ro : List String := []
  /-- Absolute paths needing read-write access. -/
  rw : List String := []
  /-- Paths relative to $HOME needing read+execute. -/
  homeRox : List String := []
  /-- Paths relative to $HOME needing read-write access. -/
  homeRw : List String := []
  /-- Additional TCP ports to allow outbound connections to (besides 443 and the MCP server port). -/
  extraPorts : List UInt16 := []
deriving Repr

instance : FromJson SandboxPaths where
  fromJson? j := do
    let rox     := j.getObjValAs? (List String) "rox"      |>.toOption |>.getD []
    let ro      := j.getObjValAs? (List String) "ro"       |>.toOption |>.getD []
    let rw      := j.getObjValAs? (List String) "rw"       |>.toOption |>.getD []
    let homeRox := j.getObjValAs? (List String) "home_rox" |>.toOption |>.getD []
    let homeRw  := j.getObjValAs? (List String) "home_rw"  |>.toOption |>.getD []
    return { rox, ro, rw, homeRox, homeRw }

structure AppConfig where
  appId : Nat
  privateKeyPath : String
  installationId : Option Nat := none
  pat : String := ""
  pluginDirs : Array String := #[]
  /-- Long-lived Claude OAuth token set via `claude setup-token`.
      Exposed to the agent as `CLAUDE_CODE_OAUTH_TOKEN`. -/
  claudeToken : Option String := none
  /-- Anthropic API key passed to the agent as ANTHROPIC_API_KEY. -/
  anthropicApiKey : Option String := none
  /-- Anthropic base URL passed to the agent as ANTHROPIC_BASE_URL. -/
  anthropicBaseUrl : Option String := none
  /-- Anthropic auth token passed to the agent as ANTHROPIC_AUTH_TOKEN. -/
  anthropicAuthToken : Option String := none
  /-- GitHub logins allowed to trigger any listener. Empty = allow everyone.
      Can be overridden per listener via `authorized_users` in the source config. -/
  authorizedUsers : List String := []
  /-- Per-backend authentication source configurations.
      Allows configuring multiple named authentication sources for each agent backend. -/
  agentAuthConfigs : Array AgentAuthConfig := #[]
  /-- Additional sandbox paths to expose to every agent launched by this instance.
      Merged on top of the agent-backend's built-in paths.
      Useful for granting rw access to directories like `.cache`. -/
  additionalSandboxPaths : SandboxPaths := {}
  /-- Optional banner template appended to PR bodies created via `create_pr`.
      Supports `{backend}` and `{model}` placeholders which are replaced with the
      running agent backend and model respectively.
      Generic "Generated with Claude Code" footers are stripped before this is appended.
      Example: "Generated by [orchestra](https://github.com/chrisflav/orchestra/) using {model}" -/
  prBanner : Option String := none
deriving Repr

instance : FromJson AppConfig where
  fromJson? j := do
    let ghApp ← j.getObjVal? "github_app"
    let appId ← ghApp.getObjValAs? Nat "app_id"
    let privateKeyPath ← ghApp.getObjValAs? String "private_key_path"
    let installationId := ghApp.getObjValAs? Nat "installation_id" |>.toOption
    let pat := (do
      let gh ← j.getObjVal? "github"
      gh.getObjValAs? String "pat"
    ) |>.toOption |>.getD ""
    let pluginDirs := j.getObjValAs? (Array String) "plugin_dirs" |>.toOption |>.getD #[]
    let claudeToken := j.getObjValAs? String "claude_token" |>.toOption
    let anthropicApiKey := j.getObjValAs? String "anthropic_api_key" |>.toOption
    let anthropicBaseUrl := j.getObjValAs? String "anthropic_base_url" |>.toOption
    let anthropicAuthToken := j.getObjValAs? String "anthropic_auth_token" |>.toOption
    let authorizedUsers := j.getObjValAs? (List String) "authorized_users" |>.toOption |>.getD []
    let agentAuthConfigs := j.getObjValAs? (Array AgentAuthConfig) "agents" |>.toOption |>.getD #[]
    let additionalSandboxPaths := j.getObjValAs? SandboxPaths "additional_sandbox_paths" |>.toOption |>.getD {}
    let prBanner := j.getObjValAs? String "pr_banner" |>.toOption
    return { appId, privateKeyPath, installationId, pat, pluginDirs,
             claudeToken, anthropicApiKey, anthropicBaseUrl, anthropicAuthToken, authorizedUsers,
             agentAuthConfigs, additionalSandboxPaths, prBanner }

structure TaskFile where
  tasks : Array Task
deriving Repr

instance : FromJson TaskFile where
  fromJson? j := do
    let tasks ← j.getObjValAs? (Array Task) "tasks"
    return { tasks }

private def expandHome (path : String) : IO System.FilePath := do
  if path.startsWith "~/" then
    match ← IO.getEnv "HOME" with
    | some h => return System.FilePath.mk h / (path.drop 2).toString
    | none => throw (.userError "HOME not set")
  else return .mk path

def loadJsonFile (α : Type) [FromJson α] (path : System.FilePath) : IO α := do
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error e => throw (.userError s!"{path}: JSON parse error: {e}")
  | .ok j =>
    match FromJson.fromJson? j with
    | .error e => throw (.userError s!"{path}: {e}")
    | .ok v => return v

/-- Load `secrets.json` from the config base directory.
    Returns a list of `(key, value)` pairs for use in template substitution.
    If the file does not exist the empty list is returned silently. -/
def loadSecrets : IO (List (String × String)) := do
  let path := (← Dirs.configBase) / "secrets.json"
  if !(← path.pathExists) then return []
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error e => throw (.userError s!"{path}: JSON parse error: {e}")
  | .ok j =>
    match j with
    | .obj kvs =>
      let pairs ← kvs.toList.mapM fun (k, v) => do
        match v with
        | .str s => return (k, s)
        | _ => throw (.userError s!"{path}: secret '{k}' must be a string")
      return pairs
    | _ => throw (.userError s!"{path}: expected a JSON object")

/-- Replace every `{{key}}` occurrence in `text` with the corresponding secret value. -/
def applySecrets (secrets : List (String × String)) (text : String) : String :=
  secrets.foldl (fun acc (k, v) => acc.replace ("{{" ++ k ++ "}}") v) text

/-- Like `loadJsonFile` but substitutes `{{key}}` patterns from `secrets` before parsing. -/
def loadJsonFileWithSecrets (α : Type) [FromJson α] (path : System.FilePath)
    (secrets : List (String × String)) : IO α := do
  let contents := applySecrets secrets (← IO.FS.readFile path)
  match Json.parse contents with
  | .error e => throw (.userError s!"{path}: JSON parse error: {e}")
  | .ok j =>
    match FromJson.fromJson? j with
    | .error e => throw (.userError s!"{path}: {e}")
    | .ok v => return v

def loadAppConfig (path : Option System.FilePath := none) : IO AppConfig := do
  let configPath : System.FilePath ← match path with
    | some p => expandHome p.toString
    | none   => do pure ((← Dirs.configBase) / "config.json")
  let secrets ← loadSecrets
  loadJsonFileWithSecrets AppConfig configPath secrets

def loadTaskFile (path : System.FilePath) : IO TaskFile :=
  loadJsonFile TaskFile path

/--
Load a system prompt from the prompts directory (`<config>/prompts/<name>.md`).
If `name` is `none`, reads `default.md`. Returns `none` if the file does not exist.
-/
def loadSystemPrompt (name : Option String := none) : IO (Option String) := do
  let promptName := name.getD "default"
  let promptPath := (← Dirs.configBase) / "prompts" / s!"{promptName}.md"
  if ← promptPath.pathExists then
    return some (← IO.FS.readFile promptPath)
  else
    return none

/--
Load a prepend prompt from the prompts directory (`<config>/prompts/<name>.md`).
If `name` is `none`, reads `default-prepend.md`. Returns `none` if the file does not exist.
-/
def loadPrependPrompt (name : Option String := none) : IO (Option String) := do
  let promptName := name.getD "default-prepend"
  let promptPath := (← Dirs.configBase) / "prompts" / s!"{promptName}.md"
  if ← promptPath.pathExists then
    return some (← IO.FS.readFile promptPath)
  else
    return none

end Orchestra
