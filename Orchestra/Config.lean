import Lean.Data.Json

open Lean (Json FromJson ToJson)

namespace Orchestra

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
    - `global`  – global memory only (`~/.agent/memory/`)
    - `project` – per-project memory only (`~/.agent/memory/<project>/`)
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
  upstream : String
  fork : String
  /-- Legacy mode field (deprecated). Use `tools` instead.
      If `tools` is absent, this field is used to derive the allowed tools:
      - `fork` → no tools
      - `pr`   → `["create_pr"]` -/
  mode : TaskMode
  prompt : String
  agent : Option String := none
  systemPrompt : Option String := none
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
  /-- Priority of this task. Natural number; higher = more important.
      Defaults to 10 if not set. -/
  priority : Nat := 10
deriving Repr, Inhabited

instance : FromJson Task where
  fromJson? j := do
    let upstream ← j.getObjValAs? String "upstream"
    let fork ← j.getObjValAs? String "fork"
    let mode ← j.getObjValAs? TaskMode "mode"
    let prompt ← j.getObjValAs? String "prompt"
    let agent := j.getObjValAs? String "agent" |>.toOption
    let systemPrompt := j.getObjValAs? String "system_prompt" |>.toOption
    let backend := j.getObjValAs? String "backend" |>.toOption
    let model := j.getObjValAs? String "model" |>.toOption
    let budget := j.getObjValAs? Float "budget" |>.toOption
    let memory := j.getObjValAs? MemoryMode "memory" |>.toOption |>.getD .both
    let authSource := j.getObjValAs? String "auth_source" |>.toOption
    let tools := j.getObjValAs? (List String) "tools" |>.toOption
    let readOnly := j.getObjValAs? Bool "read_only" |>.toOption |>.getD false
    let priority := j.getObjValAs? Nat "priority" |>.toOption |>.getD 10
    return { upstream, fork, mode, prompt, agent, systemPrompt, backend, model, budget, memory,
             authSource, tools, readOnly, priority }

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
    return { appId, privateKeyPath, installationId, pat, pluginDirs,
             claudeToken, anthropicApiKey, anthropicBaseUrl, anthropicAuthToken, authorizedUsers,
             agentAuthConfigs }

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

def loadAppConfig (path : Option System.FilePath := none) : IO AppConfig := do
  let configPath ← expandHome (path.getD "~/.agent/config.json").toString
  loadJsonFile AppConfig configPath

def loadTaskFile (path : System.FilePath) : IO TaskFile :=
  loadJsonFile TaskFile path

/--
Load a system prompt from `~/.agent/prompts/<name>.md`.
If `name` is `none`, reads `default.md`. Returns `none` if the file does not exist.
-/
def loadSystemPrompt (name : Option String := none) : IO (Option String) := do
  let promptName := name.getD "default"
  let promptPath ← expandHome s!"~/.agent/prompts/{promptName}.md"
  if ← promptPath.pathExists then
    return some (← IO.FS.readFile promptPath)
  else
    return none

/--
Load a prepend prompt from `~/.agent/prompts/default-prepend.md`.
If the file exists, its contents will be prepended to every task prompt.
Returns `none` if the file does not exist.
-/
def loadPrependPrompt : IO (Option String) := do
  let promptPath ← expandHome "~/.agent/prompts/default-prepend.md"
  if ← promptPath.pathExists then
    return some (← IO.FS.readFile promptPath)
  else
    return none

end Orchestra
