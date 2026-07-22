import Lean.Data.Json
import Orchestra.Dirs
import Orchestra.Taxis
import Std.Sync

open Lean (Json FromJson ToJson)

namespace Orchestra

private initialize uniqueTokenMutex : Std.BaseMutex ← Std.BaseMutex.new
private initialize uniqueTokenCounter : IO.Ref Nat ← IO.mkRef 0
/-- Set once `uniqueToken` has reported a clock past its 16-digit field; see there. -/
private initialize uniqueTokenOverflowed : IO.Ref Bool ← IO.mkRef false

/-- Lowercase hex, zero-padded to four digits. -/
private def hex4 (n : Nat) : String :=
  let digit := fun (k : Nat) =>
    let v := (n >>> (4 * k)) &&& 15
    if v < 10 then Char.ofNat (v + '0'.toNat) else Char.ofNat (v - 10 + 'a'.toNat)
  String.ofList [digit 3, digit 2, digit 1, digit 0]

/-- A process-wide unique, monotonically increasing identifier.

    `IO.monoNanosNow` on its own is not sufficient once the queue daemon runs tasks on
    several threads: two workers can read the same nanosecond, and the value names task
    records, per-task log files and temp directories, where a collision means two live tasks
    silently overwriting each other's state. Appending a mutex-guarded counter makes distinct
    calls yield distinct results regardless of clock resolution.

    Both components are fixed-width and zero-padded so that lexicographic ordering on the
    result still agrees with chronological ordering — the queue relies on that when sorting
    entries and picking the oldest one at a given priority.

    That ordering guarantee has a ceiling: `IO.monoNanosNow` counts from an unspecified epoch
    which on Linux is boot, and at 10^16 ns — about 116 days of uptime — it outgrows the
    16-digit field and the padding truncates it, wrapping ids back to `0000…`. Widening the
    field is not free, because ids are compared against those already on disk and a wider
    field sorts *below* every existing id, so the discontinuity would be immediate rather
    than once per 116 days. The width therefore stays, and the overflow is reported instead
    of passing silently. -/
def uniqueToken : IO String := do
  let nanos ← IO.monoNanosNow
  uniqueTokenMutex.lock
  let n ← try uniqueTokenCounter.modifyGet (fun n => (n, n + 1))
          finally uniqueTokenMutex.unlock
  let digits := toString nanos
  if digits.length > 16 then
    -- Warn once rather than on every id: past the ceiling this fires for the rest of the
    -- process's life, and the ordering anomaly it describes is a single event, not a stream.
    if ← uniqueTokenOverflowed.modifyGet (fun seen => (!seen, true)) then
      IO.eprintln s!"Warning: the monotonic clock ({digits} ns) has outgrown the 16-digit id \
field. New ids sort before existing ones, so queue entries may be picked out of order until \
this host reboots."
  let padded := ("0000000000000000" ++ digits).takeEnd 16
  return padded.toString ++ hex4 (n % 65536)

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
  projectId : Option Taxis.IssueId := none
  /-- Orchestra issue this task is working on (optional).
      Set by `claim_issue`; release on terminal status flips it back to `.open`. -/
  issueId : Option Taxis.IssueId := none
  /-- Optional role name this task was spawned for (e.g. "implementor"). Used
      by the project-dispatcher to count active per-role tasks unambiguously,
      avoiding fragile `tools` list comparisons. -/
  role : Option String := none
  /-- Labels to automatically apply to every pull request created by `create_pr` during this task.
      Labels that do not exist on the target repository are created automatically. -/
  prLabels : List String := []
  /-- Labels to add to the issue or PR when using the `triage` backend. -/
  triageAddLabels : List String := []
  /-- Labels to remove from the issue or PR when using the `triage` backend. -/
  triageRemoveLabels : List String := []
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
    let projectId   := j.getObjValAs? Taxis.IssueId "project_id" |>.toOption
    let issueId     := j.getObjValAs? Taxis.IssueId   "issue_id"   |>.toOption
    let role        := j.getObjValAs? String    "role"       |>.toOption
    let prLabels          := j.getObjValAs? (List String) "pr_labels"           |>.toOption |>.getD []
    let triageAddLabels    := j.getObjValAs? (List String) "triage_add_labels"    |>.toOption |>.getD []
    let triageRemoveLabels := j.getObjValAs? (List String) "triage_remove_labels" |>.toOption |>.getD []
    return { i, o, ioTask := { upstream, fork, mode, prompt, agent, systemPrompt, prependPrompt, backend, model,
                                budget, memory, authSource, tools, readOnly, series, priority,
                                issueNumber, projectId, issueId, role, prLabels,
                                triageAddLabels, triageRemoveLabels } }

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
  /-- Paths relative to $HOME needing read+write+execute. Needed by toolchain managers, which
      both install binaries and run them: `~/.elan` has to be writable (elan records settings and
      unpacks toolchains into it) *and* executable (the `lean`/`lake` it unpacks live under it).
      Read-only would break installs; write-without-execute would break running what was
      installed. -/
  homeRwx : List String := []
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
    let homeRwx := j.getObjValAs? (List String) "home_rwx" |>.toOption |>.getD []
    return { rox, ro, rw, homeRox, homeRw, homeRwx }

/-- Queue daemon concurrency, from the `queue` object in `config.json`.

    Both default to 1, which is the serial behaviour the daemon had before parallel mode
    existed. `orchestra queue start`'s `--parallel` / `--parallel-per-repo` flags override
    these for a single run. -/
structure QueueConfig where
  /-- Maximum tasks running at once across all repositories. -/
  parallel : Nat := 1
  /-- Maximum tasks running at once on any one repository. Each gets its own clone slot, so
      raising this costs a working tree per slot — run `orchestra prepare --slots N` to match,
      or the first task to reach each new slot pays its repository's init hook. -/
  parallelPerRepo : Nat := 1
deriving Repr, Inhabited

instance : FromJson QueueConfig where
  fromJson? j := do
    -- Both optional: a `queue` block that sets only one of them keeps the default for the
    -- other, and `max 1` because zero workers would be a daemon that silently does nothing.
    let parallel := j.getObjValAs? Nat "parallel" |>.toOption |>.getD 1
    let parallelPerRepo := j.getObjValAs? Nat "parallel_per_repo" |>.toOption |>.getD 1
    return { parallel := max 1 parallel, parallelPerRepo := max 1 parallelPerRepo }

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
  /-- Organisation under which orchestra may create repositories, i.e. where it forks a
      target repository the GitHub App cannot push to. Used by every project/role-based task:
      when the App has no write access to a task's target repo, the target is forked into this
      org and the fork is what the agent pushes to. `none` disables forking — a task whose
      target is not writable is then skipped rather than dispatched at a repo it cannot push to. -/
  defaultOrganization : Option String := none
  /-- taxis instance backing the project/issue/claim subsystem (`Orchestra.Project`). `none`
      disables it — any project/issue/claim operation then fails with a clear "not configured"
      error rather than falling back to the old file-based storage. -/
  taxis : Option Taxis.Config := none
  /-- Queue daemon concurrency. Read only by `orchestra queue start`. -/
  queue : QueueConfig := {}
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
    let taxis := j.getObjValAs? Taxis.Config "taxis" |>.toOption
    let queue := j.getObjValAs? QueueConfig "queue" |>.toOption |>.getD {}
    let defaultOrganization := j.getObjValAs? String "default_organization" |>.toOption
    return { appId, privateKeyPath, installationId, pat, pluginDirs,
             claudeToken, anthropicApiKey, anthropicBaseUrl, anthropicAuthToken, authorizedUsers,
             agentAuthConfigs, additionalSandboxPaths, taxis, queue, defaultOrganization }

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
