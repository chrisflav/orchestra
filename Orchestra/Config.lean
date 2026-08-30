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

/-- A process-wide unique identifier, increasing for as long as the host stays up.

    `IO.monoNanosNow` on its own is not sufficient once the queue daemon runs tasks on
    several threads: two workers can read the same nanosecond, and the value names task
    records, per-task log files and temp directories, where a collision means two live tasks
    silently overwriting each other's state. Appending a mutex-guarded counter makes distinct
    calls yield distinct results regardless of clock resolution.

    Both components are fixed-width and zero-padded, so lexicographic ordering on the result
    agrees with chronological ordering — but only within a single boot. `IO.monoNanosNow`
    counts from an unspecified epoch which on Linux is boot, so a reboot restarts it at zero
    and every id minted afterwards sorts below every id minted before. **Nothing may order
    records by id.** Every store records a wall-clock timestamp beside the id and sorts on that
    (`Orchestra.Time.sortNewestFirst`); an id is a name, and inside one boot a tiebreaker for
    timestamps that only resolve to the second.

    Within a boot the field has a ceiling too: at 10^16 ns — about 116 days of uptime — the
    clock outgrows the 16 digits and the padding truncates it, wrapping ids back to `0000…`.
    Widening the field is not free, because ids are compared against those already on disk and
    a wider field sorts *below* every existing id, so the discontinuity would be immediate
    rather than once per 116 days. The width therefore stays, and the overflow is reported
    instead of passing silently. -/
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
field, so new ids sort before existing ones. Records are ordered by their recorded timestamp \
rather than by id, so listings and the queue keep their order; only the tiebreak between two \
records stamped in the same second inverts. Ids stop reading as ascending until this host \
reboots."
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

/-- The repositories a task works on: the upstream it targets, and the fork it pushes to.

    The two halves are never separable. An agent holding a fork it can push to but no upstream to
    open the pull request against — or the reverse — is not a state anything downstream knows what
    to do with, so they travel as one value and a task either has the pair or has no repository at
    all. `Option RepoPair` is the only spelling of "no repository"; `parseRepoPair?` is how that is
    read out of a config. -/
structure RepoPair where
  upstream : Repository
  fork     : Repository
deriving BEq, Repr, Inhabited

/-- Read the `upstream`/`fork` pair out of the object `j`.

    Both keys or neither. Absent means the task is repository-independent: it runs in a scratch
    workspace with nothing checked out. That is worth saying explicitly rather than inferring from
    half a pair — reading a lone `upstream` as "no repository" would send a task that was written
    to open a pull request into an empty directory, and the failure would surface far from the key
    that caused it. -/
def parseRepoPair? (j : Json) : Except String (Option RepoPair) :=
  match j.getObjVal? "upstream", j.getObjVal? "fork" with
  | .error _, .error _ => .ok none
  | .ok u,    .ok f    => do
    let upstream ← FromJson.fromJson? (α := Repository) u
    let fork     ← FromJson.fromJson? (α := Repository) f
    return some { upstream, fork }
  | .ok _,    .error _ =>
    .error "'upstream' is set but 'fork' is not; a task names both repositories or neither"
  | .error _, .ok _    =>
    .error "'fork' is set but 'upstream' is not; a task names both repositories or neither"

/-- The `upstream`/`fork` JSON fields of a repository pair, and nothing at all when there is none
    — which is what `parseRepoPair?` reads back as a repository-independent task. -/
def repoPairFields (repo : Option RepoPair) : List (String × Json) :=
  match repo with
  | some p => [("upstream", ToJson.toJson p.upstream), ("fork", ToJson.toJson p.fork)]
  | none   => []

/-- How a task's repository is named in listings and logs. -/
def repoLabel (repo : Option RepoPair) : String :=
  match repo with
  | some p => p.fork.toString
  | none   => "(no repository)"

/-- Where a task's per-run logs live, relative to `<data>/logs`.

    A repository's own `owner/repo` — two path components — or one fixed name for every
    repository-independent task. That name carries no `/`, so it sits at a level no repository's
    log directory can reach and the two cannot collide. -/
def repoLogDir (repo : Option RepoPair) : String :=
  match repo with
  | some p => p.fork.toString
  | none   => "no-repository"

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

/-- Read the deprecated `mode` field out of the object `j`.

    Absent reads as `fork` — grant nothing — which is what a task that writes neither `mode` nor
    `tools` has always got, and the only answer a repository-independent task could have. Present
    but unreadable is still an error: `"mode": "PR"` swallowed as `fork` would leave a task that
    was written to open a pull request holding no tools at all, and, because the deprecation
    warning only fires when the fallback grants something, nothing on the way would say so. -/
def parseTaskMode? (j : Json) : Except String TaskMode :=
  match j.getObjVal? "mode" with
  | .error _ => .ok .fork
  | .ok v    => FromJson.fromJson? v

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

/-- How a task picks among several configured authentication sources.

    Lives here rather than beside the usage monitor because it is a *config* value — tasks,
    queue entries and listener actions all carry one — and `Orchestra.Usage`, which consumes it,
    imports this module. -/
inductive AuthMode where
  /-- Use the sources in the order listed, falling through to the next when one is limited.
      The default: with a single source it is indistinguishable from having no modes at all. -/
  | ordered
  /-- Spread work across every source that is not limited, preferring the least-consumed. -/
  | distribute
deriving Repr, BEq, Inhabited

def AuthMode.toString : AuthMode → String
  | .ordered    => "ordered"
  | .distribute => "distribute"

def AuthMode.ofString? : String → Option AuthMode
  | "ordered"    => some .ordered
  | "distribute" => some .distribute
  | _            => none

instance : ToJson AuthMode where toJson m := Json.str m.toString
instance : FromJson AuthMode where
  fromJson?
    | .str s => match AuthMode.ofString? s with
      | some m => .ok m
      | none   => .error s!"expected \"ordered\" or \"distribute\", got \"{s}\""
    | j => .error s!"expected auth mode string, got {j}"

/-- What a task may queue, and how much of it, through the `queue_task` MCP tool. Absent from a
    task's config means the task cannot queue anything: `queue_task` is not offered at all,
    rather than offered and always refused.

    Lives here rather than beside the tool for the same reason `AuthMode` does — tasks, queue
    entries, listener actions and roles all carry one. `Orchestra.Spawn` holds the rules it
    states and the module doc that explains them.

    Every list is a *widening*: it says what the agent may name instead of inheriting the
    spawning task's own value. Empty — the default on every field — means it may name nothing,
    which still leaves it able to queue a copy of itself. -/
structure SpawnPolicy where
  /-- Backends a queued task may be put on. Empty: only the spawning task's own. -/
  backends      : List String := []
  /-- Models a queued task may be put on. Empty: only the spawning task's own.

      Not checked against the backend — orchestra does not hold a model list per backend, and a
      policy that named one would go stale on the vendor's schedule rather than the operator's.
      A model the backend does not know fails when the task runs, the same way it does when a
      role names one. -/
  models        : List String := []
  /-- Tools a queued task may be granted, by the same names a role's `permissions` uses. Empty:
      only the tools the spawning task itself holds.

      May exceed what the spawning task holds — see the module docs; that is the operator's call
      to make, and the reason it is written in a config file rather than derived. -/
  tools         : List String := []
  /-- Repositories a queued task may run against, as `owner/name` upstreams. Empty: only the
      spawning task's own repository (and nothing at all, for a repository-independent one).

      The fork is never named here. It is resolved when the task is queued, by the same rule
      every other dispatch path uses (`GitHub.resolveFork`): the target itself when the App can
      push to it, a fork in the default organisation otherwise. -/
  repos         : List Repository := []
  /-- How many tasks one task may queue over its whole run. Counted over the queue rather than
      in memory, so a daemon restart mid-run cannot reset it.

      Defaults to 1. A queued task costs an agent run's worth of somebody's money, and the
      failure of a too-generous default is a queue full of work nobody asked for; the failure of
      a too-strict one is a refusal with the number in it, which the operator can raise. -/
  maxTasks      : Nat := 1
  /-- Whether the tool may claim the issue it binds, for the task it queues.

      Off by default. A claim takes an issue out of every dispatcher's candidate set until the
      task that holds it ends, which is exactly what you want when the queued task is the one
      that should do the work — and exactly what you do not want by accident. -/
  allowPreClaim : Bool := false
  /-- Ceiling on a queued task's budget in USD, and the budget it gets when the agent names none.

      Both halves matter: without the default, a policy capping the budget at 2.0 would still
      hand an unbudgeted task the daemon's own default of 4.0. -/
  maxBudget     : Option Float := none
  /-- Priority of the queued task. Operator-set: the agent has no say, since a priority is only
      meaningful relative to every other entry in the queue. -/
  priority      : Nat := 10
  /-- Whether the queued task's workspace is mounted read-only. Absent inherits the queueing
      task's own answer, which is what stops an empty policy from handing a read-only reviewer a
      read-write child — the one way "queue a copy of itself" could have granted more than the
      task had. Set it explicitly for the case the tool exists for: a read-only planner queueing
      an implementor that has to write. -/
  readOnly      : Option Bool := none
deriving Repr, Inhabited

instance : ToJson SpawnPolicy where
  toJson p :=
    let f : List (String × Json) := []
    let f := if p.backends.isEmpty then f else f ++ [("backends", ToJson.toJson p.backends)]
    let f := if p.models.isEmpty   then f else f ++ [("models",   ToJson.toJson p.models)]
    let f := if p.tools.isEmpty    then f else f ++ [("tools",    ToJson.toJson p.tools)]
    let f := if p.repos.isEmpty    then f else f ++ [("repos",    ToJson.toJson p.repos)]
    let f := f ++ [("max_tasks", Json.num p.maxTasks)]
    let f := if p.allowPreClaim then f ++ [("allow_pre_claim", Json.bool true)] else f
    let f := match p.maxBudget with
      | some b => f ++ [("max_budget", ToJson.toJson b)]
      | none   => f
    let f := if p.priority != 10 then f ++ [("priority", Json.num p.priority)] else f
    let f := match p.readOnly with
      | some b => f ++ [("read_only", Json.bool b)]
      | none   => f
    Json.mkObj f

instance : FromJson SpawnPolicy where
  fromJson? j := do
    -- Every list is read strictly. A `repos` entry misspelled as `"my-account orchestra"`, or a
    -- `backends` written as a bare string, would otherwise be swallowed as the empty list — and
    -- the empty list is not "no opinion" here, it is "the agent may name nothing", so the policy
    -- would quietly do the opposite of what it says while still parsing.
    let listOr (key : String) : Except String (List String) :=
      match j.getObjVal? key with
      | .error _ => .ok []
      | .ok v    => FromJson.fromJson? v
    let backends ← listOr "backends"
    let models   ← listOr "models"
    let tools    ← listOr "tools"
    let repos ← match j.getObjVal? "repos" with
      | .error _ => pure []
      | .ok v    => (FromJson.fromJson? v : Except String (List Repository))
    let maxTasks      := j.getObjValAs? Nat   "max_tasks"       |>.toOption |>.getD 1
    let allowPreClaim := j.getObjValAs? Bool  "allow_pre_claim" |>.toOption |>.getD false
    let maxBudget     := j.getObjValAs? Float "max_budget"      |>.toOption
    let priority      := j.getObjValAs? Nat   "priority"        |>.toOption |>.getD 10
    let readOnly      := j.getObjValAs? Bool  "read_only"       |>.toOption
    return { backends, models, tools, repos, maxTasks, allowPreClaim, maxBudget, priority,
             readOnly }

/-- Read a `spawn_policy` out of the object `j`.

    Absent is fine — most tasks queue nothing. Present and unreadable is not: a policy that
    failed to parse would leave the tool switched off entirely, and "the agent never called it"
    looks exactly like "the agent could not see it". -/
def parseSpawnPolicy? (j : Json) : Except String (Option SpawnPolicy) :=
  match j.getObjVal? "spawn_policy" with
  | .error _  => .ok none
  -- An explicit `null` is how "not set" is serialised — by `Option`'s own `ToJson`, among
  -- others — so a document this module wrote has to read back as the policy-less task it was.
  | .ok .null => .ok none
  | .ok v     => (FromJson.fromJson? v : Except String SpawnPolicy).map some

/-- Whether `p` names only tools that exist, given the vocabulary the caller validates against
    (`Project.Role.knownPermissions`, which is also what a role's `permissions` are checked
    against — the tool must not be a way to hand a task something a role could not be given).

    Taken as a parameter because the list lives with the roles, which are defined above this
    module rather than below it — and because keeping one list is what stops the two checks from
    disagreeing about what a task may be granted. -/
def SpawnPolicy.validate (p : SpawnPolicy) (knownTools : List String) : Except String Unit := do
  let unknown := p.tools.filter (fun t => !knownTools.contains t)
  unless unknown.isEmpty do
    .error s!"spawn_policy names unknown tool(s) {String.intercalate ", " unknown}; \
the tools a queued task may be granted are {String.intercalate ", " knownTools}"
  if p.maxTasks == 0 then
    .error "spawn_policy 'max_tasks' is 0, which refuses every call; drop the whole \
'spawn_policy' block to take the tool away instead"
  if let some b := p.maxBudget then
    if b ≤ 0.0 then
      .error "spawn_policy 'max_budget' must be greater than 0; a task budgeted at nothing \
cannot run"

/-- A typed task with phantom input type `i` and output type `o`. -/
structure IOTask (i o : ResultType) where
  /-- The repositories this task works on, or `none` for a **repository-independent** task.

      Such a task runs in the sandbox like any other, but with nothing checked out: instead of a
      clone slot it gets an empty scratch workspace, and the tools that name a repository —
      `create_pr`, `merge_pr`, `label_issue`, `comment`, `get_pr_comments` — are withheld. It is
      the shape meta-work takes: coordinating issues across projects on the taxis tracker,
      maintenance that belongs to no single repository, where cloning one repository would mean
      picking an arbitrary one of the several the task is about.

      Withholding those tools is a guardrail, not an isolation boundary. Where the config names an
      installation the task still holds a GitHub App token in its sandbox, and a task granted
      `review_issues` can still approve an issue, which queues a merger against whatever pull
      request that issue carries. What it cannot do is act on a repository *this task* named,
      because it named none. -/
  repo : Option RepoPair
  /-- Legacy mode field (deprecated). Use `tools` instead.
      If `tools` is absent, this field is used to derive the allowed tools:
      - `fork` → no tools
      - `pr`   → `["create_pr"]` -/
  mode : TaskMode := .fork
  prompt : String
  /-- Condition the run is held to: the agent must not stop before it holds, and a second model
      call — not the agent itself — decides whether it does.

      Deliberately not derived from `prompt`. A task launched for an orchestra issue gets the
      issue's own `goal` field and nothing else; the prompt around it is the role template with
      the issue body and its comment thread rendered in, which is the wrong thing entirely to
      judge "is this done?" against. Backends with no goal mechanism ignore it (see
      `AgentDef.goalArgs`). -/
  goal : Option String := none
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
  /-- Candidate authentication sources, tried according to `authMode`.

      Takes precedence over `authSource`. Resolution to one label is deferred until the task is
      about to launch, not fixed when it is queued: an entry can wait hours for a slot, and the
      source that was free when it was created may be exhausted by the time it runs. -/
  authSources : List String := []
  /-- How to choose among the candidates. Ignored when fewer than two are in play.

      `none` means the task did not say, and the backend's `default_auth_mode` decides — which is
      the only way a pooled default gets walked in the mode it was configured with. Absent is
      therefore not the same as `ordered`, and the two must stay distinguishable. -/
  authMode : Option AuthMode := none
  /-- Optional tools to enable beyond the always-available ones (health, refresh_token,
      get_pr_comments): `"create_pr"`, `"merge_pr"`, `"label_issue"`, `"comment"`,
      `"create_repository"`, and the project/issue permission groups.
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
  /-- What this task may put on the queue itself (`Orchestra.Spawn`). `none` — the default —
      means it may queue nothing, and the `queue_task` tool is not offered to it.

      Never set on a task that was itself queued by that tool: see `Orchestra.Spawn` for why the
      fan-out is bounded by a rule rather than by a depth counter. -/
  spawnPolicy : Option SpawnPolicy := none
  /-- The issue this task may write at or below, when something other than the task's own issue
      and project decides it. Set only on a task queued by `queue_task`, where it carries the
      *queueing* task's scope: without it the child re-derives its own from the issue it was
      bound to (`Project.projectRootOf`), which walks up to the nearest project anchor and can
      land above the scope the task that queued it was held to — see `writeScopeRoot`. -/
  scopeRoot : Option Taxis.IssueId := none
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
  /-- The sources a task that names none of its own runs on.

      Written as a single label to pin one account, or as a list to pool several. A pool is what
      lets accounts be kept level without every listener, role and workflow step naming them
      itself — which matters because several dispatch paths have no field to name them in:
      `Listener.buildRoleEntry` sets no candidates on a dispatched role, and a concert step's
      YAML has nowhere to write them. Both arrive here with nothing named and take the pool.

      Empty means "not configured": with exactly one source configured that source is used
      anyway, and with several the task is asked to name one. -/
  defaultAuthSources : List String := []
  /-- How to choose among `defaultAuthSources` when it holds more than one.
      Ignored when it holds zero or one, which is every config that predates the pool. -/
  defaultAuthMode : AuthMode := .ordered
  /-- Additional TCP ports the agent is allowed to connect to inside the sandbox.
      Appended to the ports the agent backend already opens (MCP server port + 443). -/
  extraPorts : Array Nat := #[]
  /-- Whether to proactively poll subscription usage for this backend's OAuth sources.

      Polling is no longer free: the numbers ride on the rate-limit headers of an inference call,
      so each poll spends a real `max_tokens: 1` request (a `setup-token` cannot reach the metadata
      endpoint). Set `false` to stop the automatic polls — the daemon and claim-time selection —
      and rely on observed hits (`markLimited`) instead. `orchestra usage --refresh` still forces a
      one-off poll. Defaults to `true`, preserving prior behaviour. -/
  pollUsage : Bool := true
deriving Repr, Inhabited

instance : FromJson AgentAuthConfig where
  fromJson? j := do
    let name             ← j.getObjValAs? String "name"
    let authSources       := j.getObjValAs? (Array AuthSource) "auth_sources" |>.toOption |>.getD #[]
    -- One key, two spellings: a bare string is the pre-pool syntax and still pins, a list pools.
    -- Kept as one key so there is no second field to disagree with the first about what the
    -- default is.
    --
    -- Absent is fine; present-and-unreadable is not. Both of these decide which account real
    -- work runs on, and the failure is silent in the direction that hurts: a swallowed
    -- `"distributed"` leaves the pool walking in `ordered`, i.e. pinned to its first member —
    -- exactly what the pool was configured to stop — with nothing in the logs to say so.
    let defaultAuthSources ← match j.getObjVal? "default_auth_source" with
      | .error _ => pure []
      | .ok v    => match (FromJson.fromJson? v : Except String String) with
        | .ok s    => pure [s]
        | .error _ => match (FromJson.fromJson? v : Except String (List String)) with
          | .ok ls   => pure ls
          | .error _ => throw s!"default_auth_source must be a label or a list of labels, got {v}"
    let defaultAuthMode ← match j.getObjVal? "default_auth_mode" with
      | .error _ => pure AuthMode.ordered
      | .ok v    => (FromJson.fromJson? v : Except String AuthMode)
    let extraPorts        := j.getObjValAs? (Array Nat) "extra_ports" |>.toOption |>.getD #[]
    let pollUsage         := j.getObjValAs? Bool "poll_usage" |>.toOption |>.getD true
    return { name, authSources, defaultAuthSources, defaultAuthMode, extraPorts, pollUsage }

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
    let repo       ← parseRepoPair? j
    let mode       ← parseTaskMode? j
    let prompt     ← j.getObjValAs? String "prompt"
    let goal       := j.getObjValAs? String "goal"           |>.toOption
    let agent      := j.getObjValAs? String "agent"          |>.toOption
    let systemPrompt := j.getObjValAs? String "system_prompt" |>.toOption
    let prependPrompt := j.getObjValAs? String "prepend_prompt" |>.toOption
    let backend    := j.getObjValAs? String "backend"        |>.toOption
    let model      := j.getObjValAs? String "model"          |>.toOption
    let budget     := j.getObjValAs? Float "budget"          |>.toOption
    let memory     := j.getObjValAs? MemoryMode "memory"     |>.toOption |>.getD .both
    let authSource := j.getObjValAs? String "auth_source"    |>.toOption
    let authSources := j.getObjValAs? (List String) "auth_sources" |>.toOption |>.getD []
    let authMode   := j.getObjValAs? AuthMode "auth_mode"    |>.toOption
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
    -- Strict, unlike the fields above: a policy that fails to parse leaves the task unable to
    -- queue anything, and nothing on the way says the field was why.
    let spawnPolicy ← parseSpawnPolicy? j
    let scopeRoot := j.getObjValAs? Taxis.IssueId "scope_root" |>.toOption
    return { i, o, ioTask := { repo, mode, prompt, goal, agent, systemPrompt, prependPrompt, backend, model,
                                budget, memory, authSource, authSources, authMode, tools, readOnly,
                                series, priority,
                                issueNumber, projectId, issueId, role, prLabels,
                                triageAddLabels, triageRemoveLabels, spawnPolicy, scopeRoot } }

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

/-- What bounds interactive sessions: the `interactive` block in `config.json`.

    Both are capacity, not access. A session pins a clone slot and an agent process for as long
    as it lives, and an abandoned browser tab should not hold either forever. Spelled like the
    `queue` block above, which bounds the same resources for tasks.

    Zero is allowed here, unlike the queue's limits, and means what it says: a daemon that will
    not hold interactive sessions at all. -/
structure InteractiveConfig where
  /-- Sessions that may be up at once, across all repositories. -/
  maxSessions : Nat := 2
  /-- How long a session may sit without a turn before the daemon closes it. -/
  idleTimeoutSeconds : Nat := 1800
deriving Repr, Inhabited

instance : FromJson InteractiveConfig where
  fromJson? j := do
    let maxSessions := j.getObjValAs? Nat "max_sessions" |>.toOption |>.getD 2
    let idleTimeoutSeconds :=
      j.getObjValAs? Nat "idle_timeout_seconds" |>.toOption |>.getD 1800
    return { maxSessions, idleTimeoutSeconds }

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
  /-- What bounds interactive sessions. -/
  interactive : InteractiveConfig := {}
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
    -- Strict when present. Swallowing the error here drops the whole `agents` block, and the
    -- backends then fall through to the legacy flat token fields — which on a config that
    -- defines `agents` means no credentials at all, reported as "no auth sources configured"
    -- with no mention of the key that failed to parse. Absent stays absent: that is the legacy
    -- flat-token install, which is a supported config and not a mistake.
    let agentAuthConfigs ← match j.getObjVal? "agents" with
      | .error _ => pure #[]
      | .ok v    => (FromJson.fromJson? v : Except String (Array AgentAuthConfig))
    let additionalSandboxPaths := j.getObjValAs? SandboxPaths "additional_sandbox_paths" |>.toOption |>.getD {}
    let taxis := j.getObjValAs? Taxis.Config "taxis" |>.toOption
    let queue := j.getObjValAs? QueueConfig "queue" |>.toOption |>.getD {}
    let interactive := j.getObjValAs? InteractiveConfig "interactive" |>.toOption |>.getD {}
    let defaultOrganization := j.getObjValAs? String "default_organization" |>.toOption
    return { appId, privateKeyPath, installationId, pat, pluginDirs,
             claudeToken, anthropicApiKey, anthropicBaseUrl, anthropicAuthToken, authorizedUsers,
             agentAuthConfigs, additionalSandboxPaths, taxis, queue, interactive,
             defaultOrganization }

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
