import Lean.Data.Json
import Orchestra.Config
import Orchestra.Dirs
import Orchestra.Queue
import Orchestra.Secret
import Orchestra.Spawn
import Orchestra.Project.Role
import Orchestra.Utils.Files

open Lean (Json FromJson ToJson)

namespace Orchestra.ApiClient

/-! # API clients: queueing work over HTTP

The dashboard's password is the operator's: anything holding it can read every run, rewrite every
listener and start an interactive session with any repository. That is the right credential for a
person at the dashboard and the wrong one for another *system* that only needs to put work on the
queue — a review platform asking for a review of one pull request, say. Handing such a system the
password would hand it the operator's power over everything else as well.

An **API client** is the credential for that system instead. It is a file,
`<config>/clients/<name>.json`, holding a bearer token and the bounds on what the client may
queue:

```json
{
  "token": "{{review_platform_token}}",
  "backend": "claude",
  "model": "sonnet",
  "read_only": true,
  "kleis_spec": "reviewer",
  "queue_policy": { "repos": ["acme/widgets"], "max_budget": 3.0, "priority": 20 },
  "max_active": 20
}
```

## The policy is `queue_task`'s

What a client may queue is decided by exactly the rules an agent's `queue_task` is held to
(`Orchestra.Spawn`): the client's own fields — `backend`, `model`, `tools`, `read_only`,
`kleis_spec`, `kleis_narrow`, `execution_image` — play the part of the spawning task, so they are
what a request inherits when it names nothing, and `queue_policy` is a `spawn_policy` saying what
it may name instead. One set of rules rather than two, so the two ways of putting work on the
queue from outside a person's shell cannot drift apart.

Two fields of a spawn policy mean something different for a client, and are handled here:

  * `max_tasks` counts what one *task* queues over its run. A client has no run; it is refused in
    a client's policy, and the client's ceiling is `max_active` — how many of its entries may be
    pending or running at once.
  * `allow_pre_claim` binds a taxis issue, which a client cannot ask for in this version: the
    request has no `issue_id`. It is refused rather than accepted and ignored.

## The image its tasks run in

A client's work is usually of one kind — reviews driving a review platform's CLI, say — and the
tools that work needs belong in an image of the operator's choosing rather than in orchestra's
default one. `execution_image` in the client file is that image: every task the client queues
runs in it, unless the request names another from `queue_policy.images`. As for any task, it beats
the repository's own image and the operator's per-repository pin (`docs/kubernetes.md`), because
it says what kind of work this is, which does not change with the repository.

## What a client never chooses

Its kleis source. The client's `kleis_spec` is what every task it queues derives its token from,
as a spawning task's source is inherited by what it queues; a request may add narrowings
(`kleis_narrow`) and never name another source. A client file without one leaves its tasks to
`kleis.default_spec`, like any task with no source of its own — set one.

Nor does a request choose the fork: a read-only task works in the upstream itself, as the
auto-reviewer does, and a writable one gets its fork resolved by the same rule every other
dispatch path uses (`GitHub.resolveFork`).

## The token

`token` must be a `{{secret}}` reference into `secrets.json`, never the token itself, for the
reason a task's `kleis_token` must: a client file is configuration, and configuration is read
and copied by more people than a secret should be. -/

/-- One API client, as read from its file with its token resolved. -/
structure ApiClient where
  /-- The file's name, which is what the client's entries are stamped with (`api_client`). -/
  name      : String
  /-- The bearer token, resolved from `secrets.json`. Never serialised or printed. -/
  token     : String
  /-- What a task the client queues gets for every field the request leaves out. -/
  defaults  : SpawnContext
  /-- What a request may name instead of inheriting `defaults`. -/
  policy    : SpawnPolicy
  /-- How many of the client's entries may be pending or running at once. -/
  maxActive : Nat := 10

/-- The keys a client file may carry. Anything else is a typo, and a typo in a file that bounds
    what another system may spend is refused rather than read as absent. -/
private def knownKeys : List String :=
  ["description", "token", "backend", "model", "tools", "read_only", "kleis_spec", "kleis_narrow",
   "queue_policy", "max_active", "execution_image"]

private def unknownKeys (j : Json) (known : List String) : List String :=
  match j.getObj? with
  | .ok kvs => kvs.toList.map (·.1) |>.filter (!known.contains ·)
  | .error _ => []

/-- An optional string field, strictly: present and not a string is an error. -/
private def optString (j : Json) (key what : String) : Except String (Option String) :=
  match j.getObjVal? key with
  | .error _  => .ok none
  | .ok .null => .ok none
  | .ok (.str s) => .ok (some s)
  | .ok _ => .error s!"{what}: '{key}' must be a string"

/-- Read client `name` out of `j`, resolving its token from `secrets`. -/
def parse (name : String) (j : Json) (secrets : List (String × String)) :
    Except String ApiClient := do
  let what := s!"client '{name}'"
  if j.getObj?.toOption.isNone then .error s!"{what}: the file must hold a JSON object"
  let unknown := unknownKeys j knownKeys
  unless unknown.isEmpty do
    .error s!"{what}: unknown field(s) {String.intercalate ", " unknown}; a client file may \
carry {String.intercalate ", " knownKeys}"
  let some ref ← optString j "token" what
    | .error s!"{what} has no 'token'; a client is its token"
  let some key := secretRefKey? ref
    | .error (s!"{what}: 'token' must be a reference to a secret in secrets.json, such as " ++
        "\"{{my_client_token}}\", never the token itself")
  let some token := secrets.lookup key
    | .error s!"{what}: 'token' names the secret '{key}', which secrets.json does not define"
  -- A token short enough to guess is a password nobody chose. Sixteen bytes of entropy is what
  -- `Secret.randomHex 16` mints, which is the least this should be generated with.
  if token.length < 32 then
    .error s!"{what}: the secret '{key}' is shorter than 32 characters; generate one with \
`openssl rand -hex 32`"
  let backend ← optString j "backend" what
  let model   ← optString j "model" what
  let tools ← match j.getObjVal? "tools" with
    | .error _ | .ok .null => pure []
    | .ok v => match (FromJson.fromJson? v : Except String (List String)) with
      | .ok ts => pure ts
      | .error _ => .error s!"{what}: 'tools' must be a list of strings"
  let unknownTools := tools.filter (!Project.Role.knownPermissions.contains ·)
  unless unknownTools.isEmpty do
    .error s!"{what} names unknown tool(s) {String.intercalate ", " unknownTools}; the tools a \
queued task may be granted are {String.intercalate ", " Project.Role.knownPermissions}"
  let readOnly ← match j.getObjVal? "read_only" with
    | .error _ | .ok .null => pure false
    | .ok (.bool b) => pure b
    | .ok _ => .error s!"{what}: 'read_only' must be true or false"
  let kleisSpec ← parseKleisSpec? j s!"{what}: kleis_spec"
  let kleisNarrow ← parseKleisNarrow? j s!"{what}: kleis_narrow"
  let executionImage ← parseExecutionImage? j s!"{what}: execution_image"
  let policyJson ← match j.getObjVal? "queue_policy" with
    | .error _ | .ok .null => pure (Json.mkObj [])
    | .ok v => pure v
  if (policyJson.getObjVal? "max_tasks").toOption.isSome then
    .error s!"{what}: 'queue_policy' sets 'max_tasks', which counts what one task queues over its \
run; a client has no run, and its ceiling is 'max_active' beside the policy"
  let policy ← (FromJson.fromJson? policyJson : Except String SpawnPolicy).mapError
    (s!"{what}: queue_policy: " ++ ·)
  if policy.allowPreClaim then
    .error s!"{what}: 'queue_policy' sets 'allow_pre_claim', but a client cannot bind a taxis \
issue to what it queues"
  -- `validate` refuses `max_tasks = 0`; a client policy never carries one, and the default it
  -- parses to is 1, so this checks the tools and the budget and nothing it does not mean.
  policy.validate Project.Role.knownPermissions |>.mapError (s!"{what}: " ++ ·)
  let maxActive ← match j.getObjVal? "max_active" with
    | .error _ | .ok .null => pure 10
    | .ok v => match v.getNat? with
      | .ok 0 => .error s!"{what}: 'max_active' is 0, which refuses every request; delete the \
client to take its access away instead"
      | .ok n => pure n
      | .error _ => .error s!"{what}: 'max_active' must be a positive whole number"
  return { name, token, policy, maxActive
         , defaults := { backend, model, tools, readOnly, kleisSpec, kleisNarrow, executionImage } }

/-! ## Filesystem layout -/

/-- Optional override for the clients directory (tests redirect this, as they do for roles and
    identities). -/
initialize clientsDirOverride : IO.Ref (Option System.FilePath) ← IO.mkRef none

def setClientsDirOverride (p : Option System.FilePath) : IO Unit :=
  clientsDirOverride.set p

/-- Where the clients live: `<config>/clients`. -/
def clientsDir : IO System.FilePath := do
  match ← clientsDirOverride.get with
  | some p => return p
  | none   => return (← Dirs.configBase) / "clients"

/-- Read client `name`. `none` when there is no such file; an error when there is one and it
    cannot be read. -/
def loadClient (name : String) : IO (Option ApiClient) := do
  Utils.ensureConfigName "client" name
  let path := (← clientsDir) / s!"{name}.json"
  if !(← path.pathExists) then return none
  match Json.parse (← IO.FS.readFile path) with
  | .error e => throw (.userError s!"{path}: JSON parse error: {e}")
  | .ok j =>
    match parse name j (← loadSecrets) with
    | .ok c    => return some c
    | .error e => throw (.userError e)

/-- Every client that can be read. One that cannot is reported on stderr and left out: a broken
    file locks out the client it describes, not every other one. -/
def loadAllClients : IO (Array ApiClient) := do
  let dir ← clientsDir
  if !(← dir.pathExists) then return #[]
  let mut out := #[]
  for entry in ← dir.readDir do
    let some stem := entry.fileName.dropSuffix? ".json" | continue
    let name := stem.toString
    unless Utils.validConfigName name do continue
    try
      if let some c ← loadClient name then out := out.push c
    catch e =>
      IO.eprintln s!"[clients] {e}"
  return out

/-- The client `token` belongs to, if any. Every client is compared, in constant time each, so
    the answer takes as long for the first client as for none.

    A token two clients share names neither: which of them a request came from would depend on
    the order the directory happens to list, and so would every policy it is held to. -/
def authenticate (token : String) : IO (Option ApiClient) := do
  let mut found : Array ApiClient := #[]
  for c in ← loadAllClients do
    if Secret.constantTimeEq c.token token then found := found.push c
  if found.size > 1 then
    IO.eprintln s!"[clients] {String.intercalate ", " (found.map (·.name)).toList} share a \
token; refusing it for all of them"
    return none
  return found[0]?

/-! ## Queueing -/

/-- What a client asked `POST /api/v1/queue` for, before any of it has been checked against its
    policy. -/
structure QueueRequest where
  spawn       : SpawnRequest
  goal        : Option String := none
  issueNumber : Option Nat := none
  externalId  : Option String := none
deriving Inhabited

private def requestKeys : List String :=
  ["prompt", "goal", "upstream", "backend", "model", "tools", "budget", "issue_number",
   "external_id", "kleis_narrow", "execution_image", "checkout"]

/-- Read a queue request out of a request body, strictly: an unknown field is refused, because a
    field the client believes it set — a `priority`, a `fork` — and the server ignored is a task
    run on terms nobody asked for. -/
def parseRequest (j : Json) : Except String QueueRequest := do
  if j.getObj?.toOption.isNone then .error "the body must be a JSON object"
  let unknown := unknownKeys j requestKeys
  unless unknown.isEmpty do
    .error s!"unknown field(s) {String.intercalate ", " unknown}; a queue request may carry \
{String.intercalate ", " requestKeys}"
  let some prompt ← optString j "prompt" "the request"
    | .error "the request has no 'prompt'"
  let backend ← optString j "backend" "the request"
  let model ← optString j "model" "the request"
  -- A blank model is no model, as for an interactive session: `--model ""` is refused by the
  -- vendor CLI only after the clone and the sandbox launch.
  let model := model.filter (!·.trimAscii.isEmpty)
  let tools ← match j.getObjVal? "tools" with
    | .error _ | .ok .null => pure none
    | .ok v => match (FromJson.fromJson? v : Except String (List String)) with
      | .ok ts => pure (some ts)
      | .error _ => .error "'tools' must be a list of strings"
  let repo ← match ← optString j "upstream" "the request" with
    | none => pure none
    | some s =>
      let r ← Repository.parse s |>.mapError (fun _ => s!"invalid upstream '{s}', expected \
'owner/repo'")
      -- Both halves become path components of a clone directory.
      Utils.checkConfigName "upstream owner" r.owner
      Utils.checkConfigName "upstream name" r.name
      pure (some r)
  let budget ← match j.getObjVal? "budget" with
    | .error _ | .ok .null => pure none
    | .ok v => match v.getNum? with
      | .ok n =>
        -- A floor as well as the policy's ceiling, as for an interactive session: a budget under
        -- a cent reaches the agent as `--max-budget-usd 0.000000`, a run that clones, mints a
        -- token and launches a sandbox in order to refuse its first turn.
        if n.toFloat < 0.01 then .error "'budget' must be at least 0.01 USD"
        else pure (some n.toFloat)
      | .error _ => .error "'budget' must be a number of USD"
  let issueNumber ← match j.getObjVal? "issue_number" with
    | .error _ | .ok .null => pure none
    | .ok v => match v.getNat? with
      | .ok n => pure (some n)
      | .error _ => .error "'issue_number' must be a whole number"
  if issueNumber.isSome && repo.isNone then
    .error "'issue_number' names an issue or pull request, which needs an 'upstream' to be one of"
  -- No `series`: a series is a name the operator's own runs share, and a client naming one would
  -- move its pointer to the client's task, which `orchestra resume` then continues outside the
  -- client's policy.
  let goal ← optString j "goal" "the request"
  let externalId ← optString j "external_id" "the request"
  if let some e := externalId then
    if e.trimAscii.isEmpty || e.length > 200 then
      .error "'external_id' must be between 1 and 200 characters"
  let kleisNarrow ← parseKleisNarrow? j
  -- Checked for being an image reference here, and against `queue_policy.images` by `resolve`.
  let executionImage ← parseExecutionImage? j
  -- Checked for its shape here — a full commit name, a `fetch_ref` under `refs/pull/` or
  -- `refs/heads/` — and for being the upstream's when the checkout is made: a client can only
  -- ever name code the upstream has published (`Repo.applyCheckout`).
  let checkout ← parseCheckout? j
  if checkout.isSome && repo.isNone then
    .error "'checkout' names a commit of the 'upstream', and the request names none"
  return { spawn := { prompt, backend, model, tools, repo, budget, kleisNarrow, executionImage,
                      checkout }
         , goal, issueNumber, externalId }

/-- Check `r` against the client's policy and fill in everything it left out. Pure. -/
def ApiClient.resolve (c : ApiClient) (r : QueueRequest) : Except String ResolvedSpawn :=
  c.policy.resolve c.defaults r.spawn |>.mapError fun e =>
    -- `Spawn` speaks to an agent about "this task" and its "spawn policy"; a client is neither.
    ((e.replace "this task" s!"client '{c.name}'").replace "its spawn policy"
      "its queue_policy").replace "the spawn policy" "its queue_policy"

/-- The queue entry for a request the policy has approved. Pure, so what an entry carries can be
    tested without a daemon: the fork is the caller's to resolve. -/
def ApiClient.entry (c : ApiClient) (r : QueueRequest) (resolved : ResolvedSpawn)
    (fork : Option Repository) (id createdAt : String) : Queue.QueueEntry :=
  { id, createdAt
  , repo := match resolved.repo, fork with
      | some upstream, some f => some { upstream, fork := f }
      | some upstream, none   => some { upstream, fork := upstream }
      | none, _               => none
  , mode := .pr
  , prompt := resolved.prompt
  , goal := r.goal
  , backend := resolved.backend
  , model := resolved.model
  -- Always `some`: an absent list makes a task fall back to `mode` for its tools.
  , tools := some resolved.tools
  , budget := resolved.budget
  , readOnly := resolved.readOnly
  , priority := resolved.priority
  , issueNumber := r.issueNumber
  , kleisSpec := resolved.kleisSpec
  , kleisNarrow := resolved.kleisNarrow
  -- Never a policy of its own: what a client queues cannot queue more.
  , spawnPolicy := none
  , apiClient := some c.name
  , externalId := r.externalId
  -- The client's image, or one its policy lists that the request named.
  , executionImage := resolved.executionImage
  , checkout := resolved.checkout }

end Orchestra.ApiClient