import Lean.Data.Json
import Orchestra.Config
import Orchestra.Dirs
import Orchestra.Utils.Files

open Lean (Json FromJson ToJson)

/-!
# Identities

An *identity* is a persistent someone for a task to be. It is the fourth configuration surface,
after listeners, roles and skills, and the only one that outlives the run it is used in: a task
names an identity, and the identity hands it a memory that was there before the task started and
is still there afterwards, plus the credentials to act on the tracker as itself rather than as
the instance.

```
<config>/identities/<name>.json     -- the record
<data>/identities/<name>/memory/    -- its memory, mounted read-write into the sandbox
```

An identity is *not* a role and the two are worth keeping apart. A role says what a task does —
its prompt, its tools, its model. An identity says who does it, and it is the half that
accumulates: two tasks dispatched for the same role a week apart share nothing, while two tasks
run under the same identity share everything the first one wrote down.

## Why the memory is not under `<data>/memory`

The obvious home for it would be beside the global and per-project memories, as
`<data>/memory/identities/<name>`. It cannot go there. Global memory is the memory *root*
itself — `resolveMemoryDirs` hands `<data>/memory` to any task whose `memory` is `global` or
`both` — so every directory below it is readable and writable by every such task. An identity's
memory kept there would be one every ordinary task could read and rewrite, which is the opposite
of dedicated. A separate root under `<data>/identities` is reachable only by naming the identity.

For the same reason `memory: "none"` does not suppress it. That field chooses among the *shared*
memories; the identity's own memory is part of the identity, and a task that asked to run as
somebody and got none of their memory would have got the shell of an identity and not the thing.

## What "acting as" reaches, and what it does not

An identity may carry a `taxis_token`. Every write a task's issue tools make — comments, reviews,
issues created and updated, context notes — is then made with that token, so the tracker records
the identity as the author instead of whatever actor orchestra's own token belongs to. This is
the whole of the first version. GitHub is still reached with orchestra's App installation token
and the configured PAT: a pull request an identity opens is opened by orchestra, and there is no
per-identity credential for it here.

Claims are the deliberate exception on the taxis side. `o-claimed` is orchestra's own bookkeeping,
written and read back by the daemon rather than authored by anyone, and the holder it records is
a task id. It stays on the instance token so that a claim taken under an identity whose token is
later rotated or revoked is still one the daemon can release.

Permissions per identity are not here either. They want an authorization service to hold them
(kleis), and a permission list in this file would be a second, weaker answer to the question that
service exists to answer — one every task-configuration surface could already override, since
`tools` is written on the task. The token is what an identity carries for now, and the tracker is
what enforces what that token may do.

## An identity is assigned, never chosen

Nothing an agent says selects an identity: it is written on the task, the role or the listener
action, all of which are configuration. `queue_task` does not offer it as a field either — a task
queued by a task inherits the identity of the task that queued it (`SpawnContext`), so an agent
can hand its own identity onwards but cannot put on another one.
-/

namespace Orchestra.Identity

/-- One identity, as configuration writes it and as a task carries it. -/
structure Identity where
  /-- The name a task, role or listener action refers to it by, and the file it is stored in. -/
  name : String
  /-- One or two sentences saying who this is, shown to the agent in its system prompt.

      Worth writing even though nothing reads it mechanically: the agent is told the name
      regardless, and a name with nothing behind it ("maintainer") tells it as little as it tells
      a person reading the tracker afterwards. -/
  description : Option String := none
  /-- The taxis API token this identity's tracker writes are made with. `none` leaves them on
      orchestra's own token, which is a usable identity — one with a dedicated memory and no
      separate presence on the tracker. -/
  taxisToken : Option String := none
deriving Repr, Inhabited

instance : ToJson Identity where
  toJson i :=
    let fields : List (String × Json) := [("name", Json.str i.name)]
    let fields := if let some d := i.description then fields ++ [("description", Json.str d)]
                  else fields
    -- The token is deliberately not serialized. This instance is what an API listing and
    -- `orchestra` printing a record would use, and a credential that goes out over either is a
    -- credential in a log. Reading a record back through `FromJson` is therefore not a
    -- round-trip, which is the right asymmetry: the file on disk is the only place the token
    -- lives.
    Json.mkObj fields

instance : FromJson Identity where
  fromJson? j := do
    let name        ← j.getObjValAs? String "name"
    let description := j.getObjValAs? String "description" |>.toOption
    let taxisToken  := j.getObjValAs? String "taxis_token"  |>.toOption
    return { name, description, taxisToken }

/-! ## Filesystem layout -/

/-- Optional override for the identities directory (tests redirect this, exactly as
    `Project.globalRolesDirOverride` does for roles). -/
initialize identitiesDirOverride : IO.Ref (Option System.FilePath) ← IO.mkRef none

def setIdentitiesDirOverride (p : Option System.FilePath) : IO Unit :=
  identitiesDirOverride.set p

/-- Where the records live: `<config>/identities`. -/
def identitiesDir : IO System.FilePath := do
  match ← identitiesDirOverride.get with
  | some p => return p
  | none   => return (← Dirs.configBase) / "identities"

def identityFile (name : String) : IO System.FilePath := do
  Utils.ensureConfigName "identity" name
  return (← identitiesDir) / s!"{name}.json"

/-- The root of one identity's own data: `<data>/identities/<name>`. -/
def identityDataDir (name : String) : IO System.FilePath := do
  Utils.ensureConfigName "identity" name
  return (← Dirs.dataBase) / "identities" / name

/-- The identity's memory directory. Not created — see `ensureMemoryDir`. -/
def memoryDir (name : String) : IO System.FilePath := do
  return (← identityDataDir name) / "memory"

/-- The identity's memory directory, created if this is the first task to run as it. -/
def ensureMemoryDir (name : String) : IO System.FilePath := do
  let dir ← memoryDir name
  IO.FS.createDirAll dir
  return dir

/-! ## Reading -/

/-- Read one identity. `none` when no record of that name exists.

    A file that exists and cannot be read is an error rather than a `none`, which is where this
    parts company with `Project.loadRole`. A role that fails to parse takes that role out of the
    dispatcher's rotation and nothing else happens; an identity that answered `none` would leave
    the task it belongs to looking exactly like a task that named no identity at all — running on
    orchestra's own taxis token, against no memory, with the run's whole point silently dropped.

    `{{secret}}` substitution is applied first, so the `taxis_token` can be held in
    `secrets.json` with the record naming it rather than holding it. -/
def loadIdentity (name : String) : IO (Option Identity) := do
  let path ← identityFile name
  if !(← path.pathExists) then return none
  let secrets ← loadSecrets
  let identity ← loadJsonFileWithSecrets Identity path secrets
  -- The filename is what every other surface names it by, so a record whose `name` disagrees
  -- would be reachable under one spelling and print itself under another.
  unless identity.name == name do
    throw (.userError s!"{path}: this record names the identity '{identity.name}', but it is \
stored as '{name}'")
  return some identity

private def stripJsonExt (s : String) : Option String :=
  let ext := ".json"
  if s.endsWith ext then some (s.dropEnd ext.length).toString else none

/-- Every configured identity, ordered by name so a listing is stable across calls.

    Unreadable records are reported on stderr and skipped rather than failing the listing: this
    is what answers "which identities are there?", and one broken file should not stop it
    answering. The task path uses `requireIdentity`, which does fail. -/
def loadAllIdentities : IO (Array Identity) := do
  let dir ← identitiesDir
  if !(← dir.pathExists) then return #[]
  let mut names : Array String := #[]
  for entry in ← System.FilePath.readDir dir do
    if let some name := stripJsonExt entry.fileName then
      if Utils.validConfigName name then names := names.push name
  let mut out : Array Identity := #[]
  for name in names.qsort (· < ·) do
    try
      if let some i ← loadIdentity name then out := out.push i
    catch e =>
      IO.eprintln s!"[identity] {name}: not a usable identity record, so it is being skipped: {e}"
  return out

/-- Resolve the identity a task names, or fail saying which ones exist.

    Every path that runs a task as somebody goes through this. A name with no record behind it is
    a configuration mistake with no safe reading: running the task anyway would run it as the
    instance, writing to the tracker under orchestra's own actor and into a memory the identity
    never sees, and it would look like it had worked. -/
def requireIdentity (name : String) : IO Identity := do
  match ← loadIdentity name with
  | some i => return i
  | none   =>
    let known := (← loadAllIdentities).map (·.name)
    let listed := if known.isEmpty then "none are configured"
                  else String.intercalate ", " known.toList
    let dir ← identitiesDir
    throw (.userError s!"no identity named '{name}' is configured in {dir} ({listed})")

end Orchestra.Identity
