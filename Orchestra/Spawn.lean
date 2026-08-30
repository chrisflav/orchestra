import Lean.Data.Json
import Orchestra.Config

open Lean (Json FromJson ToJson)

namespace Orchestra

/-! # Queueing a task from inside a task

An agent can put work on orchestra's queue itself, through the `queue_task` MCP tool, and bind
a taxis issue to it — optionally claiming that issue on the new task's behalf so no dispatcher
hands it to anyone else in the meantime.

Every other tool an agent holds does one bounded thing: `create_pr` opens a pull request against
the repository the task already has, `claim_issue` takes a lock on an issue that already exists.
This one hands the agent a *whole task* to configure — which backend runs it, on which model,
holding which tools, against which repository — and each of those is a way to reach further than
the task itself was ever granted. So the tool has no fixed capability at all: everything it may
do is written down in a `SpawnPolicy`, and **the policy is the switch**. A task with no policy is
not offered the tool, and there is no permission label that turns it on without one — a label
would be a second way to enable a tool whose whole safety is that somebody wrote down its bounds.

A policy is configured where the task it applies to is configured: `spawn_policy` on a queue
entry or a task file, on a listener's `action`, or on a role (which is what the two dispatchers
build their entries from). See `docs/queue-task.md`.

## The rules a policy states

Three of the four choices follow one rule:

> **Omitted means "the same as mine".** A named value must appear in the policy's list for that
> field; an empty list means nothing may be named, so the child inherits and the agent has no
> say at all.

That holds for `backend`, `model` and `repo` alike, and it is why an empty policy is still a
useful one: it lets a task queue more of *itself* — the same backend, model and repository, with
the tools it already holds — and nothing else. The lists widen what may be named; they never
take away what the spawning task already has.

`tools` follows the same rule with one wrinkle worth stating out loud: the policy's list may name
tools the spawning task does not hold, and that is deliberate rather than an oversight. A
read-only planner queueing an implementor that can open a pull request is the case the tool
exists for, and the operator writing the policy is the one deciding it.

## Two bounds worth stating, because they are not symmetric

`max_budget` is both a ceiling and the default a queued task gets when the agent names none.
`tools` is a ceiling only: omitting it inherits what the queueing task holds, it does not hand
over the policy's whole list. So a policy listing `create_pr` permits an implementor that can
open a pull request; the agent still has to *ask* for it.

And `allow_pre_claim` grants something the `tools` vocabulary cannot express. Taking a claim is
otherwise reached through `claim_issue`, which needs `work_issues`; a policy may set
`allow_pre_claim` on a task holding neither. That is the point — the queued task is the one that
will do the work, and claiming it at queue time is what stops a dispatcher handing the same issue
to somebody else in between — but it is a capability granted by this field alone, and no check
against `Project.Role.knownPermissions` will tell an operator so.

## What the agent never chooses

`priority` comes from the policy alone: a queue-wide priority is only meaningful against every
other entry in the queue, and is a way to starve all of them. `read_only` is the policy's too,
but it *inherits* rather than resetting — see the field.

Nor does a spawned task carry a policy of its own: `queue_task` is one level deep, always. That
is what bounds the fan-out to `max_tasks` instead of `max_tasks` to the power of however many
generations the agent felt like — and it is a rule rather than a depth counter because a counter
has to be got right in every path that builds an entry, while "the field is never set" only has
to be got right where entries are built from a spawn. -/

/-- What the agent asked `queue_task` for, before any of it has been checked. -/
structure SpawnRequest where
  prompt   : String
  backend  : Option String := none
  model    : Option String := none
  /-- `none` is "whatever I hold"; `some []` is a task with no optional tools at all, which is a
      thing worth being able to ask for. -/
  tools    : Option (List String) := none
  repo     : Option Repository := none
  budget   : Option Float := none
  issueId  : Option Taxis.IssueId := none
  preClaim : Bool := false
deriving Repr, Inhabited

/-- What the spawning task itself has, which is what every omitted field inherits. -/
structure SpawnContext where
  backend   : Option String := none
  model     : Option String := none
  /-- Only the upstream is carried: the fork of the queued task is resolved when it is queued,
      even when the repository is inherited, so that one rule decides it everywhere. -/
  repo      : Option Repository := none
  tools     : List String := []
  readOnly  : Bool := false
  projectId : Option Taxis.IssueId := none
deriving Repr, Inhabited

/-- A spawn the policy has approved, in the shape the queue needs it. -/
structure ResolvedSpawn where
  prompt    : String
  backend   : Option String
  model     : Option String
  tools     : List String
  /-- Upstream repository; `none` for a repository-independent task. -/
  repo      : Option Repository
  budget    : Option Float
  priority  : Nat
  readOnly  : Bool
  projectId : Option Taxis.IssueId
  issueId   : Option Taxis.IssueId
  /-- The subtree the queued task may write at or below: the *queueing* task's own scope, filled
      in by the caller because it costs taxis reads to work out. Carried onto the entry so the
      child cannot re-derive a wider one from the issue it was bound to (`Config.IOTask.scopeRoot`
      and `Tools.writeScopeRoot`). -/
  scopeRoot : Option Taxis.IssueId := none
  preClaim  : Bool
  /-- Carried through so the enqueuing side can report the ceiling it enforced with the number
      in it, rather than saying "too many" and leaving the agent to guess. -/
  maxTasks  : Nat
deriving Repr, Inhabited

/-- One field's worth of the inherit-or-name rule, shared by `backend` and `model` so the two
    cannot drift: `none` takes `mine`, and anything named has to be on `allowed`. -/
private def chooseFrom (field : String) (allowed : List String) (mine : Option String)
    (asked : Option String) : Except String (Option String) :=
  match asked with
  | none => .ok mine
  | some a =>
    if allowed.contains a then .ok (some a)
    else if allowed.isEmpty then
      .error s!"this task may not choose the {field} of a task it queues, only inherit its own \
({mine.getD "unset"}); no {field}s are listed in its spawn policy"
    else
      .error s!"{field} '{a}' is not one this task may queue onto; the spawn policy allows \
{String.intercalate ", " allowed}"

/-- Check `r` against `p` and fill in everything the agent left out. Pure: the queue, taxis and
    GitHub are all the caller's business, and this is the part worth testing without them. -/
def SpawnPolicy.resolve (p : SpawnPolicy) (ctx : SpawnContext) (r : SpawnRequest) :
    Except String ResolvedSpawn := do
  if r.prompt.trimAscii.isEmpty then
    .error "'prompt' is empty; a queued task with nothing to say runs an agent against a blank \
instruction"
  let backend ← chooseFrom "backend" p.backends ctx.backend r.backend
  let model   ← chooseFrom "model"   p.models   ctx.model   r.model
  let repo ← match r.repo with
    | none => .ok ctx.repo
    | some want =>
      if p.repos.contains want then .ok (some want)
      else if p.repos.isEmpty then
        .error s!"this task may not choose the repository of a task it queues, only inherit its \
own ({ctx.repo.map (·.toString) |>.getD "none"}); no repositories are listed in its spawn policy"
      else
        .error s!"repository {want} is not one this task may queue onto; the spawn policy \
allows {String.intercalate ", " (p.repos.map (·.toString))}"
  let tools ← match r.tools with
    | none => .ok ctx.tools
    | some want =>
      -- The spawning task's own tools are grantable without being listed: naming what you
      -- already hold reaches nothing new, and refusing it would make the common case — queue
      -- another one of me — require the operator to list the task's own permissions back to it.
      let ungrantable := want.filter (fun t => !(p.tools.contains t || ctx.tools.contains t))
      if !ungrantable.isEmpty then
        .error s!"this task may not grant {String.intercalate ", " ungrantable} to a task it \
queues; its spawn policy allows {if p.tools.isEmpty then "no tools beyond the ones this task \
holds itself" else String.intercalate ", " p.tools}"
      else .ok want
  let budget ← match r.budget, p.maxBudget with
    | none,   ceiling => .ok ceiling
    -- Closed like every other field, rather than open. A budget is the one thing here that
    -- spends real money, and "the policy said nothing about it" has to mean the agent may not
    -- name one — otherwise the field that bounds *how many* tasks may be queued would sit next
    -- to an unbounded amount each of them may spend.
    | some _, none    =>
      .error "this task may not choose the budget of a task it queues, only inherit orchestra's default; no 'max_budget' is set in its spawn policy"
    | some b, some c  =>
      -- Refused rather than lowered to the ceiling. A task queued at a quarter of the budget its
      -- prompt assumes stops halfway through and looks like a failure; a refusal with the
      -- ceiling in it is something the agent can act on in the same turn.
      if b ≤ c then .ok (some b)
      else .error s!"budget {b} is over the {c} this task may queue"
  if r.preClaim then
    if r.issueId.isNone then
      .error "'pre_claim' was asked for without an 'issue_id'; there is nothing to claim"
    if !p.allowPreClaim then
      .error "this task may not claim an issue for a task it queues; its spawn policy does not \
set 'allow_pre_claim'"
  return { prompt := r.prompt, backend, model, tools, repo, budget
         , priority := p.priority, readOnly := p.readOnly.getD ctx.readOnly
         , projectId := ctx.projectId, issueId := r.issueId, preClaim := r.preClaim
         , maxTasks := p.maxTasks }

end Orchestra
