import Lean.Data.Json
import Orchestra.Config
import Orchestra.TaskStore
import Orchestra.Taxis
import Orchestra.Utils.Labels

open Lean (Json FromJson ToJson)

namespace Orchestra.Project

/-! ## Targets

A `RepoTarget` is the (repo, branch) pair against which an issue's PRs must
be opened. Issues may set this individually (multi-org projects); otherwise
they inherit the project's `defaultTarget`. -/

structure RepoTarget where
  repo   : Repository
  branch : String
deriving Repr, Inhabited, BEq

instance : ToJson RepoTarget where
  toJson t := Json.mkObj [("repo", ToJson.toJson t.repo), ("branch", Json.str t.branch)]

instance : FromJson RepoTarget where
  fromJson? j := do
    let repo   ← j.getObjValAs? Repository "repo"
    let branch ← j.getObjValAs? String "branch"
    return { repo, branch }

/-! ## Pull request references

A pointer from an issue to an open or merged PR. `taskId` is the orchestra
task that produced the PR (so the merger task can rebuild context). -/

structure PRRef where
  repo   : Repository
  number : Nat
  branch : String
  taskId : Option String := none
deriving Repr, Inhabited, BEq

instance : ToJson PRRef where
  toJson p :=
    let base : List (String × Json) :=
      [ ("repo",   ToJson.toJson p.repo)
      , ("number", Json.num p.number)
      , ("branch", Json.str p.branch) ]
    let fields := if let some t := p.taskId then base ++ [("task_id", Json.str t)] else base
    Json.mkObj fields

instance : FromJson PRRef where
  fromJson? j := do
    let repo   ← j.getObjValAs? Repository "repo"
    let number ← j.getObjValAs? Nat "number"
    let branch ← j.getObjValAs? String "branch"
    let taskId := j.getObjValAs? String "task_id" |>.toOption
    return { repo, number, branch, taskId }

/-- `PRRef` as a taxis `github-pr` artifact payload — that handler's `validate` requires a `url`
    field (see `Taxis.Plugins.Github`), which `PRRef.toJson` alone doesn't produce; this adds it
    while keeping `repo`/`number`/`branch`/`task_id` too, so `FromJson PRRef` still round-trips it
    unchanged on read. -/
def prRefArtifactPayload (pr : PRRef) : Json :=
  let base : List (String × Json) :=
    [ ("url", Json.str s!"https://github.com/{pr.repo}/pull/{pr.number}")
    , ("repo", ToJson.toJson pr.repo)
    , ("number", Json.num pr.number)
    , ("branch", Json.str pr.branch) ]
  Json.mkObj (base ++ (pr.taskId.map fun t => [("task_id", Json.str t)] : Option _).getD [])

/-! ## Issue lifecycle -/

inductive IssueStatus where
  | open
  | claimed
  | completed
  | abandoned
deriving Repr, Inhabited, BEq, DecidableEq

instance : ToJson IssueStatus where
  toJson
    | .open      => "open"
    | .claimed   => "claimed"
    | .completed => "completed"
    | .abandoned => "abandoned"

instance : FromJson IssueStatus where
  fromJson?
    | .str "open"      => .ok .open
    | .str "claimed"   => .ok .claimed
    | .str "completed" => .ok .completed
    | .str "abandoned" => .ok .abandoned
    | j => .error s!"expected issue status string, got {j}"

/-! ## Project record -/

/-- Optional automatic reviewer task template (decision F1).
    When set on a `Project`, `attach_pr` will enqueue a review task for the
    new PR using these parameters. Leave `none` to disable auto-review. -/
structure ReviewerTemplate where
  /-- Agent backend label for the reviewer (e.g. "claude"). Defaults to claude. -/
  backend : Option String := none
  /-- Prompt template; supports {{repo}}, {{pr_number}}, {{branch}}, {{issue_id}}. -/
  promptTemplate : String
deriving Repr, Inhabited

instance : ToJson ReviewerTemplate where
  toJson r :=
    let base : List (String × Json) := [("prompt_template", Json.str r.promptTemplate)]
    let fields := if let some b := r.backend then base ++ [("backend", Json.str b)] else base
    Json.mkObj fields

instance : FromJson ReviewerTemplate where
  fromJson? j := do
    let promptTemplate ← j.getObjValAs? String "prompt_template"
    let backend := j.getObjValAs? String "backend" |>.toOption
    return { backend, promptTemplate }

structure Project where
  id            : Taxis.IssueId
  name          : String
  description   : Option String      := none
  createdAt     : String
  /-- Default target inherited by issues that don't set their own. Multi-org
      projects can leave this `none` and require each issue to specify. -/
  defaultTarget : Option RepoTarget  := none
  /-- Optional reviewer template (F1). Set to enable auto-review on attach_pr. -/
  reviewer      : Option ReviewerTemplate := none
deriving Repr, Inhabited

/-! ## Issue record

`parentId` is the only hierarchy pointer (decision B1); backed 1:1 by taxis's own single-parent
`Issue.parent`. The child set is recovered by scanning the project's issues, same as before this
migration (taxis has no server-side "children of X" filter). -/

structure Issue where
  id           : Taxis.IssueId
  projectId    : Taxis.IssueId
  parentId     : Option Taxis.IssueId  := none
  title        : String
  description  : String
  /-- The condition that decides whether the issue is done, as taxis's own `goal` field. Empty
      when unset. Kept apart from `description` on purpose: it is what a task launched for this
      issue is held to (see `Sandbox.launchAgent`'s `goal`), and the description — issue body
      plus rendered comment thread — is far too long to serve as one. -/
  goal         : String          := ""
  status       : IssueStatus     := .open
  /-- Per-issue override of the project's default target. -/
  target       : Option RepoTarget := none
  attachedPRs  : Array PRRef     := #[]
  /-- Issues that must be completed before this one can be dispatched. Native taxis dependency
      graph — no separate plumbing needed. -/
  dependencies : Array Taxis.IssueId   := #[]
  createdAt    : String
  updatedAt    : String
deriving Repr, Inhabited

/-! ## taxis backing

Every `Project`/`Issue` is a taxis issue: a project is one carrying the `t-project` label; an
Orchestra issue belonging to project `pid` is a taxis issue whose native `parent` is `pid` (for a
root issue) or another Orchestra issue's id (for a sub-issue, e.g. one produced by `split_issue`).
A project id and an issue id are therefore both plain `Taxis.IssueId` — there is no separate
"project" concept on the wire, only issues, some of which carry the `t-project` label.

`RepoTarget`/`PRRef`/`ReviewerTemplate` have no native taxis field: they're encoded as a JSON
metadata blob appended to the taxis issue's `description`, behind a machine-readable marker
(a trailing fenced ` ```orchestra-meta ` block), parsed back out on load and stripped before a
human ever sees the description. `IssueStatus` has no native field either: it's `state` (open /
closed / completed) plus the single `o-claimed` label — `open`/`completed`/`abandoned` need none,
see `statusOf`/`labelsFor`).

Nothing else is a status. Three conditions people reach for as one are read from the data
instead, so none of them can drift out of step with it and issues touched directly in the taxis
UI behave the same as ones the tools created:

* **decomposed** — the issue has open children (`dispatchCandidates`);
* **awaiting review** — it is open with an unmerged pull request attached;
* **rejected** — its latest review comment asked for changes (`ReviewState.requestChanges`).

There is likewise no "in review" status. An issue is under review when it is open and carries an
attached pull request that has not been merged — read from the tree and from GitHub rather than
from a label that something has to remember to set and clear.

There is deliberately no "blocked" status. A parent that has been decomposed is recognised by
still having open children (`dispatchCandidates`), which needs no label to maintain and cannot
drift out of sync with the tree the way a label can. -/

private def metaFence := "\n```orchestra-meta\n"
private def metaFenceEnd := "\n```"

/-! `encodeMeta`/`decodeMeta` are the wire format for everything taxis has no native field for,
so they're not `private`: `OrchestraTest.ProjectMeta` round-trips them directly, which is the
one part of this module testable without a live taxis instance. -/

/-- Append `metaFields` (if non-empty) to `human` as a trailing fenced metadata block. -/
def encodeMeta (human : String) (metaFields : List (String × Json)) : String :=
  if metaFields.isEmpty then human
  else human ++ metaFence ++ (Json.mkObj metaFields).compress ++ metaFenceEnd

/-- Split a taxis issue description into (human-readable text, metadata) by finding the trailing
    `metaFence` marker, if any. Splits on the *last* fence, not the first: `encodeMeta` always
    appends its block at the end, and a human description may legitimately contain the marker
    (an issue *about* this format, say) without that truncating everything after it.

    Tolerates a missing/malformed block by treating the whole description as human text — a
    corrupt metadata blob should never make an issue unreadable. That fallback is stable rather
    than compounding: re-encoding the returned `raw` appends any fresh metadata *after* the
    corrupt block, and this function then reads back the fresh one. -/
def decodeMeta (raw : String) : String × Json :=
  match raw.splitOn metaFence with
  | [] | [_] => (raw, Json.mkObj [])
  | parts =>
    let human := metaFence.intercalate parts.dropLast
    match (parts.getLastD "").splitOn metaFenceEnd with
    | metaStr :: _ =>
      match Json.parse metaStr with
      | .ok j => (human, j)
      | .error _ => (raw, Json.mkObj [])
    | [] => (raw, Json.mkObj [])

/-! ## Status ↔ (state, label) mapping -/

private structure StatusLabelIds where
  claimed  : Orchestra.Taxis.LabelId
  project  : Orchestra.Taxis.LabelId

private initialize statusLabelIdsRef : IO.Ref (Option StatusLabelIds) ← IO.mkRef none

/-- Resolve (creating on first use if necessary) the fixed set of taxis labels the status mapping
    and project marker need. Label ids don't change once created, so this is cached for the
    process's lifetime — every call after the first is free. -/
private def statusLabelIds : IO StatusLabelIds := do
  match ← statusLabelIdsRef.get with
  | some ids => return ids
  | none =>
    let cfg ← Orchestra.Taxis.getConfig
    let ensure (name : String) : IO Orchestra.Taxis.LabelId := do
      match ← Orchestra.Taxis.ensureLabel cfg name with
      | .ok id => pure id
      | .error e => throw (.userError s!"taxis: failed to ensure label '{name}': {e}")
    let ids : StatusLabelIds :=
      { claimed  := ← ensure "o-claimed"
        project  := ← ensure "t-project" }
    statusLabelIdsRef.set (some ids)
    return ids

/-- Whether an issue anchors a project: it carries `t-project`, or it has both a `repository` and
    a `github-branch` artifact.

    The single definition behind both `findIssue` (which project an issue belongs to) and
    `projectRootOf` (what an agent may write to). They used to disagree — this one accepted only
    `t-project` — so in a label-dispatched tree with no `t-project` anywhere, every agent-side
    tool that resolves an issue reported it as not found, while write scoping happily anchored on
    the artifacts. The artifact pair is the same marker the dispatcher already uses to decide
    where an issue's target comes from, so treating it as a project boundary here is what makes
    "the project this work belongs to" mean one thing across the codebase. -/
private def anchorsProject (ids : StatusLabelIds) (detail : Orchestra.Taxis.IssueDetail) : Bool :=
  detail.issue.labels.contains ids.project ||
    (detail.attachedArtifacts.any (·.kind == "repository") &&
     detail.attachedArtifacts.any (·.kind == "github-branch"))

/-- The `IssueStatus` a taxis issue's `state` + `labels` represent. -/
private def statusOf (ids : StatusLabelIds) (state : Orchestra.Taxis.IssueState) (labels : Array Orchestra.Taxis.LabelId) :
    IssueStatus :=
  match state with
  | .completed => .completed
  | .closed => .abandoned
  | .open =>
    if labels.contains ids.claimed then .claimed
    else .open

/-- The taxis `state` a given `IssueStatus` maps to. -/
private def stateOf : IssueStatus → Orchestra.Taxis.IssueState
  | .completed => .completed
  | .abandoned => .closed
  | _ => .open

/-- Rewrite `current`'s status labels (clearing all three and re-adding the one for `status`, if
    any) — always exactly zero or one of the three is present afterward. -/
private def labelsFor (ids : StatusLabelIds) (current : Array Orchestra.Taxis.LabelId) (status : IssueStatus) :
    Array Orchestra.Taxis.LabelId :=
  let statusIds := #[ids.claimed]
  let cleared := current.filter (fun l => !statusIds.contains l)
  match status with
  | .claimed => cleared.push ids.claimed
  | .open | .completed | .abandoned => cleared

/-! ## Conversions -/

private def toProject (raw : Orchestra.Taxis.Issue) : IO Project := do
  let (human, metaFields) := decodeMeta raw.description
  let createdAt ← Orchestra.Taxis.epochToIso8601 raw.createdAt
  return {
    id := raw.id
    name := raw.title
    description := if human.isEmpty then none else some human
    createdAt
    defaultTarget := metaFields.getObjValAs? RepoTarget "target" |>.toOption
    reviewer := metaFields.getObjValAs? ReviewerTemplate "reviewer" |>.toOption }

/-- `raw.id`'s parent, reinterpreted in Orchestra's terms: `none` if it points at the project
    itself (the implicit root, not a "parent issue" from Orchestra's point of view) or is absent,
    `some` otherwise. -/
private def orchestraParentOf (projectId : Taxis.IssueId) (raw : Orchestra.Taxis.Issue) : Option Taxis.IssueId :=
  match raw.parent with
  | none => none
  | some p => if p.val == projectId.val then none else some p

private def toIssue (ids : StatusLabelIds) (projectId : Taxis.IssueId) (raw : Orchestra.Taxis.Issue)
    (artifacts : Array Orchestra.Taxis.ArtifactView := #[]) : IO Issue := do
  let (human, metaFields) := decodeMeta raw.description
  let attachedPRs := artifacts.filterMap fun a =>
    if a.kind == "github-pr" then (FromJson.fromJson? a.payload : Except String PRRef).toOption else none
  let createdAt ← Orchestra.Taxis.epochToIso8601 raw.createdAt
  let updatedAt ← Orchestra.Taxis.epochToIso8601 raw.updatedAt
  return {
    id := raw.id
    projectId
    parentId := orchestraParentOf projectId raw
    title := raw.title
    description := human
    goal := raw.goal
    status := statusOf ids raw.state raw.labels
    target := metaFields.getObjValAs? RepoTarget "target" |>.toOption
    attachedPRs
    dependencies := raw.dependencies
    createdAt
    updatedAt }

/-! ## Persistence

Unlike the pre-migration file-based API, creating and updating are genuinely different
operations here (taxis assigns an issue's id on creation — there's no way to know it in advance
the way `freshProjectId`/`freshIssueId` used to hand out a locally-generated one). `saveProject`/
`saveIssue` are therefore update-only (PATCH an *existing* record); use `createProject`/
`createIssue` to make a new one. -/

private def unwrap {α} : Except String α → IO α
  | .ok v => pure v
  | .error e => throw (.userError s!"taxis: {e}")

/-- Bound on how far an ancestor walk (`findIssue`) follows `parent` pointers before giving up. -/
private def maxAncestorDepth : Nat := 256

def createProject (name : String) (description : Option String := none)
    (defaultTarget : Option RepoTarget := none) (reviewer : Option ReviewerTemplate := none) :
    IO Project := do
  let cfg ← Orchestra.Taxis.getConfig
  let ids ← statusLabelIds
  let metaFields : List (String × Json) :=
    (defaultTarget.map fun t => [("target", ToJson.toJson t)] : Option _).getD [] ++
    (reviewer.map fun r => [("reviewer", ToJson.toJson r)] : Option _).getD []
  let desc := encodeMeta (description.getD "") metaFields
  let raw ← unwrap (← Orchestra.Taxis.createIssue cfg { title := name, description := desc, labels := #[ids.project] })
  toProject raw

/-- Update an *existing* project's name/description/target/reviewer. -/
def saveProject (p : Project) : IO Unit := do
  let cfg ← Orchestra.Taxis.getConfig
  let metaFields : List (String × Json) :=
    (p.defaultTarget.map fun t => [("target", ToJson.toJson t)] : Option _).getD [] ++
    (p.reviewer.map fun r => [("reviewer", ToJson.toJson r)] : Option _).getD []
  let desc := encodeMeta (p.description.getD "") metaFields
  let _ ← unwrap (← Orchestra.Taxis.updateIssue cfg p.id { title := some p.name, description := some desc })

def loadProject (pid : Taxis.IssueId) : IO (Option Project) := do
  let cfg ← Orchestra.Taxis.getConfig
  match ← Orchestra.Taxis.getIssue cfg pid with
  | .error _ => return none
  | .ok raw => return some (← toProject raw)

/-- All projects, newest first by id (matching the old newest-first-by-generated-id ordering). -/
def loadAllProjects : IO (Array Project) := do
  let cfg ← Orchestra.Taxis.getConfig
  let ids ← statusLabelIds
  let raws ← unwrap (← Orchestra.Taxis.listIssues cfg (label := some ids.project))
  let projects ← raws.mapM toProject
  return projects.qsort (fun a b => a.id.val > b.id.val)

/-- Permanently delete a project. Note this is *not* a status transition (there's no "deleted"
    `IssueStatus`) — the taxis issue itself is removed, unlike `saveIssue { .. with status := ..}`
    for the ordinary lifecycle states. taxis's `parent_id` foreign key is `ON DELETE SET NULL`, not
    cascading, so this does *not* also delete the project's issues — they become orphaned (no
    project); callers that want a clean sweep should delete issues first. -/
def deleteProject (pid : Taxis.IssueId) : IO Unit := do
  let cfg ← Orchestra.Taxis.getConfig
  unwrap (← Orchestra.Taxis.deleteIssue cfg pid)

/-- Permanently delete an issue (see `deleteProject`'s note on `ON DELETE SET NULL` — this doesn't
    cascade to children either). -/
def deleteIssue (iid : Taxis.IssueId) : IO Unit := do
  let cfg ← Orchestra.Taxis.getConfig
  unwrap (← Orchestra.Taxis.deleteIssue cfg iid)

def createIssue (projectId : Taxis.IssueId) (title description : String)
    (parentId : Option Taxis.IssueId := none) (target : Option RepoTarget := none)
    (dependencies : Array Taxis.IssueId := #[]) : IO Issue := do
  let cfg ← Orchestra.Taxis.getConfig
  let ids ← statusLabelIds
  let metaFields : List (String × Json) :=
    (target.map fun t => [("target", ToJson.toJson t)] : Option _).getD []
  let desc := encodeMeta description metaFields
  let parent := parentId.getD projectId
  let raw ← unwrap (← Orchestra.Taxis.createIssue cfg
    { title, description := desc, parent := some parent, dependencies })
  toIssue ids projectId raw

/-- Update an *existing* issue's title/description/goal/status/target/dependencies.

    Deliberately does **not** write `attachedPRs` — use `attachPR`. An issue's PR set lives in
    taxis artifacts rather than its body, so reconciling it here would make every status patch
    also assert the full PR list; combined with `loadIssues` not populating `attachedPRs`, a
    load-modify-save of a listed issue would then silently detach every PR on it. Attaching is
    the only mutation Orchestra ever performs on the set, so it gets its own entry point.

    Also does not change `parentId` — nothing in Orchestra re-parents an issue after creation. -/
def saveIssue (i : Issue) : IO Unit := do
  let cfg ← Orchestra.Taxis.getConfig
  let ids ← statusLabelIds
  -- Need the issue's current label set to only touch the status labels, not clobber any others.
  let raw ← unwrap (← Orchestra.Taxis.getIssue cfg i.id)
  let metaFields : List (String × Json) :=
    (i.target.map fun t => [("target", ToJson.toJson t)] : Option _).getD []
  let desc := encodeMeta i.description metaFields
  let _ ← unwrap (← Orchestra.Taxis.updateIssue cfg i.id
    { title := some i.title
      description := some desc
      goal := some i.goal
      state := some (stateOf i.status)
      labels := some (labelsFor ids raw.labels i.status)
      dependencies := some i.dependencies })

/-- Remove the `o-claimed` label from `iid`, leaving everything else — including the taxis
    `state`, and therefore whether the issue reads as completed or abandoned — untouched.

    Exists because `IssueStatus` is not a stored field: `.claimed` *is* this label (see
    `statusOf`). Dropping a claim by deleting its `session` artifact alone therefore leaves the
    issue reading as claimed forever, which hides it from every consumer that gates on `.open`
    — the reviewer sweep (`Listener.classifyReview`), the dispatcher (`workableIssues`) and
    `list_issues_in_review`. `Project.forceRelease` calls this; `release` does not need it,
    since it writes a status through `saveIssue`.

    A no-op when the label is absent, so callers can use it unconditionally. -/
def clearClaimedLabel (iid : Taxis.IssueId) : IO Unit := do
  let cfg ← Orchestra.Taxis.getConfig
  let ids ← statusLabelIds
  let raw ← unwrap (← Orchestra.Taxis.getIssue cfg iid)
  if !raw.labels.contains ids.claimed then return
  let _ ← unwrap (← Orchestra.Taxis.updateIssue cfg iid
    { labels := some (raw.labels.filter (· != ids.claimed)) })

/-! ## Labels and assignees, as an agent sets them

Two taxis fields orchestra derives nothing from and so never wrote: the labels on an issue and
the actors assigned to it. Both are how work is routed rather than done — the label-dispatcher
selects the issues it offers by label (`issuesWithLabel`), and an assignee is how a specific
human is put on the hook for an issue no agent should settle — so both are reachable from a
triaging agent (`manage_issues`, see `Orchestra.Project.Tools`).

Both take an add/remove **delta** rather than the set to write. taxis takes the whole set in one
`PATCH`, and an agent handed that wholesale would clear `o-claimed` by omission — and with it the
issue's claim, since the label *is* the claim (`statusOf`) — on every call that only meant to add
a label. The delta is turned into a set here, against the issue as it is at that moment. -/

/-- Labels orchestra maintains itself, which an agent may not add or remove.

    `o-claimed` *is* an issue's claim and `t-project` *is* what makes an issue a project
    (`statusOf`, `anchorsProject`): both are derived state with entry points of their own, and an
    agent setting them by hand would put the tracker and orchestra's reading of it out of step
    silently — an issue labelled claimed that no task holds is invisible to the dispatcher, the
    reviewer sweep and `list_issues_in_review` alike. -/
def reservedLabels : List String := ["o-claimed", "t-project"]

/-- What a relabelling request comes down to, and the label set to write for it — or why it
    cannot be served. `known` is every label the tracker defines, `current` the ids on the issue.

    The name arithmetic is `Utils.Labels.planLabelChange`'s, shared with the GitHub side. What is
    taxis's own is the two ends around it: refusing the labels above, and folding the answer back
    into one array of ids, because taxis writes labels as a whole set rather than one call per
    label. Pure, so the arithmetic that decides what an agent's call does is tested without a
    tracker. -/
def planTaxisLabels (known : Array Orchestra.Taxis.Label)
    (current : Array Orchestra.Taxis.LabelId) (add remove : List String) :
    Except String (Utils.Labels.LabelChange × Array Orchestra.Taxis.LabelId) := do
  let reserved := (add ++ remove).filter fun n => reservedLabels.contains n.toLower
  unless reserved.isEmpty do
    .error s!"{String.intercalate ", " reserved}: orchestra maintains this label itself and it \
      is not yours to set — claiming an issue and marking one a project have tools of their own"
  -- taxis's label names are unique under a case-*sensitive* constraint, so `t-ready` and
  -- `T-Ready` can be two different labels with two different ids — and the dispatcher, which
  -- resolves its configured label by exact name, is watching exactly one of them. Matching
  -- case-insensitively (`planLabelChange`, so that `T-Bug` finds GitHub's `t-bug`) would then
  -- have to pick one, and picking wrong is silent: the request is planned and reported against
  -- one id and applied to a set holding the other, so the call answers "removed t-ready" having
  -- changed nothing. Refuse instead, the way an ambiguous actor name is refused.
  --
  -- Preferring an exact match would serve most such requests correctly and is tempting, since
  -- exact matching is what `issuesWithLabel` and `ensureLabel` already do. It is not taken
  -- because the case it gets wrong is the silent one: an agent picking `T-Ready` off `list_labels`
  -- when the dispatcher watches `t-ready` would be given exactly what it asked for, and the issue
  -- would sit unrouted with nothing to show why. A refusal names both spellings and is actionable.
  let ambiguous := (add ++ remove).filter fun n =>
    (known.filter (·.name.toLower == n.toLower)).size > 1
  unless ambiguous.isEmpty do
    let spellings (n : String) : String :=
      String.intercalate ", " ((known.filter (·.name.toLower == n.toLower)).map (·.name)).toList
    .error s!"{String.intercalate ", " ambiguous}: the tracker defines more than one label \
      differing only in case ({String.intercalate "; " (ambiguous.map spellings)}), so which one \
      is meant cannot be told from the name — and a dispatcher is watching only one of them. \
      This is a fault in the tracker, not in the request: merge the duplicates in taxis"
  let name? (id : Orchestra.Taxis.LabelId) : Option String := (known.find? (·.id == id)).map (·.name)
  let currentNames := (current.filterMap name?).toList
  let change ← Utils.Labels.planLabelChange (known.map (·.name)).toList currentNames add remove
    (owner := "the tracker")
  let idsOf (names : List String) : List Orchestra.Taxis.LabelId :=
    names.filterMap fun n => (known.find? (·.name == n)).map (·.id)
  let removed := idsOf change.remove
  let kept := current.filter fun l => !removed.contains l
  let final := (idsOf change.add).foldl (fun acc l => if acc.contains l then acc else acc.push l) kept
  return (change, final)

/-- Add and remove taxis labels on `iid`, reporting what changed.

    A request that turns out to be a no-op writes nothing: taxis would accept the unchanged set,
    but a `PATCH` that changes nothing still stamps the issue as updated and lands in its event
    log, and a triage agent re-asserting labels it already set every pass is exactly the caller
    this has.

    **The read and the write are two calls, and taxis offers nothing to make them one.** Its
    `PATCH /issues/:id` takes the whole label array and carries no `If-Match`, so a claim taken
    (or dropped) between the `getIssue` here and the `updateIssue` below is overwritten by a set
    that predates it — the issue then reads as unclaimed while a task holds it, and the
    dispatcher offers it to a second worker. `saveIssue` has always had the same window; this
    widens it by adding a second writer that a triage role is meant to call regularly. Closing it
    needs optimistic concurrency in taxis, not a change here. -/
def setIssueLabels (iid : Taxis.IssueId) (add remove : List String) :
    IO Utils.Labels.LabelChange := do
  let cfg ← Orchestra.Taxis.getConfig
  let known ← unwrap (← Orchestra.Taxis.listLabels cfg)
  let raw ← unwrap (← Orchestra.Taxis.getIssue cfg iid)
  match planTaxisLabels known raw.labels add remove with
  | .error why => throw (.userError s!"issue {iid.toString} was not relabelled: {why}")
  | .ok (change, final) =>
    unless change.add.isEmpty && change.remove.isEmpty do
      let _ ← unwrap (← Orchestra.Taxis.updateIssue cfg iid { labels := some final })
    return change

/-- Resolve one actor by email or display name, case-insensitively.

    Two keys because neither alone is the name a person is known by: the display name is what a
    reviewer is called on an issue and what an agent will have read there, the email is what is
    unique. An ambiguous display name is refused rather than guessed at — assigning the wrong
    person is worse than saying which two were meant. -/
def findActor? (known : Array Orchestra.Taxis.Actor) (name : String) :
    Except String Orchestra.Taxis.Actor :=
  let key := name.toLower
  let byEmail := known.filter (·.email.toLower == key)
  let found := if byEmail.isEmpty then known.filter (·.displayName.toLower == key) else byEmail
  match found.toList with
  | [a] => .ok a
  | [] =>
    let vocabulary :=
      if known.isEmpty then "the tracker has no actors"
      else s!"the tracker knows: {String.intercalate ", " (known.map (·.email)).toList}"
    .error s!"no such actor: {name} — {vocabulary}"
  | several =>
    -- Reached by a shared display name in practice; the email branch can only collide if the
    -- tracker holds two actors with one address, so the wording covers both rather than
    -- asserting which it was.
    .error s!"{name} names several actors \
      ({String.intercalate ", " (several.map (·.email))}); say which by email"

/-- What an assignment request comes down to, and the assignee set to write for it.

    Reported as a `Utils.Labels.LabelChange` — its four fields are lists of names, and here they
    hold actors rather than labels, so the same one-line `summary` says what happened to either.
    Assignees are answered as emails, the key that identifies an actor uniquely. Pure. -/
def planAssignees (known : Array Orchestra.Taxis.Actor)
    (current : Array Orchestra.Taxis.ActorId) (add remove : List String) :
    Except String (Utils.Labels.LabelChange × Array Orchestra.Taxis.ActorId) := do
  let resolve (names : List String) : Except String (List Orchestra.Taxis.Actor) :=
    names.mapM (findActor? known)
  let toAdd ← resolve add
  let toRemove ← resolve remove
  let contradictory := toAdd.filter fun a => toRemove.any (·.id == a.id)
  unless contradictory.isEmpty do
    .error s!"asked to both assign and unassign \
      {String.intercalate ", " (contradictory.map (·.email))}"
  let held (a : Orchestra.Taxis.Actor) : Bool := current.contains a.id
  -- By id, not by string: one actor named twice in a request — once by email and once by display
  -- name — is one assignment, and `final` below already treats it as one. Without this the two
  -- disagree and the summary reads "assigned chris@…, chris@…".
  let once (actors : List Orchestra.Taxis.Actor) : List String :=
    (actors.foldl (fun acc a => if acc.any (·.id == a.id) then acc else acc ++ [a]) []).map (·.email)
  let change : Utils.Labels.LabelChange :=
    { add            := once (toAdd.filter (!held ·))
      remove         := once (toRemove.filter held)
      alreadyPresent := once (toAdd.filter held)
      notPresent     := once (toRemove.filter (!held ·)) }
  let removedIds := (toRemove.map (·.id))
  let kept := current.filter fun a => !removedIds.contains a
  let final := (toAdd.map (·.id)).foldl
    (fun acc a => if acc.contains a then acc else acc.push a) kept
  return (change, final)

/-- Assign and unassign taxis actors on `iid`, reporting what changed. Writes nothing when the
    request is a no-op, for the reason `setIssueLabels` does not. -/
def setIssueAssignees (iid : Taxis.IssueId) (add remove : List String) :
    IO Utils.Labels.LabelChange := do
  let cfg ← Orchestra.Taxis.getConfig
  let known ← unwrap (← Orchestra.Taxis.listActors cfg)
  let detail ← unwrap (← Orchestra.Taxis.getIssueDetail cfg iid)
  match planAssignees known (detail.assignedActors.map (·.id)) add remove with
  | .error why => throw (.userError s!"issue {iid.toString} was not reassigned: {why}")
  | .ok (change, final) =>
    unless change.add.isEmpty && change.remove.isEmpty do
      let _ ← unwrap (← Orchestra.Taxis.updateIssue cfg iid { assignees := some final })
    return change

/-- An issue's labels and assignees, by name — the two fields `Issue` does not carry, fetched for
    `get_issue` so an agent can read the routing state it is allowed to change.

    `none` when the fetch failed, which the caller has to render as its own thing: an empty pair
    would read as "this issue carries no labels", and an agent that believes that about a routing
    label goes on to add one the issue already has, or reports the work unrouted. The rest of
    `get_issue` is still worth answering, so this does not throw. -/
def issueLabelsAndAssignees (iid : Taxis.IssueId) : IO (Option (Array String × Array String)) := do
  let cfg ← Orchestra.Taxis.getConfig
  match ← Orchestra.Taxis.getIssueDetail cfg iid with
  | .error _ => return none
  | .ok detail =>
    return some (detail.issueLabels.map (·.name), detail.assignedActors.map (·.displayName))

/-- Every label the tracker defines, by name. What an agent is shown when it needs to know which
    labels it may route with — the same vocabulary `planTaxisLabels` refuses against. -/
def listLabelNames : IO (Array String) := do
  let cfg ← Orchestra.Taxis.getConfig
  let labels ← unwrap (← Orchestra.Taxis.listLabels cfg)
  return labels.map (·.name)

/-- Every actor the tracker knows: (display name, email, is-a-bot). What an agent is shown when
    it needs to put a named human on an issue. -/
def listActorSummaries : IO (Array (String × String × Bool)) := do
  let cfg ← Orchestra.Taxis.getConfig
  let actors ← unwrap (← Orchestra.Taxis.listActors cfg)
  return actors.map fun a => (a.displayName, a.email, a.bot)

/-- Attach `pr` to `iid` as a `github-pr` artifact — the counterpart to `Issue.attachedPRs` on
    the read side (see `saveIssue` for why this isn't folded into it).

    The `github-pr` handler's `validate` requires a `url` field (see `Taxis.Plugins.Github`),
    which `PRRef.toJson` alone doesn't produce — `prRefArtifactPayload` adds it on write, and
    `FromJson PRRef` still round-trips the result on read since `repo`/`number`/`branch`/
    `task_id` are included too. -/
def attachPR (iid : Taxis.IssueId) (pr : PRRef) : IO Unit := do
  let cfg ← Orchestra.Taxis.getConfig
  let _ ← unwrap (← Orchestra.Taxis.createArtifact cfg iid "github-pr" (prRefArtifactPayload pr))

def loadIssue (pid : Taxis.IssueId) (iid : Taxis.IssueId) : IO (Option Issue) := do
  let cfg ← Orchestra.Taxis.getConfig
  match ← Orchestra.Taxis.getIssueDetail cfg iid with
  | .error _ => return none
  | .ok detail =>
    let ids ← statusLabelIds
    return some (← toIssue ids pid detail.issue detail.attachedArtifacts)

/-- All issues belonging to `pid` — every taxis issue whose ancestor chain (via native `parent`)
    reaches `pid`. taxis has no server-side "descendants of X" filter (see the "Migrate Project/
    Issue data layer" tracking issue), so this fetches every issue in the tracker once and filters
    client-side. For speed, `attachedPRs` is left empty here — populating it would cost one
    `GET /issues/:id` per issue, and none of this function's callers (status/dependency
    filtering, the dispatcher) read it; `loadIssue`/`findIssue` fetch it. Feeding a result of
    this function back into `saveIssue` is safe regardless: `saveIssue` doesn't write the PR
    set (see its docs). -/
def loadIssues (pid : Taxis.IssueId) : IO (Array Issue) := do
  let cfg ← Orchestra.Taxis.getConfig
  let ids ← statusLabelIds
  let all ← unwrap (← Orchestra.Taxis.listIssues cfg)
  let byId : Std.HashMap Int64 Orchestra.Taxis.Issue := Std.HashMap.ofList (all.toList.map fun i => (i.id.val, i))
  let rec belongsTo (i : Orchestra.Taxis.Issue) : Nat → Bool
    | 0 => false
    | fuel + 1 =>
      match i.parent with
      | none => false
      | some p =>
        if p.val == pid.val then true
        else match byId.get? p.val with
          | some parentIssue => belongsTo parentIssue fuel
          | none => false
  let mine := all.filter (belongsTo · all.size)
  mine.mapM (toIssue ids pid ·)

/-- Find an issue by ID across all projects, without knowing which one in advance. Returns the
    owning project too, so callers can resolve `effectiveTarget`. Used by the CLI where users name
    an issue without giving its project, and by every agent-side tool that takes an issue id.

    Walks the ancestor chain (the issue itself counts) to the nearest one that `anchorsProject`.
    *Nearest*, so a sub-issue pinning its own `repository` + `github-branch` becomes the project
    for everything beneath it rather than the `t-project` further up — matching `projectRootOf`,
    which has always scoped writes that way. -/
def findIssue (iid : Taxis.IssueId) : IO (Option (Project × Issue)) := do
  let cfg ← Orchestra.Taxis.getConfig
  let ids ← statusLabelIds
  -- Fuel-bounded like `loadIssues`' `belongsTo`: orchestra hierarchies are shallow (project →
  -- issue → split sub-issue), so the bound only exists so that a `parent` cycle on the taxis
  -- side fails fast instead of looping forever over HTTP.
  let rec findProjectOf (id : Taxis.IssueId) : Nat → IO (Option Project)
    | 0 => do
      IO.eprintln s!"[warn] findIssue: ancestor chain of {iid.toString} exceeds \
        {maxAncestorDepth} levels; giving up (parent cycle?)"
      return none
    | fuel + 1 => do
      -- Detail rather than the bare issue: the anchor test reads artifacts. Same number of round
      -- trips, a little more per hop.
      match ← Orchestra.Taxis.getIssueDetail cfg id with
      | .error _ => return none
      | .ok detail =>
        if anchorsProject ids detail then
          return some (← toProject detail.issue)
        else
          match detail.issue.parent with
          | none => return none
          | some p => findProjectOf p fuel
  match ← Orchestra.Taxis.getIssueDetail cfg iid with
  | .error _ => return none
  | .ok detail =>
    match ← findProjectOf iid maxAncestorDepth with
    | none => return none
    | some project => return some (project, ← toIssue ids project.id detail.issue detail.attachedArtifacts)

/-! ## Artifact-derived targets

The project-independent dispatcher (`Listener.SourceConfig.labelDispatcher`) works on issues
picked out by a label, with no orchestra `Project` behind them and so no `defaultTarget` to
inherit. Their target is read off taxis artifacts instead, walking up the parent chain:

* the repository comes from a `repository` artifact (`{ url, name? }`, see
  `Taxis.Plugins.Standard`), whose URL is parsed for the `owner/repo` pair;
* the branch comes from a `github-branch` artifact (`{ owner, repo, branch }`, see
  `Taxis.Plugins.Github`).

Each is taken from the *nearest* ancestor carrying it, starting at the issue itself, so a
sub-issue can pin its own branch while inheriting the repository from the project above it. Both
are required — a labelled issue missing either is skipped rather than guessed at, since
dispatching an agent at the wrong repository or branch is worse than not dispatching. -/

/-- The `owner/repo` in a repository artifact's `url`, e.g.
    `https://github.com/leanprover/lean4` (with or without a trailing `.git` or slash). -/
def repositoryOfArtifactUrl (url : String) : Option Repository :=
  let trimmed := (url.splitOn "://").getLast!
  let parts := (trimmed.splitOn "/").filter (!·.isEmpty)
  match parts with
  | _host :: owner :: repo :: _ =>
    let repo := if repo.endsWith ".git" then (repo.dropEnd 4).toString else repo
    if owner.isEmpty || repo.isEmpty then none else some { owner, name := repo }
  | _ => none

/-- Why `artifactTarget` could not produce a target — carried so the caller can say which half
    is missing rather than emitting one vague "no target" line. -/
inductive TargetGap where
  | noRepository
  | noBranch (repo : Repository)
deriving Repr, BEq

/-- An artifact-derived target, plus the ancestor the `repository` artifact was found on. That
    ancestor stands in for the "project" a label-dispatched issue belongs to: it is what fills
    `QueueEntry.projectId` and supplies `{{project_name}}` when rendering the role prompt. -/
structure ArtifactTarget where
  target      : RepoTarget
  /-- Issue carrying the `repository` artifact — may be the dispatched issue itself. -/
  repoOwner   : Taxis.IssueId
deriving Repr

/-- Resolve an issue's `RepoTarget` from `repository` + `github-branch` artifacts on it or any
    ancestor (nearest wins). See this section's docs. -/
def artifactTarget (iid : Taxis.IssueId) : IO (Except TargetGap ArtifactTarget) := do
  let cfg ← Orchestra.Taxis.getConfig
  let rec walk (id : Taxis.IssueId) (repo? : Option (Repository × Taxis.IssueId))
      (branch? : Option String) :
      Nat → IO (Option (Repository × Taxis.IssueId) × Option String)
    | 0 => pure (repo?, branch?)
    | fuel + 1 => do
      match ← Orchestra.Taxis.getIssueDetail cfg id with
      | .error _ => pure (repo?, branch?)
      | .ok detail =>
        let arts := detail.attachedArtifacts
        let repo? := repo? <|> (arts.findSome? fun a =>
          if a.kind == "repository" then
            ((a.payload.getObjValAs? String "url").toOption.bind repositoryOfArtifactUrl).map (·, id)
          else none)
        let branch? := branch? <|> (arts.findSome? fun a =>
          if a.kind == "github-branch" then (a.payload.getObjValAs? String "branch").toOption
          else none)
        if repo?.isSome && branch?.isSome then pure (repo?, branch?)
        else match detail.issue.parent with
          | none => pure (repo?, branch?)
          | some p => walk p repo? branch? fuel
  match ← walk iid none none maxAncestorDepth with
  | (none, _) => return .error .noRepository
  | (some (repo, _), none) => return .error (.noBranch repo)
  | (some (repo, owner), some branch) =>
    return .ok { target := { repo, branch }, repoOwner := owner }

/-- The dispatch candidates among `all` for trigger label `label`, by the two rules the
    project-independent dispatcher selects on:

    * **Open only.** Completed and abandoned issues are never candidates.
    * **Inherited label.** An issue is in scope if it or any ancestor carries `label`, so
      labelling a project once opts its whole subtree in instead of needing the label repeated on
      every issue.
    * **No open children.** An issue with children still open has been decomposed and those
      children are the work; dispatching it as well would put an agent on the whole while others
      work the parts. Only *open* children hold a parent back — once they are all completed or
      abandoned the parent becomes workable again on its own. A child
      being worked on is still open (claimed and in-review both map to taxis `open`), so work in
      flight does hold the parent back. This structural test replaces the `o-blocked` label the
      file-based system used: nothing has to remember to set or clear it, and it cannot disagree
      with the actual tree.

    Pure, and the entire selection policy — everything else in `issuesWithLabel` is fetching. -/
def inScopeOpenIssues (all : Array Orchestra.Taxis.Issue) (label : Orchestra.Taxis.LabelId) :
    Array Orchestra.Taxis.Issue :=
  let byId : Std.HashMap Int64 Orchestra.Taxis.Issue :=
    Std.HashMap.ofList (all.toList.map fun i => (i.id.val, i))
  let rec inScope (i : Orchestra.Taxis.Issue) : Nat → Bool
    | 0 => false
    | fuel + 1 =>
      if i.labels.contains label then true
      else match i.parent with
        | none => false
        | some p => match byId.get? p.val with
          | some parent => inScope parent fuel
          | none => false
  -- Only open issues. Without this a completed issue stays "in scope" forever — it still
  -- inherits the label — so every tick would re-resolve its target and, if it has no artifacts,
  -- warn about it again.
  all.filter fun i => i.state == .open && inScope i all.size

/-! ### When an issue can be worked on

One rule, applied wherever a worker might be dispatched: an issue is workable when it is open, has
**no open children**, and has **no open dependencies**. Anything else is waiting on something.

Both conditions are about *open*-ness specifically. A completed or abandoned child does not make
its parent a container any more, and a completed or abandoned dependency does not hold its
dependent back — abandoning is the decision that the work will not happen, so treating it as
forever-unsatisfied would strand everything downstream.

Both are decided where the full picture is available, never from a set that has already been
narrowed to open issues: derived from such a set, "done" and "absent" are indistinguishable and
everything with a dependency blocks forever. The two functions below are the same rule over the
two shapes the callers hold. -/

/-- The workable subset of `all`, over raw taxis issues — used by the label-dispatcher, which
    lists the whole tracker and so knows the state of every child and dependency.

    `excludeRoots` drops the issues carrying `label` *directly* — the roots — from the answer,
    leaving only what inherited it. For a tree where the label marks the epic rather than the
    work, that is the difference between "this subtree is in scope" and "this issue is a unit of
    work": the root is a leaf on the day it is created, and without this it is dispatched as work
    before it is ever decomposed. It is opt-in because the opposite tree is just as common, and
    there labelling one issue and having it worked is the whole point. -/
def dispatchCandidates (all : Array Orchestra.Taxis.Issue) (label : Orchestra.Taxis.LabelId)
    (excludeRoots : Bool := false) : Array Orchestra.Taxis.Issue :=
  let openIds : Std.HashMap Int64 Unit :=
    Std.HashMap.ofList (all.toList.filterMap fun i =>
      match i.state with | .open => some (i.id.val, ()) | _ => none)
  let hasOpenChildren : Std.HashMap Int64 Unit :=
    Std.HashMap.ofList (all.toList.filterMap fun i =>
      match i.state with
      | .open => i.parent.map (·.val, ())
      | _ => none)
  (inScopeOpenIssues all label).filter fun i =>
    !(excludeRoots && i.labels.contains label) &&
    !hasOpenChildren.contains i.id.val &&
    i.dependencies.all (fun d => !openIds.contains d.val)

/-- The workable subset of `all`, over orchestra issues — used by the project-dispatcher, which
    holds every issue in its project.

    A dependency pointing outside the project is not visible here and so does not block. Blocking
    on an id whose state cannot be seen would strand the dependent permanently, which is the worse
    failure. -/
def workableIssues (all : Array Issue) : Array Issue :=
  let stillOpen (i : Issue) := i.status != .completed && i.status != .abandoned
  let openIds : Std.HashMap Int64 Unit :=
    Std.HashMap.ofList (all.toList.filterMap fun i =>
      if stillOpen i then some (i.id.val, ()) else none)
  let hasOpenChildren : Std.HashMap Int64 Unit :=
    Std.HashMap.ofList (all.toList.filterMap fun i =>
      if stillOpen i then i.parentId.map (·.val, ()) else none)
  all.filter fun i =>
    i.status == .open &&
    !hasOpenChildren.contains i.id.val &&
    i.dependencies.all (fun d => !openIds.contains d.val)

/-- How many open in-scope issues each labelled root owns, keyed by the root's id.

    An issue is counted against its **nearest open labelled ancestor-or-self**, so nested roots
    partition the in-scope set instead of each counting the whole subtree below it. Nearest-wins
    is the same rule the project anchor uses, and open-ness is what keeps the tally aligned with
    `LabelDispatchSets.roots`: a labelled but completed ancestor is no longer a root, so its
    descendants belong to whichever root is still standing above them.

    The counts therefore need not add up to `inScopeOpenIssues`. Scope is inherited from a
    labelled ancestor in any state, so an issue whose only labelled ancestor has completed is
    still in scope and still dispatched onto, but belongs to no root's tally — which is right,
    since no root exists there for an unbound role to be placed on either.

    A root counts itself unless `excludeRoots` says the label marks the epic rather than the work
    (the same option `dispatchCandidates` takes, and it has to be the same on both or the bound
    would count issues no agent can be given). Without it no root's answer is zero, so a root with
    nothing under it is one issue — the case where exactly one agent should be planning it. With
    it, an empty root counts zero, and the floor in `Listener.capToAvailable` is what still leaves
    that one agent there to plan.

    Nested roots are excluded from each other's counts too, not just from their own: an issue
    carrying the label is a root wherever it sits, and if it is not work for its own agents it is
    not work for its parent's either.

    Pure, so the arithmetic that bounds unclaimed dispatch is testable without a tracker. -/
def openIssuesByRoot (all : Array Orchestra.Taxis.Issue) (label : Orchestra.Taxis.LabelId)
    (excludeRoots : Bool := false) : Std.HashMap Int64 Nat := Id.run do
  let byId : Std.HashMap Int64 Orchestra.Taxis.Issue :=
    Std.HashMap.ofList (all.toList.map fun i => (i.id.val, i))
  let rec nearestRoot (i : Orchestra.Taxis.Issue) : Nat → Option Int64
    | 0 => none
    | fuel + 1 =>
      if i.state == .open && i.labels.contains label then some i.id.val
      else match i.parent with
        | none => none
        | some p => match byId.get? p.val with
          | some parent => nearestRoot parent fuel
          | none => none
  let mut counts : Std.HashMap Int64 Nat := {}
  for i in inScopeOpenIssues all label do
    if excludeRoots && i.labels.contains label then continue
    if let some root := nearestRoot i all.size then
      counts := counts.insert root ((counts.getD root 0) + 1)
  return counts

/-- Every issue carrying `labelName`, regardless of project — the issue set the
    project-independent dispatcher works from. `none` means no such label exists on the tracker
    (distinct from "the label exists and nothing carries it", which is `some #[]`); the label is
    looked up rather than created, since a dispatcher configured against a typo'd label should
    report that, not silently conjure it.

    `projectId` on each returned issue is the ancestor carrying the `repository` artifact, so
    issues from different projects can appear side by side; issues whose target can't be resolved
    are omitted along with the reason, for the caller to report — every exclusion is reported,
    since an issue that silently never dispatches gives you nothing to debug.

    `excludeRoots` takes the labelled issues themselves out of `work` and out of the per-root
    counts, for trees where the label marks the epic rather than a unit of work. It does not touch
    `roots` — a root that is not work is still the scope an unbound role is placed in — nor
    `reviewable`: a pull request that exists needs reviewing whoever opened it, and a root
    carrying one is the case `reviewable` was widened to containers for in the first place. -/
structure LabelDispatchSets where
  /-- Leaves: what a worker role may be dispatched onto. -/
  work       : Array (Issue × RepoTarget)
  /-- Open in-scope issues carrying at least one attached pull request, *including* containers —
      an issue with children can still have a PR of its own that needs reviewing. Whether those
      PRs are actually still unmerged is the caller's to check, since it costs a GitHub call. -/
  reviewable : Array (Issue × RepoTarget)
  /-- Issues in scope whose target could not be resolved, with the reason. -/
  gaps       : Array (Taxis.IssueId × TargetGap)
  /-- Open issues carrying the label *directly*, rather than inheriting it — each one roots a
      subtree. An unbound role (`always` trigger) is scoped to one of these: it has no issue of
      its own to take a target or a project from, so the root supplies both, and the claims it
      makes are swept against the root's descendants when its task ends.

      Deliberately the labelled issue, not `Issue.projectId` — that is the ancestor carrying the
      `repository` artifact (see `ArtifactTarget.repoOwner`), which is what a *bound* role gets
      and may sit well above the label. A maintainer scoped there would be planning across
      subtrees nobody labelled. -/
  roots      : Array (Issue × RepoTarget)
  /-- Open in-scope issues owned by each root (`openIssuesByRoot`), keyed by the root's issue id.
      How much work a root actually has is what bounds the unbound roles dispatched onto it when
      the listener asks for that; it is computed here because it needs the whole tracker, which
      only this function holds. -/
  openByRoot : Std.HashMap Int64 Nat

/-- Open in-scope issues under `root`, counting the root itself unless the sets were built with
    `excludeRoots`. Zero for an id no issue is counted against — a root whose subtree is empty
    under that option, or an id that is not a root of this set at all. Left as a plain count with
    no floor of its own: `Listener.capToAvailable` is the one place that decides what a cap of
    zero work should mean, and two of those would disagree eventually. -/
def LabelDispatchSets.openUnder (s : LabelDispatchSets) (root : Taxis.IssueId) : Nat :=
  s.openByRoot.getD root.val 0

def issuesWithLabel (labelName : String) (excludeRoots : Bool := false) :
    IO (Option LabelDispatchSets) := do
  let cfg ← Orchestra.Taxis.getConfig
  let labels ← unwrap (← Orchestra.Taxis.listLabels cfg)
  let some label := labels.find? (·.name == labelName) | return none
  -- The whole tracker, not just the labelled issues: the label is inherited downward, so
  -- deciding whether an issue is in scope needs its ancestors, and deciding whether it is a leaf
  -- needs everyone else's parent pointers. One list call plus in-memory work.
  let all ← unwrap (← Orchestra.Taxis.listIssues cfg)
  let workIds := (dispatchCandidates all label.id excludeRoots).map (·.id.val)
  let mut work : Array (Issue × RepoTarget) := #[]
  let mut reviewable : Array (Issue × RepoTarget) := #[]
  let mut gaps : Array (Taxis.IssueId × TargetGap) := #[]
  let mut roots : Array (Issue × RepoTarget) := #[]
  -- Walk the wider set once: workers are the leaf subset of it, reviewables are those with a
  -- PR attached, and roots are those carrying the label themselves, so all three come out of the
  -- same fetches. `inScopeOpenIssues` is already open-only, which is the whole of the root test
  -- besides carrying the label: a completed or abandoned root has nothing left to maintain.
  for raw in inScopeOpenIssues all label.id do
    match ← artifactTarget raw.id with
    | .error gap => gaps := gaps.push (raw.id, gap)
    | .ok at' =>
      -- Re-fetched (rather than reusing `raw`) to pick up attached PRs and the artifact-derived
      -- project id. One extra GET per candidate; taxis has no batch issue-detail endpoint, the
      -- same limitation `loadIssues` already documents.
      if let some issue ← loadIssue at'.repoOwner raw.id then
        if workIds.contains raw.id.val then work := work.push (issue, at'.target)
        if !issue.attachedPRs.isEmpty then reviewable := reviewable.push (issue, at'.target)
        if raw.labels.contains label.id then roots := roots.push (issue, at'.target)
  return some
    { work, reviewable, gaps, roots
    , openByRoot := openIssuesByRoot all label.id excludeRoots }

/-! ## Comments

Taxis issues carry a comment thread. It is where a reviewer's verdict and reasoning live, and
where the next agent to pick the issue up reads why it came back — so both writing and reading
are exposed to agents, not just to humans in the taxis UI. -/

/-- Post a comment on an issue. `review` marks it as a review carrying that verdict, which is
    what makes it render as one in the taxis UI and count for review-request tracking; plain
    comments leave it `none`. -/
def addComment (iid : Taxis.IssueId) (body : String)
    (review : Option Orchestra.Taxis.ReviewState := none) : IO Unit := do
  let cfg ← Orchestra.Taxis.getConfig
  let _ ← unwrap (← Orchestra.Taxis.createComment cfg iid body (review := review))

/-- An issue's comments, oldest first as taxis returns them. -/
def loadComments (iid : Taxis.IssueId) : IO (Array Orchestra.Taxis.Comment) := do
  let cfg ← Orchestra.Taxis.getConfig
  unwrap (← Orchestra.Taxis.listComments cfg iid)

/-- A block of text indented under the line that introduces it, as both an issue's comments and
    its context notes are rendered for an agent. -/
private def indentBlock (s : String) : String :=
  String.intercalate "\n" ((s.splitOn "\n").map ("    " ++ ·))

/-- One comment as a line of agent-facing text: author, time, then the body indented. -/
def renderComment (c : Orchestra.Taxis.Comment) : IO String := do
  let when ← Orchestra.Taxis.epochToIso8601 c.createdAt
  let who := c.authorName.getD "(unknown)"
  let verdict := match c.review with
    | some v => s!" [review: {repr v}]"
    | none => ""
  return s!"  {who} at {when}{verdict}\n{indentBlock c.body}"

/-- An issue's whole comment thread rendered for a prompt, or `none` when there is nothing to
    show. Lets a role template carry the discussion — a rejection's reasoning above all — without
    the agent having to know to call `list_issue_comments` first. -/
def renderCommentThread (iid : Taxis.IssueId) : IO (Option String) := do
  let comments ← loadComments iid
  if comments.isEmpty then return none
  let rendered ← comments.mapM renderComment
  return some (String.intercalate "\n" rendered.toList)

/-! ## Context notes

A `context` artifact holds prose *beside* an issue rather than in it: what an earlier run worked
out, the approach already tried and abandoned, what the build environment needs. Taxis folds it
away in the issue's artifact rail, so it can accumulate over the life of an issue without any of
it competing with the description for a human reader's attention.

That is what makes it the right home for the findings an agent would otherwise append to the
description or leave in a comment. The description says what the issue *is* and a reader has to
read it; the comment thread is a conversation and a note dropped into it sinks. Context is
neither: it is written for whoever picks the issue up next, which is usually another agent. -/

/-- One `context` artifact: the artifact's own id, and the two fields of its payload. -/
structure ContextNote where
  /-- Taxis artifact id. Needed to revise the note in place — `Basic.reviseContext`. -/
  id : Orchestra.Taxis.ArtifactId
  title : String
  text : String
deriving Repr, Inhabited

/-- The `context` artifacts among `artifacts`, in the order taxis returned them.

    An artifact whose payload does not carry both strings is skipped rather than reported: taxis
    validates the payload on the way in, so a malformed one means a kind that has moved on, and
    dropping it is better than failing every read of the issue it hangs off. -/
def contextNotesOf (artifacts : Array Orchestra.Taxis.ArtifactView) : Array ContextNote :=
  artifacts.filterMap fun a =>
    if a.kind != "context" then none
    else match a.payload.getObjValAs? String "title", a.payload.getObjValAs? String "text" with
      | .ok title, .ok text => some { id := a.id, title, text }
      | _, _ => none

/-- The context notes attached to an issue. -/
def loadContext (iid : Taxis.IssueId) : IO (Array ContextNote) := do
  let cfg ← Orchestra.Taxis.getConfig
  let detail ← unwrap (← Orchestra.Taxis.getIssueDetail cfg iid)
  return contextNotesOf detail.attachedArtifacts

private def contextPayload (title text : String) : Json :=
  Json.mkObj [("title", Json.str title), ("text", Json.str text)]

/-- Attach a new context note to an issue. -/
def attachContext (iid : Taxis.IssueId) (title text : String) : IO ContextNote := do
  let cfg ← Orchestra.Taxis.getConfig
  let a ← unwrap (← Orchestra.Taxis.createArtifact cfg iid "context" (contextPayload title text))
  return { id := a.id, title, text }

/-- Rewrite a context note in place, keeping its id.

    In place rather than delete-then-create: the note is meant to be revised as it accumulates,
    and recreating it would renumber it, move it to the end of the rail, and record one edit as a
    removal plus an unrelated addition in the issue's activity log. -/
def reviseContext (id : Orchestra.Taxis.ArtifactId) (title text : String) : IO Unit := do
  let cfg ← Orchestra.Taxis.getConfig
  let _ ← unwrap (← Orchestra.Taxis.updateArtifact cfg id (contextPayload title text)
    (kind := "context"))

/-- An issue's context notes rendered for a prompt, or `none` when it has none. Lets a role
    template carry them the way `{{issue_comments}}` carries the thread — a worker dispatched onto
    an issue should not have to know to ask for what the last worker on it left behind. -/
def renderContextNotes (iid : Taxis.IssueId) : IO (Option String) := do
  let notes ← loadContext iid
  if notes.isEmpty then return none
  let rendered := notes.map fun n => s!"  [{n.id.val}] {n.title}\n{indentBlock n.text}"
  return some (String.intercalate "\n" rendered.toList)

/-! ## Write scoping

An agent working an issue may only create and update issues within its own project subtree. The
subtree is identified by its root: the nearest ancestor — the issue itself counts — that anchors a
project, meaning it carries `t-project` or has both a `repository` and a `github-branch` artifact.
Those are the same two markers the dispatcher uses to decide where an issue's target comes from,
so "the project this work belongs to" means the same thing on both sides.

Reads are deliberately not scoped: an agent can still list and inspect issues anywhere, which is
useful for context and cannot damage anything. -/

/-- The nearest ancestor of `iid` (including `iid`) anchoring a project — see this section's docs.
    Falls back to `iid` itself when nothing up the chain qualifies, so an agent can always at
    least create children under its own issue rather than being locked out entirely. -/
def projectRootOf (iid : Taxis.IssueId) : IO Taxis.IssueId := do
  let cfg ← Orchestra.Taxis.getConfig
  let ids ← statusLabelIds
  let rec walk (id : Taxis.IssueId) : Nat → IO (Option Taxis.IssueId)
    | 0 => pure none
    | fuel + 1 => do
      match ← Orchestra.Taxis.getIssueDetail cfg id with
      | .error _ => pure none
      | .ok detail =>
        if anchorsProject ids detail then pure (some id)
        else match detail.issue.parent with
          | none => pure none
          | some p => walk p fuel
  return (← walk iid maxAncestorDepth).getD iid

/-- Whether `candidate` is `root` itself or lies beneath it. -/
def isWithinSubtree (root candidate : Taxis.IssueId) : IO Bool := do
  let cfg ← Orchestra.Taxis.getConfig
  let rec walk (id : Taxis.IssueId) : Nat → IO Bool
    | 0 => pure false
    | fuel + 1 => do
      if id.val == root.val then return true
      match ← Orchestra.Taxis.getIssueDetail cfg id with
      | .error _ => pure false
      | .ok detail =>
        match detail.issue.parent with
        | none => pure false
        | some p => walk p fuel
  walk candidate maxAncestorDepth

/-! ## Hierarchy traversal (B1: parentId-only) -/

/-- Direct children of `parent` within `pid`. -/
def childrenOf (pid : Taxis.IssueId) (parent : Taxis.IssueId) : IO (Array Issue) := do
  let all ← loadIssues pid
  return all.filter (fun i => i.parentId == some parent)

/-- Top-level (parentless) issues of `pid`. -/
def rootIssues (pid : Taxis.IssueId) : IO (Array Issue) := do
  let all ← loadIssues pid
  return all.filter (fun i => i.parentId.isNone)

/-! ## Target resolution

Returns the effective target for an issue: its own override, otherwise the
project's default. `none` means the issue can only be enqueued with an
explicit upstream/fork at queue time. -/

def effectiveTarget (project : Project) (issue : Issue) : Option RepoTarget :=
  issue.target <|> project.defaultTarget

/-! ## Goal resolution -/

/-- The goal a task launched for `issue?` is held to: the issue's own `goal` field, and nothing
    else. `none` when no issue is bound or its goal is unset, which is the ordinary case — a task
    without a goal simply runs to the agent's own idea of done.

    A one-liner, but the single place the mapping lives, so the two spawn paths (`orchestra spawn`
    and the dispatcher) cannot drift into disagreeing about what a task's goal is. In particular
    neither may reach for the rendered prompt instead: that is the role template with the issue
    body and its whole comment thread substituted in, and it is the wrong length and the wrong
    shape to be judged "met" or "not met". -/
def goalFor (issue? : Option Issue) : Option String :=
  match issue? with
  | some i => if i.goal.isEmpty then none else some i.goal
  | none   => none

/-! ## Per-project filesystem directory

Project/issue data itself is taxis-backed now, but roles (`Project.Role`) are explicitly out of
scope for this migration and stay file-based — `<data>/projects/<pid>/roles/`. This is the one
filesystem path callers still need for that. -/

/-- Optional override for the projects root directory. Tests set this to a temp directory to
    avoid touching the user's real data directory. When `none`, paths are derived from the XDG
    data base. -/
initialize projectsDirOverride : IO.Ref (Option System.FilePath) ← IO.mkRef none

def setProjectsDirOverride (p : Option System.FilePath) : IO Unit :=
  projectsDirOverride.set p

def projectsDir : IO System.FilePath := do
  match ← projectsDirOverride.get with
  | some p => return p
  | none   => return (← Dirs.dataBase) / "projects"

def projectDir (pid : Taxis.IssueId) : IO System.FilePath := do
  return (← projectsDir) / pid.toString

/-- Load the default `AppConfig` and wire its `taxis` section (if any) into
    `Orchestra.Taxis`'s active config, so every `Project.*`/`Claim.*` function can reach it
    without every CLI handler threading a `Config` value through — mirrors how `Config.loadAppConfig`
    itself is already called independently by each of `Main.lean`'s other command handlers. Called
    once at CLI startup (`Main.main`). Silently does nothing if config.json can't be loaded —
    commands that don't touch the taxis-backed project subsystem shouldn't fail just because
    config.json is missing/malformed; a genuinely unconfigured taxis only surfaces as an error once
    something actually calls `Orchestra.Taxis.getConfig`. -/
def ensureTaxisConfigured : IO Unit := do
  try
    let cfg ← loadAppConfig
    Orchestra.Taxis.setConfig cfg.taxis
  catch _ => pure ()

end Orchestra.Project
