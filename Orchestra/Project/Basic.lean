import Lean.Data.Json
import Orchestra.Config
import Orchestra.TaskStore
import Orchestra.Taxis

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
  | inReview
  | completed
  | abandoned
  /-- PR failed the merger's validation check; not picked up again automatically. -/
  | rejected
deriving Repr, Inhabited, BEq, DecidableEq

instance : ToJson IssueStatus where
  toJson
    | .open      => "open"
    | .claimed   => "claimed"
    | .inReview  => "in_review"
    | .completed => "completed"
    | .abandoned => "abandoned"
    | .rejected  => "rejected"

instance : FromJson IssueStatus where
  fromJson?
    | .str "open"      => .ok .open
    | .str "claimed"   => .ok .claimed
    | .str "in_review" => .ok .inReview
    | .str "completed" => .ok .completed
    | .str "abandoned" => .ok .abandoned
    | .str "rejected"  => .ok .rejected
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
closed / completed) plus at most one dedicated status label (`o-claimed`, `o-in-review`,
`o-rejected` — `open`/`completed`/`abandoned` need no label, see `statusOf`/`labelsFor`).

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
  inReview : Orchestra.Taxis.LabelId
  rejected : Orchestra.Taxis.LabelId
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
        inReview := ← ensure "o-in-review"
        rejected := ← ensure "o-rejected"
        project  := ← ensure "t-project" }
    statusLabelIdsRef.set (some ids)
    return ids

/-- The `IssueStatus` a taxis issue's `state` + `labels` represent. -/
private def statusOf (ids : StatusLabelIds) (state : Orchestra.Taxis.IssueState) (labels : Array Orchestra.Taxis.LabelId) :
    IssueStatus :=
  match state with
  | .completed => .completed
  | .closed => .abandoned
  | .open =>
    if labels.contains ids.claimed then .claimed
    else if labels.contains ids.inReview then .inReview
    else if labels.contains ids.rejected then .rejected
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
  let statusIds := #[ids.claimed, ids.inReview, ids.rejected]
  let cleared := current.filter (fun l => !statusIds.contains l)
  match status with
  | .claimed  => cleared.push ids.claimed
  | .inReview => cleared.push ids.inReview
  | .rejected => cleared.push ids.rejected
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

/-- Update an *existing* issue's title/description/status/target/dependencies.

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
      state := some (stateOf i.status)
      labels := some (labelsFor ids raw.labels i.status)
      dependencies := some i.dependencies })

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
    an issue without giving its project. Walks the issue's ancestor chain up to whichever ancestor
    carries the `t-project` label. -/
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
      match ← Orchestra.Taxis.getIssue cfg id with
      | .error _ => return none
      | .ok raw =>
        if raw.labels.contains ids.project then
          return some (← toProject raw)
        else
          match raw.parent with
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
def dispatchCandidates (all : Array Orchestra.Taxis.Issue) (label : Orchestra.Taxis.LabelId) :
    Array Orchestra.Taxis.Issue :=
  let byId : Std.HashMap Int64 Orchestra.Taxis.Issue :=
    Std.HashMap.ofList (all.toList.map fun i => (i.id.val, i))
  let hasOpenChildren : Std.HashMap Int64 Unit :=
    Std.HashMap.ofList (all.toList.filterMap fun i =>
      match i.state with
      | .open => i.parent.map (·.val, ())
      | _ => none)
  let rec inScope (i : Orchestra.Taxis.Issue) : Nat → Bool
    | 0 => false
    | fuel + 1 =>
      if i.labels.contains label then true
      else match i.parent with
        | none => false
        | some p => match byId.get? p.val with
          | some parent => inScope parent fuel
          | none => false
  -- Only open issues are candidates. Without this a completed issue stays "in scope" forever —
  -- it still inherits the label and has no open children — so every tick would re-resolve its
  -- target and, if it has no artifacts, warn about it again.
  all.filter fun i =>
    i.state == .open && inScope i all.size && !hasOpenChildren.contains i.id.val

/-- Every issue carrying `labelName`, regardless of project — the issue set the
    project-independent dispatcher works from. `none` means no such label exists on the tracker
    (distinct from "the label exists and nothing carries it", which is `some #[]`); the label is
    looked up rather than created, since a dispatcher configured against a typo'd label should
    report that, not silently conjure it.

    `projectId` on each returned issue is the ancestor carrying the `repository` artifact, so
    issues from different projects can appear side by side; issues whose target can't be resolved
    are omitted along with the reason, for the caller to report — every exclusion is reported,
    since an issue that silently never dispatches gives you nothing to debug. -/
def issuesWithLabel (labelName : String) :
    IO (Option (Array (Issue × RepoTarget) × Array (Taxis.IssueId × TargetGap))) := do
  let cfg ← Orchestra.Taxis.getConfig
  let labels ← unwrap (← Orchestra.Taxis.listLabels cfg)
  let some label := labels.find? (·.name == labelName) | return none
  -- The whole tracker, not just the labelled issues: the label is inherited downward, so
  -- deciding whether an issue is in scope needs its ancestors, and deciding whether it is a leaf
  -- needs everyone else's parent pointers. One list call plus in-memory work.
  let all ← unwrap (← Orchestra.Taxis.listIssues cfg)
  let mut ok : Array (Issue × RepoTarget) := #[]
  let mut gaps : Array (Taxis.IssueId × TargetGap) := #[]
  for raw in dispatchCandidates all label.id do
    match ← artifactTarget raw.id with
    | .error gap => gaps := gaps.push (raw.id, gap)
    | .ok at' =>
      -- Re-fetched (rather than reusing `raw`) to pick up attached PRs and the artifact-derived
      -- project id. One extra GET per candidate; taxis has no batch issue-detail endpoint, the
      -- same limitation `loadIssues` already documents.
      if let some issue ← loadIssue at'.repoOwner raw.id then
        ok := ok.push (issue, at'.target)
  return some (ok, gaps)

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
        let arts := detail.attachedArtifacts
        let anchors :=
          detail.issue.labels.contains ids.project ||
          (arts.any (·.kind == "repository") && arts.any (·.kind == "github-branch"))
        if anchors then pure (some id)
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
