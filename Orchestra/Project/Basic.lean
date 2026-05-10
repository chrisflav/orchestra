import Lean.Data.Json
import Orchestra.Config
import Orchestra.TaskStore
import Orchestra.Project.Id

open Lean (Json FromJson ToJson)

namespace Orchestra.Project

/-! ## Targets

A `RepoTarget` is the (repo, branch) pair against which an issue's PRs must
be opened. Issues may set this individually (multi-org projects); otherwise
they inherit the project's `defaultTarget`. -/

structure RepoTarget where
  repo   : Repository
  branch : String
deriving Repr, Inhabited

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
deriving Repr, Inhabited

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

/-! ## Review records

A `IssueReview` is appended to an issue each time a reviewer rejects it.
Only non-approving decisions are stored; approvals immediately enqueue a
merger task and leave the issue in `.inReview` until it is merged or fails
validation. -/

structure IssueReview where
  /-- Backend label or task ID of the reviewer agent (e.g. "claude"). -/
  reviewer  : String
  /-- Free-text notes explaining why the PR was rejected. -/
  notes     : String
  /-- ISO 8601 timestamp of the decision. -/
  decidedAt : String
deriving Repr, Inhabited

instance : ToJson IssueReview where
  toJson r :=
    Json.mkObj
      [ ("reviewer",   Json.str r.reviewer)
      , ("notes",      Json.str r.notes)
      , ("decided_at", Json.str r.decidedAt) ]

instance : FromJson IssueReview where
  fromJson? j := do
    let reviewer  ← j.getObjValAs? String "reviewer"
    let notes     ← j.getObjValAs? String "notes"
    let decidedAt ← j.getObjValAs? String "decided_at"
    return { reviewer, notes, decidedAt }

/-! ## Issue lifecycle -/

inductive IssueStatus where
  | open
  | claimed
  | inReview
  /-- Parent issue split into sub-issues by a worker; not pickable until
      either a reviewer / human moves it forward, or you manually re-open. -/
  | blocked
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
    | .blocked   => "blocked"
    | .completed => "completed"
    | .abandoned => "abandoned"
    | .rejected  => "rejected"

instance : FromJson IssueStatus where
  fromJson?
    | .str "open"      => .ok .open
    | .str "claimed"   => .ok .claimed
    | .str "in_review" => .ok .inReview
    | .str "blocked"   => .ok .blocked
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
  id            : ProjectId
  name          : String
  description   : Option String      := none
  createdAt     : String
  /-- Default target inherited by issues that don't set their own. Multi-org
      projects can leave this `none` and require each issue to specify. -/
  defaultTarget : Option RepoTarget  := none
  /-- Optional reviewer template (F1). Set to enable auto-review on attach_pr. -/
  reviewer      : Option ReviewerTemplate := none
deriving Repr, Inhabited

instance : ToJson Project where
  toJson p :=
    let base : List (String × Json) :=
      [ ("id",         ToJson.toJson p.id)
      , ("name",       Json.str p.name)
      , ("created_at", Json.str p.createdAt) ]
    let fields := base
    let fields := if let some d := p.description   then fields ++ [("description",    Json.str d)]      else fields
    let fields := if let some t := p.defaultTarget then fields ++ [("default_target", ToJson.toJson t)] else fields
    let fields := if let some r := p.reviewer      then fields ++ [("reviewer",       ToJson.toJson r)] else fields
    Json.mkObj fields

instance : FromJson Project where
  fromJson? j := do
    let id            ← j.getObjValAs? ProjectId "id"
    let name          ← j.getObjValAs? String "name"
    let createdAt     ← j.getObjValAs? String "created_at"
    let description   := j.getObjValAs? String "description"      |>.toOption
    let defaultTarget := j.getObjValAs? RepoTarget "default_target" |>.toOption
    let reviewer      := j.getObjValAs? ReviewerTemplate "reviewer" |>.toOption
    return { id, name, description, createdAt, defaultTarget, reviewer }

/-! ## Issue record

`parentId` is the only hierarchy pointer (decision B1). The child set is
recovered by scanning the project's issues directory; this avoids
denormalisation drift across moves and renames. -/

structure Issue where
  id           : IssueId
  projectId    : ProjectId
  parentId     : Option IssueId  := none
  title        : String
  description  : String
  status       : IssueStatus     := .open
  /-- Per-issue override of the project's default target. -/
  target       : Option RepoTarget := none
  attachedPRs  : Array PRRef     := #[]
  /-- Issues that must be completed before this one can be dispatched. -/
  dependencies : Array IssueId   := #[]
  /-- Non-approving review decisions, oldest first. -/
  reviews      : Array IssueReview := #[]
  createdAt    : String
  updatedAt    : String
deriving Repr, Inhabited

instance : ToJson Issue where
  toJson i :=
    let base : List (String × Json) :=
      [ ("id",          ToJson.toJson i.id)
      , ("project_id",  ToJson.toJson i.projectId)
      , ("title",       Json.str i.title)
      , ("description", Json.str i.description)
      , ("status",      ToJson.toJson i.status)
      , ("attached_prs", Json.arr (i.attachedPRs.map ToJson.toJson))
      , ("created_at",  Json.str i.createdAt)
      , ("updated_at",  Json.str i.updatedAt) ]
    let fields := base
    let fields := if let some p := i.parentId then fields ++ [("parent_id", ToJson.toJson p)] else fields
    let fields := if let some t := i.target   then fields ++ [("target",    ToJson.toJson t)] else fields
    let fields := if !i.dependencies.isEmpty  then fields ++ [("dependencies", Json.arr (i.dependencies.map ToJson.toJson))] else fields
    let fields := if !i.reviews.isEmpty       then fields ++ [("reviews",      Json.arr (i.reviews.map ToJson.toJson))]       else fields
    Json.mkObj fields

instance : FromJson Issue where
  fromJson? j := do
    let id          ← j.getObjValAs? IssueId "id"
    let projectId   ← j.getObjValAs? ProjectId "project_id"
    let title       ← j.getObjValAs? String "title"
    let description ← j.getObjValAs? String "description"
    let status      ← j.getObjValAs? IssueStatus "status"
    let createdAt   ← j.getObjValAs? String "created_at"
    let updatedAt   ← j.getObjValAs? String "updated_at"
    let parentId     := j.getObjValAs? IssueId "parent_id" |>.toOption
    let target       := j.getObjValAs? RepoTarget "target"   |>.toOption
    let attachedPRs  := (j.getObjValAs? (Array PRRef) "attached_prs" |>.toOption).getD #[]
    let dependencies := (j.getObjValAs? (Array IssueId) "dependencies" |>.toOption).getD #[]
    let reviews      := (j.getObjValAs? (Array IssueReview) "reviews" |>.toOption).getD #[]
    return { id, projectId, parentId, title, description, status, target, attachedPRs,
             dependencies, reviews, createdAt, updatedAt }

/-! ## Filesystem layout

```
~/.agent/projects/
  <project-id>.json              -- Project record
  <project-id>/
    issues/<issue-id>.json       -- Issue record
    claims/<issue-id>.json       -- Claim lock (separate file: §4 of plan)
```
-/

private def homeDir : IO System.FilePath := do
  match ← IO.getEnv "HOME" with
  | some h => return System.FilePath.mk h
  | none   => throw (.userError "HOME not set")

/-- Optional override for the projects root directory. Tests set this to a
    temp directory to avoid touching the user's real `~/.agent/projects`.
    When `none`, paths are derived from `$HOME` as usual. -/
initialize projectsDirOverride : IO.Ref (Option System.FilePath) ← IO.mkRef none

def setProjectsDirOverride (p : Option System.FilePath) : IO Unit :=
  projectsDirOverride.set p

def projectsDir : IO System.FilePath := do
  match ← projectsDirOverride.get with
  | some p => return p
  | none   => return (← homeDir) / ".agent" / "projects"

def projectDir (pid : ProjectId) : IO System.FilePath := do
  return (← projectsDir) / pid.value

def projectFile (pid : ProjectId) : IO System.FilePath := do
  return (← projectsDir) / s!"{pid.value}.json"

def issuesDir (pid : ProjectId) : IO System.FilePath := do
  return (← projectDir pid) / "issues"

def issueFile (pid : ProjectId) (iid : IssueId) : IO System.FilePath := do
  return (← issuesDir pid) / s!"{iid.value}.json"

def claimsDir (pid : ProjectId) : IO System.FilePath := do
  return (← projectDir pid) / "claims"

def claimFile (pid : ProjectId) (iid : IssueId) : IO System.FilePath := do
  return (← claimsDir pid) / s!"{iid.value}.json"

/-! ## ID generation

Reuse the monotonic-clock 16-char hex scheme from `TaskStore.generateId` so
all orchestra IDs share one namespace shape. Wrap into the type-safe carriers
on the way out. -/

def freshProjectId : IO ProjectId := do
  return ⟨← TaskStore.generateId⟩

def freshIssueId : IO IssueId := do
  return ⟨← TaskStore.generateId⟩

/-! ## Persistence -/

def saveProject (p : Project) : IO Unit := do
  let dir ← projectsDir
  IO.FS.createDirAll dir
  IO.FS.createDirAll (← projectDir p.id)
  IO.FS.createDirAll (← issuesDir p.id)
  IO.FS.createDirAll (← claimsDir p.id)
  IO.FS.writeFile (← projectFile p.id) (Json.compress (ToJson.toJson p))

def loadProject (pid : ProjectId) : IO (Option Project) := do
  let path ← projectFile pid
  if !(← path.pathExists) then return none
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error e =>
    IO.eprintln s!"[warn] failed to parse project file {path}: {e}"
    return none
  | .ok j    =>
    match FromJson.fromJson? j with
    | .error e =>
      IO.eprintln s!"[warn] failed to parse project file {path}: {e}"
      return none
    | .ok p    => return some p

private def stripJsonExt (name : String) : Option String :=
  let ext := ".json"
  if name.endsWith ext then some (name.dropEnd ext.length).toString else none

/-- All projects, newest first by id. -/
def loadAllProjects : IO (Array Project) := do
  let dir ← projectsDir
  if !(← dir.pathExists) then return #[]
  let entries ← System.FilePath.readDir dir
  let mut out : Array Project := #[]
  for entry in entries do
    if let some idStr := stripJsonExt entry.fileName then
      if let some p ← loadProject ⟨idStr⟩ then
        out := out.push p
  return out.qsort (fun a b => a.id.value > b.id.value)

def saveIssue (i : Issue) : IO Unit := do
  IO.FS.createDirAll (← issuesDir i.projectId)
  IO.FS.writeFile (← issueFile i.projectId i.id) (Json.compress (ToJson.toJson i))

def loadIssue (pid : ProjectId) (iid : IssueId) : IO (Option Issue) := do
  let path ← issueFile pid iid
  if !(← path.pathExists) then return none
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error e =>
    IO.eprintln s!"[warn] failed to parse issue file {path}: {e}"
    return none
  | .ok j    =>
    match FromJson.fromJson? j with
    | .error e =>
      IO.eprintln s!"[warn] failed to parse issue file {path}: {e}"
      return none
    | .ok r    => return some r

/-- All issues belonging to a project. -/
def loadIssues (pid : ProjectId) : IO (Array Issue) := do
  let dir ← issuesDir pid
  if !(← dir.pathExists) then return #[]
  let entries ← System.FilePath.readDir dir
  let mut out : Array Issue := #[]
  for entry in entries do
    if let some idStr := stripJsonExt entry.fileName then
      if let some r ← loadIssue pid ⟨idStr⟩ then
        out := out.push r
  return out

/-- Find an issue by ID across all projects. Returns the owning project too,
    so callers can resolve `effectiveTarget`. Used by the CLI where users
    name an issue without giving its project. -/
def findIssue (iid : IssueId) : IO (Option (Project × Issue)) := do
  for p in (← loadAllProjects) do
    if let some i ← loadIssue p.id iid then
      return some (p, i)
  return none

/-! ## Hierarchy traversal (B1: parentId-only) -/

/-- Direct children of `parent` within `pid`. -/
def childrenOf (pid : ProjectId) (parent : IssueId) : IO (Array Issue) := do
  let all ← loadIssues pid
  return all.filter (fun i => i.parentId == some parent)

/-- Top-level (parentless) issues of `pid`. -/
def rootIssues (pid : ProjectId) : IO (Array Issue) := do
  let all ← loadIssues pid
  return all.filter (fun i => i.parentId.isNone)

/-! ## Target resolution

Returns the effective target for an issue: its own override, otherwise the
project's default. `none` means the issue can only be enqueued with an
explicit upstream/fork at queue time. -/

def effectiveTarget (project : Project) (issue : Issue) : Option RepoTarget :=
  issue.target <|> project.defaultTarget

end Orchestra.Project
