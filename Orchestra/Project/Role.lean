import Lean.Data.Json
import Orchestra.Project.Basic

open Lean (Json FromJson ToJson)

namespace Orchestra.Project

/-! # Roles

A *role* is a reusable task template — backend, model, prompt template, the
permission set the agent is granted, and an optional auto-dispatch policy.
Role names are user-defined (`implementor`, `qa-bot`, `architect`, …); the
only things the dispatcher knows about are a small fixed set of triggers.

File layout, project overrides global by name:

```
~/.agent/roles/<name>.json                       -- global
~/.agent/projects/<pid>/roles/<name>.json        -- per-project override
```
-/

/-- Conditions under which the dispatcher will spawn a role automatically.
    Adding a new variant requires a code change; role *names* do not. -/
inductive RoleTrigger where
  /-- Spawnable while the project has at least one open issue. -/
  | hasOpenIssues
  /-- Spawnable while the project has at least one in-review issue. -/
  | hasInReviewIssues
  /-- Spawnable when the project has no open and no in-review issues
      (your "planner runs when there's nothing else to do" policy). -/
  | idle
  /-- Spawnable whenever the role is under its cap, bound to no issue. For a role that holds
      every hat at once: it decides for itself whether to plan, work or review, and claims any
      issue it works on through `claim_issue` rather than being handed one. `pre_claim` is
      meaningless here — there is no issue at spawn time to pre-claim. -/
  | always
deriving Repr, Inhabited, BEq, DecidableEq

instance : ToJson RoleTrigger where
  toJson
    | .hasOpenIssues     => "has_open_issues"
    | .hasInReviewIssues => "has_in_review_issues"
    | .idle              => "idle"
    | .always            => "always"

instance : FromJson RoleTrigger where
  fromJson?
    | .str "has_open_issues"      => .ok .hasOpenIssues
    | .str "has_in_review_issues" => .ok .hasInReviewIssues
    | .str "idle"                 => .ok .idle
    | .str "always"               => .ok .always
    | j => .error s!"unknown role trigger {j}; \
        expected has_open_issues | has_in_review_issues | idle | always"

/-- Auto-dispatch policy for a role. Default `max := 0` means "off unless a
    project-dispatcher listener explicitly sets a cap" (Q2 decision). -/
structure DispatchPolicy where
  trigger  : RoleTrigger
  /-- Default cap if the dispatcher config omits one. `0` means auto-spawn
      is opt-in only — you must set it in the dispatcher config to enable. -/
  max      : Nat  := 0
  /-- For `hasOpenIssues` roles: claim an issue at spawn time so the agent
      boots straight into work. Ignored for the other triggers. -/
  preClaim : Bool := false
deriving Repr, Inhabited

instance : ToJson DispatchPolicy where
  toJson d :=
    Json.mkObj
      [ ("trigger",   ToJson.toJson d.trigger)
      , ("max",       Json.num d.max)
      , ("pre_claim", Json.bool d.preClaim) ]

instance : FromJson DispatchPolicy where
  fromJson? j := do
    let trigger  ← j.getObjValAs? RoleTrigger "trigger"
    let max      := j.getObjValAs? Nat "max"      |>.toOption |>.getD 0
    let preClaim := j.getObjValAs? Bool "pre_claim" |>.toOption |>.getD false
    return { trigger, max, preClaim }

/-- A user-defined role template. -/
structure Role where
  name           : String
  permissions    : List String
  backend        : Option String := none
  model          : Option String := none
  systemPrompt   : Option String := none
  prependPrompt  : Option String := none
  readOnly       : Bool          := false
  priority       : Nat           := 10
  budget         : Option Float  := none
  /-- Prompt body — supports {{project_id}}, {{project_name}}, {{instructions}},
      and (when an issue is bound) {{issue_id}}, {{issue_title}}, {{issue_description}},
      {{issue_comments}}, {{target_repo}}, {{target_branch}}, {{pr_number}}, {{pr_branch}},
      {{pr_repo}}.
      Unrecognised placeholders pass through.

      Worker templates should include {{issue_description}}: it is the only way the issue body
      reaches an agent, since the tool that renders it (`get_issue`) needs `manage_issues`. -/
  promptTemplate : String
  /-- Optional auto-dispatch policy. `none` = manual-spawn only. -/
  dispatch       : Option DispatchPolicy := none
deriving Repr, Inhabited

instance : ToJson Role where
  toJson r :=
    let base : List (String × Json) :=
      [ ("name",            Json.str r.name)
      , ("permissions",     ToJson.toJson r.permissions)
      , ("read_only",       Json.bool r.readOnly)
      , ("priority",        Json.num r.priority)
      , ("prompt_template", Json.str r.promptTemplate) ]
    let f := base
    let f := if let some b := r.backend       then f ++ [("backend",        Json.str b)] else f
    let f := if let some m := r.model         then f ++ [("model",          Json.str m)] else f
    let f := if let some s := r.systemPrompt  then f ++ [("system_prompt",  Json.str s)] else f
    let f := if let some s := r.prependPrompt then f ++ [("prepend_prompt", Json.str s)] else f
    let f := if let some b := r.budget        then f ++ [("budget",         ToJson.toJson b)] else f
    let f := if let some d := r.dispatch      then f ++ [("dispatch",       ToJson.toJson d)] else f
    Json.mkObj f

instance : FromJson Role where
  fromJson? j := do
    let name           ← j.getObjValAs? String "name"
    let permissions    ← j.getObjValAs? (List String) "permissions"
    let promptTemplate ← j.getObjValAs? String "prompt_template"
    let backend       := j.getObjValAs? String "backend"        |>.toOption
    let model         := j.getObjValAs? String "model"          |>.toOption
    let systemPrompt  := j.getObjValAs? String "system_prompt"  |>.toOption
    let prependPrompt := j.getObjValAs? String "prepend_prompt" |>.toOption
    let readOnly      := j.getObjValAs? Bool "read_only" |>.toOption |>.getD false
    let priority      := j.getObjValAs? Nat "priority"   |>.toOption |>.getD 10
    let budget        := j.getObjValAs? Float "budget"   |>.toOption
    let dispatch      := j.getObjValAs? DispatchPolicy "dispatch" |>.toOption
    return { name, permissions, backend, model, systemPrompt, prependPrompt
           , readOnly, priority, budget, promptTemplate, dispatch }

/-! ## Filesystem layout -/

/-- Optional override for the global roles directory (tests redirect this). -/
initialize globalRolesDirOverride : IO.Ref (Option System.FilePath) ← IO.mkRef none

def setGlobalRolesDirOverride (p : Option System.FilePath) : IO Unit :=
  globalRolesDirOverride.set p

def globalRolesDir : IO System.FilePath := do
  match ← globalRolesDirOverride.get with
  | some p => return p
  | none   => return (← Dirs.configBase) / "roles"

def projectRolesDir (pid : Taxis.IssueId) : IO System.FilePath := do
  return (← projectDir pid) / "roles"

private def roleFileName (name : String) : String := s!"{name}.json"

private def loadRoleFromFile (path : System.FilePath) : IO (Option Role) := do
  if !(← path.pathExists) then return none
  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error _ => return none
  | .ok j    =>
    match FromJson.fromJson? j with
    | .error _ => return none
    | .ok r    => return some r

/-- Resolve a role by name. Project-scoped file wins over the global one;
    returns `none` only if neither file exists or both fail to parse. -/
def loadRole (pid : Taxis.IssueId) (name : String) : IO (Option Role) := do
  let projectPath := (← projectRolesDir pid) / roleFileName name
  if let some r ← loadRoleFromFile projectPath then return some r
  loadRoleFromFile ((← globalRolesDir) / roleFileName name)

/-- Convenience for diagnostics: report the two paths that would be searched. -/
def roleSearchPaths (pid : Taxis.IssueId) (name : String) :
    IO (System.FilePath × System.FilePath) := do
  return ((← projectRolesDir pid) / roleFileName name,
          (← globalRolesDir)      / roleFileName name)

private def stripJsonExt (s : String) : Option String :=
  let ext := ".json"
  if s.endsWith ext then some (s.dropEnd ext.length).toString else none

/-- Roles from the global directory only. Used by the project-independent dispatcher
    (`Listener.SourceConfig.labelDispatcher`), whose issues can span projects — there is no single
    project whose `roles/` directory would take precedence, so only globals apply there. -/
def loadGlobalRoles : IO (Array Role) := do
  let mut byName : Std.HashMap String Role := {}
  let gdir ← globalRolesDir
  if ← gdir.pathExists then
    for entry in ← System.FilePath.readDir gdir do
      if let some _ := stripJsonExt entry.fileName then
        if let some r ← loadRoleFromFile entry.path then
          byName := byName.insert r.name r
  return byName.toArray.map (·.2)

/-- All roles available for a project: project-scoped roles first, then any
    global roles whose names aren't already shadowed by a project file. -/
def loadAllRoles (pid : Taxis.IssueId) : IO (Array Role) := do
  let mut byName : Std.HashMap String Role := {}
  let pdir ← projectRolesDir pid
  if ← pdir.pathExists then
    for entry in ← System.FilePath.readDir pdir do
      if let some _ := stripJsonExt entry.fileName then
        if let some r ← loadRoleFromFile entry.path then
          byName := byName.insert r.name r
  let gdir ← globalRolesDir
  if ← gdir.pathExists then
    for entry in ← System.FilePath.readDir gdir do
      if let some _ := stripJsonExt entry.fileName then
        if let some r ← loadRoleFromFile entry.path then
          if !byName.contains r.name then
            byName := byName.insert r.name r
  return byName.toArray.map (·.2)

/-! ## Template rendering -/

/-- Variables available to a role template. Issue-specific fields are `none`
    for triggers that don't bind to one issue (e.g. planners).
    Supports: {{project_id}}, {{project_name}}, {{instructions}},
    {{issue_id}}, {{issue_title}}, {{target_repo}}, {{target_branch}},
    {{pr_number}}, {{pr_branch}}, {{pr_repo}}. -/
structure RenderVars where
  projectId    : String
  projectName  : String
  instructions : String
  issueId      : Option String := none
  issueTitle   : Option String := none
  /-- The issue's body. Nothing else puts it in front of the agent: `get_issue` is the only tool
      that renders it and that is gated on `manage_issues`, which worker roles do not have. -/
  issueDescription : Option String := none
  /-- The issue's comment thread. Carries a reviewer's reasoning to whoever picks the issue up
      next, which matters most for a rejection: there is no rejected status, so the
      request-changes review is the only record of what was wrong. -/
  issueComments : Option String := none
  targetRepo   : Option String := none
  targetBranch : Option String := none
  prNumber     : Option String := none
  prBranch     : Option String := none
  prRepo       : Option String := none
deriving Repr, Inhabited

/-- Substitute `{{name}}` placeholders. Unknown placeholders are left in place
    so a template error is loud, not silent. -/
def render (tmpl : String) (v : RenderVars) : String :=
  let subs : List (String × String) :=
    [ ("{{project_id}}",    v.projectId)
    , ("{{project_name}}",  v.projectName)
    , ("{{instructions}}",  v.instructions)
    , ("{{issue_id}}",      v.issueId.getD "")
    , ("{{issue_title}}",   v.issueTitle.getD "")
    , ("{{issue_description}}", v.issueDescription.getD "")
    , ("{{issue_comments}}", v.issueComments.getD "")
    , ("{{target_repo}}",   v.targetRepo.getD "")
    , ("{{target_branch}}", v.targetBranch.getD "")
    , ("{{pr_number}}",     v.prNumber.getD "")
    , ("{{pr_branch}}",     v.prBranch.getD "")
    , ("{{pr_repo}}",       v.prRepo.getD "") ]
  subs.foldl (fun acc (k, val) => acc.replace k val) tmpl

/-- Build render vars for a project + optional issue. Pulls the effective
    target from `effectiveTarget` so per-issue overrides are honoured.
    If the issue has attached PRs, the most recent one populates the pr_* vars.

    `targetOverride` mirrors `Listener.buildRoleEntry`'s: for label-dispatched issues the target
    comes from taxis artifacts, and without threading it through here `{{target_repo}}` and
    `{{target_branch}}` would render empty in the very prompts that need them. -/
def renderVarsFor (project : Project) (issue? : Option Issue) (instructions : String)
    (targetOverride : Option RepoTarget := none) (comments : Option String := none)
    : RenderVars :=
  match issue? with
  | none => { projectId := project.id.toString, projectName := project.name, instructions }
  | some i =>
    let target := targetOverride <|> effectiveTarget project i
    let pr? := i.attachedPRs.toList.reverse.head?
    { projectId    := project.id.toString
    , projectName  := project.name
    , instructions
    , issueId      := some i.id.toString
    , issueTitle   := some i.title
    , issueDescription := some i.description
    , issueComments := comments
    , targetRepo   := target.map (·.repo.toString)
    , targetBranch := target.map (·.branch)
    , prNumber     := pr?.map (fun p => toString p.number)
    , prBranch     := pr?.map (·.branch)
    , prRepo       := pr?.map (·.repo.toString) }

end Orchestra.Project
