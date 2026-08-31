import OrchestraTest.TestM
import Orchestra

open Lean (Json)
open Orchestra
open Orchestra.Project.Tools (WriteScope)

/-!
# Sessions a person is driving

A queued task is confined to the subtree it was dispatched to. An interactive session is
dispatched to nothing at all — no issue, no project — and used to inherit the consequence of
that rather than a decision about it: the three issue permission groups were granted, every
taxis tool was listed, and then every write was refused for want of a subtree to scope it to.
The tools were there and none of them worked.

`Env.unscopedWrites` is the decision. It says a session with a person in front of it writes
without a subtree bound, which is what `Interactive.allOptionalTools` already claimed to grant.

What has to hold, and is checked here without a tracker or a network:

* the flag lifts the bound, and nothing else does — a task that sets neither it nor an
  issue/project is still refused, exactly as before;
* a `scopeRoot` handed down by a queueing task outranks the flag, so the precedence cannot be
  inverted by a caller that sets both; and
* `project_info` is offered to a session that has no project, and answers rather than errors —
  it is where an agent finds out that nothing bounds it.

The subtree case itself (`.subtree` from a real issue) walks the tracker's parent chain, so it
lives with the tests that have one.
-/

namespace OrchestraTest.UnscopedWrites

open Orchestra.Project.Tools

private def iid (n : Int64) : Taxis.IssueId := ⟨n⟩

/-! ## The scope itself -/

@[test]
def noIssueNoProjectAndNoLicenceIsStillRefused : Test := do
  -- The pre-existing behaviour, pinned. A queued task that somehow reached the tools without an
  -- issue or a project must not start writing just because the interactive path now can.
  let env : Env := { allowedTools := ["manage_issues"] }
  match ← writeScopeRoot env with
  | .unattached => TestM.assert true "nothing to scope to"
  | other => TestM.fail s!"expected .unattached, got {repr other}"

@[test]
def theFlagLiftsTheBound : Test := do
  let env : Env := { allowedTools := ["manage_issues"], unscopedWrites := true }
  match ← writeScopeRoot env with
  | .unbounded => TestM.assert true "a person is driving"
  | other => TestM.fail s!"expected .unbounded, got {repr other}"

@[test]
def aHandedDownScopeOutranksTheFlag : Test := do
  -- The security-relevant ordering. `scopeRoot` is set only on a task queued by another one, and
  -- carries the queueing task's own bound; a descendant that also carried `unscopedWrites` would
  -- otherwise lift the very bound it was given. Checked here because it is the one case where
  -- two fields disagree and only one answer is safe.
  let env : Env :=
    { allowedTools := ["manage_issues"], unscopedWrites := true, scopeRoot := some (iid 42) }
  match ← writeScopeRoot env with
  | .subtree root => TestM.assertEqual root.val (42 : Int64) (msg := "the handed-down root wins")
  | other => TestM.fail s!"expected .subtree 42, got {repr other}"

@[test]
def anIssuelessProjectIsStillItsOwnBound : Test := do
  -- Unchanged by this work, and worth pinning next to the cases that did change: a role
  -- dispatched to a project without an issue is scoped to that project, not left unattached and
  -- not handed the whole tracker.
  let env : Env := { allowedTools := ["manage_issues"], projectId := some (iid 7) }
  match ← writeScopeRoot env with
  | .subtree root => TestM.assertEqual root.val (7 : Int64) (msg := "scoped to its project")
  | other => TestM.fail s!"expected .subtree 7, got {repr other}"

/-! ## What the agent is told -/

private def textOf (j : Json) : String :=
  let arr := (j.getObjVal? "content" |>.toOption.bind (·.getArr?.toOption)).getD #[]
  ((arr[0]?).bind (·.getObjValAs? String "text" |>.toOption)).getD ""

private def isError (j : Json) : Bool :=
  (j.getObjValAs? Bool "isError" |>.toOption).getD false

@[test]
def projectInfoAnswersAnUnscopedSessionInsteadOfErroring : Test := do
  -- `isError` on this would read to the agent as a tool it should stop calling, when in fact
  -- the honest answer — no project, and no bound either — is the thing it most needs to hear
  -- before it goes hunting for a project it assumes it was given.
  let env : Env := { allowedTools := ["manage_issues"], unscopedWrites := true }
  let result ← evalProjectTool env .projectInfo
  TestM.assert (!isError result) (msg := "not attached to a project is not a failure here")
  TestM.assert ((textOf result).containsSubstr "list_projects")
    (msg := "says where to look instead")

@[test]
def projectInfoStillErrorsForATaskThatShouldHaveHadAProject : Test := do
  let env : Env := { allowedTools := ["manage_issues"] }
  let result ← evalProjectTool env .projectInfo
  TestM.assert (isError result) (msg := "a dispatched task with no project is a real problem")

@[test]
def anUnattachedTaskIsRefusedInTheWordsItAlwaysWas : Test := do
  -- Reached before any tracker call, so this runs without one. It is the message the interactive
  -- session used to get for every write it attempted.
  let env : Env := { allowedTools := ["work_issues"] }
  let result ← evalProjectTool env (.addContext (iid 1) "a title" "some text")
  TestM.assert (isError result) (msg := "refused")
  TestM.assert ((textOf result).containsSubstr "attached to no project or issue")
    (msg := "and says why")

/-! ## What the session is offered -/

private def toolNames (state : Server.State) : Array String :=
  ((Server.toolsList state).getObjVal? "tools" |>.toOption.bind (·.getArr?.toOption))
    |>.getD #[] |>.filterMap (·.getObjValAs? String "name" |>.toOption)

private def baseState : Server.State :=
  { repo := none, allowedTools := ["manage_issues", "work_issues", "review_issues"]
  , appId := 0, privateKeyPath := "", installationId := none, pat := "" }

@[test]
def projectInfoIsOfferedToAnUnscopedSession : Test := do
  -- `tools/list` is what the agent plans against, so a tool that will answer has to appear in
  -- it. Gated on the project id alone, this was the one taxis tool a session could not even see.
  TestM.assert ((toolNames { baseState with unscopedWrites := true }).contains "project_info")
    (msg := "offered when writes are unscoped, though there is no project")
  TestM.assert (!(toolNames baseState).contains "project_info")
    (msg := "and still withheld from a task that has neither")

@[test]
def theIssueGroupsAreListedForASessionAsBefore : Test := do
  -- Unchanged, and the reason the bug looked like a configuration problem rather than a missing
  -- field: the tools were always listed. Only calling them failed.
  let names := toolNames { baseState with unscopedWrites := true }
  for expected in ["create_issue", "update_issue", "add_context", "list_issues"] do
    TestM.assert (names.contains expected) (msg := s!"{expected} is offered")

/-! ## Finding the config the daemon was started with -/

@[test]
def theConfigFlagIsFoundBeforeTheParserRuns : Test := do
  -- `ensureTaxisConfigured` runs ahead of `validate`, so it reads the flag out of the raw
  -- arguments. Loading the default path regardless — what it used to do — left a daemon started
  -- with `--config` reporting "taxis is not configured" from every taxis tool it served.
  let cases : List (List String × Option String) :=
    [ (["serve", "--config", "/etc/orchestra.json"], some "/etc/orchestra.json")
    , (["serve", "--config=/etc/orchestra.json"],    some "/etc/orchestra.json")
    , (["serve", "-c", "/etc/orchestra.json"],       some "/etc/orchestra.json")
    , (["serve", "--debug"],                         none)
    , ([],                                           none)
      -- A trailing flag with no value is a user error the parser will report; reaching past the
      -- end of the list for it would be a crash before the parser ever gets to say so.
    , (["serve", "--config"],                        none) ]
  for (args, expected) in cases do
    let got := (Project.configPathInArgs args).map (·.toString)
    TestM.assertEqual got expected (msg := s!"args: {args}")

end OrchestraTest.UnscopedWrites
