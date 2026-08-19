import OrchestraTest.TestM
import Orchestra.Server

open Lean (Json)
open Orchestra
open Orchestra.Server

namespace OrchestraTest.MergePr

/-!
# The `merge_pr` tool

Merging is the one tool that cannot be taken back, so the two things worth pinning down are that
a task which was not granted it is refused, and that a call which *is* granted goes to GitHub
with the arguments the agent asked for.

The merge itself needs a network and a pull request, so it is not exercised here. Everything
below stops before `gh` is spawned: the refusals return immediately, and the rest is parsing and
the pure state mapping (`GitHub.mergeBlockedReason`) that turns a refused merge into a sentence
the calling agent can report.
-/

/-! ## Permission gating -/

private def state (tools : List String) (pat : String := "pat") : State :=
  { repo     := some { upstream := { owner := "up",   name := "repo" }
                     , fork     := { owner := "fork", name := "repo" } }
  , allowedTools := tools
  , appId := 0
  , privateKeyPath := ""
  , installationId := some 0
  , pat }

/-- Extract the inner `text` payload from a tool-content JSON envelope. -/
private def textOf (j : Json) : String :=
  let arr? := j.getObjVal? "content" |>.toOption |>.bind (·.getArr? |>.toOption)
  let first? := arr?.bind (·[0]?)
  (first?.bind (·.getObjValAs? String "text" |>.toOption)).getD ""

private def isError (j : Json) : Bool :=
  j.getObjValAs? Bool "isError" |>.toOption |>.getD false

@[test]
def mergePr_deniedWithoutThePermission : Test := do
  let result ← evalToolCall (state []) (.mergePr 12 .squash true)
  TestM.assert (isError result) "a merge the task was not granted is an error"
  TestM.assert ((textOf result).contains "not enabled for this task")
    "the refusal says the tool is not enabled, not that the merge failed"

@[test]
def mergePr_deniedWhenOnlyOtherToolsAreGranted : Test := do
  -- `create_pr` is the neighbouring PAT-authenticated tool and the one most tasks hold; holding
  -- it must not carry the right to merge what it opened.
  let result ← evalToolCall (state ["create_pr", "comment"]) (.mergePr 12 .squash true)
  TestM.assert (isError result) "create_pr does not imply merge_pr"

@[test]
def mergePr_grantedButNoPatIsReported : Test := do
  -- Past the gate, and the next thing checked is the credential the merge runs on. A PAT-less
  -- config must say so rather than shell out to an unauthenticated `gh`.
  let result ← evalToolCall (state ["merge_pr"] (pat := "")) (.mergePr 12 .squash true)
  TestM.assert (isError result) "a missing PAT is an error"
  TestM.assert ((textOf result).contains "github.pat")
    "the refusal names the config field to set"

/-! ## Argument parsing -/

@[test]
def parseMergePr_defaults : Test := do
  match parseToolCall "merge_pr" (Json.mkObj [("pr_number", .num ⟨42, 0⟩)]) with
  | .mergePr prNumber method deleteBranch =>
    TestM.assertEqual prNumber 42 (msg := "pr_number")
    -- The defaults are the agent-less `merger` backend's behaviour (`--squash --delete-branch`),
    -- so a PR merged by an agent lands the same way as one merged by the queue.
    TestM.assert (method == .squash) "merge method defaults to squash"
    TestM.assertEqual deleteBranch true (msg := "delete_branch defaults to true")
  | _ => TestM.fail "expected .mergePr"

@[test]
def parseMergePr_explicitArguments : Test := do
  let args := Json.mkObj [
    ("pr_number",     .num ⟨7, 0⟩),
    ("merge_method",  .str "rebase"),
    ("delete_branch", .bool false)
  ]
  match parseToolCall "merge_pr" args with
  | .mergePr prNumber method deleteBranch =>
    TestM.assertEqual prNumber 7 (msg := "pr_number")
    TestM.assert (method == .rebase) "merge_method is honoured"
    TestM.assertEqual deleteBranch false (msg := "delete_branch is honoured")
  | _ => TestM.fail "expected .mergePr"

@[test]
def parseMergePr_missingPrNumber : Test := do
  match parseToolCall "merge_pr" (Json.mkObj []) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for missing pr_number"

@[test]
def parseMergePr_nonPositivePrNumber : Test := do
  match parseToolCall "merge_pr" (Json.mkObj [("pr_number", .num ⟨0, 0⟩)]) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for pr_number = 0"

@[test]
def parseMergePr_nonIntegerPrNumber : Test := do
  match parseToolCall "merge_pr" (Json.mkObj [("pr_number", .str "12")]) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for non-integer pr_number"

@[test]
def parseMergePr_unknownMergeMethod : Test := do
  -- Rejected rather than quietly squashed: a caller asking for a method GitHub does not offer
  -- has misunderstood something, and merging anyway is not the recoverable direction to fail in.
  let args := Json.mkObj [("pr_number", .num ⟨3, 0⟩), ("merge_method", .str "fast-forward")]
  match parseToolCall "merge_pr" args with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for an unknown merge_method"

/-! ## What blocks a merge -/

@[test]
def blockedReason_reportsTerminalStates : Test := do
  TestM.assert (GitHub.mergeBlockedReason "MERGED" "" "" |>.isSome)
    (msg := "an already-merged PR is refused")
  TestM.assert (GitHub.mergeBlockedReason "CLOSED" "" "" |>.isSome)
    (msg := "a closed PR is refused")

@[test]
def blockedReason_reportsFixableStates : Test := do
  TestM.assert (GitHub.mergeBlockedReason "OPEN" "CONFLICTING" "DIRTY" |>.isSome)
    (msg := "conflicts are refused")
  TestM.assert (GitHub.mergeBlockedReason "OPEN" "MERGEABLE" "DRAFT" |>.isSome)
    (msg := "a draft is refused")
  TestM.assert (GitHub.mergeBlockedReason "OPEN" "MERGEABLE" "BLOCKED" |>.isSome)
    (msg := "branch protection is refused")
  TestM.assert (GitHub.mergeBlockedReason "OPEN" "MERGEABLE" "BEHIND" |>.isSome)
    (msg := "an out-of-date branch is refused")

@[test]
def blockedReason_letsGitHubDecideTheRest : Test := do
  TestM.assertEqual (GitHub.mergeBlockedReason "OPEN" "MERGEABLE" "CLEAN") none
    (msg := "a clean PR is not blocked")
  -- A failing check the base branch does not require: GitHub merges those, and refusing here
  -- would be stricter than the repository's own rules.
  TestM.assertEqual (GitHub.mergeBlockedReason "OPEN" "MERGEABLE" "UNSTABLE") none
    (msg := "an unstable PR is not blocked")
  -- Mergeability still being computed, and fields that could not be read at all: neither is an
  -- answer, and the merge request itself settles the question.
  TestM.assertEqual (GitHub.mergeBlockedReason "OPEN" "UNKNOWN" "UNKNOWN") none
    (msg := "an undetermined mergeability is not blocked")
  TestM.assertEqual (GitHub.mergeBlockedReason "" "" "") none
    (msg := "unreadable state fields block nothing")

end OrchestraTest.MergePr
