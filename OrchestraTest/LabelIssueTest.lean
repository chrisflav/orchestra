import OrchestraTest.TestM
import Orchestra.Server

open Lean (Json)
open Orchestra
open Orchestra.Server

namespace OrchestraTest.LabelIssue

/-!
# The `label_issue` tool

Triage writes to issues the task was never launched from, so what is worth pinning down is that
a task which was not granted the tool cannot relabel anything, and that a request which *is*
granted turns into exactly the calls the agent asked for — no label invented, none dropped.

Labelling needs a network and an issue, so it is not exercised here. Everything below stops
before `gh` is spawned: the refusals return immediately, and the rest is parsing and the pure
planning (`GitHub.planLabelChange`) that decides which calls a request actually needs.
-/

/-! ## Permission gating -/

private def state (tools : List String) (pat : String := "pat") : State :=
  { upstream := { owner := "up", name := "repo" }
  , fork     := { owner := "fork", name := "repo" }
  , allowedTools := tools
  , appId := 0
  , privateKeyPath := ""
  , installationId := 0
  , pat }

/-- Extract the inner `text` payload from a tool-content JSON envelope. -/
private def textOf (j : Json) : String :=
  let arr? := j.getObjVal? "content" |>.toOption |>.bind (·.getArr? |>.toOption)
  let first? := arr?.bind (·[0]?)
  (first?.bind (·.getObjValAs? String "text" |>.toOption)).getD ""

private def isError (j : Json) : Bool :=
  j.getObjValAs? Bool "isError" |>.toOption |>.getD false

@[test]
def labelIssue_deniedWithoutThePermission : Test := do
  let result ← evalToolCall (state []) (.labelIssue 12 ["t-bug"] [])
  TestM.assert (isError result) "a relabelling the task was not granted is an error"
  TestM.assert ((textOf result).contains "not enabled for this task")
    "the refusal says the tool is not enabled, not that the labelling failed"

@[test]
def labelIssue_deniedWhenOnlyOtherToolsAreGranted : Test := do
  -- `comment` is the neighbouring tool that writes to an issue, and the one a triage-shaped task
  -- is most likely to hold; holding it must not carry the right to relabel.
  let result ← evalToolCall (state ["create_pr", "comment"]) (.labelIssue 12 ["t-bug"] [])
  TestM.assert (isError result) "comment does not imply label_issue"

@[test]
def labelIssue_grantedButNoPatIsReported : Test := do
  -- Past the gate, and the next thing checked is the credential the calls run on. A PAT-less
  -- config must say so rather than shell out to an unauthenticated `gh`.
  let result ← evalToolCall (state ["label_issue"] (pat := "")) (.labelIssue 12 ["t-bug"] [])
  TestM.assert (isError result) "a missing PAT is an error"
  TestM.assert ((textOf result).contains "github.pat")
    "the refusal names the config field to set"

/-! ## Argument parsing -/

@[test]
def parseLabelIssue_addAndRemove : Test := do
  let args := Json.mkObj [
    ("issue_number", .num ⟨42, 0⟩),
    ("add",    .arr #[.str "t-bug", .str "p-high"]),
    ("remove", .arr #[.str "needs-triage"])
  ]
  match parseToolCall "label_issue" args with
  | .labelIssue issueNumber add remove =>
    TestM.assertEqual issueNumber 42 (msg := "issue_number")
    TestM.assertEqual add ["t-bug", "p-high"] (msg := "add")
    TestM.assertEqual remove ["needs-triage"] (msg := "remove")
  | _ => TestM.fail "expected .labelIssue"

@[test]
def parseLabelIssue_oneListIsEnough : Test := do
  let args := Json.mkObj [
    ("issue_number", .num ⟨7, 0⟩),
    ("remove", .arr #[.str "needs-triage"])
  ]
  match parseToolCall "label_issue" args with
  | .labelIssue _ add remove =>
    TestM.assertEqual add ([] : List String) (msg := "an absent list is empty, not an error")
    TestM.assertEqual remove ["needs-triage"] (msg := "remove")
  | _ => TestM.fail "expected .labelIssue"

@[test]
def parseLabelIssue_labelNamesAreTrimmed : Test := do
  let args := Json.mkObj [
    ("issue_number", .num ⟨7, 0⟩),
    ("add", .arr #[.str "  t-bug  "])
  ]
  match parseToolCall "label_issue" args with
  | .labelIssue _ add _ => TestM.assertEqual add ["t-bug"] (msg := "surrounding space is dropped")
  | _ => TestM.fail "expected .labelIssue"

@[test]
def parseLabelIssue_missingIssueNumber : Test := do
  match parseToolCall "label_issue" (Json.mkObj [("add", .arr #[.str "t-bug"])]) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for missing issue_number"

@[test]
def parseLabelIssue_nonPositiveIssueNumber : Test := do
  let args := Json.mkObj [("issue_number", .num ⟨0, 0⟩), ("add", .arr #[.str "t-bug"])]
  match parseToolCall "label_issue" args with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for issue_number = 0"

@[test]
def parseLabelIssue_noLabelsAtAll : Test := do
  -- A call that would write nothing is a mistake worth naming, not a successful no-op.
  match parseToolCall "label_issue" (Json.mkObj [("issue_number", .num ⟨7, 0⟩)]) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError when neither add nor remove is given"

@[test]
def parseLabelIssue_rejectsMalformedLists : Test := do
  -- Dropping the bad entry and applying the rest would tell the agent its triage succeeded when
  -- part of it never happened, which is the one outcome it cannot report accurately.
  let notAnArray := Json.mkObj [("issue_number", .num ⟨7, 0⟩), ("add", .str "t-bug")]
  match parseToolCall "label_issue" notAnArray with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for a non-array 'add'"
  let notStrings := Json.mkObj [
    ("issue_number", .num ⟨7, 0⟩),
    ("add", .arr #[.str "t-bug", .num ⟨3, 0⟩])
  ]
  match parseToolCall "label_issue" notStrings with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for a non-string label"
  let empty := Json.mkObj [("issue_number", .num ⟨7, 0⟩), ("add", .arr #[.str "   "])]
  match parseToolCall "label_issue" empty with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "expected .parseError for an empty label name"

/-! ## Planning the change -/

private def known : List String := ["t-bug", "t-feature", "needs-triage", "good first issue"]

@[test]
def plan_addsAndRemovesWhatIsAsked : Test := do
  match GitHub.planLabelChange known ["needs-triage"] ["t-bug"] ["needs-triage"] with
  | .error e => TestM.fail s!"expected a plan, got: {e}"
  | .ok change =>
    TestM.assertEqual change.add ["t-bug"] (msg := "add")
    TestM.assertEqual change.remove ["needs-triage"] (msg := "remove")
    TestM.assertEqual change.alreadyPresent ([] : List String) (msg := "alreadyPresent")
    TestM.assertEqual change.notPresent ([] : List String) (msg := "notPresent")

@[test]
def plan_skipsWhatIsAlreadyTheCase : Test := do
  -- Both halves of a repeated call: GitHub would accept the addition, but answers a removal of
  -- an absent label with a 404 that would fail the whole call over a label already gone.
  match GitHub.planLabelChange known ["t-bug"] ["t-bug"] ["needs-triage"] with
  | .error e => TestM.fail s!"expected a plan, got: {e}"
  | .ok change =>
    TestM.assertEqual change.add ([] : List String) (msg := "a label already present is not added")
    TestM.assertEqual change.remove ([] : List String) (msg := "a label not present is not removed")
    TestM.assertEqual change.alreadyPresent ["t-bug"] (msg := "but it is reported")
    TestM.assertEqual change.notPresent ["needs-triage"] (msg := "and so is that")

@[test]
def plan_answersInTheRepositorysSpelling : Test := do
  -- An agent asking for `T-Bug` means the `t-bug` the repository defines; sending its spelling
  -- back would have GitHub create a second label rather than say so.
  match GitHub.planLabelChange known [] ["T-Bug"] [] with
  | .error e => TestM.fail s!"expected a plan, got: {e}"
  | .ok change => TestM.assertEqual change.add ["t-bug"] (msg := "canonical spelling")

@[test]
def plan_dropsDuplicateRequests : Test := do
  match GitHub.planLabelChange known [] ["t-bug", "T-BUG"] [] with
  | .error e => TestM.fail s!"expected a plan, got: {e}"
  | .ok change => TestM.assertEqual change.add ["t-bug"] (msg := "asked for twice, added once")

@[test]
def plan_refusesUnknownLabels : Test := do
  match GitHub.planLabelChange known [] ["t-buhg"] [] with
  | .ok _ => TestM.fail "expected an unknown label to be refused"
  | .error e =>
    TestM.assert (e.contains "t-buhg") "the refusal names the label that does not exist"
    -- The vocabulary comes back with the refusal: a triage agent that guessed wrong can pick a
    -- real label from the answer instead of guessing again.
    TestM.assert (e.contains "t-feature") "and lists the labels that do"

@[test]
def plan_refusesUnknownLabelsOnRemoval : Test := do
  match GitHub.planLabelChange known ["t-bug"] [] ["no-such-label"] with
  | .ok _ => TestM.fail "expected an unknown label to be refused on removal too"
  | .error _ => TestM.assert true

@[test]
def plan_refusesContradictoryRequests : Test := do
  -- Serving this would come down to which call GitHub ran last, so neither is run.
  match GitHub.planLabelChange known [] ["t-bug"] ["t-bug"] with
  | .ok _ => TestM.fail "expected adding and removing the same label to be refused"
  | .error e => TestM.assert (e.contains "t-bug") "the refusal names the label"

/-! ## Naming a label in a URL -/

@[test]
def percentEncode_survivesALabelWithSpaces : Test := do
  -- `good first issue` is GitHub's own default label, and the name goes into the path of the
  -- DELETE that removes it. Interpolated raw it was not a path at all.
  TestM.assertEqual (GitHub.percentEncode "good first issue") "good%20first%20issue"
    (msg := "spaces are encoded")
  TestM.assertEqual (GitHub.percentEncode "t-bug") "t-bug"
    (msg := "an ordinary label passes through untouched")
  TestM.assertEqual (GitHub.percentEncode "a/b?c#d") "a%2Fb%3Fc%23d"
    (msg := "characters that would restructure the URL are encoded")
  TestM.assertEqual (GitHub.percentEncode "priority:高") "priority%3A%E9%AB%98"
    (msg := "a multi-byte character is encoded byte by byte")

/-! ## Reporting the change -/

@[test]
def summary_saysWhatItDid : Test := do
  let change : GitHub.LabelChange := { add := ["t-bug"], remove := ["needs-triage"] }
  let text := change.summary "up/repo#12"
  TestM.assert (text.contains "up/repo#12") "the summary names the issue"
  TestM.assert (text.contains "added t-bug") "and what it added"
  TestM.assert (text.contains "removed needs-triage") "and what it removed"

@[test]
def summary_saysWhenThereWasNothingToDo : Test := do
  let change : GitHub.LabelChange :=
    { alreadyPresent := ["t-bug"], notPresent := ["needs-triage"] }
  let text := change.summary "up/repo#12"
  TestM.assert (text.contains "nothing to change") "a no-op call says so"
  TestM.assert (text.contains "already had t-bug") "and why"
  TestM.assert (text.contains "did not have needs-triage") "for both halves"

end OrchestraTest.LabelIssue
