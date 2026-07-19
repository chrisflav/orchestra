import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Listener

namespace OrchestraTest.LabelDispatcher

/-! Tests for the project-independent dispatcher (`SourceConfig.labelDispatcher`).

    Its issue set and target resolution both need a live taxis instance, so those are covered by
    the opt-in integration tests. What is pure — the config round-trip and the repository-URL
    parsing that turns a `repository` artifact into a `Repository` — is covered here. -/

@[test]
def configRoundTrips : Test := do
  let src := SourceConfig.labelDispatcher "ready" [("implementor", 2), ("reviewer", 1)]
  match FromJson.fromJson? (ToJson.toJson src) (α := SourceConfig) with
  | .error e => TestM.fail s!"label-dispatcher round-trip: {e}"
  | .ok got =>
    match got with
    | .labelDispatcher label caps =>
      TestM.assertEqual label "ready"
      TestM.assertEqual (caps.lookup "implementor") (some 2)
      TestM.assertEqual (caps.lookup "reviewer") (some 1)
    | _ => TestM.fail "round-trip produced a different source type"

@[test]
def configParsesFromJson : Test := do
  let raw := r#"{"type": "label-dispatcher", "label": "agent-ready", "caps": {"implementor": 3}}"#
  match Json.parse raw >>= FromJson.fromJson? (α := SourceConfig) with
  | .error e => TestM.fail s!"label-dispatcher parse: {e}"
  | .ok (.labelDispatcher label caps) =>
    TestM.assertEqual label "agent-ready"
    TestM.assertEqual (caps.lookup "implementor") (some 3)
  | .ok _ => TestM.fail "parsed as the wrong source type"

@[test]
def configRequiresLabel : Test := do
  let raw := r#"{"type": "label-dispatcher", "caps": {"implementor": 1}}"#
  match Json.parse raw >>= FromJson.fromJson? (α := SourceConfig) with
  | .error _ => TestM.assert true
  | .ok _ => TestM.fail "a label-dispatcher without a label should not parse"

/-! ## Repository URL parsing

    The `repository` artifact stores a URL (`Taxis.Plugins.Standard`), but a queue entry needs an
    `owner/repo` pair. -/

private def parsed (url : String) : Option String :=
  (Project.repositoryOfArtifactUrl url).map (·.toString)

@[test]
def parsesGithubUrls : Test := do
  TestM.assertEqual (parsed "https://github.com/leanprover/lean4") (some "leanprover/lean4")
  TestM.assertEqual (parsed "http://github.com/leanprover/lean4") (some "leanprover/lean4")
  TestM.assertEqual (parsed "https://github.com/leanprover/lean4/") (some "leanprover/lean4")

/-- A `.git` suffix and deeper paths both appear in URLs people paste in; neither should end up
    in the repository name. -/
@[test]
def parsesUrlVariants : Test := do
  TestM.assertEqual (parsed "https://github.com/leanprover/lean4.git") (some "leanprover/lean4")
  TestM.assertEqual (parsed "https://github.com/leanprover/lean4/tree/master")
    (some "leanprover/lean4")
  -- Self-hosted forges have the same owner/repo shape.
  TestM.assertEqual (parsed "https://git.example.com/team/tool") (some "team/tool")

@[test]
def rejectsUrlsWithoutOwnerAndRepo : Test := do
  TestM.assert (parsed "https://github.com/leanprover").isNone "owner alone is not a repository"
  TestM.assert (parsed "https://github.com").isNone "host alone is not a repository"
  TestM.assert (parsed "").isNone "empty url is not a repository"

end OrchestraTest.LabelDispatcher
