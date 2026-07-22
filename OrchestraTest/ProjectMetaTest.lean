import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Project

namespace OrchestraTest.ProjectMeta

/-! Tests for the `orchestra-meta` description codec — the wire format carrying everything taxis
    has no native field for (`RepoTarget`, `ReviewerTemplate`). Unlike the rest of the
    taxis-backed subsystem these are pure functions, so they run without a taxis instance. -/

private def target : RepoTarget :=
  { repo := { owner := "org", name := "repo" }, branch := "main" }

private def targetField : List (String × Json) :=
  [("target", ToJson.toJson target)]

/-- `RepoTarget` has `BEq` but not `DecidableEq`, so these go through `TestM.assert` rather than
    `assertEqual`. -/
private def decodedTarget (j : Json) : Option RepoTarget :=
  j.getObjValAs? RepoTarget "target" |>.toOption

@[test]
def encodeWithoutFieldsLeavesDescriptionUntouched : Test := do
  TestM.assertEqual (encodeMeta "just prose" []) "just prose"
  let (human, metaJson) := decodeMeta "just prose"
  TestM.assertEqual human "just prose"
  TestM.assert (decodedTarget metaJson).isNone "no metadata expected"

@[test]
def roundTripsTargetAndHidesItFromHumanText : Test := do
  let raw := encodeMeta "implement the thing" targetField
  TestM.assert (raw.startsWith "implement the thing") "human text must come first"
  let (human, metaJson) := decodeMeta raw
  TestM.assertEqual human "implement the thing"
  TestM.assert (decodedTarget metaJson == some target) "target must round-trip"

@[test]
def roundTripsEmptyHumanText : Test := do
  let (human, metaJson) := decodeMeta (encodeMeta "" targetField)
  TestM.assertEqual human ""
  TestM.assert (decodedTarget metaJson == some target) "target must round-trip"

/-- The regression this codec is most likely to hit in practice: an issue whose *prose* discusses
    the marker (an issue about this very format). `decodeMeta` must split on the last fence, not
    the first, or everything after the mention is silently swallowed as metadata. -/
@[test]
def humanTextMayContainTheMarker : Test := do
  let prose := "metadata goes after a\n```orchestra-meta\n{\"target\": \"fake\"}\n``` block"
  let (human, metaJson) := decodeMeta (encodeMeta prose targetField)
  TestM.assertEqual human prose
  TestM.assert (decodedTarget metaJson == some target) "target must round-trip"

/-- A corrupt blob must leave the issue readable, and must not compound: re-encoding what
    `decodeMeta` handed back and decoding again yields the same text plus the fresh metadata. -/
@[test]
def corruptMetadataIsToleratedAndStable : Test := do
  let corrupt := "some prose" ++ "\n```orchestra-meta\n" ++ "{not json" ++ "\n```"
  let (human, metaJson) := decodeMeta corrupt
  TestM.assertEqual human corrupt "corrupt metadata is surfaced as human text, not dropped"
  TestM.assert (decodedTarget metaJson).isNone "corrupt metadata yields no fields"
  let (human2, metaJson2) := decodeMeta (encodeMeta human targetField)
  TestM.assertEqual human2 corrupt
  TestM.assert (decodedTarget metaJson2 == some target) "target must round-trip"

end OrchestraTest.ProjectMeta
