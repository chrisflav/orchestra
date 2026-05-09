import OrchestraTest.TestM
import Orchestra.GitHub

open Lean (Json)
open Orchestra
open Orchestra.GitHub

-- JSON construction helpers

private def mkComment (author path body : String) (line : Option Nat := none) : Json :=
  let lineField := match line with
    | some l => [("line", .num ⟨(l : Int), 0⟩)]
    | none   => []
  Json.mkObj ([
    ("author", Json.mkObj [("login", .str author)]),
    ("path",   .str path),
    ("body",   .str body)
  ] ++ lineField)

private def mkThread (isResolved isOutdated : Bool) (comments : Array Json := #[]) : Json :=
  Json.mkObj [
    ("isResolved", .bool isResolved),
    ("isOutdated", .bool isOutdated),
    ("comments",   Json.mkObj [("nodes", .arr comments)])
  ]

private def mkResponse (threads : Array Json) : Json :=
  Json.mkObj [("data", Json.mkObj [
    ("repository", Json.mkObj [
      ("pullRequest", Json.mkObj [
        ("reviewThreads", Json.mkObj [("nodes", .arr threads)])
      ])
    ])
  ])]

-- Tests

@[test]
def format_noThreads : Test := do
  let result := formatPrReviewThreads (mkResponse #[]) false false
  TestM.assertEqual result "(0 thread(s))" (msg := "empty response")

@[test]
def format_singleUnresolvedThread : Test := do
  let response := mkResponse #[mkThread false false]
  let result := formatPrReviewThreads response false false
  TestM.assert (result.startsWith "--- Thread 1 (unresolved)") (msg := "thread header")
  TestM.assert (result.endsWith "(1 thread(s))")              (msg := "summary")

@[test]
def format_singleResolvedThread : Test := do
  let response := mkResponse #[mkThread true false]
  let result := formatPrReviewThreads response false false
  TestM.assert (result.startsWith "--- Thread 1 (resolved)") (msg := "thread header")

@[test]
def format_outdatedThread : Test := do
  let response := mkResponse #[mkThread false true]
  let result := formatPrReviewThreads response false false
  TestM.assert (result.contains "unresolved, outdated") (msg := "outdated + unresolved tags")

@[test]
def format_commentWithLine : Test := do
  let comment := mkComment "alice" "src/Foo.lean" "Fix this" (some 42)
  let response := mkResponse #[mkThread false false #[comment]]
  let result := formatPrReviewThreads response false false
  TestM.assert (result.contains "@alice — src/Foo.lean:42") (msg := "author, path and line")
  TestM.assert (result.contains "    Fix this")             (msg := "indented body")

@[test]
def format_commentWithoutLine : Test := do
  let comment := mkComment "bob" "README.md" "Note" none
  let response := mkResponse #[mkThread false false #[comment]]
  let result := formatPrReviewThreads response false false
  TestM.assert (result.contains "@bob — README.md\n") (msg := "no line suffix when absent")

@[test]
def format_multiLineBody : Test := do
  let comment := mkComment "carol" "Foo.lean" "line one\nline two" none
  let response := mkResponse #[mkThread false false #[comment]]
  let result := formatPrReviewThreads response false false
  TestM.assert (result.contains "    line one") (msg := "first body line indented")
  TestM.assert (result.contains "    line two") (msg := "second body line indented")

@[test]
def format_unresolvedOnly_filtersResolved : Test := do
  let response := mkResponse #[mkThread true false, mkThread false false]
  let result := formatPrReviewThreads response (unresolvedOnly := true) false
  TestM.assert (result.contains "1 thread(s) shown, 1 filtered") (msg := "resolved thread filtered")
  TestM.assert (!result.contains "--- Thread 2") (msg := "only one thread shown")

@[test]
def format_unresolvedOnly_keepsUnresolved : Test := do
  let response := mkResponse #[mkThread false false]
  let result := formatPrReviewThreads response (unresolvedOnly := true) false
  TestM.assert (result.contains "--- Thread 1") (msg := "unresolved thread kept")
  TestM.assert (result.contains "(1 thread(s))")  (msg := "no filtered count")

@[test]
def format_excludeOutdated_filtersOutdated : Test := do
  let response := mkResponse #[mkThread false true, mkThread false false]
  let result := formatPrReviewThreads response false (excludeOutdated := true)
  TestM.assert (result.contains "1 thread(s) shown, 1 filtered") (msg := "outdated thread filtered")

@[test]
def format_multipleThreadsNumbered : Test := do
  let response := mkResponse #[mkThread true false, mkThread false false, mkThread false true]
  let result := formatPrReviewThreads response false false
  TestM.assert (result.contains "--- Thread 1") (msg := "thread 1 present")
  TestM.assert (result.contains "--- Thread 2") (msg := "thread 2 present")
  TestM.assert (result.contains "--- Thread 3") (msg := "thread 3 present")
  TestM.assert (result.contains "(3 thread(s))") (msg := "summary count")

@[test]
def format_missingAuthor_defaultsToUnknown : Test := do
  let comment := Json.mkObj [("path", .str "Foo.lean"), ("body", .str "x")]
  let response := mkResponse #[mkThread false false #[comment]]
  let result := formatPrReviewThreads response false false
  TestM.assert (result.contains "@unknown") (msg := "missing author defaults to unknown")

@[test]
def format_malformedResponse_treatedAsEmpty : Test := do
  let result := formatPrReviewThreads (Json.mkObj []) false false
  TestM.assertEqual result "(0 thread(s))" (msg := "malformed response gives empty result")
