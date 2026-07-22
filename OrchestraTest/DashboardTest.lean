import OrchestraTest.TestM
import Orchestra

open Orchestra
open Orchestra.Dashboard

namespace OrchestraTest.Dashboard

/-!
# Dashboard static-file serving

`dashboard serve --site` hands out files from a directory in response to paths supplied by
whoever is on the other end of the socket, so where a path is allowed to land is the one part
of it that must not quietly regress. `staticCandidate` is the whole decision, and it is pure.
-/

private def root : System.FilePath := "/srv/site"

@[test]
def staticCandidate_resolvesTheGeneratedLayout : Test := do
  TestM.assertEqual (staticCandidate root "/") (some root)
    (msg := "the root path is the directory itself (the caller then looks for index.html)")
  TestM.assertEqual (staticCandidate root "/dashboard.css") (some (root / "dashboard.css"))
  TestM.assertEqual (staticCandidate root "/queue/") (some (root / "queue"))
    (msg := "a trailing slash is not an empty final segment")
  TestM.assertEqual (staticCandidate root "/queue/index.html")
    (some (root / "queue" / "index.html"))

@[test]
def staticCandidate_rejectsTraversal : Test := do
  -- Rejected, not normalised: `..` never gets the chance to cancel a segment, so no
  -- arrangement of them lands outside the site directory.
  TestM.assertEqual (staticCandidate root "/../etc/passwd") none (msg := "leading ..")
  TestM.assertEqual (staticCandidate root "/queue/../../etc/passwd") none
    (msg := ".. after a legitimate segment")
  TestM.assertEqual (staticCandidate root "/./dashboard.css") none (msg := "single dot")
  TestM.assertEqual (staticCandidate root "/a\\..\\b") none
    (msg := "a backslash, in case the separator is ever read as one")

@[test]
def staticCandidate_doesNotDecodeEscapes : Test := do
  -- Percent-decoding is what would turn `%2e%2e` into a second spelling of `..`; not decoding
  -- leaves it an ordinary filename, which simply does not exist in the generated site.
  TestM.assertEqual (staticCandidate root "/%2e%2e/etc/passwd")
    (some (root / "%2e%2e" / "etc" / "passwd"))
    (msg := "an undecoded escape stays a (nonexistent) name, never a traversal")

end OrchestraTest.Dashboard
