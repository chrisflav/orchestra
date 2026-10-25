import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra

/-!
# A task's checkout at a given commit

A task may ask for its workspace at a commit rather than on the default branch (`CheckoutSpec`):
a review of a pull request's head, a task scored against a past commit. The daemon builds it in a
repository only it touches, fetched from the upstream for this task (`Repo.prepareCheckout`), and
reads the task's configuration and hooks from the upstream default branch's `.orchestra/`, never
from the named commit's.

The git half runs against a bare repository on this disk standing in for the upstream, so none of
it needs the network. The rest is the field's validation where it is read, and the entry carrying
it through retry and resume.
-/

namespace OrchestraTest.Checkout

private def mentions (msg needle : String) : Bool := (msg.splitOn needle).length > 1

/-- `git` in `cwd`, with the identity and signing settings a commit needs pinned, so a developer's
    own `~/.gitconfig` cannot change what the fixture builds. -/
private def git (cwd : System.FilePath) (args : Array String) : IO String := do
  let out ← IO.Process.output
    { cmd := "git"
      args := #["-c", "user.name=t", "-c", "user.email=t@example.org", "-c", "commit.gpgsign=false",
                "-c", "tag.gpgsign=false"] ++ args
      cwd }
  if out.exitCode != 0 then
    throw (.userError s!"git {args} failed: {out.stderr}")
  return out.stdout.trimAscii.toString

private def gitOk (cwd : System.FilePath) (args : Array String) : IO Bool := do
  try let _ ← git cwd args; pure true catch _ => pure false

private def commitFile (repo : System.FilePath) (name content : String) : IO String := do
  if let some parent := (repo / name).parent then IO.FS.createDirAll parent
  IO.FS.writeFile (repo / name) content
  let _ ← git repo #["add", name]
  let _ ← git repo #["commit", "--quiet", "-m", name]
  git repo #["rev-parse", "HEAD"]

/-- An upstream (`bare`) with history `c1 — c2 — c4` on `main` and a pull request `c2 — c3` under
    `refs/pull/7/head`. `c1` carries the repository's own `.orchestra/`; `c3` rewrites it — a hook
    that would run outside the sandbox, and an image to run in. `c4` is what came after: the merged
    result a scored task must not see. `slot` is a clone of it, standing in for the pooled slot an
    agent may have written into, with a commit `x` only it has. -/
private structure Fixture where
  bare : System.FilePath
  slot : System.FilePath
  dir  : System.FilePath
  c1 : String
  c2 : String
  c3 : String
  c4 : String
  x  : String
  /-- `c2` and every commit before it, newest first: all an isolated checkout of `c2` may hold. -/
  c2History : List String

private def goodConfig : String := "{\"execution\": {\"image\": \"ghcr.io/acme/good:1\"}}"
private def evilConfig : String := "{\"execution\": {\"image\": \"evil.example/pwn:1\"}}"

private def fork : Repository := { owner := "acme", name := "widgets" }

private def withFixture (act : Fixture → IO α) : IO α :=
  Orchestra.withTempData "checkout-git" do
    let dir ← Dirs.dataBase
    let bare := dir / "upstream.git"
    let work := dir / "work"
    let slot := dir / "slot"
    IO.FS.createDirAll work
    let _ ← git dir #["init", "--quiet", "--bare", "-b", "main", bare.toString]
    let _ ← git dir #["init", "--quiet", "-b", "main", work.toString]
    let _ ← commitFile work ".orchestra/config.json" goodConfig
    let c1 ← commitFile work ".orchestra/init.sh" "echo good\n"
    let c2 ← commitFile work "b.txt" "two"
    let _ ← git work #["push", "--quiet", bare.toString, "main:refs/heads/main"]
    let _ ← git work #["checkout", "--quiet", "-b", "pr"]
    let _ ← commitFile work ".orchestra/config.json" evilConfig
    let _ ← commitFile work ".orchestra/init.sh" "echo pwned\n"
    let c3 ← commitFile work "pr.txt" "the change under review"
    let _ ← git work #["push", "--quiet", bare.toString, "pr:refs/pull/7/head"]
    let _ ← git work #["checkout", "--quiet", "main"]
    let c4 ← commitFile work "merged.txt" "the answer"
    let _ ← git work #["push", "--quiet", bare.toString, "main:refs/heads/main"]
    let _ ← git dir #["clone", "--quiet", bare.toString, slot.toString]
    let x ← commitFile slot "fork-only.txt" "only the slot has this"
    let c2History := (← git work #["rev-list", c2]).splitOn "\n"
    act { bare, slot, dir, c1, c2, c3, c4, x, c2History }

private def detached (repo : System.FilePath) : IO Bool := do
  return !(← gitOk repo #["symbolic-ref", "-q", "HEAD"])

private def prepare (f : Fixture) (spec : CheckoutSpec) (taskId := "t-1") :
    IO (System.FilePath × System.FilePath) :=
  Repo.prepareCheckout fork fork taskId spec (url := some f.bare.toString)

private def refusal (f : Fixture) (spec : CheckoutSpec) : IO (Option String) := do
  try let _ ← prepare f spec; pure none catch e => pure (some s!"{e}")

private def sha1 : String := "0123456789abcdef0123456789abcdef01234567"

/-! ## Checking out a commit -/

@[test]
def aPullRequestsHeadIsCheckedOutDetached : Test := do
  let r ← withFixture fun f => do
    let (tree, _) ← prepare f { commit := f.c3, fetchRef := some "refs/pull/7/head" }
    return (← git tree #["rev-parse", "HEAD"], f.c3, ← detached tree,
            ← (tree / "pr.txt").pathExists, ← gitOk tree #["rev-parse", "refs/remotes/upstream/main"],
            ← git tree #["for-each-ref", "--format=%(refname)", "refs/orchestra/"],
            ← (← Repo.checkoutSourcePath fork "t-1").pathExists)
  let (head, c3, isDetached, file, upstreamMain, orchestraRefs, sourceLeft) := r
  TestM.assertEqual head c3 (msg := "HEAD is the pull request's head")
  TestM.assert isDetached "on a detached HEAD"
  TestM.assert file "the pull request's change is in the tree"
  TestM.assert upstreamMain "the upstream's branches are there to compare against"
  TestM.assertEqual orchestraRefs "" (msg := "no temporary ref is left in the tree")
  TestM.assert (!sourceLeft) "the daemon's source repository is gone once the tree is built"

/-- The commit's own `.orchestra/` is what the task reviews, not what it runs: the hooks and the
    image come from the upstream's default branch, copied where only the daemon reads them. -/
@[test]
def aCommitsOwnOrchestraDirectoryIsIgnored : Test := do
  let r ← withFixture fun f => do
    let (tree, config) ← prepare f { commit := f.c3, fetchRef := some "refs/pull/7/head" }
    let used ← RepoConfig.loadRepoConfig tree (configRoot := some config)
    let theCommits ← RepoConfig.loadRepoConfig tree
    return (← IO.FS.readFile (config / ".orchestra" / "init.sh"), used.image, theCommits.image)
  let (initSh, used, theCommits) := r
  TestM.assertEqual initSh "echo good\n" (msg := "init.sh is the default branch's")
  TestM.assertEqual used (some "ghcr.io/acme/good:1") (msg := "and so is execution.image")
  TestM.assertEqual theCommits (some "evil.example/pwn:1")
    (msg := "while the tree itself is the commit, as the review needs")

@[test]
def aCommitOnABranchNeedsNoFetchRef : Test := do
  let r ← withFixture fun f => do
    let (tree, _) ← prepare f { commit := f.c1 }
    return (← git tree #["rev-parse", "HEAD"], f.c1, ← (tree / "merged.txt").pathExists)
  let (head, c1, merged) := r
  TestM.assertEqual head c1 (msg := "HEAD is the commit asked for")
  TestM.assert (!merged) "the tree is the commit's, not the default branch's"

@[test]
def aCommitNoUpstreamRefReachesIsRefused : Test := do
  -- `c3` is only under `refs/pull/7/head`. Asked for without that ref, it is not reachable from
  -- any of the upstream's branches, which is the rule without a `fetch_ref`.
  let (outcome, left) ← withFixture fun f => do
    let o ← refusal f { commit := f.c3 }
    return (o, ← (← Repo.taskCheckoutPath fork "t-1").pathExists)
  match outcome with
  | none => TestM.fail "a commit no upstream branch reaches was checked out"
  | some e => TestM.assert (mentions e "not reachable") e
  TestM.assert (!left) "a refused checkout leaves nothing behind"

@[test]
def anUnreachableCommitFailsClearly : Test := do
  let sha := "0123456789abcdef0123456789abcdef01234567"
  let outcome ← withFixture fun f => refusal f { commit := sha, fetchRef := some "refs/pull/7/head" }
  match outcome with
  | none => TestM.fail "a commit the upstream does not have was checked out"
  | some e =>
    TestM.assert (mentions e sha) s!"the failure names the commit: {e}"
    TestM.assert (mentions e "refs/pull/7/head") s!"and where it was looked for: {e}"

@[test]
def aCommitOnAnotherRefThanTheOneNamedIsRefused : Test := do
  -- `c4` is on `main`, not under the pull request's ref: naming that ref does not vouch for it.
  let outcome ← withFixture fun f => refusal f { commit := f.c4, fetchRef := some "refs/pull/7/head" }
  TestM.assert outcome.isSome "a commit the named ref does not reach is refused"

/-! ## Nothing an agent wrote counts

The repro the review found: a ref planted under `refs/remotes/upstream/` in the slot made a
fork-only commit pass the reachability check, and a `refs/replace/` or an `info/grafts` in the
repository a pack was built from added later commits to an isolated checkout. The checkout is now
built from a repository fetched for the task alone, with replacements and grafts switched off on
both sides of every transfer. -/

@[test]
def aRefPlantedInTheSlotDoesNotVouchForACommit : Test := do
  let outcome ← withFixture fun f => do
    let _ ← git f.slot #["update-ref", "refs/remotes/upstream/evil", f.x]
    let _ ← git f.slot #["update-ref", "refs/remotes/origin/main", f.x]
    refusal f { commit := f.x }
  match outcome with
  | none => TestM.fail "a commit only the slot has was checked out on the strength of a planted ref"
  | some e => TestM.assert (mentions e "not reachable") e

@[test]
def replacementsAndGraftsAddNothingToAnIsolatedCheckout : Test := do
  let r ← withFixture fun f => do
    -- A replacement of `c2` claiming `c4` as a second parent in the repository the checkout is
    -- fetched from, and the same replacement and a graft in the slot, which plays no part at all.
    let tree ← git f.bare #["rev-parse", s!"{f.c2}^\{tree}"]
    let fake ← git f.bare #["commit-tree", "-p", f.c1, "-p", f.c4, "-m", "fake", tree]
    let _ ← git f.bare #["replace", "-f", f.c2, fake]
    let _ ← git f.slot #["fetch", "--quiet", f.bare.toString, "refs/replace/*:refs/replace/*"]
    IO.FS.writeFile (f.slot / ".git" / "info" / "grafts") s!"{f.c2} {f.c1} {f.c4}\n"
    let (dest, _) ← prepare f { commit := f.c2, isolate := true }
    return (← gitOk dest #["cat-file", "-e", f.c4], ← git dest #["rev-list", "--all", "HEAD"],
            f.c2History)
  let (hasLater, all, history) := r
  TestM.assert (!hasLater) "the later commit is not in the isolated object store"
  TestM.assertEqual (all.splitOn "\n") history
    (msg := "the commit and its real history, nothing else")

/-! ## Isolation -/

@[test]
def anIsolatedCheckoutHasNothingPastItsCommit : Test := do
  let r ← withFixture fun f => do
    let (dest, _) ← prepare f { commit := f.c2, isolate := true }
    let refs ← git dest #["for-each-ref", "--format=%(refname)"]
    let all ← git dest #["rev-list", "--all", "HEAD"]
    let hasLater ← gitOk dest #["cat-file", "-e", f.c4]
    let hasPr ← gitOk dest #["cat-file", "-e", f.c3]
    let reflog ← git dest #["reflog"]
    let logs ← (dest / ".git" / "logs").pathExists
    let remotes ← git dest #["remote"]
    return (refs, all.splitOn "\n", hasLater, hasPr, reflog, logs, ← git dest #["rev-parse", "HEAD"],
            ← detached dest, remotes, f.c2History, f.c2)
  let (refs, all, hasLater, hasPr, reflog, logs, head, isDetached, remotes, history, c2) := r
  TestM.assertEqual refs "" (msg := "no branches, tags or remote-tracking refs")
  TestM.assertEqual all history (msg := "`--all` reaches the commit and its history, nothing else")
  TestM.assert (!hasLater) "the commit that came after is not in the object store at all"
  TestM.assert (!hasPr) "nor is the pull request's"
  TestM.assertEqual reflog "" (msg := "no reflog")
  TestM.assert (!logs) "no logs directory"
  TestM.assertEqual head c2
  TestM.assert isDetached "HEAD is detached"
  TestM.assertEqual remotes "" (msg := "and no remotes")

/-- A symlink in the default branch's `.orchestra/` that leads outside it fails the checkout: the
    hooks would otherwise run a file the configuration copy does not hold. -/
@[test]
def aSymlinkOutOfTheConfigurationIsRefused : Test := do
  let outcome ← withFixture fun f => do
    let work := f.dir / "work"
    let _ ← git work #["checkout", "--quiet", "main"]
    IO.FS.writeFile (work / "evil.sh") "echo outside\n"
    let _ ← git work #["rm", "--quiet", ".orchestra/init.sh"]
    let _ ← IO.Process.output { cmd := "ln", args := #["-s", "../evil.sh", ".orchestra/init.sh"]
                                cwd := work }
    let _ ← git work #["add", "evil.sh", ".orchestra/init.sh"]
    let _ ← git work #["commit", "--quiet", "-m", "link"]
    let _ ← git work #["push", "--quiet", f.bare.toString, "main:refs/heads/main"]
    refusal f { commit := f.c1 }
  match outcome with
  | none => TestM.fail "a configuration with a symlink out of it was accepted"
  | some e => TestM.assert (mentions e "symlink") e

/-- For a continuation whose backend hands back the predecessor's workspace, no tree is built, but
    the commit is still checked and the configuration still copied. -/
@[test]
def withoutATreeTheConfigurationIsStillCopied : Test := do
  let r ← withFixture fun f => do
    let spec : CheckoutSpec := { commit := f.c3, fetchRef := some "refs/pull/7/head" }
    let (tree, config) ← Repo.prepareCheckout fork fork "t-1" spec
      (url := some f.bare.toString) (buildTree := false)
    return ((← tree.readDir).size, ← (config / ".orchestra" / "init.sh").pathExists)
  let (entries, hasConfig) := r
  TestM.assertEqual entries 0 (msg := "an empty directory stands where the tree would")
  TestM.assert hasConfig "the configuration copy is made"

@[test]
def clearingRefsNeverReachesAnEnclosingRepository : Test := do
  let outer ← withFixture fun f => do
    -- The slot is a repository; a plain directory inside it is not one of its own.
    let _ ← git f.slot #["update-ref", "refs/orchestra/keep", f.x]
    let inner := f.slot / "plain"
    IO.FS.createDirAll inner
    Repo.clearOrchestraRefs inner
    git f.slot #["for-each-ref", "--format=%(refname)", "refs/orchestra/"]
  TestM.assertEqual outer "refs/orchestra/keep"
    (msg := "a directory that is no repository is left alone, and so is the one around it")

/-! ## Where a checkout may run -/

private def exec (backend : String) (allow : Bool := false) : ExecutionConfig :=
  { backend, allowCheckoutHooksOnHost := allow }

@[test]
def aCheckoutIsRefusedWhereHooksRunOnTheHost : Test := do
  let c : Option CheckoutSpec := some { commit := sha1 }
  let iso : Option CheckoutSpec := some { commit := sha1, isolate := true }
  TestM.assert ((exec "landrun").checkoutRefusal? c none).isSome "landrun runs hooks on the host"
  TestM.assert ((exec "local").checkoutRefusal? c none).isSome "and so does local"
  TestM.assert ((exec "kubernetes").checkoutRefusal? c none).isNone "a pod does not"
  TestM.assert ((exec "landrun" (allow := true)).checkoutRefusal? c none).isNone
    "unless the operator allows it"
  TestM.assert ((exec "local" (allow := true)).checkoutRefusal? iso none).isSome
    "isolation is refused on local whatever is allowed"
  TestM.assert ((exec "kubernetes").checkoutRefusal? c (some "merger")).isSome "nor for the merger"
  TestM.assert ((exec "kubernetes").checkoutRefusal? c (some "triage")).isSome "nor for triage"
  TestM.assert ((exec "landrun").checkoutRefusal? none none).isNone "no checkout, no refusal"
  match (FromJson.fromJson? (Json.mkObj [("backend", .str "landrun"),
      ("allow_checkout_hooks_on_host", .bool true)]) : Except String ExecutionConfig) with
  | .error e => TestM.fail e
  | .ok x => TestM.assert x.allowCheckoutHooksOnHost "the setting is read from config.json"

@[test]
def anIsolatedTaskGetsItsOwnScratchDirectory : Test := do
  let paths := (Sandbox.grantsFor {} {} (System.FilePath.mk "/work/t") true #[] #[]
    "/data/repos/acme/widgets-tasks/t.tmp").map (·.path)
  TestM.assert (paths.contains "/data/repos/acme/widgets-tasks/t.tmp") "its own scratch directory"
  TestM.assert (!paths.contains "/tmp") "and not the shared /tmp"

@[test]
def leftoverOrchestraRefsAreClearedAtStartup : Test := do
  let refs ← withFixture fun f => do
    let _ ← git f.slot #["update-ref", "refs/orchestra/checkout", f.x]
    let _ ← git f.slot #["update-ref", "refs/orchestra/isolate", f.x]
    Repo.clearOrchestraRefs f.slot
    git f.slot #["for-each-ref", "--format=%(refname)", "refs/orchestra/"]
  TestM.assertEqual refs "" (msg := "every refs/orchestra/* is gone")

/-! ## Validation where it is read -/

private def checkoutOf (body : String) : Except String (Option CheckoutSpec) :=
  match Json.parse body with
  | .ok j => parseCheckout? j
  | .error e => .error e


@[test]
def aCheckoutIsReadStrictly : Test := do
  match checkoutOf s!"\{\"checkout\": \{\"commit\": \"{sha1}\", \"fetch_ref\": \"refs/pull/7/head\", \"isolate\": true}}" with
  | .ok (some c) =>
    TestM.assertEqual c.commit sha1
    TestM.assertEqual c.fetchRef (some "refs/pull/7/head")
    TestM.assert c.isolate "isolate"
  | other => TestM.fail s!"a well-formed checkout should parse: {repr other}"
  let refused (body : String) (needle : String) : Test := do
    match checkoutOf body with
    | .ok c => TestM.fail s!"{body} should be refused, got {repr c}"
    | .error e => TestM.assert (mentions e needle) s!"{body}: {e}"
  refused "{\"checkout\": {\"commit\": \"abc123\"}}" "full commit name"
  refused s!"\{\"checkout\": \{\"commit\": \"{sha1.toUpper}\"}}" "full commit name"
  refused "{\"checkout\": {\"fetch_ref\": \"refs/pull/7/head\"}}" "no 'commit'"
  refused s!"\{\"checkout\": \{\"commit\": \"{sha1}\", \"fetch_ref\": \"refs/tags/v1\"}}" "refs/pull"
  refused s!"\{\"checkout\": \{\"commit\": \"{sha1}\", \"fetch_ref\": \"refs/pull/x/head\"}}" "refs/pull"
  refused s!"\{\"checkout\": \{\"commit\": \"{sha1}\", \"fetch_ref\": \"refs/heads/../x\"}}" "refs/pull"
  refused s!"\{\"checkout\": \{\"commit\": \"{sha1}\", \"fetch_ref\": \"refs/heads/-x --upload-pack=y\"}}" "refs/pull"
  refused s!"\{\"checkout\": \{\"commit\": \"{sha1}\", \"fetch_ref\": \"refs/remotes/upstream/main\"}}" "refs/pull"
  refused s!"\{\"checkout\": \{\"commit\": \"{sha1}\", \"isolate\": \"yes\"}}" "isolate"
  refused s!"\{\"checkout\": \{\"commit\": \"{sha1}\", \"branch\": \"main\"}}" "branch"
  refused "{\"checkout\": \"main\"}" "must be an object"
  TestM.assert ((fetchRefError? "refs/heads/feature/x-1").isNone) "a nested branch is fine"
  TestM.assert ((fetchRefError? "refs/pull/12/merge").isNone) "and a merge ref"

@[test]
def aTaskFileWithACheckoutNeedsARepository : Test := do
  let body := s!"\{\"prompt\": \"review\", \"checkout\": \{\"commit\": \"{sha1}\"}}"
  match Json.parse body >>= FromJson.fromJson? (α := Task) with
  | .ok _ => TestM.fail "a checkout on a task with no repository should be refused"
  | .error e => TestM.assert (mentions e "checkout") e
  let withRepo := s!"\{\"prompt\": \"review\", \"upstream\": \"acme/widgets\", \
\"fork\": \"acme/widgets\", \"checkout\": \{\"commit\": \"{sha1}\"}}"
  match Json.parse withRepo >>= FromJson.fromJson? (α := Task) with
  | .error e => TestM.fail s!"a task file with a checkout should parse: {e}"
  | .ok t => TestM.assertEqual (t.ioTask.checkout.map (·.commit)) (some sha1)

@[test]
def queueTaskReadsACheckout : Test := do
  let args := Json.mkObj [("prompt", "review"),
    ("checkout", Json.mkObj [("commit", .str sha1), ("fetch_ref", .str "refs/pull/7/head")])]
  match Project.Tools.tryParseToolCall "queue_task" args with
  | some (.ok (.queueTask r)) => TestM.assertEqual (r.checkout.map (·.commit)) (some sha1)
  | other => TestM.fail s!"unexpected parse: {repr other}"
  let bad := Json.mkObj [("prompt", "review"), ("checkout", Json.mkObj [("commit", .str "HEAD")])]
  match Project.Tools.tryParseToolCall "queue_task" bad with
  | some (.error _) => TestM.assert true
  | other => TestM.fail s!"a malformed commit should be reported, got: {repr other}"
  -- And nothing to check out for a repository-independent queued task.
  match ({} : SpawnPolicy).resolve {} { prompt := "x", checkout := some { commit := sha1 } } with
  | .ok _ => TestM.fail "a checkout with no repository should be refused"
  | .error e => TestM.assert (mentions e "checkout") e

@[test]
def anApiClientRequestsCheckoutIsValidated : Test := do
  let req (body : String) := match Json.parse body with
    | .ok j => ApiClient.parseRequest j
    | .error e => .error e
  match req s!"\{\"prompt\": \"review\", \"upstream\": \"acme/widgets\", \
\"checkout\": \{\"commit\": \"{sha1}\", \"fetch_ref\": \"refs/pull/7/head\"}}" with
  | .error e => TestM.fail s!"a request with a checkout should parse: {e}"
  | .ok r => TestM.assertEqual (r.spawn.checkout.map (·.fetchRef)) (some (some "refs/pull/7/head"))
  match req s!"\{\"prompt\": \"review\", \"checkout\": \{\"commit\": \"{sha1}\"}}" with
  | .ok _ => TestM.fail "a checkout names a commit of the upstream; with none it names nothing"
  | .error e => TestM.assert (mentions e "upstream") e
  match req "{\"prompt\": \"review\", \"upstream\": \"acme/widgets\", \"checkout\": {\"commit\": \"main\"}}" with
  | .ok _ => TestM.fail "a branch name is not a commit"
  | .error e => TestM.assert (mentions e "full commit name") e
  match req s!"\{\"prompt\": \"review\", \"upstream\": \"acme/widgets\", \
\"checkout\": \{\"commit\": \"{sha1}\", \"fetch_ref\": \"refs/remotes/origin/main\"}}" with
  | .ok _ => TestM.fail "a ref outside refs/pull/ and refs/heads/ should be refused"
  | .error e => TestM.assert (mentions e "refs/pull") e

/-! ## The entry carries it -/

private def checkedOut : Queue.QueueEntry :=
  { id := "q-1", createdAt := "2026-10-25T08:00:00Z", status := .unfinished
  , repo := none, prompt := "review", taskId := some "t-1"
  , checkout := some { commit := sha1, fetchRef := some "refs/pull/7/head", isolate := true } }

@[test]
def anEntryKeepsItsCheckoutThroughJsonRowsRetryAndResume : Test := do
  let expected := checkedOut.checkout
  match (FromJson.fromJson? (ToJson.toJson checkedOut) : Except String Queue.QueueEntry) with
  | .error e => TestM.fail s!"the entry should read back: {e}"
  | .ok back => TestM.assert (back.checkout == expected) "through its JSON"
  match Queue.QueueEntry.ofRow? checkedOut.toRow with
  | .error e => TestM.fail s!"the row should read back: {e}"
  | .ok back => TestM.assert (back.checkout == expected) "through its row"
  let retried := Queue.retryEntryFor checkedOut (some "t-1") "q-2" "2026-10-25T09:00:00Z"
  TestM.assert (retried.checkout == expected) "through a retry"
  let record : TaskStore.TaskRecord :=
    { id := "t-1", createdAt := "2026-10-25T08:00:00Z", status := .unfinished
    , sessionId := some "s", repo := none, prompt := "review" }
  match Queue.resumeEntryFor checkedOut record { hasSession := true } "q-3" "2026-10-25T09:00:00Z" with
  | .error e => TestM.fail s!"the run should be resumed: {e}"
  | .ok e => TestM.assert (e.checkout == expected) "through a restart resume"

@[test]
def theStoreKeepsTheCheckoutAndAContinuationTakesIt : Test := do
  let (stored, carried) ← Orchestra.withTempData "checkout" do
    Queue.saveEntry { checkedOut with status := .done }
    let stored := (← Queue.loadEntry "q-1").bind (·.checkout)
    let carried := (← Queue.carriedForContinuation "t-1").2
    return (stored, carried)
  TestM.assert (stored == checkedOut.checkout) "the column round-trips"
  TestM.assert (carried == checkedOut.checkout) "a continuation built from the record takes it"

@[test]
def theCheckoutMigrationIsTheTwelfth : Test := do
  let names := Store.migrations.map (·.name)
  TestM.assertEqual ((names.drop 10).take 2) ["0011_execution_image", "0012_checkout"]

end OrchestraTest.Checkout