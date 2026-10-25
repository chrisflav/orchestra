import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra

/-!
# A task's checkout at a given commit

A task may ask for its workspace at a commit rather than on the default branch (`CheckoutSpec`):
a review of a pull request's head, a task scored against a past commit. The daemon fetches and
checks it out before the sandbox exists (`Repo.applyCheckout`), and with `isolate` makes a
repository of its own holding nothing past the commit (`Repo.isolateCheckout`).

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
  IO.FS.writeFile (repo / name) content
  let _ ← git repo #["add", name]
  let _ ← git repo #["commit", "--quiet", "-m", name]
  git repo #["rev-parse", "HEAD"]

/-- An upstream with history `c1 — c2 — c4` on `main` and a pull request `c2 — c3` under
    `refs/pull/7/head`, and a clone of it with the upstream added as `upstream`, the way a slot has
    it. `c4` is what came after: the merged result a scored task must not see. -/
private structure Fixture where
  clone : System.FilePath
  dir   : System.FilePath
  c1 : String
  c2 : String
  c3 : String
  c4 : String

private def withFixture (act : Fixture → IO α) : IO α := do
  let tmp ← IO.Process.output { cmd := "mktemp", args := #["-d"] }
  let dir := System.FilePath.mk tmp.stdout.trimAscii.toString
  try
    let bare := dir / "upstream.git"
    let work := dir / "work"
    let clone := dir / "clone"
    IO.FS.createDirAll work
    let _ ← git dir #["init", "--quiet", "--bare", bare.toString]
    let _ ← git dir #["init", "--quiet", "-b", "main", work.toString]
    let c1 ← commitFile work "a.txt" "one"
    let c2 ← commitFile work "b.txt" "two"
    let _ ← git work #["push", "--quiet", bare.toString, "main:refs/heads/main"]
    let _ ← git work #["checkout", "--quiet", "-b", "pr"]
    let c3 ← commitFile work "pr.txt" "the change under review"
    let _ ← git work #["push", "--quiet", bare.toString, "pr:refs/pull/7/head"]
    let _ ← git work #["checkout", "--quiet", "main"]
    let c4 ← commitFile work "merged.txt" "the answer"
    let _ ← git work #["push", "--quiet", bare.toString, "main:refs/heads/main"]
    let _ ← git dir #["clone", "--quiet", bare.toString, clone.toString]
    let _ ← git clone #["remote", "add", "upstream", bare.toString]
    act { clone, dir, c1, c2, c3, c4 }
  finally
    IO.FS.removeDirAll dir

private def detached (repo : System.FilePath) : IO Bool := do
  return !(← gitOk repo #["symbolic-ref", "-q", "HEAD"])

/-! ## Checking out a commit -/

@[test]
def aPullRequestsHeadIsCheckedOutDetached : Test := do
  let (head, isDetached, file) ← withFixture fun f => do
    Repo.applyCheckout f.clone { commit := f.c3, fetchRef := some "refs/pull/7/head" }
    return (← git f.clone #["rev-parse", "HEAD"], ← detached f.clone,
            ← (f.clone / "pr.txt").pathExists)
  TestM.assert isDetached "HEAD is detached, not a branch"
  TestM.assert file "the pull request's change is in the tree"
  TestM.assert (head.length == 40) s!"HEAD is a commit: {head}"

@[test]
def aCommitOnABranchNeedsNoFetchRef : Test := do
  let (ok, head, c1, merged) ← withFixture fun f => do
    Repo.applyCheckout f.clone { commit := f.c1 }
    return (true, ← git f.clone #["rev-parse", "HEAD"], f.c1, ← (f.clone / "merged.txt").pathExists)
  TestM.assert ok "checked out"
  TestM.assertEqual head c1 (msg := "HEAD is the commit asked for")
  TestM.assert (!merged) "the tree is the commit's, not the default branch's"

@[test]
def aCommitNoUpstreamRefReachesIsRefused : Test := do
  -- `c3` is only under `refs/pull/7/head`. Asked for without that ref, it is not reachable from
  -- any of the upstream's branches, which is the rule without a `fetch_ref`.
  let outcome ← withFixture fun f => do
    try Repo.applyCheckout f.clone { commit := f.c3 }; pure (none : Option String)
    catch e => pure (some s!"{e}")
  match outcome with
  | none => TestM.fail "a commit no upstream branch reaches was checked out"
  | some e => TestM.assert (mentions e "not reachable") e

@[test]
def anUnreachableCommitFailsClearly : Test := do
  let sha := "0123456789abcdef0123456789abcdef01234567"
  let outcome ← withFixture fun f => do
    try Repo.applyCheckout f.clone { commit := sha, fetchRef := some "refs/pull/7/head" }
        pure (none : Option String)
    catch e => pure (some s!"{e}")
  match outcome with
  | none => TestM.fail "a commit the upstream does not have was checked out"
  | some e =>
    TestM.assert (mentions e sha) s!"the failure names the commit: {e}"
    TestM.assert (mentions e "refs/pull/7/head") s!"and where it was looked for: {e}"

@[test]
def aCommitOnAnotherRefThanTheOneNamedIsRefused : Test := do
  -- `c4` is on `main`, not under the pull request's ref: naming that ref does not vouch for it.
  let outcome ← withFixture fun f => do
    try Repo.applyCheckout f.clone { commit := f.c4, fetchRef := some "refs/pull/7/head" }
        pure (none : Option String)
    catch e => pure (some s!"{e}")
  TestM.assert outcome.isSome "a commit the named ref does not reach is refused"

/-! ## Isolation -/

@[test]
def anIsolatedCheckoutHasNothingPastItsCommit : Test := do
  let r ← withFixture fun f => do
    Repo.applyCheckout f.clone { commit := f.c2 }
    let dest := f.dir / "isolated"
    Repo.isolateCheckout f.clone dest f.c2
    let refs ← git dest #["for-each-ref", "--format=%(refname)"]
    let all ← git dest #["rev-list", "--all", "HEAD"]
    let hasLater ← gitOk dest #["cat-file", "-e", f.c4]
    let hasPr ← gitOk dest #["cat-file", "-e", f.c3]
    let reflog ← git dest #["reflog"]
    let logs ← (dest / ".git" / "logs").pathExists
    let head ← git dest #["rev-parse", "HEAD"]
    let sourceRefs ← git f.clone #["for-each-ref", "--format=%(refname)", "refs/orchestra/"]
    return (refs, all.splitOn "\n", hasLater, hasPr, reflog, logs, head, ← detached dest,
            sourceRefs, f.c1, f.c2)
  let (refs, all, hasLater, hasPr, reflog, logs, head, isDetached, sourceRefs, c1, c2) := r
  TestM.assertEqual refs "" (msg := "no branches, tags or remote-tracking refs")
  TestM.assertEqual all [c2, c1] (msg := "`--all` reaches the commit and its parents, nothing else")
  TestM.assert (!hasLater) "the commit that came after is not in the object store at all"
  TestM.assert (!hasPr) "nor is the pull request's"
  TestM.assertEqual reflog "" (msg := "no reflog")
  TestM.assert (!logs) "no logs directory"
  TestM.assertEqual head c2
  TestM.assert isDetached "HEAD is detached"
  TestM.assertEqual sourceRefs "" (msg := "the source is left without the temporary ref")

/-! ## Validation where it is read -/

private def checkoutOf (body : String) : Except String (Option CheckoutSpec) :=
  match Json.parse body with
  | .ok j => parseCheckout? j
  | .error e => .error e

private def sha1 : String := "0123456789abcdef0123456789abcdef01234567"

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