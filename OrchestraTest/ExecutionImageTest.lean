import OrchestraTest.TestM
import Orchestra

open Lean (Json FromJson ToJson)
open Orchestra
open Orchestra.Exec

/-!
# The image a task runs in

A task may name the image it runs in (`execution_image`), because what kind of work it is decides
what has to be installed as much as its repository does. What is pinned here is each place a task
is described carrying the field through to the entry, the policy bound on the two paths an
untrusted party drives (`queue_task` and an API client's request), the entry keeping its image
across a retry and a restart resume, the store's column, and the order `Kubernetes.imageFor`
settles the four sources in — the task's own first.
-/

namespace OrchestraTest.ExecutionImage

private def reviewerImage : String := "ghcr.io/acme/reviewer:1.4"

private def mentions (msg needle : String) : Bool := (msg.splitOn needle).length > 1

/-! ## What counts as an image reference -/

@[test]
def anImageReferenceIsCheckedWhereItIsRead : Test := do
  TestM.assert (imageRefError? reviewerImage).isNone "an ordinary reference is fine"
  TestM.assert (imageRefError? "registry.internal:5000/team/img@sha256:abc123").isNone
    "so is a port and a digest"
  TestM.assert (imageRefError? "").isSome "an empty one is not"
  TestM.assert (imageRefError? "ghcr.io/acme/img:1 --privileged").isSome "nor one with a space"
  TestM.assert (imageRefError? "img\n").isSome "nor one with a newline"
  TestM.assert (imageRefError? ("".pushn 'a' 513)).isSome
    "nor one longer than any registry would take"

/-! ## Every place a task is described -/

@[test]
def aTaskFileCarriesItsImage : Test := do
  let raw := "{\"prompt\": \"review\", \"execution_image\": \"" ++ reviewerImage ++ "\"}"
  match Json.parse raw >>= FromJson.fromJson? (α := Task) with
  | .error e => TestM.fail s!"a task file with an image should parse: {e}"
  | .ok t => TestM.assertEqual t.ioTask.executionImage (some reviewerImage)
  match Json.parse "{\"prompt\": \"review\", \"execution_image\": \"no good\"}"
      >>= FromJson.fromJson? (α := Task) with
  | .ok _ => TestM.fail "a malformed image in a task file should be refused, not read as absent"
  | .error e => TestM.assert (mentions e "execution_image") e
  match Json.parse "{\"prompt\": \"review\", \"execution_image\": 4}"
      >>= FromJson.fromJson? (α := Task) with
  | .ok _ => TestM.fail "an image that is not a string should be refused"
  | .error e => TestM.assert (mentions e "execution_image") e

@[test]
def aRoleCarriesItsImage : Test := do
  let raw := "{\"name\": \"reviewer\", \"permissions\": [\"comment\"], \
\"prompt_template\": \"review\", \"execution_image\": \"" ++ reviewerImage ++ "\"}"
  match Project.validateRole "reviewer" raw with
  | .error e => TestM.fail s!"a role with an image should validate: {e}"
  | .ok role =>
    TestM.assertEqual role.executionImage (some reviewerImage)
    match (FromJson.fromJson? (ToJson.toJson role) : Except String Project.Role) with
    | .error e => TestM.fail s!"the role should read back: {e}"
    | .ok back => TestM.assertEqual back.executionImage (some reviewerImage)
  let bad := "{\"name\": \"reviewer\", \"permissions\": [], \"prompt_template\": \"r\", \
\"execution_image\": \"a b\"}"
  match Project.validateRole "reviewer" bad with
  | .ok _ => TestM.fail "a role naming a malformed image should be refused"
  | .error e => TestM.assert (mentions e "execution_image") e

@[test]
def aListenerActionCarriesItsImageOntoItsEntry : Test := do
  let raw := "{\"prompt_template\": \"go\", \"execution_image\": \"" ++ reviewerImage ++ "\"}"
  match Json.parse raw >>= FromJson.fromJson? (α := Listener.ActionConfig) with
  | .error e => TestM.fail s!"an action with an image should parse: {e}"
  | .ok a =>
    TestM.assertEqual a.executionImage (some reviewerImage)
    let entry ← Listener.buildQueueEntry a [("upstream", "acme/widgets"), ("fork", "acme/widgets")]
    TestM.assertEqual entry.executionImage (some reviewerImage)
      (msg := "the entry the listener queues runs in it")
  match Json.parse "{\"prompt_template\": \"go\", \"execution_image\": \"\"}"
      >>= FromJson.fromJson? (α := Listener.ActionConfig) with
  | .ok _ => TestM.fail "an empty image on an action should be refused"
  | .error e => TestM.assert (mentions e "execution_image") e

private def workflowYaml (stepImage : String) : String := s!"name: review
upstream: acme/widgets
fork: acme/widgets

steps:
  review:
    task:
      prompt: \"review\"
      execution-image: {stepImage}
  summarise:
    task:
      prompt: \"summarise\"
"

private def stepImages (prog : WorkflowProgram) : List (Option String) :=
  prog.steps.map fun s => match s.action with
    | .task t => t.executionImage
    | _ => none

@[test]
def aWorkflowStepCarriesItsImage : Test := do
  match Workflow.WorkflowProgram.parseYaml (workflowYaml reviewerImage) with
  | .error e => TestM.fail s!"a step with an image should parse: {e}"
  | .ok prog =>
    TestM.assertEqual (stepImages prog) [some reviewerImage, none]
    -- A listener's image holds of the steps that name none, and never over a step's own.
    let fromListener := prog.withListenerImage (some "ghcr.io/acme/other:2")
    TestM.assertEqual (stepImages fromListener) [some reviewerImage, some "ghcr.io/acme/other:2"]
  match Workflow.WorkflowProgram.parseYaml (workflowYaml "\"a b\"") with
  | .ok _ => TestM.fail "a step naming a malformed image should be refused"
  | .error e => TestM.assert (mentions e "execution-image") e

@[test]
def aConcertStepKeepsItsImageThroughItsJson : Test := do
  let t : IOTask .unit .unit := { repo := none, prompt := "review", executionImage := some reviewerImage }
  match (FromJson.fromJson? (ToJson.toJson t) : Except String (IOTask .unit .unit)) with
  | .error e => TestM.fail s!"the step should read back: {e}"
  | .ok back => TestM.assertEqual back.executionImage (some reviewerImage)

@[test]
def queueTaskReadsAnImage : Test := do
  let args := Json.mkObj [("prompt", "review"), ("execution_image", .str reviewerImage)]
  match Project.Tools.tryParseToolCall "queue_task" args with
  | some (.ok (.queueTask r)) => TestM.assertEqual r.executionImage (some reviewerImage)
  | other => TestM.fail s!"unexpected parse: {repr other}"
  let bad := Json.mkObj [("prompt", "review"), ("execution_image", .str "x y")]
  match Project.Tools.tryParseToolCall "queue_task" bad with
  | some (.error _) => TestM.assert true "reported"
  | other => TestM.fail s!"a malformed image should be reported, got: {repr other}"

/-! ## The policy bound -/

private def parent : SpawnContext :=
  { backend := some "claude", repo := some { owner := "acme", name := "widgets" } }

private def ask (image : Option String := none) : SpawnRequest :=
  { prompt := "review it", executionImage := image }

@[test]
def aListedImageMayBeNamed : Test := do
  let p : SpawnPolicy := { images := [reviewerImage] }
  match p.resolve parent (ask (some reviewerImage)) with
  | .error e => TestM.fail s!"an image the policy lists should be allowed: {e}"
  | .ok r => TestM.assertEqual r.executionImage (some reviewerImage)

@[test]
def anUnlistedImageIsRefused : Test := do
  let p : SpawnPolicy := { images := [reviewerImage] }
  match p.resolve parent (ask (some "docker.io/library/ubuntu:24.04")) with
  | .ok _ => TestM.fail "an image the policy does not list should be refused"
  | .error e => TestM.assert (mentions e reviewerImage) s!"the refusal names what is allowed: {e}"

@[test]
def anEmptyListRefusesNamingAnImage : Test := do
  match ({} : SpawnPolicy).resolve parent (ask (some reviewerImage)) with
  | .ok _ => TestM.fail "a policy listing no images lets nothing be named"
  | .error e => TestM.assert (mentions e "no images are listed") e

@[test]
def anOmittedImageIsInherited : Test := do
  match ({} : SpawnPolicy).resolve { parent with executionImage := some reviewerImage } (ask) with
  | .error e => TestM.fail s!"inheriting should need no listing: {e}"
  | .ok r =>
    TestM.assertEqual r.executionImage (some reviewerImage)
      (msg := "a follow-up of the same kind of work runs in the same image")
  match ({} : SpawnPolicy).resolve parent (ask) with
  | .error e => TestM.fail s!"a task with no image should queue one with none: {e}"
  | .ok r => TestM.assertEqual r.executionImage none

@[test]
def aPolicysImagesRoundTripAndAreValidated : Test := do
  let p : SpawnPolicy := { images := [reviewerImage] }
  match (FromJson.fromJson? (ToJson.toJson p) : Except String SpawnPolicy) with
  | .error e => TestM.fail s!"the policy should read back: {e}"
  | .ok back => TestM.assertEqual back.images [reviewerImage]
  match ({ images := ["not an image"] } : SpawnPolicy).validate Project.Role.knownPermissions with
  | .ok _ => TestM.fail "a policy listing a malformed image should be refused"
  | .error e => TestM.assert (mentions e "images") e
  match Json.parse "{\"images\": \"ghcr.io/acme/reviewer:1.4\"}"
      >>= FromJson.fromJson? (α := SpawnPolicy) with
  | .ok _ => TestM.fail "a bare string where a list belongs should be refused, not read as []"
  | .error _ => TestM.assert true

/-! ## The entry keeps it -/

private def imaged : Queue.QueueEntry :=
  { id := "q-1", createdAt := "2026-10-25T08:00:00Z", status := .unfinished, repo := none
  , prompt := "review", taskId := some "t-1", apiClient := some "reviews"
  , executionImage := some reviewerImage }

@[test]
def anEntryRoundTripsItsImage : Test := do
  match (FromJson.fromJson? (ToJson.toJson imaged) : Except String Queue.QueueEntry) with
  | .error e => TestM.fail s!"the entry should read back: {e}"
  | .ok back => TestM.assertEqual back.executionImage (some reviewerImage)
  match Queue.QueueEntry.ofRow? imaged.toRow with
  | .error e => TestM.fail s!"the row should read back: {e}"
  | .ok back => TestM.assertEqual back.executionImage (some reviewerImage)

@[test]
def aRetryKeepsTheImage : Test := do
  let e := Queue.retryEntryFor imaged (some "t-1") "q-2" "2026-10-25T09:00:00Z"
  TestM.assertEqual e.executionImage (some reviewerImage)
  TestM.assertEqual e.apiClient (some "reviews") (msg := "and the client's ownership")
  TestM.assertEqual e.continuesFrom (some "t-1")
  TestM.assert (e.status == .pending) (msg := "queued, not running")

@[test]
def aRestartResumeKeepsTheImage : Test := do
  let record : TaskStore.TaskRecord :=
    { id := "t-1", createdAt := "2026-10-25T08:00:00Z", status := .unfinished
    , sessionId := some "sess-1", repo := none, prompt := "review" }
  match Queue.resumeEntryFor imaged record { hasSession := true } "q-2" "2026-10-25T09:00:00Z" with
  | .error e => TestM.fail s!"an interrupted run with a session should be resumed: {e}"
  | .ok e => TestM.assertEqual e.executionImage (some reviewerImage)

@[test]
def theStoreKeepsTheImage : Test := do
  let back ← Orchestra.withTempData "execution-image" do
    Queue.saveEntry { imaged with status := .pending, taskId := none }
    return (← Queue.loadEntry "q-1").bind (·.executionImage)
  TestM.assertEqual back (some reviewerImage)

/-- Appended, and in its place: the schema agreeing with the migrations is
    `StoreTest.theMigrationsBuildTheDeclaredSchema`'s to check; this is that the column has a
    migration of its own after the API clients', rather than an edit to one already applied. -/
@[test]
def theImageMigrationIsTheEleventh : Test := do
  let names := Store.migrations.map (·.name)
  TestM.assertEqual ((names.drop 9).take 2) ["0010_api_clients", "0011_execution_image"]

/-! ## Which image wins -/

private def cluster : Kubernetes.Config :=
  match Kubernetes.Config.fromJson (Json.mkObj
      [ ("image", .str "ghcr.io/example/agent:1")
      , ("mcp_host", .str "orchestra.orchestra.svc.cluster.local")
      , ("images", Json.mkObj [("acme/widgets", .str "ghcr.io/acme/widgets-ci:pinned")])
      , ("allowed_image_prefixes", .arr #[.str "ghcr.io/acme/widgets"]) ]) with
  | .ok c    => c
  | .error _ => { image := "unparsed", mcpHost := "unparsed" }

private def session (repo : String) (repoImage taskImage : Option String) : SessionSpec :=
  { workdir := System.FilePath.mk "/var/lib/orchestra/work/slot0", label := "t-1"
  , repo := some repo, image := repoImage, taskImage }

private def imageOf (spec : SessionSpec) : String :=
  match Kubernetes.imageFor cluster spec with | .ok i => i | .error e => s!"<refused: {e}>"

@[test]
def theTasksImageBeatsThePinTheRepositoryAndTheDefault : Test := do
  TestM.assertEqual (imageOf (session "acme/widgets" (some "ghcr.io/acme/widgets-ci:own")
      (some reviewerImage))) reviewerImage (msg := "over a pin and a repository's own")
  TestM.assertEqual (imageOf (session "acme/gadgets" (some "ghcr.io/acme/widgets-ci:own")
      (some reviewerImage))) reviewerImage (msg := "over a repository's own")
  TestM.assertEqual (imageOf (session "acme/gadgets" none (some reviewerImage))) reviewerImage
    (msg := "over the default")
  -- `allowed_image_prefixes` is about what a repository may ask for; the task's image is outside
  -- it here and still wins, since what bounded it is the policy that let it be named.
  TestM.assert (!reviewerImage.startsWith "ghcr.io/acme/widgets") "the fixture is outside it"

@[test]
def withoutATaskImageTheOrderIsUnchanged : Test := do
  TestM.assertEqual (imageOf (session "acme/widgets" (some "ghcr.io/acme/widgets-ci:own") none))
    "ghcr.io/acme/widgets-ci:pinned" (msg := "the pin beats the repository")
  TestM.assertEqual (imageOf (session "acme/gadgets" (some "ghcr.io/acme/widgets-ci:own") none))
    "ghcr.io/acme/widgets-ci:own" (msg := "the repository beats the default")
  TestM.assertEqual (imageOf (session "acme/gadgets" none none)) "ghcr.io/example/agent:1"

@[test]
def thePodRunsTheTasksImage : Test := do
  let spec := session "acme/widgets" none (some reviewerImage)
  let .ok image := Kubernetes.imageFor cluster spec | TestM.fail "the task's image was refused"
  let manifest := Kubernetes.podManifest cluster spec "orchestra-abc123" image #[]
  let images : List String :=
    match manifest.getObjVal? "spec" |>.toOption |>.bind (·.getObjVal? "containers" |>.toOption) with
    | some (.arr cs) => cs.toList.filterMap (·.getObjValAs? String "image" |>.toOption)
    | _ => []
  TestM.assert (!images.isEmpty) "the manifest has a container"
  TestM.assert (images.all (· == reviewerImage)) s!"every container runs it: {images}"

@[test]
def aMalformedTaskImageIsRefusedNotReplaced : Test := do
  match Kubernetes.imageFor cluster (session "acme/widgets" none (some "a b")) with
  | .ok i => TestM.fail s!"a malformed image was turned into {i}"
  | .error e => TestM.assert (mentions e "unusable image") e

/-! ## The operator's absolute pins and allowed task images -/

private def clusterWith (extra : List (String × Json)) : Except String Kubernetes.Config :=
  Kubernetes.Config.fromJson (Json.mkObj ([ ("image", .str "ghcr.io/example/agent:1")
    , ("mcp_host", .str "orchestra.orchestra.svc.cluster.local") ] ++ extra))

private def strict : Kubernetes.Config :=
  match clusterWith
      [ ("images", Json.mkObj
          [ ("acme/widgets", .str "ghcr.io/acme/widgets-ci:pinned")
          , ("acme/secrets", Json.mkObj [("image", .str "ghcr.io/acme/hardened:3"),
                                         ("absolute", .bool true)]) ])
      , ("allowed_task_images", .arr #[.str reviewerImage, .str "ghcr.io/acme/tools/"]) ] with
  | .ok c    => c
  | .error _ => { image := "unparsed", mcpHost := "unparsed" }

private def strictImageOf (spec : SessionSpec) : String :=
  match Kubernetes.imageFor strict spec with | .ok i => i | .error e => s!"<refused: {e}>"

@[test]
def anAbsolutePinBeatsTheTasksImage : Test := do
  TestM.assertEqual (strictImageOf (session "acme/secrets" none (some reviewerImage)))
    "ghcr.io/acme/hardened:3" (msg := "absolute: the task cannot move it")
  TestM.assertEqual (strictImageOf (session "acme/secrets" (some "ghcr.io/acme/own:1") none))
    "ghcr.io/acme/hardened:3" (msg := "nor can the repository")
  TestM.assertEqual (strictImageOf (session "acme/widgets" none (some reviewerImage)))
    reviewerImage (msg := "an ordinary pin still loses to the task")
  -- Absolute is settled before the task image is checked against the allowed list: the task's
  -- image is irrelevant on that repository, so it is not a reason to refuse the task.
  TestM.assertEqual (strictImageOf (session "acme/secrets" none (some "docker.io/evil:1")))
    "ghcr.io/acme/hardened:3"

@[test]
def taskImagesAreHeldToTheAllowedList : Test := do
  TestM.assertEqual (strictImageOf (session "acme/gadgets" none (some "ghcr.io/acme/tools/lint:2")))
    "ghcr.io/acme/tools/lint:2" (msg := "under a listed prefix")
  match Kubernetes.imageFor strict (session "acme/gadgets" none (some "ghcr.io/acme/reviewer:1.45")) with
  | .ok i => TestM.fail s!"an exact entry is not a prefix, but {i} was accepted"
  | .error e => TestM.assert (mentions e "allowed_task_images") e
  match Kubernetes.imageFor strict (session "acme/widgets" none (some "docker.io/evil:1")) with
  | .ok i => TestM.fail s!"an image outside the list was accepted as {i}"
  | .error e => TestM.assert (mentions e reviewerImage) s!"the refusal names the list: {e}"
  TestM.assertEqual (strictImageOf (session "acme/gadgets" none none)) "ghcr.io/example/agent:1"
    (msg := "the list says nothing about tasks that name no image")

@[test]
def pinsAndTheAllowedListAreReadStrictly : Test := do
  TestM.assert (clusterWith [("images", Json.mkObj [("a/b", Json.mkObj [("absolute", .bool true)])])]
    |>.toOption.isNone) "an object pin needs an image"
  TestM.assert (clusterWith [("images", Json.mkObj [("a/b", Json.mkObj
      [("image", .str "x:1"), ("absolute", .str "yes")])])] |>.toOption.isNone)
    "absolute must be a boolean"
  TestM.assert (clusterWith [("images", Json.mkObj [("a/b", .str "not an image")])]
    |>.toOption.isNone) "a pin must be an image reference"
  TestM.assert (clusterWith [("images", Json.mkObj [("a/b", .num 3)])] |>.toOption.isNone)
    "nor a number"
  TestM.assert (clusterWith [("allowed_task_images", .str reviewerImage)] |>.toOption.isNone)
    "the allowed list must be a list"
  TestM.assert (clusterWith [("allowed_task_images", .arr #[.str ""])] |>.toOption.isNone)
    "an empty entry matches nothing and is refused"
  TestM.assert ((clusterWith [("images", Json.mkObj [("a/b", .str "x:1")])]).toOption.map
    (·.repoImages) == some #[("a/b", { image := "x:1" })]) "a plain string is an ordinary pin"

/-- GitHub treats the two spellings as one repository, so a pin must match either. -/
@[test]
def aPinMatchesWithoutRegardToCase : Test := do
  match clusterWith [("images", Json.mkObj
      [ ("Acme/Widgets", .str "ghcr.io/acme/widgets-ci:pinned")
      , ("ACME/Secrets", Json.mkObj [("image", .str "ghcr.io/acme/hardened:3"),
                                     ("absolute", .bool true)]) ])] with
  | .error e => TestM.fail s!"the pins should parse: {e}"
  | .ok c =>
    let imageOf (spec : SessionSpec) : String :=
      match Kubernetes.imageFor c spec with | .ok i => i | .error e => s!"<refused: {e}>"
    TestM.assertEqual (imageOf (session "acme/widgets" (some "ghcr.io/acme/own:1") none))
      "ghcr.io/acme/widgets-ci:pinned" (msg := "an ordinary pin under another spelling")
    TestM.assertEqual (imageOf (session "acme/secrets" none (some reviewerImage)))
      "ghcr.io/acme/hardened:3" (msg := "and an absolute one, which a task cannot slip past")
    TestM.assertEqual (c.unmatchedPins ["acme/widgets", "acme/secrets"]) []
      (msg := "known repositories match whatever their case")
    TestM.assertEqual (c.unmatchedPins ["acme/widgets"]) ["ACME/Secrets"]
      (msg := "a key no configured repository names is reported")

/-- The daemon refuses to start on settings the backend cannot read (`Daemon.run`), which is what
    makes a malformed pin an error at startup rather than a guarantee that silently does not hold. -/
@[test]
def aMalformedPinIsABackendThatCannotBeBuilt : Test := do
  match Exec.factoryOf? "kubernetes" with
  | none => TestM.fail "the kubernetes backend should be registered"
  | some f =>
    let opts := Json.mkObj [("mcp_host", .str "orchestra.svc"),
      ("images", Json.mkObj [("a/b", Json.mkObj [("absolute", .bool true)])])]
    TestM.assert (f.make opts).toOption.isNone "an object pin with no image cannot be built"

@[test]
def namingTheInheritedImagePassesUnlisted : Test := do
  let ctx := { parent with executionImage := some reviewerImage }
  match ({} : SpawnPolicy).resolve ctx (ask (some reviewerImage)) with
  | .error e => TestM.fail s!"asking for what it would inherit anyway should pass: {e}"
  | .ok r => TestM.assertEqual r.executionImage (some reviewerImage)
  match ({} : SpawnPolicy).resolve ctx (ask (some "ghcr.io/acme/other:1")) with
  | .ok _ => TestM.fail "any other image is still a choice, and unlisted"
  | .error _ => TestM.assert true

/-! ## Continuations built from a task record -/

@[test]
def aContinuationTakesTheImageOfTheEntryThatRanTheTask : Test := do
  let (found, missing) ← Orchestra.withTempData "execution-image-continue" do
    Queue.saveEntry { imaged with status := .done, taskId := some "t-ran" }
    let found ← Queue.imageForContinuation "t-ran"
    let missing ← Queue.imageForContinuation "t-run-by-hand"
    return (found, missing)
  TestM.assertEqual found (some reviewerImage)
  TestM.assertEqual missing none (msg := "no entry ran it: nothing to carry")

end OrchestraTest.ExecutionImage