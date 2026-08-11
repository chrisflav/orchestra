import OrchestraTest.TestM
import Orchestra.Deploy
import Orchestra.Server

open Lean (Json FromJson)
open Orchestra
open Orchestra.Deploy

namespace OrchestraTest.Deploy

/-! Tests for preview deployments (`Orchestra.Deploy`).

    Everything that talks to a cluster needs a cluster, so what is covered here is what decides
    whether the cluster is asked the right question: the name a spec deploys under, the manifests
    that name produces, the configuration that says where to send them, and the tool-call parsing
    in front of all three.

    The naming tests carry the most weight. A name is an identity — pod, service, ingress,
    hostname and compose project all take it — so a name that collides silently redeploys over
    someone else's preview, and a name that is not a DNS-1123 label is rejected by Kubernetes
    with an error a long way from its cause. -/

private def spec (repoOwner repoName ref : String) (pr : Option Nat := none) : Spec :=
  { repo := { owner := repoOwner, name := repoName }
  , ref
  , sourcePath := "/tmp/clone"
  , prNumber := pr }

private def cfg : DeployConfig :=
  { kubeconfig := "/etc/orchestra/previews.kubeconfig"
  , baseDomain := "previews.example.com" }

/-! ## Naming -/

@[test]
def nameIsADnsLabel : Test := do
  let name := deploymentName (spec "an-org" "My_Repo" "feature/Some Branch!")
  TestM.assert (name.all fun c => (c.isAlpha && c.isLower) || c.isDigit || c == '-')
    s!"name must be lowercase alphanumeric or dash, got {name}"
  TestM.assert (!name.startsWith "-" && !name.endsWith "-")
    s!"name must not start or end with a dash, got {name}"
  -- 63 is the DNS-1123 label limit, and a hostname made of one is what this becomes.
  TestM.assert (name.length <= 63) s!"name must fit a DNS label, got {name.length} chars"

@[test]
def sameSpecSameName : Test := do
  -- The property re-deploying depends on: a second run must replace the first, not join it.
  TestM.assertEqual
    (deploymentName (spec "org" "repo" "abc123" (pr := some 7)))
    (deploymentName (spec "org" "repo" "abc123" (pr := some 7)))

@[test]
def prNumberDecidesIdentity : Test := do
  -- Same pull request, new commit: the same preview, replaced. This is why the ref is not part
  -- of the name when a PR number is present.
  TestM.assertEqual
    (deploymentName (spec "org" "repo" "commit-one" (pr := some 7)))
    (deploymentName (spec "org" "repo" "commit-two" (pr := some 7)))

@[test]
def differentPrsDiffer : Test := do
  TestM.assert
    (deploymentName (spec "org" "repo" "x" (pr := some 7)) !=
     deploymentName (spec "org" "repo" "x" (pr := some 8)))
    "two pull requests must not share a deployment"

@[test]
def differentOwnersDiffer : Test := do
  -- Two forks of the same repository name are a routine case, and the owner is not in the slug —
  -- only in the hash. If the hash were not there, these would collide.
  TestM.assert
    (deploymentName (spec "alice" "repo" "main") != deploymentName (spec "bob" "repo" "main"))
    "same repository name under different owners must not collide"

@[test]
def longBranchesDoNotCollide : Test := do
  let a := deploymentName (spec "org" "repo" ("feature/" ++ String.mk (List.replicate 60 'a') ++ "-one"))
  let b := deploymentName (spec "org" "repo" ("feature/" ++ String.mk (List.replicate 60 'a') ++ "-two"))
  TestM.assert (a != b) "long branches sharing a prefix must not collide after truncation"
  TestM.assert (a.length <= 63 && b.length <= 63) "truncation must still fit a DNS label"

@[test]
def urlUsesTheName : Test := do
  let name := deploymentName (spec "org" "repo" "main")
  TestM.assertEqual (deploymentUrl cfg name) s!"https://{name}.previews.example.com"

/-! ## Manifests -/

private def podOf (c : DeployConfig) (s : Spec) : Json :=
  podManifest c s (deploymentName s) "2026-01-01T00:00:00Z"

@[test]
def podRequestsTheRuntimeClass : Test := do
  let pod := podOf cfg (spec "org" "repo" "main")
  let rc := (do (← pod.getObjVal? "spec").getObjValAs? String "runtimeClassName") |>.toOption
  -- The single most important field in this file: without it the pod shares the node's kernel,
  -- and running an uninspected compose file stops being defensible.
  TestM.assertEqual rc (some "kata")

@[test]
def emptyRuntimeClassOmitsTheField : Test := do
  let pod := podOf { cfg with runtimeClass := "" } (spec "org" "repo" "main")
  let rc := (do (← pod.getObjVal? "spec").getObjValAs? String "runtimeClassName") |>.toOption
  TestM.assertEqual rc none

@[test]
def podCarriesItsExpiry : Test := do
  let s := spec "org" "repo" "main"
  let pod := podManifest cfg s (deploymentName s) "2026-01-01T00:00:00Z"
  let expiry :=
    (do
      let md ← pod.getObjVal? "metadata"
      let ann ← md.getObjVal? "annotations"
      ann.getObjValAs? String "orchestra.dev/expires-at") |>.toOption
  -- The sweeper reads this and nothing else, so a pod without it never expires.
  TestM.assertEqual expiry (some "2026-01-01T00:00:00Z")

@[test]
def ingressRoutesTheHostname : Test := do
  let s := spec "org" "repo" "main"
  let name := deploymentName s
  let ing := ingressManifest cfg name "2026-01-01T00:00:00Z"
  let host :=
    (do
      let spec ← ing.getObjVal? "spec"
      let rules ← spec.getObjValAs? (Array Json) "rules"
      let first ← rules[0]?.getDM (throw "no rules")
      first.getObjValAs? String "host") |>.toOption
  TestM.assertEqual host (some s!"{name}.previews.example.com")

@[test]
def serviceTargetsTheDeclaredPort : Test := do
  let s := { spec "org" "repo" "main" with port := 8080 }
  let svc := serviceManifest cfg s (deploymentName s) "2026-01-01T00:00:00Z"
  let target :=
    (do
      let sp ← svc.getObjVal? "spec"
      let ports ← sp.getObjValAs? (Array Json) "ports"
      let first ← ports[0]?.getDM (throw "no ports")
      first.getObjValAs? Nat "targetPort") |>.toOption
  TestM.assertEqual target (some 8080)

@[test]
def allObjectsShareTheSelector : Test := do
  let s := spec "org" "repo" "main"
  let name := deploymentName s
  let all := manifests cfg s name "2026-01-01T00:00:00Z"
  let items := (all.getObjValAs? (Array Json) "items") |>.toOption |>.getD #[]
  TestM.assertEqual items.size 3 (msg := "pod, service and ingress")
  -- `destroy` deletes by this label. Anything not carrying it survives teardown and leaks.
  for item in items do
    let label :=
      (do
        let md ← item.getObjVal? "metadata"
        let labels ← md.getObjVal? "labels"
        labels.getObjValAs? String "orchestra.dev/deployment") |>.toOption
    TestM.assertEqual label (some name)

/-! ## Configuration -/

@[test]
def configParsesWithDefaults : Test := do
  let raw := r#"{"kubeconfig": "/etc/k.conf", "base_domain": "p.example.com"}"#
  match Json.parse raw >>= FromJson.fromJson? (α := DeployConfig) with
  | .error e => TestM.fail s!"deploy config parse: {e}"
  | .ok c =>
    TestM.assertEqual c.kubeconfig "/etc/k.conf"
    TestM.assertEqual c.baseDomain "p.example.com"
    TestM.assertEqual c.ns "previews"
    TestM.assertEqual c.runtimeClass "kata" (msg := "isolation must not be off by default")
    TestM.assertEqual c.ttlMinutes 240

@[test]
def configOverridesDefaults : Test := do
  let raw := r#"{"kubeconfig": "/k", "base_domain": "d", "namespace": "pr",
                 "runtime_class": "", "ttl_minutes": 30, "memory_limit": "8Gi"}"#
  match Json.parse raw >>= FromJson.fromJson? (α := DeployConfig) with
  | .error e => TestM.fail s!"deploy config parse: {e}"
  | .ok c =>
    TestM.assertEqual c.ns "pr"
    TestM.assertEqual c.runtimeClass ""
    TestM.assertEqual c.ttlMinutes 30
    TestM.assertEqual c.memoryLimit "8Gi"

@[test]
def configNeedsAClusterAndADomain : Test := do
  match Json.parse r#"{"base_domain": "d"}"# >>= FromJson.fromJson? (α := DeployConfig) with
  | .ok _ => TestM.fail "a deploy config without a kubeconfig should not parse"
  | .error _ => TestM.assert true
  match Json.parse r#"{"kubeconfig": "/k"}"# >>= FromJson.fromJson? (α := DeployConfig) with
  | .ok _ => TestM.fail "a deploy config without a base domain should not parse"
  | .error _ => TestM.assert true

@[test]
def appConfigWithoutDeploySectionDisablesIt : Test := do
  let raw := r#"{"github_app": {"app_id": 1, "private_key_path": "/k.pem"}}"#
  match Json.parse raw >>= FromJson.fromJson? (α := AppConfig) with
  | .error e => TestM.fail s!"app config parse: {e}"
  | .ok c => TestM.assert c.deploy.isNone "no deploy section must leave the feature off"

/-! ## Tool calls -/

@[test]
def parsesDeployPreview : Test := do
  let args := Json.mkObj [("ref", .str "feature-x")]
  match Server.parseToolCall "deploy_preview" args with
  | .deployPreview ref composeFile port pr =>
    TestM.assertEqual ref "feature-x"
    TestM.assertEqual composeFile "docker-compose.yaml"
    TestM.assertEqual port 80
    TestM.assertEqual pr none
  | _ => TestM.fail "expected .deployPreview"

@[test]
def parsesDeployPreviewOverrides : Test := do
  let args := Json.mkObj
    [ ("ref", .str "  feature-x  ")
    , ("compose_file", .str "deploy/compose.yaml")
    , ("port", .num 3000)
    , ("pr_number", .num 12) ]
  match Server.parseToolCall "deploy_preview" args with
  | .deployPreview ref composeFile port pr =>
    TestM.assertEqual ref "feature-x" (msg := "surrounding whitespace is trimmed")
    TestM.assertEqual composeFile "deploy/compose.yaml"
    TestM.assertEqual port 3000
    TestM.assertEqual pr (some 12)
  | _ => TestM.fail "expected .deployPreview"

@[test]
def rejectsDeployPreviewWithoutRef : Test := do
  match Server.parseToolCall "deploy_preview" (Json.mkObj []) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "deploy_preview without a ref should not parse"

@[test]
def rejectsUnusablePorts : Test := do
  match Server.parseToolCall "deploy_preview"
      (Json.mkObj [("ref", .str "x"), ("port", .num 0)]) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "port 0 should not parse"
  match Server.parseToolCall "deploy_preview"
      (Json.mkObj [("ref", .str "x"), ("port", .num 70000)]) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "a port above 65535 should not parse"

@[test]
def parsesTheRestOfTheGroup : Test := do
  match Server.parseToolCall "destroy_preview" (Json.mkObj [("name", .str "p-1")]) with
  | .destroyPreview name => TestM.assertEqual name "p-1"
  | _ => TestM.fail "expected .destroyPreview"
  match Server.parseToolCall "list_deployments" (Json.mkObj []) with
  | .listDeployments => TestM.assert true
  | _ => TestM.fail "expected .listDeployments"
  match Server.parseToolCall "deployment_logs" (Json.mkObj [("name", .str "p-1")]) with
  | .deploymentLogs name tail =>
    TestM.assertEqual name "p-1"
    TestM.assertEqual tail 200
  | _ => TestM.fail "expected .deploymentLogs"

@[test]
def rejectsDestroyWithoutName : Test := do
  match Server.parseToolCall "destroy_preview" (Json.mkObj []) with
  | .parseError _ => TestM.assert true
  | _ => TestM.fail "destroy_preview without a name should not parse"

end OrchestraTest.Deploy
