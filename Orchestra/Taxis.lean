import Taxis.Client

/-!
# taxis client

Orchestra's project/issue/claim subsystem (`Orchestra.Project`) is backed by a taxis instance
instead of its own file-based storage (see the "Migrate to taxis" tracking issue). This module
used to hand-roll a second typed HTTP client mirroring taxis's own (`Taxis.Client`) — that
duplication is gone: everything below is a re-export of `Taxis.Client`/`Taxis`'s own domain types
and wrapper functions, plus the one piece of genuinely Orchestra-specific machinery, an
active-config ref so `Orchestra.Project.*`/`Orchestra.Project.Claim.*` don't need to thread a
`Config` value through every call site.
-/

namespace Orchestra.Taxis

export Taxis.Client (Config listIssues createIssue getIssueDetail getIssue updateIssue deleteIssue
  createArtifact deleteArtifact listComments createComment listLabels createLabel ensureLabel
  listActors getMe epochToIso8601)

export _root_.Taxis (Issue IssueDetail IssueInput IssueUpdate IssueState Label LabelId Comment
  ReviewState Actor Artifact ArtifactView ArtifactDisplay)

/-! ## Active configuration

`Orchestra.Project.Basic`'s functions (`loadProject`, `saveIssue`, ...) keep the same signatures
they had as file-backed operations — no `Config` parameter — so they read the active taxis config
from this process-wide ref instead, exactly mirroring the pre-migration `projectsDirOverride`
pattern in `Project.Basic`. Set once at startup from `AppConfig.taxis`; tests override it directly
via `setConfig` instead of pointing at a temp directory. -/
initialize configRef : IO.Ref (Option Config) ← IO.mkRef none

def setConfig (cfg : Option Config) : IO Unit := configRef.set cfg

/-- The active taxis config, or an error if none is set — callers throw this straight through
    (unconfigured taxis is a hard error, not a silently-skipped feature, once anything tries to
    use it). -/
def getConfig : IO Config := do
  match ← configRef.get with
  | some cfg => pure cfg
  | none => throw (.userError
      "taxis is not configured — set a \"taxis\": {\"url\": ..., \"token\": ...} section in config.json")

end Orchestra.Taxis
