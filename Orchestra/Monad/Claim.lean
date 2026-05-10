import Orchestra.Project.Claim

namespace Orchestra

/-- Typeclass for monads that can read and write issue claim records. -/
class MonadClaim (m : Type → Type) where
  loadClaim         : Project.ProjectId → Project.IssueId → m (Option Project.Claim)
  loadClaims        : Project.ProjectId → m (Array (Project.IssueId × Project.Claim))
  tryClaim          : Project.ClaimManager → Project.ProjectId → Project.IssueId
                      → String → String → String → Option String → m Project.ClaimResult
  release           : Project.ClaimManager → Project.ProjectId → Project.IssueId
                      → Project.IssueStatus → String → m Bool
  updateClaimTaskId : Project.ClaimManager → Project.ProjectId → Project.IssueId
                      → String → m Unit
  forceRelease      : Project.ClaimManager → Project.ProjectId → Project.IssueId → m Bool

instance : MonadClaim IO where
  loadClaim         pid iid                   := Project.loadClaim pid iid
  loadClaims        pid                       := Project.loadClaims pid
  tryClaim          mgr pid iid tid ag now ser := Project.tryClaim mgr pid iid tid ag now ser
  release           mgr pid iid st now        := Project.release mgr pid iid st now
  updateClaimTaskId mgr pid iid tid           := Project.updateClaimTaskId mgr pid iid tid
  forceRelease      mgr pid iid               := Project.forceRelease mgr pid iid

end Orchestra
