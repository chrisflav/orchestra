import Lean.Data.Json
import Orchestra.Config
import Orchestra.GitHub

open Lean (Json)

namespace Orchestra

open GitHub (InlineComment)

/--
Abstract interface for GitHub App-level authentication operations.
Used when the agent acts as a GitHub App (JWT-based flow).
-/
class MonadGitHubApp (m : Type → Type) where
  createJWT               : Nat → String → m String
  getInstallationId       : String → String → m Nat
  createInstallationToken : String → Nat → m String
  setupGhAuth             : String → m Unit

/--
Abstract interface for GitHub API operations using a PAT or pre-authenticated token.
Covers pull requests, issue comments, and PR review threads.
-/
class MonadGitHub (m : Type → Type) where
  createPullRequest          : String → Repository → String → String → String → String → m String
  createPullRequestOnRepo    : String → Repository → String → String → String → String → m String
  getPrReviewThreads         : Repository → Nat → String → m Json
  createIssueComment         : String → Repository → Nat → String → m String
  replyToPrReviewComment     : String → Repository → Nat → Nat → String → m String
  createPrReviewComment      : String → Repository → Nat → String → String → Nat → String → m String
  createPrReview             : String → Repository → Nat → String → Array InlineComment → m String
  getPrReviewCommentPrNumber : String → Repository → Nat → m Nat

instance : MonadGitHubApp IO where
  createJWT               := Orchestra.GitHub.createJWT
  getInstallationId       := Orchestra.GitHub.getInstallationId
  createInstallationToken := Orchestra.GitHub.createInstallationToken
  setupGhAuth             := Orchestra.GitHub.setupGhAuth

instance : MonadGitHub IO where
  createPullRequest          := Orchestra.GitHub.createPullRequest
  createPullRequestOnRepo    := Orchestra.GitHub.createPullRequestOnRepo
  getPrReviewThreads         := Orchestra.GitHub.getPrReviewThreads
  createIssueComment         := Orchestra.GitHub.createIssueComment
  replyToPrReviewComment     := Orchestra.GitHub.replyToPrReviewComment
  createPrReviewComment      := Orchestra.GitHub.createPrReviewComment
  createPrReview             := fun pat upstream prNumber body comments =>
                                  Orchestra.GitHub.createPrReview pat upstream prNumber body comments
  getPrReviewCommentPrNumber := Orchestra.GitHub.getPrReviewCommentPrNumber

end Orchestra
