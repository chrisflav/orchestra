import Lean.Data.Json
import Orchestra.Config
import Orchestra.GitHub

open Lean (Json)

namespace Orchestra

/-- Typeclass for monads that can perform GitHub App authentication operations.
    This covers operations requiring the GitHub App private key (JWT creation,
    installation token generation). -/
class MonadGitHubApp (m : Type → Type) where
  /-- Create a GitHub App JWT from an app ID and private key path. -/
  createJWT : Nat → String → m String
  /-- Resolve the installation ID for a given owner using a JWT. -/
  getInstallationId : String → String → m Nat
  /-- Create a short-lived installation access token. -/
  createInstallationToken : String → Nat → m String
  /-- Configure the `gh` CLI to use the given token. -/
  setupGhAuth : String → m Unit

export MonadGitHubApp (createJWT getInstallationId createInstallationToken setupGhAuth)

instance : MonadGitHubApp IO where
  createJWT               := GitHub.createJWT
  getInstallationId       := GitHub.getInstallationId
  createInstallationToken := GitHub.createInstallationToken
  setupGhAuth             := GitHub.setupGhAuth

/-- Typeclass for monads that can interact with the GitHub API using a PAT or
    installation token.  Differentiated from `MonadGitHubApp` because many
    operations only need a token, not the private key. -/
class MonadGitHub (m : Type → Type) where
  /-- Create a pull request on the upstream repo using a PAT. -/
  createPullRequest : String → Repository → String → String → String → String → m String
  /-- Create a pull request within a single repository using a token. -/
  createPullRequestOnRepo : String → Repository → String → String → String → String → m String
  /-- Fetch review threads for a pull request via GraphQL. -/
  getPrReviewThreads : Repository → Nat → String → m Json
  /-- Post a comment on an issue or pull request. -/
  createIssueComment : String → Repository → Nat → String → m String
  /-- Reply to an inline PR review comment. -/
  replyToPrReviewComment : String → Repository → Nat → Nat → String → m String
  /-- Create a new inline PR review comment on a specific file and line. -/
  createPrReviewComment : String → Repository → Nat → String → String → Nat → String → m String
  /-- Post a review (with optional inline comments) on a pull request. -/
  createPrReview : String → Repository → Nat → String → Array GitHub.InlineComment → m String
  /-- Return the pull-request number that a review comment belongs to. -/
  getPrReviewCommentPrNumber : String → Repository → Nat → m Nat

export MonadGitHub (createPullRequest createPullRequestOnRepo getPrReviewThreads
  createIssueComment replyToPrReviewComment createPrReviewComment createPrReview
  getPrReviewCommentPrNumber)

instance : MonadGitHub IO where
  createPullRequest          := GitHub.createPullRequest
  createPullRequestOnRepo    := GitHub.createPullRequestOnRepo
  getPrReviewThreads         := GitHub.getPrReviewThreads
  createIssueComment         := GitHub.createIssueComment
  replyToPrReviewComment     := GitHub.replyToPrReviewComment
  createPrReviewComment      := GitHub.createPrReviewComment
  createPrReview pat repo n body comments := GitHub.createPrReview pat repo n body comments
  getPrReviewCommentPrNumber := GitHub.getPrReviewCommentPrNumber

end Orchestra
