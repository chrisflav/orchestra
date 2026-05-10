import Orchestra.Monad.TaskStore
import Orchestra.Monad.Queue
import Orchestra.Project.Basic

open Orchestra (MonadTaskStore MonadQueue generateTaskId currentIso8601 saveEntry)
open Orchestra.Project (ProjectId IssueId PRRef Project ReviewerTemplate)

namespace Orchestra.Project.Enqueue

private def renderReviewerPrompt (tmpl : String) (pr : PRRef) (iid : IssueId) : String :=
  tmpl.replace "{{repo}}"      pr.repo.toString
    |>.replace "{{pr_number}}" (toString pr.number)
    |>.replace "{{branch}}"    pr.branch
    |>.replace "{{issue_id}}"  iid.value

/-- Enqueue a merger task for `pr` so the queue daemon will merge it later.
    Returns the new queue entry's ID. -/
def enqueueMergerImpl [Monad m] [MonadTaskStore m] [MonadQueue m]
    (pid : ProjectId) (iid : IssueId) (pr : PRRef) : m String := do
  let id ← generateTaskId
  let createdAt ← currentIso8601
  let entry : Orchestra.Queue.QueueEntry :=
    { id, createdAt
    , upstream := pr.repo, fork := pr.repo
    , mode := .pr
    , prompt := s!"merge {pr.repo}#{pr.number}"
    , backend := some "merger"
    , projectId := some pid
    , issueId := some iid
    , priority := 100 }
  saveEntry entry
  return id

/-- Enqueue a reviewer task for `pr` against `tmpl`.
    Called by `attach_pr` when the project has a `reviewer` template configured.
    Returns the new queue entry's ID. -/
def enqueueReviewerImpl [Monad m] [MonadTaskStore m] [MonadQueue m]
    (project : Project) (iid : IssueId) (pr : PRRef) (tmpl : ReviewerTemplate) : m String := do
  let id ← generateTaskId
  let createdAt ← currentIso8601
  let entry : Orchestra.Queue.QueueEntry :=
    { id, createdAt
    , upstream := pr.repo, fork := pr.repo
    , mode := .pr
    , prompt := renderReviewerPrompt tmpl.promptTemplate pr iid
    , backend := tmpl.backend
    , projectId := some project.id
    , issueId := some iid
    , tools := some ["review_issues", "comment", "get_pr_comments"]
    , readOnly := true
    , issueNumber := some pr.number
    , priority := 50 }
  saveEntry entry
  return id

end Orchestra.Project.Enqueue
