import Orchestra.Project.Basic
import Orchestra.Project.Role

namespace Orchestra

/-- Typeclass for monads that can read and write project and issue records,
    and load role definitions. -/
class MonadProject (m : Type → Type) where
  freshProjectId  : m Project.ProjectId
  freshIssueId    : m Project.IssueId
  saveProject     : Project.Project → m Unit
  loadProject     : Project.ProjectId → m (Option Project.Project)
  loadAllProjects : m (Array Project.Project)
  saveIssue       : Project.Issue → m Unit
  loadIssue       : Project.ProjectId → Project.IssueId → m (Option Project.Issue)
  loadIssues      : Project.ProjectId → m (Array Project.Issue)
  /-- Find an issue by ID across all projects, returning the owning project too. -/
  findIssue       : Project.IssueId → m (Option (Project.Project × Project.Issue))
  /-- Direct children of `parent` within `pid`. -/
  childrenOf      : Project.ProjectId → Project.IssueId → m (Array Project.Issue)
  /-- Top-level (parentless) issues of `pid`. -/
  rootIssues      : Project.ProjectId → m (Array Project.Issue)
  loadRole        : Project.ProjectId → String → m (Option Project.Role)
  loadAllRoles    : Project.ProjectId → m (Array Project.Role)
  roleSearchPaths : Project.ProjectId → String → m (System.FilePath × System.FilePath)

instance : MonadProject IO where
  freshProjectId  := Project.freshProjectId
  freshIssueId    := Project.freshIssueId
  saveProject     := Project.saveProject
  loadProject     := Project.loadProject
  loadAllProjects := Project.loadAllProjects
  saveIssue       := Project.saveIssue
  loadIssue       := Project.loadIssue
  loadIssues      := Project.loadIssues
  findIssue       := Project.findIssue
  childrenOf      := Project.childrenOf
  rootIssues      := Project.rootIssues
  loadRole        := Project.loadRole
  loadAllRoles    := Project.loadAllRoles
  roleSearchPaths := Project.roleSearchPaths

end Orchestra
