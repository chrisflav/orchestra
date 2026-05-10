import Orchestra.Monad.Log
import Orchestra.Monad.Config
import Orchestra.Monad.GitHub
import Orchestra.Monad.TaskStore
import Orchestra.Monad.Queue
import Orchestra.Monad.ProjectTool
import Orchestra.Monad.SubmitOutput
import Orchestra.Monad.Project
import Orchestra.Monad.Claim

/-!
# Orchestra Monadic Interfaces

This module collects the typeclass-based monadic interfaces for Orchestra's
main effect categories.  Each typeclass abstracts over a concrete IO operation,
enabling alternative instances (e.g. test monads) that do not perform real
side effects.

| Typeclass          | Abstracts                                        |
|--------------------|--------------------------------------------------|
| `MonadLog`          | stdout / stderr logging                          |
| `MonadConfig`       | reading application and prompt configuration     |
| `MonadGitHubApp`    | GitHub App authentication (JWT, install tokens)  |
| `MonadGitHub`       | GitHub API operations via PAT / install token    |
| `MonadTaskStore`    | task-record persistence and series pointers      |
| `MonadQueue`        | queue-entry persistence and PID-file management  |
| `MonadProjectTool`  | MCP project-tool evaluation                      |
| `MonadSubmitOutput` | recording the agent's task output value          |
| `MonadProject`      | project and issue record persistence             |
| `MonadClaim`        | issue claim lock acquire / release               |

Every typeclass ships with an `IO` instance that delegates to the existing
concrete implementations, preserving all prior behaviour.
-/
