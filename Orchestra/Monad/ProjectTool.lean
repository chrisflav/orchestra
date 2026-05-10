import Lean.Data.Json
import Orchestra.Project.Tools

open Lean (Json)

namespace Orchestra

/-- Typeclass for monads that can evaluate MCP project tools.
    Abstracts the concrete `IO`-based `Project.Tools.evalProjectTool` so that
    alternative instances (e.g. test monads) can be provided without real
    side effects. -/
class MonadProjectTool (m : Type → Type) where
  /-- Evaluate a project tool call in the given environment. -/
  evalProjectTool : Project.Tools.Env → Project.Tools.ProjectTool → m Json

export MonadProjectTool (evalProjectTool)

instance : MonadProjectTool IO where
  evalProjectTool := Project.Tools.evalProjectTool

end Orchestra
