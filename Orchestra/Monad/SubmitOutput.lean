import Lean.Data.Json

open Lean (Json)

namespace Orchestra

/-- Typeclass for monads that can record the agent's task output.
    Only meaningful when the current task has a non-unit output type;
    the `submit_task_output` MCP tool is only exposed in that case. -/
class MonadSubmitOutput (m : Type → Type) where
  submitOutput : Json → m Unit

export MonadSubmitOutput (submitOutput)

end Orchestra
