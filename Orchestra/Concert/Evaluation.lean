import Orchestra.Concert.Basic
import Orchestra.TaskRunner
import Std.Sync

namespace Orchestra.Concert

open Orchestra.TaskRunner (runIOTask)

mutual
  private partial def evalWhile (appConfig : Orchestra.AppConfig) (debug : Bool)
      (cancelToken : Option Std.CancellationToken)
      (cond : Concert Bool) (body : Concert Unit) : IO Unit := do
    if ← eval appConfig debug cancelToken cond then
      eval appConfig debug cancelToken body
      evalWhile appConfig debug cancelToken cond body

  /-- Evaluate a Concert program in IO. -/
  partial def eval (appConfig : Orchestra.AppConfig) (debug : Bool := false)
      (cancelToken : Option Std.CancellationToken := none) : Concert α → IO α
    | .pure a              => pure a
    | .abort               => throw (IO.userError "Concert aborted")
    | .op (.run o t input) k => do
        let spec := t.toIOTask input
        let (_, maybeResult) ← runIOTask appConfig spec 0 debug input
          (cancelToken := cancelToken)
        let result := maybeResult.getD default
        eval appConfig debug cancelToken (k result)
    | .op (.while cond body) k => do
        evalWhile appConfig debug cancelToken cond body
        eval appConfig debug cancelToken (k ())
end

def evalWithConfig (c : Concert α) : IO α := do
  let appConfig ← loadAppConfig
  eval appConfig false none c

end Orchestra.Concert
