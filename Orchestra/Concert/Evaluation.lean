import Orchestra.Concert.Basic
import Orchestra.ConcertManager
import Orchestra.Queue
import Orchestra.TaskRunner
import Orchestra.TaskStore
import Std.Sync

namespace Orchestra.Concert

open Orchestra.TaskRunner (runIOTask)
open Orchestra.ConcertManager (ConcertManager)

mutual
  private partial def evalWhile (appConfig : Orchestra.AppConfig) (debug : Bool)
      (cancelToken : Option Std.CancellationToken)
      (cond : Concert Bool) (body : Concert Unit) : IO Unit := do
    if ← eval appConfig debug cancelToken cond then
      eval appConfig debug cancelToken body
      evalWhile appConfig debug cancelToken cond body

  /-- Evaluate a Concert program directly in IO, running each task immediately. -/
  partial def eval (appConfig : Orchestra.AppConfig) (debug : Bool := false)
      (cancelToken : Option Std.CancellationToken := none) : Concert α → IO α
    | .pure a              => pure a
    | .abort               => throw (IO.userError "Concert aborted")
    | .op (.run o t input) k => do
        let spec := t.toIOTask input
        let ((_, _), maybeResult, _) ← runIOTask appConfig spec 0 debug input
          (cancelToken := cancelToken)
        let result := maybeResult.getD default
        eval appConfig debug cancelToken (k result)
    | .op (.while cond body) k => do
        evalWhile appConfig debug cancelToken cond body
        eval appConfig debug cancelToken (k ())
end

mutual
  private partial def evalQueuedWhile (mgr : ConcertManager) (appConfig : Orchestra.AppConfig)
      (debug : Bool) (cancelToken : Option Std.CancellationToken)
      (cond : Concert Bool) (body : Concert Unit) : IO Unit := do
    if ← evalQueued mgr appConfig debug cancelToken cond then
      evalQueued mgr appConfig debug cancelToken body
      evalQueuedWhile mgr appConfig debug cancelToken cond body

  /-- Evaluate a Concert program via the queue daemon: each task is submitted as a
      `QueueEntry` and the fiber suspends until the daemon signals completion. -/
  partial def evalQueued (mgr : ConcertManager) (appConfig : Orchestra.AppConfig)
      (debug : Bool := false) (cancelToken : Option Std.CancellationToken := none)
      : Concert α → IO α
    | .pure a  => pure a
    | .abort   => throw (IO.userError "Concert aborted")
    | .op (.run o t input) k => do
        let spec       := t.toIOTask input
        let stepKey    ← TaskStore.generateId
        let promise    ← ConcertManager.register mgr stepKey
        let createdAt  ← TaskStore.currentIso8601
        let entry : Queue.QueueEntry := {
          id            := stepKey
          createdAt
          status        := .pending
          upstream      := spec.upstream
          fork          := spec.fork
          mode          := spec.mode
          prompt        := spec.prompt
          agent         := spec.agent
          systemPrompt  := spec.systemPrompt
          backend       := spec.backend
          model         := spec.model
          budget        := spec.budget
          memory        := spec.memory
          authSource    := spec.authSource
          tools         := spec.tools
          readOnly      := spec.readOnly
          priority      := spec.priority
          concertStepKey := some stepKey
          inputType     := ResultType.unit
          outputType    := o
          inputJson     := none
        }
        Queue.saveEntry entry
        -- Block until the queue worker signals completion (always resolved, safe to use result!)
        let resultJson ← IO.wait promise.result!
        let result :=
          match resultJson.bind (ResultType.valueFromJson o · |>.toOption) with
          | some v => v
          | none   => default
        evalQueued mgr appConfig debug cancelToken (k result)
    | .op (.while cond body) k => do
        evalQueuedWhile mgr appConfig debug cancelToken cond body
        evalQueued mgr appConfig debug cancelToken (k ())
end

def evalWithConfig (c : Concert α) : IO α := do
  let appConfig ← loadAppConfig
  eval appConfig false none c

end Orchestra.Concert
