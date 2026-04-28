import Lean.Data.Json
import Std.Sync

open Lean (Json)

namespace Orchestra.ConcertManager

/-- Manages suspended concert step fibers waiting for their queue task to complete.
    Each step registers a promise; the queue worker resolves it with the task output. -/
structure ConcertManager where
  private mutex    : Std.BaseMutex
  private registry : IO.Ref (List (String × IO.Promise (Option Json)))

def new : IO ConcertManager := do
  return { mutex := ← Std.BaseMutex.new, registry := ← IO.mkRef [] }

/-- Register `key` and return a promise the concert fiber will block on. -/
def register (mgr : ConcertManager) (key : String) : IO (IO.Promise (Option Json)) := do
  let promise ← IO.Promise.new
  mgr.mutex.lock
  mgr.registry.modify ((key, promise) :: ·)
  mgr.mutex.unlock
  return promise

/-- Deliver `result` to the concert fiber waiting on `key`. No-op if key is unknown. -/
def signal (mgr : ConcertManager) (key : String) (result : Option Json) : IO Unit := do
  mgr.mutex.lock
  let reg ← mgr.registry.get
  let entry := reg.find? (·.1 == key)
  mgr.registry.modify (·.filter (·.1 != key))
  mgr.mutex.unlock
  if let some (_, promise) := entry then
    promise.resolve result

/-- Resolve all pending promises with `none` (e.g. on daemon shutdown). -/
def cancelAll (mgr : ConcertManager) : IO Unit := do
  mgr.mutex.lock
  let reg ← mgr.registry.get
  mgr.registry.set []
  mgr.mutex.unlock
  for (_, promise) in reg do
    promise.resolve none

end Orchestra.ConcertManager

/-- Alias used by callers outside the namespace. -/
abbrev Orchestra.ConcertManager := Orchestra.ConcertManager.ConcertManager
