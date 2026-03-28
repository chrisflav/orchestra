import Orchestra.Config

namespace Orchestra.Concert

/-- A named series for grouping tasks in a sequence.
    The constructor is private so that users must create series via `freshSeries`. -/
structure Series where
  private mk ::
  name : String

/-- A task with associated series information, produced by the Concert DSL. -/
structure ConcertTask extends Task where
  series : Option String := none

/-- The state maintained during Concert program execution. -/
structure ConcertState where
  /-- The accumulated list of tasks (the eventual output when compiling). -/
  tasks : Array ConcertTask := #[]
  /-- All series created during this concert. -/
  series : Array Series := #[]
  /-- The currently active series (if any). -/
  currentSeries : Option Series := none

/-- The Concert monad: a state monad over `ConcertState`. -/
abbrev ConcertM (α : Type) := StateM ConcertState α

/-- Append a new task to the current task list.
    The series of the task is taken from the current series in the state.
    Note: the `series` field of `task` is ignored; use `setSeries`/`withSeries` to control it. -/
def run (task : Task) : ConcertM Unit := do
  let state ← get
  let ctask : ConcertTask := {
    toTask := task
    series := state.currentSeries.map (·.name)
  }
  set { state with tasks := state.tasks.push ctask }

/-- Create a fresh `Series` with the given name and register it in the state. -/
def freshSeries (name : String) : ConcertM Series := do
  let s := Series.mk name
  modify fun state => { state with series := state.series.push s }
  return s

/-- Set the current series. -/
def setSeries (s : Series) : ConcertM Unit :=
  modify fun state => { state with currentSeries := some s }

/-- Run the given action with the current series temporarily set to `s`,
    then restore the previous series. -/
def withSeries {α : Type} (s : Series) (action : ConcertM α) : ConcertM α := do
  let prevSeries := (← get).currentSeries
  setSeries s
  let result ← action
  modify fun state => { state with currentSeries := prevSeries }
  return result

/-- Compile a Concert program into an array of `ConcertTask`s. -/
def compile (program : ConcertM Unit) : Array ConcertTask :=
  (program.run {}).2.tasks

end Orchestra.Concert
