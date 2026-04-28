import Orchestra.Config

namespace Orchestra.Concert

instance : CoeSort ResultType Type where
  coe t := t.Type

/-- A typed task with input type `i` and output type `o`. -/
structure Task (i o : ResultType) where
  toIOTask : i → IOTask i o

mutual
  /-- Primitive operations in the Concert language. -/
  inductive ConcertOp : Type → Type 1 where
    /-- Run a task with the given input. `o` is explicit so the evaluator can recover it. -/
    | run   : (o : ResultType) → Task i o → i → ConcertOp o
    | while : Concert Bool → Concert Unit → ConcertOp Unit

  /-- A Concert program: a free monad over `ConcertOp`.
      `abort` represents a cancelled or failed program. -/
  inductive Concert : Type → Type 1 where
    | pure  : α → Concert α
    | op    : ConcertOp β → (β → Concert α) → Concert α
    | abort : Concert α
end

instance : Inhabited (Concert α) := ⟨.abort⟩

@[inline]
private def bindConcert {α β : Type} : Concert α → (α → Concert β) → Concert β
  | .pure a, f  => f a
  | .op op k, f => .op op (fun x => bindConcert (k x) f)
  | .abort, _   => .abort

instance : Monad Concert where
  pure := Concert.pure
  bind := bindConcert

/-- Lift a typed task into Concert. -/
def task {i o : ResultType} (t : Task i o) (input : i) : Concert o :=
  .op (.run o t input) .pure

/-- Loop `body` while `cond` evaluates to true. -/
def whileM (cond : Concert Bool) (body : Concert Unit) : Concert Unit :=
  .op (.while cond body) .pure

/-- Convenience: lift a `IOTask .unit .unit` directly into Concert. -/
def run {i o : ResultType} (spec : IOTask i o) (x : i) : Concert o :=
  task { toIOTask := fun _ => spec } x

end Orchestra.Concert
