namespace Orchestra

/-- Abstract interface for logging. -/
class MonadLog (m : Type → Type) where
  logInfo  : String → m Unit
  logError : String → m Unit

instance : MonadLog IO where
  logInfo  := IO.println
  logError := IO.eprintln

end Orchestra
