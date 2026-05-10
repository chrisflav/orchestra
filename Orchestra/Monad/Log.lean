namespace Orchestra

/-- Typeclass for monads that support structured logging. -/
class MonadLog (m : Type → Type) where
  /-- Log an informational message (stdout). -/
  logInfo  : String → m Unit
  /-- Log an error message (stderr). -/
  logError : String → m Unit

export MonadLog (logInfo logError)

instance : MonadLog IO where
  logInfo  := IO.println
  logError msg := do
    let stderr ← IO.getStderr
    stderr.putStrLn msg
    stderr.flush

end Orchestra
