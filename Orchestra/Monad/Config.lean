import Orchestra.Config

namespace Orchestra

/-- Typeclass for monads that can read orchestra configuration. -/
class MonadConfig (m : Type → Type) where
  /-- Read the application config from the given path, or `~/.agent/config.json` if none. -/
  readAppConfig     : Option System.FilePath → m AppConfig
  /-- Read a named system prompt from `~/.agent/prompts/<name>.md`. -/
  readSystemPrompt  : Option String → m (Option String)
  /-- Read a named prepend prompt from `~/.agent/prompts/<name>.md`. -/
  readPrependPrompt : Option String → m (Option String)

export MonadConfig (readAppConfig readSystemPrompt readPrependPrompt)

instance : MonadConfig IO where
  readAppConfig path    := loadAppConfig path
  readSystemPrompt name  := loadSystemPrompt name
  readPrependPrompt name := loadPrependPrompt name

end Orchestra
