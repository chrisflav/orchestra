import Orchestra.Config

namespace Orchestra

/-- Abstract interface for reading configuration and prompt files. -/
class MonadConfig (m : Type → Type) where
  loadAppConfig     : Option System.FilePath → m AppConfig
  loadSystemPrompt  : Option String → m (Option String)
  loadPrependPrompt : Option String → m (Option String)

instance : MonadConfig IO where
  loadAppConfig     := fun path => Orchestra.loadAppConfig path
  loadSystemPrompt  := fun name => Orchestra.loadSystemPrompt name
  loadPrependPrompt := fun name => Orchestra.loadPrependPrompt name

end Orchestra
