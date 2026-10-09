import Db

/-!
# `0002_usage_estimate`

Adds `usage_source.estimate`: the per-source consumption rates `Orchestra.Usage` learns from its
polls, and the accumulators it learns them with. Nullable, so every existing row reads as a source
nothing has been learned about yet and starts from the built-in prior.
-/

namespace Orchestra.Store.Migrations

def migration_0002_usage_estimate : Db.Migration.Migration where
  name := "0002_usage_estimate"
  steps := [
    .addColumn "usage_source" "estimate" { type := .text, nullable := true }]

end Orchestra.Store.Migrations
