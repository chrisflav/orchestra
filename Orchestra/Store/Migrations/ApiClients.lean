import Db

/-!
# `0010_api_clients`

Adds `queue_entry.api_client` and `queue_entry.external_id`: which API client queued an entry over
HTTP (`Orchestra.ApiClient`), and the client's own id for it. Both nullable and left NULL on
existing rows, which is what every row written before them meant: nothing queued it over HTTP.

The pair is unique. NULLs are distinct in both SQLite and PostgreSQL, so the index constrains only
the rows a client wrote with an id of its own — which is what lets queueing the same work twice
answer the entry already there, even when the two requests race.
-/

namespace Orchestra.Store.Migrations

def migration_0010_api_clients : Db.Migration.Migration where
  name := "0010_api_clients"
  steps := [
    .addColumn "queue_entry" "api_client" { type := .text, nullable := true },
    .addColumn "queue_entry" "external_id" { type := .text, nullable := true },
    .createIndex "queue_entry"
      { name := "idx_queue_entry_client_external_id",
        keys := [{ column := "api_client" }, { column := "external_id" }],
        unique := true }]

end Orchestra.Store.Migrations