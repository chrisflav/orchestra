import Orchestra.Store.Schema
import Orchestra.Store.Convert
import Orchestra.Store.Connection

/-!
# `Orchestra.Store`

Orchestra's record database: the schema and its migrations (`Store.Schema`), the conversions
between a record and a row (`Store.Convert`), and the connection every store goes through
(`Store.Connection`). Named `Store` rather than `Db` so that `Db.Migration` and the rest of the
library still resolve to the library from inside it.

The stores themselves — `TaskStore`, `Queue`, and the ones later stages move — import this and
convert their own records to and from the row structures declared here. The one-time import of
the JSON directories they used to be is `Orchestra.Store.Import`, which sits *above* the stores
because it needs all of them.
-/
