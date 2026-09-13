import Lean.Data.Json
import Orchestra.Config
import Orchestra.Store.Schema
import Orchestra.Utils.Time

/-!
# Between a record and its row

A column holds a `String`, an `Int`, a `Bool`, a `Float` or an `Option` of one, and orchestra's
records hold enumerations, repository pairs, typed ids and lists. These are the handful of
conversions that bridge the two, shared by every store rather than written out per record.

The rule throughout is that a column carries the spelling the record's own JSON instances already
use — the status name, the `"owner/repo"`, the compressed JSON array. A row is therefore readable,
and, more to the point, the legacy import can build one straight out of the file it parsed.

Conversion out of a row is total and returns `Except String`: a row written by a newer orchestra,
or by hand, may hold a status name this build does not know, and a store that threw on one would
lose every other record in the same listing. `keepConvertible` is what the listings use — it
reports such a row on stderr and skips it, which is exactly what a file that did not parse did.
-/

open Lean (Json ToJson FromJson)

namespace Orchestra.Store

/-- An enumeration as its column value: the lower-case name its `ToJson` instance writes.

    Anything that is not a JSON string is a bug in the instance rather than something a caller
    can act on, so it is compressed into the column instead of refused; it will not read back,
    and the row it is on will be reported when it does not. -/
def enumColumn {α : Type} [ToJson α] (x : α) : String :=
  match ToJson.toJson x with
  | .str s => s
  | j      => j.compress

/-- Read an enumeration back out of a column, naming the field when it is not one this build
    knows. -/
def enumOfColumn? {α : Type} [FromJson α] (field : String) (s : String) : Except String α :=
  (FromJson.fromJson? (Json.str s)).mapError fun e => s!"{field}: {e}"

/-- Anything structured as its column value: the compressed JSON its `ToJson` writes. -/
def jsonColumn {α : Type} [ToJson α] (x : α) : String :=
  Json.compress (ToJson.toJson x)

/-- Read something structured back out of a column. -/
def jsonOfColumn? {α : Type} [FromJson α] (field : String) (s : String) : Except String α := do
  let j ← (Json.parse s).mapError fun e => s!"{field}: invalid JSON: {e}"
  (FromJson.fromJson? j).mapError fun e => s!"{field}: {e}"

/-- A repository pair as its two columns, or two NULLs for a repository-independent record.

    Two columns rather than one because that is what the record says: the pair travels together
    or not at all, and a row holding one half is a row `repoOfColumns?` refuses. -/
def repoColumns (repo : Option RepoPair) : Option String × Option String :=
  match repo with
  | some p => (some p.upstream.toString, some p.fork.toString)
  | none   => (none, none)

/-- Read a repository pair back. Both columns or neither: half a pair would send a task written
    to open a pull request into an empty directory, and the failure would surface far from here. -/
def repoOfColumns? (upstream fork : Option String) : Except String (Option RepoPair) :=
  match upstream, fork with
  | none,   none   => .ok none
  | some u, some f => do
    let upstream ← Repository.parse u
    let fork     ← Repository.parse f
    return some { upstream, fork }
  | some _, none   => .error "'upstream' is set but 'fork' is not"
  | none,   some _ => .error "'fork' is set but 'upstream' is not"

/-- A taxis id as its column value: the decimal string its `ToString` writes. -/
def issueIdColumn (id : Option Taxis.IssueId) : Option String :=
  id.map Taxis.IssueId.toString

/-- Read a taxis id back, naming the field when the column is not a number. -/
def issueIdOfColumn? (field : String) : Option String → Except String (Option Taxis.IssueId)
  | none   => .ok none
  | some s =>
    match Taxis.IssueId.parse? s with
    | some id => .ok (some id)
    | none    => .error s!"{field}: not an issue id: '{s}'"


/-- A JSON document as its column value, exactly as it was handed over: a queue entry's task
    input and output are whatever the task's declared types say, so there is nothing to decode
    them into here and nothing that should be lost on the way through. -/
def rawJsonColumn (j : Option Lean.Json) : Option String :=
  j.map Lean.Json.compress

/-- Read such a document back. -/
def rawJsonOfColumn? (field : String) : Option String → Except String (Option Json)
  | none   => .ok none
  | some s => (Json.parse s).mapError (fun e => s!"{field}: invalid JSON: {e}") |>.map some

/-- A `Nat` column value. `Nat` is not a column type; `int` is, and orchestra's counters —
    priorities, slots, sequence numbers — are naturals that no arithmetic here takes below zero. -/
def natColumn (n : Nat) : Int := Int.ofNat n

/-- The lower bound a paged listing compares its timestamp column against.

    Timestamps are the RFC 3339 UTC strings the records carry, and that format orders
    lexicographically, so "created at or after this instant" is a string comparison against
    `Time.secsToIso8601`. No filter at all is the empty string: no timestamp is empty, so every
    row is at or above it, and a listing stays one query of one shape instead of two spellings
    picked at runtime. -/
def sinceBound (since? : Option Int) : String :=
  match since? with
  | none   => ""
  | some s => Time.secsToIso8601 s

/-- Convert the rows of a listing, reporting the ones that do not convert and leaving them out.

    A row this build cannot read is one record missing from a listing, which is what a file that
    did not parse has always been. Refusing the whole listing instead would mean one row written
    by a newer orchestra hiding every other task in the history. -/
def keepConvertible {ρ α : Type} (what : String) (rowId : ρ → String)
    (conv : ρ → Except String α) (rows : Array ρ) : IO (Array α) := do
  let mut out : Array α := #[]
  for row in rows do
    match conv row with
    | .ok x    => out := out.push x
    | .error e => IO.eprintln s!"[orchestra] skipping {what} '{rowId row}': {e}"
  return out

end Orchestra.Store
