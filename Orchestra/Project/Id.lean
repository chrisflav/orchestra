import Lean.Data.Json

open Lean (Json FromJson ToJson)

namespace Orchestra.Project

/-! # Type-safe identifiers

`ProjectId` and `IssueId` are one-field wrappers around `String`. They live
in their own tiny module so that low-level types in `Orchestra.Config` (such
as `IOTask`) can depend on them without pulling in the rest of the project
machinery — `Project.Basic` itself imports `Config`, so the IDs must be
strictly upstream of it.

Treat the wrappers as opaque: only reach for `value` when you really need a
raw string (filesystem paths, JSON crossing an MCP boundary). -/

structure ProjectId where
  value : String
deriving BEq, DecidableEq, Hashable, Repr, Inhabited

structure IssueId where
  value : String
deriving BEq, DecidableEq, Hashable, Repr, Inhabited

def ProjectId.toString (p : ProjectId) : String := p.value
def IssueId.toString   (i : IssueId)   : String := i.value

instance : ToString ProjectId where toString := ProjectId.toString
instance : ToString IssueId   where toString := IssueId.toString

instance : ToJson ProjectId where toJson p := Json.str p.value
instance : ToJson IssueId   where toJson i := Json.str i.value

instance : FromJson ProjectId where
  fromJson? j := do let s ← FromJson.fromJson? (α := String) j; return ⟨s⟩
instance : FromJson IssueId where
  fromJson? j := do let s ← FromJson.fromJson? (α := String) j; return ⟨s⟩

end Orchestra.Project
