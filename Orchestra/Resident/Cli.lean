import Orchestra.Utils.Files

/-!
# `orchestra resident …`

What the command line asks of a resident, parsed apart from `Main` so it can be tested. Everything
the commands do goes through the API, as `orchestra chat` does: the daemon holds the residents, and
the CLI may be on another machine.
-/

namespace Orchestra.Resident

/-- One `orchestra resident` invocation. -/
inductive Command where
  | list
  | show (name : String)
  | pause (name : String)
  | resume (name : String)
  | reset (name : String)
  | inbox (name : String)
  /-- Say something to it, through its inbox. -/
  | send (name : String) (text : String)
deriving Repr, BEq

/-- The verbs, in the order the usage line names them. -/
def verbs : List String := ["list", "show", "pause", "resume", "reset", "inbox", "send"]

/-- Read `orchestra resident <verb> [<name>] [<text>…]`. -/
def parseCommand (args : List String) : Except String Command := do
  let usage := s!"usage: orchestra resident <{"|".intercalate verbs}> [name]"
  let named (verb : String) (rest : List String) (k : String → Command) : Except String Command :=
    match rest with
    | [name] => do Utils.checkConfigName "resident" name; pure (k name)
    | []     => throw s!"orchestra resident {verb} needs the resident's name"
    | _      => throw s!"orchestra resident {verb} takes one name, not {rest.length}"
  match args with
  | [] => throw usage
  | "list" :: [] => pure .list
  | "list" :: _ => throw "orchestra resident list takes no name"
  | "show" :: rest => named "show" rest .show
  | "pause" :: rest => named "pause" rest .pause
  | "resume" :: rest => named "resume" rest .resume
  | "reset" :: rest => named "reset" rest .reset
  | "inbox" :: rest => named "inbox" rest .inbox
  | "send" :: name :: words@(_ :: _) => do
    Utils.checkConfigName "resident" name
    let text := " ".intercalate words
    if text.trimAscii.isEmpty then throw "the message is empty"
    pure (.send name text)
  | "send" :: _ => throw "usage: orchestra resident send <name> <message…>"
  | verb :: _ => throw s!"unknown resident command '{verb}'; {usage}"

/-- The API path a command reads or writes, and whether it is a write. -/
def Command.route : Command → String × Bool
  | .list       => ("/api/v1/residents", false)
  | .show n     => (s!"/api/v1/residents/{n}", false)
  | .inbox n    => (s!"/api/v1/residents/{n}/inbox", false)
  | .pause n    => (s!"/api/v1/residents/{n}/pause", true)
  | .resume n   => (s!"/api/v1/residents/{n}/resume", true)
  | .reset n    => (s!"/api/v1/residents/{n}/reset", true)
  | .send n _   => (s!"/api/v1/residents/{n}/messages", true)

end Orchestra.Resident
