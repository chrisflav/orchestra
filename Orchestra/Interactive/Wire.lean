import Orchestra.StreamFormat
import Lean.Data.Json

open Lean (Json)

/-!
# The wire between a session and its agent

Everything that knows what a turn looks like on the agent's stdin, how a turn ends, and how one
is interrupted. Nothing else does.

That isolation is the point. The two things here are the details most likely to move with a CLI
release, and confining them to one small module means a change is a change to this file rather
than to the session manager, the daemon and the API at once.

## What this is checked against

The shapes below are the ones Claude Code's own streaming-input parser accepts, read off the
installed CLI rather than assumed. Its parser admits exactly two message types and rejects the
rest — "Expected message type 'user' or 'control'" — then, for a user message, requires
`message.role` to be `"user"`, and for a control request requires a `request` object. Its message
loop handles `interrupt` by aborting the turn in flight and answering with a `control_response`,
which is what makes an interrupt something other than killing the process.

## What is deliberately not here

Permission prompts. A session runs with permissions skipped, as every other orchestra launch
does, so the `can_use_tool` control request the CLI can send in the other direction never
arrives. Surfacing approvals to the person in the chat is the obvious next thing this module
would grow, and it would grow here.
-/

namespace Orchestra.Interactive.Wire

open Orchestra.StreamFormat

/-- One user turn, as the line to write to the agent's stdin.

    `parent_tool_use_id` and `session_id` are sent because the CLI's own clients send them; the
    parser does not require either. Content goes as a string rather than a block array, which the
    same parser accepts and which cannot be malformed in a way a block array can. -/
def userTurn (text : String) : String :=
  Json.compress <| Json.mkObj [
    ("type", .str "user"),
    ("message", Json.mkObj [("role", .str "user"), ("content", .str text)]),
    ("parent_tool_use_id", Json.null),
    ("session_id", .str "")
  ]

/-- Ask the agent to abandon the turn it is working on, keeping the process and the conversation.

    `requestId` correlates the `control_response` the CLI answers with. Nothing here waits for
    that answer: an interrupt is over when the turn ends, which arrives as a result event on the
    stream like any other turn ending, and a caller that blocked on the acknowledgement would be
    waiting on the wrong thing. -/
def interrupt (requestId : String) : String :=
  Json.compress <| Json.mkObj [
    ("type", .str "control_request"),
    ("request_id", .str requestId),
    ("request", Json.mkObj [("subtype", .str "interrupt")])
  ]

/-- Whether this event ends the turn.

    The agent says so with a result event, whatever the outcome — success, an error, a budget
    exhausted. All of them end the turn; what *kind* of ending it was is the subtype's business,
    read by the caller, not this predicate's. -/
def endsTurn : Event → Bool
  | .result .. => true
  | _          => false

/-- Whether this event is bookkeeping the transcript should not carry.

    `keep_alive` is the stream saying it is still there, and `control_response` is the answer to
    something the daemon asked — neither is anything the person reading the conversation asked
    for, and both would otherwise land in the transcript as an unknown event with no content. -/
def isNoise : Event → Bool
  | .unknown t => t == "keep_alive" || t == "control_response" || t == "control_cancel_request"
  | _          => false

end Orchestra.Interactive.Wire
