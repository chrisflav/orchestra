import OrchestraTest.TestM
import Orchestra

open Lean (Json)
open Orchestra
open Orchestra.StreamFormat
open Orchestra.Interactive

/-!
# The wire between a session and its agent

These assert against the shapes Claude Code's own streaming-input parser accepts. It admits two
message types and rejects everything else, requires `message.role` to be `"user"` on a user
turn, and requires a `request` object on a control request. A turn that fails any of those does
not produce a bad answer — the CLI writes a parse error and exits, taking the session with it.
-/

private def field (line key : String) : Option Json :=
  (Json.parse line).toOption |>.bind (·.getObjVal? key |>.toOption)

private def str (line key : String) : String :=
  match field line key with
  | some (.str s) => s
  | _             => ""

@[test]
def aTurnIsAUserMessageTheParserAccepts : Test := do
  let line := Wire.userTurn "add a test for the retry path"
  TestM.assertEqual (str line "type") "user"
    (msg := "the parser admits only 'user' and 'control_request'")
  match field line "message" with
  | some msg => do
    TestM.assertEqual (msg.getObjValAs? String "role" |>.toOption |>.getD "") "user"
      (msg := "the parser requires role 'user' on a user message")
    TestM.assertEqual (msg.getObjValAs? String "content" |>.toOption |>.getD "")
      "add a test for the retry path" (msg := "the text rides as the content")
  | none => TestM.fail "a user turn must carry a message"

@[test]
def aTurnIsOneLine : Test := do
  -- The agent reads its input a line at a time. A turn carrying a newline of its own would be
  -- read as two, the second of them not JSON, which the CLI answers by exiting.
  let line := Wire.userTurn "first line\nsecond line\n\nand a paragraph"
  TestM.assertEqual (line.splitOn "\n").length 1
    (msg := s!"a multi-line turn is still one line on the wire; got {line}")
  match field line "message" with
  | some msg =>
    TestM.assertEqual (msg.getObjValAs? String "content" |>.toOption |>.getD "")
      "first line\nsecond line\n\nand a paragraph"
      (msg := "and the newlines survive the round trip")
  | none => TestM.fail "a user turn must carry a message"

@[test]
def interruptIsAControlRequestCarryingItsId : Test := do
  let line := Wire.interrupt "req-1"
  TestM.assertEqual (str line "type") "control_request" (msg := "the second admitted type")
  TestM.assertEqual (str line "request_id") "req-1"
    (msg := "the id the control response is correlated by")
  match field line "request" with
  | some r =>
    -- The parser rejects a control request with no `request` object, and the message loop
    -- dispatches on this subtype to abort the turn in flight.
    TestM.assertEqual (r.getObjValAs? String "subtype" |>.toOption |>.getD "") "interrupt"
      (msg := "the subtype the message loop aborts on")
  | none => TestM.fail "the parser requires a request object"

@[test]
def aResultEndsTheTurnWhateverTheOutcome : Test := do
  -- Every result ends the turn. Which *kind* of ending it was is the subtype's business, read
  -- by the session manager; a turn that ended badly is still a turn that ended.
  TestM.assert (Wire.endsTurn (.result .success none none none ""))
    (msg := "success ends it")
  TestM.assert (Wire.endsTurn (.result .errorMaxBudgetUsd none none none ""))
    (msg := "an exhausted budget ends it too")
  TestM.assert (Wire.endsTurn (.result (.error "boom") none none none ""))
    (msg := "and so does an error")
  TestM.assert (!Wire.endsTurn (.assistant (.text "still working")))
    (msg := "but nothing the agent says mid-turn does")

@[test]
def bookkeepingStaysOutOfTheTranscript : Test := do
  -- These would otherwise land in the conversation as an unknown event with no content.
  TestM.assert (Wire.isNoise (.unknown "keep_alive"))
    (msg := "the stream saying it is still there")
  TestM.assert (Wire.isNoise (.unknown "control_response"))
    (msg := "the answer to something the daemon asked")
  TestM.assert (!Wire.isNoise (.unknown "some_new_event_type"))
    (msg := "an event type we do not know is not noise — it is news, and it is kept")
  TestM.assert (!Wire.isNoise (.assistant (.text "hello")))
    (msg := "and nothing the agent actually said is noise")
