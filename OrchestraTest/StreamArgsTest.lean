import OrchestraTest.TestM
import Orchestra

open Orchestra
open Orchestra.AgentDef

/-!
# The bidirectional streaming invocation

What an interactive session launches. The flags below are not decoration: each one is what makes
the process a conversation rather than a one-shot run, and dropping any of them changes what the
CLI does rather than merely how it looks.
-/

private def streamArgs (o : StreamOptions) : Array String :=
  (AgentDef.claude.buildStreamArgs o).getD #[]

/-- Whether `flag` appears with `value` right after it. -/
private def hasPair (args : Array String) (flag value : String) : Bool :=
  match args.findIdx? (· == flag) with
  | some i => args[i + 1]? == some value
  | none   => false

@[test]
def streamingIsPrintPlusBothStreamFormats : Test := do
  let args := streamArgs { mcpContext := "/tmp/mcp.json" }
  -- `--input-format stream-json` is the whole difference between a process that takes one
  -- prompt and exits and one that reads turns for as long as the session lives. The CLI accepts
  -- it only alongside `--print`.
  TestM.assert (args.contains "--print") (msg := "--print is required for either stream format")
  TestM.assert (hasPair args "--input-format" "stream-json")
    (msg := "turns arrive on stdin as JSON")
  TestM.assert (hasPair args "--output-format" "stream-json")
    (msg := "events leave on stdout as JSON")
  TestM.assert (args.contains "--verbose") (msg := "--verbose, or the stream carries little")

@[test]
def userTurnsAreReplayedIntoTheStream : Test := do
  let args := streamArgs { mcpContext := "/tmp/mcp.json" }
  -- Without this the transcript has two writers — the daemon appending the turn it just sent,
  -- and the pump appending what the agent says about it — racing to get the order right. With
  -- it there is one writer, and the order is the agent's own.
  TestM.assert (args.contains "--replay-user-messages")
    (msg := "user turns come back through the same stream")

@[test]
def thereIsNoPromptArgument : Test := do
  let args := streamArgs { mcpContext := "/tmp/mcp.json" }
  TestM.assert (!args.contains "-p")
    (msg := "the prompt arrives on stdin; a -p here would end the session after one turn")

@[test]
def aFreshSessionIsNamedAndAResumedOneIsNot : Test := do
  let fresh := streamArgs { mcpContext := "m", sessionId := some "uuid-1" }
  TestM.assert (hasPair fresh "--session-id" "uuid-1")
    (msg := "a fresh session is told its own id, so a crash before the first event still \
resumes")
  -- Resuming names a session that already exists; passing both would be asking the CLI to call
  -- one session two things.
  let resumed := streamArgs { mcpContext := "m", sessionId := some "uuid-1",
                              resume := some "uuid-0" }
  TestM.assert (hasPair resumed "--resume" "uuid-0") (msg := "resume wins")
  TestM.assert (!resumed.contains "--session-id")
    (msg := "and the assignment is dropped rather than sent alongside it")

@[test]
def partialMessagesAreOptOut : Test := do
  TestM.assert (!(streamArgs { mcpContext := "m" }).contains "--include-partial-messages")
    (msg := "off by default: it multiplies the volume of the transcript")
  TestM.assert ((streamArgs { mcpContext := "m", partialMessages := true }).contains
      "--include-partial-messages")
    (msg := "on when a client asks to render text as it is typed")

@[test]
def theBudgetBoundsTheWholeSession : Test := do
  let args := streamArgs { mcpContext := "m", budget := 20.0 }
  TestM.assert (args.contains "--max-budget-usd")
    (msg := "a conversation still has a ceiling")

@[test]
def backendsWithoutAStreamingModeSaySoRatherThanSubstitute : Test := do
  let o : StreamOptions := { mcpContext := "m" }
  -- The point of `none` is that the caller refuses the session and names the backend. A default
  -- that fell back to the one-shot args would give the caller a process that answers the first
  -- turn and exits, which looks like a session that ended on its own.
  TestM.assert (AgentDef.vibe.buildStreamArgs o).isNone (msg := "vibe has no streaming input")
  TestM.assert (AgentDef.opencode.buildStreamArgs o).isNone (msg := "opencode has none")
  TestM.assert (AgentDef.pi.buildStreamArgs o).isNone (msg := "pi has none")
  TestM.assert (AgentDef.claude.buildStreamArgs o).isSome (msg := "claude does")
