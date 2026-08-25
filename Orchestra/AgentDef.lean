import Orchestra.StreamFormat
import Orchestra.Config
import Lean.Data.Json

open Lean (Json)
open Orchestra.StreamFormat

namespace Orchestra

/-- Everything a bidirectional streaming invocation needs to know.

    A structure rather than eight more positional parameters, because that is what the two
    existing builders already cost to read: `buildArgs` takes six `Option String`s in a row, and
    which one is the system prompt is a matter of counting commas. Nothing here is shared with
    those two, so the new field can be spelled the better way without touching them. -/
structure StreamOptions where
  /-- The context string `setupMcp` returned — for most backends, the MCP config path. -/
  mcpContext : String
  pluginDirs : Array String := #[]
  subAgent : Option String := none
  model : Option String := none
  systemPrompt : Option String := none
  /-- Resume a session this backend created earlier. Set when a dead session is revived; the
      agent picks up the conversation rather than starting one. -/
  resume : Option String := none
  /-- Assign the session id up front rather than reading it back out of the stream.

      Worth doing because the alternative only works if the process lives long enough to say
      it: a crash before the first event leaves a session with no id, and so no way to resume
      the conversation it had already started. Ignored by a backend whose CLI cannot be told
      what to call a session. -/
  sessionId : Option String := none
  /-- Maximum spend, in USD, for the whole session — not for one turn. -/
  budget : Float := 20.0
  /-- Ask for partial message chunks as they arrive, for a client that renders text as it is
      typed. Off by default: it multiplies the volume of the transcript several times over. -/
  partialMessages : Bool := false

/-- Describes how to invoke and communicate with a specific coding agent backend. -/
structure AgentDef where
  /-- The executable name (e.g., "claude"). -/
  command : String
  /-- Filesystem paths the agent needs inside the sandbox. -/
  sandboxPaths : SandboxPaths
  /-- Set up agent-specific infrastructure before launch (e.g., write MCP config files).
      Receives the MCP server port, optional model override, and optional appended system prompt.
      Returns a context string (passed to buildArgs, extractSessionId, and cleanup)
      and any extra sandbox env vars. -/
  setupMcp : UInt16 → Option String → Option String → IO (String × Array (String × Option String))
  /-- Build command-line args for a specific invocation.
      Receives: context string from setupMcp, plugin directories, sub-agent name,
      model override, appended system prompt, session ID to resume, budget in USD,
      and the user prompt. -/
  buildArgs : String → Array String → Option String → Option String → Option String → Option String → Float → String
            → Array String
  /-- Build command-line args for an interactive (TUI) invocation.
      Same parameters as `buildArgs` minus the prompt; omits headless-only flags
      like `--print` and `--output-format=stream-json`. -/
  buildInteractiveArgs : String → Array String → Option String → Option String → Option String → Option String → Float
                       → Array String
  /-- Build command-line args for a **bidirectional streaming** invocation: one process that
      reads user turns from stdin as they arrive and streams its events on stdout, rather than
      one process per prompt.

      `none` — the default — for a backend whose CLI has no such mode. That is not a detail to
      paper over: a caller that asks such a backend to host an interactive session is told so
      and refused, rather than silently given something else. -/
  buildStreamArgs : StreamOptions → Option (Array String) := fun _ => none
  /-- Extra command-line args that hold the run to a *goal*: a condition the agent must not stop
      before it satisfies, judged by a second model call rather than by the agent itself.

      Receives the goal condition on its own — never the task prompt, which carries the issue
      body and its whole comment thread and would make the judge's job unanswerable.

      `none` for a backend whose CLI has no such mechanism. The caller
      (`Sandbox.launchAgent`) then says so once and runs without a goal, rather than passing a
      flag the backend would reject. -/
  goalArgs : String → Option (Array String) := fun _ => none
  /-- Parse one line of the agent's stdout stream output into the events it carries.

      An array because one line is not one event: an assistant message carries a list of content
      items, and rendering only one of them loses the rest. An empty array is a line with
      nothing to show — one that does not parse, or one that is deliberately suppressed. -/
  parseOutputLine : String → Array Event
  /-- Try to extract the session ID after the run.
      Used for agents that don't emit the session ID in the output stream.
      Receives the context string from setupMcp. -/
  extractSessionId : String → IO (Option String)
  /-- Clean up any resources created by setupMcp. -/
  cleanup : String → IO Unit
  /-- Return true if the agent exited because it hit a usage/quota limit.
      Receives the process exit code and the full stderr content. -/
  isUsageLimitError : UInt32 → String → Bool
  /-- Map an authentication source to environment variable names for this backend.
      Called to determine which environment variables to inject when a specific auth source is selected.
      Authentication kinds not supported by the backend produce an empty array. -/
  envVarsOfAuthSource : AuthSource → Array (String × String)
  /-- Whether two tasks on this backend may run at the same time in one daemon.

      False for backends whose CLI keeps per-run state at a fixed, process-global location —
      a hard-coded port, or a config file under `$HOME` that `setupMcp` overwrites and
      `cleanup` restores. Two concurrent runs then read each other's configuration, so an
      agent can end up talking to another task's MCP server and acting on its repository and
      issue. The queue daemon runs such backends exclusively; see `Main.queueStartHandler`. -/
  parallelSafe : Bool := true

namespace AgentDef

/-- Case-insensitive substring check. -/
def containsCI (haystack needle : String) : Bool :=
  (haystack.toLower.splitOn needle.toLower).length > 1

/-- Shared usage-limit detection patterns used by all backends.

    Matched against the agent's stderr *and* the text of its final result event, because the two
    backends differ in where they say it: an API-key run reports the 429 on stderr, while a
    subscription run reports it through the output stream as a result the CLI marks as an error
    (`"You've reached your Fable 5 limit."`). Reading only one of the two misses half the cases.

    The patterns are deliberately phrase-level rather than word-level. `"limit"` alone would fire
    on an agent that merely discussed a rate limiter; each phrase here is one a provider writes
    and ordinary output does not. -/
def stdUsageLimitError (exitCode : UInt32) (output : String) : Bool :=
  exitCode != 0 && (
    let s := output.toLower
    containsCI s "usage limit" ||
    containsCI s "rate_limit_error" ||
    containsCI s "rate limit exceeded" ||
    containsCI s "reached your" ||
    containsCI s "limit reached" ||
    containsCI s "exceed your" ||
    containsCI s "exceeded your" ||
    containsCI s "insufficient credits" ||
    containsCI s "credit balance" ||
    -- anthropics/claude-code#79597, verbatim: the client refuses a model the subscription covers
    -- because a setup-token cannot state the plan, and says it "requires usage credits". Without
    -- this the run is not a limit at all — it comes back `failed` rather than `unfinished`,
    -- nothing is recorded against the source, and the next queued task is dispatched straight
    -- into it again. Not a bare "usage credits": the ordinary limit message ends "Run
    -- /usage-credits to continue", which is a window with a reset and not this.
    containsCI s "requires usage credits")

/-! ## What the limit was about

`stdUsageLimitError` answers whether a run hit a limit. That is not enough to record one, because
the answer decides how much of an account to close: a limit the provider attributed to one model
family leaves the rest of the account perfectly runnable, an account-wide window closes all of it,
and an entitlement problem is not a window at all — it does not reset on a clock, so waiting for it
is waiting forever.

The message says which. It was already being read and thrown away. -/

/-- Which limit a usage-limit message describes. -/
inductive LimitScope where
  /-- The provider named a model family: "You've reached your Fable 5 limit." -/
  | family (name : String)
  /-- A window that names no model — a session or weekly total, or a transport-level 429. -/
  | account
  /-- Credits or entitlement rather than a window, carrying the family when one was named. -/
  | credits (family : Option String)
  /-- A limit phrase matched but nothing in it says which limit. The caller falls back to
      whatever it already assumed, so classifying is never worse than not classifying. -/
  | unknown
deriving Repr, BEq, Inhabited

/-- The families a limit message can name, as the lowercase needle to look for and the display
    name to record.

    Display names rather than model ids, because that is what the usage store scopes by and what
    the provider writes: the message says "Fable", the task asked for `claude-fable-5`, and
    `Usage.modelMatchesScope` is what reconciles the two. -/
def limitFamilies : List (String × String) :=
  [("fable", "Fable"), ("opus", "Opus"), ("sonnet", "Sonnet"), ("haiku", "Haiku")]

/-- How much text after a marker phrase may name the family.

    "reached your Fable 5 limit" puts twelve characters between the two. The text being classified
    is not a provider message but a whole run's stderr with the final result appended, so an
    unbounded span is the difference between reading a limit message and reading a transcript —
    and a transcript mentions model names routinely. -/
def familySpanChars : Nat := 40

/-- How much of the classified text is worth reading at all.

    A limit message is the last thing said, so the tail is where it is. Clamping here is what
    keeps the rest of this file cheap: the caller may hand it a whole failing run's stderr, which
    is measured in megabytes, and every helper below would otherwise walk all of it. -/
def classifyTailChars : Nat := 4096

-- `String.take` and `String.takeEnd` walk only the characters they keep and return a slice
-- without copying the rest, which is the point: the input here can be a whole failing run's
-- stderr, and clamping it by materialising a `List Char` of every character first would cost more
-- than the scan it exists to avoid.
private def clampAfter (s : String) : String :=
  (s.take familySpanChars).toString

private def clampBefore (s : String) : String :=
  (s.takeEnd familySpanChars).toString

/-- The last `classifyTailChars` characters of `s`: where a limit message is, if there is one. -/
private def clampTail (s : String) : String :=
  (s.takeEnd classifyTailChars).toString

/-- Every occurrence of `marker` in `s`, as (position, the text before it, the text after it),
    latest first.

    Position is the whole point. The provider's message is the *last* thing in the text, so a
    limit notice the run recovered from earlier ("You've reached your Opus limit, now using
    Sonnet") must not outrank the one that actually ended it. -/
private def markerHits (s marker : String) : List (Nat × String × String) :=
  match s.splitOn marker with
  | []      => []
  | p :: ps =>
    (ps.foldl (init := (p.length, p, ([] : List (Nat × String × String))))
      fun (off, prev, acc) seg =>
        (off + marker.length + seg.length, seg, (off, prev, seg) :: acc)).2.2

private def familyIn (span : String) : Option String :=
  (limitFamilies.find? fun (needle, _) => containsCI span needle).map (·.2)

/-- Phrases that mark a limit being reported somewhere in the text.

    Both halves of the classification hang off these. The *last* occurrence of any of them is the
    report that ended the run, and everything else is read from a short window around that one
    occurrence — never from the text at large. An agent that discussed a model family, or a credit
    balance, earlier in its run must not get to decide how much of an account gets closed.

    Every phrase `stdUsageLimitError` detects on belongs here, or a message it recognises would
    produce no window at all and classify as `unknown` — which falls back to the model the task
    asked for, and so records an *account-wide* limit like "5-hour limit reached" as a block on
    whatever family happened to be running.

    "reached the " is deliberately *not* here, though `familyNamedIn` still opens spans on it
    inside a window. It is the one phrase common in ordinary prose ("reached the maximum number
    of retries"), so letting it decide *where* the deciding report is would let stray output move
    the window off the real message. -/
private def limitMarkers : List String :=
  ["reached your ", "exceeded your ", "exceed your ",
   "usage limit", "weekly limit", "rate limit", "limit reached", "rate_limit_error",
   "insufficient credits", "credit balance", "requires usage credits"]

/-- The window around the deciding limit report: `familySpanChars` of context either side of the
    last marker occurrence, or `none` when nothing in the text reports a limit at all. -/
private def decidingWindow (lowered : String) : Option String :=
  let hits := limitMarkers.flatMap fun m =>
    (markerHits lowered m).map fun (pos, before, after) =>
      (pos, clampBefore before ++ m ++ clampAfter after)
  ((hits.toArray.qsort fun a b => a.1 > b.1).toList.head?).map (·.2)

/-- The family named between "your" and "limit" inside `window`, if it names one.

    Bounded twice over: the window is already short, and within it only the span between the
    marker and the next "limit" is searched. Both bounds earn their keep — a bare search for
    "fable" would fire on an agent that merely mentioned the model, and taking everything after a
    marker when no "limit" follows would hand the search the rest of the window. -/
def familyNamedIn (window : String) : Option String :=
  let spans := ["reached your ", "exceeded your ", "exceed your ", "reached the "].flatMap
    (markerHits window)
  let ranked := spans.toArray.qsort (fun a b => a.1 > b.1)
  let lastSpan := ranked.toList.findSome? fun (_, _, after) =>
    match (clampAfter after).splitOn "limit" with
    | span :: _ :: _ => some span
    | _              => none
  lastSpan.bind familyIn

/-- The family named just before `marker`, for the phrasing that puts it there: "Fable requires
    usage credits". Bounded the same way and for the same reason. -/
def familyBefore (window marker : String) : Option String :=
  match (window.splitOn marker).dropLast.reverse with
  | []            => none
  | before :: _   => familyIn (clampBefore before)

/-- Classify a usage-limit message. Only meaningful once `isUsageLimitError` has said there is
    something to classify; on anything else it answers `unknown`, which is the caller's cue to
    keep whatever it already believed. -/
def classifyUsageLimit (output : String) : LimitScope :=
  -- Clamped and lowercased once, here, rather than in each helper: every read below scans this
  -- string, and the caller's input can be a whole run's stderr.
  let s := (clampTail output).toLower
  match decidingWindow s with
  | none => .unknown
  | some w =>
  let named := familyNamedIn w
  -- Read from the window, not the text. A credits phrase anywhere in a transcript used to decide
  -- this, which meant an agent that merely wrote "credit balance" in its summary cost the whole
  -- account six hours on a limit that was really a one-hour window — and threw away the reset
  -- time that would have lifted it.
  --
  -- "requires usage credits", not a bare "usage credits": the ordinary subscription-limit message
  -- ends "Run /usage-credits to continue or switch models", and that is a window with a reset,
  -- not a balance problem. Only the hyphen separates the two today; the phrase does not.
  if containsCI w "insufficient credits" || containsCI w "credit balance"
     || containsCI w "requires usage credits" then
    -- A credits message does not always route the family through a "your … limit" span: the
    -- entitlement refusal reads "Fable requires usage credits", with the family in front.
    .credits (named.orElse fun _ => familyBefore w "requires usage credits")
  else match named with
  | some f => .family f
  | none   =>
    -- A message that names a limit but no family is about the account. "rate limit" is safe to
    -- read that way *here* and would not be safe on its own: this is reached only from inside a
    -- window built around a limit report, so the agent that merely discussed a rate limiter never
    -- arrives. Ambiguity resolves toward the account on purpose — over-blocking costs an hour of
    -- one source, under-blocking costs a clone and a run per queued task.
    if containsCI w "rate_limit_error" || containsCI w "rate limit"
       || containsCI w "usage limit" || containsCI w "weekly limit"
       || containsCI w "limit reached" then .account
    else .unknown

end AgentDef

end Orchestra
