# routing around a model-scoped usage limit

A `weekly_scoped` limit is the one limit orchestra cannot see coming. The
`anthropic-ratelimit-unified-*` headers a `setup-token` can reach carry the 5-hour session window
and the 7-day account window and nothing else, so an exhausted Fable quota announces itself the
only way it can: a run walks into it. `GET /api/oauth/usage` would say so in advance, but a
setup-token has inference scope and no profile scope and is answered with 403 — the same gap that
makes Claude Code refuse Fable on a Max account
([anthropics/claude-code#79597](https://github.com/anthropics/claude-code/issues/79597)) and that
`CLAUDE_CODE_SUBSCRIPTION_TYPE` exists to paper over.

This document is about what to do given that blindness. It is deliberately not a proposal for one
system: six options are laid out, they close different holes, and the order in which they are worth
doing is not the order of their ambition.

The starting position is better than it looks. Availability is *already* a question about
`(source, model)` rather than about a source alone — `availabilityOf` (`Orchestra/Usage.lean:669`)
takes the model the task wants and answers for that pairing, and an exhausted Fable window
correctly leaves Sonnet work on the same account runnable. A pool of accounts is already walked
`ordered` or `distribute`, with the winner stamped under the daemon's claim lock so two workers
cannot both pick the same least-used account. An entry whose every source is limited is already
reported as `wait` and *stays pending*, so it runs the moment a window resets without anyone
re-queueing it. And two writers already cover each other: the five-minute poll sees a limit coming
and knows its reset time, while `markLimited` records one that a run actually hit.

What follows are holes in that design, not a missing subsystem.

## the gaps

### G1 — one block slot per account

```
Orchestra/Usage.lean:355     block : Option Block := none
```

`SourceState` holds exactly one block. But "Fable is spent until Friday" and "the 5-hour session
window is spent until 14:00" are two independent facts about the same account, and the second
`markLimited` overwrites the first. When the session block expires, the Fable block goes with it,
and the account reads as Fable-capable again.

### G2 — a successful run erases what the account cannot do

```
Orchestra/Usage.lean:805     def markOk (backend label : String) : IO Unit
```

`markOk` clears `block` outright after any completed run. Its reasoning is sound as far as it goes
— the block was a guess about a window nobody could see the end of, and a completed run is proof it
has passed — but it does not ask *which* window the run proved anything about. A Sonnet task
finishing on account A wipes "Fable exhausted on A", so the very next Fable task dispatches straight
into the wall, burning a clone, a token mint and a run to rediscover a fact orchestra already knew.

For the case this document is about, this is the largest hole and the smallest fix.

### G3 — no fallback: `model` is one string, end to end

```
Orchestra/Listener.lean:235    model : Option String := none    (role / listener action)
Orchestra/Queue.lean:103       model : Option String := none    (queue entry)
Orchestra/TaskStore.lean:56    model : Option String := none    (task record)
Orchestra/Config.lean:380      model : Option String := none    (task file)
```

One `Option String` threads unchanged from the role that names it to the CLI's `--model` flag.
There is no way to express "Fable if you can, Opus otherwise", so when every account's Fable window
is spent the work waits — and a `weekly_scoped` reset can be days out, not hours.

### G4 — a limit hit mid-run strands the task

A limited run lands as `.unfinished` (`Orchestra/TaskRunner.lean:694`), which is the honest status:
the work is not done and not failed. But the daemon only ever claims entries in `.pending`
(`Orchestra/Queue.lean:336`), so an `.unfinished` entry sits there until somebody types
`orchestra queue retry`. And that handler (`Main.lean:1042`) rebuilds the entry from
`repo, mode, prompt, goal, agent, systemPrompt, backend, model, continuesFrom, series, configPath,
priority` — with no `authSources`, `authSource` or `authMode`, so the retry silently loses its pool
and falls back to the backend default.

Note the asymmetry with the `wait` path: a limit discovered *before* dispatch costs a delay, and a
limit discovered *during* a run costs human attention.

### G5 — the scope of an observed hit is a guess

```
Orchestra/TaskRunner.lean:666    Usage.markLimited backendName label ioTask.model "…"
```

`markLimited` is handed `ioTask.model` — the model the task *asked for*, not the one the provider
named. Both directions are wrong:

- A task that named no model gets `Block.model = none`, which `availabilityOf` treats as covering
  every model, so a Fable-scoped cause idles the whole account.
- A Fable task that actually tripped the account-wide 5-hour window gets a Fable-scoped block, so
  Opus work keeps dispatching into an account with nothing left. The poll papers over this within
  five minutes — but not at all when `poll_usage` is `false`, which is a supported configuration
  precisely because polling now costs a real request.

The information needed to get this right is already in hand and thrown away.
`stdUsageLimitError` (`Orchestra/AgentDef.lean:123`) reads the provider's own text — it matches
`"reached your"` specifically so that `"You've reached your Fable 5 limit."` is caught — and then
returns a bare `Bool`, discarding the one detail that says which limit was hit.

### G6 — the poll can only see two windows

```
Orchestra/Usage.lean:902     def probeModel : String := "claude-haiku-4-5"
```

`parseUnifiedHeaders` reads `5h` and `7d` and hardcodes `scopeModel := none`, with a comment saying
plainly that the headers carry no per-model-family window. Whether probing *per model* would
surface a scoped one is exactly what the untracked `probe-limits.sh` in the repo root was written to
answer, and that answer decides whether option F below is thirty lines or fiction.

## the options

### A — store blocks as a set, keyed by scope

Replace `block : Option Block` with `blocks : Array Block`. Availability becomes "is any live
block's scope covering this model". `markLimited` upserts by scope key instead of overwriting, and
`markOk` retires only the blocks whose scope covers the model that just ran — so a Sonnet success
clears the account-wide block and the Sonnet block and leaves the Fable one standing.

- **fixes** G1, G2
- **cost** one struct, three call sites, and a tolerant `FromJson` that lifts a legacy single
  `"block"` object into a one-element array. No new config, no user-visible policy change.
- **risk** low. The state files under `<data>/usage/` are a regenerable cache, so a tolerant reader
  is the whole migration.

Everything else here is more correct once this lands, because everything else reads state that
currently forgets. Worth doing first regardless of which direction the rest goes.

### B — classify the hit before recording it

Turn `isUsageLimitError` from `Bool` into a verdict:

```
inductive LimitVerdict
  | none
  | scoped (model : String)   -- "You've reached your Fable 5 limit."
  | account                   -- a session or weekly-all window
  | credits                   -- "insufficient credits" / "credit balance"
  | unknown                   -- matched a phrase, could not attribute it
```

The text is already in hand: `LaunchResult.resultText` carries the result event, and
`Orchestra/Sandbox.lean:471` already feeds the combined output to the matcher. Record the block with
the scope the provider named rather than the one the task guessed.

- **fixes** G5. Also separates the #79597 entitlement refusal from a genuine window — that refusal
  never clears on a timer, so blocking for an hour and retrying forever is exactly the wrong
  response to it; it wants a long block and a loud diagnostic instead.
- **cost** an inductive return type, one plumb through `LaunchResult`, per-backend matchers
  unchanged in substance.
- **risk** string matching on provider prose, which changes. Bounded: `unknown` falls back to
  today's behaviour, so it is never worse than now.

### C — a model fallback ladder

Let dispatch search over `(source, model)` pairs instead of sources for a fixed model. Two shapes:

**C1, a config-level family ladder.** One map in `AgentAuthConfig` or beside it:

```json
"model_fallback": { "claude-fable-5": ["claude-opus-5", "claude-sonnet-5"] }
```

Tasks keep naming one model; `resolveLabel` walks the ladder. One place to tune, no churn in any
task file, role or listener.

**C2, a per-task list.** `"model": ["claude-fable-5", "claude-opus-5"]`, or a separate
`model_fallback` field, threaded through role → listener action → queue entry → task record. More
expressive, and five structs plus their JSON round-trips and tests wider.

- **fixes** G3
- **decide** search order is a real policy fork, not an implementation detail: *model-first* (keep
  Fable, change account) and *source-first* (keep the account, drop a tier) give different answers
  whenever both a spare account and a spare tier exist. Make it an explicit knob rather than an
  accident of loop nesting.
- **risk** a silent downgrade is a quality regression nobody can see in the output. Three
  mitigations, all cheap:
  - make it opt-in per role — a review role may well prefer to wait for Fable;
  - record the model actually chosen on the queue entry and the task record, or the dashboard keeps
    reporting the model that was *asked for*;
  - resolve once before attempt 0 and pin it, for the same reason the auth label is pinned there
    (`Orchestra/TaskRunner.lean:609`): the retry loop `--resume`s the same conversation, and moving
    it to a different model part-way through is the same class of mistake as moving it to a
    different account.

### D — put a limited entry back in the queue instead of stranding it

When a run dies on a limit: record the block, then return the entry to `.pending` with a bounded
counter on it (`limitRetries : Nat`) rather than leaving it `.unfinished`. The existing `wait` gate
in `resolveEntryAuth` then holds it until some source can run it, and it goes automatically — which
is the behaviour a pre-dispatch limit already gets. Fix `queue retry` to carry `authSources`,
`authSource` and `authMode` while in there.

- **fixes** G4, and the pool-dropping bug in `queue retry`
- **cost** small: a status change, one counter field, and reuse of machinery that already exists
- **risk** resuming a session on a *different account* is unsafe — the new account has never seen
  that conversation. So an automatic re-dispatch either waits for the same account or starts clean;
  it must not silently move a resumed session. Downgrading the *model* on the same account's
  resumed session is fine.

### E — stop guessing an hour for a weekly window

```
Orchestra/Usage.lean:774     def defaultBackoffSecs : Int := 3600
```

An hour is a reasonable guess for a session window and a badly wrong one for a scoped weekly limit:
an hour later the entry retries, burns another failed run, and repeats all week. Two ways to do
better, and only the first is needed:

**E1, per-kind defaults.** Once B says the hit was scoped-weekly, shape the fallback like a week —
align to the account's already-polled `7d` reset time, which is sitting right there in
`st.limits`, or fall back to a day rather than an hour.

**E2, learn from history.** The `Window` history already recorded and retained for 180 days
(`recordWindows`, `historyRetentionSecs`) is enough to predict the next reset: scoped windows recur
at a stable wall-clock time, so one observation predicts the following one.

- **fixes** the wasted-retry tail of G5
- **cost** E1 is a handful of lines. E2 is a follow-on, not a prerequisite.
- **risk** an over-long block idles an account that has in fact reset. `markOk` already retires a
  block on proof, and `orchestra usage --refresh` is the manual escape.

### F — probe per model family

Poll each family the install actually dispatches and merge whatever scoped windows come back,
turning Fable exhaustion from something discovered by hitting it into something seen coming. This
is the only option that removes the guessing rather than managing it.

- **fixes** G6, and most of G5 with it
- **blocked on** whether the unified headers differ per requested model at all. `probe-limits.sh`
  answers it in one run, for the price of one near-empty request per model. Nothing else in this
  document should wait on that answer.
- **cost** N requests per poll per account instead of one, each nudging the very window it
  measures. At a 300-second interval across three families and several accounts that is real
  traffic. Mitigate by probing only the expensive family, on a longer interval, or only once the
  weekly-all line is close.
- **risk** it may simply not be available. If the headers are model-independent, F is off the table
  and A + B + E carry the case.

## coverage

|                              |  A  |  B  |  C  |  D  |  E  |  F  |
| ---------------------------- | :-: | :-: | :-: | :-: | :-: | :-: |
| G1 one block slot            |  ●  |  –  |  –  |  –  |  –  |  –  |
| G2 success erases scope      |  ●  |  –  |  –  |  –  |  –  |  –  |
| G3 no fallback               |  –  |  –  |  ●  |  ◐  |  –  |  –  |
| G4 stranded task             |  –  |  –  |  –  |  ●  |  –  |  –  |
| G5 guessed scope             |  ◐  |  ●  |  –  |  –  |  ◐  |  ◐  |
| G6 blind poll                |  –  |  ◐  |  –  |  –  |  –  |  ●  |

`●` closes it · `◐` reduces the damage · `–` untouched

No single option is sufficient, and A and B are the ones everything else reads better on top of.

## a sequence that works

Ordered so each phase is shippable alone and none of them blocks on the experiment.

1. **Make the existing model honest** — A + B + E1. Stop forgetting scoped blocks, record the scope
   the provider named, and stop retrying a weekly limit hourly. No new config and nothing to opt
   into: this is the current design working as it already claims to.
2. **Stop stranding work** — D in its minimal form. A limited entry goes back to pending with a
   bounded counter and the existing wait gate holds it, so a Fable limit costs a delay rather than
   a manual retry.
3. **Add the ladder** — C1, opt-in per role, with the chosen model recorded on the entry. By this
   point the store is accurate enough that the ladder is walked on real information rather than on
   a block some unrelated Sonnet run happened to erase.
4. **Go predictive, if the headers allow** — F, gated on what `probe-limits.sh` reports. If the
   answer is no, the first three phases already carry the case and nothing was built on a promise.

## two things worth deciding early

**Run the probe before committing to F.** `probe-limits.sh` is already written and costs one
near-empty request per model. It answers the only genuinely unknown question here — whether a
per-model window is observable at all — and it is the difference between F being cheap and F being
fiction.

**Decide whether a downgrade is ever silent.** A review role probably wants to wait for Fable
rather than be reviewed by something weaker; a triage role almost certainly does not. That is a
per-role judgement rather than a global default, and it determines whether C is opt-in or opt-out.
Everything else in C follows from the answer.

---

Read against `master` @ `e63167d`. Line references are to that commit and will drift.
