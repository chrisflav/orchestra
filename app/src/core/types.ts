/**
 * The payload types of the orchestra dashboard API.
 *
 * Three copies of these shapes exist and are kept in step by hand: the `Json` builders in
 * `Orchestra/Dashboard.lean`, which emit them; `web/src/api.ts`, which the dashboard reads
 * them with; and this file. `docs/openapi.json` is the contract all three answer to — if they
 * drift, that is the document to settle it against, and the fix is to generate rather than to
 * make one import the other (see `docs/native-app.md`).
 *
 * Nothing here is optional: every field the Lean side emits is always present, with absent
 * values flattened to `null` rather than omitted.
 *
 * This file holds types only. Reaching a backend is `core/api.ts`, over the Rust core.
 */

export type QueueStatus = "pending" | "running" | "done" | "failed" | "unfinished" | "cancelled";
export type TaskStatus = "running" | "completed" | "failed" | "unfinished" | "cancelled";
export type ConcertStatus = "running" | "done" | "failed" | "cancelled";
export type IssueStatus = "open" | "claimed" | "completed" | "abandoned";

export interface QueueEntry {
  id: string;
  status: QueueStatus;
  createdAt: string;
  priority: number;
  upstream: string;
  fork: string;
  prompt: string;
  series: string | null;
  backend: string | null;
  model: string | null;
  /** The run this entry became, once claimed. */
  taskId: string | null;
  concertId: string | null;
  concertStepKey: string | null;
}

export interface TaskRecord {
  id: string;
  status: TaskStatus;
  createdAt: string;
  upstream: string;
  fork: string;
  prompt: string;
  series: string | null;
  backend: string | null;
  model: string | null;
  sessionId: string | null;
  continuesFrom: string | null;
  budgetUsd: number | null;
}

export interface ConcertRun {
  id: string;
  status: ConcertStatus;
  name: string | null;
  workflowFile: string | null;
  startedAt: string;
  finishedAt: string | null;
}

export interface Overview {
  counts: {
    running: number;
    pending: number;
    failed: number;
    concerts: number;
    listeners: number;
    totalTasks: number;
    authFree: number;
    authTotal: number;
  };
  activeQueue: QueueEntry[];
  recentTasks: TaskRecord[];
  /**
   * Base URL of the configured taxis tracker, or `null` if there is none.
   *
   * Projects live in taxis, so the dashboard links out rather than rendering them. `null`
   * means there is nothing to link to and the destination is hidden entirely.
   */
  taxisUrl: string | null;
}

/**
 * The envelope every collection endpoint answers in.
 *
 * `total` counts what matched before `limit` and `offset` were applied, which is what lets a
 * caller say "50 of 812" without a second request.
 */
export interface Collection<T> {
  items: T[];
  total: number;
  limit: number;
  offset: number;
}

export interface ConcertDetail {
  concert: ConcertRun;
  steps: QueueEntry[];
}

export interface ListenerSummary {
  name: string;
  enabled: boolean;
  sourceType: string;
  intervalSeconds: number;
  lastCheckedAt: string | null;
  eventCount: number;
}

export interface ActionConfig {
  mode: "fork" | "pr";
  upstream: string;
  fork: string;
  series: string | null;
  backend: string | null;
  model: string | null;
  workflowPath: string | null;
  priority: number;
  promptTemplate: string;
}

export interface ListenerDetail {
  name: string;
  enabled: boolean;
  intervalSeconds: number;
  lastCheckedAt: string | null;
  eventCount: number;
  sourceType: string;
  sourceDetail: string;
  /** Source-kind-specific extras, as `[label, value]` pairs. */
  sourceExtras: [string, string][];
  action: ActionConfig;
  recentEvents: string[];
}

/**
 * One structured event from a task's JSONL log. The agent backends are free to add event
 * types, so this is a discriminated union with an open fallback rather than a closed set.
 */
export interface LogEvent {
  type?: string;
  subtype?: string;
  event_type?: string;
  model?: string;
  session_id?: string;
  stdout?: string;
  stderr?: string;
  result?: string;
  num_turns?: number;
  duration_ms?: number;
  total_cost_usd?: number;
  item?: {
    type?: string;
    text?: string;
    name?: string;
    input?: Record<string, unknown>;
  };
  [key: string]: unknown;
}

export interface TaskDetail {
  id: string;
  /**
   * The run `log` was read under. A queue entry and the task it becomes carry separate ids, so
   * an entry addressed by its own id resolves to this one; `null` means the entry has no run —
   * still queued, or failed before it started — and so has no trace.
   */
  taskId: string | null;
  status: TaskStatus | QueueStatus;
  fork: string;
  createdAt: string;
  prompt: string;
  /** The trailing `logLimit` events of the run's log, oldest first. */
  log: LogEvent[];
  /** How many events the log holds in total. */
  logTotal: number;
  /** The tail size the server used. */
  logLimit: number;
  /** Whether `log` is a tail rather than the whole thing. */
  logTruncated: boolean;
}

export interface IssueCounts {
  open: number;
  claimed: number;
  completed: number;
  abandoned: number;
}

export interface ProjectSummary {
  id: string;
  name: string;
  description: string | null;
  createdAt: string;
  defaultTarget: string | null;
  issueCount: number;
  counts: IssueCounts;
}

export interface IssueNode {
  id: string;
  title: string;
  status: IssueStatus;
  parentId: string | null;
  dependencies: string[];
  prCount: number;
  claimedBy: string | null;
  updatedAt: string;
}

export interface ProjectDetail {
  project: ProjectSummary;
  issues: IssueNode[];
}

export interface UsageLimit {
  kind: string;
  scope: string | null;
  percent: number;
  severity: "normal" | "warning" | "critical" | string;
  active: boolean;
  resetsAt: string | null;
}

export interface AuthSource {
  label: string;
  backend: string;
  kind: "oauth" | "api-key";
  baseUrl: string | null;
  isDefault: boolean;
  pollable: boolean;
  /** Branch on this, not on `availableAt`: a blocked source may report no reset time. */
  state: "available" | "blocked";
  reason: string | null;
  availableAt: string | null;
  pressure: number;
  /** How fresh these numbers are. */
  polledAt: string | null;
  lastUsedAt: string | null;
  lastError: string | null;
  backoffUntil: string | null;
  limits: UsageLimit[];
}

export interface AuthBackend {
  name: string;
  defaultSource: string | null;
  sources: AuthSource[];
}

export interface AuthView {
  configError: string | null;
  backends: AuthBackend[];
}

/** What `POST /api/v1/queue/{id}/cancel` reports back: the work the id resolved to. */
export interface CancelResult {
  /** The queue entry that was cancelled. */
  id: string;
  /** The run it became, if it had started one. */
  taskId: string | null;
}

/**
 * One session or weekly limit window, rolled up from every poll that landed inside it.
 *
 * `peakPercent` is what the window consumed — utilisation only climbs inside a window — and
 * `percent` is where it last stood, so on a closed window the two agree and on the open one
 * they say how much of it is already gone.
 */
export interface UsageWindow {
  kind: string;
  /** Model family a scoped window applies to. */
  scope: string | null;
  /** The first poll that landed in this window, not the window's own start. */
  startedAt: string;
  updatedAt: string;
  resetsAt: string | null;
  peakPercent: number;
  percent: number;
  /** How many polls saw this window. One is a glimpse of it, not a measurement. */
  samples: number;
  /** Whether this is the window still filling: the newest of its series, not yet past its reset. */
  open: boolean;
}

export interface UsageHistorySource {
  label: string;
  backend: string;
  kind: "oauth" | "api-key";
  /** False for API-key sources, which have no subscription window to accumulate history in. */
  pollable: boolean;
  /** Session windows, oldest first — the order a graph is drawn in. */
  sessions: UsageWindow[];
  /** Weekly windows, account-wide and model-scoped alike, oldest first. */
  weeks: UsageWindow[];
  /** Any other kind upstream reported, so nothing recorded is unreachable. */
  other: UsageWindow[];
}

export interface UsageHistoryBackend {
  name: string;
  sources: UsageHistorySource[];
}

export interface UsageHistory {
  configError: string | null;
  backends: UsageHistoryBackend[];
}


/* ── Interactive sessions ───────────────────────────────────────────────────────────────── */

export type SessionStatus = "starting" | "idle" | "running" | "ended" | "failed";

export interface SessionSummary {
  id: string;
  status: SessionStatus;
  createdAt: string;
  lastActivityAt: string;
  endedAt: string | null;
  upstream: string;
  fork: string;
  backend: string;
  model: string | null;
  turnCount: number;
  costUsd: number;
  /** The last seq in the transcript. A client that has read this far is current. */
  lastEventSeq: number;
  title: string | null;
  error: string | null;
}

export interface SessionDetail extends SessionSummary {
  budget: number;
  slot: number;
  agentSessionId: string | null;
  resumedFrom: string | null;
}

/**
 * One line of a transcript. `kind` says what happened and the rest of the fields depend on it,
 * which is why they are all optional here: narrowing on `kind` is the only safe way to read one.
 */
export interface TranscriptEvent {
  seq: number;
  occurredAt: string;
  kind: "user" | "agent" | "turnStarted" | "turnEnded" | "notice";
  /** `user`. */
  text?: string;
  /** `agent`: a stream event, the same shape `LogView` already renders. */
  event?: LogEvent;
  /** `turnStarted`, `turnEnded`. */
  turn?: number;
  /** `turnEnded`. */
  subtype?: string;
  costUsd?: number | null;
  durationSeconds?: number | null;
  /** `notice`. */
  level?: "info" | "warning" | "error";
  message?: string;
}

/** A page of a transcript. Not a `Collection`: a cursor is not an offset. */
export interface Transcript {
  items: TranscriptEvent[];
  /** How many events follow the cursor in total, before the window. */
  total: number;
  limit: number;
  /** The cursor this answered. */
  after: number;
}

/** What starting a session asks for. Only the two repositories are required. */
export interface SessionRequest {
  upstream: string;
  fork: string;
  backend?: string;
  model?: string;
  budget?: number;
  tools?: string[];
  systemPrompt?: string;
  resumeFrom?: string;
}

/**
 * Maps each endpoint to the payload it returns. Detail endpoints take a path component, so
 * they are spelled as template literal types — that is what makes `useLiveData("tasks/" + id)`
 * resolve to `TaskDetail` rather than to `unknown`.
 */
export interface Endpoints {
  overview: Overview;
  queue: Collection<QueueEntry>;
  concerts: Collection<ConcertRun>;
  listeners: Collection<ListenerSummary>;
  tasks: Collection<TaskRecord>;
  projects: Collection<ProjectSummary>;
  auth: AuthView;
  usage: UsageHistory;
  interactive: Collection<SessionSummary>;
}

export type DetailEndpoint =
  | `tasks/${string}`
  | `concerts/${string}`
  | `listeners/${string}`
  | `projects/${string}`
  | `interactive/${string}`;

export type Endpoint = keyof Endpoints | DetailEndpoint;

/** The payload `E` resolves to. */
export type PayloadOf<E extends Endpoint> = E extends keyof Endpoints
  ? Endpoints[E]
  : E extends `tasks/${string}`
    ? TaskDetail
    : E extends `concerts/${string}`
      ? ConcertDetail
      : E extends `listeners/${string}`
        ? ListenerDetail
        : E extends `projects/${string}`
          ? ProjectDetail
          : E extends `interactive/${string}`
            ? SessionDetail
            : never;
