/**
 * Typed client for the `orchestra dashboard` JSON API.
 *
 * The shapes below mirror the `Json` builders in `Orchestra/Dashboard.lean`; the two are kept
 * in step by hand, so a change on one side is a change on both. Every field the Lean side
 * emits is always present — optionals are flattened to `""` there rather than omitted — which
 * is why nothing here is declared optional.
 *
 * Authentication is a cookie the browser holds and sends on its own (see `auth.ts`); no call
 * in this file attaches a credential, and none should.
 */

/** Thrown when the server rejects a request for want of a valid session. */
export class UnauthorizedError extends Error {
  constructor() {
    super("unauthorized");
    this.name = "UnauthorizedError";
  }
}

/** Thrown for any other non-2xx response, carrying whatever the server explained. */
export class ApiError extends Error {
  readonly status: number;
  constructor(status: number, message: string) {
    super(message);
    this.name = "ApiError";
    this.status = status;
  }
}

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
}

export type DetailEndpoint =
  | `tasks/${string}`
  | `concerts/${string}`
  | `listeners/${string}`
  | `projects/${string}`;

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
          : never;

/** The version prefix every read lives under. See `docs/openapi.json`. */
export const API_VERSION = "v1";

/**
 * Build the URL for an endpoint.
 *
 * The path component of a detail endpoint is percent-encoded, and the server percent-decodes
 * it before use (`safeSegment` in `Orchestra/Dashboard.lean`) — so an id containing a slash
 * or a `..` arrives as an ordinary name that simply matches nothing, rather than as a path.
 */
function endpointUrl(kind: "api" | "sse", endpoint: string, query?: QueryParams): string {
  const slash = endpoint.indexOf("/");
  const path =
    slash === -1
      ? endpoint
      : `${endpoint.slice(0, slash)}/${encodeURIComponent(endpoint.slice(slash + 1))}`;
  const search = new URLSearchParams();
  for (const [key, value] of Object.entries(query ?? {})) {
    if (value !== undefined) search.set(key, String(value));
  }
  const suffix = search.toString();
  return `/${kind}/${API_VERSION}/${path}${suffix === "" ? "" : `?${suffix}`}`;
}

/** Query parameters the API accepts. See `docs/openapi.json` for which endpoint takes which. */
export interface QueryParams {
  limit?: number;
  offset?: number;
  /** RFC 3339. Only on collections ordered by time. */
  since?: string;
  /** Only on a task detail. */
  logLimit?: number;
}

export function apiUrl(endpoint: string, query?: QueryParams): string {
  return endpointUrl("api", endpoint, query);
}

export function sseUrl(endpoint: string, query?: QueryParams): string {
  return endpointUrl("sse", endpoint, query);
}

async function readError(response: Response): Promise<string> {
  try {
    const body: unknown = await response.json();
    if (body && typeof body === "object" && "error" in body) {
      return String((body as { error: unknown }).error);
    }
  } catch {
    /* fall through to the status line */
  }
  return `request failed (${response.status})`;
}

/** GET a payload, translating a 401 into `UnauthorizedError` so callers can show the login. */
export async function fetchEndpoint<E extends Endpoint>(
  endpoint: E,
  query?: QueryParams,
): Promise<PayloadOf<E>> {
  const response = await fetch(apiUrl(endpoint, query), {
    credentials: "same-origin",
    headers: { Accept: "application/json" },
  });
  if (response.status === 401) throw new UnauthorizedError();
  if (!response.ok) throw new ApiError(response.status, await readError(response));
  return (await response.json()) as PayloadOf<E>;
}

async function postJson(path: string, body?: unknown): Promise<Response> {
  return fetch(path, {
    method: "POST",
    credentials: "same-origin",
    headers: { "Content-Type": "application/json" },
    ...(body === undefined ? {} : { body: JSON.stringify(body) }),
  });
}

/** Exchange the password for a session cookie. Resolves `false` if it was rejected. */
export async function login(password: string): Promise<boolean> {
  const response = await postJson("/api/login", { password });
  if (response.status === 401) return false;
  if (!response.ok) throw new ApiError(response.status, await readError(response));
  return true;
}

/** Drop the session, server-side and in the browser. */
export async function logout(): Promise<void> {
  await postJson("/api/logout");
}

/** Whether the browser currently holds a valid session. */
export async function checkSession(): Promise<boolean> {
  const response = await fetch("/api/session", { credentials: "same-origin" });
  if (!response.ok) return false;
  const body = (await response.json()) as { authenticated?: boolean };
  return body.authenticated === true;
}
