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
  fork: string;
  createdAt: string;
  priority: number;
  series: string;
  concertId: string;
  concertStepKey: string;
  prompt: string;
}

export interface TaskRecord {
  id: string;
  status: TaskStatus;
  fork: string;
  createdAt: string;
  series: string;
  prompt: string;
}

export interface ConcertRun {
  id: string;
  status: ConcertStatus;
  name: string;
  workflowFile: string;
  startedAt: string;
  finishedAt: string;
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
}

export interface QueueView {
  concerts: ConcertRun[];
  entries: QueueEntry[];
}

export interface ConcertsView {
  concerts: ConcertRun[];
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
  lastChecked: string;
  eventCount: number;
}

export interface ListenersView {
  listeners: ListenerSummary[];
}

export interface ActionConfig {
  mode: "fork" | "pr";
  upstream: string;
  fork: string;
  series: string;
  backend: string;
  model: string;
  workflowPath: string;
  priority: number;
  promptTemplate: string;
}

export interface ListenerDetail {
  name: string;
  enabled: boolean;
  intervalSeconds: number;
  lastChecked: string;
  eventCount: number;
  sourceType: string;
  sourceDetail: string;
  /** Source-kind-specific extras, as `[label, value]` pairs. */
  sourceExtras: [string, string][];
  action: ActionConfig;
  recentEvents: string[];
}

export interface TasksView {
  tasks: TaskRecord[];
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
  /** The tail of the log — at most the last 500 events. */
  log: LogEvent[];
  /** How many events the log holds in total. */
  logTotal: number;
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
  description: string;
  createdAt: string;
  defaultTarget: string;
  issueCount: number;
  counts: IssueCounts;
}

export interface ProjectsView {
  projects: ProjectSummary[];
}

export interface IssueNode {
  id: string;
  title: string;
  status: IssueStatus;
  parentId: string;
  dependencies: string[];
  prCount: number;
  claimedBy: string;
  updatedAt: string;
}

export interface ProjectDetail {
  project: ProjectSummary;
  issues: IssueNode[];
}

export interface UsageLimit {
  kind: string;
  scope: string;
  percent: number;
  severity: "normal" | "warning" | "critical" | string;
  active: boolean;
  resets: string;
}

export interface AuthSource {
  label: string;
  kind: "oauth" | "api-key";
  baseUrl: string;
  isDefault: boolean;
  pollable: boolean;
  state: "available" | "blocked";
  reason: string;
  resets: string;
  pressure: number;
  polled: string;
  lastUsed: string;
  lastError: string;
  backoff: string;
  limits: UsageLimit[];
}

export interface AuthBackend {
  name: string;
  defaultSource: string;
  sources: AuthSource[];
}

export interface AuthView {
  configError: string;
  backends: AuthBackend[];
}

/**
 * Maps each endpoint to the payload it returns. Detail endpoints take a path component, so
 * they are spelled as template literal types — that is what makes `useLiveData("tasks/" + id)`
 * resolve to `TaskDetail` rather than to `unknown`.
 */
export interface Endpoints {
  overview: Overview;
  queue: QueueView;
  concerts: ConcertsView;
  listeners: ListenersView;
  tasks: TasksView;
  projects: ProjectsView;
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

/**
 * Build the URL for an endpoint.
 *
 * The path component of a detail endpoint is percent-encoded, and the server percent-decodes
 * it before use (`safeSegment` in `Orchestra/Dashboard.lean`) — so an id containing a slash
 * or a `..` arrives as an ordinary name that simply matches nothing, rather than as a path.
 */
function endpointUrl(kind: "api" | "sse", endpoint: string): string {
  const slash = endpoint.indexOf("/");
  const path =
    slash === -1
      ? endpoint
      : `${endpoint.slice(0, slash)}/${encodeURIComponent(endpoint.slice(slash + 1))}`;
  return `/${kind}/${path}`;
}

export function apiUrl(endpoint: string): string {
  return endpointUrl("api", endpoint);
}

export function sseUrl(endpoint: string): string {
  return endpointUrl("sse", endpoint);
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
export async function fetchEndpoint<E extends Endpoint>(endpoint: E): Promise<PayloadOf<E>> {
  const response = await fetch(apiUrl(endpoint), {
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
