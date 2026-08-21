/**
 * The orchestra dashboard API, as this app calls it.
 *
 * Every call names the backend it is for. That is the whole difference from the dashboard's
 * client, which has exactly one backend — the origin that served it — and so never has to say.
 * Here a screen reads from the *selected* backend, but a stream against another one may still
 * be running, so the id is a parameter rather than an ambient fact.
 *
 * Nothing here opens a socket: `transport.ts` hands the request to the Rust core, which holds
 * the origin and the token. See `docs/native-app.md`.
 */

import { apiRequest } from "./transport";
import type { Method } from "./transport";
import type {
  CancelResult,
  Endpoint,
  PayloadOf,
  SessionDetail,
  SessionRequest,
  Transcript,
} from "./types";

/** Query parameters the API accepts. See `docs/openapi.json` for which endpoint takes which. */
export interface QueryParams {
  limit?: number;
  offset?: number;
  /** RFC 3339. Only on collections ordered by time. */
  since?: string;
  /** Only on a task detail. */
  logLimit?: number;
  /** Only on usage history. */
  windows?: number;
}

/**
 * Build the path for an endpoint, under a version prefix.
 *
 * The component after the first slash is percent-encoded and the server percent-decodes it
 * before use (`safeSegment` in `Orchestra/Dashboard.lean`), so an id containing a slash arrives
 * as an ordinary name that matches nothing rather than as a path.
 */
export function pathFor(endpoint: string, query?: QueryParams): string {
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
  return `v1/${path}${suffix === "" ? "" : `?${suffix}`}`;
}

/** Read one endpoint once. Screens stream instead; this is for the first load and for polls. */
export async function read<E extends Endpoint>(
  backend: string,
  endpoint: E,
  query?: QueryParams,
): Promise<PayloadOf<E>> {
  const response = await apiRequest(backend, pathFor(endpoint, query));
  return response.body as PayloadOf<E>;
}

async function write<T>(
  backend: string,
  path: string,
  method: Method,
  body?: unknown,
): Promise<T> {
  const response = await apiRequest(backend, path, method, body);
  return response.body as T;
}

/**
 * Stop one running task, named by its queue entry id or by the id of the run it became — the
 * server resolves either.
 *
 * A `404` means no entry and no run carries that id; a `409` means it is not running, or the
 * daemon is not. Both come back as a `CoreError` with the server's own words.
 */
export function cancelTask(backend: string, id: string): Promise<CancelResult> {
  return write(backend, `v1/queue/${encodeURIComponent(id)}/cancel`, "POST", {});
}

/**
 * Turn a listener on or off.
 *
 * The one configuration write the app makes. Editing a listener's source or its prompt
 * template is editing a document, which the dashboard does; a switch is a switch.
 */
export function setListenerEnabled(
  backend: string,
  name: string,
  enabled: boolean,
): Promise<unknown> {
  return write(backend, `v1/listeners/${encodeURIComponent(name)}/enabled`, "PUT", { enabled });
}

/* ── interactive sessions ───────────────────────────────────────────────────────────────── */

/** Start a session: the daemon clones, mints a token, starts an MCP server and an agent. */
export function startSession(
  backend: string,
  request: SessionRequest,
): Promise<SessionDetail> {
  return write(backend, "v1/interactive", "POST", request);
}

/** Post a turn. Answers the seq it was written at, so a reader knows where it landed. */
export function sendTurn(
  backend: string,
  id: string,
  text: string,
): Promise<{ seq: number }> {
  return write(backend, `v1/interactive/${encodeURIComponent(id)}/messages`, "POST", { text });
}

/** Abandon the turn in flight. A `409` means there was none. */
export function interruptSession(backend: string, id: string): Promise<unknown> {
  return write(backend, `v1/interactive/${encodeURIComponent(id)}/interrupt`, "POST", {});
}

/** End a session: the agent is killed and its clone slot released. */
export function endSession(backend: string, id: string): Promise<unknown> {
  return write(backend, `v1/interactive/${encodeURIComponent(id)}`, "DELETE");
}

/** A page of a transcript from a cursor. The stream carries the rest. */
export function readTranscript(
  backend: string,
  id: string,
  after: number,
): Promise<Transcript> {
  return write(
    backend,
    `v1/interactive/${encodeURIComponent(id)}/events?after=${after}`,
    "GET",
  );
}

/** The SSE path for a transcript, without its cursor — the core owns that. */
export function transcriptStreamPath(id: string): string {
  return `v1/interactive/${encodeURIComponent(id)}/events`;
}
