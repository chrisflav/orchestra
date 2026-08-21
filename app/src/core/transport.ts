/**
 * The only way out of the webview.
 *
 * There is no `fetch` in this app and no `EventSource`. A browser page cannot reach a backend
 * that did not serve it — the dashboard API sends no CORS headers, deliberately — and
 * `EventSource` cannot carry a bearer token, so both the request and the stream are made by the
 * Rust core and the results are handed back here. See `docs/native-app.md` for why that is the
 * whole reason this is a native app.
 *
 * Nothing in this module ever holds a credential. A backend is named by its id; the core
 * resolves the id to an origin and a secret, and returns only the answer.
 */

import { invoke } from "@tauri-apps/api/core";
import { listen } from "@tauri-apps/api/event";
import type { UnlistenFn } from "@tauri-apps/api/event";

/** What every failure from the core looks like. */
export interface CoreErrorShape {
  kind: "noSuchBackend" | "badRequest" | "unauthorized" | "http" | "unreachable" | "storage";
  message: string;
  status: number | null;
}

/**
 * A failure raised by the core, carrying which kind it was.
 *
 * Two kinds change what a screen does rather than merely what it says: `unauthorized` means
 * the stored password is wrong and puts *that* backend into its needs-credential state, and
 * `unreachable` means the daemon is not answering and marks it offline in the switcher.
 * Everything else is text to show.
 */
export class CoreError extends Error implements CoreErrorShape {
  readonly kind: CoreErrorShape["kind"];
  readonly status: number | null;

  constructor(shape: CoreErrorShape) {
    super(shape.message);
    this.name = "CoreError";
    this.kind = shape.kind;
    this.status = shape.status;
  }

  get unauthorized(): boolean {
    return this.kind === "unauthorized";
  }

  get offline(): boolean {
    return this.kind === "unreachable";
  }
}

function isShape(value: unknown): value is CoreErrorShape {
  return (
    typeof value === "object" &&
    value !== null &&
    "kind" in value &&
    "message" in value &&
    typeof (value as { message: unknown }).message === "string"
  );
}

/**
 * Call a command, normalising whatever it rejected with into a `CoreError`.
 *
 * Tauri rejects with the serialised error value, which is our `{kind, message, status}` — but
 * a command that panics or a name that does not exist rejects with a string, and a caller
 * should not have to tell the two apart.
 */
async function call<T>(command: string, args?: Record<string, unknown>): Promise<T> {
  try {
    return await invoke<T>(command, args);
  } catch (raw) {
    if (raw instanceof CoreError) throw raw;
    if (isShape(raw)) throw new CoreError(raw);
    throw new CoreError({
      kind: "badRequest",
      message: typeof raw === "string" ? raw : "the app could not complete that",
      status: null,
    });
  }
}

/* ── the registry ───────────────────────────────────────────────────────────────────────── */

export type BackendColor = "brass" | "strings" | "winds" | "perc";

export interface BackendRecord {
  id: string;
  name: string;
  url: string;
  color: BackendColor;
  allowInsecureTls: boolean;
  notify: boolean;
  addedAt: string;
  /** Whether a password is stored for it. Never the password itself. */
  hasSecret: boolean;
}

export interface RegistryView {
  backends: BackendRecord[];
  selected: string | null;
  /** Where the passwords actually are. `file` is the fallback where no keychain answered. */
  secretStore: "keychain" | "file";
}

export interface ProbeResult {
  ok: boolean;
  outcome: "authenticated" | "rejected" | "notOrchestra" | "unreachable";
  message: string;
  running: number | null;
  pending: number | null;
}

export interface BackendInput {
  name: string;
  url: string;
  secret: string;
  color?: BackendColor;
  allowInsecureTls?: boolean;
}

export interface BackendPatch {
  id: string;
  name?: string;
  url?: string;
  color?: BackendColor;
  allowInsecureTls?: boolean;
  notify?: boolean;
  /** Absent leaves the stored password alone — which is how a rename does not ask for it. */
  secret?: string;
}

export const registry = {
  list: () => call<RegistryView>("backends_list"),
  probe: (input: BackendInput) => call<ProbeResult>("backend_probe", { input }),
  add: (input: BackendInput) => call<RegistryView>("backend_add", { input }),
  update: (patch: BackendPatch) => call<RegistryView>("backend_update", { patch }),
  remove: (id: string) => call<RegistryView>("backend_remove", { id }),
  select: (id: string | null) => call<RegistryView>("backend_select", { id }),
};

/* ── requests ───────────────────────────────────────────────────────────────────────────── */

export interface ApiResponse {
  status: number;
  body: unknown;
}

export type Method = "GET" | "POST" | "PUT" | "DELETE";

/**
 * One request against one backend.
 *
 * `path` is relative to `/api/` and never a URL — the core refuses anything that looks like
 * one, which is what stops a bug here addressing a token somewhere else.
 */
export function apiRequest(
  backend: string,
  path: string,
  method: Method = "GET",
  body?: unknown,
): Promise<ApiResponse> {
  return call<ApiResponse>("api_request", {
    input: { backend, path, method, body: body ?? null },
  });
}

/* ── streams ────────────────────────────────────────────────────────────────────────────── */

export interface StreamFrame<T = unknown> {
  stream: string;
  /** The backend the frame came from. A view drops anything that is not the one it shows. */
  backend: string;
  kind: "open" | "data" | "closed" | "failed";
  data?: T;
  /** The `id:` the frame carried — the transcript's seq. */
  cursor?: number;
  message?: string;
}

type Handler = (frame: StreamFrame<never>) => void;

/**
 * Every frame from every stream arrives on one event, and is dispatched here by stream id.
 *
 * One listener rather than one per stream, because the id is only known once `stream_start`
 * answers and the core starts reading the moment it is called: a listener attached after that
 * misses whatever landed in between. Frames for an id nothing has claimed yet are held, and
 * delivered when it registers.
 */
const handlers = new Map<string, Handler>();
const held = new Map<string, StreamFrame<never>[]>();
let attached: Promise<UnlistenFn> | null = null;

function attach(): Promise<UnlistenFn> {
  attached ??= listen<StreamFrame<never>>("stream-frame", (event) => {
    const frame = event.payload;
    const handler = handlers.get(frame.stream);
    if (handler !== undefined) {
      handler(frame);
      return;
    }
    // Bounded: a stream nobody claims is one whose `stream_start` failed, and the ten frames
    // that is worth are not what this app runs out of memory on.
    const queue = held.get(frame.stream) ?? [];
    if (queue.length < 10) queue.push(frame);
    held.set(frame.stream, queue);
  });
  return attached;
}

/**
 * Subscribe to an SSE path on one backend.
 *
 * Answers a function that stops it. The core reconnects on its own — at the cursor, for the
 * transcript — so a caller never re-subscribes; it only unsubscribes when its view goes away.
 * Unsubscribing before the stream has started is handled: the id is stopped as soon as it
 * arrives, which is the case a component that mounts and unmounts immediately produces.
 */
export async function openStream<T>(
  backend: string,
  path: string,
  cursor: number | null,
  onFrame: (frame: StreamFrame<T>) => void,
): Promise<() => void> {
  await attach();
  let stopped = false;
  let id: string | null = null;

  const stop = () => {
    stopped = true;
    if (id !== null) {
      handlers.delete(id);
      held.delete(id);
      void call<void>("stream_stop", { id });
    }
  };

  try {
    id = await call<string>("stream_start", { input: { backend, path, cursor } });
  } catch (error) {
    if (!stopped) {
      onFrame({
        stream: "",
        backend,
        kind: "failed",
        message: error instanceof Error ? error.message : "the stream could not be started",
      });
    }
    return () => {};
  }
  if (stopped) {
    void call<void>("stream_stop", { id });
    return () => {};
  }

  handlers.set(id, onFrame as Handler);
  for (const frame of held.get(id) ?? []) onFrame(frame as StreamFrame<T>);
  held.delete(id);
  return stop;
}
