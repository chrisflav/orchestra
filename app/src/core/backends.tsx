/**
 * The registry, as the app holds it: the list, which one is selected, and how each is doing.
 *
 * Two things live here that the dashboard has no equivalent of.
 *
 * **Switching is a hard cut.** Selecting a backend tears down every stream against the one
 * being left (the core does that, under `backend_select`), and bumps a generation counter that
 * every live view keys off, so a payload from the old backend cannot be shown under the new
 * one's name.
 *
 * **A backend is watched even when it is not selected.** A slow poll of `/api/v1/overview`
 * against every configured backend is what makes the switcher worth opening: each entry says
 * reachable or not, and how much is running there. It is a poll and not a stream on purpose —
 * a stream per backend would hold a connection open to every host you have ever added.
 */

import {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import type { ReactNode } from "react";

import { read } from "./api";
import { CoreError, registry } from "./transport";
import type { BackendInput, BackendPatch, BackendRecord, RegistryView } from "./transport";

/** How often each configured backend is checked. Slow: this is a heartbeat, not a view. */
const POLL_MS = 60_000;
/** And how soon after a switch or an add, so a new entry does not sit blank for a minute. */
const POLL_SOON_MS = 400;

/** What the poll last found for one backend. */
export interface Health {
  state: "unknown" | "ok" | "unauthorized" | "offline";
  running: number;
  pending: number;
  failed: number;
  /** The failure, when there was one. */
  message: string | null;
  checkedAt: number | null;
}

const UNKNOWN: Health = {
  state: "unknown",
  running: 0,
  pending: 0,
  failed: 0,
  message: null,
  checkedAt: null,
};

interface BackendsState {
  /** `null` until the first read of the registry has answered. */
  view: RegistryView | null;
  backends: BackendRecord[];
  selected: BackendRecord | null;
  health: Record<string, Health>;
  /** Bumped on every switch. A live view that sees it change drops what it is holding. */
  generation: number;
  select: (id: string | null) => Promise<void>;
  add: (input: BackendInput) => Promise<void>;
  update: (patch: BackendPatch) => Promise<void>;
  remove: (id: string) => Promise<void>;
  /** Re-check one backend now, or all of them. */
  refresh: (id?: string) => void;
}

const BackendsContext = createContext<BackendsState | null>(null);

export function BackendsProvider({ children }: { children: ReactNode }) {
  const [view, setView] = useState<RegistryView | null>(null);
  const [health, setHealth] = useState<Record<string, Health>>({});
  const [generation, setGeneration] = useState(0);
  // A nudge the poll effect keys off, so `refresh()` re-runs it without waiting for the timer.
  const [nudge, setNudge] = useState(0);
  const alive = useRef(true);

  useEffect(() => {
    alive.current = true;
    return () => {
      alive.current = false;
    };
  }, []);

  const apply = useCallback((next: RegistryView) => {
    if (alive.current) setView(next);
  }, []);

  useEffect(() => {
    void registry.list().then(apply);
  }, [apply]);

  const select = useCallback(
    async (id: string | null) => {
      apply(await registry.select(id));
      // After the core has torn the old backend's streams down, never before: a view that
      // re-subscribed first would open a stream the teardown then closed.
      setGeneration((g) => g + 1);
      setNudge((n) => n + 1);
    },
    [apply],
  );

  const add = useCallback(
    async (input: BackendInput) => {
      apply(await registry.add(input));
      setNudge((n) => n + 1);
    },
    [apply],
  );

  const update = useCallback(
    async (patch: BackendPatch) => {
      apply(await registry.update(patch));
      // An address, a TLS policy or a password may have moved under a live view.
      setGeneration((g) => g + 1);
      setNudge((n) => n + 1);
    },
    [apply],
  );

  const remove = useCallback(
    async (id: string) => {
      apply(await registry.remove(id));
      setHealth((current) => {
        const next = { ...current };
        delete next[id];
        return next;
      });
      setGeneration((g) => g + 1);
    },
    [apply],
  );

  const refresh = useCallback((id?: string) => {
    if (id !== undefined) {
      setHealth((current) => ({ ...current, [id]: { ...UNKNOWN } }));
    }
    setNudge((n) => n + 1);
  }, []);

  // The poll. One pass over every configured backend, then every minute.
  const ids = (view?.backends ?? []).map((b) => `${b.id}:${b.hasSecret}`).join(",");
  useEffect(() => {
    let cancelled = false;
    const entries = (view?.backends ?? []).filter((b) => b.hasSecret);

    const check = async (backend: BackendRecord) => {
      let next: Health;
      try {
        const overview = await read(backend.id, "overview");
        next = {
          state: "ok",
          running: overview.counts.running,
          pending: overview.counts.pending,
          failed: overview.counts.failed,
          message: null,
          checkedAt: Date.now(),
        };
      } catch (error) {
        const core = error instanceof CoreError ? error : null;
        next = {
          ...UNKNOWN,
          state: core?.unauthorized === true ? "unauthorized" : "offline",
          message: core?.message ?? "could not be reached",
          checkedAt: Date.now(),
        };
      }
      if (!cancelled) setHealth((current) => ({ ...current, [backend.id]: next }));
    };

    const pass = () => {
      for (const backend of entries) void check(backend);
    };

    const soon = setTimeout(pass, POLL_SOON_MS);
    const timer = setInterval(pass, POLL_MS);
    return () => {
      cancelled = true;
      clearTimeout(soon);
      clearInterval(timer);
    };
    // `ids` rather than `view`: a poll should restart when the set of backends changes, not
    // when an unrelated field of one of them does.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [ids, nudge]);

  const value = useMemo<BackendsState>(() => {
    const backends = view?.backends ?? [];
    const selected = backends.find((b) => b.id === view?.selected) ?? null;
    return {
      view,
      backends,
      selected,
      health,
      generation,
      select,
      add,
      update,
      remove,
      refresh,
    };
  }, [view, health, generation, select, add, update, remove, refresh]);

  return <BackendsContext.Provider value={value}>{children}</BackendsContext.Provider>;
}

export function useBackends(): BackendsState {
  const context = useContext(BackendsContext);
  if (context === null) throw new Error("useBackends must be used within a BackendsProvider");
  return context;
}

/**
 * The selected backend's id, for a screen that only runs when one is selected.
 *
 * Throwing rather than returning `null` is deliberate: the shell does not render a screen
 * without a selection, so a screen that got here with none is a routing bug, and a page that
 * silently rendered empty would hide it.
 */
export function useSelectedBackend(): BackendRecord {
  const { selected } = useBackends();
  if (selected === null) throw new Error("no backend is selected");
  return selected;
}

export function healthOf(health: Record<string, Health>, id: string): Health {
  return health[id] ?? UNKNOWN;
}
