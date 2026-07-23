import { useEffect, useRef, useState } from "react";
import { fetchEndpoint, sseUrl, UnauthorizedError } from "./api";
import type { Endpoint, PayloadOf, QueryParams } from "./api";
import { useAuth } from "./auth";

export interface LiveData<T> {
  data: T | null;
  /** Set when the first load failed; a later SSE frame clears it. */
  error: string | null;
  /** Whether the SSE stream is currently attached. */
  live: boolean;
}

/**
 * Load an endpoint, then keep it current from its SSE stream.
 *
 * The initial `fetch` is what surfaces failures — `EventSource` reports errors as an opaque
 * event with no status code, so a 401 is indistinguishable from a dropped connection there.
 * Doing the first read over `fetch` means an expired session shows a login screen instead of
 * an empty page that silently retries forever.
 *
 * The server only pushes frames whose content changed, so an idle orchestra produces no
 * traffic and this hook does not re-render.
 */
export function useLiveData<E extends Endpoint>(
  endpoint: E,
  query?: QueryParams,
): LiveData<PayloadOf<E>> {
  // Serialised for the dependency list: a fresh object literal on every render would tear
  // down and rebuild the SSE connection on every render.
  const queryKey = JSON.stringify(query ?? {});
  const [data, setData] = useState<PayloadOf<E> | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [live, setLive] = useState(false);
  const { onUnauthorized } = useAuth();
  // Kept in a ref so the effect below does not re-subscribe when the callback identity
  // changes; re-running it would tear down and rebuild the SSE connection.
  const onUnauthorizedRef = useRef(onUnauthorized);
  onUnauthorizedRef.current = onUnauthorized;

  useEffect(() => {
    let cancelled = false;
    let source: EventSource | null = null;

    // A fresh endpoint means the previous payload is the wrong shape; clear it so a stale
    // page is never rendered against new data.
    setData(null);
    setError(null);
    setLive(false);

    const subscribe = () => {
      if (cancelled) return;
      source = new EventSource(sseUrl(endpoint, query), { withCredentials: true });
      source.onopen = () => {
        if (!cancelled) setLive(true);
      };
      source.onmessage = (event: MessageEvent<string>) => {
        if (cancelled) return;
        try {
          setData(JSON.parse(event.data) as PayloadOf<E>);
          setError(null);
        } catch {
          /* a malformed frame is dropped; the next one supersedes it */
        }
      };
      source.onerror = () => {
        if (!cancelled) setLive(false);
        // EventSource reconnects on its own. It cannot tell us *why* it dropped, so a
        // revoked session would otherwise reconnect forever: re-probe over fetch, which
        // can, and let that path route to the login screen.
        void fetchEndpoint(endpoint, query).catch((err: unknown) => {
          if (err instanceof UnauthorizedError && !cancelled) {
            source?.close();
            onUnauthorizedRef.current();
          }
        });
      };
    };

    void fetchEndpoint(endpoint, query)
      .then((initial) => {
        if (cancelled) return;
        setData(initial);
        subscribe();
      })
      .catch((err: unknown) => {
        if (cancelled) return;
        if (err instanceof UnauthorizedError) {
          onUnauthorizedRef.current();
          return;
        }
        setError(err instanceof Error ? err.message : String(err));
      });

    return () => {
      cancelled = true;
      source?.close();
    };
    // `query` is covered by `queryKey`; including the object itself would defeat it.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [endpoint, queryKey]);

  return { data, error, live };
}
