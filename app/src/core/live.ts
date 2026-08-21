/**
 * Reading a backend, and staying current with it.
 *
 * The dashboard's `useLiveData` and `useTranscript` in the shape this app needs: keyed by
 * backend rather than by origin, and reading through the Rust core rather than through
 * `fetch` and `EventSource`.
 *
 * Two things the browser versions have to do are simply absent here. There is no reconnect
 * logic — the core reconnects, at the cursor — and there is no re-probe to tell a dropped
 * connection from a revoked credential, because the core knows which it was and says so on the
 * frame. What is left is the part that was always the interesting part: what to do with a
 * payload when it arrives.
 */

import { useEffect, useRef, useState } from "react";

import { pathFor, readTranscript, transcriptStreamPath } from "./api";
import type { QueryParams } from "./api";
import { useBackends } from "./backends";
import { apiRequest, CoreError, openStream } from "./transport";
import type { Endpoint, PayloadOf, Transcript, TranscriptEvent } from "./types";

export interface Live<T> {
  data: T | null;
  /** Set when the load failed; a later frame clears it. */
  error: string | null;
  /** Whether the stream is attached right now. */
  live: boolean;
  /** True when the failure was the credential, so a screen can offer to fix it. */
  unauthorized: boolean;
}

/**
 * Read an endpoint from the selected backend, then keep it current from its stream.
 *
 * The first read goes over a request rather than the stream, because it is what surfaces a
 * failure: a stream that cannot authenticate reports it as a frame, eventually, and a screen
 * should say "this backend rejected the password" on the first paint rather than after a
 * retry. The stream is opened once that read has answered.
 *
 * Everything re-runs when the selected backend changes or the registry generation moves. A
 * frame that names a different backend is dropped — the core tears streams down on a switch,
 * but a frame already in flight has nowhere else to land.
 */
export function useLive<E extends Endpoint>(endpoint: E, query?: QueryParams): Live<PayloadOf<E>> {
  const { selected, generation } = useBackends();
  const backendId = selected?.id ?? null;
  const [data, setData] = useState<PayloadOf<E> | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [unauthorized, setUnauthorized] = useState(false);
  const [live, setLive] = useState(false);
  // Serialised for the dependency list: a fresh object literal on every render would tear the
  // stream down and rebuild it on every render.
  const queryKey = JSON.stringify(query ?? {});

  useEffect(() => {
    if (backendId === null) return;
    let cancelled = false;
    let close: (() => void) | null = null;

    // A different backend or a different endpoint means the payload on screen is the wrong
    // one; clear it rather than render a stale page under a new heading.
    setData(null);
    setError(null);
    setUnauthorized(false);
    setLive(false);

    const parameters = JSON.parse(queryKey) as QueryParams;

    void (async () => {
      try {
        const response = await apiRequest(backendId, pathFor(endpoint, parameters));
        if (cancelled) return;
        setData(response.body as PayloadOf<E>);
        setError(null);
      } catch (raw) {
        if (cancelled) return;
        const core = raw instanceof CoreError ? raw : null;
        setUnauthorized(core?.unauthorized ?? false);
        setError(core?.message ?? "could not read this backend");
        return;
      }

      // The same path under `/sse/` — every read in this API streams at its own address.
      close = await openStream<PayloadOf<E>>(
        backendId,
        pathFor(endpoint, parameters),
        null,
        (frame) => {
          if (cancelled || frame.backend !== backendId) return;
          switch (frame.kind) {
            case "open":
              setLive(true);
              break;
            case "data":
              if (frame.data !== undefined) {
                setData(frame.data);
                setError(null);
              }
              break;
            case "closed":
              setLive(false);
              break;
            case "failed":
              setLive(false);
              setUnauthorized(true);
              setError(frame.message ?? "the stream stopped");
              break;
          }
        },
      );
      if (cancelled) close();
    })();

    return () => {
      cancelled = true;
      if (close !== null) close();
    };
  }, [backendId, generation, endpoint, queryKey]);

  return { data, error, live, unauthorized };
}

export interface LiveTranscript {
  events: TranscriptEvent[];
  error: string | null;
  live: boolean;
}

/**
 * Read a session's transcript and keep it current.
 *
 * `useLive`'s sibling, and deliberately not `useLive` itself. That hook *replaces* its payload
 * on every frame, which is right for a queue — the state as it stands now — and wrong for a
 * conversation, which is the sum of everything said rather than its latest state. This one
 * appends.
 *
 * Appending is what lets the stream carry a cursor: each frame holds only what follows the
 * last, so an hour-long chat costs what was said in the last second rather than the whole
 * thing on every word, and a reconnect resumes rather than replays.
 */
export function useTranscript(id: string): LiveTranscript {
  const { selected, generation } = useBackends();
  const backendId = selected?.id ?? null;
  const [events, setEvents] = useState<TranscriptEvent[]>([]);
  const [error, setError] = useState<string | null>(null);
  const [live, setLive] = useState(false);
  // In a ref rather than in state: the append below reads it as of that moment, not as of the
  // last render.
  const cursor = useRef(0);

  useEffect(() => {
    if (backendId === null) return;
    let cancelled = false;
    let close: (() => void) | null = null;

    setEvents([]);
    setError(null);
    setLive(false);
    cursor.current = 0;

    const append = (page: Transcript) => {
      if (cancelled || page.items.length === 0) return;
      cursor.current = Math.max(cursor.current, ...page.items.map((e) => e.seq));
      // Filtered on seq rather than trusted wholesale: a reconnect that raced an in-flight
      // frame can deliver the same event twice, and a transcript that repeats itself is worse
      // than one that lags.
      setEvents((prev) => {
        const seen = new Set(prev.map((e) => e.seq));
        const fresh = page.items.filter((e) => !seen.has(e.seq));
        return fresh.length === 0 ? prev : [...prev, ...fresh];
      });
      setError(null);
    };

    void (async () => {
      try {
        append(await readTranscript(backendId, id, 0));
      } catch (raw) {
        if (cancelled) return;
        setError(raw instanceof CoreError ? raw.message : "could not read the transcript");
        return;
      }
      if (cancelled) return;

      // Opened at the cursor the first page reached, so nothing between the read and the
      // subscription is missed and nothing already shown is sent again.
      close = await openStream<Transcript>(
        backendId,
        transcriptStreamPath(id),
        cursor.current,
        (frame) => {
          if (cancelled || frame.backend !== backendId) return;
          switch (frame.kind) {
            case "open":
              setLive(true);
              break;
            case "data":
              if (frame.data !== undefined) append(frame.data);
              break;
            case "closed":
              setLive(false);
              break;
            case "failed":
              setLive(false);
              setError(frame.message ?? "the transcript stream stopped");
              break;
          }
        },
      );
      if (cancelled) close();
    })();

    return () => {
      cancelled = true;
      if (close !== null) close();
    };
  }, [backendId, generation, id]);

  return { events, error, live };
}
