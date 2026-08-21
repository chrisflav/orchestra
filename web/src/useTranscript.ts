import { useEffect, useRef, useState } from "react";
import { transcriptStreamUrl, transcriptUrl, UnauthorizedError } from "./api";
import type { Transcript, TranscriptEvent } from "./api";
import { useAuth } from "./auth";

export interface LiveTranscript {
  events: TranscriptEvent[];
  /** Set when the first load failed; a later frame clears it. */
  error: string | null;
  /** Whether the stream is currently attached. */
  live: boolean;
}

/**
 * Read a session's transcript and keep it current.
 *
 * `useLiveData`'s sibling, and deliberately not `useLiveData` itself. That hook *replaces* its
 * payload on every frame, which is right for a dashboard — the queue as it stands now — and
 * wrong for a conversation, which is the sum of everything said rather than its latest state.
 * This one appends.
 *
 * Appending is what lets the stream carry a cursor. Each frame holds only what follows the last
 * one, so an hour-long chat costs the network what was said in the last second rather than
 * re-sending the whole thing on every word — and a reconnect resumes from the last seq seen
 * instead of starting over.
 */
export function useTranscript(id: string): LiveTranscript {
  const [events, setEvents] = useState<TranscriptEvent[]>([]);
  const [error, setError] = useState<string | null>(null);
  const [live, setLive] = useState(false);
  const { onUnauthorized } = useAuth();
  // Kept in a ref so the effect does not re-subscribe when the callback identity changes;
  // re-running it would tear down and rebuild the stream.
  const onUnauthorizedRef = useRef(onUnauthorized);
  onUnauthorizedRef.current = onUnauthorized;
  // The cursor, in a ref rather than in state: it is read by the append below and must be the
  // value as of that moment, not as of the last render.
  const cursor = useRef(0);

  useEffect(() => {
    let cancelled = false;
    let source: EventSource | null = null;

    // A different session means the events on screen belong to another conversation.
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

    const subscribe = () => {
      if (cancelled) return;
      source = new EventSource(transcriptStreamUrl(id, cursor.current), {
        withCredentials: true,
      });
      source.onopen = () => {
        if (!cancelled) setLive(true);
      };
      source.onmessage = (event: MessageEvent<string>) => {
        try {
          append(JSON.parse(event.data) as Transcript);
        } catch {
          /* a malformed frame is dropped; the next one supersedes it */
        }
      };
      source.onerror = () => {
        if (cancelled) return;
        setLive(false);
        // `EventSource` reconnects on its own but cannot say *why* it dropped, so a revoked
        // session would reconnect forever. Re-probe over fetch, which can, and route to the
        // login screen. The reconnect resumes from the cursor either way.
        void fetch(transcriptUrl(id, cursor.current), { credentials: "same-origin" }).then(
          (r) => {
            if (r.status === 401 && !cancelled) {
              source?.close();
              onUnauthorizedRef.current();
            }
          },
          () => {
            /* transport failure; EventSource is already retrying */
          },
        );
      };
    };

    // The first page comes over fetch so a 401 is distinguishable from a dropped connection,
    // exactly as `useLiveData` does it.
    void fetch(transcriptUrl(id, 0), {
      credentials: "same-origin",
      headers: { Accept: "application/json" },
    })
      .then(async (response) => {
        if (cancelled) return;
        if (response.status === 401) {
          onUnauthorizedRef.current();
          return;
        }
        if (!response.ok) {
          setError(`could not read the transcript (${response.status})`);
          return;
        }
        append((await response.json()) as Transcript);
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
  }, [id]);

  return { events, error, live };
}
