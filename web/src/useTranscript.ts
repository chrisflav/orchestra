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
/**
 * @param finished the session has reached a terminal state, so the transcript is complete and
 *   the stream is not coming back. The first page is still read — a finished conversation is
 *   still worth showing — but nothing subscribes, and a stream that drops is not retried.
 */
export function useTranscript(id: string, finished = false): LiveTranscript {
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
  // In a ref rather than the effect's deps: this flips from false to true partway through a
  // session's life, and re-running the effect would clear the transcript and start over.
  const finishedRef = useRef(finished);
  finishedRef.current = finished;

  useEffect(() => {
    let cancelled = false;
    let source: EventSource | null = null;
    // What throttles the probe below. A stream drops for ordinary reasons — the server closes a
    // quiet one on its own timer — and `EventSource` retries every few seconds while a server is
    // down, firing `onerror` each time. Probing on every one of those turns an outage into two
    // requests where there was one, from every open tab.
    let errorsSinceOpen = 0;
    let lastProbe = 0;

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
      if (cancelled || finishedRef.current) return;
      source = new EventSource(transcriptStreamUrl(id, cursor.current), {
        withCredentials: true,
      });
      source.onopen = () => {
        if (cancelled) return;
        errorsSinceOpen = 0;
        setLive(true);
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
        // The server closes the stream of a finished session on purpose, and `EventSource`
        // cannot tell that from a connection it should retry — so it retries, every few seconds,
        // for as long as the tab is open, each attempt costing a full read of the transcript on
        // the far end. Closing it here is what makes "the conversation is over" an ending rather
        // than a loop.
        if (finishedRef.current) {
          source?.close();
          return;
        }
        errorsSinceOpen += 1;
        // `EventSource` reconnects on its own but cannot say *why* it dropped, so a revoked
        // session would reconnect forever. Re-probe over fetch, which can, and route to the
        // login screen. The reconnect resumes from the cursor either way.
        //
        // Not on the first error and not more than once a quarter-minute: a single drop is
        // routine and the reconnect answers it, while a session that has genuinely been revoked
        // keeps failing and is caught on the second try a few seconds later.
        const now = Date.now();
        if (errorsSinceOpen < 2 || now - lastProbe < 15000) return;
        lastProbe = now;
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
