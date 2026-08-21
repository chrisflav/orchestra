import { useState } from "react";
import { cancelTask, UnauthorizedError } from "../api";
import { useAuth } from "../auth";

/** What the control is in the middle of, which is the only state it has. */
type Phase =
  | { kind: "idle" }
  /** Asked, not yet confirmed. */
  | { kind: "armed" }
  | { kind: "busy" }
  | { kind: "said"; text: string; bad: boolean };

/**
 * Stop this run, from the page that shows it running.
 *
 * One task, named by the id in the URL: the other agents on this host are not part of the
 * decision the reader is making, and a control that stopped them too would be a different and
 * much larger button wearing this one's label.
 *
 * Two clicks rather than one, because cancelling throws away a partial run and the second click
 * is what stands between a mis-aimed pointer and an agent losing an hour of work. It is a pair
 * of buttons rather than `window.confirm`, which blocks the page — including the stream that is
 * at that moment telling you the task just finished on its own.
 *
 * Nothing here rewrites the status beside it. The worker running the task is what stamps the
 * entry `cancelled`, and the subscription carries that back a moment later — at which point the
 * task is no longer running and this control is gone with the state it was holding.
 */
export function CancelTask({ id }: { id: string }) {
  const [phase, setPhase] = useState<Phase>({ kind: "idle" });
  const { onUnauthorized } = useAuth();

  const run = async () => {
    setPhase({ kind: "busy" });
    try {
      await cancelTask(id);
      setPhase({ kind: "said", text: "Cancel sent.", bad: false });
    } catch (err: unknown) {
      // A revoked session is the app's problem, not this button's: hand it over and let the
      // login screen take the page.
      if (err instanceof UnauthorizedError) {
        onUnauthorized();
        return;
      }
      setPhase({ kind: "said", text: err instanceof Error ? err.message : String(err), bad: true });
    }
  };

  if (phase.kind === "armed") {
    return (
      <span className="cancel">
        <span className="cancel-ask">Stop this run?</span>
        <button className="cancel-button danger" type="button" onClick={() => void run()}>
          Cancel it
        </button>
        <button className="cancel-button" type="button" onClick={() => setPhase({ kind: "idle" })}>
          Keep going
        </button>
      </span>
    );
  }

  return (
    <span className="cancel">
      <button
        className="cancel-button danger"
        type="button"
        disabled={phase.kind === "busy"}
        onClick={() => setPhase({ kind: "armed" })}
      >
        {phase.kind === "busy" ? "Cancelling…" : "Cancel"}
      </button>
      {phase.kind === "said" && (
        <span
          className={phase.bad ? "cancel-note bad" : "cancel-note"}
          role={phase.bad ? "alert" : "status"}
        >
          {phase.text}
        </span>
      )}
    </span>
  );
}
