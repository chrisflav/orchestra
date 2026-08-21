import { useState } from "react";

import { cancelTask } from "../core/api";
import { useSelectedBackend } from "../core/backends";
import { CoreError } from "../core/transport";

/** What the control is in the middle of, which is the only state it has. */
type Phase =
  | { kind: "idle" }
  /** Asked, not yet confirmed. */
  | { kind: "armed" }
  | { kind: "busy" }
  | { kind: "said"; text: string; bad: boolean };

/**
 * Stop this run, on the backend the page is showing.
 *
 * One task, named by the id in the URL: the other agents on that host are not part of the
 * decision the reader is making, and a control that stopped them too would be a different and
 * much larger button wearing this one's label.
 *
 * Two clicks rather than one, because cancelling throws away a partial run and the second click
 * is what stands between a mis-aimed pointer and an agent losing an hour of work. It is a pair
 * of buttons rather than `window.confirm`, which blocks the page — including the stream that is
 * at that moment telling you the task just finished on its own.
 *
 * Nothing here rewrites the status beside it. The worker running the task is what stamps the
 * entry `cancelled`, and the subscription carries that back a moment later.
 */
export function CancelTask({ id }: { id: string }) {
  const [phase, setPhase] = useState<Phase>({ kind: "idle" });
  const backend = useSelectedBackend();

  const run = async () => {
    setPhase({ kind: "busy" });
    try {
      await cancelTask(backend.id, id);
      setPhase({ kind: "said", text: "Cancel sent.", bad: false });
    } catch (error: unknown) {
      setPhase({
        kind: "said",
        text: error instanceof CoreError ? error.message : String(error),
        bad: true,
      });
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
