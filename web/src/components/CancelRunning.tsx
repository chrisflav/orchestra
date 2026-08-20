import { useState } from "react";
import { cancelRunningTasks, UnauthorizedError } from "../api";
import { useAuth } from "../auth";

/** What the button is in the middle of, which is the only state this control has. */
type Phase =
  | { kind: "idle" }
  /** Asked, not yet confirmed. */
  | { kind: "armed" }
  | { kind: "busy" }
  | { kind: "said"; text: string; bad: boolean };

const tasks = (n: number) => `${n} ${n === 1 ? "task" : "tasks"}`;

/**
 * Stop what the daemon is running, from the page that shows it running.
 *
 * Two clicks rather than one, because cancelling throws away a partial run and the second
 * click is what stands between a mis-aimed pointer and every agent on this host stopping at
 * once. The confirmation counts the tasks, since "cancel" means something different at one
 * than at nine. It is a pair of buttons rather than `window.confirm`, which blocks the page —
 * including the stream that is at that moment telling you a task just finished.
 *
 * Nothing here touches the rendered queue. The worker running each task is what stamps its
 * entry `cancelled`, and the SSE subscription carries that back a moment later; a status
 * guessed here would be a second writer disagreeing with the first.
 */
export function CancelRunning({ running }: { running: number }) {
  const [phase, setPhase] = useState<Phase>({ kind: "idle" });
  const { onUnauthorized } = useAuth();

  const run = async () => {
    setPhase({ kind: "busy" });
    try {
      const result = await cancelRunningTasks();
      setPhase({
        kind: "said",
        text:
          result.cancelled === 0
            ? "Nothing was running by then."
            : `Cancelled ${tasks(result.cancelled)}.`,
        bad: false,
      });
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
        <span className="cancel-ask">Cancel {tasks(running)}?</span>
        <button className="cancel-button danger" type="button" onClick={() => void run()}>
          Cancel them
        </button>
        <button className="cancel-button" type="button" onClick={() => setPhase({ kind: "idle" })}>
          Keep running
        </button>
      </span>
    );
  }

  return (
    <span className="cancel">
      {phase.kind === "said" && (
        <span
          className={phase.bad ? "cancel-note bad" : "cancel-note"}
          role={phase.bad ? "alert" : "status"}
        >
          {phase.text}
        </span>
      )}
      <button
        className="cancel-button danger"
        type="button"
        disabled={running === 0 || phase.kind === "busy"}
        {...(running === 0 ? { title: "Nothing is running." } : {})}
        onClick={() => setPhase({ kind: "armed" })}
      >
        {phase.kind === "busy" ? "Cancelling…" : "Cancel running"}
      </button>
    </span>
  );
}
