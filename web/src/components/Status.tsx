/**
 * The status vocabulary, used identically on every page.
 *
 * Statuses come from four different Lean enums (queue, task, concert, issue) that overlap but
 * do not match. Mapping all of them onto five orchestral sections here is what makes a colour
 * mean one thing across the whole console: brass is working, strings are waiting, winds
 * finished, percussion failed, and a rest is something that stopped without finishing.
 */
type Section = "running" | "pending" | "done" | "failed" | "rest";

const SECTION: Record<string, Section> = {
  running: "running",
  pending: "pending",
  claimed: "pending",
  open: "pending",
  done: "done",
  completed: "done",
  available: "done",
  failed: "failed",
  blocked: "failed",
  rejected: "failed",
  cancelled: "rest",
  unfinished: "rest",
  abandoned: "rest",
};

export function sectionOf(status: string): Section {
  return SECTION[status] ?? "rest";
}

export function Status({ status }: { status: string }) {
  return (
    <span className={`status status-${sectionOf(status)}`}>
      <span className="status-dot" />
      <span className="status-label">{status}</span>
    </span>
  );
}

/** A listener's on/off state, which is a switch rather than a lifecycle. */
export function EnabledStatus({ enabled }: { enabled: boolean }) {
  return (
    <span className={`status status-${enabled ? "done" : "rest"}`}>
      <span className="status-dot" />
      <span className="status-label">{enabled ? "enabled" : "disabled"}</span>
    </span>
  );
}
