import { NavLink, Outlet } from "react-router-dom";
import { useAuth } from "../auth";
import { useOverview } from "../overview";
import { ThemeToggle } from "../theme";
import { Mark } from "./Mark";

/**
 * Page chrome: a sticky bar over a centred column.
 *
 * Six destinations fit across the top comfortably, which is what removed the need for a rail.
 * Concerts folded into Queue — a concert *is* queue activity, and the two pages were listing
 * the same runs — so the nav lost an item rather than gaining one to compensate.
 *
 * Detail routes nest under their list route (`/tasks/:id` under `/tasks`), so `NavLink`'s
 * default prefix matching highlights the right item with no extra wiring; only Overview needs
 * `end`, since every path is a prefix match for `/`.
 */
export function Layout() {
  const { signOut } = useAuth();
  const { data } = useOverview();
  const counts = data?.counts;

  const inFlight = counts === undefined ? 0 : counts.running + counts.pending;
  const authExhausted = counts !== undefined && counts.authTotal > 0 && counts.authFree === 0;
  // `undefined` while the first payload is in flight, so the destination does not flash in
  // and out on load; only a loaded payload with no tracker hides it for good.
  const taxisUrl = data?.taxisUrl ?? null;

  return (
    <>
      <header className="topbar">
        <div className="topbar-inner">
          <NavLink to="/" className="brand">
            <Mark size={20} />
            <span className="brand-name">Orchestra</span>
          </NavLink>

          <nav className="nav">
            <NavLink to="/" end className="nav-link">
              Overview
            </NavLink>
            <NavLink to="/queue" className="nav-link">
              Queue
              {inFlight > 0 && (
                <span className={`nav-count ${counts && counts.running > 0 ? "busy" : ""}`}>
                  {inFlight}
                </span>
              )}
            </NavLink>
            <NavLink to="/tasks" className="nav-link">
              Tasks
            </NavLink>
            {/* Projects live in taxis, which already displays them well; re-rendering them
                here was a worse copy of a tool the operator already has open. The
                destination disappears entirely when no tracker is configured, rather than
                becoming a link to nowhere. */}
            {taxisUrl !== null && (
              <a
                className="nav-link nav-out"
                href={taxisUrl}
                target="_blank"
                rel="noreferrer noopener"
              >
                Projects
                <svg viewBox="0 0 12 12" width="9" height="9" aria-hidden="true">
                  <path
                    d="M4 2h6v6M10 2 3 9"
                    fill="none"
                    stroke="currentColor"
                    strokeWidth="1.6"
                    strokeLinecap="round"
                    strokeLinejoin="round"
                  />
                </svg>
                <span className="visually-hidden"> (opens taxis in a new tab)</span>
              </a>
            )}
            <NavLink to="/listeners" className="nav-link">
              Listeners
            </NavLink>
            <NavLink to="/auth" className="nav-link">
              Auth
              {authExhausted && <span className="nav-count alert">0</span>}
            </NavLink>
          </nav>

          <div className="topbar-end">
            <ThemeToggle />
            <button className="text-button" onClick={() => void signOut()}>
              Sign out
            </button>
          </div>
        </div>
      </header>

      <main className="page">
        <Outlet />
      </main>
    </>
  );
}
