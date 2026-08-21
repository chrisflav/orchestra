import { NavLink, Outlet } from "react-router-dom";

import { useBackends } from "../core/backends";
import { useOverview } from "../core/overview";
import { ThemeToggle } from "../theme";
import { Mark } from "./Mark";
import { Switcher } from "./Switcher";

/**
 * Page chrome: a sticky bar over a centred column.
 *
 * The dashboard's layout with the switcher added and the sign-out button removed. There is no
 * signing out of an app that holds several credentials — the equivalent is removing a backend,
 * which is on the Backends screen where the rest of that lives.
 *
 * Detail routes nest under their list route (`/tasks/:id` under `/tasks`), so `NavLink`'s
 * default prefix matching highlights the right item; only Overview needs `end`, since every
 * path is a prefix match for `/`.
 */
export function Layout() {
  const { selected } = useBackends();
  // The overview drives two badges in the bar. It is the one payload the chrome itself reads,
  // and it is the *shared* subscription rather than a second one: the Overview page reads the
  // same stream. With no backend selected it simply holds nothing.
  const { data } = useOverview();
  const counts = data?.counts;

  const inFlight = counts === undefined ? 0 : counts.running + counts.pending;
  const authExhausted = counts !== undefined && counts.authTotal > 0 && counts.authFree === 0;

  return (
    <>
      <header className="topbar">
        <div className="topbar-inner">
          <NavLink to="/" className="brand">
            <Mark size={20} />
            <span className="brand-name">Orchestra</span>
          </NavLink>

          <Switcher />

          {/* Only Backends, until there is a backend: every other destination is a view of
              one, and offering them before there is one to view is offering a blank page. */}
          <nav className="nav">
            {selected === null ? (
              <NavLink to="/backends" className="nav-link active">
                Backends
              </NavLink>
            ) : (
              <>
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
                <NavLink to="/chat" className="nav-link">
                  Chat
                </NavLink>
                <NavLink to="/tasks" className="nav-link">
                  Tasks
                </NavLink>
                <NavLink to="/listeners" className="nav-link">
                  Listeners
                </NavLink>
                <NavLink to="/auth" className="nav-link">
                  Auth
                  {authExhausted && <span className="nav-count alert">0</span>}
                </NavLink>
                <NavLink to="/backends" className="nav-link">
                  Backends
                </NavLink>
              </>
            )}
          </nav>

          <div className="topbar-end">
            <ThemeToggle />
          </div>
        </div>
      </header>

      <main className="page">
        <Outlet />
      </main>
    </>
  );
}
