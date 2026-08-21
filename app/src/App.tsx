import { HashRouter, Navigate, Route, Routes } from "react-router-dom";

import { Layout } from "./components/Layout";
import { BackendsProvider, useBackends } from "./core/backends";
import { OverviewProvider } from "./core/overview";
import { ThemeProvider } from "./theme";
import { Auth } from "./pages/Auth";
import { Backends } from "./pages/Backends";
import { Chat, ChatDetail } from "./pages/Chat";
import { ConcertDetail } from "./pages/Concerts";
import { ListenerDetail, Listeners } from "./pages/Listeners";
import { Overview } from "./pages/Overview";
import { Queue } from "./pages/Queue";
import { TaskDetail, Tasks } from "./pages/Tasks";

/**
 * What is shown before there is a backend to show anything from.
 *
 * The dashboard's equivalent is its login screen, and the difference is the whole shape of this
 * app: there is no signing in here, because there is no single thing to sign in to. What stands
 * in its place is the list of backends — empty on first run, which is why the Backends screen is
 * also the first-run screen and needs no separate welcome.
 */
function Shell() {
  const { view, selected } = useBackends();

  if (view === null) {
    return (
      <div className="gate">
        <p className="empty">Loading…</p>
      </div>
    );
  }

  // No selection means either nothing is configured or the selected one was removed. Either
  // way the only screen with anything on it is the list, so every route resolves to it — and it
  // is a route rather than a modal, so the switcher and the theme toggle are still reachable.
  if (selected === null) {
    return (
      <Routes>
        <Route element={<Layout />}>
          <Route path="*" element={<Backends />} />
        </Route>
      </Routes>
    );
  }

  return (
    <Routes>
      <Route element={<Layout />}>
        <Route index element={<Overview />} />
        <Route path="queue" element={<Queue />} />
        <Route path="concerts/:id" element={<ConcertDetail />} />
        <Route path="listeners" element={<Listeners />} />
        <Route path="listeners/:name" element={<ListenerDetail />} />
        <Route path="chat" element={<Chat />} />
        <Route path="chat/:id" element={<ChatDetail />} />
        <Route path="tasks" element={<Tasks />} />
        <Route path="tasks/:id" element={<TaskDetail />} />
        <Route path="auth" element={<Auth />} />
        <Route path="backends" element={<Backends />} />
        <Route path="*" element={<Navigate to="/" replace />} />
      </Route>
    </Routes>
  );
}

/**
 * `HashRouter` rather than `BrowserRouter`: the bundle is served from `tauri://` (and from a
 * custom scheme on mobile), where there is no server to route a deep path back to `index.html`.
 * A hash keeps every route reachable after a reload without one.
 */
export function App() {
  return (
    <HashRouter>
      <ThemeProvider>
        <BackendsProvider>
          {/* One subscription to `overview` for the whole app: the bar reads it on every page
              and the Overview page reads the same one. It is inert until a backend is
              selected, which is why it can sit outside the branch that requires one. */}
          <OverviewProvider>
            <Shell />
          </OverviewProvider>
        </BackendsProvider>
      </ThemeProvider>
    </HashRouter>
  );
}
