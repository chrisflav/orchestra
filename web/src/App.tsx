import { BrowserRouter, Navigate, Route, Routes } from "react-router-dom";
import { AuthProvider, useAuth } from "./auth";
import { Layout } from "./components/Layout";
import { Login } from "./components/Login";
import { OverviewProvider } from "./overview";
import { ThemeProvider } from "./theme";
import { Auth } from "./pages/Auth";
import { ConcertDetail } from "./pages/Concerts";
import { ListenerDetail, Listeners } from "./pages/Listeners";
import { Overview } from "./pages/Overview";
import { ProjectDetail, Projects } from "./pages/Projects";
import { Queue } from "./pages/Queue";
import { TaskDetail, Tasks } from "./pages/Tasks";

/**
 * Everything behind the session gate.
 *
 * The gate is a convenience, not the security boundary: it decides which screen to show, but
 * the server rejects unauthenticated `/api` and `/sse` requests regardless of what this
 * renders. `authenticated === null` is the not-yet-known state, before the session probe has
 * answered — rendering the login screen then would flash it at an already-signed-in user on
 * every page load.
 *
 * The overview subscription lives inside the gate, so it is only opened once there is a
 * session to open it with.
 */
function Gate() {
  const { authenticated } = useAuth();

  if (authenticated === null) {
    return (
      <div className="gate">
        <p className="empty">Loading…</p>
      </div>
    );
  }
  if (!authenticated) {
    return <Login />;
  }

  return (
    <OverviewProvider>
      <Routes>
        <Route element={<Layout />}>
          <Route index element={<Overview />} />
          <Route path="queue" element={<Queue />} />
          <Route path="concerts/:id" element={<ConcertDetail />} />
          <Route path="projects" element={<Projects />} />
          <Route path="projects/:id" element={<ProjectDetail />} />
          <Route path="listeners" element={<Listeners />} />
          <Route path="listeners/:name" element={<ListenerDetail />} />
          <Route path="tasks" element={<Tasks />} />
          <Route path="tasks/:id" element={<TaskDetail />} />
          <Route path="auth" element={<Auth />} />
          <Route path="*" element={<Navigate to="/" replace />} />
        </Route>
      </Routes>
    </OverviewProvider>
  );
}

export function App() {
  return (
    <BrowserRouter>
      <ThemeProvider>
        <AuthProvider>
          <Gate />
        </AuthProvider>
      </ThemeProvider>
    </BrowserRouter>
  );
}
