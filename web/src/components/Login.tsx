import { useState } from "react";
import type { FormEvent } from "react";
import { useAuth } from "../auth";
import { ThemeToggle } from "../theme";
import { Mark } from "./Mark";

/**
 * The login screen.
 *
 * A real `<form>` with a `password` input, so browsers and password managers treat it as one:
 * the field is masked, offered for saving, and submitted on Enter. The value goes straight to
 * `signIn` and is never stored — the server answers with a cookie.
 *
 * The hint below the button names where the password comes from, because the most common way
 * to arrive here is having just started the server and not read its output.
 */
export function Login() {
  const { signIn } = useAuth();
  const [password, setPassword] = useState("");
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);

  const onSubmit = async (event: FormEvent) => {
    event.preventDefault();
    if (busy) return;
    setBusy(true);
    setError(null);
    try {
      const ok = await signIn(password);
      if (!ok) {
        setError("That password was not accepted.");
        setPassword("");
      }
    } catch (err: unknown) {
      setError(err instanceof Error ? err.message : String(err));
    } finally {
      setBusy(false);
    }
  };

  return (
    <div className="gate">
      <div className="gate-corner">
        <ThemeToggle />
      </div>
      <div className="gate-card">
        <div className="gate-mark">
          <Mark size={30} />
        </div>
        <h1 className="gate-title">Orchestra</h1>
        <p className="gate-note">Sign in to watch the queue.</p>

        <form className="gate-form" onSubmit={(e) => void onSubmit(e)}>
          <label className="gate-label" htmlFor="password">
            Password
          </label>
          <input
            id="password"
            name="password"
            className="gate-input"
            type="password"
            autoComplete="current-password"
            autoFocus
            value={password}
            onChange={(e) => setPassword(e.target.value)}
            disabled={busy}
          />
          {error !== null && (
            <p className="gate-error" role="alert">
              {error}
            </p>
          )}
          <button className="gate-button" type="submit" disabled={busy || password === ""}>
            {busy ? "Signing in…" : "Sign in"}
          </button>
        </form>

        <p className="gate-hint">
          <code>orchestra dashboard</code> prints the password when it generates one, and keeps it
          in <code>&lt;data&gt;/dashboard.secret</code>.
        </p>
      </div>
    </div>
  );
}
