import { createContext, useCallback, useContext, useEffect, useMemo, useState } from "react";
import type { ReactNode } from "react";
import { checkSession, login as apiLogin, logout as apiLogout } from "./api";

/**
 * Session state for the whole app.
 *
 * The password is exchanged once for an `HttpOnly` cookie and is never held here — not in
 * state, not in `localStorage`. That is the point of the cookie: script running on this page
 * cannot read the credential, and every `fetch`/`EventSource` still carries it because the
 * browser attaches it. The only thing this context tracks is the *answer* to "am I logged
 * in", which the server is the authority on.
 */
interface AuthState {
  /** `null` while the initial session probe is in flight. */
  authenticated: boolean | null;
  signIn: (password: string) => Promise<boolean>;
  signOut: () => Promise<void>;
  /** Called when any request comes back 401, so the app falls back to the login screen. */
  onUnauthorized: () => void;
}

const AuthContext = createContext<AuthState | null>(null);

export function AuthProvider({ children }: { children: ReactNode }) {
  const [authenticated, setAuthenticated] = useState<boolean | null>(null);

  useEffect(() => {
    let cancelled = false;
    void checkSession().then((ok) => {
      if (!cancelled) setAuthenticated(ok);
    });
    return () => {
      cancelled = true;
    };
  }, []);

  const signIn = useCallback(async (password: string) => {
    const ok = await apiLogin(password);
    if (ok) setAuthenticated(true);
    return ok;
  }, []);

  const signOut = useCallback(async () => {
    await apiLogout();
    setAuthenticated(false);
  }, []);

  const onUnauthorized = useCallback(() => setAuthenticated(false), []);

  const value = useMemo<AuthState>(
    () => ({ authenticated, signIn, signOut, onUnauthorized }),
    [authenticated, signIn, signOut, onUnauthorized],
  );

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
}

export function useAuth(): AuthState {
  const ctx = useContext(AuthContext);
  if (!ctx) throw new Error("useAuth must be used within an AuthProvider");
  return ctx;
}
