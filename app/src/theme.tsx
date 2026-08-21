import { createContext, useCallback, useContext, useEffect, useMemo, useState } from "react";
import type { ReactNode } from "react";

export type ThemePreference = "system" | "light" | "dark";

const STORAGE_KEY = "orchestra-theme";
const ORDER: ThemePreference[] = ["system", "light", "dark"];

interface ThemeState {
  preference: ThemePreference;
  /** Move to the next preference: system, then light, then dark, then back. */
  cycle: () => void;
}

const ThemeContext = createContext<ThemeState | null>(null);

function read(): ThemePreference {
  try {
    const saved = localStorage.getItem(STORAGE_KEY);
    if (saved === "light" || saved === "dark") return saved;
  } catch {
    /* private mode */
  }
  return "system";
}

/**
 * Light and dark, defaulting to whatever the operating system says.
 *
 * "System" is a stored preference in its own right rather than the absence of one, so someone
 * who has explicitly chosen it keeps following their machine when it switches at sunset. The
 * stylesheet reads `prefers-color-scheme` directly, so this only has to add or remove the
 * `data-theme` attribute that overrides it — see `index.html` for the pre-paint half.
 */
export function ThemeProvider({ children }: { children: ReactNode }) {
  const [preference, setPreference] = useState<ThemePreference>(read);

  useEffect(() => {
    const root = document.documentElement;
    if (preference === "system") root.removeAttribute("data-theme");
    else root.setAttribute("data-theme", preference);
    try {
      if (preference === "system") localStorage.removeItem(STORAGE_KEY);
      else localStorage.setItem(STORAGE_KEY, preference);
    } catch {
      /* private mode: the choice lasts for this session only */
    }
  }, [preference]);

  const cycle = useCallback(() => {
    setPreference((current) => {
      const next = ORDER[(ORDER.indexOf(current) + 1) % ORDER.length];
      return next ?? "system";
    });
  }, []);

  const value = useMemo<ThemeState>(() => ({ preference, cycle }), [preference, cycle]);
  return <ThemeContext.Provider value={value}>{children}</ThemeContext.Provider>;
}

export function useTheme(): ThemeState {
  const ctx = useContext(ThemeContext);
  if (ctx === null) throw new Error("useTheme must be used within a ThemeProvider");
  return ctx;
}

const LABEL: Record<ThemePreference, string> = {
  system: "Match system",
  light: "Light",
  dark: "Dark",
};

/** Sun, moon, or a screen for "whatever the system says". */
function ThemeIcon({ preference }: { preference: ThemePreference }) {
  if (preference === "light") {
    return (
      <svg viewBox="0 0 16 16" width="15" height="15" aria-hidden="true">
        <circle cx="8" cy="8" r="3.1" fill="currentColor" />
        <g stroke="currentColor" strokeWidth="1.3" strokeLinecap="round">
          <path d="M8 1v1.6M8 13.4V15M1 8h1.6M13.4 8H15M3 3l1.1 1.1M11.9 11.9L13 13M13 3l-1.1 1.1M4.1 11.9L3 13" />
        </g>
      </svg>
    );
  }
  if (preference === "dark") {
    return (
      <svg viewBox="0 0 16 16" width="15" height="15" aria-hidden="true">
        <path
          d="M13.2 9.6A5.6 5.6 0 0 1 6.4 2.8a5.6 5.6 0 1 0 6.8 6.8Z"
          fill="currentColor"
        />
      </svg>
    );
  }
  return (
    <svg viewBox="0 0 16 16" width="15" height="15" aria-hidden="true">
      <rect
        x="1.6"
        y="2.6"
        width="12.8"
        height="9"
        rx="1.6"
        fill="none"
        stroke="currentColor"
        strokeWidth="1.3"
      />
      <path d="M5 13.8h6" stroke="currentColor" strokeWidth="1.3" strokeLinecap="round" />
    </svg>
  );
}

export function ThemeToggle() {
  const { preference, cycle } = useTheme();
  return (
    <button
      className="icon-button"
      onClick={cycle}
      title={`Theme: ${LABEL[preference]}`}
      aria-label={`Theme: ${LABEL[preference]}. Change it.`}
    >
      <ThemeIcon preference={preference} />
    </button>
  );
}
