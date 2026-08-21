/**
 * The backend switcher: the one control this app has that the dashboard cannot.
 *
 * It is in the title bar, it says which backend you are looking at, and it opens a list of the
 * others with what each is doing right now — because the useful question when you run several
 * is not "which am I on" but "is anything wrong somewhere else". The counts come from the slow
 * poll in `core/backends.tsx`, which is why they are there at all.
 *
 * Switching is a hard cut: the core tears down every stream against the backend being left
 * before the new one's are opened. That happens in `select`; this component only calls it.
 */

import { useEffect, useRef, useState } from "react";
import { useNavigate } from "react-router-dom";

import { healthOf, useBackends } from "../core/backends";
import type { Health } from "../core/backends";
import type { BackendRecord } from "../core/transport";

/** A backend's dot: its colour when it is answering, muted when it is not. */
export function BackendDot({
  backend,
  health,
}: {
  backend: BackendRecord;
  health: Health;
}) {
  const state = health.state === "ok" ? "on" : health.state === "unknown" ? "unknown" : "off";
  return <span className={`backend-dot hue-${backend.color} ${state}`} />;
}

function summarise(health: Health, backend: BackendRecord): string {
  if (!backend.hasSecret) return "no password stored";
  switch (health.state) {
    case "unknown":
      return "checking…";
    case "unauthorized":
      return "rejected the password";
    case "offline":
      return health.message ?? "not answering";
    case "ok": {
      const parts: string[] = [];
      if (health.running > 0) parts.push(`${health.running} running`);
      if (health.pending > 0) parts.push(`${health.pending} pending`);
      if (health.failed > 0) parts.push(`${health.failed} failed`);
      return parts.length === 0 ? "idle" : parts.join(" · ");
    }
  }
}

export function Switcher() {
  const { backends, selected, health, select } = useBackends();
  const [open, setOpen] = useState(false);
  const box = useRef<HTMLDivElement>(null);
  const navigate = useNavigate();

  // ⌘K / Ctrl-K opens it, Escape closes it, and ⌘1…9 jumps straight to one. The number keys
  // are what makes two backends feel like two windows rather than like a menu.
  useEffect(() => {
    const onKey = (event: KeyboardEvent) => {
      const meta = event.metaKey || event.ctrlKey;
      if (meta && event.key.toLowerCase() === "k") {
        event.preventDefault();
        setOpen((current) => !current);
        return;
      }
      if (event.key === "Escape") {
        setOpen(false);
        return;
      }
      if (meta && /^[1-9]$/.test(event.key)) {
        const target = backends[Number(event.key) - 1];
        if (target !== undefined) {
          event.preventDefault();
          setOpen(false);
          void select(target.id).then(() => navigate("/"));
        }
      }
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [backends, select, navigate]);

  // A click anywhere else closes it. Pointerdown rather than click, so the menu is gone before
  // whatever was clicked underneath reacts.
  useEffect(() => {
    if (!open) return;
    const onDown = (event: PointerEvent) => {
      if (box.current !== null && !box.current.contains(event.target as Node)) setOpen(false);
    };
    window.addEventListener("pointerdown", onDown);
    return () => window.removeEventListener("pointerdown", onDown);
  }, [open]);

  const choose = (backend: BackendRecord) => {
    setOpen(false);
    if (backend.id === selected?.id) return;
    // Home, not wherever you were: a task id belongs to the backend you just left.
    void select(backend.id).then(() => navigate("/"));
  };

  return (
    <div className="switcher" ref={box}>
      <button
        type="button"
        className="switcher-button"
        onClick={() => setOpen((current) => !current)}
        aria-haspopup="listbox"
        aria-expanded={open}
      >
        {selected === null ? (
          <span className="switcher-name">no backend</span>
        ) : (
          <>
            <BackendDot backend={selected} health={healthOf(health, selected.id)} />
            <span className="switcher-name">{selected.name}</span>
          </>
        )}
        <span className="switcher-caret">⌄</span>
      </button>

      {open && (
        <div className="switcher-menu" role="listbox">
          {backends.length === 0 && <p className="empty">Nothing added yet.</p>}
          {backends.map((backend, index) => (
            <button
              type="button"
              key={backend.id}
              className={`switcher-item${backend.id === selected?.id ? " current" : ""}`}
              onClick={() => choose(backend)}
              role="option"
              aria-selected={backend.id === selected?.id}
            >
              <BackendDot backend={backend} health={healthOf(health, backend.id)} />
              <span className="switcher-item-main">
                <span className="switcher-item-name">{backend.name}</span>
                <span className="switcher-item-note">{summarise(healthOf(health, backend.id), backend)}</span>
              </span>
              {index < 9 && <span className="switcher-item-key data">⌘{index + 1}</span>}
            </button>
          ))}
          <button
            type="button"
            className="switcher-item manage"
            onClick={() => {
              setOpen(false);
              navigate("/backends");
            }}
          >
            <span className="switcher-item-main">
              <span className="switcher-item-name">Manage backends…</span>
            </span>
          </button>
        </div>
      )}
    </div>
  );
}
