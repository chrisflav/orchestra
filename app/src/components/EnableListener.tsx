import { useState } from "react";

import { setListenerEnabled } from "../core/api";
import { useSelectedBackend } from "../core/backends";
import { CoreError } from "../core/transport";

/**
 * Turn one listener on or off.
 *
 * The only configuration this app writes. A listener's source, its interval and its prompt
 * template are a document, and editing a document is what the dashboard is for; on/off is a
 * switch, and a switch is exactly the thing you want to reach for from a phone when something
 * is queueing work it should not be.
 *
 * No confirmation, unlike Cancel: this is reversible by pressing it again, and nothing is lost
 * in between. The state on screen is not rewritten here either — the stream carries the new
 * value back, so what is shown is always what the backend actually holds.
 */
export function EnableListener({ name, enabled }: { name: string; enabled: boolean }) {
  const backend = useSelectedBackend();
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const toggle = async () => {
    setBusy(true);
    setError(null);
    try {
      await setListenerEnabled(backend.id, name, !enabled);
    } catch (raw) {
      setError(raw instanceof CoreError ? raw.message : String(raw));
    } finally {
      setBusy(false);
    }
  };

  return (
    <span className="cancel">
      <button className="cancel-button" type="button" disabled={busy} onClick={() => void toggle()}>
        {busy ? "…" : enabled ? "Disable" : "Enable"}
      </button>
      {error !== null && (
        <span className="cancel-note bad" role="alert">
          {error}
        </span>
      )}
    </span>
  );
}
