/**
 * The screen the dashboard has no equivalent of: every backend, and what each is doing.
 *
 * It is the app's home when nothing is selected, and its answer to three questions a client
 * with one backend never has to ask — where else is work running, which of these is not
 * answering, and where is my password actually stored.
 */

import { useState } from "react";
import { useNavigate } from "react-router-dom";

import { BackendDot } from "../components/Switcher";
import { Empty, List } from "../components/List";
import { Notice, PageHead, Section } from "../components/Page";
import { Time } from "../components/Time";
import { healthOf, useBackends } from "../core/backends";
import type { Health } from "../core/backends";
import { CoreError, registry } from "../core/transport";
import type { BackendColor, BackendRecord, ProbeResult } from "../core/transport";

const COLORS: BackendColor[] = ["strings", "brass", "winds", "perc"];

function stateWord(health: Health, backend: BackendRecord): string {
  if (!backend.hasSecret) return "no password stored";
  switch (health.state) {
    case "unknown":
      return "checking…";
    case "ok":
      return "answering";
    case "unauthorized":
      return "rejected the password";
    case "offline":
      return "not answering";
  }
}

/** The add-or-edit form. One form for both, because they differ only in what is prefilled. */
function BackendForm({
  existing,
  onDone,
  onCancel,
}: {
  existing?: BackendRecord;
  onDone: () => void;
  onCancel: () => void;
}) {
  const { add, update } = useBackends();
  const [name, setName] = useState(existing?.name ?? "");
  const [url, setUrl] = useState(existing?.url ?? "");
  const [secret, setSecret] = useState("");
  const [color, setColor] = useState<BackendColor>(existing?.color ?? "strings");
  const [insecure, setInsecure] = useState(existing?.allowInsecureTls ?? false);
  const [probe, setProbe] = useState<ProbeResult | null>(null);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const editing = existing !== undefined;

  const test = async () => {
    setBusy(true);
    setError(null);
    setProbe(null);
    try {
      setProbe(await registry.probe({ name, url, secret, color, allowInsecureTls: insecure }));
    } catch (raw) {
      setError(raw instanceof CoreError ? raw.message : String(raw));
    } finally {
      setBusy(false);
    }
  };

  const save = async () => {
    setBusy(true);
    setError(null);
    try {
      if (editing) {
        await update({
          id: existing.id,
          name,
          url,
          color,
          allowInsecureTls: insecure,
          // An empty box means "leave the stored password alone", which is what makes a
          // rename not ask for a credential it already has.
          ...(secret === "" ? {} : { secret }),
        });
      } else {
        await add({ name, url, secret, color, allowInsecureTls: insecure });
      }
      onDone();
    } catch (raw) {
      setError(raw instanceof CoreError ? raw.message : String(raw));
    } finally {
      setBusy(false);
    }
  };

  return (
    <div className="panel form">
      <label className="field">
        <span className="field-key">Name</span>
        <input
          className="field-input"
          value={name}
          placeholder="home"
          onChange={(e) => setName(e.target.value)}
        />
      </label>

      <label className="field">
        <span className="field-key">Address</span>
        <input
          className="field-input data"
          value={url}
          placeholder="https://orchestra.example.com"
          autoCapitalize="off"
          autoCorrect="off"
          spellCheck={false}
          onChange={(e) => setUrl(e.target.value)}
        />
        <span className="field-note">
          The origin the dashboard is served on. A bare host is taken as <code>https</code>.
        </span>
      </label>

      <label className="field">
        <span className="field-key">Password</span>
        <input
          className="field-input data"
          type="password"
          value={secret}
          placeholder={editing ? "unchanged" : "the dashboard password"}
          onChange={(e) => setSecret(e.target.value)}
        />
        <span className="field-note">
          The same secret <code>orchestra</code> uses — <code>--password</code>,{" "}
          <code>$ORCHESTRA_DASHBOARD_PASSWORD</code>, or <code>&lt;data&gt;/dashboard.secret</code>{" "}
          on the daemon's host. It is stored by the operating system, never by this window.
        </span>
      </label>

      <div className="field">
        <span className="field-key">Colour</span>
        <div className="swatches">
          {COLORS.map((c) => (
            <button
              key={c}
              type="button"
              className={`swatch hue-${c}${c === color ? " on" : ""}`}
              onClick={() => setColor(c)}
              aria-label={c}
            />
          ))}
        </div>
      </div>

      <label className="field check">
        <input
          type="checkbox"
          checked={insecure}
          onChange={(e) => setInsecure(e.target.checked)}
        />
        <span>
          <span className="field-key">Skip TLS verification</span>
          <span className="field-note">
            For a box on your own network with a self-signed certificate. Refused for any address
            that is not loopback or private-range — there it would not be a preference, it would
            be the absence of TLS.
          </span>
        </span>
      </label>

      {probe !== null && (
        <p className={probe.ok ? "form-note good" : "form-note bad"} role="status">
          {probe.ok
            ? `Reached it${
                probe.running === null
                  ? ""
                  : ` — ${probe.running} running, ${probe.pending ?? 0} pending`
              }.`
            : probe.message}
        </p>
      )}
      {error !== null && (
        <p className="form-note bad" role="alert">
          {error}
        </p>
      )}

      <div className="form-actions">
        <button className="button" type="button" disabled={busy} onClick={() => void test()}>
          Test
        </button>
        <button
          className="button primary"
          type="button"
          disabled={busy || name === "" || url === "" || (!editing && secret === "")}
          onClick={() => void save()}
        >
          {editing ? "Save" : "Add"}
        </button>
        <button className="button" type="button" onClick={onCancel}>
          Cancel
        </button>
      </div>
    </div>
  );
}

/** One row: the backend, what it is doing, and what can be done to it. */
function BackendRow({ backend }: { backend: BackendRecord }) {
  const { health, selected, select, remove } = useBackends();
  const navigate = useNavigate();
  const [editing, setEditing] = useState(false);
  const [confirming, setConfirming] = useState(false);
  const state = healthOf(health, backend.id);
  const current = backend.id === selected?.id;

  if (editing) {
    return (
      <BackendForm
        existing={backend}
        onDone={() => setEditing(false)}
        onCancel={() => setEditing(false)}
      />
    );
  }

  return (
    <div className={current ? "backend-row current" : "backend-row"}>
      <BackendDot backend={backend} health={state} />
      <div className="backend-main">
        <div className="backend-name">
          {backend.name}
          {current && <span className="backend-tag">selected</span>}
          {backend.allowInsecureTls && <span className="backend-tag warn">TLS unverified</span>}
        </div>
        <div className="backend-meta data">{backend.url}</div>
        <div className="backend-meta">
          {stateWord(state, backend)}
          {state.state === "ok" && (
            <>
              <span className="sep">·</span>
              {state.running} running
              <span className="sep">·</span>
              {state.pending} pending
              {state.failed > 0 && (
                <>
                  <span className="sep">·</span>
                  <span className="bad">{state.failed} failed</span>
                </>
              )}
            </>
          )}
          {state.checkedAt !== null && (
            <>
              <span className="sep">·</span>
              <Time iso={new Date(state.checkedAt).toISOString()} />
            </>
          )}
        </div>
        {state.message !== null && state.state !== "ok" && (
          <div className="backend-meta bad">{state.message}</div>
        )}
      </div>
      <div className="backend-actions">
        {!current && (
          <button
            className="button"
            type="button"
            onClick={() => void select(backend.id).then(() => navigate("/"))}
          >
            Switch
          </button>
        )}
        <button className="button" type="button" onClick={() => setEditing(true)}>
          Edit
        </button>
        {confirming ? (
          <>
            <button
              className="button danger"
              type="button"
              onClick={() => void remove(backend.id)}
            >
              Remove it
            </button>
            <button className="button" type="button" onClick={() => setConfirming(false)}>
              Keep
            </button>
          </>
        ) : (
          <button className="button" type="button" onClick={() => setConfirming(true)}>
            Remove
          </button>
        )}
      </div>
    </div>
  );
}

export function Backends() {
  const { backends, view, refresh } = useBackends();
  const [adding, setAdding] = useState(false);

  return (
    <>
      <PageHead
        title="Backends"
        qualifier={backends.length === 0 ? undefined : `${backends.length}`}
        actions={
          <>
            <button className="button" type="button" onClick={() => refresh()}>
              Re-check
            </button>
            <button className="button primary" type="button" onClick={() => setAdding(true)}>
              Add
            </button>
          </>
        }
      />

      {adding && (
        <Section title="New backend">
          <BackendForm onDone={() => setAdding(false)} onCancel={() => setAdding(false)} />
        </Section>
      )}

      <Section title="Configured">
        <List>
          {backends.length === 0 ? (
            <Empty>
              Nothing added yet. Add the address of an <code>orchestrad dashboard</code> and the
              password it was started with.
            </Empty>
          ) : (
            backends.map((backend) => <BackendRow key={backend.id} backend={backend} />)
          )}
        </List>
      </Section>

      {/* Where the passwords are is not a detail: the fallback is a file, and an app that let
          you assume otherwise would be telling you something untrue about your own machine. */}
      {view?.secretStore === "file" && (
        <Notice title="Passwords are in a file, not the keychain">
          <p className="notice-note">
            No system keychain answered — a headless host, or a desktop with no keyring running.
            The passwords are in this app's private data directory instead, readable only by your
            user account.
          </p>
        </Notice>
      )}
    </>
  );
}
