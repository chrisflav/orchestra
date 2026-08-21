//! The list of backends, and which one is selected.
//!
//! One JSON file in the app's config directory, holding no secret — a backend is a name, an
//! origin and a colour, and the password that reaches it lives in `secrets.rs`. That split is
//! what makes the file copyable between machines and readable when something is wrong.
//!
//! Every write is a write-and-rename, so a reader never sees a partial file and a crash
//! mid-write leaves the previous list rather than an empty one. This is the same discipline the
//! daemon writes its own state with.

use std::fs;
use std::path::PathBuf;
use std::sync::Mutex;

use serde::{Deserialize, Serialize};

use crate::error::{Error, Result};

/// One `orchestrad dashboard` instance.
///
/// `id` is generated here and is the only thing the front-end ever names a backend by: it is
/// the key into the secret store, and it is stable across a rename or a move to a new address.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Backend {
    pub id: String,
    pub name: String,
    /// Origin, without a trailing slash: `https://orchestra.example.com` or
    /// `http://127.0.0.1:8080`. Normalised by `normalise_url` before it is stored.
    pub url: String,
    /// One of the four section hues the dashboard's palette defines — `brass`, `strings`,
    /// `winds`, `perc` — used to tell backends apart at a glance in the switcher.
    #[serde(default = "default_color")]
    pub color: String,
    /// Skip TLS verification for this backend. Off by default, and refused for anything but a
    /// loopback or private-range host (see `is_private_host`).
    #[serde(default)]
    pub allow_insecure_tls: bool,
    /// Whether this backend's events are worth an OS notification.
    #[serde(default)]
    pub notify: bool,
    pub added_at: String,
}

fn default_color() -> String {
    "strings".to_string()
}

/// The file on disk. `selected` names a backend by id; a dangling id is treated as none, which
/// is what makes removing the selected backend a one-line operation.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Registry {
    #[serde(default)]
    pub backends: Vec<Backend>,
    #[serde(default)]
    pub selected: Option<String>,
}

pub struct Store {
    path: PathBuf,
    state: Mutex<Registry>,
}

impl Store {
    /// Load the registry, or start an empty one.
    ///
    /// A file that cannot be parsed is *not* an error and is *not* overwritten: it is left
    /// alone and reported as an empty list, because silently replacing a list of backends
    /// someone hand-edited is worse than showing none.
    pub fn load(path: PathBuf) -> Self {
        let state = fs::read_to_string(&path)
            .ok()
            .and_then(|s| serde_json::from_str::<Registry>(&s).ok())
            .unwrap_or_default();
        Store {
            path,
            state: Mutex::new(state),
        }
    }

    pub fn snapshot(&self) -> Registry {
        self.state.lock().expect("registry lock").clone()
    }

    pub fn get(&self, id: &str) -> Result<Backend> {
        self.state
            .lock()
            .expect("registry lock")
            .backends
            .iter()
            .find(|b| b.id == id)
            .cloned()
            .ok_or_else(|| Error::NoSuchBackend(format!("no backend with id {id}")))
    }

    /// Apply a change under the lock and persist the result.
    ///
    /// Persisting inside the lock is deliberate: two writes racing would otherwise be able to
    /// land on disk in the opposite order to the one they took the lock in, and the file would
    /// disagree with memory.
    pub fn update<T>(&self, f: impl FnOnce(&mut Registry) -> Result<T>) -> Result<T> {
        let mut guard = self.state.lock().expect("registry lock");
        let mut draft = guard.clone();
        let out = f(&mut draft)?;
        write(&self.path, &draft)?;
        *guard = draft;
        Ok(out)
    }
}

fn write(path: &PathBuf, registry: &Registry) -> Result<()> {
    let parent = path
        .parent()
        .ok_or_else(|| Error::Storage("the registry path has no directory".into()))?;
    fs::create_dir_all(parent)
        .map_err(|e| Error::Storage(format!("could not create {}: {e}", parent.display())))?;
    let body = serde_json::to_string_pretty(registry)
        .map_err(|e| Error::Storage(format!("could not serialise the registry: {e}")))?;
    let tmp = path.with_extension("json.tmp");
    fs::write(&tmp, body)
        .map_err(|e| Error::Storage(format!("could not write {}: {e}", tmp.display())))?;
    fs::rename(&tmp, path)
        .map_err(|e| Error::Storage(format!("could not replace {}: {e}", path.display())))
}

/// A short random hex id. Not a UUID: it is a key into two local files, and eight bytes of
/// randomness is already far more than a list of a dozen backends can collide in.
pub fn new_id() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    // Mixed with the address of a fresh allocation, which ASLR makes unpredictable across
    // runs — enough for a local key, and it pulls in no dependency to get it.
    let boxed = Box::new(0u8);
    let addr = (&*boxed as *const u8) as usize as u128;
    format!("{:016x}", (nanos ^ (addr << 32) ^ addr) as u64)
}

/// Normalise what someone typed into an origin.
///
/// `example.com` becomes `https://example.com`, a trailing slash is dropped, and a path,
/// query or fragment is refused rather than quietly ignored: `https://host/orchestra/` looks
/// like it should work, and a client that silently discarded the path would answer every page
/// with "unreachable" and never say why.
pub fn normalise_url(input: &str) -> Result<String> {
    let trimmed = input.trim().trim_end_matches('/');
    if trimmed.is_empty() {
        return Err(Error::BadRequest("give the backend an address".into()));
    }
    let with_scheme = if trimmed.contains("://") {
        trimmed.to_string()
    } else {
        format!("https://{trimmed}")
    };
    let parsed = url::Url::parse(&with_scheme)
        .map_err(|e| Error::BadRequest(format!("{with_scheme} is not a URL: {e}")))?;
    match parsed.scheme() {
        "http" | "https" => {}
        other => {
            return Err(Error::BadRequest(format!(
                "{other} is not a scheme this can speak; use http or https"
            )))
        }
    }
    if parsed.host_str().is_none() {
        return Err(Error::BadRequest("that address names no host".into()));
    }
    if parsed.path() != "/" && !parsed.path().is_empty() {
        return Err(Error::BadRequest(
            "give the origin only — the API paths are added by the app".into(),
        ));
    }
    if parsed.query().is_some() || parsed.fragment().is_some() {
        return Err(Error::BadRequest(
            "give the origin only — no query and no fragment".into(),
        ));
    }
    Ok(with_scheme.trim_end_matches('/').to_string())
}

/// Whether a host is one where skipping TLS verification is a defensible local choice: a
/// loopback address, a private IPv4 range, a link-local address, or a `.local` name.
///
/// Anything else — a public name, a routable address — is refused, because "trust any
/// certificate" on the open internet is not a preference, it is the absence of TLS.
pub fn is_private_host(url: &str) -> bool {
    let Ok(parsed) = url::Url::parse(url) else {
        return false;
    };
    let Some(host) = parsed.host_str() else {
        return false;
    };
    if host == "localhost" || host.ends_with(".localhost") || host.ends_with(".local") {
        return true;
    }
    match host.parse::<std::net::IpAddr>() {
        Ok(std::net::IpAddr::V4(v4)) => v4.is_loopback() || v4.is_private() || v4.is_link_local(),
        Ok(std::net::IpAddr::V6(v6)) => {
            // `is_unique_local`/`is_unicast_link_local` are still unstable; match the two
            // prefixes directly rather than wait for them.
            v6.is_loopback()
                || (v6.segments()[0] & 0xfe00) == 0xfc00
                || (v6.segments()[0] & 0xffc0) == 0xfe80
        }
        Err(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_bare_host_gets_https() {
        assert_eq!(
            normalise_url("orchestra.example.com").unwrap(),
            "https://orchestra.example.com"
        );
    }

    #[test]
    fn a_trailing_slash_goes() {
        assert_eq!(
            normalise_url("http://127.0.0.1:8080/").unwrap(),
            "http://127.0.0.1:8080"
        );
    }

    #[test]
    fn a_path_is_refused_rather_than_dropped() {
        assert!(normalise_url("https://host/orchestra").is_err());
        assert!(normalise_url("https://host/?x=1").is_err());
    }

    #[test]
    fn only_http_and_https() {
        assert!(normalise_url("ftp://host").is_err());
        assert!(normalise_url("").is_err());
    }

    #[test]
    fn private_hosts_are_the_only_ones_that_may_skip_tls() {
        assert!(is_private_host("https://127.0.0.1:8080"));
        assert!(is_private_host("https://192.168.1.9"));
        assert!(is_private_host("https://box.local"));
        assert!(is_private_host("https://localhost"));
        assert!(!is_private_host("https://orchestra.example.com"));
        assert!(!is_private_host("https://8.8.8.8"));
    }

    #[test]
    fn ids_differ() {
        assert_ne!(new_id(), new_id());
    }
}
