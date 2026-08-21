//! Where a backend's password lives.
//!
//! One secret per backend, keyed by the backend's id, held in the OS keychain: Keychain on
//! macOS, the Credential Manager on Windows, a Secret Service keyring on Linux. Nothing here
//! ever returns a secret to the front-end — `client.rs` reads one, puts it in an
//! `Authorization` header and drops it, and that is the only path out of this module.
//!
//! There are two platforms with no keychain to speak of: a headless Linux host, where no Secret
//! Service is running, and a phone, where the app's own data directory is already private to
//! it and the keyring crate does not build. Both fall back to a file under the app's local data
//! directory, mode `0600` where the platform has modes. The fallback is not silent: the app
//! reports which store is in use (`secret_store` command) and the Backends screen says so, so
//! "my secrets are in the keychain" is never assumed when it is not true.

use std::fs;
use std::path::{Path, PathBuf};

use crate::error::{Error, Result};

const SERVICE: &str = "orchestra-app";

/// Which store the secrets are actually in. Reported to the front-end verbatim.
#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub enum Store {
    Keychain,
    File,
}

pub struct Secrets {
    /// Where the fallback store writes. Always present; used only when the keychain is not.
    dir: PathBuf,
}

impl Secrets {
    pub fn new(dir: PathBuf) -> Self {
        Secrets { dir }
    }

    /// Which store a write would land in. Determined by trying the keychain rather than by
    /// guessing from the platform: a Linux desktop has one and a Linux server does not, and
    /// the difference is not visible at compile time.
    pub fn store(&self) -> Store {
        // A probe entry, never written: constructing one succeeds even where no keychain
        // daemon is running, so the *read* is what actually answers the question. `NoEntry`
        // means the store works and is empty, which is the answer we want.
        #[cfg(has_keychain)]
        let store =
            match keyring::Entry::new(SERVICE, "__probe__").and_then(|e| match e.get_password() {
                Ok(_) | Err(keyring::Error::NoEntry) => Ok(()),
                Err(e) => Err(e),
            }) {
                Ok(()) => Store::Keychain,
                Err(_) => Store::File,
            };
        #[cfg(not(has_keychain))]
        let store = Store::File;
        store
    }

    pub fn set(&self, id: &str, secret: &str) -> Result<()> {
        #[cfg(has_keychain)]
        if self.store() == Store::Keychain {
            return keyring::Entry::new(SERVICE, id)
                .and_then(|e| e.set_password(secret))
                .map_err(|e| Error::Storage(format!("could not write to the keychain: {e}")));
        }
        self.file_set(id, secret)
    }

    pub fn get(&self, id: &str) -> Result<Option<String>> {
        #[cfg(has_keychain)]
        if self.store() == Store::Keychain {
            return match keyring::Entry::new(SERVICE, id).and_then(|e| e.get_password()) {
                Ok(s) => Ok(Some(s)),
                Err(keyring::Error::NoEntry) => Ok(None),
                Err(e) => Err(Error::Storage(format!("could not read the keychain: {e}"))),
            };
        }
        self.file_get(id)
    }

    /// Removing a backend removes its secret. A missing one is not an error: the registry is
    /// the record of what exists, and a secret without a backend is exactly what this clears.
    pub fn delete(&self, id: &str) -> Result<()> {
        #[cfg(has_keychain)]
        if self.store() == Store::Keychain {
            return match keyring::Entry::new(SERVICE, id).and_then(|e| e.delete_credential()) {
                Ok(()) | Err(keyring::Error::NoEntry) => Ok(()),
                Err(e) => Err(Error::Storage(format!("could not clear the keychain: {e}"))),
            };
        }
        self.file_delete(id)
    }

    fn path(&self, id: &str) -> PathBuf {
        // Ids are generated here (`registry::new_id`) and are hex, so they cannot escape the
        // directory. The check is kept anyway: the day an id comes from somewhere else, this
        // is the line that has to still be true.
        let safe: String = id
            .chars()
            .filter(|c| c.is_ascii_alphanumeric() || *c == '-')
            .collect();
        self.dir.join("secrets").join(format!("{safe}.secret"))
    }

    fn file_set(&self, id: &str, secret: &str) -> Result<()> {
        let path = self.path(id);
        let parent = path.parent().expect("a secret path has a parent");
        fs::create_dir_all(parent)
            .map_err(|e| Error::Storage(format!("could not create {}: {e}", parent.display())))?;
        restrict(parent)?;
        // Write-and-rename, as everything else in orchestra writes: a reader never sees half a
        // file, and a crash mid-write leaves the old secret rather than an empty one.
        let tmp = path.with_extension("secret.tmp");
        fs::write(&tmp, secret)
            .map_err(|e| Error::Storage(format!("could not write {}: {e}", tmp.display())))?;
        restrict(&tmp)?;
        fs::rename(&tmp, &path)
            .map_err(|e| Error::Storage(format!("could not replace {}: {e}", path.display())))
    }

    fn file_get(&self, id: &str) -> Result<Option<String>> {
        match fs::read_to_string(self.path(id)) {
            Ok(s) => Ok(Some(s)),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
            Err(e) => Err(Error::Storage(format!("could not read the secret: {e}"))),
        }
    }

    fn file_delete(&self, id: &str) -> Result<()> {
        match fs::remove_file(self.path(id)) {
            Ok(()) => Ok(()),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(e) => Err(Error::Storage(format!("could not remove the secret: {e}"))),
        }
    }
}

/// Owner-only permissions, where the platform has them. On Windows the file inherits the
/// user profile's ACL, which is the same guarantee by a different mechanism.
fn restrict(path: &Path) -> Result<()> {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mode = if path.is_dir() { 0o700 } else { 0o600 };
        fs::set_permissions(path, fs::Permissions::from_mode(mode))
            .map_err(|e| Error::Storage(format!("could not restrict {}: {e}", path.display())))?;
    }
    #[cfg(not(unix))]
    let _ = path;
    Ok(())
}
