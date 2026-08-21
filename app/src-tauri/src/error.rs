//! The one error type every command answers with.
//!
//! It serialises as `{ "kind": …, "message": … }` rather than as a string, because the
//! front-end branches on two of these and shows the rest: `unauthorized` puts one backend into
//! its needs-credential state, `unreachable` marks it offline in the switcher, and everything
//! else is text for the user. A bare string would make that a matter of parsing prose.
//!
//! No variant ever carries a secret. `Http` carries the status and whatever the server put in
//! the body's `error` field, both of which are the server's own words.

use serde::ser::{Serialize, SerializeStruct, Serializer};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// No backend with that id, or it has no stored secret.
    #[error("{0}")]
    NoSuchBackend(String),

    /// The path or URL a caller gave could not be used.
    #[error("{0}")]
    BadRequest(String),

    /// The backend answered 401: the stored secret is wrong, or was revoked.
    #[error("the backend rejected the credential")]
    Unauthorized,

    /// The backend answered, but not with success.
    #[error("{message}")]
    Http { status: u16, message: String },

    /// The backend could not be reached at all: DNS, TCP, TLS, timeout.
    #[error("{0}")]
    Unreachable(String),

    /// The registry file or the secret store could not be read or written.
    #[error("{0}")]
    Storage(String),
}

impl Error {
    fn kind(&self) -> &'static str {
        match self {
            Error::NoSuchBackend(_) => "noSuchBackend",
            Error::BadRequest(_) => "badRequest",
            Error::Unauthorized => "unauthorized",
            Error::Http { .. } => "http",
            Error::Unreachable(_) => "unreachable",
            Error::Storage(_) => "storage",
        }
    }

    fn status(&self) -> Option<u16> {
        match self {
            Error::Unauthorized => Some(401),
            Error::Http { status, .. } => Some(*status),
            _ => None,
        }
    }
}

impl Serialize for Error {
    fn serialize<S: Serializer>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error> {
        let mut s = serializer.serialize_struct("Error", 3)?;
        s.serialize_field("kind", self.kind())?;
        s.serialize_field("message", &self.to_string())?;
        s.serialize_field("status", &self.status())?;
        s.end()
    }
}

/// A `reqwest` failure is always a failure to *reach*: anything the server answered, however
/// unhappily, comes back as a response and is classified from its status instead.
impl From<reqwest::Error> for Error {
    fn from(e: reqwest::Error) -> Self {
        Error::Unreachable(describe_transport(&e))
    }
}

/// Say which of the four ways a request can fail before it is answered actually happened.
/// "error sending request for url (…)" names the URL and not the cause, which is the one thing
/// the person adding a backend already knows.
pub fn describe_transport(e: &reqwest::Error) -> String {
    if e.is_timeout() {
        "timed out".to_string()
    } else if e.is_connect() {
        // Walk the source chain: the useful sentence ("certificate verify failed", "connection
        // refused") is at the bottom of it, not at the top.
        let mut cause: &dyn std::error::Error = e;
        let mut deepest = e.to_string();
        while let Some(next) = cause.source() {
            deepest = next.to_string();
            cause = next;
        }
        format!("could not connect: {deepest}")
    } else if e.is_request() {
        format!("bad request: {e}")
    } else {
        e.to_string()
    }
}

pub type Result<T> = std::result::Result<T, Error>;
