#![doc = include_str!("../README.md")]

pub mod de;
mod error;
pub mod lexer;
pub mod parser;
pub(crate) mod unescape;

pub use self::error::Error;

use serde::de::DeserializeOwned;

/// A full response body from the parser.
///
/// Contains an optional token (if one was sent),
/// as well as the response itself.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Response<T: DeserializeOwned, Tkn: DeserializeOwned = u64> {
    pub token: Option<Tkn>,
    pub body: ResponseBody<T>,
}

/// A response body, with the user-specific deserialized response type.
///
/// This wraps the deserialized output in the type of
/// message (result, notify, etc.).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResponseBody<T: DeserializeOwned> {
    /// A normal result record, typically in response
    /// to a command sent to MI.
    Result(T),
    /// Asynchronous execution state change.
    AsyncExec(T),
    /// Asynchronous environment state change.
    AsyncEnvironment(T),
    /// Asynchronous progress state change.
    ///
    /// **Note:** This one is quite rare, typically seen when
    /// downloading targets from a remote URL.
    AsyncStatus(T),
    /// A stream response, targeting the CLI console window.
    Console(String),
    /// A stream response, output from the running target.
    ///
    /// This is only present when GDB’s event loop is truly asynchronous,
    /// which is currently only the case for remote targets.
    TargetOutput(String),
    /// Log output stream, produced by GDB's logging internals.
    InternalLogging(String),
    /// The `(gdb)` prompt. Generally useless except as used as a
    /// stream marker of some sort.
    Prompt,
}

impl<T: DeserializeOwned> ResponseBody<T> {
    /// Distill the response body type down to either structured data
    /// or stream data.
    ///
    /// If the `ResponseBody` variants are too granular, you can
    /// call this method to quickly reduce the types of data down
    /// one of the two types of data.
    ///
    /// To get an owning reference to this data, call
    /// [`ResponseBody::into_simple()`].
    ///
    /// Returns `None` if the line was [`ResponseBody::Prompt`].
    pub fn simple(&self) -> Option<SimpleResponseBodyRef<'_, T>> {
        match self {
            Self::Result(v)
            | Self::AsyncExec(v)
            | Self::AsyncEnvironment(v)
            | Self::AsyncStatus(v) => Some(SimpleResponseBodyRef::Data(v)),
            Self::Console(s) | Self::TargetOutput(s) | Self::InternalLogging(s) => {
                Some(SimpleResponseBodyRef::Stream(s))
            }
            Self::Prompt => None,
        }
    }

    /// Owning version of [`ResponseBody::simple()`].
    ///
    /// Returns `None` if the line was [`ResponseBody::Prompt`].
    pub fn into_simple(self) -> Option<SimpleResponseBody<T>> {
        match self {
            Self::Result(v)
            | Self::AsyncExec(v)
            | Self::AsyncEnvironment(v)
            | Self::AsyncStatus(v) => Some(SimpleResponseBody::Data(v)),
            Self::Console(s) | Self::TargetOutput(s) | Self::InternalLogging(s) => {
                Some(SimpleResponseBody::Stream(s))
            }
            Self::Prompt => None,
        }
    }
}

/// If the [`ResponseBody`] enum is too granular, you can [`ResponseBody::simple()`]-ify
/// it, which results in either a [`SimpleResponseBody::Stream`] or [`SimpleResponseBody::Data`]
/// variant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimpleResponseBodyRef<'a, T: DeserializeOwned> {
    /// The message contains data, deserialized to the user-defined message type.
    Data(&'a T),
    /// The message contains stream data.
    Stream(&'a String),
}

/// If the [`ResponseBody`] enum is too granular, you can [`ResponseBody::simple()`]-ify
/// it, which results in either a [`SimpleResponseBody::Stream`] or [`SimpleResponseBody::Data`]
/// variant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SimpleResponseBody<T: DeserializeOwned> {
    /// The message contains data, deserialized to the user-defined message type.
    Data(T),
    /// The message contains stream data.
    Stream(String),
}

/// Returns a [`Response`] given a line string and a user-defined parsable
/// MI type.
pub fn from_str<T: DeserializeOwned>(line: &str) -> Result<Response<T>, Error> {
    from_str_with_token(line)
}

fn from_str_with_token<T: DeserializeOwned, Tkn: DeserializeOwned>(
    line: &str,
) -> Result<Response<T, Tkn>, Error> {
    // Check for (gdb) prompt
    if line.trim() == "(gdb)" {
        return Ok(Response {
            token: None,
            body: ResponseBody::Prompt,
        });
    }

    // Parse the line into the AST
    let parsed = parser::Response::try_from(line)?;

    // Deserialize the token
    let token = if let Some(ref token_str) = parsed.token {
        let deserializer = de::TokenDeserializer::from_str(token_str);
        Some(Tkn::deserialize(deserializer)?)
    } else {
        None
    };

    // Deserialize the body
    let body = match &parsed.body {
        parser::ResponseBody::Stream(stream) => {
            use parser::StreamSymbol;
            match stream.symbol {
                StreamSymbol::Console => ResponseBody::Console(stream.text.clone()),
                StreamSymbol::TargetOutput => ResponseBody::TargetOutput(stream.text.clone()),
                StreamSymbol::InternalLogging => ResponseBody::InternalLogging(stream.text.clone()),
            }
        }
        parser::ResponseBody::Data(data) => {
            let data_de = de::DataDeserializer::from_data(&data.class, &data.variables);
            let inner = T::deserialize(data_de)?;

            use parser::DataSymbol;
            match data.symbol {
                DataSymbol::Result => ResponseBody::Result(inner),
                DataSymbol::AsyncExec => ResponseBody::AsyncExec(inner),
                DataSymbol::AsyncEnvironment => ResponseBody::AsyncEnvironment(inner),
                DataSymbol::AsyncStatus => ResponseBody::AsyncStatus(inner),
            }
        }
    };

    Ok(Response { token, body })
}
