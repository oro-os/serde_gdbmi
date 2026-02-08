/// The error type returned by the various parts of the `serde_gdbmi` library.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(
        "unexpected character while lexing: '{chr}' (char offset {char_offset}, byte offset {byte_offset})"
    )]
    UnexpectedCharacter {
        chr: char,
        char_offset: usize,
        byte_offset: usize,
    },
    #[error("unexpected end of line")]
    Eof,
    #[error("invalid escape sequence: {0}")]
    Escape(#[from] unescape_zero_copy::Error),
    #[error("unexpected token: {0:?}")]
    UnexpectedToken(crate::lexer::Token),
    #[error("deserialization error: {0}")]
    Custom(String),
    #[error("invalid type: expected {expected}, got {got}")]
    InvalidType { expected: String, got: String },
    #[error("missing field: {0}")]
    MissingField(String),
    #[error("unknown variant: {0}")]
    UnknownVariant(String),
    #[error("invalid value: {0}")]
    InvalidValue(String),
}

impl serde::de::Error for Error {
    fn custom<T: std::fmt::Display>(msg: T) -> Self {
        Error::Custom(msg.to_string())
    }
}
