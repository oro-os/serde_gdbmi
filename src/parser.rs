use std::collections::HashMap;

use crate::lexer::Token;

/// The root response object.
///
/// Note that this does _not_ check for `(gdb)` prompts;
/// perform a simple string comparison before feeding it
/// into this parser.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Response {
    /// The token, if present.
    ///
    /// This is the non-symbol text
    /// prior to the opening symbol, e.g.
    /// the `1234` in:
    ///
    /// ```ignore
    /// 12345^done
    /// ```
    pub token: Option<String>,
    /// The response itself. The body is dependent
    /// upon the symbol.
    pub body: ResponseBody,
}

impl TryFrom<&str> for Response {
    type Error = crate::Error;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        ParseStream {
            token_stream: crate::lexer::lex(value)?.into_iter(),
        }
        .parse()
    }
}

impl Parse for Response {
    fn parse(input: &mut ParseStream) -> Result<Self, crate::Error> {
        Ok(Self {
            token: input.parse()?,
            body: input.parse()?,
        })
    }
}

/// A raw response body. Can either be a data response
/// or a stream response.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResponseBody {
    /// A stream response
    Stream(StreamBody),
    /// A data response
    Data(DataBody),
}

impl Parse for ResponseBody {
    fn parse(input: &mut ParseStream) -> Result<Self, crate::Error> {
        if let Some(body) = input.parse()? {
            return Ok(ResponseBody::Stream(body));
        }

        Ok(ResponseBody::Data(input.parse()?))
    }
}

/// A streaming response body. Holds the symbol and the text data.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StreamBody {
    /// The streaming symbol (where the output is coming from/directed to)
    pub symbol: StreamSymbol,
    /// The text of the body.
    pub text: String,
}

impl Parse for StreamBody {
    fn parse(input: &mut ParseStream) -> Result<Self, crate::Error> {
        let symbol = input.parse()?;

        let mut text = String::new();
        for tkn in &mut input.token_stream {
            tkn.to_string_into(&mut text);
        }

        Ok(StreamBody { symbol, text })
    }
}

/// A stream symbol, indicating where the output is coming from / directed to
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StreamSymbol {
    /// Output targets the CLI console window
    ///
    /// `~`
    Console,
    /// Output is coming from the remote target.
    ///
    /// This is only present when GDB’s event loop is truly asynchronous,
    /// which is currently only the case for remote targets.
    ///
    /// `@`
    TargetOutput,
    /// Output coming from the internal GDB log stream.
    ///
    /// `&`
    InternalLogging,
}

impl Parse for StreamSymbol {
    fn parse(input: &mut ParseStream) -> Result<Self, crate::Error> {
        let Some(next) = input.token_stream.next() else {
            return Err(crate::Error::Eof);
        };

        match next {
            Token::Punct('~') => Ok(StreamSymbol::Console),
            Token::Punct('@') => Ok(StreamSymbol::TargetOutput),
            Token::Punct('&') => Ok(StreamSymbol::InternalLogging),
            other => Err(crate::Error::UnexpectedToken(other)),
        }
    }
}

/// A structured data body response.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataBody {
    /// The response body symbol, indicating which type of
    /// message it is.
    pub symbol: DataSymbol,
    /// The response class. This is the "command" of the message,
    /// indicating which kind of arguments it might have.
    pub class: String,
    /// Any and all arguments that the response provided.
    pub variables: HashMap<String, Value>,
}

impl Parse for DataBody {
    fn parse(input: &mut ParseStream) -> Result<Self, crate::Error> {
        let symbol = input.parse()?;
        let class = input.parse()?;
        let variables = if let Some(Token::Punct(',')) = input.token_stream.next() {
            input.parse()?
        } else {
            HashMap::new()
        };

        Ok(DataBody {
            symbol,
            class,
            variables,
        })
    }
}

/// The data symbol, indicating which type of structured data response this is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataSymbol {
    /// Indicates a result body, typically in response to a command.
    ///
    /// `^`
    Result,
    /// Indicates a change of the execution status of the target.
    ///
    /// Asynchronous, meaning it's not typically sent in response to
    /// a command.
    ///
    /// `*`
    AsyncExec,
    /// Indicates a change in the environment, or some other informational
    /// event.
    ///
    /// Asynchronous, meaning it's not typically sent in response to
    /// a command.
    ///
    /// `=`
    AsyncEnvironment,
    /// Indicates a status update, often when downloading files.
    ///
    /// Not very common to see.
    ///
    /// Asynchronous, meaning it's not typically sent in response to
    /// a command.
    ///
    /// `+`
    AsyncStatus,
}

impl Parse for DataSymbol {
    fn parse(input: &mut ParseStream) -> Result<Self, crate::Error> {
        let Some(next) = input.token_stream.next() else {
            return Err(crate::Error::Eof);
        };

        match next {
            Token::Punct('^') => Ok(DataSymbol::Result),
            Token::Punct('*') => Ok(DataSymbol::AsyncExec),
            Token::Punct('=') => Ok(DataSymbol::AsyncEnvironment),
            Token::Punct('+') => Ok(DataSymbol::AsyncStatus),
            other => Err(crate::Error::UnexpectedToken(other)),
        }
    }
}

/// A generic value from the arguments portion of a data message.
///
/// Does not handle parsing to e.g. integers, booleans, etc.
/// All values are treated as strings.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    String(String),
    List(Vec<Value>),
    Dict(HashMap<String, Value>),
}

impl Parse for Value {
    fn parse(input: &mut ParseStream) -> Result<Self, crate::Error> {
        let Some(tkn) = input.token_stream.next() else {
            return Err(crate::Error::Eof);
        };

        match tkn {
            Token::Text(s) => Ok(Value::String(s)),
            Token::Braced(b) => {
                let mut child_stream = ParseStream {
                    token_stream: b.into_iter(),
                };
                let r = Ok(Value::Dict(child_stream.parse()?));
                if let Some(tkn) = child_stream.token_stream.next() {
                    Err(crate::Error::UnexpectedToken(tkn))
                } else {
                    r
                }
            }
            Token::Bracketed(b) => {
                let mut child_stream = ParseStream {
                    token_stream: b.into_iter(),
                };
                let r = Ok(Value::List(child_stream.parse()?));
                if let Some(tkn) = child_stream.token_stream.next() {
                    Err(crate::Error::UnexpectedToken(tkn))
                } else {
                    r
                }
            }
            other => Err(crate::Error::UnexpectedToken(other)),
        }
    }
}

/// Parser trait for all AST types.
pub trait Parse: Sized {
    fn parse(input: &mut ParseStream) -> Result<Self, crate::Error>;
}

/// A parser stream, usable with [`Parse`] implementations.
pub struct ParseStream {
    token_stream: <Vec<Token> as IntoIterator>::IntoIter,
}

impl ParseStream {
    /// Parses the given return type.
    ///
    /// Equivalent to `T::parse(input)`. Useful to DRY up
    /// parser code.
    #[inline]
    pub fn parse<T: Parse>(&mut self) -> Result<T, crate::Error> {
        T::parse(self)
    }
}

macro_rules! impl_punct {
    ($($name:ident($chr:literal) => {$tt:tt}),* $(,)?) => {
        /// Different token types. Use [`crate::Token!`] to refer to them.
        pub mod token {$(
            #[allow(unused_imports, reason = "seems like a false-positive")]
            use super::*;

            #[doc = concat!("`", stringify!($tt), "` (", stringify!($chr) , ")")]
            #[derive(Clone, Copy, Default, PartialEq, Eq, Debug)]
            pub struct $name;

            impl Parse for $name {
                fn parse(input: &mut ParseStream) -> Result<Self, crate::Error> {
                    let Some(tkn) = input.token_stream.next() else {
                        return Err(crate::Error::Eof);
                    };

                    if let Token::Punct(c) = tkn && c == $chr {
                        return Ok($name);
                    }

                    return Err(crate::Error::UnexpectedToken(tkn.clone()));
                }
            }
        )*}

        /// Resolves a literal punctuation token to its token type.
        ///
        /// ```ignore
        /// Token![,] // serde_gdbmi::parser::token::Comma
        /// Token![@] // serde_gdbmi::parser::token::At
        /// // ...
        /// ```
        #[macro_export]
        macro_rules! Token {
            $(
                [$tt] => {$crate::parser::token::$name};
            )*
        }
    }
}

impl_punct! {
    Comma(',') => {,},
    Chevron('^') => {^},
    Tilde('~') => {~},
    At('@') => {@},
    Ampersand('&') => {&},
    Equal('=') => {=},
    Asterisk('*') => {*},
    Plus('+') => {+},
}

impl Parse for String {
    fn parse(input: &mut ParseStream) -> Result<Self, crate::Error> {
        let Some(s) = input.token_stream.next() else {
            return Err(crate::Error::Eof);
        };

        if let Token::Text(s) = s {
            Ok(s)
        } else {
            Err(crate::Error::UnexpectedToken(s))
        }
    }
}

impl<T: Parse> Parse for Option<T> {
    fn parse(input: &mut ParseStream) -> Result<Self, crate::Error> {
        let mut child = ParseStream {
            token_stream: input.token_stream.clone(),
        };
        if let Ok(v) = T::parse(&mut child) {
            input.token_stream = child.token_stream;
            Ok(Some(v))
        } else {
            Ok(None)
        }
    }
}

impl<T: Parse> Parse for Vec<T> {
    fn parse(input: &mut ParseStream) -> Result<Self, crate::Error> {
        let mut items = Vec::new();

        // TODO(qix-): This isn't great, maybe need to let it be peekable.
        // TODO(qix-): Works for now.
        let mut is_first = true;
        while input.token_stream.clone().next().is_some() {
            if is_first {
                is_first = false;
            } else {
                let _comma: Token![,] = input.parse()?;
            }
            items.push(input.parse()?);
        }

        Ok(items)
    }
}

impl<K: Parse + Eq + std::hash::Hash, T: Parse> Parse for HashMap<K, T> {
    fn parse(input: &mut ParseStream) -> Result<Self, crate::Error> {
        let mut items = HashMap::new();

        // TODO(qix-): This isn't great, maybe need to let it be peekable.
        // TODO(qix-): Works for now.
        let mut is_first = true;
        while input.token_stream.clone().next().is_some() {
            if is_first {
                is_first = false;
            } else {
                let _comma: Token![,] = input.parse()?;
            }

            let key = input.parse()?;
            let _eq: Token![=] = input.parse()?;
            let value = input.parse()?;

            items.insert(key, value);
        }

        Ok(items)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_punct() {
        let mut s = ParseStream {
            token_stream: crate::lexer::lex(",^~@&=*+").unwrap().into_iter(),
        };
        let _t: Token![,] = s.parse().unwrap();
        let _t: Token![^] = s.parse().unwrap();
        let _t: Token![~] = s.parse().unwrap();
        let _t: Token![@] = s.parse().unwrap();
        let _t: Token![&] = s.parse().unwrap();
        let _t: Token![=] = s.parse().unwrap();
        let _t: Token![*] = s.parse().unwrap();
        let _t: Token![+] = s.parse().unwrap();
        assert!(s.token_stream.next().is_none());
    }

    #[test]
    #[should_panic]
    fn parse_punct_invalid() {
        let mut s = ParseStream {
            token_stream: crate::lexer::lex(",^~@&=*+").unwrap().into_iter(),
        };
        let _t: Token![,] = s.parse().unwrap();
        let _t: Token![^] = s.parse().unwrap();
        let _t: Token![~] = s.parse().unwrap();
        let _t: Token![@] = s.parse().unwrap();
        let _t: Token![*] = s.parse().unwrap(); // <- wrong, should be &
    }

    #[test]
    fn parse_string() {
        let mut s = ParseStream {
            token_stream: crate::lexer::lex("foo=bar").unwrap().into_iter(),
        };
        let foo: String = s.parse().unwrap();
        let _t: Token![=] = s.parse().unwrap();
        let bar: String = s.parse().unwrap();

        assert_eq!(foo, "foo");
        assert_eq!(bar, "bar");

        assert!(s.token_stream.next().is_none());
    }

    #[test]
    fn parse_optional() {
        let mut s = ParseStream {
            token_stream: crate::lexer::lex("foo=bar").unwrap().into_iter(),
        };
        let maybe_amp: Option<Token![&]> = s.parse().unwrap();
        let maybe_foo: Option<String> = s.parse().unwrap();
        let maybe_chevron: Option<Token![^]> = s.parse().unwrap();
        let _eq: Token![=] = s.parse().unwrap();
        let bar: String = s.parse().unwrap();
        assert!(s.token_stream.next().is_none());

        assert!(maybe_amp.is_none());
        assert_eq!(maybe_foo.unwrap(), "foo");
        assert_eq!(maybe_chevron, None);
        assert_eq!(bar, "bar");
    }

    #[test]
    fn parse_list() {
        let mut s = ParseStream {
            token_stream: crate::lexer::lex("foo,bar,\"baz\\e\"").unwrap().into_iter(),
        };
        let v: Vec<String> = s.parse().unwrap();
        assert!(s.token_stream.next().is_none());

        assert_eq!(v, vec!["foo", "bar", "baz\x1b"]);
    }

    #[test]
    fn parse_dict() {
        let mut s = ParseStream {
            token_stream: crate::lexer::lex("foo=foo,bar=bar,baz=\"baz\\e\"")
                .unwrap()
                .into_iter(),
        };
        let v: HashMap<String, String> = s.parse().unwrap();
        assert!(s.token_stream.next().is_none());

        assert_eq!(
            v,
            HashMap::from([
                ("foo".into(), "foo".into()),
                ("bar".into(), "bar".into()),
                ("baz".into(), "baz\x1b".into())
            ])
        );
    }

    #[test]
    fn parse_value() {
        let mut s = ParseStream {
            token_stream: crate::lexer::lex("foo=bar,bax={qux=qix},blah=[1,2,3]")
                .unwrap()
                .into_iter(),
        };
        let v: HashMap<String, Value> = s.parse().unwrap();
        assert!(s.token_stream.next().is_none());

        assert_eq!(
            v,
            HashMap::from([
                ("foo".into(), Value::String("bar".into())),
                (
                    "bax".into(),
                    Value::Dict(HashMap::from([("qux".into(), Value::String("qix".into()))]))
                ),
                (
                    "blah".into(),
                    Value::List(vec![
                        Value::String("1".into()),
                        Value::String("2".into()),
                        Value::String("3".into())
                    ])
                )
            ])
        );
    }

    #[test]
    fn parse_full_response_stream() {
        let mut s = ParseStream {
            token_stream: crate::lexer::lex(r#"~"27\t\t\t\e[31m::\e[mcore\e[31m::\e[march\e[31m::\e[masm\e[31m!(\e[m\e[31m\"cli\"\e[m\e[31m);\e[m\n""#).unwrap().into_iter()
        };

        let v: Response = s.parse().unwrap();
        assert!(s.token_stream.next().is_none());

        assert_eq!(v, Response {
            token: None,
            body: ResponseBody::Stream(StreamBody {
                symbol: StreamSymbol::Console,
                text: "27\t\t\t\x1b[31m::\x1b[mcore\x1b[31m::\x1b[march\x1b[31m::\x1b[masm\x1b[31m!(\x1b[m\x1b[31m\"cli\"\x1b[m\x1b[31m);\x1b[m\n".into()
            })
        });
    }

    #[test]
    fn parse_full_response_data() {
        let mut s = ParseStream {
            token_stream: crate::lexer::lex(r#"abcd*stopped,reason="breakpoint-hit",disp="keep",bkptno="1",frame={addr="0xffffffff80000011",func="oro_limine_x86_64::_start",args=["foo","bar"],file="orok-boot-limine/bin/x86_64.rs",fullname="/src/oro-os/kernel/orok-boot-limine/bin/x86_64.rs",line="27",arch="i386:x86-64"},thread-id="1",stopped-threads="all""#).unwrap().into_iter()
        };

        let v: Response = s.parse().unwrap();
        assert!(s.token_stream.next().is_none());

        assert_eq!(
            v,
            Response {
                token: Some("abcd".into()),
                body: ResponseBody::Data(DataBody {
                    symbol: DataSymbol::AsyncExec,
                    class: "stopped".into(),
                    variables: HashMap::from([
                        ("reason".into(), Value::String("breakpoint-hit".into())),
                        ("disp".into(), Value::String("keep".into())),
                        ("bkptno".into(), Value::String("1".into())),
                        (
                            "frame".into(),
                            Value::Dict(HashMap::from([
                                ("addr".into(), Value::String("0xffffffff80000011".into())),
                                (
                                    "func".into(),
                                    Value::String("oro_limine_x86_64::_start".into())
                                ),
                                (
                                    "args".into(),
                                    Value::List(vec![
                                        Value::String("foo".into()),
                                        Value::String("bar".into()),
                                    ])
                                ),
                                (
                                    "file".into(),
                                    Value::String("orok-boot-limine/bin/x86_64.rs".into())
                                ),
                                (
                                    "fullname".into(),
                                    Value::String(
                                        "/src/oro-os/kernel/orok-boot-limine/bin/x86_64.rs".into()
                                    )
                                ),
                                ("line".into(), Value::String("27".into())),
                                ("arch".into(), Value::String("i386:x86-64".into()))
                            ]))
                        ),
                        ("thread-id".into(), Value::String("1".into())),
                        ("stopped-threads".into(), Value::String("all".into())),
                    ])
                })
            }
        );
    }
}
