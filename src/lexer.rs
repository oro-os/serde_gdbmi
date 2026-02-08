//! Lexer implementation for MI messages.
//!
//! Follows a similar design approach to `syn`.

use std::iter::Peekable;

use crate::{Error, unescape::TakeUnescaped};

/// A single token in the MI lexicon.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    /// `foo`, `"foo"`, `1234`, etc.
    Text(String),
    /// `+`, `^`, `,`, etc. - though notably,
    /// _not_ `{}` nor `[]`.
    Punct(char),
    /// Tokens within a `{...}` pair.
    Braced(TokenStream),
    /// Tokens within a `[...]` pair.
    Bracketed(TokenStream),
}

impl Token {
    /// Whether or not this is a `Token::Text`
    pub const fn is_text(&self) -> bool {
        matches!(self, Token::Text(_))
    }

    /// Whether or not this is a `Token::Punct`
    pub const fn is_punct(&self) -> bool {
        matches!(self, Token::Punct(_))
    }

    /// Whether or not this is a `Token::Braced`
    pub const fn is_braced(&self) -> bool {
        matches!(self, Token::Braced(_))
    }

    /// Whether or not this is a `Token::Bracketed`
    pub const fn is_bracketed(&self) -> bool {
        matches!(self, Token::Bracketed(_))
    }

    /// String-izes this token into the given [`String`].
    pub fn to_string_into(&self, s: &mut String) {
        match self {
            Token::Text(t) => s.push_str(t),
            Token::Punct(c) => s.push(*c),
            Token::Braced(b) => {
                s.push('{');
                for t in b.iter() {
                    t.to_string_into(s);
                }
                s.push('}');
            }
            Token::Bracketed(b) => {
                s.push('[');
                for t in b.iter() {
                    t.to_string_into(s);
                }
                s.push(']');
            }
        }
    }
}

/// A stream of tokens from a line of MI output.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct TokenStream(Vec<Token>);

impl TokenStream {
    /// Creates an empty `TokenStream`
    #[inline]
    pub const fn new() -> Self {
        Self(Vec::new())
    }

    /// Creates an empty `TokenStream` with the given capacity
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self(Vec::with_capacity(capacity))
    }

    /// Extends this stream with another stream's tokens.
    #[inline]
    pub fn extend(&mut self, other: impl IntoIterator<Item = Token>) {
        self.0.extend(other);
    }

    /// Returns an iterator over the contents of this token stream.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &Token> {
        self.0.iter()
    }
}

impl IntoIterator for TokenStream {
    type Item = Token;
    type IntoIter = <Vec<Token> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// Lexes the given string into a [`TokenStream`]
///
/// Returns an error if it encounters an unsupported character.
///
/// Should not contain any newlines, etc. This parses a single
/// line at a time.
pub fn lex(line: &str) -> Result<TokenStream, Error> {
    let mut iter = line.char_indices().enumerate().peekable();

    let ts = lex_inner(&mut iter, None)?;

    if let Some((char_offset, (byte_offset, chr))) = iter.next() {
        return Err(Error::UnexpectedCharacter {
            chr,
            char_offset,
            byte_offset,
        });
    }

    Ok(ts)
}

fn lex_inner(
    iter: &mut Peekable<impl Iterator<Item = (usize, (usize, char))>>,
    mut end_at: Option<char>,
) -> Result<TokenStream, Error> {
    let mut tokens = Vec::new();

    let mut strval = String::new();
    let mut in_str = false;

    while let Some((char_offset, (byte_offset, chr))) = iter.next() {
        // A bit of a special case.
        if !in_str && !chr.is_text_char() && !strval.is_empty() {
            tokens.push(Token::Text(strval.take_unescaped()?));
        }

        match chr {
            '\\' if in_str => {
                let Some((_, (_, chr))) = iter.next() else {
                    return Err(Error::Eof);
                };

                strval.push('\\');
                strval.push(chr);
            }
            '"' if in_str => {
                in_str = false;
                tokens.push(Token::Text(strval.take_unescaped()?));
            }
            '"' if !in_str => {
                in_str = true;
                debug_assert!(strval.is_empty());
            }
            c if in_str => {
                strval.push(c);
            }
            c if Some(c) == end_at => {
                // Makes the check later not error on EOF.
                end_at = None;
                break;
            }
            '{' => {
                tokens.push(Token::Braced(lex_inner(iter, Some('}'))?));
            }
            '[' => {
                tokens.push(Token::Bracketed(lex_inner(iter, Some(']'))?));
            }
            '}' | ']' => {
                return Err(Error::UnexpectedCharacter {
                    chr,
                    char_offset,
                    byte_offset,
                });
            }
            '^' | '~' | '@' | '&' | ',' | '=' | '*' | '+' => {
                tokens.push(Token::Punct(chr));
            }
            c if c.is_text_char() => {
                strval.push(c);
            }
            _ => {
                return Err(Error::UnexpectedCharacter {
                    chr,
                    char_offset,
                    byte_offset,
                });
            }
        }
    }

    if in_str || end_at.is_some() {
        return Err(Error::Eof);
    }

    if !strval.is_empty() {
        tokens.push(Token::Text(strval.take_unescaped()?));
    }

    Ok(TokenStream(tokens))
}

trait MiChar: Copy {
    fn is_text_char(self) -> bool;
}

impl MiChar for char {
    fn is_text_char(self) -> bool {
        self.is_alphanumeric() || matches!(self, '_' | '-')
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[test]
    fn lex_empty() {
        assert_eq!(lex("").unwrap(), TokenStream::new());
    }

    #[test]
    fn lex_done() {
        assert_eq!(
            lex("^done").unwrap(),
            TokenStream(vec![Punct('^'), Text("done".into())])
        );
    }

    #[test]
    fn lex_unexpected() {
        assert!(matches!(
            lex("foo}"),
            Err(Error::UnexpectedCharacter {
                chr: '}',
                byte_offset: 3,
                char_offset: 3
            })
        ));
    }

    #[test]
    fn lex_quoted() {
        assert_eq!(
            lex("foo=\"bar baz boo \\\\ }] [{}] \\\" qix\"").unwrap(),
            TokenStream(vec![
                Text("foo".into()),
                Punct('='),
                Text("bar baz boo \\ }] [{}] \" qix".into())
            ])
        );
    }

    #[test]
    fn lex_eof_str() {
        assert!(matches!(lex("foo=\"foobar"), Err(Error::Eof)));
    }

    #[test]
    fn lex_braced() {
        assert_eq!(
            lex("foo={bar=baz}").unwrap(),
            TokenStream(vec![
                Text("foo".into()),
                Punct('='),
                Braced(TokenStream(vec![
                    Text("bar".into()),
                    Punct('='),
                    Text("baz".into())
                ]))
            ])
        );
    }

    #[test]
    fn lex_braced_nested() {
        assert_eq!(
            lex("foo={bar={blah=1234,foo=10},qux=\"qix\"}").unwrap(),
            TokenStream(vec![
                Text("foo".into()),
                Punct('='),
                Braced(TokenStream(vec![
                    Text("bar".into()),
                    Punct('='),
                    Braced(TokenStream(vec![
                        Text("blah".into()),
                        Punct('='),
                        Text("1234".into()),
                        Punct(','),
                        Text("foo".into()),
                        Punct('='),
                        Text("10".into()),
                    ])),
                    Punct(','),
                    Text("qux".into()),
                    Punct('='),
                    Text("qix".into()),
                ]))
            ])
        );
    }

    #[test]
    fn lex_bracketed() {
        assert_eq!(
            lex("foo=[bar,baz]").unwrap(),
            TokenStream(vec![
                Text("foo".into()),
                Punct('='),
                Bracketed(TokenStream(vec![
                    Text("bar".into()),
                    Punct(','),
                    Text("baz".into())
                ]))
            ])
        );
    }

    #[test]
    fn lex_deeply_wrapped() {
        assert_eq!(
            lex("{{[{[[{oro}]]}]}}").unwrap(),
            // Sorry for the formatting :|
            TokenStream(vec![Braced(TokenStream(vec![Braced(TokenStream(vec![
                Bracketed(TokenStream(vec![Braced(TokenStream(vec![Bracketed(
                    TokenStream(vec![Bracketed(TokenStream(vec![Braced(TokenStream(
                        vec![Text("oro".into())]
                    ))]))])
                )]))]))
            ]))]))])
        );
    }

    #[test]
    fn lex_eof_bracketed() {
        assert!(matches!(lex("[[{foo=bar}]"), Err(Error::Eof)));
    }

    #[test]
    fn lex_eof_braced() {
        assert!(matches!(lex("{{foo=bar}"), Err(Error::Eof)));
    }

    #[test]
    fn lex_mismatch() {
        assert!(matches!(
            lex("{foo=[bar}]"),
            Err(Error::UnexpectedCharacter {
                chr: '}',
                byte_offset: 9,
                char_offset: 9
            })
        ));
    }

    #[test]
    fn lex_multibyte_string() {
        assert_eq!(
            lex("os=\"オロオペレーティング·システム\"").unwrap(),
            TokenStream(vec![
                Text("os".into()),
                Punct('='),
                Text("オロオペレーティング·システム".into()),
            ])
        );
    }

    #[test]
    fn lex_multibyte_char_err() {
        static STR: &str = "os=\"オロオペレーティング·システム\"},foo=bar";
        let bad_byte_offset = STR
            .bytes()
            .enumerate()
            .find(|(_, b)| *b == b'}')
            .map(|(off, _)| off)
            .unwrap();
        let bad_char_offset = STR
            .chars()
            .enumerate()
            .find(|(_, c)| *c == '}')
            .map(|(off, _)| off)
            .unwrap();

        // We know this won't be true, so if it is, something's wrong with the test.
        assert_ne!(bad_byte_offset, bad_char_offset);
        assert_ne!(bad_byte_offset, 0);
        assert_ne!(bad_char_offset, 0);

        let Err(Error::UnexpectedCharacter {
            chr: '}',
            byte_offset,
            char_offset,
        }) = lex(STR)
        else {
            panic!();
        };

        assert_eq!(byte_offset, bad_byte_offset);
        assert_eq!(char_offset, bad_char_offset);
    }

    #[test]
    fn lex_unescape() {
        assert_eq!(
            lex(r#""\t\nfoo\u001b\u{001b}""#).unwrap(),
            TokenStream(vec![Text("\t\nfoo\u{001b}\u{001b}".to_string())])
        );
    }

    #[test]
    fn lex_realworld() {
        // Just some real-world strings I've encountered
        // (namely that failed a cruddy AI-generated parser
        // I tried to use in the Oro kernel TUI, which was
        // so incredibly terrible I detoured and wrote this
        // library by hand to replace it, couldn't parse).
        lex(r#"*stopped,reason="signal-received",signal-name="SIGINT",signal-meaning="Interrupt",frame={addr="0x00000000000fd0b1",func="??",args=[],arch="i386:x86-64"},thread-id="4",stopped-threads="all""#).unwrap();
        lex(r#"=breakpoint-created,bkpt={number="1",type="breakpoint",disp="keep",enabled="y",addr="0xffffffff80000011",func="oro_limine_x86_64::_start",file="orok-boot-limine/bin/x86_64.rs",fullname="/src/oro-os/kernel/orok-boot-limine/bin/x86_64.rs",line="27",thread-groups=["i1"],times="0",original-location="_start"}"#).unwrap();
        lex(r#"~"27\t\t\t\e[31m::\e[mcore\e[31m::\e[march\e[31m::\e[masm\e[31m!(\e[m\e[31m\"cli\"\e[m\e[31m);\e[m\n""#).unwrap();
    }
}
