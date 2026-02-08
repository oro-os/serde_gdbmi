pub(crate) trait TakeUnescaped {
    fn take_unescaped(&mut self) -> Result<String, crate::Error>;
}

impl TakeUnescaped for String {
    fn take_unescaped(&mut self) -> Result<String, crate::Error> {
        let r = unescape_zero_copy::unescape(mi_escape_sequence, self)?.to_string();
        self.clear();
        Ok(r)
    }
}

pub fn mi_escape_sequence(s: &str) -> Result<(char, &str), unescape_zero_copy::Error> {
    use unescape_zero_copy::Error;

    let mut chars = s.chars();
    let next = chars.next().ok_or(Error::IncompleteSequence)?;
    match next {
        'a' => Ok(('\x07', chars.as_str())),
        'b' => Ok(('\x08', chars.as_str())),
        'f' => Ok(('\x0C', chars.as_str())),
        'n' => Ok(('\n', chars.as_str())),
        'r' => Ok(('\r', chars.as_str())),
        't' => Ok(('\t', chars.as_str())),
        'v' => Ok(('\x0B', chars.as_str())),
        // Copied this entire thing just for this:
        'e' => Ok(('\x1b', chars.as_str())),
        '\\' | '\'' | '\"' | '/' => Ok((next, chars.as_str())),
        '\r' | '\n' => Ok((next, chars.as_str())),
        'x' => unicode_char(chars.as_str(), 2),
        'u' => {
            let s = chars.as_str();
            if chars.next() == Some('{') {
                let s = chars.as_str();
                let size = chars.by_ref().take_while(|n| *n != '}').count();
                let num = u32::from_str_radix(&s[0..size], 16)?;
                let ch = char::from_u32(num).ok_or(Error::InvalidUnicode(num))?;
                Ok((ch, chars.as_str()))
            } else {
                unicode_char(s, 4)
            }
        }
        'U' => unicode_char(chars.as_str(), 8),
        _ => {
            let count = s.chars().take_while(|n| n.is_digit(8)).count().min(3);
            if count > 0 {
                let num = u32::from_str_radix(&s[0..count], 8)?;
                let ch = char::from_u32(num).ok_or(Error::InvalidUnicode(num))?;
                Ok((ch, &s[count..]))
            } else {
                Err(Error::UnknownSequence(next))
            }
        }
    }
}

#[inline]
fn unicode_char(s: &str, chars: usize) -> Result<(char, &str), unescape_zero_copy::Error> {
    use unescape_zero_copy::Error;
    if s.len() < chars {
        Err(Error::IncompleteUnicode)
    } else {
        let num = u32::from_str_radix(&s[0..chars], 16)?;
        let ch = char::from_u32(num).ok_or(Error::InvalidUnicode(num))?;
        Ok((ch, &s[chars..]))
    }
}
