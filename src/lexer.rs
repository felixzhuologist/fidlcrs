use std::fmt;
use std::str::Chars;

use self::Error::*;
use crate::span::{spanned, Location, Spanned};
use crate::token::Token;

struct CharLocations<'input> {
    location: Location,
    chars: Chars<'input>,
}

impl<'input> CharLocations<'input> {
    pub fn new(input: &'input str) -> CharLocations<'input> {
        CharLocations {
            location: Location {
                line: 0,
                col: 0,
                absolute: 0,
            },
            chars: input.chars(),
        }
    }
}

impl<'input> Iterator for CharLocations<'input> {
    type Item = (Location, char);

    fn next(&mut self) -> Option<(Location, char)> {
        self.chars.next().map(|ch| {
            let location = self.location;
            self.location = location.shift(ch);
            (location, ch)
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    UnexpectedChar(char),
    UnexpectedEof,
    UnterminatedStringLiteral,
    UnexpectedEscapeCode(char),
    NonParseableInt,
    // TODO split this error up
    BinaryLiteralError,
    // TODO split this error up
    HexLiteralError,
    NumericLiteralOverflow,
    NumericLiteralUnderflow,
    NumericLiteralWrongPrefix,
    NumericLiteralIncomplete,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::UnexpectedChar(ch) => write!(f, "unexpected character: {}", ch),
            _ => write!(f, "todo"),
        }
    }
}

fn error<T>(location: Location, code: Error) -> Result<T, SpannedError> {
    Err(spanned(location, location, code))
}

fn is_ident_start(ch: char) -> bool {
    match ch {
        'a'..='z' | 'A'..='Z' => true,
        _ => false,
    }
}

fn is_ident_body(ch: char) -> bool {
    match ch {
        '_' | '0'..='9' => true,
        ch => is_ident_start(ch),
    }
}

fn is_digit(ch: char) -> bool {
    ch.is_digit(10)
}

fn is_hex(ch: char) -> bool {
    ch.is_digit(16)
}

fn is_binary(ch: char) -> bool {
    ch.is_digit(2)
}

pub type SpannedToken<'input> = Spanned<Token<'input>, Location>;

pub type SpannedError = Spanned<Error, Location>;

pub struct Tokenizer<'input> {
    input: &'input str,
    chars: CharLocations<'input>,
    // keeps track of the latest position, which will be the eof_location at
    // the end of lexing
    eof_location: Location,
    lookahead: Option<(Location, char)>,
}

impl<'input> Tokenizer<'input> {
    pub fn new(input: &'input str) -> Tokenizer<'input> {
        let mut chars = CharLocations::new(input);
        let eof_location = chars.location;

        Tokenizer {
            input: input,
            eof_location: eof_location,
            lookahead: chars.next(),
            chars: chars,
        }
    }

    fn bump(&mut self) -> Option<(Location, char)> {
        match self.lookahead {
            Some((location, ch)) => {
                self.eof_location = self.eof_location.shift(ch);
                self.lookahead = self.chars.next();
                Some((location, ch))
            }
            None => None,
        }
    }

    fn skip_to_end(&mut self) {
        while let Some(_) = self.bump() {}
    }

    fn error<T>(&mut self, location: Location, code: Error) -> Result<T, SpannedError> {
        self.skip_to_end();
        error(location, code)
    }

    fn test_lookahead<F>(&self, mut test: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        self.lookahead.map_or(false, |(_, ch)| test(ch))
    }

    fn string_literal(&mut self, start: Location) -> Result<SpannedToken<'input>, SpannedError> {
        let mut string = String::new();

        while let Some((next, ch)) = self.bump() {
            match ch {
                '\\' => string.push(self.escape_code()?),
                '"' => {
                    let end = next.shift(ch);
                    let token = Token::StringLiteral(string);
                    return Ok(spanned(start, end, token));
                }
                ch => string.push(ch),
            }
        }

        self.error(start, UnterminatedStringLiteral)
    }

    fn escape_code(&mut self) -> Result<char, SpannedError> {
        match self.bump() {
            Some((_, '\'')) => Ok('\''),
            Some((_, '"')) => Ok('"'),
            Some((_, '\\')) => Ok('\\'),
            Some((_, '/')) => Ok('/'),
            Some((_, 'n')) => Ok('\n'),
            Some((_, 'r')) => Ok('\r'),
            Some((_, 't')) => Ok('\t'),
            // TODO: unicode escape codes
            Some((start, ch)) => self.error(start, UnexpectedEscapeCode(ch)),
            None => self.eof_error(),
        }
    }

    fn numeric_literal(
        &mut self,
        start: Location,
        is_negative: bool,
    ) -> Result<SpannedToken<'input>, SpannedError> {
        let num_start = if is_negative { start.shift('-') } else { start };
        let (end, prefix) = self.take_while(num_start, is_digit);

        let (start, end, token) = match self.lookahead {
            Some((_, '.')) => {
                self.bump(); // skip '.'
                let (end, float) = self.take_while(num_start, is_digit);
                match self.lookahead {
                    Some((_, ch)) if is_ident_start(ch) => {
                        return self.error(end, UnexpectedChar(ch))
                    }
                    _ => (start, end, Token::FloatLiteral(float.parse().unwrap())),
                }
            }
            // TODO: hex/binary literal code can be shared, especially if
            // negative binary literals are allowed
            Some((end, 'b')) => {
                self.bump(); // skip 'b'
                let (end, bin) = self.take_while(end.shift('b'), is_binary);
                if prefix != "0" || is_negative {
                    return self.error(start, NumericLiteralWrongPrefix);
                }
                match self.lookahead {
                    Some((_, ch)) if is_ident_start(ch) => {
                        return self.error(end, UnexpectedChar(ch))
                    }
                    _ => match u64::from_str_radix(bin, 2) {
                        Ok(val) => (start, end, Token::IntLiteral(val, false)),
                        Err(_) => return self.error(start, BinaryLiteralError),
                    },
                }
            }
            Some((end, 'x')) => {
                self.bump(); // skip 'x'
                let (end, hex) = self.take_while(end.shift('x'), is_hex);
                if prefix != "0" {
                    return self.error(start, NumericLiteralWrongPrefix);
                }
                match self.lookahead {
                    Some((_, ch)) if is_ident_start(ch) => {
                        return self.error(end, UnexpectedChar(ch))
                    }
                    _ => match u64::from_str_radix(hex, 16) {
                        Ok(val) => (start, end, Token::IntLiteral(val, is_negative)),
                        // TODO: convert IntErrorKind
                        Err(_) => return self.error(start, HexLiteralError),
                    },
                }
            }
            Some((start, ch)) if is_ident_start(ch) => {
                return self.error(start, UnexpectedChar(ch))
            }
            None | Some(_) => {
                if let Ok(val) = prefix.parse() {
                    (start, end, Token::IntLiteral(val, is_negative))
                } else {
                    return self.error(start, NonParseableInt);
                }
            }
        };

        Ok(spanned(start, end, token))
    }

    fn comment_or_doc_comment(&mut self, start: Location) -> Option<SpannedToken<'input>> {
        let (end, comment) = self.take_until(start, |ch| ch == '\n');

        // Anything with more than 3 slashes is likely a section break
        if comment.starts_with("////") {
            None
        } else if comment.starts_with("///") {
            Some(spanned(
                start,
                end,
                Token::DocComment(comment[3..].trim().to_string()),
            ))
        } else {
            None
        }
    }

    fn identifier(&mut self, start: Location) -> SpannedToken<'input> {
        let (end, ident) = self.take_while(start, is_ident_body);
        let token = match ident {
            "as" => Token::As,
            "library" => Token::Library,
            "using" => Token::Using,

            "bits" => Token::Bits,
            "const" => Token::Const,
            "enum" => Token::Enum,
            "protocol" => Token::Protocol,
            "service" => Token::Service,
            "strict" => Token::Strict,
            "struct" => Token::Struct,
            "flexible" => Token::Flexible,
            "table" => Token::Table,
            "union" => Token::Union,
            "xunion" => Token::XUnion,
            "error" => Token::Error,
            "reserved" => Token::Reserved,
            "compose" => Token::Compose,
            _ => Token::Identifier(&ident),
        };
        return spanned(start, end, token);
    }

    /// Consumes characters while F returns true, then returns the str from the
    /// start Location to the current Location
    fn take_while<F>(&mut self, start: Location, mut keep_going: F) -> (Location, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        self.take_until(start, |c| !keep_going(c))
    }

    /// Consumes characters until F returns true, then returns the str from the
    /// start Location to the current Location
    fn take_until<F>(&mut self, start: Location, mut terminate: F) -> (Location, &'input str)
    where
        F: FnMut(char) -> bool,
    {
        while let Some((end, ch)) = self.lookahead {
            if terminate(ch) {
                return (end, self.slice(start, end));
            } else {
                self.bump();
            }
        }
        (self.eof_location, self.slice(start, self.eof_location))
    }

    fn eof_error<T>(&mut self) -> Result<T, SpannedError> {
        let location = self.eof_location;
        self.error(location, UnexpectedEof)
    }

    fn slice(&self, start: Location, end: Location) -> &'input str {
        let start = start.absolute;
        let end = end.absolute;
        &self.input[start..end]
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<SpannedToken<'input>, SpannedError>;

    fn next(&mut self) -> Option<Result<SpannedToken<'input>, SpannedError>> {
        while let Some((start, ch)) = self.bump() {
            return match ch {
                ch if ch.is_whitespace() => continue,

                ch if ch == '-' && self.test_lookahead(|ch| ch == '>') => {
                    self.bump();
                    Some(Ok(spanned(start, start.shift(ch).shift('>'), Token::Arrow)))
                }
                ch if is_digit(ch) => Some(self.numeric_literal(start, false)),
                ch if (ch == '-' && self.test_lookahead(is_digit)) => {
                    self.bump(); // skip the negative sign
                    Some(self.numeric_literal(start, true))
                }
                ch if is_ident_start(ch) => Some(Ok(self.identifier(start))),

                '"' => Some(self.string_literal(start)),
                '/' if self.test_lookahead(|ch| ch == '/') => {
                    match self.comment_or_doc_comment(start) {
                        Some(doc_comment) => Some(Ok(doc_comment)),
                        None => continue,
                    }
                }

                '(' => Some(Ok(spanned(start, start.shift(ch), Token::LParen))),
                ')' => Some(Ok(spanned(start, start.shift(ch), Token::RParen))),
                '[' => Some(Ok(spanned(start, start.shift(ch), Token::LSquare))),
                ']' => Some(Ok(spanned(start, start.shift(ch), Token::RSquare))),
                '{' => Some(Ok(spanned(start, start.shift(ch), Token::LCurly))),
                '}' => Some(Ok(spanned(start, start.shift(ch), Token::RCurly))),
                '<' => Some(Ok(spanned(start, start.shift(ch), Token::LAngle))),
                '>' => Some(Ok(spanned(start, start.shift(ch), Token::RAngle))),

                '.' => Some(Ok(spanned(start, start.shift(ch), Token::Dot))),
                ',' => Some(Ok(spanned(start, start.shift(ch), Token::Comma))),
                ';' => Some(Ok(spanned(start, start.shift(ch), Token::Semi))),
                ':' => Some(Ok(spanned(start, start.shift(ch), Token::Colon))),
                '?' => Some(Ok(spanned(start, start.shift(ch), Token::Question))),
                '=' => Some(Ok(spanned(start, start.shift(ch), Token::Equal))),
                '&' => Some(Ok(spanned(start, start.shift(ch), Token::Ampersand))),
                '|' => Some(Ok(spanned(start, start.shift(ch), Token::Pipe))),

                ch if is_ident_start(ch) => Some(Ok(self.identifier(start))),

                ch => Some(self.error(start, UnexpectedChar(ch))),
            };
        }
        Some(Ok(spanned(
            self.eof_location,
            self.eof_location,
            Token::EOF,
        )))
    }
}

// TODO: the Lexer and Tokenizer should be flattened into one. The Tokenizer
// does not need to return full Locations
// TODO: maybe lexer should exist and keep track of some state to implement
// stuff that lalrpop can't handle (e.g. comments within doc comments). we could
// also check e.g. that there are no blank lines within doc comments
pub struct Lexer<'input> {
    tokenizer: Tokenizer<'input>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Lexer<'input> {
        Lexer {
            tokenizer: Tokenizer::new(input),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Result<(usize, Token<'input>, usize), SpannedError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.tokenizer.next() {
            Some(Ok(Spanned {
                value: Token::EOF, ..
            })) => None,
            Some(Ok(spanned)) => Some(Ok((
                spanned.span.start.absolute,
                spanned.value,
                spanned.span.end.absolute,
            ))),
            Some(Err(err)) => Some(Err(err)),
            None => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use super::Token::*;
    use crate::source_file::{to_offsets, Lines};

    fn tokenizer<'input>(
        input: &'input str,
    ) -> Box<dyn Iterator<Item = Result<SpannedToken<'input>, SpannedError>> + 'input> where {
        Box::new(Tokenizer::new(input).take_while(|token| match *token {
            Ok(Spanned {
                value: Token::EOF, ..
            }) => false,
            _ => true,
        }))
    }

    fn test(input: &str, expected: Vec<(&str, Token)>) {
        let mut tokenizer = tokenizer(input);
        let mut count = 0;
        let length = expected.len();
        let lines = Lines::new(to_offsets(input), input.len());
        for (token, (expected_span, expected_tok)) in tokenizer.by_ref().zip(expected.into_iter()) {
            count += 1;
            println!("{:?}", token);
            let start_byte = expected_span.find("@").unwrap();
            let start = lines.location(start_byte.into()).unwrap();

            let end_byte = expected_span.rfind("@").unwrap() + 1;
            let end = lines.location(end_byte.into()).unwrap();

            assert_eq!(Ok(spanned(start, end, expected_tok)), token);
        }

        assert_eq!(count, length);
        assert_eq!(true, count > 0);

        // Make sure that there is nothing else to consume
        assert_eq!(None, tokenizer.next());
    }

    #[test]
    fn sample_array() {
        test(
            r#"[1, a]"#,
            vec![
                (r#"@....."#, LSquare),
                (r#".@...."#, IntLiteral(1, false)),
                (r#"..@..."#, Comma),
                (r#"....@."#, Identifier("a")),
                (r#".....@"#, RSquare),
            ],
        );
    }

    #[test]
    fn keywords() {
        test(
            r#"as library using bits"#,
            vec![
                (r#"@@..................."#, As),
                (r#"...@@@@@@@..........."#, Library),
                (r#"...........@@@@@....."#, Using),
                (r#".................@@@@"#, Bits),
            ],
        )
    }

    #[test]
    fn numeric_literals() {
        test(
            r#"0xdef -0x321aF"#,
            vec![
                (r#"@@@@@........."#, IntLiteral(0xdef, false)),
                (r#"......@@@@@@@@"#, IntLiteral(0x321af, true)),
            ],
        )
    }

    #[test]
    fn string_literals() {
        test(
            r#""hello"  "\nwhite\tspace\n\r\"slash:\\" "#,
            vec![
                (
                    r#"@@@@@@@  "\nwhite\tspace\n\r\"slash:\\" "#,
                    StringLiteral("hello".into()),
                ),
                (
                    r#""hello"  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ "#,
                    StringLiteral("\nwhite\tspace\n\r\"slash:\\".into()),
                ),
            ],
        )
    }
}
