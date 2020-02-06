#[derive(Clone, PartialEq, Debug)]
pub enum Token<'input> {
    // keywords
    As,
    Library,
    Using,

    Bits,
    Const,
    Enum,
    Protocol,
    Service,
    Strict,
    Struct,
    Flexible,
    Table,
    Union,
    XUnion,
    Error,
    Reserved,
    Compose,
    Identifier(&'input str),

    // literals
    StringLiteral(String),
    // the u64 stores the bits of the integer, and the bool stores whether the
    // literal had a leading "-" sign. fidlc just lexes these as a string and
    // then parses and validates at the same time; for now we parse the number
    // while lexing since we're already working with the invidual chars.
    IntLiteral(u64, bool),
    FloatLiteral(f64),
    DocComment(String),
    True,
    False,

    // symbols
    LCurly,
    LSquare,
    LParen,
    LAngle,
    RCurly,
    RSquare,
    RParen,
    RAngle,
    Dot,
    Comma,
    Semi,
    Colon,
    Question,
    Equal,
    Ampersand,
    Arrow,
    Pipe,

    EOF,
}
