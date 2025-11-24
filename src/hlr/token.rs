#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    NumLiteral(String),
    BoolLiteral(bool),

    LBrace,             // {
    RBrace,             // }
    LParen,             // (
    RParen,             // )
    Semicolon,          // ;
    Comma,              // ,
    Equal,              // =
    EqualEqual,         // ==
    BangEqual,          // !=
    Dot,                // .
    Colon,              // :
    ColonColon,         // ::
    Ampersand,          // &
    AmpersandAmpersand, // &&
    Pipe,               // |
    PipePipe,           // ||
    Arrow,              // ->
    BoldArrow,          // =>
    Plus,               // +
    Minus,              // -
    Asterisk,           // *
    Slash,              // /
    Percent,            // %
    Smaller,            // <
    Greater,            // >
    SmallerEqual,       // <=
    GreaterEqual,       // >=
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Break,
    Else,
    Enum,
    Fn,
    If,
    Let,
    Loop,
    Match,
    Return,
    Struct,
}
