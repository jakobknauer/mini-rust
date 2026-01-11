#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Identifier(String),
    NumLiteral(String),
    BoolLiteral(bool),
    CCharLiteral(u8),
    CStringLiteral(Vec<u8>),

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
    Dots,               // ...
    Underscore,         // _
    Bang,               // !
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    As,
    Break,
    FnTrait,
    Else,
    Enum,
    Fn,
    For,
    If,
    Impl,
    Let,
    Loop,
    Match,
    Return,
    SelfTy,
    Self_,
    Struct,
    Trait,
    Where,
    While,
}
