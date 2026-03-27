use super::{ParserErr, Token, token::Keyword};

pub(super) struct TokenStream<'token> {
    input: &'token [Token],
    position: usize,
}

impl<'token> TokenStream<'token> {
    pub(super) fn new(input: &'token [Token]) -> Self {
        TokenStream { input, position: 0 }
    }

    pub(super) fn current(&self) -> Option<&Token> {
        self.input.get(self.position)
    }

    pub(super) fn next(&self) -> Option<&Token> {
        self.input.get(self.position + 1)
    }

    pub(super) fn advance(&mut self) {
        self.position += 1;
    }

    pub(super) fn advance_if(&mut self, token: Token) -> bool {
        if let Some(t) = self.current()
            && *t == token
        {
            self.position += 1;
            true
        } else {
            false
        }
    }

    pub(super) fn advance_if_turbofish(&mut self) -> bool {
        if self.current() == Some(&Token::ColonColon) && self.next() == Some(&Token::Smaller) {
            self.position += 2;
            true
        } else {
            false
        }
    }

    pub(super) fn expect_token(&mut self, token: Token) -> Result<Token, ParserErr> {
        let current = self.current().ok_or(ParserErr::UnexpectedEOF)?;
        if *current == token {
            self.position += 1;
            Ok(token)
        } else {
            Err(ParserErr::UnexpectedToken(current.clone()))
        }
    }

    pub(super) fn expect_keyword(&mut self, keyword: Keyword) -> Result<Keyword, ParserErr> {
        match self.expect_token(Token::Keyword(keyword))? {
            Token::Keyword(k) => Ok(k),
            _ => unreachable!(),
        }
    }

    pub(super) fn expect_identifier(&mut self) -> Result<String, ParserErr> {
        let name = match self.current().ok_or(ParserErr::UnexpectedEOF)? {
            Token::Identifier(name) => name.clone(),
            token => return Err(ParserErr::UnexpectedToken(token.clone())),
        };
        match self.expect_token(Token::Identifier(name))? {
            Token::Identifier(name) => Ok(name),
            _ => unreachable!(),
        }
    }
}
