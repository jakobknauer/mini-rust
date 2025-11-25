#[macro_use]
mod macros;

use crate::hlr::{defs::*, lexer, token::Keyword};

pub use crate::hlr::{lexer::LexerError, token::Token};

pub fn build_program(input: &str) -> Result<Program, ParserError> {
    let tokens = lexer::get_tokens(input).map_err(ParserError::LexerError)?;
    let mut parser = HlrParser::new(&tokens[..]);
    parser.parse_program()
}

#[cfg(test)]
fn build_expr(input: &str) -> Result<Expr, ParserError> {
    let tokens = lexer::get_tokens(input).map_err(ParserError::LexerError)?;
    let mut parser = HlrParser::new(&tokens[..]);
    parser.parse_expr(true)
}

#[cfg(test)]
fn build_block(input: &str) -> Result<Block, ParserError> {
    let tokens = lexer::get_tokens(input).map_err(ParserError::LexerError)?;
    let mut parser = HlrParser::new(&tokens[..]);
    parser.parse_block()
}

#[derive(Debug)]
pub enum ParserError {
    LexerError(LexerError),
    UnexpectedToken(Token),
    UndelimitedStmt,
    InvalidLiteral,
    UnexpectedEOF,
}

struct HlrParser<'a> {
    input: &'a [Token],
    position: usize,
}

#[derive(PartialEq, Eq)]
enum StmtKind {
    /// A statement that is an expression but is delimited by a closing brace, e.g. if, loop,
    /// match, or a block. This is allowed as the last statement in a block, but is also allowed
    /// to be followed by other statements without requireing a semicolon in between.
    BlockExpr,
    /// A statement that is an expression but is not delimited by a semicolon, e.g. `1 + a`.
    /// This is only allowed as the final statement in a block, representing the return value.
    UndelimitedExpr,
    /// A statement that is explicitly delimited by a semicolon. If the last statement in a block
    /// is of this kind, it does not represent the return value of the block.
    ExplicitelyDelimited,
}

impl<'a> HlrParser<'a> {
    fn new(input: &'a [Token]) -> Self {
        HlrParser { input, position: 0 }
    }

    fn current(&self) -> Option<&Token> {
        self.input.get(self.position)
    }

    fn expect_keyword(&mut self, keyword: Keyword) -> Result<(), ParserError> {
        let current = self.current().ok_or(ParserError::UnexpectedEOF)?;
        match current {
            Token::Keyword(k) if *k == keyword => {
                self.position += 1;
                Ok(())
            }
            token => Err(ParserError::UnexpectedToken(token.clone())),
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParserError> {
        let current = self.current().ok_or(ParserError::UnexpectedEOF)?;
        match current {
            Token::Identifier(name) => {
                let name = name.to_string();
                self.position += 1;
                Ok(name)
            }
            token => Err(ParserError::UnexpectedToken(token.clone())),
        }
    }

    fn expect_token(&mut self, token: Token) -> Result<(), ParserError> {
        let current = self.current().ok_or(ParserError::UnexpectedEOF)?;
        if *current == token {
            self.position += 1;
            Ok(())
        } else {
            Err(ParserError::UnexpectedToken(current.clone()))
        }
    }

    fn advance_if(&mut self, token: Token) -> bool {
        if let Some(t) = self.current()
            && *t == token
        {
            self.position += 1;
            true
        } else {
            false
        }
    }

    fn parse_program(&mut self) -> Result<Program, ParserError> {
        let mut program = Program::new();

        while let Some(token) = self.current() {
            match token {
                Token::Keyword(Keyword::Fn) => program.fns.push(self.parse_function()?),
                Token::Keyword(Keyword::Struct) => program.structs.push(self.parse_struct()?),
                Token::Keyword(Keyword::Enum) => program.enums.push(self.parse_enum()?),
                token => return Err(ParserError::UnexpectedToken(token.clone())),
            }
        }

        Ok(program)
    }

    fn parse_function(&mut self) -> Result<Fn, ParserError> {
        self.expect_keyword(Keyword::Fn)?;
        let name = self.expect_identifier()?;

        let gen_params = if self.current() == Some(&Token::Smaller) {
            self.parse_fn_generic_params()?
        } else {
            Vec::new()
        };

        self.expect_token(Token::LParen)?;
        let params = self.parse_fn_params()?;
        self.expect_token(Token::RParen)?;

        let return_ty = self.parse_function_return_type()?;
        let body = self.parse_block()?;

        Ok(Fn {
            name,
            gen_params,
            params,
            return_ty,
            body,
        })
    }

    fn parse_fn_generic_params(&mut self) -> Result<Vec<String>, ParserError> {
        self.expect_token(Token::Smaller)?;

        let mut params = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            let arg = self.expect_identifier()?;
            params.push(arg);

            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        self.expect_token(Token::Greater)?;
        Ok(params)
    }

    fn parse_fn_params(&mut self) -> Result<Vec<Param>, ParserError> {
        let mut params = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            params.push(self.parse_function_param()?);

            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(params)
    }

    fn parse_function_param(&mut self) -> Result<Param, ParserError> {
        let name = self.expect_identifier()?;
        self.expect_token(Token::Colon)?;
        let ty = self.parse_ty_annot()?;
        Ok(Param { name, ty })
    }

    fn parse_function_return_type(&mut self) -> Result<Option<TyAnnot>, ParserError> {
        if self.advance_if(Token::Arrow) {
            let return_type = self.parse_ty_annot()?;
            Ok(Some(return_type))
        } else {
            Ok(None)
        }
    }

    fn parse_struct(&mut self) -> Result<Struct, ParserError> {
        self.expect_keyword(Keyword::Struct)?;
        let name = self.expect_identifier()?;

        self.expect_token(Token::LBrace)?;
        let fields = self.parse_struct_fields()?;
        self.expect_token(Token::RBrace)?;

        Ok(Struct { name, fields })
    }

    fn parse_struct_fields(&mut self) -> Result<Vec<StructField>, ParserError> {
        let mut fields = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            fields.push(self.parse_struct_field()?);

            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(fields)
    }

    fn parse_struct_field(&mut self) -> Result<StructField, ParserError> {
        let name = self.expect_identifier()?;
        self.expect_token(Token::Colon)?;
        let ty = self.parse_ty_annot()?;
        Ok(StructField { name, ty })
    }

    fn parse_enum(&mut self) -> Result<Enum, ParserError> {
        self.expect_keyword(Keyword::Enum)?;
        let name = self.expect_identifier()?;

        self.expect_token(Token::LBrace)?;
        let variants = self.parse_enum_variants()?;
        self.expect_token(Token::RBrace)?;

        Ok(Enum { name, variants })
    }

    fn parse_enum_variants(&mut self) -> Result<Vec<EnumVariant>, ParserError> {
        let mut variants = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            variants.push(self.parse_enum_variant()?);

            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(variants)
    }

    fn parse_enum_variant(&mut self) -> Result<EnumVariant, ParserError> {
        let name = self.expect_identifier()?;
        self.expect_token(Token::LBrace)?;
        let fields = self.parse_struct_fields()?;
        self.expect_token(Token::RBrace)?;
        Ok(EnumVariant { name, fields })
    }

    /// TODO allow loop/if/block expression statements without delimiting semicolon
    fn parse_block(&mut self) -> Result<Block, ParserError> {
        let mut stmts = Vec::new();
        let mut return_expr = None;

        self.expect_token(Token::LBrace)?;

        loop {
            while self.advance_if(Token::Semicolon) {}

            if let Some(Token::RBrace) = self.current() {
                break;
            }

            let (stmt, stmt_kind) = self.parse_stmt()?;

            if stmt_kind == StmtKind::UndelimitedExpr {
                // expect that we are at the end of the block
                if self.current() != Some(&Token::RBrace) {
                    return Err(ParserError::UndelimitedStmt);
                }
            }

            if self.current() == Some(&Token::RBrace) {
                if stmt_kind == StmtKind::UndelimitedExpr || stmt_kind == StmtKind::BlockExpr {
                    let Stmt::Expr(expr) = stmt else {
                        unreachable!();
                    };
                    return_expr = Some(Box::new(expr));
                } else {
                    stmts.push(stmt);
                }
                break;
            } else {
                stmts.push(stmt);
            }
        }

        self.expect_token(Token::RBrace)?;

        Ok(Block { stmts, return_expr })
    }

    fn parse_stmt(&mut self) -> Result<(Stmt, StmtKind), ParserError> {
        match self.current() {
            Some(Token::Keyword(Keyword::Let)) => {
                let stmt = self.parse_let_stmt()?;
                self.expect_token(Token::Semicolon)?;
                Ok((stmt, StmtKind::ExplicitelyDelimited))
            }
            Some(Token::Keyword(Keyword::Return)) => {
                let stmt = self.parse_return_stmt()?;
                self.expect_token(Token::Semicolon)?;
                Ok((stmt, StmtKind::ExplicitelyDelimited))
            }
            Some(Token::Keyword(Keyword::Break)) => {
                let stmt = self.parse_break_stmt()?;
                self.expect_token(Token::Semicolon)?;
                Ok((stmt, StmtKind::ExplicitelyDelimited))
            }
            Some(Token::LBrace) => {
                let expr = self.parse_block()?;
                let stmt = Stmt::Expr(Expr::Block(expr));
                Ok((stmt, StmtKind::BlockExpr))
            }
            Some(Token::Keyword(Keyword::If)) => {
                let expr = self.parse_if_expr()?;
                Ok((Stmt::Expr(expr), StmtKind::BlockExpr))
            }
            Some(Token::Keyword(Keyword::Loop)) => {
                let expr = self.parse_loop()?;
                Ok((Stmt::Expr(expr), StmtKind::BlockExpr))
            }
            Some(Token::Keyword(Keyword::Match)) => {
                let expr = self.parse_match()?;
                Ok((Stmt::Expr(expr), StmtKind::BlockExpr))
            }
            _ => {
                let stmt = self.parse_expr_stmt()?;
                let semicolon = self.advance_if(Token::Semicolon);
                let stmt_kind = if semicolon {
                    StmtKind::ExplicitelyDelimited
                } else {
                    StmtKind::UndelimitedExpr
                };
                Ok((stmt, stmt_kind))
            }
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.expect_keyword(Keyword::Let)?;
        let name = self.expect_identifier()?;
        let ty_annot = if self.advance_if(Token::Colon) {
            Some(self.parse_ty_annot()?)
        } else {
            None
        };
        self.expect_token(Token::Equal)?;
        let value = self.parse_expr(true)?;
        Ok(Stmt::Let { name, ty_annot, value })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.expect_keyword(Keyword::Return)?;
        if let Some(Token::Semicolon) = self.current() {
            Ok(Stmt::Return(None))
        } else {
            let expr = self.parse_expr(true)?;
            Ok(Stmt::Return(Some(expr)))
        }
    }

    fn parse_break_stmt(&mut self) -> Result<Stmt, ParserError> {
        self.expect_keyword(Keyword::Break)?;
        Ok(Stmt::Break)
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.parse_expr(true)?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserError> {
        self.parse_assign_expr(allow_top_level_struct_expr)
    }

    fn parse_assign_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserError> {
        let target = self.parse_disjunction(allow_top_level_struct_expr)?;
        if self.advance_if(Token::Equal) {
            let value = self.parse_disjunction(allow_top_level_struct_expr)?;
            Ok(Expr::Assign {
                target: Box::new(target),
                value: Box::new(value),
            })
        } else {
            Ok(target)
        }
    }

    parse_left_associative!(parse_disjunction, parse_conjunction, [
        Token::Pipe => BinaryOperator::BitOr
    ]);

    parse_left_associative!(parse_conjunction, parse_equality_expr, [
        Token::Ampersand => BinaryOperator::BitAnd
    ]);

    parse_left_associative!(parse_equality_expr, parse_comparison, [
        Token::EqualEqual => BinaryOperator::Equal,
        Token::BangEqual => BinaryOperator::NotEqual
    ]);

    parse_left_associative!(parse_comparison, parse_sum, [
        Token::Smaller => BinaryOperator::LessThan,
        Token::Greater => BinaryOperator::GreaterThan,
        Token::SmallerEqual => BinaryOperator::LessThanOrEqual,
        Token::GreaterEqual => BinaryOperator::GreaterThanOrEqual
    ]);

    parse_left_associative!(parse_sum, parse_product, [
        Token::Plus => BinaryOperator::Add,
        Token::Minus => BinaryOperator::Subtract
    ]);

    parse_left_associative!(parse_product, parse_unary_expr, [
        Token::Asterisk => BinaryOperator::Multiply,
        Token::Slash => BinaryOperator::Divide,
        Token::Percent => BinaryOperator::Remainder
    ]);

    fn parse_unary_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserError> {
        if self.advance_if(Token::Asterisk) {
            let base = self.parse_unary_expr(allow_top_level_struct_expr)?;
            Ok(Expr::Deref { base: Box::new(base) })
        } else if self.advance_if(Token::Ampersand) {
            let base = self.parse_unary_expr(allow_top_level_struct_expr)?;
            Ok(Expr::AddrOf { base: Box::new(base) })
        } else if self.advance_if(Token::AmpersandAmpersand) {
            let base = self.parse_unary_expr(allow_top_level_struct_expr)?;
            Ok(Expr::AddrOf {
                base: Box::new(Expr::AddrOf { base: Box::new(base) }),
            })
        } else {
            self.parse_function_call_and_field_access(allow_top_level_struct_expr)
        }
    }

    fn parse_function_call_and_field_access(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserError> {
        let mut acc = self.parse_primary_expr(allow_top_level_struct_expr)?;

        loop {
            if self.advance_if(Token::LParen) {
                // function call
                let mut arguments = Vec::new();
                while self.current() != Some(&Token::RParen) {
                    let argument = self.parse_expr(true)?; // Allow top-level struct expression in argument
                    arguments.push(argument);
                    if !self.advance_if(Token::Comma) {
                        break;
                    }
                }
                self.expect_token(Token::RParen)?;
                acc = Expr::Call {
                    callee: Box::new(acc),
                    arguments,
                };
            } else if self.advance_if(Token::Dot) {
                // field access
                let field_name = self.expect_identifier()?;
                acc = Expr::FieldAccess {
                    base: Box::new(acc),
                    name: field_name,
                };
            } else {
                break;
            }
        }

        Ok(acc)
    }

    fn parse_primary_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserError> {
        let current = self.current().ok_or(ParserError::UnexpectedEOF)?;

        match current {
            Token::NumLiteral(value) => {
                let value = value.parse().map_err(|_| ParserError::InvalidLiteral)?;
                self.position += 1;
                Ok(Expr::Lit(Lit::Int(value)))
            }
            Token::BoolLiteral(b) => {
                let value = *b;
                self.position += 1;
                Ok(Expr::Lit(Lit::Bool(value)))
            }
            Token::Identifier(ident) => {
                let ident = ident.clone();
                self.position += 1;

                if allow_top_level_struct_expr && self.advance_if(Token::LBrace) {
                    // parse struct expr
                    let mut fields = Vec::new();
                    while let Some(Token::Identifier(field_name)) = self.current() {
                        let field_name = field_name.clone();
                        self.position += 1; // consume field name
                        self.expect_token(Token::Colon)?;
                        let field_value = self.parse_expr(true)?;
                        fields.push((field_name, field_value));
                        if !self.advance_if(Token::Comma) {
                            break;
                        }
                    }
                    self.expect_token(Token::RBrace)?;
                    Ok(Expr::Struct { name: ident, fields })
                } else if self.advance_if(Token::ColonColon) {
                    // parse generic qualified ident
                    let mut gen_args = Vec::new();
                    self.expect_token(Token::Smaller)?;
                    while self.current() != Some(&Token::Greater) {
                        let gen_arg = self.parse_ty_annot()?;
                        gen_args.push(gen_arg);
                        if !self.advance_if(Token::Comma) {
                            break;
                        }
                    }
                    self.expect_token(Token::Greater)?;
                    Ok(Expr::GenQualIdent { ident, gen_args })
                } else {
                    Ok(Expr::Ident(ident))
                }
            }
            Token::LParen => {
                self.position += 1;
                let expr = self.parse_expr(true)?; // Allow top-level struct expression in parens
                self.expect_token(Token::RParen)?;
                Ok(expr)
            }
            Token::LBrace => {
                let block = self.parse_block()?;
                Ok(Expr::Block(block))
            }
            Token::Keyword(Keyword::If) => self.parse_if_expr(),
            Token::Keyword(Keyword::Loop) => self.parse_loop(),
            Token::Keyword(Keyword::Match) => self.parse_match(),

            token => Err(ParserError::UnexpectedToken(token.clone())),
        }
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect_keyword(Keyword::If)?;
        let condition = self.parse_expr(false)?; // Don't allow top-level struct in if condition
        let then_block = self.parse_block()?;
        let else_block = if self.advance_if(Token::Keyword(Keyword::Else)) {
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Expr::If {
            condition: Box::new(condition),
            then_block,
            else_block,
        })
    }

    fn parse_loop(&mut self) -> Result<Expr, ParserError> {
        self.expect_keyword(Keyword::Loop)?;
        let body = self.parse_block()?;
        Ok(Expr::Loop { body })
    }

    fn parse_match(&mut self) -> Result<Expr, ParserError> {
        self.expect_keyword(Keyword::Match)?;
        let scrutinee = self.parse_expr(false)?; // Don't allow top-level struct in match scrutinee

        let mut arms = Vec::new();
        self.expect_token(Token::LBrace)?;

        while Some(&Token::RBrace) != self.current() {
            let pattern = self.parse_struct_pattern()?;
            self.expect_token(Token::BoldArrow)?;
            let value = self.parse_expr(true)?; // Allow top-level struct expr in match arm body

            arms.push(MatchArm {
                pattern,
                value: Box::new(value),
            });

            if !self.advance_if(Token::Comma) {
                break;
            }
        }

        self.expect_token(Token::RBrace)?;

        Ok(Expr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
        })
    }

    fn parse_struct_pattern(&mut self) -> Result<StructPattern, ParserError> {
        let variant = self.expect_identifier()?;

        self.expect_token(Token::LBrace)?;

        let mut fields = Vec::new();
        while let Some(Token::Identifier(field_name)) = self.current() {
            let field_name = field_name.clone();
            self.position += 1; // consume field name
            self.expect_token(Token::Colon)?;
            let binding_name = self.expect_identifier()?;

            fields.push(StructPatternField {
                field_name,
                binding_name,
            });

            if !self.advance_if(Token::Comma) {
                break;
            }
        }

        self.expect_token(Token::RBrace)?;

        Ok(StructPattern { variant, fields })
    }

    fn parse_ty_annot(&mut self) -> Result<TyAnnot, ParserError> {
        let current = self.current().ok_or(ParserError::UnexpectedEOF)?;
        match current {
            Token::Identifier(ident) => {
                let ident = ident.clone();
                self.position += 1;
                Ok(TyAnnot::Named(ident))
            }
            Token::Ampersand => {
                self.position += 1;
                let inner_ty = self.parse_ty_annot()?;
                Ok(TyAnnot::Reference(Box::new(inner_ty)))
            }
            Token::AmpersandAmpersand => {
                // Two levels of reference
                self.position += 1;
                let inner_ty = self.parse_ty_annot()?;
                Ok(TyAnnot::Reference(Box::new(TyAnnot::Reference(Box::new(inner_ty)))))
            }
            Token::LParen => {
                self.position += 1;
                self.expect_token(Token::RParen)?;
                Ok(TyAnnot::Unit)
            }
            Token::Keyword(Keyword::Fn) => {
                self.position += 1;
                self.expect_token(Token::LParen)?;
                let mut param_tys = Vec::new();
                while self.current() != Some(&Token::RParen) {
                    let param_type = self.parse_ty_annot()?;
                    param_tys.push(param_type);
                    if !self.advance_if(Token::Comma) {
                        break;
                    }
                }
                self.expect_token(Token::RParen)?;

                let return_ty = if self.advance_if(Token::Arrow) {
                    Some(Box::new(self.parse_ty_annot()?))
                } else {
                    None
                };

                Ok(TyAnnot::Fn { param_tys, return_ty })
            }
            token => Err(ParserError::UnexpectedToken(token.clone())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod program {
        use super::*;

        fn parse_and_compare(input: &str, expected: Program) {
            let parsed = build_program(input).expect("Failed to parse HLR");
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_empty_program_items() {
            let input = r#"
                fn empty() {
                }

                struct Empty {
                }

                enum Empty {
                }
                "#;

            let expected = Program {
                fns: vec![Fn {
                    name: "empty".to_string(),
                    gen_params: vec![],
                    params: vec![],
                    return_ty: None,
                    body: Block {
                        stmts: vec![],
                        return_expr: None,
                    },
                }],
                structs: vec![Struct {
                    name: "Empty".to_string(),
                    fields: vec![],
                }],
                enums: vec![Enum {
                    name: "Empty".to_string(),
                    variants: vec![],
                }],
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_simple_function() {
            let input = r#"
                fn add(a: int, b: int) -> int {
                    return a + b;
                }
                "#;

            let expected = Program {
                fns: vec![Fn {
                    name: "add".to_string(),
                    gen_params: vec![],
                    params: vec![
                        Param {
                            name: "a".to_string(),
                            ty: TyAnnot::Named("int".to_string()),
                        },
                        Param {
                            name: "b".to_string(),
                            ty: TyAnnot::Named("int".to_string()),
                        },
                    ],
                    return_ty: Some(TyAnnot::Named("int".to_string())),
                    body: Block {
                        stmts: vec![Stmt::Return(Some(Expr::BinaryOp {
                            left: Box::new(Expr::Ident("a".to_string())),
                            operator: BinaryOperator::Add,
                            right: Box::new(Expr::Ident("b".to_string())),
                        }))],
                        return_expr: None,
                    },
                }],
                structs: vec![],
                enums: vec![],
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_simple_struct() {
            let input = r#"
                struct Point {
                    x: int,
                    y: int,
                }"#;

            let expected = Program {
                fns: vec![],
                structs: vec![Struct {
                    name: "Point".to_string(),
                    fields: vec![
                        StructField {
                            name: "x".to_string(),
                            ty: TyAnnot::Named("int".to_string()),
                        },
                        StructField {
                            name: "y".to_string(),
                            ty: TyAnnot::Named("int".to_string()),
                        },
                    ],
                }],
                enums: vec![],
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_simple_enum() {
            let input = r#"
                enum State {
                    Off {},
                    On {},
                    Unknown {},
                }"#;

            let expected = Program {
                fns: vec![],
                structs: vec![],
                enums: vec![Enum {
                    name: "State".to_string(),
                    variants: vec![
                        EnumVariant {
                            name: "Off".to_string(),
                            fields: vec![],
                        },
                        EnumVariant {
                            name: "On".to_string(),
                            fields: vec![],
                        },
                        EnumVariant {
                            name: "Unknown".to_string(),
                            fields: vec![],
                        },
                    ],
                }],
            };

            parse_and_compare(input, expected);
        }
    }

    mod block {
        use super::*;

        fn parse_and_compare(input: &str, expected: Block) {
            let parsed = build_block(input).expect("Failed to parse HLR");
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_empty_block() {
            let input = "{}";

            let expected = Block {
                stmts: vec![],
                return_expr: None,
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_control_structures() {
            let input = r#"{
                let result = 1;
                loop {
                    if n == 1 {
                        break;
                    };
                    result = result * n;
                    n = n - 1;
                };
                result
            }"#;

            let expected = Block {
                stmts: vec![
                    Stmt::Let {
                        name: "result".to_string(),
                        ty_annot: None,
                        value: Expr::Lit(Lit::Int(1)),
                    },
                    Stmt::Expr(Expr::Loop {
                        body: Block {
                            stmts: vec![
                                Stmt::Expr(Expr::If {
                                    condition: Box::new(Expr::BinaryOp {
                                        left: Box::new(Expr::Ident("n".to_string())),
                                        operator: BinaryOperator::Equal,
                                        right: Box::new(Expr::Lit(Lit::Int(1))),
                                    }),
                                    then_block: Block {
                                        stmts: vec![Stmt::Break],
                                        return_expr: None,
                                    },
                                    else_block: None,
                                }),
                                Stmt::Expr(Expr::Assign {
                                    target: Box::new(Expr::Ident("result".to_string())),
                                    value: Box::new(Expr::BinaryOp {
                                        left: Box::new(Expr::Ident("result".to_string())),
                                        operator: BinaryOperator::Multiply,
                                        right: Box::new(Expr::Ident("n".to_string())),
                                    }),
                                }),
                                Stmt::Expr(Expr::Assign {
                                    target: Box::new(Expr::Ident("n".to_string())),
                                    value: Box::new(Expr::BinaryOp {
                                        left: Box::new(Expr::Ident("n".to_string())),
                                        operator: BinaryOperator::Subtract,
                                        right: Box::new(Expr::Lit(Lit::Int(1))),
                                    }),
                                }),
                            ],
                            return_expr: None,
                        },
                    }),
                ],
                return_expr: Some(Box::new(Expr::Ident("result".to_string()))),
            };

            parse_and_compare(input, expected);
        }
    }

    mod expr {
        use super::*;

        fn parse_and_compare(input: &str, expected: Expr) {
            let parsed = build_expr(input).expect("Failed to parse HLR");
            assert_eq!(parsed, expected);
        }

        #[test]
        fn test_parse_arithmetic_expr() {
            let input = r#"1 + 2 * a - arctan2(x, y)"#;

            let expected = Expr::BinaryOp {
                left: Box::new(Expr::BinaryOp {
                    left: Box::new(Expr::Lit(Lit::Int(1))),
                    operator: BinaryOperator::Add,
                    right: Box::new(Expr::BinaryOp {
                        left: Box::new(Expr::Lit(Lit::Int(2))),
                        operator: BinaryOperator::Multiply {},
                        right: Box::new(Expr::Ident("a".to_string())),
                    }),
                }),
                operator: BinaryOperator::Subtract,
                right: Box::new(Expr::Call {
                    callee: Box::new(Expr::Ident("arctan2".to_string())),
                    arguments: vec![Expr::Ident("x".to_string()), Expr::Ident("y".to_string())],
                }),
            };
            parse_and_compare(input, expected);
        }

        #[test]
        fn parse_struct_expr() {
            let input = r#"
                Circle {
                    center: Point { x: 1, y: 2 },
                    radius: 3
                }"#;

            let expected = Expr::Struct {
                name: "Circle".to_string(),
                fields: vec![
                    (
                        "center".to_string(),
                        Expr::Struct {
                            name: "Point".to_string(),
                            fields: vec![
                                ("x".to_string(), Expr::Lit(Lit::Int(1))),
                                ("y".to_string(), Expr::Lit(Lit::Int(2))),
                            ],
                        },
                    ),
                    ("radius".to_string(), Expr::Lit(Lit::Int(3))),
                ],
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_int_literal() {
            let input = "42";
            let expected = Expr::Lit(Lit::Int(42));
            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_if_expr() {
            let input = r#"if condition {
                42
            } else {
                0
            }"#;

            let expected = Expr::If {
                condition: Box::new(Expr::Ident("condition".to_string())),
                then_block: Block {
                    stmts: vec![],
                    return_expr: Some(Box::new(Expr::Lit(Lit::Int(42)))),
                },
                else_block: Some(Block {
                    stmts: vec![],
                    return_expr: Some(Box::new(Expr::Lit(Lit::Int(0)))),
                }),
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_function_call_and_field_access() {
            let input = "obj.method(arg1, arg2).field";
            let expected = Expr::FieldAccess {
                base: Box::new(Expr::Call {
                    callee: Box::new(Expr::FieldAccess {
                        base: Box::new(Expr::Ident("obj".to_string())),
                        name: "method".to_string(),
                    }),
                    arguments: vec![Expr::Ident("arg1".to_string()), Expr::Ident("arg2".to_string())],
                }),
                name: "field".to_string(),
            };
            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_match_expr() {
            let input = r#"match value {
                Some { inner: a } => a,
                None {} => 0,
            }"#;

            let expected = Expr::Match {
                scrutinee: Box::new(Expr::Ident("value".to_string())),
                arms: vec![
                    MatchArm {
                        pattern: StructPattern {
                            variant: "Some".to_string(),
                            fields: vec![StructPatternField {
                                field_name: "inner".to_string(),
                                binding_name: "a".to_string(),
                            }],
                        },
                        value: Box::new(Expr::Ident("a".to_string())),
                    },
                    MatchArm {
                        pattern: StructPattern {
                            variant: "None".to_string(),
                            fields: vec![],
                        },
                        value: Box::new(Expr::Lit(Lit::Int(0))),
                    },
                ],
            };

            parse_and_compare(input, expected);
        }
    }
}
