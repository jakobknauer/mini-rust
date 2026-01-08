#[macro_use]
mod macros;

use crate::hlr::{defs::*, lexer, token::Keyword};

pub use crate::hlr::{lexer::LexerErr, token::Token};

pub fn parse(input: &str, output: &mut Program) -> Result<(), ParserErr> {
    let tokens = lexer::get_tokens(input)?;
    let mut parser = HlrParser::new(&tokens[..]);
    parser.parse_program(output)
}

#[cfg(test)]
fn parse_expr(input: &str) -> Result<Expr, ParserErr> {
    let tokens = lexer::get_tokens(input)?;
    let mut parser = HlrParser::new(&tokens[..]);
    parser.parse_expr(true)
}

#[cfg(test)]
fn parse_block(input: &str) -> Result<Block, ParserErr> {
    let tokens = lexer::get_tokens(input)?;
    let mut parser = HlrParser::new(&tokens[..]);
    parser.parse_block()
}

#[derive(Debug)]
pub enum ParserErr {
    LexerErr(LexerErr),
    UnexpectedToken(Token),
    UndelimitedStmt,
    InvalidLiteral,
    UnexpectedEOF,
    TraitMthdWithBody,
    ExpectedTraitName,
    UnexpectedReceiverArg,
}

impl From<LexerErr> for ParserErr {
    fn from(err: LexerErr) -> Self {
        ParserErr::LexerErr(err)
    }
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

    fn expect_keyword(&mut self, keyword: Keyword) -> Result<(), ParserErr> {
        let current = self.current().ok_or(ParserErr::UnexpectedEOF)?;
        match current {
            Token::Keyword(k) if *k == keyword => {
                self.position += 1;
                Ok(())
            }
            token => Err(ParserErr::UnexpectedToken(token.clone())),
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParserErr> {
        let current = self.current().ok_or(ParserErr::UnexpectedEOF)?;
        match current {
            Token::Identifier(name) => {
                let name = name.to_string();
                self.position += 1;
                Ok(name)
            }
            token => Err(ParserErr::UnexpectedToken(token.clone())),
        }
    }

    fn expect_token(&mut self, token: Token) -> Result<(), ParserErr> {
        let current = self.current().ok_or(ParserErr::UnexpectedEOF)?;
        if *current == token {
            self.position += 1;
            Ok(())
        } else {
            Err(ParserErr::UnexpectedToken(current.clone()))
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

    fn parse_program(&mut self, output: &mut Program) -> Result<(), ParserErr> {
        while let Some(token) = self.current() {
            match token {
                Token::Keyword(Keyword::Fn) => output.fns.push(self.parse_function(false)?),
                Token::Keyword(Keyword::Struct) => output.structs.push(self.parse_struct()?),
                Token::Keyword(Keyword::Enum) => output.enums.push(self.parse_enum()?),
                Token::Keyword(Keyword::Impl) => output.impls.push(self.parse_impl()?),
                Token::Keyword(Keyword::Trait) => output.traits.push(self.parse_trait()?),
                token => return Err(ParserErr::UnexpectedToken(token.clone())),
            }
        }

        Ok(())
    }

    fn parse_function(&mut self, allow_receiver_param: bool) -> Result<Fn, ParserErr> {
        self.expect_keyword(Keyword::Fn)?;
        let name = self.expect_identifier()?;

        let gen_params = self.parse_gen_params()?;

        self.expect_token(Token::LParen)?;
        let (params, var_args) = self.parse_fn_params(allow_receiver_param)?;
        self.expect_token(Token::RParen)?;

        let return_ty = self.parse_function_return_type()?;

        let constraints = if self.advance_if(Token::Keyword(Keyword::Where)) {
            self.parse_function_constraints()?
        } else {
            Vec::new()
        };

        let body = if self.current() == Some(&Token::LBrace) {
            Some(self.parse_block()?)
        } else {
            self.expect_token(Token::Semicolon)?;
            None
        };

        Ok(Fn {
            name,
            gen_params,
            params,
            var_args,
            return_ty,
            constraints,
            body,
        })
    }

    fn parse_fn_params(&mut self, allow_receiver: bool) -> Result<(Vec<Param>, bool), ParserErr> {
        let mut params = Vec::new();
        let mut first = true;
        while !matches!(self.current(), Some(&Token::RParen | &Token::Dots)) {
            params.push(self.parse_fn_param(first && allow_receiver)?);
            first = false;

            if !self.advance_if(Token::Comma) {
                return Ok((params, false));
            }
        }

        let var_args = self.advance_if(Token::Dots);
        Ok((params, var_args))
    }

    fn parse_fn_param(&mut self, allow_receiver: bool) -> Result<Param, ParserErr> {
        if self.current() == Some(&Token::Keyword(Keyword::Self_)) {
            self.position += 1;
            if !allow_receiver {
                return Err(ParserErr::UnexpectedReceiverArg);
            }
            Ok(Param::Receiver)
        } else if self.current() == Some(&Token::Ampersand) {
            self.position += 1;
            self.expect_token(Token::Keyword(Keyword::Self_))?;
            if !allow_receiver {
                return Err(ParserErr::UnexpectedReceiverArg);
            }
            Ok(Param::ReceiverByRef)
        } else {
            let name = self.expect_identifier()?;
            self.expect_token(Token::Colon)?;
            let ty = self.parse_ty_annot()?;
            Ok(Param::Regular { name, ty })
        }
    }

    fn parse_function_return_type(&mut self) -> Result<Option<TyAnnot>, ParserErr> {
        if self.advance_if(Token::Arrow) {
            let return_type = self.parse_ty_annot()?;
            Ok(Some(return_type))
        } else {
            Ok(None)
        }
    }

    fn parse_function_constraints(&mut self) -> Result<Vec<Constraint>, ParserErr> {
        let mut constraints = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            constraints.push(self.parse_constraint()?);
            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(constraints)
    }

    fn parse_constraint(&mut self) -> Result<Constraint, ParserErr> {
        let subject = self.expect_identifier()?;
        self.expect_token(Token::Colon)?;
        let requirement = self.parse_constraint_requirement()?;
        Ok(Constraint { subject, requirement })
    }

    fn parse_constraint_requirement(&mut self) -> Result<ConstraintRequirement, ParserErr> {
        let requirement = match self.current() {
            Some(Token::Keyword(Keyword::FnTrait)) => {
                self.position += 1;
                self.expect_token(Token::LParen)?;

                let mut params = Vec::new();
                while self.current() != Some(&Token::RParen) {
                    let param = self.parse_ty_annot()?;
                    params.push(param);
                    if !self.advance_if(Token::Comma) {
                        break;
                    }
                }
                self.expect_token(Token::RParen)?;

                let return_ty = if self.advance_if(Token::Arrow) {
                    Some(self.parse_ty_annot()?)
                } else {
                    None
                };

                ConstraintRequirement::Callable { params, return_ty }
            }
            Some(Token::Identifier(trait_)) => {
                let trait_name = trait_.clone();
                self.position += 1;

                let trait_args = if self.advance_if(Token::Smaller) {
                    let mut gen_args = Vec::new();
                    while self.current() != Some(&Token::Greater) {
                        gen_args.push(self.parse_ty_annot()?);
                        if !self.advance_if(Token::Comma) {
                            break;
                        }
                    }
                    self.expect_token(Token::Greater)?;
                    gen_args
                } else {
                    vec![]
                };

                ConstraintRequirement::Trait { trait_name, trait_args }
            }
            _ => return Err(ParserErr::UnexpectedToken(self.current().unwrap().clone())),
        };
        Ok(requirement)
    }

    fn parse_struct(&mut self) -> Result<Struct, ParserErr> {
        self.expect_keyword(Keyword::Struct)?;
        let name = self.expect_identifier()?;
        let gen_params = self.parse_gen_params()?;

        self.expect_token(Token::LBrace)?;
        let fields = self.parse_struct_fields()?;
        self.expect_token(Token::RBrace)?;

        Ok(Struct {
            name,
            gen_params,
            fields,
        })
    }

    fn parse_struct_fields(&mut self) -> Result<Vec<StructField>, ParserErr> {
        let mut fields = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            fields.push(self.parse_struct_field()?);

            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(fields)
    }

    fn parse_struct_field(&mut self) -> Result<StructField, ParserErr> {
        let name = self.expect_identifier()?;
        self.expect_token(Token::Colon)?;
        let ty = self.parse_ty_annot()?;
        Ok(StructField { name, ty })
    }

    fn parse_enum(&mut self) -> Result<Enum, ParserErr> {
        self.expect_keyword(Keyword::Enum)?;
        let name = self.expect_identifier()?;
        let gen_params = self.parse_gen_params()?;

        self.expect_token(Token::LBrace)?;
        let variants = self.parse_enum_variants()?;
        self.expect_token(Token::RBrace)?;

        Ok(Enum {
            name,
            gen_params,
            variants,
        })
    }

    fn parse_enum_variants(&mut self) -> Result<Vec<EnumVariant>, ParserErr> {
        let mut variants = Vec::new();
        while let Some(Token::Identifier(_)) = self.current() {
            variants.push(self.parse_enum_variant()?);

            if !self.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(variants)
    }

    fn parse_enum_variant(&mut self) -> Result<EnumVariant, ParserErr> {
        let name = self.expect_identifier()?;
        let fields = if self.advance_if(Token::LBrace) {
            let fields = self.parse_struct_fields()?;
            self.expect_token(Token::RBrace)?;
            fields
        } else {
            vec![]
        };
        Ok(EnumVariant { name, fields })
    }

    fn parse_impl(&mut self) -> Result<Impl, ParserErr> {
        self.expect_keyword(Keyword::Impl)?;
        let gen_params = self.parse_gen_params()?;
        let ty = self.parse_ty_annot()?;

        let (trait_name, trait_args, ty) = if self.advance_if(Token::Keyword(Keyword::For)) {
            let ty2 = self.parse_ty_annot()?;
            match ty {
                TyAnnot::Named(trait_name) => (Some(trait_name), vec![], ty2),
                TyAnnot::Generic(Ident {
                    ident: trait_name,
                    gen_args: trait_args,
                }) => (Some(trait_name), trait_args, ty2),
                _ => return Err(ParserErr::ExpectedTraitName),
            }
        } else {
            (None, vec![], ty)
        };

        self.expect_token(Token::LBrace)?;

        let mut mthds = Vec::new();
        while let Some(Token::Keyword(Keyword::Fn)) = self.current() {
            mthds.push(self.parse_function(true)?);
        }

        self.expect_token(Token::RBrace)?;
        Ok(Impl {
            gen_params,
            trait_name,
            trait_args,
            ty,
            mthds,
        })
    }

    fn parse_trait(&mut self) -> Result<Trait, ParserErr> {
        self.expect_keyword(Keyword::Trait)?;
        let name = self.expect_identifier()?;

        let gen_params = self.parse_gen_params()?;

        self.expect_token(Token::LBrace)?;
        let mut mthds = Vec::new();
        while let Some(Token::Keyword(Keyword::Fn)) = self.current() {
            let mthd = self.parse_function(true)?;
            if mthd.body.is_some() {
                return Err(ParserErr::TraitMthdWithBody);
            }
            mthds.push(mthd);
        }
        self.expect_token(Token::RBrace)?;

        Ok(Trait {
            name,
            gen_params,
            mthds,
        })
    }

    fn parse_gen_params(&mut self) -> Result<Vec<String>, ParserErr> {
        if self.current() != Some(&Token::Smaller) {
            return Ok(Vec::new());
        }
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

    /// TODO allow loop/if/block expression statements without delimiting semicolon
    fn parse_block(&mut self) -> Result<Block, ParserErr> {
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
                    return Err(ParserErr::UndelimitedStmt);
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

    fn parse_stmt(&mut self) -> Result<(Stmt, StmtKind), ParserErr> {
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

    fn parse_let_stmt(&mut self) -> Result<Stmt, ParserErr> {
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

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParserErr> {
        self.expect_keyword(Keyword::Return)?;
        if let Some(Token::Semicolon) = self.current() {
            Ok(Stmt::Return(None))
        } else {
            let expr = self.parse_expr(true)?;
            Ok(Stmt::Return(Some(expr)))
        }
    }

    fn parse_break_stmt(&mut self) -> Result<Stmt, ParserErr> {
        self.expect_keyword(Keyword::Break)?;
        Ok(Stmt::Break)
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParserErr> {
        let expr = self.parse_expr(true)?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserErr> {
        self.parse_assign_expr(allow_top_level_struct_expr)
    }

    fn parse_assign_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserErr> {
        let target = self.parse_conversion_expr(allow_top_level_struct_expr)?;
        if self.advance_if(Token::Equal) {
            let value = self.parse_conversion_expr(allow_top_level_struct_expr)?;
            Ok(Expr::Assign {
                target: Box::new(target),
                value: Box::new(value),
            })
        } else {
            Ok(target)
        }
    }

    fn parse_conversion_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserErr> {
        let mut expr = self.parse_disjunction(allow_top_level_struct_expr)?;
        while self.advance_if(Token::Keyword(Keyword::As)) {
            let ty_annot = self.parse_ty_annot()?;
            expr = Expr::As {
                expr: Box::new(expr),
                target_ty: ty_annot,
            };
        }
        Ok(expr)
    }

    parse_left_associative!(parse_disjunction, parse_conjunction, [
        Token::Pipe => BinaryOperator::BitOr,
        Token::PipePipe => BinaryOperator::LogicalOr
    ]);

    parse_left_associative!(parse_conjunction, parse_equality_expr, [
        Token::Ampersand => BinaryOperator::BitAnd,
        Token::AmpersandAmpersand => BinaryOperator::LogicalAnd
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

    fn parse_unary_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserErr> {
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

    fn parse_function_call_and_field_access(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserErr> {
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
                // field access or method call
                if let Some(Token::NumLiteral(n)) = self.current() {
                    let index = n.parse().unwrap();
                    self.position += 1;
                    // indexed field access
                    acc = Expr::FieldAccess {
                        obj: Box::new(acc),
                        field: FieldDescriptor::Indexed(index),
                    };
                } else {
                    let member = self.parse_ident()?;
                    if self.advance_if(Token::LParen) {
                        // method call
                        let mut arguments = Vec::new();
                        while self.current() != Some(&Token::RParen) {
                            let argument = self.parse_expr(true)?; // Allow top-level struct expression in argument
                            arguments.push(argument);
                            if !self.advance_if(Token::Comma) {
                                break;
                            }
                        }
                        self.expect_token(Token::RParen)?;
                        acc = Expr::MthdCall {
                            obj: Box::new(acc),
                            mthd: member,
                            arguments,
                        };
                    } else {
                        // named field access
                        acc = Expr::FieldAccess {
                            obj: Box::new(acc),
                            field: FieldDescriptor::Named(member),
                        };
                    }
                }
            } else {
                break;
            }
        }

        Ok(acc)
    }

    fn parse_primary_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr, ParserErr> {
        let current = self.current().ok_or(ParserErr::UnexpectedEOF)?;

        match current {
            Token::NumLiteral(value) => {
                let value = value.parse().map_err(|_| ParserErr::InvalidLiteral)?;
                self.position += 1;
                Ok(Expr::Lit(Lit::Int(value)))
            }
            Token::BoolLiteral(b) => {
                let value = *b;
                self.position += 1;
                Ok(Expr::Lit(Lit::Bool(value)))
            }
            Token::CCharLiteral(c) => {
                let value = *c;
                self.position += 1;
                Ok(Expr::Lit(Lit::CChar(value)))
            }
            Token::CStringLiteral(s) => {
                let value = s.clone();
                self.position += 1;
                Ok(Expr::Lit(Lit::CString(value)))
            }
            Token::Identifier(..) => {
                let ident = self.parse_ident()?;

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
                } else {
                    Ok(Expr::Ident(ident))
                }
            }
            Token::LParen => {
                self.position += 1;

                let mut inner_exprs = Vec::new();
                let mut trailing_comma = true;
                while self.current() != Some(&Token::RParen) {
                    let inner_expr = self.parse_expr(true)?; // Allow top-level struct expression
                    inner_exprs.push(inner_expr);
                    if !self.advance_if(Token::Comma) {
                        trailing_comma = false;
                        break;
                    }
                }
                self.expect_token(Token::RParen)?;

                if inner_exprs.len() == 1 && !trailing_comma {
                    Ok(inner_exprs.remove(0))
                } else {
                    Ok(Expr::Tuple(inner_exprs))
                }
            }
            Token::LBrace => {
                let block = self.parse_block()?;
                Ok(Expr::Block(block))
            }
            Token::Keyword(Keyword::If) => self.parse_if_expr(),
            Token::Keyword(Keyword::Loop) => self.parse_loop(),
            Token::Keyword(Keyword::Match) => self.parse_match(),
            Token::Keyword(Keyword::Self_) => {
                self.position += 1;
                Ok(Expr::Self_)
            }
            Token::Pipe => {
                self.position += 1;

                let mut params = Vec::new();
                while self.current() != Some(&Token::Pipe) {
                    let param = self.parse_closure_param()?;
                    params.push(param);
                    if !self.advance_if(Token::Comma) {
                        break;
                    }
                }
                self.expect_token(Token::Pipe)?;

                let return_ty = if self.advance_if(Token::Arrow) {
                    Some(self.parse_ty_annot()?)
                } else {
                    None
                };

                let body = self.parse_closure_body(return_ty.is_some())?;

                Ok(Expr::Closure {
                    params,
                    return_ty,
                    body,
                })
            }
            Token::PipePipe => {
                self.position += 1;

                let return_ty = if self.advance_if(Token::Arrow) {
                    Some(self.parse_ty_annot()?)
                } else {
                    None
                };

                let body = self.parse_closure_body(return_ty.is_some())?;

                Ok(Expr::Closure {
                    params: vec![],
                    return_ty,
                    body,
                })
            }

            token => Err(ParserErr::UnexpectedToken(token.clone())),
        }
    }

    fn parse_closure_param(&mut self) -> Result<ClosureParam, ParserErr> {
        let name = self.expect_identifier()?;
        let ty = if self.advance_if(Token::Colon) {
            Some(self.parse_ty_annot()?)
        } else {
            None
        };
        Ok(ClosureParam { name, ty })
    }

    fn parse_closure_body(&mut self, force_block: bool) -> Result<Block, ParserErr> {
        if self.current() == Some(&Token::LBrace) || force_block {
            self.parse_block()
        } else {
            let return_expr = self.parse_expr(true)?;
            let block = Block {
                return_expr: Some(Box::new(return_expr)),
                stmts: Vec::new(),
            };
            Ok(block)
        }
    }

    fn parse_ident(&mut self) -> Result<Ident, ParserErr> {
        let ident = self.expect_identifier()?;

        let gen_args = if self.advance_if(Token::ColonColon) {
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
            gen_args
        } else {
            vec![]
        };

        let ident = Ident { ident, gen_args };
        Ok(ident)
    }

    fn parse_if_expr(&mut self) -> Result<Expr, ParserErr> {
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

    fn parse_loop(&mut self) -> Result<Expr, ParserErr> {
        self.expect_keyword(Keyword::Loop)?;
        let body = self.parse_block()?;
        Ok(Expr::Loop { body })
    }

    fn parse_match(&mut self) -> Result<Expr, ParserErr> {
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

    fn parse_struct_pattern(&mut self) -> Result<StructPattern, ParserErr> {
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

    fn parse_ty_annot(&mut self) -> Result<TyAnnot, ParserErr> {
        let current = self.current().ok_or(ParserErr::UnexpectedEOF)?;
        match current {
            Token::Identifier(ident) => {
                let ident = ident.clone();
                self.position += 1;
                if self.advance_if(Token::Smaller) {
                    let mut gen_args = Vec::new();
                    while self.current() != Some(&Token::Greater) {
                        let gen_arg = self.parse_ty_annot()?;
                        gen_args.push(gen_arg);
                        if !self.advance_if(Token::Comma) {
                            break;
                        }
                    }
                    self.expect_token(Token::Greater)?;
                    Ok(TyAnnot::Generic(Ident { ident, gen_args }))
                } else {
                    Ok(TyAnnot::Named(ident))
                }
            }
            Token::Ampersand => {
                self.position += 1;
                let inner_ty = self.parse_ty_annot()?;
                Ok(TyAnnot::Ref(Box::new(inner_ty)))
            }
            Token::AmpersandAmpersand => {
                // Two levels of reference
                self.position += 1;
                let inner_ty = self.parse_ty_annot()?;
                Ok(TyAnnot::Ref(Box::new(TyAnnot::Ref(Box::new(inner_ty)))))
            }
            Token::Asterisk => {
                self.position += 1;
                let inner_ty = self.parse_ty_annot()?;
                Ok(TyAnnot::Ptr(Box::new(inner_ty)))
            }
            Token::LParen => {
                self.position += 1;

                let mut inner_tys = Vec::new();
                let mut trailing_comma = true;
                while self.current() != Some(&Token::RParen) {
                    let inner_ty = self.parse_ty_annot()?;
                    inner_tys.push(inner_ty);
                    if !self.advance_if(Token::Comma) {
                        trailing_comma = false;
                        break;
                    }
                }
                self.expect_token(Token::RParen)?;

                if inner_tys.len() == 1 && !trailing_comma {
                    Ok(inner_tys.remove(0))
                } else {
                    Ok(TyAnnot::Tuple(inner_tys))
                }
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
            Token::Underscore => {
                self.position += 1;
                Ok(TyAnnot::Wildcard)
            }
            Token::Keyword(Keyword::SelfTy) => {
                self.position += 1;
                Ok(TyAnnot::Self_)
            }
            token => Err(ParserErr::UnexpectedToken(token.clone())),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_ident(name: &str) -> Expr {
        Expr::Ident(Ident {
            ident: name.to_string(),
            gen_args: vec![],
        })
    }

    mod program {
        use super::*;
        use pretty_assertions::assert_eq;

        fn parse_and_compare(input: &str, expected: Program) {
            let mut program = Program::default();
            parse(input, &mut program).expect("Failed to parse HLR");
            assert_eq!(program, expected);
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

                impl Empty {
                }

                trait Empty {
                }
                "#;

            let expected = Program {
                fns: vec![Fn {
                    name: "empty".to_string(),
                    gen_params: vec![],
                    params: vec![],
                    var_args: false,
                    return_ty: None,
                    constraints: vec![],
                    body: Some(Block {
                        stmts: vec![],
                        return_expr: None,
                    }),
                }],
                structs: vec![Struct {
                    name: "Empty".to_string(),
                    gen_params: vec![],
                    fields: vec![],
                }],
                enums: vec![Enum {
                    name: "Empty".to_string(),
                    gen_params: vec![],
                    variants: vec![],
                }],
                impls: vec![Impl {
                    gen_params: vec![],
                    ty: TyAnnot::Named("Empty".to_string()),
                    trait_name: None,
                    trait_args: vec![],
                    mthds: vec![],
                }],
                traits: vec![Trait {
                    name: "Empty".to_string(),
                    gen_params: vec![],
                    mthds: vec![],
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
                        Param::Regular {
                            name: "a".to_string(),
                            ty: TyAnnot::Named("int".to_string()),
                        },
                        Param::Regular {
                            name: "b".to_string(),
                            ty: TyAnnot::Named("int".to_string()),
                        },
                    ],
                    var_args: false,
                    return_ty: Some(TyAnnot::Named("int".to_string())),
                    constraints: vec![],
                    body: Some(Block {
                        stmts: vec![Stmt::Return(Some(Expr::BinaryOp {
                            left: Box::new(make_ident("a")),
                            operator: BinaryOperator::Add,
                            right: Box::new(make_ident("b")),
                        }))],
                        return_expr: None,
                    }),
                }],
                ..Default::default()
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
                structs: vec![Struct {
                    name: "Point".to_string(),
                    gen_params: vec![],
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
                ..Default::default()
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_simple_enum() {
            let input = r#"
                enum State {
                    Off {},
                    On {},
                    Unknown,
                }"#;

            let expected = Program {
                enums: vec![Enum {
                    name: "State".to_string(),
                    gen_params: vec![],
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
                ..Default::default()
            };

            parse_and_compare(input, expected);
        }

        #[test]
        fn test_parse_simple_impl() {
            let input = r#"
                impl A {
                    fn get_b(self) -> B {
                        self.b
                    }
                }"#;

            let expected = Program {
                impls: vec![Impl {
                    gen_params: vec![],
                    ty: TyAnnot::Named("A".to_string()),
                    trait_name: None,
                    trait_args: vec![],
                    mthds: vec![Fn {
                        name: "get_b".to_string(),
                        gen_params: vec![],
                        params: vec![Param::Receiver],
                        var_args: false,
                        return_ty: Some(TyAnnot::Named("B".to_string())),
                        constraints: vec![],
                        body: Some(Block {
                            stmts: vec![],
                            return_expr: Some(Box::new(Expr::FieldAccess {
                                obj: Box::new(Expr::Self_),
                                field: FieldDescriptor::Named(Ident {
                                    ident: "b".to_string(),
                                    gen_args: vec![],
                                }),
                            })),
                        }),
                    }],
                }],
                ..Default::default()
            };

            parse_and_compare(input, expected);
        }
    }

    mod block {
        use super::*;
        use pretty_assertions::assert_eq;

        fn parse_and_compare(input: &str, expected: Block) {
            let parsed = parse_block(input).expect("Failed to parse HLR");
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
                                        left: Box::new(make_ident("n")),
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
                                    target: Box::new(make_ident("result")),
                                    value: Box::new(Expr::BinaryOp {
                                        left: Box::new(make_ident("result")),
                                        operator: BinaryOperator::Multiply,
                                        right: Box::new(make_ident("n")),
                                    }),
                                }),
                                Stmt::Expr(Expr::Assign {
                                    target: Box::new(make_ident("n")),
                                    value: Box::new(Expr::BinaryOp {
                                        left: Box::new(make_ident("n")),
                                        operator: BinaryOperator::Subtract,
                                        right: Box::new(Expr::Lit(Lit::Int(1))),
                                    }),
                                }),
                            ],
                            return_expr: None,
                        },
                    }),
                ],
                return_expr: Some(Box::new(make_ident("result"))),
            };

            parse_and_compare(input, expected);
        }
    }

    mod expr {
        use super::*;
        use pretty_assertions::assert_eq;

        fn parse_and_compare(input: &str, expected: Expr) {
            let parsed = parse_expr(input).expect("Failed to parse HLR");
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
                        right: Box::new(make_ident("a")),
                    }),
                }),
                operator: BinaryOperator::Subtract,
                right: Box::new(Expr::Call {
                    callee: Box::new(make_ident("arctan2")),
                    arguments: vec![make_ident("x"), make_ident("y")],
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
                name: Ident {
                    ident: "Circle".to_string(),
                    gen_args: vec![],
                },
                fields: vec![
                    (
                        "center".to_string(),
                        Expr::Struct {
                            name: Ident {
                                ident: "Point".to_string(),
                                gen_args: vec![],
                            },
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
                condition: Box::new(make_ident("condition")),
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
        fn test_parse_calls_and_field_access() {
            let input = "(function(arg0).method(arg1, arg2).field)(arg3)";
            let expected = Expr::Call {
                callee: Box::new(Expr::FieldAccess {
                    obj: Box::new(Expr::MthdCall {
                        obj: Box::new(Expr::Call {
                            callee: Box::new(make_ident("function")),
                            arguments: vec![make_ident("arg0")],
                        }),
                        mthd: Ident {
                            ident: "method".to_string(),
                            gen_args: vec![],
                        },
                        arguments: vec![make_ident("arg1"), make_ident("arg2")],
                    }),
                    field: FieldDescriptor::Named(Ident {
                        ident: "field".to_string(),
                        gen_args: vec![],
                    }),
                }),
                arguments: vec![make_ident("arg3")],
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
                scrutinee: Box::new(make_ident("value")),
                arms: vec![
                    MatchArm {
                        pattern: StructPattern {
                            variant: "Some".to_string(),
                            fields: vec![StructPatternField {
                                field_name: "inner".to_string(),
                                binding_name: "a".to_string(),
                            }],
                        },
                        value: Box::new(make_ident("a")),
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
