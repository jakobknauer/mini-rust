mod lexer;
mod token;
mod token_stream;

#[macro_use]
mod macros;

use crate::{ast::builder::AstBuilder, ast::*, parse::token::Keyword};

pub use lexer::LexerErr;
pub use token::Token;

pub fn parse<'ast>(input: &str, output: &'ast Ast<'ast>) -> Result<(), ParserErr> {
    let tokens = lexer::get_tokens(input)?;
    let mut parser = AstParser::new(&tokens[..], output);
    parser.parse()
}

#[derive(Debug)]
pub enum ParserErr {
    LexerErr(LexerErr),
    UnexpectedToken(Token),
    UndelimitedStmt,
    InvalidLiteral,
    UnexpectedEOF,

    ExpectedTraitName,
    UnexpectedReceiverArg,
}

impl From<LexerErr> for ParserErr {
    fn from(err: LexerErr) -> Self {
        ParserErr::LexerErr(err)
    }
}

struct AstParser<'ast, 'token> {
    tokens: token_stream::TokenStream<'token>,
    builder: AstBuilder<'ast>,
}

#[derive(PartialEq, Eq)]
enum StmtType {
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

impl<'ast, 'token> AstParser<'ast, 'token> {
    fn new(input: &'token [Token], ast: &'ast Ast<'ast>) -> Self {
        AstParser {
            tokens: token_stream::TokenStream::new(input),
            builder: AstBuilder::new(ast),
        }
    }

    fn parse(&mut self) -> Result<(), ParserErr> {
        while let Some(token) = self.tokens.current() {
            match token {
                Token::Keyword(Keyword::Fn) => {
                    let fn_ = self.parse_function(true)?;
                    self.builder.add_free_fn(fn_);
                }
                Token::Keyword(Keyword::Struct) => {
                    let struct_ = self.parse_struct()?;
                    self.builder.add_struct(struct_);
                }
                Token::Keyword(Keyword::Enum) => {
                    let enum_ = self.parse_enum()?;
                    self.builder.add_enum(enum_);
                }
                Token::Keyword(Keyword::Impl) => {
                    let impl_ = self.parse_impl()?;
                    self.builder.add_impl(impl_);
                }
                Token::Keyword(Keyword::Trait) => {
                    let trait_ = self.parse_trait()?;
                    self.builder.add_trait(trait_);
                }
                token => return Err(ParserErr::UnexpectedToken(token.clone())),
            }
        }

        Ok(())
    }

    fn parse_function(&mut self, allow_receiver_param: bool) -> Result<Fn<'ast>, ParserErr> {
        self.tokens.expect_keyword(Keyword::Fn)?;
        let name = self.tokens.expect_identifier()?;

        let gen_params = self.parse_gen_params()?;

        self.tokens.expect_token(Token::LParen)?;
        let (params, var_args) = self.parse_fn_params(allow_receiver_param)?;
        self.tokens.expect_token(Token::RParen)?;

        let return_ty = self.parse_function_return_type()?;

        let constraints = if self.tokens.advance_if(Token::Keyword(Keyword::Where)) {
            self.parse_generic_constraints()?
        } else {
            Vec::new()
        };

        let body = if self.tokens.current() == Some(&Token::LBrace) {
            Some(self.parse_block()?)
        } else {
            self.tokens.expect_token(Token::Semicolon)?;
            None
        };

        let fn_def = FnDef {
            name,
            gen_params,
            params,
            var_args,
            return_ty,
            constraints,
            body,
        };
        Ok(self.builder.add_fn(fn_def))
    }

    fn parse_fn_params(&mut self, allow_receiver: bool) -> Result<(Vec<Param<'ast>>, bool), ParserErr> {
        let mut params = Vec::new();
        let mut first = true;
        while !matches!(self.tokens.current(), Some(&Token::RParen | &Token::Dots)) {
            params.push(self.parse_fn_param(first && allow_receiver)?);
            first = false;

            if !self.tokens.advance_if(Token::Comma) {
                return Ok((params, false));
            }
        }

        let var_args = self.tokens.advance_if(Token::Dots);
        Ok((params, var_args))
    }

    fn parse_fn_param(&mut self, allow_receiver: bool) -> Result<Param<'ast>, ParserErr> {
        if self.tokens.advance_if_keyword(Keyword::Self_) {
            if !allow_receiver {
                return Err(ParserErr::UnexpectedReceiverArg);
            }
            Ok(Param::Receiver)
        } else if self.tokens.advance_if(Token::Ampersand) {
            let mutable = self.tokens.advance_if_keyword(Keyword::Mut);
            self.tokens.expect_token(Token::Keyword(Keyword::Self_))?;
            if !allow_receiver {
                return Err(ParserErr::UnexpectedReceiverArg);
            }
            if mutable {
                Ok(Param::ReceiverByRefMut)
            } else {
                Ok(Param::ReceiverByRef)
            }
        } else {
            let mutable = self.tokens.advance_if_keyword(Keyword::Mut);
            if mutable && self.tokens.advance_if_keyword(Keyword::Self_) {
                if !allow_receiver {
                    return Err(ParserErr::UnexpectedReceiverArg);
                }
                Ok(Param::ReceiverMut)
            } else {
                let name = self.tokens.expect_identifier()?;
                self.tokens.expect_token(Token::Colon)?;
                let ty = self.parse_ty_annot()?;
                Ok(Param::Regular { name, ty, mutable })
            }
        }
    }

    fn parse_function_return_type(&mut self) -> Result<Option<TyAnnot<'ast>>, ParserErr> {
        if self.tokens.advance_if(Token::Arrow) {
            let return_type = self.parse_ty_annot()?;
            Ok(Some(return_type))
        } else {
            Ok(None)
        }
    }

    fn parse_generic_constraints(&mut self) -> Result<Vec<Constraint<'ast>>, ParserErr> {
        let mut constraints = Vec::new();
        while let Some(Token::Identifier(_)) = self.tokens.current() {
            constraints.push(self.parse_constraint()?);
            if !self.tokens.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(constraints)
    }

    fn parse_constraint(&mut self) -> Result<Constraint<'ast>, ParserErr> {
        let subject = self.parse_ty_annot()?;
        self.tokens.expect_token(Token::Colon)?;
        let requirement = self.parse_constraint_requirement()?;
        Ok(Constraint { subject, requirement })
    }

    fn parse_constraint_requirement(&mut self) -> Result<ConstraintRequirement<'ast>, ParserErr> {
        let requirement = match self.tokens.current() {
            Some(Token::Keyword(Keyword::FnTrait)) => {
                self.tokens.advance();
                self.tokens.expect_token(Token::LParen)?;

                let mut params = Vec::new();
                while self.tokens.current() != Some(&Token::RParen) {
                    let param = self.parse_ty_annot()?;
                    params.push(param);
                    if !self.tokens.advance_if(Token::Comma) {
                        break;
                    }
                }
                self.tokens.expect_token(Token::RParen)?;
                let params = self.builder.ty_annot_slice(&params);

                let return_ty = if self.tokens.advance_if(Token::Arrow) {
                    Some(self.parse_ty_annot()?)
                } else {
                    None
                };

                ConstraintRequirement::Callable { params, return_ty }
            }
            Some(Token::Identifier(trait_)) => {
                let trait_name = trait_.clone();
                self.tokens.advance();

                let mut trait_args = Vec::new();
                let mut assoc_bindings = Vec::new();
                if self.tokens.advance_if(Token::Smaller) {
                    while self.tokens.current() != Some(&Token::Greater) {
                        if let Some(Token::Identifier(_)) = self.tokens.current() {
                            if self.tokens.next() == Some(&Token::Equal) {
                                let name = self.tokens.expect_identifier()?;
                                self.tokens.expect_token(Token::Equal)?;
                                let ty = self.parse_ty_annot()?;
                                assoc_bindings.push(AssocBinding::Eq { name, ty });
                                if !self.tokens.advance_if(Token::Comma) {
                                    break;
                                }
                                continue;
                            }
                            if self.tokens.next() == Some(&Token::Colon) {
                                let name = self.tokens.expect_identifier()?;
                                self.tokens.expect_token(Token::Colon)?;
                                let requirement = self.parse_constraint_requirement()?;
                                assoc_bindings.push(AssocBinding::Bound { name, requirement });
                                if !self.tokens.advance_if(Token::Comma) {
                                    break;
                                }
                                continue;
                            }
                        }
                        trait_args.push(self.parse_ty_annot()?);
                        if !self.tokens.advance_if(Token::Comma) {
                            break;
                        }
                    }
                    self.tokens.expect_token(Token::Greater)?;
                }
                let trait_args = self.builder.ty_annot_slice(&trait_args);
                let assoc_bindings = self.builder.assoc_binding_slice(&assoc_bindings);

                ConstraintRequirement::Trait {
                    trait_name,
                    trait_args,
                    assoc_bindings,
                }
            }
            _ => return Err(ParserErr::UnexpectedToken(self.tokens.current().unwrap().clone())),
        };
        Ok(requirement)
    }

    fn parse_struct(&mut self) -> Result<StructDef<'ast>, ParserErr> {
        self.tokens.expect_keyword(Keyword::Struct)?;
        let name = self.tokens.expect_identifier()?;
        let gen_params = self.parse_gen_params()?;

        self.tokens.expect_token(Token::LBrace)?;
        let fields = self.parse_struct_fields()?;
        self.tokens.expect_token(Token::RBrace)?;

        Ok(StructDef {
            name,
            gen_params,
            fields,
        })
    }

    fn parse_struct_fields(&mut self) -> Result<Vec<StructField<'ast>>, ParserErr> {
        let mut fields = Vec::new();
        while let Some(Token::Identifier(_)) = self.tokens.current() {
            fields.push(self.parse_struct_field()?);

            if !self.tokens.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(fields)
    }

    fn parse_struct_field(&mut self) -> Result<StructField<'ast>, ParserErr> {
        let name = self.tokens.expect_identifier()?;
        self.tokens.expect_token(Token::Colon)?;
        let ty = self.parse_ty_annot()?;
        Ok(StructField { name, ty })
    }

    fn parse_enum(&mut self) -> Result<EnumDef<'ast>, ParserErr> {
        self.tokens.expect_keyword(Keyword::Enum)?;
        let name = self.tokens.expect_identifier()?;
        let gen_params = self.parse_gen_params()?;

        self.tokens.expect_token(Token::LBrace)?;
        let variants = self.parse_enum_variants()?;
        self.tokens.expect_token(Token::RBrace)?;

        Ok(EnumDef {
            name,
            gen_params,
            variants,
        })
    }

    fn parse_enum_variants(&mut self) -> Result<Vec<EnumVariant<'ast>>, ParserErr> {
        let mut variants = Vec::new();
        while let Some(Token::Identifier(_)) = self.tokens.current() {
            variants.push(self.parse_enum_variant()?);

            if !self.tokens.advance_if(Token::Comma) {
                break;
            }
        }
        Ok(variants)
    }

    fn parse_enum_variant(&mut self) -> Result<EnumVariant<'ast>, ParserErr> {
        let name = self.tokens.expect_identifier()?;
        let fields = if self.tokens.advance_if(Token::LBrace) {
            let fields = self.parse_struct_fields()?;
            self.tokens.expect_token(Token::RBrace)?;
            fields
        } else {
            vec![]
        };
        Ok(EnumVariant { name, fields })
    }

    fn parse_impl(&mut self) -> Result<ImplDef<'ast>, ParserErr> {
        self.tokens.expect_keyword(Keyword::Impl)?;
        let gen_params = self.parse_gen_params()?;
        let ty = self.parse_ty_annot()?;

        let (trait_annot, ty) = if self.tokens.advance_if(Token::Keyword(Keyword::For)) {
            let ty2 = self.parse_ty_annot()?;
            match ty {
                TyAnnotKind::Path(path) => match path.segments.as_slice() {
                    [segment] if !segment.is_self => (
                        Some(TraitAnnot {
                            name: segment.ident.clone(),
                            args: segment.args,
                        }),
                        ty2,
                    ),
                    _ => return Err(ParserErr::ExpectedTraitName),
                },
                _ => return Err(ParserErr::ExpectedTraitName),
            }
        } else {
            (None, ty)
        };

        let constraints = if self.tokens.advance_if(Token::Keyword(Keyword::Where)) {
            self.parse_generic_constraints()?
        } else {
            Vec::new()
        };

        let mut mthds = Vec::new();
        let mut assoc_tys = Vec::new();

        self.tokens.expect_token(Token::LBrace)?;
        loop {
            match self.tokens.current() {
                Some(Token::Keyword(Keyword::Type)) => {
                    self.tokens.advance();
                    let name = self.tokens.expect_identifier()?;
                    self.tokens.expect_token(Token::Equal)?;
                    let ty = self.parse_ty_annot()?;
                    self.tokens.expect_token(Token::Semicolon)?;

                    assoc_tys.push(AssocTy { name, ty });
                }
                Some(Token::Keyword(Keyword::Fn)) => {
                    let mthd = self.parse_function(true)?;
                    mthds.push(mthd);
                }
                Some(Token::RBrace) => break,
                Some(token) => return Err(ParserErr::UnexpectedToken(token.clone())),
                None => return Err(ParserErr::UnexpectedEOF),
            }
        }
        self.tokens.expect_token(Token::RBrace)?;

        Ok(ImplDef {
            gen_params,
            trait_annot,
            ty,
            constraints,
            mthds: self.builder.fn_slice(&mthds),
            assoc_tys,
        })
    }

    fn parse_trait(&mut self) -> Result<TraitDef<'ast>, ParserErr> {
        self.tokens.expect_keyword(Keyword::Trait)?;
        let name = self.tokens.expect_identifier()?;

        let gen_params = self.parse_gen_params()?;

        let mut mthds = Vec::new();
        let mut assoc_tys = Vec::new();

        self.tokens.expect_token(Token::LBrace)?;
        loop {
            match self.tokens.current() {
                Some(Token::Keyword(Keyword::Type)) => {
                    self.tokens.advance();
                    let name = self.tokens.expect_identifier()?;
                    let mut bounds = Vec::new();
                    if self.tokens.advance_if(Token::Colon) {
                        bounds.push(self.parse_constraint_requirement()?);
                        while self.tokens.advance_if(Token::Plus) {
                            bounds.push(self.parse_constraint_requirement()?);
                        }
                    }
                    self.tokens.expect_token(Token::Semicolon)?;
                    assoc_tys.push(AssocTyDef { name, bounds });
                }
                Some(Token::Keyword(Keyword::Fn)) => {
                    let mthd = self.parse_function(true)?;
                    mthds.push(mthd);
                }
                Some(Token::RBrace) => break,
                Some(token) => return Err(ParserErr::UnexpectedToken(token.clone())),
                None => return Err(ParserErr::UnexpectedEOF),
            }
        }
        self.tokens.expect_token(Token::RBrace)?;

        Ok(TraitDef {
            name,
            gen_params,
            mthds: self.builder.fn_slice(&mthds),
            assoc_tys,
        })
    }

    fn parse_gen_params(&mut self) -> Result<Vec<String>, ParserErr> {
        if self.tokens.current() != Some(&Token::Smaller) {
            return Ok(Vec::new());
        }
        self.tokens.expect_token(Token::Smaller)?;

        let mut params = Vec::new();
        while let Some(Token::Identifier(_)) = self.tokens.current() {
            let arg = self.tokens.expect_identifier()?;
            params.push(arg);

            if !self.tokens.advance_if(Token::Comma) {
                break;
            }
        }
        self.tokens.expect_token(Token::Greater)?;
        Ok(params)
    }

    fn parse_block(&mut self) -> Result<Block<'ast>, ParserErr> {
        let mut stmts = Vec::new();
        let mut return_expr = None;

        self.tokens.expect_token(Token::LBrace)?;

        loop {
            while self.tokens.advance_if(Token::Semicolon) {}

            if let Some(Token::RBrace) = self.tokens.current() {
                break;
            }

            let (stmt, stmt_kind) = self.parse_stmt()?;

            if stmt_kind == StmtType::UndelimitedExpr {
                // expect that we are at the end of the block
                if self.tokens.current() != Some(&Token::RBrace) {
                    return Err(ParserErr::UndelimitedStmt);
                }
            }

            if self.tokens.current() == Some(&Token::RBrace) {
                if stmt_kind == StmtType::UndelimitedExpr || stmt_kind == StmtType::BlockExpr {
                    let &StmtKind::Expr(expr) = stmt else {
                        unreachable!();
                    };
                    return_expr = Some(expr);
                } else {
                    stmts.push(stmt);
                }
                break;
            } else {
                stmts.push(stmt);
            }
        }

        self.tokens.expect_token(Token::RBrace)?;

        let stmts = self.builder.stmt_slice(&stmts);
        Ok(Block { stmts, return_expr })
    }

    fn parse_stmt(&mut self) -> Result<(Stmt<'ast>, StmtType), ParserErr> {
        // Block-like expressions (if, loop, match, bare blocks, etc.) must be parsed directly
        // via their specific parsers, NOT through parse_expr. If routed through parse_expr,
        // infix operators on the next line would be consumed as continuations. For example:
        //   if cond { ... }
        //   *ptr = val;        // `*` would be parsed as multiplication with the if expr
        match self.tokens.current() {
            Some(Token::Keyword(Keyword::Let)) => {
                let stmt = self.parse_let_stmt()?;
                self.tokens.expect_token(Token::Semicolon)?;
                Ok((stmt, StmtType::ExplicitelyDelimited))
            }
            Some(Token::Keyword(Keyword::Return)) => {
                let stmt = self.parse_return_stmt()?;
                self.tokens.expect_token(Token::Semicolon)?;
                Ok((stmt, StmtType::ExplicitelyDelimited))
            }
            Some(Token::Keyword(Keyword::Break)) => {
                let stmt = self.parse_break_stmt()?;
                self.tokens.expect_token(Token::Semicolon)?;
                Ok((stmt, StmtType::ExplicitelyDelimited))
            }
            Some(Token::LBrace) => {
                let block = self.parse_block()?;
                let expr = self.builder.block(block);
                let stmt = self.builder.expr_stmt(expr);
                Ok((stmt, StmtType::BlockExpr))
            }
            Some(Token::Keyword(Keyword::If)) => {
                let expr = self.parse_if_expr()?;
                let stmt = self.builder.expr_stmt(expr);
                Ok((stmt, StmtType::BlockExpr))
            }
            Some(Token::Keyword(Keyword::Loop)) => {
                let expr = self.parse_loop()?;
                let stmt = self.builder.expr_stmt(expr);
                Ok((stmt, StmtType::BlockExpr))
            }
            Some(Token::Keyword(Keyword::While)) => {
                let expr = self.parse_while()?;
                let stmt = self.builder.expr_stmt(expr);
                Ok((stmt, StmtType::BlockExpr))
            }
            Some(Token::Keyword(Keyword::For)) => {
                let expr = self.parse_for()?;
                let stmt = self.builder.expr_stmt(expr);
                Ok((stmt, StmtType::BlockExpr))
            }
            Some(Token::Keyword(Keyword::Match)) => {
                let expr = self.parse_match()?;
                let stmt = self.builder.expr_stmt(expr);
                Ok((stmt, StmtType::BlockExpr))
            }
            _ => {
                let stmt = self.parse_expr_stmt()?;
                let semicolon = self.tokens.advance_if(Token::Semicolon);
                let stmt_kind = if semicolon {
                    StmtType::ExplicitelyDelimited
                } else {
                    StmtType::UndelimitedExpr
                };
                Ok((stmt, stmt_kind))
            }
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt<'ast>, ParserErr> {
        self.tokens.expect_keyword(Keyword::Let)?;
        let mutable = self.tokens.advance_if_keyword(Keyword::Mut);
        let name = self.tokens.expect_identifier()?;
        let ty_annot = if self.tokens.advance_if(Token::Colon) {
            Some(self.parse_ty_annot()?)
        } else {
            None
        };
        self.tokens.expect_token(Token::Equal)?;
        let value = self.parse_expr(true)?;
        let stmt = self.builder.let_stmt(name, mutable, ty_annot, value);
        Ok(stmt)
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt<'ast>, ParserErr> {
        self.tokens.expect_keyword(Keyword::Return)?;
        let return_expr = if let Some(Token::Semicolon) = self.tokens.current() {
            None
        } else {
            let expr = self.parse_expr(true)?;
            Some(expr)
        };
        let stmt = self.builder.return_stmt(return_expr);
        Ok(stmt)
    }

    fn parse_break_stmt(&mut self) -> Result<Stmt<'ast>, ParserErr> {
        self.tokens.expect_keyword(Keyword::Break)?;
        let stmt = self.builder.break_stmt();
        Ok(stmt)
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt<'ast>, ParserErr> {
        let expr = self.parse_expr(true)?;
        let stmt = self.builder.expr_stmt(expr);
        Ok(stmt)
    }

    fn parse_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr<'ast>, ParserErr> {
        self.parse_assign_expr(allow_top_level_struct_expr)
    }

    fn parse_assign_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr<'ast>, ParserErr> {
        let target = self.parse_conversion_expr(allow_top_level_struct_expr)?;
        if self.tokens.advance_if(Token::Equal) {
            let value = self.parse_conversion_expr(allow_top_level_struct_expr)?;
            let expr = self.builder.assign(target, value);
            Ok(expr)
        } else {
            Ok(target)
        }
    }

    fn parse_conversion_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr<'ast>, ParserErr> {
        let mut expr = self.parse_disjunction(allow_top_level_struct_expr)?;
        while self.tokens.advance_if(Token::Keyword(Keyword::As)) {
            let ty_annot = self.parse_ty_annot()?;
            expr = self.builder.as_(expr, ty_annot);
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

    fn parse_unary_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr<'ast>, ParserErr> {
        match self.tokens.current() {
            Some(Token::Asterisk) => {
                self.tokens.advance();
                let base = self.parse_unary_expr(allow_top_level_struct_expr)?;
                Ok(self.builder.deref(base))
            }
            Some(Token::Ampersand) => {
                self.tokens.advance();
                let mutable = self.tokens.advance_if_keyword(Keyword::Mut);
                let base = self.parse_unary_expr(allow_top_level_struct_expr)?;
                if mutable {
                    Ok(self.builder.addr_of_mut(base))
                } else {
                    Ok(self.builder.addr_of(base))
                }
            }
            Some(Token::AmpersandAmpersand) => {
                self.tokens.advance();
                let mutable = self.tokens.advance_if_keyword(Keyword::Mut);
                let base = self.parse_unary_expr(allow_top_level_struct_expr)?;
                let inner = if mutable {
                    self.builder.addr_of_mut(base)
                } else {
                    self.builder.addr_of(base)
                };
                Ok(self.builder.addr_of(inner))
            }
            Some(Token::Bang) => {
                self.tokens.advance();
                let base = self.parse_unary_expr(allow_top_level_struct_expr)?;
                Ok(self.builder.unary_op(UnaryOperator::Not, base))
            }
            Some(Token::Minus) => {
                self.tokens.advance();
                let base = self.parse_unary_expr(allow_top_level_struct_expr)?;
                Ok(self.builder.unary_op(UnaryOperator::Negative, base))
            }
            _ => self.parse_function_call_and_member_access(allow_top_level_struct_expr),
        }
    }

    fn parse_function_call_and_member_access(
        &mut self,
        allow_top_level_struct_expr: bool,
    ) -> Result<Expr<'ast>, ParserErr> {
        let mut acc = self.parse_primary_expr(allow_top_level_struct_expr)?;

        loop {
            if self.tokens.advance_if(Token::LParen) {
                // function call
                let mut args = Vec::new();
                while self.tokens.current() != Some(&Token::RParen) {
                    let argument = self.parse_expr(true)?; // Allow top-level struct expression in argument
                    args.push(argument);
                    if !self.tokens.advance_if(Token::Comma) {
                        break;
                    }
                }
                self.tokens.expect_token(Token::RParen)?;
                acc = self.builder.call(acc, &args);
            } else if self.tokens.advance_if(Token::Dot) {
                // field access or method call
                if let Some(Token::NumLiteral(n)) = self.tokens.current() {
                    // indexed field access
                    let index = n.parse().unwrap();
                    self.tokens.advance();
                    acc = self.builder.field_access(acc, FieldDescriptor::Indexed(index));
                } else {
                    let member = self.parse_path_segment(true)?;
                    if self.tokens.advance_if(Token::LParen) {
                        // method call
                        let mut args = Vec::new();
                        while self.tokens.current() != Some(&Token::RParen) {
                            let argument = self.parse_expr(true)?; // Allow top-level struct expression in argument
                            args.push(argument);
                            if !self.tokens.advance_if(Token::Comma) {
                                break;
                            }
                        }
                        self.tokens.expect_token(Token::RParen)?;
                        acc = self.builder.mthd_call(acc, member, &args);
                    } else {
                        // named field access
                        acc = self.builder.field_access(acc, FieldDescriptor::Named(member));
                    }
                }
            } else {
                break;
            }
        }

        Ok(acc)
    }

    fn parse_primary_expr(&mut self, allow_top_level_struct_expr: bool) -> Result<Expr<'ast>, ParserErr> {
        let current = self.tokens.current().ok_or(ParserErr::UnexpectedEOF)?;

        match current {
            Token::NumLiteral(value) => {
                let value = value.parse().map_err(|_| ParserErr::InvalidLiteral)?;
                self.tokens.advance();
                let expr = self.builder.lit(Lit::Int(value));
                Ok(expr)
            }
            Token::BoolLiteral(b) => {
                let value = *b;
                self.tokens.advance();
                let expr = self.builder.lit(Lit::Bool(value));
                Ok(expr)
            }
            Token::CCharLiteral(c) => {
                let value = *c;
                self.tokens.advance();
                let expr = self.builder.lit(Lit::CChar(value));
                Ok(expr)
            }
            Token::CStringLiteral(s) => {
                let value = s.clone();
                self.tokens.advance();
                let expr = self.builder.lit(Lit::CString(value));
                Ok(expr)
            }
            Token::Identifier(..) => {
                let path = self.parse_path(true)?;

                if allow_top_level_struct_expr && self.tokens.advance_if(Token::LBrace) {
                    // parse struct expr
                    let mut fields = Vec::new();
                    while let Some(Token::Identifier(field_name)) = self.tokens.current() {
                        let field_name = field_name.clone();
                        self.tokens.advance(); // consume field name
                        let field_value = if self.tokens.advance_if(Token::Colon) {
                            self.parse_expr(true)?
                        } else {
                            let segment = PathSegment {
                                ident: field_name.clone(),
                                args: None,
                                is_self: false,
                            };
                            self.builder.path(Path {
                                segments: vec![segment],
                            })
                        };
                        fields.push((field_name, field_value));
                        if !self.tokens.advance_if(Token::Comma) {
                            break;
                        }
                    }
                    self.tokens.expect_token(Token::RBrace)?;
                    let expr = self.builder.struct_expr(path, fields);
                    Ok(expr)
                } else {
                    let expr = self.builder.path(path);
                    Ok(expr)
                }
            }
            Token::LParen => {
                self.tokens.advance();

                let mut inner_exprs = Vec::new();
                let mut trailing_comma = true;
                while self.tokens.current() != Some(&Token::RParen) {
                    let inner_expr = self.parse_expr(true)?; // Allow top-level struct expression
                    inner_exprs.push(inner_expr);
                    if !self.tokens.advance_if(Token::Comma) {
                        trailing_comma = false;
                        break;
                    }
                }
                self.tokens.expect_token(Token::RParen)?;

                if inner_exprs.len() == 1 && !trailing_comma {
                    Ok(inner_exprs.remove(0))
                } else {
                    let expr = self.builder.tuple(&inner_exprs);
                    Ok(expr)
                }
            }
            Token::LBrace => {
                let block = self.parse_block()?;
                let expr = self.builder.block(block);
                Ok(expr)
            }
            Token::Keyword(Keyword::If) => self.parse_if_expr(),
            Token::Keyword(Keyword::Loop) => self.parse_loop(),
            Token::Keyword(Keyword::While) => self.parse_while(),
            Token::Keyword(Keyword::For) => self.parse_for(),
            Token::Keyword(Keyword::Match) => self.parse_match(),
            Token::Keyword(Keyword::Self_) => {
                self.tokens.advance();
                let expr = self.builder.self_();
                Ok(expr)
            }
            Token::Pipe => {
                self.tokens.advance();

                let mut params = Vec::new();
                while self.tokens.current() != Some(&Token::Pipe) {
                    let param = self.parse_closure_param()?;
                    params.push(param);
                    if !self.tokens.advance_if(Token::Comma) {
                        break;
                    }
                }
                self.tokens.expect_token(Token::Pipe)?;

                let return_ty = if self.tokens.advance_if(Token::Arrow) {
                    Some(self.parse_ty_annot()?)
                } else {
                    None
                };

                let body = self.parse_closure_body(return_ty.is_some())?;

                let expr = self.builder.closure(params, return_ty, body);
                Ok(expr)
            }
            Token::PipePipe => {
                self.tokens.advance();

                let return_ty = if self.tokens.advance_if(Token::Arrow) {
                    Some(self.parse_ty_annot()?)
                } else {
                    None
                };

                let body = self.parse_closure_body(return_ty.is_some())?;

                let expr = self.builder.closure(vec![], return_ty, body);
                Ok(expr)
            }
            Token::Smaller => {
                self.tokens.advance();
                let ty = self.parse_ty_annot()?;

                let trait_ = if self.tokens.advance_if(Token::Keyword(Keyword::As)) {
                    Some(self.parse_trait_annot()?)
                } else {
                    None
                };

                self.tokens.expect_token(Token::Greater)?;
                self.tokens.expect_token(Token::ColonColon)?;
                let path = self.parse_path(true)?;

                let qual_path = QualifiedPath { ty, trait_, path };

                let expr = self.builder.qualified_path(qual_path);
                Ok(expr)
            }

            token => Err(ParserErr::UnexpectedToken(token.clone())),
        }
    }

    fn parse_closure_param(&mut self) -> Result<ClosureParam<'ast>, ParserErr> {
        let name = self.tokens.expect_identifier()?;
        let ty = if self.tokens.advance_if(Token::Colon) {
            Some(self.parse_ty_annot()?)
        } else {
            None
        };
        Ok(ClosureParam { name, ty })
    }

    fn parse_closure_body(&mut self, force_block: bool) -> Result<Block<'ast>, ParserErr> {
        if self.tokens.current() == Some(&Token::LBrace) || force_block {
            self.parse_block()
        } else {
            let return_expr = self.parse_expr(true)?;
            let block = Block {
                return_expr: Some(return_expr),
                stmts: self.builder.stmt_slice(&[]),
            };
            Ok(block)
        }
    }

    fn parse_path(&mut self, in_expression: bool) -> Result<Path<'ast>, ParserErr> {
        let mut segments = Vec::new();

        loop {
            let segment = self.parse_path_segment(in_expression)?;
            segments.push(segment);

            if !self.tokens.advance_if(Token::ColonColon) {
                break;
            }
        }

        Ok(Path { segments })
    }

    fn parse_path_segment(&mut self, in_expression: bool) -> Result<PathSegment<'ast>, ParserErr> {
        if self.tokens.advance_if(Token::Keyword(Keyword::SelfTy)) {
            return Ok(PathSegment {
                ident: "Self".to_string(),
                args: None,
                is_self: true,
            });
        }

        let ident = self.tokens.expect_identifier()?;

        let args = if (!in_expression && self.tokens.advance_if(Token::Smaller)) || self.tokens.advance_if_turbofish() {
            let mut args = Vec::new();
            while self.tokens.current() != Some(&Token::Greater) {
                let gen_arg = self.parse_ty_annot()?;
                args.push(gen_arg);
                if !self.tokens.advance_if(Token::Comma) {
                    break;
                }
            }
            self.tokens.expect_token(Token::Greater)?;
            Some(self.builder.ty_annot_slice(&args))
        } else {
            None
        };

        Ok(PathSegment {
            ident,
            args,
            is_self: false,
        })
    }

    fn parse_if_expr(&mut self) -> Result<Expr<'ast>, ParserErr> {
        self.tokens.expect_keyword(Keyword::If)?;
        let cond = self.parse_expr(false)?; // Don't allow top-level struct in if condition
        let then_block = self.parse_block()?;
        let else_block = if self.tokens.advance_if(Token::Keyword(Keyword::Else)) {
            Some(self.parse_block()?)
        } else {
            None
        };
        let expr = self.builder.if_(cond, then_block, else_block);
        Ok(expr)
    }

    fn parse_loop(&mut self) -> Result<Expr<'ast>, ParserErr> {
        self.tokens.expect_keyword(Keyword::Loop)?;
        let body = self.parse_block()?;
        let expr = self.builder.loop_(body);
        Ok(expr)
    }

    fn parse_while(&mut self) -> Result<Expr<'ast>, ParserErr> {
        self.tokens.expect_keyword(Keyword::While)?;
        let cond = self.parse_expr(false)?; // Don't allow top-level struct in while condition
        let body = self.parse_block()?;
        let expr = self.builder.while_(cond, body);
        Ok(expr)
    }

    fn parse_for(&mut self) -> Result<Expr<'ast>, ParserErr> {
        self.tokens.expect_keyword(Keyword::For)?;
        let mutable = self.tokens.advance_if_keyword(Keyword::Mut);
        let binding = self.tokens.expect_identifier()?;
        self.tokens.expect_keyword(Keyword::In)?;
        let iter = self.parse_expr(false)?;
        let body = self.parse_block()?;
        Ok(self.builder.for_(binding, mutable, iter, body))
    }

    fn parse_match(&mut self) -> Result<Expr<'ast>, ParserErr> {
        self.tokens.expect_keyword(Keyword::Match)?;
        let scrutinee = self.parse_expr(false)?; // Don't allow top-level struct in match scrutinee

        let mut arms = Vec::new();
        self.tokens.expect_token(Token::LBrace)?;

        while Some(&Token::RBrace) != self.tokens.current() {
            let pattern = self.parse_pattern()?;
            self.tokens.expect_token(Token::BoldArrow)?;
            let comma_optional = matches!(
                self.tokens.current(),
                Some(
                    Token::LBrace
                        | Token::Keyword(Keyword::If | Keyword::Loop | Keyword::While | Keyword::For | Keyword::Match)
                )
            );
            let value = self.parse_expr(true)?;
            arms.push(MatchArm { pattern, value });

            let had_comma = self.tokens.advance_if(Token::Comma);
            if !had_comma && !comma_optional {
                break;
            }
        }

        self.tokens.expect_token(Token::RBrace)?;

        let expr = self.builder.match_(scrutinee, arms);
        Ok(expr)
    }

    fn parse_pattern(&mut self) -> Result<Pattern<'ast>, ParserErr> {
        // `(p1, p2, ...)` → tuple pattern
        if self.tokens.advance_if(Token::LParen) {
            let mut fields = Vec::new();
            while !matches!(self.tokens.current(), Some(Token::RParen)) {
                fields.push(self.parse_pattern()?);
                if !self.tokens.advance_if(Token::Comma) {
                    break;
                }
            }
            self.tokens.expect_token(Token::RParen)?;
            return Ok(self.builder.pattern(PatternKind::Tuple(fields)));
        }

        // `mut name` → mutable identifier pattern
        if self.tokens.advance_if_keyword(Keyword::Mut) {
            let name = self.tokens.expect_identifier()?;
            return Ok(self.builder.pattern(PatternKind::Identifier { name, mutable: true }));
        }

        // `_` → wildcard pattern
        if matches!(self.tokens.current(), Some(Token::Underscore)) {
            self.tokens.advance();
            return Ok(self.builder.pattern(PatternKind::Wildcard));
        }

        // `name` not followed by `::` or `{` → plain identifier pattern
        if let Some(Token::Identifier(name)) = self.tokens.current() {
            let name = name.clone();
            if !matches!(self.tokens.next(), Some(Token::ColonColon | Token::LBrace)) {
                self.tokens.advance();
                return Ok(self.builder.pattern(PatternKind::Identifier { name, mutable: false }));
            }
        }

        let variant = self.parse_path(true)?;

        let fields = if self.tokens.advance_if(Token::LBrace) {
            let mut fields = Vec::new();
            while matches!(
                self.tokens.current(),
                Some(Token::Identifier(_)) | Some(Token::Keyword(Keyword::Mut))
            ) {
                let mutable_prefix = self.tokens.advance_if_keyword(Keyword::Mut);
                let field_name = self.tokens.expect_identifier()?;
                let has_colon = self.tokens.advance_if(Token::Colon);
                if mutable_prefix && has_colon {
                    return Err(ParserErr::UnexpectedToken(Token::Colon));
                }
                let pattern = if has_colon {
                    self.parse_pattern()?
                } else {
                    // Shorthand: `{ field }` or `{ mut field }` — desugar to an identifier pattern
                    self.builder.pattern(PatternKind::Identifier {
                        name: field_name.clone(),
                        mutable: mutable_prefix,
                    })
                };

                fields.push(VariantPatternField { field_name, pattern });

                if !self.tokens.advance_if(Token::Comma) {
                    break;
                }
            }

            self.tokens.expect_token(Token::RBrace)?;
            fields
        } else {
            Vec::new()
        };

        Ok(self
            .builder
            .pattern(PatternKind::Variant(VariantPattern { variant, fields })))
    }

    fn parse_ty_annot(&mut self) -> Result<TyAnnot<'ast>, ParserErr> {
        let current = self.tokens.current().ok_or(ParserErr::UnexpectedEOF)?;
        match current {
            Token::Identifier(_) | Token::Keyword(Keyword::SelfTy) => {
                let path = self.parse_path(false)?;
                let annot = self.builder.path_annot(path);
                Ok(annot)
            }
            Token::Ampersand => {
                self.tokens.advance();
                let mutable = self.tokens.advance_if_keyword(Keyword::Mut);
                let inner_ty = self.parse_ty_annot()?;
                let annot = if mutable {
                    self.builder.ref_mut_annot(inner_ty)
                } else {
                    self.builder.ref_annot(inner_ty)
                };
                Ok(annot)
            }
            Token::AmpersandAmpersand => {
                // Two levels of reference
                self.tokens.advance();
                let mutable = self.tokens.advance_if_keyword(Keyword::Mut);
                let inner_ty = self.parse_ty_annot()?;
                let inner = if mutable {
                    self.builder.ref_mut_annot(inner_ty)
                } else {
                    self.builder.ref_annot(inner_ty)
                };
                let annot = self.builder.ref_annot(inner);
                Ok(annot)
            }
            Token::Asterisk => {
                self.tokens.advance();
                let inner_ty = self.parse_ty_annot()?;
                let annot = self.builder.ptr_annot(inner_ty);
                Ok(annot)
            }
            Token::LParen => {
                self.tokens.advance();

                let mut inner_tys = Vec::new();
                let mut trailing_comma = true;
                while self.tokens.current() != Some(&Token::RParen) {
                    let inner_ty = self.parse_ty_annot()?;
                    inner_tys.push(inner_ty);
                    if !self.tokens.advance_if(Token::Comma) {
                        trailing_comma = false;
                        break;
                    }
                }
                self.tokens.expect_token(Token::RParen)?;

                if inner_tys.len() == 1 && !trailing_comma {
                    Ok(inner_tys.remove(0))
                } else {
                    let annot = self.builder.tuple_annot(&inner_tys);
                    Ok(annot)
                }
            }
            Token::Keyword(Keyword::Fn) => {
                self.tokens.advance();
                self.tokens.expect_token(Token::LParen)?;
                let mut param_tys = Vec::new();
                while self.tokens.current() != Some(&Token::RParen) {
                    let param_type = self.parse_ty_annot()?;
                    param_tys.push(param_type);
                    if !self.tokens.advance_if(Token::Comma) {
                        break;
                    }
                }
                self.tokens.expect_token(Token::RParen)?;

                let return_ty = if self.tokens.advance_if(Token::Arrow) {
                    Some(self.parse_ty_annot()?)
                } else {
                    None
                };

                let annot = self.builder.fn_annot(&param_tys, return_ty);
                Ok(annot)
            }
            Token::Keyword(Keyword::Impl) => {
                self.tokens.advance();
                let req = self.parse_constraint_requirement()?;
                Ok(self.builder.impl_trait_annot(req))
            }
            Token::Underscore => {
                self.tokens.advance();
                let annot = self.builder.wildcard_annot();
                Ok(annot)
            }
            Token::Smaller => {
                self.tokens.advance();
                let ty = self.parse_ty_annot()?;
                let trait_ = if self.tokens.advance_if(Token::Keyword(Keyword::As)) {
                    Some(self.parse_trait_annot()?)
                } else {
                    None
                };
                self.tokens.expect_token(Token::Greater)?;
                self.tokens.expect_token(Token::ColonColon)?;
                let path = self.parse_path(false)?;
                let qual_path = QualifiedPath { ty, trait_, path };
                Ok(self.builder.qualified_path_annot(qual_path))
            }
            token => Err(ParserErr::UnexpectedToken(token.clone())),
        }
    }

    fn parse_trait_annot(&mut self) -> Result<TraitAnnot<'ast>, ParserErr> {
        let ty_annot = self.parse_ty_annot()?;
        match ty_annot {
            TyAnnotKind::Path(path) => match path.segments.as_slice() {
                [segment] => Ok(TraitAnnot {
                    name: segment.ident.clone(),
                    args: segment.args,
                }),
                _ => Err(ParserErr::ExpectedTraitName),
            },
            _ => Err(ParserErr::ExpectedTraitName),
        }
    }
}
