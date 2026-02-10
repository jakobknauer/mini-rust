#![allow(unused)]

mod resolve_util;

use std::collections::{HashMap, VecDeque};

use crate::{
    ast,
    ast_to_hlr::resolve_util::TyResolution,
    ctxt::{self, fns},
    hlr,
};

pub fn ast_to_hlr(
    ctxt: &ctxt::Ctxt,
    fn_: fns::Fn,
    ast: &ast::Ast,
    ast_body: ast::Block,
) -> AstToHlrResult<(hlr::Hlr, hlr::Expr)> {
    let converter = AstToHlr::new(ctxt, fn_, ast);
    converter.lower_block(ast_body)
}

struct AstToHlr<'a> {
    ctxt: &'a ctxt::Ctxt,
    fn_: fns::Fn,
    ast: &'a ast::Ast,
    hlr: hlr::Hlr,

    scopes: VecDeque<Scope>,
    blocks: VecDeque<Vec<hlr::Stmt>>,

    next_var_id: hlr::VarId,
    self_var_id: Option<hlr::VarId>,
}

pub type AstToHlrResult<T> = Result<T, AstToHlrError>;
pub struct AstToHlrError {
    msg: String,
}

impl<'a> AstToHlr<'a> {
    fn new(ctxt: &'a ctxt::Ctxt, fn_: fns::Fn, ast: &'a ast::Ast) -> Self {
        Self {
            ctxt,
            fn_,
            ast,
            hlr: hlr::Hlr::new(),

            scopes: VecDeque::new(),
            blocks: VecDeque::new(),

            next_var_id: hlr::VarId(0),
            self_var_id: None,
        }
    }

    pub fn lower_block(mut self, block: ast::Block) -> AstToHlrResult<(hlr::Hlr, hlr::Expr)> {
        let signature = self.get_signature();
        if signature.var_args {
            return Err(AstToHlrError {
                msg: "Varargs functions are not supported in HLR".to_string(),
            });
        }

        let params = signature.params.clone();

        self.scopes.push_back(Scope::default());
        for fns::FnParam { kind: name, ty } in params {
            let var_id = self.get_next_var_id();

            match name {
                fns::FnParamKind::Regular(name) => {
                    self.scopes
                        .back_mut()
                        .unwrap()
                        .bindings
                        .insert(name.to_string(), var_id);
                }
                fns::FnParamKind::Self_ | fns::FnParamKind::SelfByRef => {
                    self.self_var_id = Some(var_id);
                }
            }
        }

        self.start_new_block();

        let return_val = self.build_block(block)?;
        let body = self.release_current_block(return_val);

        Ok((self.hlr, body))
    }

    fn get_next_var_id(&mut self) -> hlr::VarId {
        let id = self.next_var_id.0;
        self.next_var_id.0 += 1;
        hlr::VarId(id)
    }

    fn get_signature(&self) -> &fns::FnSig {
        self.ctxt.fns.get_sig(self.fn_).unwrap()
    }

    fn start_new_block(&mut self) {
        self.blocks.push_back(Vec::new());
    }

    fn release_current_block(&mut self, trailing: hlr::Expr) -> hlr::Expr {
        let stmts = self.blocks.pop_back().expect("self.blocks should never be empty");
        let block = hlr::ExprDef::Block { stmts, trailing };
        self.hlr.new_expr(block)
    }

    fn build_block(&mut self, body: ast::Block) -> AstToHlrResult<hlr::Expr> {
        self.scopes.push_back(Scope::default());

        for &stmt in self.ast.stmt_slice(body.stmts) {
            self.lower_stmt(stmt)?;
        }

        let output = match body.return_expr {
            Some(expr) => self.lower_expr(expr)?,
            None => {
                let unit_expr = hlr::ExprDef::Tuple(vec![]);
                self.hlr.new_expr(unit_expr)
            }
        };

        self.scopes.pop_back();
        Ok(output)
    }

    fn lower_stmt(&mut self, stmt: ast::Stmt) -> AstToHlrResult<()> {
        use ast::StmtKind::*;

        let stmt = self.ast.stmt(stmt);

        match *stmt {
            Let {
                ref name,
                ty_annot,
                value,
            } => self.lower_let_stmt(name, ty_annot, value),
            Expr(expr) => self.lower_expr_stmt(expr),
            Return(expr) => self.lower_return_stmt(expr),
            Break => self.lower_break_stmt(),
        }
    }

    fn lower_let_stmt(&mut self, name: &str, ty_annot: Option<ast::TyAnnot>, value: ast::Expr) -> AstToHlrResult<()> {
        let init = self.lower_expr(value)?;

        let var = self.get_next_var_id();
        self.scopes.back_mut().unwrap().bindings.insert(name.to_string(), var);

        let ty = ty_annot.map(|ty_annot| self.lower_ty_annot(ty_annot)).transpose()?;

        let stmt = hlr::StmtDef::Let { var, ty, init };
        self.push_stmt(stmt)
    }

    fn lower_expr_stmt(&mut self, expr: ast::Expr) -> AstToHlrResult<()> {
        let expr = self.lower_expr(expr)?;
        let stmt = hlr::StmtDef::Expr(expr);
        self.push_stmt(stmt)
    }

    fn lower_return_stmt(&mut self, expr: Option<ast::Expr>) -> AstToHlrResult<()> {
        let return_expr = expr.map(|e| self.lower_expr(e)).transpose()?;
        let stmt = hlr::StmtDef::Return(return_expr);
        self.push_stmt(stmt)
    }

    fn lower_break_stmt(&mut self) -> AstToHlrResult<()> {
        let stmt = hlr::StmtDef::Break;
        self.push_stmt(stmt)
    }

    fn push_stmt(&mut self, stmt: hlr::StmtDef) -> AstToHlrResult<()> {
        let stmt = self.hlr.new_stmt(stmt);
        self.blocks.back_mut().unwrap().push(stmt);
        Ok(())
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> AstToHlrResult<hlr::Expr> {
        use ast::ExprKind::*;

        let expr = self.ast.expr(expr);

        match expr {
            Lit(lit) => self.lower_lit(lit),
            Path(path) => self.lower_path(path),
            QualifiedPath(qualified_path) => todo!(),
            &Tuple(fields) => self.lower_tuple_expr(fields),
            &BinaryOp { left, operator, right } => self.lower_binary_op(left, operator, right),
            &UnaryOp { operator, operand } => self.lower_unary_op(operator, operand),
            &Assign { target, value } => self.lower_assign_expr(target, value),
            &Call { callee, args } => self.lower_call_expr(callee, args),
            &MthdCall { obj, ref mthd, args } => self.lower_method_call_expr(obj, mthd, args),
            Struct { ty_path, fields } => self.lower_struct_expr(ty_path, fields),
            &FieldAccess { obj, ref field } => self.lower_field_access_expr(obj, field),
            &Block(block) => self.build_block(block),
            &If { cond, then, else_ } => self.lower_if_expr(cond, then, else_),
            &Loop { body } => self.lower_loop_expr(body),
            &While { cond, body } => self.lower_while_expr(cond, body),
            &Match { scrutinee, ref arms } => self.lower_match_expr(scrutinee, arms),
            &Deref { base } => self.lower_deref_expr(base),
            &AddrOf { base } => self.lower_addr_of_expr(base),
            &As { expr, target_ty } => self.lower_as_expr(expr, target_ty),
            Self_ => self.lower_self_expr(),
            Closure {
                params,
                return_ty,
                body,
            } => todo!(),
        }
    }

    fn lower_ty_annot(&mut self, ty_annot: ast::TyAnnot) -> AstToHlrResult<hlr::TyAnnot> {
        let ty_annot = self.ast.ty_annot(ty_annot);

        let ty_annot = match ty_annot {
            ast::TyAnnotKind::Path(path) => self.lower_path_ty_annot(path)?,
            &ast::TyAnnotKind::Tuple(fields) => {
                let fields = self
                    .ast
                    .ty_annot_slice(fields)
                    .iter()
                    .map(|&field| self.lower_ty_annot(field))
                    .collect::<AstToHlrResult<_>>()?;
                hlr::TyAnnotDef::Tuple(fields)
            }
            &ast::TyAnnotKind::Ref(ty_annot) => hlr::TyAnnotDef::Ref(self.lower_ty_annot(ty_annot)?),
            &ast::TyAnnotKind::Ptr(ty_annot) => hlr::TyAnnotDef::Ptr(self.lower_ty_annot(ty_annot)?),
            &ast::TyAnnotKind::Fn { param_tys, return_ty } => {
                let param_tys = self
                    .ast
                    .ty_annot_slice(param_tys)
                    .iter()
                    .map(|&param_ty| self.lower_ty_annot(param_ty))
                    .collect::<AstToHlrResult<_>>()?;
                let return_ty = return_ty.map(|ret_ty| self.lower_ty_annot(ret_ty)).transpose()?;
                hlr::TyAnnotDef::Fn {
                    params: param_tys,
                    ret: return_ty,
                }
            }
            ast::TyAnnotKind::Wildcard => hlr::TyAnnotDef::Infer,
        };

        let ty_annot = self.hlr.new_ty_annot(ty_annot);
        Ok(ty_annot)
    }

    fn lower_path_ty_annot(&mut self, path: &ast::Path) -> AstToHlrResult<hlr::TyAnnotDef> {
        match path.segments.as_slice() {
            [simple] => {
                let ty_annot = self.lower_path_segment_to_ty_annot(simple)?;
                Ok(ty_annot)
            }
            [parent, sub] => {
                let parent_ty_annot = self.lower_path_segment_to_ty_annot(parent)?;

                if sub.is_self {
                    return Err(AstToHlrError {
                        msg: "Invalid use of 'Self' in type annotation".to_string(),
                    });
                }

                if sub.args.is_some() {
                    return Err(AstToHlrError {
                        msg: "Generic associated types are not supported".to_string(),
                    });
                }

                let base = self.lower_path_segment_to_ty_annot(parent)?;
                Ok(hlr::TyAnnotDef::AssocTy {
                    base: self.hlr.new_ty_annot(base),
                    trait_: None,
                    name: sub.ident.clone(),
                })
            }
            _ => Err(AstToHlrError {
                msg: format!("Complex path type annotations are not supported yet: {:#?}", path),
            }),
        }
    }

    fn lower_path_segment_to_ty_annot(&mut self, segment: &ast::PathSegment) -> AstToHlrResult<hlr::TyAnnotDef> {
        if segment.is_self {
            return Ok(hlr::TyAnnotDef::Self_);
        }

        let resolved_ty = self.resolve_ident_to_ty(&segment.ident)?;

        match segment.args {
            None => match resolved_ty {
                TyResolution::GenVar(gen_var) => Ok(hlr::TyAnnotDef::GenVar(gen_var)),
                TyResolution::NamedTy(named_ty) => match named_ty {
                    ctxt::Named::Ty(ty) => Ok(hlr::TyAnnotDef::Ty(ty)),
                    ctxt::Named::Struct(struct_) => Ok(hlr::TyAnnotDef::Struct(struct_, None)),
                    ctxt::Named::Enum(enum_) => Ok(hlr::TyAnnotDef::Enum(enum_, None)),
                },
            },
            Some(args) => {
                let gen_args = self
                    .ast
                    .ty_annot_slice(args)
                    .iter()
                    .map(|&arg| self.lower_ty_annot(arg))
                    .collect::<AstToHlrResult<_>>()?;

                match resolved_ty {
                    TyResolution::GenVar(gen_var) => Err(AstToHlrError {
                        msg: format!(
                            "Generic type variable '{}' cannot have generic arguments in type annotation",
                            segment.ident
                        ),
                    }),
                    TyResolution::NamedTy(named_ty) => match named_ty {
                        ctxt::Named::Ty(ty) => Err(AstToHlrError {
                            msg: format!(
                                "Primitive type '{}' cannot have generic arguments in type annotation",
                                segment.ident
                            ),
                        }),
                        ctxt::Named::Struct(struct_) => Ok(hlr::TyAnnotDef::Struct(struct_, Some(gen_args))),
                        ctxt::Named::Enum(enum_) => Ok(hlr::TyAnnotDef::Enum(enum_, Some(gen_args))),
                    },
                }
            }
        }
    }

    fn lower_lit(&mut self, lit: &ast::Lit) -> AstToHlrResult<hlr::Expr> {
        use ast::Lit::*;

        let lit = match lit {
            &Int(i) => hlr::Lit::Int(i),
            &Bool(b) => hlr::Lit::Bool(b),
            &CChar(c) => hlr::Lit::CChar(c),
            CString(items) => hlr::Lit::CString(items.clone()),
        };

        let expr = hlr::ExprDef::Lit(lit);
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_path(&mut self, path: &ast::Path) -> AstToHlrResult<hlr::Expr> {
        let expr: hlr::ExprDef = match path.segments.as_slice() {
            [segment] if segment.is_self => {
                return Err(AstToHlrError {
                    msg: "Invalid use of 'Self' as value".to_string(),
                });
            }
            [segment] => match segment.args {
                None => self
                    .resolve_ident_to_val_def(&segment.ident)
                    .map(hlr::ExprDef::Def)
                    .ok_or_else(|| AstToHlrError {
                        msg: format!("Unresolvable path: {}", &segment.ident),
                    })?,

                Some(args) => {
                    let base_val = self
                        .resolve_ident_to_val_def(&segment.ident)
                        .ok_or_else(|| AstToHlrError {
                            msg: format!("Unresolvable path: {}", segment.ident),
                        })?;

                    match base_val {
                        hlr::Def::Fn(fn_) => {
                            let gen_args = self
                                .ast
                                .ty_annot_slice(args)
                                .iter()
                                .map(|&annot| self.lower_ty_annot(annot))
                                .collect::<AstToHlrResult<_>>()?;
                            hlr::ExprDef::GenDef {
                                base: hlr::Def::Fn(fn_),
                                gen_args,
                            }
                        }
                        _ => {
                            return Err(AstToHlrError {
                                msg: format!("Only functions can be used in generic paths, found {:?}", base_val),
                            });
                        }
                    }
                }
            },
            [ty_path, mthd_name] => {
                let ty = self.lower_path_segment_to_ty_annot(ty_path)?;
                let ty = self.hlr.new_ty_annot(ty);

                if mthd_name.is_self {
                    return Err(AstToHlrError {
                        msg: "Invalid use of 'Self' as method name".to_string(),
                    });
                }

                match mthd_name.args {
                    None => hlr::ExprDef::Def(hlr::Def::Mthd(ty, mthd_name.ident.clone())),
                    Some(args) => {
                        let gen_args = self
                            .ast
                            .ty_annot_slice(args)
                            .iter()
                            .map(|&annot| self.lower_ty_annot(annot))
                            .collect::<AstToHlrResult<_>>()?;
                        hlr::ExprDef::GenDef {
                            base: hlr::Def::Mthd(ty, mthd_name.ident.clone()),
                            gen_args,
                        }
                    }
                }
            }
            _ => {
                return Err(AstToHlrError {
                    msg: format!("Complex paths are not supported yet: {:#?}", path),
                });
            }
        };

        let expr = self.hlr.new_expr(expr);
        Ok(expr)
    }

    fn lower_tuple_expr(&mut self, fields: ast::ExprSlice) -> AstToHlrResult<hlr::Expr> {
        let field_exprs = self
            .ast
            .expr_slice(fields)
            .iter()
            .map(|&field| self.lower_expr(field))
            .collect::<AstToHlrResult<_>>()?;

        let expr = hlr::ExprDef::Tuple(field_exprs);
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_binary_op(
        &mut self,
        left: ast::Expr,
        operator: ast::BinaryOperator,
        right: ast::Expr,
    ) -> AstToHlrResult<hlr::Expr> {
        let left = self.lower_expr(left)?;
        let right = self.lower_expr(right)?;

        let expr = hlr::ExprDef::BinaryOp { left, operator, right };
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_unary_op(&mut self, operator: ast::UnaryOperator, operand: ast::Expr) -> AstToHlrResult<hlr::Expr> {
        let operand = self.lower_expr(operand)?;

        let expr = hlr::ExprDef::UnaryOp { operator, operand };
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_assign_expr(&mut self, target: ast::Expr, value: ast::Expr) -> AstToHlrResult<hlr::Expr> {
        let target = self.lower_expr(target)?;
        let value = self.lower_expr(value)?;

        let expr = hlr::ExprDef::Assign { target, value };
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_call_expr(&mut self, callee: ast::Expr, args: ast::ExprSlice) -> AstToHlrResult<hlr::Expr> {
        let callee = self.lower_expr(callee)?;
        let arg_exprs = self
            .ast
            .expr_slice(args)
            .iter()
            .map(|&arg| self.lower_expr(arg))
            .collect::<AstToHlrResult<_>>()?;

        let expr = hlr::ExprDef::Call {
            callee,
            args: arg_exprs,
        };
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_method_call_expr(
        &mut self,
        obj: ast::Expr,
        mthd: &ast::PathSegment,
        args: ast::ExprSlice,
    ) -> AstToHlrResult<hlr::Expr> {
        let obj = self.lower_expr(obj)?;
        let args = self
            .ast
            .expr_slice(args)
            .iter()
            .map(|&arg| self.lower_expr(arg))
            .collect::<AstToHlrResult<_>>()?;

        if mthd.is_self {
            return Err(AstToHlrError {
                msg: "Invalid use of 'Self' as method name".to_string(),
            });
        }

        let gen_args = mthd
            .args
            .map(|args| {
                self.ast
                    .ty_annot_slice(args)
                    .iter()
                    .map(|&arg| self.lower_ty_annot(arg))
                    .collect::<AstToHlrResult<_>>()
            })
            .transpose()?;

        let expr = hlr::ExprDef::MethodCall {
            receiver: obj,
            method_name: mthd.ident.clone(),
            gen_args,
            args,
        };

        Ok(self.hlr.new_expr(expr))
    }

    fn lower_struct_expr(&mut self, ty_path: &ast::Path, fields: &[(String, ast::Expr)]) -> AstToHlrResult<hlr::Expr> {
        let (constructor, gen_args) = self.resolve_path_to_constructor(ty_path)?;

        let fields = fields
            .iter()
            .map(|(field_name, field_expr)| {
                let expr = self.lower_expr(*field_expr)?;
                let field = hlr::FieldSpec::Name(field_name.clone());
                Ok((field, expr))
            })
            .collect::<AstToHlrResult<_>>()?;

        let expr = hlr::ExprDef::Struct {
            constructor,
            fields,
            gen_args: Some(gen_args),
        };
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_field_access_expr(&mut self, obj: ast::Expr, field: &ast::FieldDescriptor) -> AstToHlrResult<hlr::Expr> {
        let obj = self.lower_expr(obj)?;

        let field_spec = match field {
            ast::FieldDescriptor::Named(name) => {
                if name.is_self {
                    return Err(AstToHlrError {
                        msg: "Invalid use of 'Self' as field name".to_string(),
                    });
                }
                if name.args.is_some() {
                    return Err(AstToHlrError {
                        msg: "Generic field names are not valid".to_string(),
                    });
                }
                hlr::FieldSpec::Name(name.ident.clone())
            }
            ast::FieldDescriptor::Indexed(index) => hlr::FieldSpec::Index(*index),
        };

        let expr = hlr::ExprDef::FieldAccess {
            base: obj,
            field: field_spec,
        };
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_if_expr(
        &mut self,
        cond: ast::Expr,
        then: ast::Block,
        else_: Option<ast::Block>,
    ) -> AstToHlrResult<hlr::Expr> {
        let cond = self.lower_expr(cond)?;

        self.start_new_block();
        let then = self.build_block(then)?;
        let then = self.release_current_block(then);

        let else_ = else_
            .map(|else_| self.build_block(else_))
            .transpose()?
            .map(|else_| self.release_current_block(else_));

        let expr = hlr::ExprDef::If { cond, then, else_ };
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_loop_expr(&mut self, body: ast::Block) -> AstToHlrResult<hlr::Expr> {
        self.start_new_block();
        let body = self.build_block(body)?;
        let body = self.release_current_block(body);

        let expr = hlr::ExprDef::Loop { body };
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_while_expr(&mut self, cond: ast::Expr, body: ast::Block) -> AstToHlrResult<hlr::Expr> {
        self.start_new_block();

        let cond = self.lower_expr(cond)?;

        self.start_new_block();
        let result = self.build_block(body)?;
        let then_block = self.release_current_block(result);

        self.start_new_block();
        self.lower_break_stmt()?;
        let unit = self.hlr.new_expr(hlr::ExprDef::Tuple(vec![]));
        let else_block = self.release_current_block(unit);

        let expr = hlr::ExprDef::If {
            cond,
            then: then_block,
            else_: Some(else_block),
        };
        let loop_expr = hlr::ExprDef::Loop {
            body: self.hlr.new_expr(expr),
        };
        Ok(self.hlr.new_expr(loop_expr))
    }

    fn lower_match_expr(&mut self, scrutinee: ast::Expr, arms: &[ast::MatchArm]) -> AstToHlrResult<hlr::Expr> {
        let scrutinee = self.lower_expr(scrutinee)?;

        let hlr_arms = arms
            .iter()
            .map(|arm| {
                self.scopes.push_back(Scope::default());
                let pattern = self.lower_pattern(&arm.pattern)?;
                let body = self.lower_expr(arm.value)?;
                self.scopes.pop_back();
                Ok(hlr::MatchArm { pattern, body })
            })
            .collect::<AstToHlrResult<_>>()?;

        let expr = hlr::ExprDef::Match {
            scrutinee,
            arms: hlr_arms,
        };
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_pattern(&mut self, pattern: &ast::VariantPattern) -> AstToHlrResult<hlr::Pattern> {
        let (variant, gen_args) = self.resolve_path_to_constructor(&pattern.variant)?;

        let hlr::Def::Variant(enum_, variant_index) = variant.clone() else {
            return Err(AstToHlrError {
                msg: format!("Only enum variants are supported in patterns, found {:?}", variant),
            });
        };

        let variant_struct = self.ctxt.tys.get_enum_def(enum_).unwrap().variants[variant_index].struct_;
        let variant_struct = self.ctxt.tys.get_struct_def(variant_struct).unwrap();

        if pattern.fields.len() != variant_struct.fields.len() {
            return Err(AstToHlrError {
                msg: format!(
                    "Pattern for variant has wrong number of fields: expected {}, found {}",
                    variant_struct.fields.len(),
                    pattern.fields.len()
                ),
            });
        }

        let fields = pattern
            .fields
            .iter()
            .map(|field| {
                let field_index = variant_struct
                    .fields
                    .iter()
                    .position(|f| f.name == field.field_name)
                    .ok_or_else(|| AstToHlrError {
                        msg: format!(
                            "Unknown field '{}' in pattern for variant '{}'",
                            field.field_name, enum_.0
                        ),
                    })?;

                let binding = self.get_next_var_id();
                self.scopes
                    .back_mut()
                    .unwrap()
                    .bindings
                    .insert(field.binding_name.clone(), binding);

                Ok(hlr::VariantPatternField { field_index, binding })
            })
            .collect::<AstToHlrResult<_>>()?;

        Ok(hlr::VariantPattern {
            variant,
            gen_args: Some(gen_args),
            fields,
        })
    }

    fn lower_deref_expr(&mut self, base: ast::Expr) -> AstToHlrResult<hlr::Expr> {
        let base = self.lower_expr(base)?;
        let expr = hlr::ExprDef::Deref(base);
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_addr_of_expr(&mut self, base: ast::Expr) -> AstToHlrResult<hlr::Expr> {
        let base = self.lower_expr(base)?;
        let expr = hlr::ExprDef::AddrOf(base);
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_as_expr(&mut self, expr: ast::Expr, target_ty: ast::TyAnnot) -> AstToHlrResult<hlr::Expr> {
        let expr = self.lower_expr(expr)?;
        let ty = self.lower_ty_annot(target_ty)?;
        let expr = hlr::ExprDef::As { expr, ty };
        Ok(self.hlr.new_expr(expr))
    }

    fn lower_self_expr(&mut self) -> AstToHlrResult<hlr::Expr> {
        let self_var_id = self.self_var_id.ok_or_else(|| AstToHlrError {
            msg: "Cannot use 'self' outside of a method".to_string(),
        })?;
        let expr = hlr::ExprDef::Def(hlr::Def::Var(self_var_id));
        Ok(self.hlr.new_expr(expr))
    }
}

#[derive(Default)]
struct Scope {
    bindings: HashMap<String, hlr::VarId>,
}
