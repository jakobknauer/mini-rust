#![allow(unused)]

mod resolve_util;

use std::collections::{HashMap, VecDeque};

use crate::{
    ast,
    ast_to_hlr::resolve_util::TyResolution,
    ctxt::{self, fns},
    hlr,
};

pub fn ast_to_hlr<'ast, 'hlr>(
    ctxt: &ctxt::Ctxt,
    fn_: fns::Fn,
    ast: &'ast ast::Ast<'ast>,
    ast_body: ast::Block<'ast>,
    hlr: &'hlr hlr::Hlr<'hlr>,
) -> AstToHlrResult<hlr::FnHlr<'hlr>> {
    let converter = AstToHlr::new(ctxt, fn_, ast, hlr);
    converter.lower_function_body(ast_body)
}

struct AstToHlr<'ast, 'ctxt, 'hlr> {
    fn_: fns::Fn,

    ctxt: &'ctxt ctxt::Ctxt,
    ast: &'ast ast::Ast<'ast>,
    hlr: &'hlr hlr::Hlr<'hlr>,

    scopes: VecDeque<Scope>,
    blocks: VecDeque<Vec<hlr::Stmt<'hlr>>>,

    self_var_id: Option<hlr::VarId>,
}

pub type AstToHlrResult<T> = Result<T, AstToHlrError>;
#[derive(Debug)]
pub struct AstToHlrError {
    msg: String,
}

impl<'ast, 'ctxt, 'hlr> AstToHlr<'ast, 'ctxt, 'hlr> {
    fn new(ctxt: &'ctxt ctxt::Ctxt, fn_: fns::Fn, ast: &'ast ast::Ast<'ast>, hlr: &'hlr hlr::Hlr<'hlr>) -> Self {
        Self {
            fn_,

            ctxt,
            ast,
            hlr,

            scopes: VecDeque::new(),
            blocks: VecDeque::new(),

            self_var_id: None,
        }
    }

    fn lower_function_body(mut self, block: ast::Block<'ast>) -> AstToHlrResult<hlr::FnHlr<'hlr>> {
        let signature = self.get_signature();
        if signature.var_args {
            return Err(AstToHlrError {
                msg: "Varargs functions are not supported in HLR".to_string(),
            });
        }

        let params = signature.params.clone();
        let mut param_var_ids = Vec::new();

        self.scopes.push_back(Scope::default());
        for fns::FnParam { kind: name, ty } in params {
            let var_id = self.hlr.var_id();
            param_var_ids.push(var_id);

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

        Ok(hlr::FnHlr {
            fn_: self.fn_,
            body,
            param_var_ids,
        })
    }

    fn get_next_var_id(&mut self) -> hlr::VarId {
        self.hlr.var_id()
    }

    fn get_signature(&self) -> &fns::FnSig {
        self.ctxt.fns.get_sig(self.fn_).unwrap()
    }

    fn start_new_block(&mut self) {
        self.blocks.push_back(Vec::new());
    }

    fn release_current_block(&mut self, trailing: hlr::Expr<'hlr>) -> hlr::Expr<'hlr> {
        let stmts = self.blocks.pop_back().expect("self.blocks should never be empty");
        let stmts = self.hlr.stmt_slice(&stmts);
        let block = hlr::ExprDef::Block { stmts, trailing };
        self.hlr.expr(block)
    }

    fn build_block(&mut self, body: ast::Block<'ast>) -> AstToHlrResult<hlr::Expr<'hlr>> {
        self.scopes.push_back(Scope::default());

        for &stmt in body.stmts {
            self.lower_stmt(stmt)?;
        }

        let output = match body.return_expr {
            Some(expr) => self.lower_expr(expr)?,
            None => {
                let unit_expr = hlr::ExprDef::Tuple(self.hlr.expr_slice(&[]));
                self.hlr.expr(unit_expr)
            }
        };

        self.scopes.pop_back();
        Ok(output)
    }

    fn lower_stmt(&mut self, stmt: ast::Stmt<'ast>) -> AstToHlrResult<()> {
        use ast::StmtKind::*;

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

    fn lower_let_stmt(
        &mut self,
        name: &str,
        ty_annot: Option<ast::TyAnnot>,
        value: ast::Expr<'ast>,
    ) -> AstToHlrResult<()> {
        let init = self.lower_expr(value)?;

        let var = self.hlr.var_id();
        self.scopes.back_mut().unwrap().bindings.insert(name.to_string(), var);

        let ty = ty_annot.map(|ty_annot| self.lower_ty_annot(ty_annot)).transpose()?;

        let stmt = hlr::StmtDef::Let { var, ty, init };
        self.push_stmt(stmt)
    }

    fn lower_expr_stmt(&mut self, expr: ast::Expr<'ast>) -> AstToHlrResult<()> {
        let expr = self.lower_expr(expr)?;
        let stmt = hlr::StmtDef::Expr(expr);
        self.push_stmt(stmt)
    }

    fn lower_return_stmt(&mut self, expr: Option<ast::Expr<'ast>>) -> AstToHlrResult<()> {
        let return_expr = expr.map(|e| self.lower_expr(e)).transpose()?;
        let stmt = hlr::StmtDef::Return(return_expr);
        self.push_stmt(stmt)
    }

    fn lower_break_stmt(&mut self) -> AstToHlrResult<()> {
        let stmt = hlr::StmtDef::Break;
        self.push_stmt(stmt)
    }

    fn push_stmt(&mut self, stmt: hlr::StmtDef<'hlr>) -> AstToHlrResult<()> {
        let stmt = self.hlr.stmt(stmt);
        self.blocks.back_mut().unwrap().push(stmt);
        Ok(())
    }

    fn lower_expr(&mut self, expr: ast::Expr<'ast>) -> AstToHlrResult<hlr::Expr<'hlr>> {
        use ast::ExprKind::*;

        match expr {
            Lit(lit) => self.lower_lit(lit),
            Path(path) => self.lower_path(path),
            QualifiedPath(qualified_path) => self.lower_qualified_path(qualified_path),
            &Tuple(fields) => self.lower_tuple_expr(fields),
            &BinaryOp { left, operator, right } => self.lower_binary_op(left, operator, right),
            &UnaryOp { operator, operand } => self.lower_unary_op(operator, operand),
            &Assign { target, value } => self.lower_assign_expr(target, value),
            &Call { callee, args } => self.lower_call_expr(callee, args),
            &MthdCall { obj, ref mthd, args } => self.lower_mthd_call_expr(obj, mthd, args),
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
            &Closure {
                ref params,
                return_ty,
                body,
            } => self.lower_closure_expr(params, return_ty, body),
        }
    }

    fn lower_ty_annot(&mut self, ty_annot: ast::TyAnnot) -> AstToHlrResult<hlr::TyAnnot<'hlr>> {
        let ty_annot = match ty_annot {
            ast::TyAnnotKind::Path(path) => self.lower_path_ty_annot(path)?,
            &ast::TyAnnotKind::Tuple(fields) => {
                let fields = self.lower_ty_annots(fields)?;
                hlr::TyAnnotDef::Tuple(fields)
            }
            &ast::TyAnnotKind::Ref(ty_annot) => hlr::TyAnnotDef::Ref(self.lower_ty_annot(ty_annot)?),
            &ast::TyAnnotKind::Ptr(ty_annot) => hlr::TyAnnotDef::Ptr(self.lower_ty_annot(ty_annot)?),
            &ast::TyAnnotKind::Fn { param_tys, return_ty } => {
                let param_tys = self.lower_ty_annots(param_tys)?;
                let return_ty = return_ty.map(|ret_ty| self.lower_ty_annot(ret_ty)).transpose()?;
                hlr::TyAnnotDef::Fn {
                    params: param_tys,
                    ret: return_ty,
                }
            }
            ast::TyAnnotKind::Wildcard => hlr::TyAnnotDef::Infer,
        };

        let ty_annot = self.hlr.ty_annot(ty_annot);
        Ok(ty_annot)
    }

    fn lower_path_ty_annot(&mut self, path: &ast::Path) -> AstToHlrResult<hlr::TyAnnotDef<'hlr>> {
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
                    base: self.hlr.ty_annot(base),
                    trait_: None,
                    name: sub.ident.clone(),
                })
            }
            _ => Err(AstToHlrError {
                msg: format!("Complex path type annotations are not supported yet: {:#?}", path),
            }),
        }
    }

    fn lower_path_segment_to_ty_annot(&mut self, segment: &ast::PathSegment) -> AstToHlrResult<hlr::TyAnnotDef<'hlr>> {
        if segment.is_self {
            return Ok(hlr::TyAnnotDef::Self_);
        }

        let resolved_ty = self.resolve_ident_to_ty(&segment.ident)?;
        let args = segment.args.map(|args| self.lower_ty_annots(args)).transpose()?;

        match (resolved_ty, args) {
            (TyResolution::GenVar(gen_var), None) => Ok(hlr::TyAnnotDef::GenVar(gen_var)),
            (TyResolution::NamedTy(ctxt::Named::Ty(ty)), None) => Ok(hlr::TyAnnotDef::Ty(ty)),

            (TyResolution::NamedTy(ctxt::Named::Struct(struct_)), args) => Ok(hlr::TyAnnotDef::Struct(struct_, args)),
            (TyResolution::NamedTy(ctxt::Named::Enum(enum_)), args) => Ok(hlr::TyAnnotDef::Enum(enum_, args)),

            ((TyResolution::GenVar(..) | TyResolution::NamedTy(ctxt::Named::Ty(..)), Some(_))) => Err(AstToHlrError {
                msg: format!(
                    "Type '{}' cannot have generic arguments in type annotation",
                    segment.ident
                ),
            }),
        }
    }

    fn lower_ty_annots(&mut self, ty_annots: ast::TyAnnotSlice) -> AstToHlrResult<hlr::TyAnnotSlice<'hlr>> {
        ty_annots
            .iter()
            .map(|&annot| self.lower_ty_annot(annot))
            .collect::<AstToHlrResult<Vec<_>>>()
            .map(|annot_vec| self.hlr.ty_annot_slice(&annot_vec))
    }

    fn lower_lit(&mut self, lit: &ast::Lit) -> AstToHlrResult<hlr::Expr<'hlr>> {
        use ast::Lit::*;

        let lit = match lit {
            &Int(i) => hlr::Lit::Int(i),
            &Bool(b) => hlr::Lit::Bool(b),
            &CChar(c) => hlr::Lit::CChar(c),
            CString(items) => hlr::Lit::CString(items.clone()),
        };

        let expr = hlr::ExprDef::Lit(lit);
        Ok(self.hlr.expr(expr))
    }

    fn lower_path(&mut self, path: &ast::Path) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let expr: hlr::ExprDef = match path.segments.as_slice() {
            [segment] if segment.is_self => {
                return Err(AstToHlrError {
                    msg: "Invalid use of 'Self' as value".to_string(),
                });
            }
            [segment] => {
                let val = self
                    .resolve_ident_to_val_def(&segment.ident)
                    .ok_or_else(|| AstToHlrError {
                        msg: format!("Unresolvable path: {}", segment.ident),
                    })?;
                let args = segment.args.map(|args| self.lower_ty_annots(args)).transpose()?;

                match (val, args) {
                    (val, None) => hlr::ExprDef::Val(val),
                    (hlr::Val::Fn(fn_, _), args) => hlr::ExprDef::Val(hlr::Val::Fn(fn_, args)),

                    (other_val, Some(_)) => {
                        return Err(AstToHlrError {
                            msg: format!("Only functions can be used in generic paths, found {:?}", other_val),
                        });
                    }
                }
            }
            [ty_path, mthd] => {
                let ty = self.lower_path_segment_to_ty_annot(ty_path)?;
                let ty = self.hlr.ty_annot(ty);

                if mthd.is_self {
                    return Err(AstToHlrError {
                        msg: "Invalid use of 'Self' as method name".to_string(),
                    });
                }

                let args = mthd.args.map(|args| self.lower_ty_annots(args)).transpose()?;

                hlr::ExprDef::Val(hlr::Val::Mthd(ty, mthd.ident.clone(), args))
            }
            _ => {
                return Err(AstToHlrError {
                    msg: format!("Complex paths are not supported yet: {:#?}", path),
                });
            }
        };

        let expr = self.hlr.expr(expr);
        Ok(expr)
    }

    fn lower_qualified_path(&mut self, qualified_path: &ast::QualifiedPath) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let ty = self.lower_ty_annot(qualified_path.ty)?;

        let trait_ = qualified_path
            .trait_
            .as_ref()
            .map(|trait_annot| {
                self.ctxt
                    .traits
                    .resolve_trait_name(&trait_annot.name)
                    .ok_or_else(|| AstToHlrError {
                        msg: format!("Unknown trait '{}'", trait_annot.name),
                    })
            })
            .transpose()?;

        let trait_args = qualified_path
            .trait_
            .as_ref()
            .and_then(|trait_| trait_.args)
            .map(|args| self.lower_ty_annots(args))
            .transpose()?;

        let [segment] = qualified_path.path.segments.as_slice() else {
            return Err(AstToHlrError {
                msg: format!(
                    "Only simple qualified paths are supported, found: {:#?}",
                    qualified_path.path
                ),
            });
        };
        let mthd_name = segment.ident.clone();

        let args = segment.args.map(|args| self.lower_ty_annots(args)).transpose()?;

        let expr = hlr::ExprDef::QualifiedMthd {
            ty,
            trait_,
            trait_args,
            mthd_name,
            args,
        };
        Ok(self.hlr.expr(expr))
    }

    fn lower_tuple_expr(&mut self, fields: ast::ExprSlice<'ast>) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let field_exprs: Vec<_> = fields
            .iter()
            .map(|&field| self.lower_expr(field))
            .collect::<AstToHlrResult<_>>()?;
        let field_exprs = self.hlr.expr_slice(&field_exprs);

        let expr = hlr::ExprDef::Tuple(field_exprs);
        Ok(self.hlr.expr(expr))
    }

    fn lower_binary_op(
        &mut self,
        left: ast::Expr<'ast>,
        operator: ast::BinaryOperator,
        right: ast::Expr<'ast>,
    ) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let left = self.lower_expr(left)?;
        let right = self.lower_expr(right)?;

        let expr = hlr::ExprDef::BinaryOp { left, operator, right };
        Ok(self.hlr.expr(expr))
    }

    fn lower_unary_op(
        &mut self,
        operator: ast::UnaryOperator,
        operand: ast::Expr<'ast>,
    ) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let operand = self.lower_expr(operand)?;

        let expr = hlr::ExprDef::UnaryOp { operator, operand };
        Ok(self.hlr.expr(expr))
    }

    fn lower_assign_expr(
        &mut self,
        target: ast::Expr<'ast>,
        value: ast::Expr<'ast>,
    ) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let target = self.lower_expr(target)?;
        let value = self.lower_expr(value)?;

        let expr = hlr::ExprDef::Assign { target, value };
        Ok(self.hlr.expr(expr))
    }

    fn lower_call_expr(
        &mut self,
        callee: ast::Expr<'ast>,
        args: ast::ExprSlice<'ast>,
    ) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let callee = self.lower_expr(callee)?;
        let args: Vec<_> = args
            .iter()
            .map(|&arg| self.lower_expr(arg))
            .collect::<AstToHlrResult<_>>()?;
        let args = self.hlr.expr_slice(&args);

        let expr = hlr::ExprDef::Call { callee, args };
        Ok(self.hlr.expr(expr))
    }

    fn lower_mthd_call_expr(
        &mut self,
        obj: ast::Expr<'ast>,
        mthd: &ast::PathSegment,
        args: ast::ExprSlice<'ast>,
    ) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let obj = self.lower_expr(obj)?;
        let args: Vec<_> = args
            .iter()
            .map(|&arg| self.lower_expr(arg))
            .collect::<AstToHlrResult<_>>()?;
        let args = self.hlr.expr_slice(&args);

        if mthd.is_self {
            return Err(AstToHlrError {
                msg: "Invalid use of 'Self' as method name".to_string(),
            });
        }

        let gen_args = mthd.args.map(|args| self.lower_ty_annots(args)).transpose()?;

        let expr = hlr::ExprDef::MthdCall {
            receiver: obj,
            mthd_name: mthd.ident.clone(),
            gen_args,
            args,
        };

        Ok(self.hlr.expr(expr))
    }

    fn lower_struct_expr(
        &mut self,
        ty_path: &ast::Path,
        fields: &[(String, ast::Expr<'ast>)],
    ) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let constructor = self.resolve_path_to_constructor(ty_path)?;

        let fields: Vec<_> = fields
            .iter()
            .map(|(field_name, field_expr)| {
                let expr = self.lower_expr(field_expr)?;
                let field = hlr::FieldSpec::Name(field_name.clone());
                Ok((field, expr))
            })
            .collect::<AstToHlrResult<_>>()?;
        let fields = self.hlr.struct_expr_field_slice(&fields);

        let expr = hlr::ExprDef::Struct { constructor, fields };
        Ok(self.hlr.expr(expr))
    }

    fn lower_field_access_expr(
        &mut self,
        obj: ast::Expr<'ast>,
        field: &ast::FieldDescriptor,
    ) -> AstToHlrResult<hlr::Expr<'hlr>> {
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
        Ok(self.hlr.expr(expr))
    }

    fn lower_if_expr(
        &mut self,
        cond: ast::Expr<'ast>,
        then: ast::Block<'ast>,
        else_: Option<ast::Block<'ast>>,
    ) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let cond = self.lower_expr(cond)?;

        self.start_new_block();
        let then = self.build_block(then)?;
        let then = self.release_current_block(then);

        let else_ = else_
            .map(|else_| self.build_block(else_))
            .transpose()?
            .map(|else_| self.release_current_block(else_));

        let expr = hlr::ExprDef::If { cond, then, else_ };
        Ok(self.hlr.expr(expr))
    }

    fn lower_loop_expr(&mut self, body: ast::Block<'ast>) -> AstToHlrResult<hlr::Expr<'hlr>> {
        self.start_new_block();
        let body = self.build_block(body)?;
        let body = self.release_current_block(body);

        let expr = hlr::ExprDef::Loop { body };
        Ok(self.hlr.expr(expr))
    }

    fn lower_while_expr(&mut self, cond: ast::Expr<'ast>, body: ast::Block<'ast>) -> AstToHlrResult<hlr::Expr<'hlr>> {
        self.start_new_block();

        let cond = self.lower_expr(cond)?;

        self.start_new_block();
        let result = self.build_block(body)?;
        let then_block = self.release_current_block(result);

        self.start_new_block();
        self.lower_break_stmt()?;
        let unit = self.hlr.expr(hlr::ExprDef::Tuple(self.hlr.expr_slice(&[])));
        let else_block = self.release_current_block(unit);

        let expr = hlr::ExprDef::If {
            cond,
            then: then_block,
            else_: Some(else_block),
        };
        let loop_expr = hlr::ExprDef::Loop {
            body: self.hlr.expr(expr),
        };
        Ok(self.hlr.expr(loop_expr))
    }

    fn lower_match_expr(
        &mut self,
        scrutinee: ast::Expr<'ast>,
        arms: &[ast::MatchArm<'ast>],
    ) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let scrutinee = self.lower_expr(scrutinee)?;

        let hlr_arms: Vec<_> = arms
            .iter()
            .map(|arm| {
                self.scopes.push_back(Scope::default());
                let pattern = self.lower_pattern(&arm.pattern)?;
                let body = self.lower_expr(arm.value)?;
                self.scopes.pop_back();
                Ok(hlr::MatchArm { pattern, body })
            })
            .collect::<AstToHlrResult<_>>()?;
        let hlr_arms = self.hlr.match_arms(&hlr_arms);

        let expr = hlr::ExprDef::Match {
            scrutinee,
            arms: hlr_arms,
        };
        Ok(self.hlr.expr(expr))
    }

    fn lower_pattern(&mut self, pattern: &ast::VariantPattern) -> AstToHlrResult<hlr::Pattern<'hlr>> {
        let variant = self.resolve_path_to_constructor(&pattern.variant)?;

        let hlr::Val::Variant(enum_, variant_index, ref args) = variant else {
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

        let fields: Vec<_> = pattern
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

                let binding = self.hlr.var_id();
                self.scopes
                    .back_mut()
                    .unwrap()
                    .bindings
                    .insert(field.binding_name.clone(), binding);

                Ok(hlr::VariantPatternField { field_index, binding })
            })
            .collect::<AstToHlrResult<_>>()?;
        let fields = self.hlr.variant_pattern_fields(&fields);

        Ok(hlr::VariantPattern { variant, fields })
    }

    fn lower_deref_expr(&mut self, base: ast::Expr<'ast>) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let base = self.lower_expr(base)?;
        let expr = hlr::ExprDef::Deref(base);
        Ok(self.hlr.expr(expr))
    }

    fn lower_addr_of_expr(&mut self, base: ast::Expr<'ast>) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let base = self.lower_expr(base)?;
        let expr = hlr::ExprDef::AddrOf(base);
        Ok(self.hlr.expr(expr))
    }

    fn lower_as_expr(&mut self, expr: ast::Expr<'ast>, target_ty: ast::TyAnnot) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let expr = self.lower_expr(expr)?;
        let ty = self.lower_ty_annot(target_ty)?;
        let expr = hlr::ExprDef::As { expr, ty };
        Ok(self.hlr.expr(expr))
    }

    fn lower_self_expr(&mut self) -> AstToHlrResult<hlr::Expr<'hlr>> {
        let self_var_id = self.self_var_id.ok_or_else(|| AstToHlrError {
            msg: "Cannot use 'self' outside of a method".to_string(),
        })?;
        let expr = hlr::ExprDef::Val(hlr::Val::Var(self_var_id));
        Ok(self.hlr.expr(expr))
    }

    fn lower_closure_expr(
        &mut self,
        params: &[ast::ClosureParam<'ast>],
        return_ty: Option<ast::TyAnnot<'ast>>,
        body: ast::Block<'ast>,
    ) -> AstToHlrResult<hlr::Expr<'hlr>> {
        self.scopes.push_back(Scope::default());

        let params = params
            .iter()
            .map(|param| -> AstToHlrResult<hlr::ClosureParam<'hlr>> {
                let ty = param.ty.map(|ty| self.lower_ty_annot(ty)).transpose()?;
                let var_id = self.hlr.var_id();
                self.scopes
                    .back_mut()
                    .unwrap()
                    .bindings
                    .insert(param.name.clone(), var_id);
                Ok(hlr::ClosureParam(var_id, ty))
            })
            .collect::<AstToHlrResult<Vec<_>>>()?;
        let params = self.hlr.closure_params(&params);

        let body = self.build_block(body)?;

        let return_ty = return_ty.map(|ty| self.lower_ty_annot(ty)).transpose()?;
        self.scopes.pop_back();

        let expr = hlr::ExprDef::Closure {
            params,
            return_ty,
            body,
        };

        Ok(self.hlr.expr(expr))
    }
}

#[derive(Default)]
struct Scope {
    bindings: HashMap<String, hlr::VarId>,
}
