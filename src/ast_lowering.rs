mod resolve_util;

use std::collections::{HashMap, VecDeque};

use crate::{
    ast,
    ast_lowering::resolve_util::TyResolution,
    ctxt::{self, fns},
    hlr,
};

pub fn ast_to_hlr<'a, 'ctxt, 'ast>(
    ctxt: &'a ctxt::Ctxt<'ctxt>,
    fn_: fns::Fn<'ctxt>,
    ast_body: ast::Block<'ast>,
    hlr: &'ctxt hlr::Hlr<'ctxt>,
) -> AstLoweringResult<hlr::Fn<'ctxt>> {
    let converter = AstLowerer::new(ctxt, fn_, hlr);
    converter.lower_function_body(ast_body)
}

struct AstLowerer<'a, 'ctxt> {
    fn_: fns::Fn<'ctxt>,

    ctxt: &'a ctxt::Ctxt<'ctxt>,
    hlr: &'ctxt hlr::Hlr<'ctxt>,

    scopes: VecDeque<Scope>,
    blocks: VecDeque<Vec<hlr::Stmt<'ctxt>>>,

    self_var_id: Option<hlr::VarId>,
}

pub type AstLoweringResult<T> = Result<T, AstLoweringError>;
#[derive(Debug)]
pub struct AstLoweringError {
    pub msg: String,
}

impl<'a, 'ctxt, 'ast> AstLowerer<'a, 'ctxt> {
    fn new(ctxt: &'a ctxt::Ctxt<'ctxt>, fn_: fns::Fn<'ctxt>, hlr: &'ctxt hlr::Hlr<'ctxt>) -> Self {
        Self {
            fn_,

            ctxt,
            hlr,

            scopes: VecDeque::new(),
            blocks: VecDeque::new(),

            self_var_id: None,
        }
    }

    fn lower_function_body(mut self, block: ast::Block) -> AstLoweringResult<hlr::Fn<'ctxt>> {
        if self.fn_.var_args {
            return Err(AstLoweringError {
                msg: "Varargs functions are not supported in HLR".to_string(),
            });
        }

        self.scopes.push_back(Scope::default());
        let mut param_var_ids = Vec::new();

        for param in &self.fn_.params {
            let var_id = self.hlr.var_id();
            param_var_ids.push(var_id);

            match &param.kind {
                fns::FnParamKind::Regular(name, ..) => {
                    self.scopes
                        .back_mut()
                        .unwrap()
                        .bindings
                        .insert(name.to_string(), var_id);
                }
                fns::FnParamKind::Self_ | fns::FnParamKind::SelfByRef | fns::FnParamKind::SelfByRefMut => {
                    self.self_var_id = Some(var_id);
                }
            }
        }

        self.start_new_block();

        let return_val = self.build_block(block)?;
        let body = self.release_current_block(return_val);

        Ok(hlr::Fn {
            fn_: self.fn_,
            body,
            param_var_ids,
        })
    }

    fn start_new_block(&mut self) {
        self.blocks.push_back(Vec::new());
    }

    fn release_current_block(&mut self, trailing: hlr::Expr<'ctxt>) -> hlr::Expr<'ctxt> {
        let stmts = self.blocks.pop_back().expect("self.blocks should never be empty");
        let stmts = self.hlr.stmt_slice(&stmts);
        let block = hlr::ExprDef::Block { stmts, trailing };
        self.hlr.expr(block)
    }

    fn build_block(&mut self, body: ast::Block<'ast>) -> AstLoweringResult<hlr::Expr<'ctxt>> {
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

    fn lower_stmt(&mut self, stmt: ast::Stmt<'ast>) -> AstLoweringResult<()> {
        use ast::StmtKind::*;

        match *stmt {
            Let {
                ref name,
                mutable,
                ty_annot,
                value,
            } => self.lower_let_stmt(name, mutable, ty_annot, value),
            Expr(expr) => self.lower_expr_stmt(expr),
            Return(expr) => self.lower_return_stmt(expr),
            Break => self.lower_break_stmt(),
        }
    }

    fn lower_let_stmt(
        &mut self,
        name: &str,
        mutable: bool,
        ty_annot: Option<ast::TyAnnot>,
        value: ast::Expr<'ast>,
    ) -> AstLoweringResult<()> {
        let init = self.lower_expr(value)?;

        let var = self.hlr.var_id();
        self.scopes.back_mut().unwrap().bindings.insert(name.to_string(), var);

        let ty = ty_annot.map(|ty_annot| self.lower_ty_annot(ty_annot)).transpose()?;

        let stmt = hlr::StmtDef::Let { var, mutable, ty, init };
        self.push_stmt(stmt)
    }

    fn lower_expr_stmt(&mut self, expr: ast::Expr<'ast>) -> AstLoweringResult<()> {
        let expr = self.lower_expr(expr)?;
        let stmt = hlr::StmtDef::Expr(expr);
        self.push_stmt(stmt)
    }

    fn lower_return_stmt(&mut self, expr: Option<ast::Expr<'ast>>) -> AstLoweringResult<()> {
        let return_expr = expr.map(|e| self.lower_expr(e)).transpose()?;
        let stmt = hlr::StmtDef::Return(return_expr);
        self.push_stmt(stmt)
    }

    fn lower_break_stmt(&mut self) -> AstLoweringResult<()> {
        let stmt = hlr::StmtDef::Break;
        self.push_stmt(stmt)
    }

    fn push_stmt(&mut self, stmt: hlr::StmtDef<'ctxt>) -> AstLoweringResult<()> {
        let stmt = self.hlr.stmt(stmt);
        self.blocks.back_mut().unwrap().push(stmt);
        Ok(())
    }

    fn lower_expr(&mut self, expr: ast::Expr<'ast>) -> AstLoweringResult<hlr::Expr<'ctxt>> {
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
            &Block(block) => {
                self.start_new_block();
                let trailing = self.build_block(block)?;
                Ok(self.release_current_block(trailing))
            }
            &If { cond, then, else_ } => self.lower_if_expr(cond, then, else_),
            &Loop { body } => self.lower_loop_expr(body),
            &While { cond, body } => self.lower_while_expr(cond, body),
            &For {
                ref binding,
                mutable,
                iter,
                body,
            } => self.lower_for_expr(binding, mutable, iter, body),
            &Match { scrutinee, ref arms } => self.lower_match_expr(scrutinee, arms),
            &Deref { base } => self.lower_deref_expr(base),
            &AddrOf { base } => self.lower_addr_of_expr(base),
            &AddrOfMut { base } => self.lower_addr_of_mut_expr(base),
            &As { expr, target_ty } => self.lower_as_expr(expr, target_ty),
            Self_ => self.lower_self_expr(),
            &Closure {
                ref params,
                return_ty,
                body,
            } => self.lower_closure_expr(params, return_ty, body),
        }
    }

    fn lower_ty_annot(&mut self, ty_annot: ast::TyAnnot) -> AstLoweringResult<hlr::TyAnnot<'ctxt>> {
        let ty_annot = match ty_annot {
            ast::TyAnnotKind::Path(path) => self.lower_path_ty_annot(path)?,
            &ast::TyAnnotKind::Tuple(fields) => {
                let fields = self.lower_ty_annots(fields)?;
                hlr::TyAnnotDef::Tuple(fields)
            }
            &ast::TyAnnotKind::Ref(ty_annot) => hlr::TyAnnotDef::Ref(self.lower_ty_annot(ty_annot)?),
            &ast::TyAnnotKind::RefMut(ty_annot) => hlr::TyAnnotDef::RefMut(self.lower_ty_annot(ty_annot)?),
            &ast::TyAnnotKind::Ptr(ty_annot) => hlr::TyAnnotDef::Ptr(self.lower_ty_annot(ty_annot)?),
            &ast::TyAnnotKind::Fn { param_tys, return_ty } => {
                let param_tys = self.lower_ty_annots(param_tys)?;
                let return_ty = return_ty.map(|ret_ty| self.lower_ty_annot(ret_ty)).transpose()?;
                hlr::TyAnnotDef::Fn {
                    params: param_tys,
                    ret: return_ty,
                }
            }
            ast::TyAnnotKind::QualifiedPath(qual_path) => self.lower_qualified_path_ty_annot(qual_path)?,
            ast::TyAnnotKind::Wildcard => hlr::TyAnnotDef::Infer,
            ast::TyAnnotKind::ImplTrait(_) => {
                return Err(AstLoweringError {
                    msg: "impl Trait is only valid in return position".to_string(),
                });
            }
        };

        let ty_annot = self.hlr.ty_annot(ty_annot);
        Ok(ty_annot)
    }

    fn lower_path_ty_annot(&mut self, path: &ast::Path) -> AstLoweringResult<hlr::TyAnnotDef<'ctxt>> {
        match path.segments.as_slice() {
            [simple] => {
                let ty_annot = self.lower_path_segment_to_ty_annot(simple)?;
                Ok(ty_annot)
            }
            [parent, sub] => {
                if sub.is_self {
                    return Err(AstLoweringError {
                        msg: "Invalid use of 'Self' in type annotation".to_string(),
                    });
                }

                if sub.args.is_some() {
                    return Err(AstLoweringError {
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
            _ => Err(AstLoweringError {
                msg: format!("Complex path type annotations are not supported yet: {:#?}", path),
            }),
        }
    }

    fn lower_qualified_path_ty_annot(
        &mut self,
        qual_path: &ast::QualifiedPath,
    ) -> AstLoweringResult<hlr::TyAnnotDef<'ctxt>> {
        let [assoc_seg] = qual_path.path.segments.as_slice() else {
            return Err(AstLoweringError {
                msg: "Qualified path type annotations must have exactly one segment after '::'".to_string(),
            });
        };

        if assoc_seg.args.is_some() {
            return Err(AstLoweringError {
                msg: "Generic arguments on associated types in qualified paths are not supported".to_string(),
            });
        }

        let base = self.lower_ty_annot(qual_path.ty)?;

        let trait_ = qual_path
            .trait_
            .as_ref()
            .map(|trait_annot| {
                let trait_ =
                    self.ctxt
                        .traits
                        .resolve_trait_name(&trait_annot.name)
                        .ok_or_else(|| AstLoweringError {
                            msg: format!("Unknown trait name in qualified path: '{}'", trait_annot.name),
                        })?;
                let gen_args = trait_annot.args.map(|args| self.lower_ty_annots(args)).transpose()?;
                Ok((trait_, gen_args))
            })
            .transpose()?;

        Ok(hlr::TyAnnotDef::AssocTy {
            base,
            trait_,
            name: assoc_seg.ident.clone(),
        })
    }

    fn lower_path_segment_to_ty_annot(
        &mut self,
        segment: &ast::PathSegment,
    ) -> AstLoweringResult<hlr::TyAnnotDef<'ctxt>> {
        if segment.is_self {
            return Ok(hlr::TyAnnotDef::Self_);
        }

        let args = segment.args.map(|args| self.lower_ty_annots(args)).transpose()?;
        let resolved_ty = self.resolve_ident_to_ty(&segment.ident)?;

        match (resolved_ty, args) {
            (TyResolution::GenVar(gen_var), None) => Ok(hlr::TyAnnotDef::GenVar(gen_var)),
            (TyResolution::NamedTy(ctxt::Named::Ty(ty)), None) => Ok(hlr::TyAnnotDef::Ty(ty)),

            (TyResolution::NamedTy(ctxt::Named::Struct(struct_)), args) => Ok(hlr::TyAnnotDef::Struct(struct_, args)),
            (TyResolution::NamedTy(ctxt::Named::Enum(enum_)), args) => Ok(hlr::TyAnnotDef::Enum(enum_, args)),

            (TyResolution::GenVar(..) | TyResolution::NamedTy(ctxt::Named::Ty(..)), Some(_)) => Err(AstLoweringError {
                msg: format!(
                    "Type '{}' cannot have generic arguments in type annotation",
                    segment.ident
                ),
            }),
        }
    }

    fn lower_ty_annots(&mut self, ty_annots: ast::TyAnnotSlice) -> AstLoweringResult<hlr::TyAnnotSlice<'ctxt>> {
        ty_annots
            .iter()
            .map(|&annot| self.lower_ty_annot(annot))
            .collect::<AstLoweringResult<Vec<_>>>()
            .map(|annot_vec| self.hlr.ty_annot_slice(&annot_vec))
    }

    fn lower_lit(&mut self, lit: &ast::Lit) -> AstLoweringResult<hlr::Expr<'ctxt>> {
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

    fn lower_path(&mut self, path: &ast::Path) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let expr: hlr::ExprDef = match path.segments.as_slice() {
            [segment] if segment.is_self => {
                return Err(AstLoweringError {
                    msg: "Invalid use of 'Self' as value".to_string(),
                });
            }
            [segment] => {
                // Try fieldless struct construction before erroring (e.g. `MyUnit`)
                if segment.args.is_none()
                    && let Some(struct_) = self.ctxt.tys.get_struct_by_name(&segment.ident)
                {
                    let is_fieldless = struct_.get_fields().is_empty();
                    if is_fieldless {
                        let constructor = hlr::Val::Struct(struct_, None);
                        let fields = self.hlr.struct_expr_field_slice(vec![]);
                        return Ok(self.hlr.expr(hlr::ExprDef::Struct { constructor, fields }));
                    }
                }

                let val = self
                    .resolve_ident_to_val_def(&segment.ident)
                    .ok_or_else(|| AstLoweringError {
                        msg: format!("Unresolvable path: {}", segment.ident),
                    })?;
                let args = segment.args.map(|args| self.lower_ty_annots(args)).transpose()?;

                match (val, args) {
                    (val, None) => hlr::ExprDef::Val(val),
                    (hlr::Val::Fn(fn_, _), args) => hlr::ExprDef::Val(hlr::Val::Fn(fn_, args)),

                    (other_val, Some(_)) => {
                        return Err(AstLoweringError {
                            msg: format!("Only functions can be used in generic paths, found {:?}", other_val),
                        });
                    }
                }
            }
            [ty_path, mthd] => {
                if mthd.is_self {
                    return Err(AstLoweringError {
                        msg: "Invalid use of 'Self' as method name".to_string(),
                    });
                }

                // Try to resolve as a fieldless enum variant first (e.g. `Option::None`)
                if mthd.args.is_none()
                    && let Ok(constructor @ hlr::Val::Variant(enum_, variant_index, _)) =
                        self.resolve_path_segments_to_variant(ty_path, mthd)
                {
                    let variant_struct = enum_.get_variant(variant_index).struct_;
                    let is_fieldless = variant_struct.get_fields().is_empty();
                    if is_fieldless {
                        let fields = self.hlr.struct_expr_field_slice(vec![]);
                        return Ok(self.hlr.expr(hlr::ExprDef::Struct { constructor, fields }));
                    }
                }

                let ty = self.lower_path_segment_to_ty_annot(ty_path)?;
                let ty = self.hlr.ty_annot(ty);
                let args = mthd.args.map(|args| self.lower_ty_annots(args)).transpose()?;

                hlr::ExprDef::Val(hlr::Val::Mthd(ty, mthd.ident.clone(), args))
            }
            _ => {
                return Err(AstLoweringError {
                    msg: format!("Complex paths are not supported yet: {:#?}", path),
                });
            }
        };

        let expr = self.hlr.expr(expr);
        Ok(expr)
    }

    fn lower_qualified_path(&mut self, qualified_path: &ast::QualifiedPath) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let ty = self.lower_ty_annot(qualified_path.ty)?;

        let trait_ = qualified_path
            .trait_
            .as_ref()
            .map(|trait_annot| {
                self.ctxt
                    .traits
                    .resolve_trait_name(&trait_annot.name)
                    .ok_or_else(|| AstLoweringError {
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
            return Err(AstLoweringError {
                msg: format!(
                    "Only simple qualified paths are supported, found: {:#?}",
                    qualified_path.path
                ),
            });
        };

        let args = segment.args.map(|args| self.lower_ty_annots(args)).transpose()?;

        let expr = hlr::ExprDef::QualifiedMthd {
            ty,
            trait_,
            trait_args,
            mthd_name: segment.ident.clone(),
            args,
        };
        Ok(self.hlr.expr(expr))
    }

    fn lower_tuple_expr(&mut self, fields: ast::ExprSlice<'ast>) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let field_exprs: Vec<_> = fields
            .iter()
            .map(|&field| self.lower_expr(field))
            .collect::<AstLoweringResult<_>>()?;
        let field_exprs = self.hlr.expr_slice(&field_exprs);

        let expr = hlr::ExprDef::Tuple(field_exprs);
        Ok(self.hlr.expr(expr))
    }

    fn lower_binary_op(
        &mut self,
        left: ast::Expr<'ast>,
        operator: ast::BinaryOperator,
        right: ast::Expr<'ast>,
    ) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let left = self.lower_expr(left)?;
        let right = self.lower_expr(right)?;

        let expr = hlr::ExprDef::BinaryOp {
            left,
            operator: operator.into(),
            right,
        };
        Ok(self.hlr.expr(expr))
    }

    fn lower_unary_op(
        &mut self,
        operator: ast::UnaryOperator,
        operand: ast::Expr<'ast>,
    ) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let operand = self.lower_expr(operand)?;

        let expr = hlr::ExprDef::UnaryOp {
            operator: operator.into(),
            operand,
        };
        Ok(self.hlr.expr(expr))
    }

    fn lower_assign_expr(
        &mut self,
        target: ast::Expr<'ast>,
        value: ast::Expr<'ast>,
    ) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let target = self.lower_expr(target)?;
        let value = self.lower_expr(value)?;

        let expr = hlr::ExprDef::Assign { target, value };
        Ok(self.hlr.expr(expr))
    }

    fn lower_call_expr(
        &mut self,
        callee: ast::Expr<'ast>,
        args: ast::ExprSlice<'ast>,
    ) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let callee = self.lower_expr(callee)?;
        let args: Vec<_> = args
            .iter()
            .map(|&arg| self.lower_expr(arg))
            .collect::<AstLoweringResult<_>>()?;
        let args = self.hlr.expr_slice(&args);

        let expr = hlr::ExprDef::Call { callee, args };
        Ok(self.hlr.expr(expr))
    }

    fn lower_mthd_call_expr(
        &mut self,
        obj: ast::Expr<'ast>,
        mthd: &ast::PathSegment,
        args: ast::ExprSlice<'ast>,
    ) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let obj = self.lower_expr(obj)?;
        let args: Vec<_> = args
            .iter()
            .map(|&arg| self.lower_expr(arg))
            .collect::<AstLoweringResult<_>>()?;
        let args = self.hlr.expr_slice(&args);

        if mthd.is_self {
            return Err(AstLoweringError {
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
    ) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let constructor = self.resolve_path_to_constructor(ty_path)?;

        let fields: Vec<_> = fields
            .iter()
            .map(|(field_name, field_expr)| {
                let expr = self.lower_expr(field_expr)?;
                let field = hlr::FieldSpec::Name(field_name.clone());
                Ok((field, expr))
            })
            .collect::<AstLoweringResult<_>>()?;
        let fields = self.hlr.struct_expr_field_slice(fields);

        let expr = hlr::ExprDef::Struct { constructor, fields };
        Ok(self.hlr.expr(expr))
    }

    fn lower_field_access_expr(
        &mut self,
        obj: ast::Expr<'ast>,
        field: &ast::FieldDescriptor,
    ) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let obj = self.lower_expr(obj)?;

        let field_spec = match field {
            ast::FieldDescriptor::Named(name) => {
                if name.is_self {
                    return Err(AstLoweringError {
                        msg: "Invalid use of 'Self' as field name".to_string(),
                    });
                }
                if name.args.is_some() {
                    return Err(AstLoweringError {
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
    ) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let cond = self.lower_expr(cond)?;

        self.start_new_block();
        let then = self.build_block(then)?;
        let then = self.release_current_block(then);

        let else_ = else_
            .map(|else_| {
                self.start_new_block();
                let trailing = self.build_block(else_)?;
                Ok(self.release_current_block(trailing))
            })
            .transpose()?;

        let expr = hlr::ExprDef::If { cond, then, else_ };
        Ok(self.hlr.expr(expr))
    }

    fn lower_loop_expr(&mut self, body: ast::Block<'ast>) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        self.start_new_block();
        let body = self.build_block(body)?;
        let body = self.release_current_block(body);

        let expr = hlr::ExprDef::Loop { body };
        Ok(self.hlr.expr(expr))
    }

    fn lower_while_expr(
        &mut self,
        cond: ast::Expr<'ast>,
        body: ast::Block<'ast>,
    ) -> AstLoweringResult<hlr::Expr<'ctxt>> {
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

    fn lower_for_expr(
        &mut self,
        binding: &str,
        mutable: bool,
        iter: ast::Expr<'ast>,
        body: ast::Block<'ast>,
    ) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        // Desugar `for x in expr { body }` into:
        //   {
        //       let mut __for_iter = <_ as IntoIterator>::into_iter(expr);
        //       loop {
        //           match <_ as Iterator>::next(&mut __for_iter) {
        //               Option::Some { item: x } => { body }
        //               Option::None => { break }
        //           }
        //       }
        //   }

        let into_iterator_trait = self.ctxt.language_items.into_iterator_trait.unwrap();
        let iterator_trait = self.ctxt.language_items.iterator_trait.unwrap();
        let option_enum = self.ctxt.tys.get_enum_by_name("Option").unwrap();
        let (some_index, _) = option_enum.resolve_variant_by_name("Some").unwrap();
        let (none_index, _) = option_enum.resolve_variant_by_name("None").unwrap();

        let infer = self.hlr.ty_annot(hlr::TyAnnotDef::Infer);

        self.start_new_block();

        // let mut __for_iter = <_ as IntoIterator>::into_iter(expr)
        let iter_expr = self.lower_expr(iter)?;
        let into_iter_fn = self.hlr.expr(hlr::ExprDef::QualifiedMthd {
            ty: infer,
            trait_: Some(into_iterator_trait),
            trait_args: None,
            mthd_name: "into_iter".to_string(),
            args: None,
        });
        let into_iter_call = self.hlr.expr(hlr::ExprDef::Call {
            callee: into_iter_fn,
            args: self.hlr.expr_slice(&[iter_expr]),
        });
        let iter_var = self.hlr.var_id();
        self.push_stmt(hlr::StmtDef::Let {
            var: iter_var,
            mutable: true,
            ty: None,
            init: into_iter_call,
        })?;

        // <_ as Iterator>::next(&mut __for_iter)
        let next_fn = self.hlr.expr(hlr::ExprDef::QualifiedMthd {
            ty: infer,
            trait_: Some(iterator_trait),
            trait_args: None,
            mthd_name: "next".to_string(),
            args: None,
        });
        let iter_var_expr = self.hlr.expr(hlr::ExprDef::Val(hlr::Val::Var(iter_var)));
        let iter_var_ref = self.hlr.expr(hlr::ExprDef::AddrOfMut(iter_var_expr));
        let next_call = self.hlr.expr(hlr::ExprDef::Call {
            callee: next_fn,
            args: self.hlr.expr_slice(&[iter_var_ref]),
        });

        // Some arm: Option::Some { item: binding_var } => body
        let binding_var = self.hlr.var_id();
        let some_pattern = hlr::VariantPattern {
            variant: hlr::Val::Variant(option_enum, some_index, None),
            fields: self.hlr.variant_pattern_fields(vec![hlr::VariantPatternField {
                field_index: 0,
                binding: binding_var,
                mutable,
            }]),
        };
        self.scopes.push_back(Scope::default());
        self.scopes
            .back_mut()
            .unwrap()
            .bindings
            .insert(binding.to_string(), binding_var);
        self.start_new_block();
        let body_trailing = self.build_block(body)?;
        let some_body = self.release_current_block(body_trailing);
        self.scopes.pop_back();

        // None arm: Option::None => { break }
        let none_pattern = hlr::VariantPattern {
            variant: hlr::Val::Variant(option_enum, none_index, None),
            fields: self.hlr.variant_pattern_fields(vec![]),
        };
        self.start_new_block();
        self.lower_break_stmt()?;
        let unit = self.hlr.expr(hlr::ExprDef::Tuple(self.hlr.expr_slice(&[])));
        let none_body = self.release_current_block(unit);

        // match <_ as Iterator>::next(&mut __for_iter) { ... }
        let arms = self.hlr.match_arms(vec![
            hlr::MatchArm {
                pattern: some_pattern,
                body: some_body,
            },
            hlr::MatchArm {
                pattern: none_pattern,
                body: none_body,
            },
        ]);
        let match_expr = self.hlr.expr(hlr::ExprDef::Match {
            scrutinee: next_call,
            arms,
        });

        // loop { match ... }
        let loop_expr = self.hlr.expr(hlr::ExprDef::Loop { body: match_expr });

        Ok(self.release_current_block(loop_expr))
    }

    fn lower_match_expr(
        &mut self,
        scrutinee: ast::Expr<'ast>,
        arms: &[ast::MatchArm<'ast>],
    ) -> AstLoweringResult<hlr::Expr<'ctxt>> {
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
            .collect::<AstLoweringResult<_>>()?;
        let hlr_arms = self.hlr.match_arms(hlr_arms);

        let expr = hlr::ExprDef::Match {
            scrutinee,
            arms: hlr_arms,
        };
        Ok(self.hlr.expr(expr))
    }

    fn lower_pattern(&mut self, pattern: &ast::VariantPattern) -> AstLoweringResult<hlr::Pattern<'ctxt>> {
        let variant = self.resolve_path_to_constructor(&pattern.variant)?;

        let hlr::Val::Variant(enum_, variant_index, ..) = variant else {
            return Err(AstLoweringError {
                msg: format!("Only enum variants are supported in patterns, found {:?}", variant),
            });
        };

        let variant_struct = enum_.get_variant(variant_index).struct_;

        let variant_fields = variant_struct.get_fields();
        if pattern.fields.len() != variant_fields.len() {
            return Err(AstLoweringError {
                msg: format!(
                    "Pattern for variant has wrong number of fields: expected {}, found {}",
                    variant_fields.len(),
                    pattern.fields.len()
                ),
            });
        }

        let fields: Vec<_> = pattern
            .fields
            .iter()
            .map(|field| {
                let field_index = variant_fields
                    .iter()
                    .position(|f| f.name == field.field_name)
                    .ok_or_else(|| AstLoweringError {
                        msg: format!(
                            "Unknown field '{}' in pattern for variant '{}'",
                            field.field_name, enum_.name
                        ),
                    })?;

                let binding = self.hlr.var_id();
                self.scopes
                    .back_mut()
                    .unwrap()
                    .bindings
                    .insert(field.binding_name.clone(), binding);

                // TODO: allow `mut` on individual struct pattern bindings, e.g. `Some(mut x)`
                Ok(hlr::VariantPatternField {
                    field_index,
                    binding,
                    mutable: false,
                })
            })
            .collect::<AstLoweringResult<_>>()?;
        let fields = self.hlr.variant_pattern_fields(fields);

        Ok(hlr::VariantPattern { variant, fields })
    }

    fn lower_deref_expr(&mut self, base: ast::Expr<'ast>) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let base = self.lower_expr(base)?;
        let expr = hlr::ExprDef::Deref(base);
        Ok(self.hlr.expr(expr))
    }

    fn lower_addr_of_expr(&mut self, base: ast::Expr<'ast>) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let base = self.lower_expr(base)?;
        let expr = hlr::ExprDef::AddrOf(base);
        Ok(self.hlr.expr(expr))
    }

    fn lower_addr_of_mut_expr(&mut self, base: ast::Expr<'ast>) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let base = self.lower_expr(base)?;
        let expr = hlr::ExprDef::AddrOfMut(base);
        Ok(self.hlr.expr(expr))
    }

    fn lower_as_expr(&mut self, expr: ast::Expr<'ast>, target_ty: ast::TyAnnot) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let expr = self.lower_expr(expr)?;
        let ty = self.lower_ty_annot(target_ty)?;
        let expr = hlr::ExprDef::As { expr, ty };
        Ok(self.hlr.expr(expr))
    }

    fn lower_self_expr(&mut self) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let self_var_id = self.self_var_id.ok_or_else(|| AstLoweringError {
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
    ) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        self.scopes.push_back(Scope::default());

        let params = params
            .iter()
            .map(|param| -> AstLoweringResult<hlr::ClosureParam<'ctxt>> {
                let ty = param.ty.map(|ty| self.lower_ty_annot(ty)).transpose()?;
                let var_id = self.hlr.var_id();
                self.scopes
                    .back_mut()
                    .unwrap()
                    .bindings
                    .insert(param.name.clone(), var_id);
                Ok(hlr::ClosureParam(var_id, ty))
            })
            .collect::<AstLoweringResult<Vec<_>>>()?;
        let params = self.hlr.closure_params(params);

        self.start_new_block();
        let trailing = self.build_block(body)?;
        let body = self.release_current_block(trailing);

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
