#![allow(unused)]

use std::collections::{HashMap, VecDeque};

use crate::{
    ast,
    ctxt::{self, fns},
    hlr,
};

pub fn ast_to_hlr(
    ctxt: &ctxt::Ctxt,
    fn_: fns::Fn,
    ast: &ast::Ast,
    ast_body: &ast::Block,
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

    pub fn lower_block(mut self, block: &ast::Block) -> AstToHlrResult<(hlr::Hlr, hlr::Expr)> {
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

    fn build_block(&mut self, body: &ast::Block) -> AstToHlrResult<hlr::Expr> {
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
            Path(path) => todo!(),
            QualifiedPath(qualified_path) => todo!(),
            &Tuple(fields) => self.lower_tuple_expr(fields),
            &BinaryOp { left, operator, right } => self.lower_binary_op(left, operator, right),
            &UnaryOp { operator, operand } => self.lower_unary_op(operator, operand),
            &Assign { target, value } => self.lower_assign_expr(target, value),
            &Call { callee, args } => self.lower_call_expr(callee, args),
            &MthdCall { obj, ref mthd, args } => self.lower_method_call_expr(obj, mthd, args),
            Struct { ty_path, fields } => self.lower_struct_expr(ty_path, fields),
            &FieldAccess { obj, ref field } => self.lower_field_access_expr(obj, field),
            Block(block) => self.build_block(block),
            If { cond, then, else_ } => todo!(),
            Loop { body } => todo!(),
            While { cond, body } => todo!(),
            Match { scrutinee, arms } => todo!(),
            Deref { base } => todo!(),
            AddrOf { base } => todo!(),
            As { expr, target_ty } => todo!(),
            Self_ => todo!(),
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
                match sub {
                    ast::PathSegment::Ident(name) => {
                        let base = self.lower_path_segment_to_ty_annot(parent)?;
                        Ok(hlr::TyAnnotDef::AssocTy {
                            base: self.hlr.new_ty_annot(base),
                            trait_: None,
                            name: name.clone(),
                        })
                    }
                    ast::PathSegment::Generic(gen_path_segment) => Err(AstToHlrError {
                        msg: "Generic associated types are not supported".to_string(),
                    }),
                    ast::PathSegment::Self_ => Err(AstToHlrError {
                        msg: "Invalid use of 'Self' in type annotation".to_string(),
                    }),
                }
            }
            _ => Err(AstToHlrError {
                msg: format!("Complex path type annotations are not supported yet: {}", path),
            }),
        }
    }

    fn lower_path_segment_to_ty_annot(&mut self, segment: &ast::PathSegment) -> AstToHlrResult<hlr::TyAnnotDef> {
        match segment {
            ast::PathSegment::Ident(ident) => {
                let sig = self.get_signature();
                // Try to resolve to generic var
                for &gen_var in &sig.gen_params {
                    if self.ctxt.tys.get_gen_var_name(gen_var) == ident {
                        // Resolve to generic var
                        return Ok(hlr::TyAnnotDef::GenVar(gen_var));
                    }
                }
                // Try to resolve to env generic var
                for &gen_var in &sig.env_gen_params {
                    if self.ctxt.tys.get_gen_var_name(gen_var) == ident {
                        // Resolve to generic var
                        return Ok(hlr::TyAnnotDef::GenVar(gen_var));
                    }
                }

                // Resolve to named type (struct/enum/primitive)
                let named_ty = self.ctxt.tys.get_ty_by_name(ident).map_err(|_| AstToHlrError {
                    msg: format!("Unknown type name in type annotation: {}", ident),
                })?;

                match *named_ty {
                    ctxt::Named::Ty(ty) => Ok(hlr::TyAnnotDef::Ty(ty)),
                    ctxt::Named::Struct(struct_) => Ok(hlr::TyAnnotDef::Struct(struct_, None)),
                    ctxt::Named::Enum(enum_) => Ok(hlr::TyAnnotDef::Enum(enum_, None)),
                }
            }
            ast::PathSegment::Generic(gen_segment) => {
                // Resolve to named type (struct/enum/primitive)
                let named_ty = self
                    .ctxt
                    .tys
                    .get_ty_by_name(&gen_segment.ident)
                    .map_err(|_| AstToHlrError {
                        msg: format!("Unknown type name in type annotation: {}", gen_segment.ident),
                    })?;

                let gen_args = gen_segment
                    .gen_args
                    .iter()
                    .map(|&arg| self.lower_ty_annot(arg))
                    .collect::<AstToHlrResult<_>>()?;

                // Resolve to named type (struct/enum) with generics
                match *named_ty {
                    ctxt::Named::Ty(ty) => Err(AstToHlrError {
                        msg: format!(
                            "Primitive type '{}' cannot have generic arguments in type annotation",
                            gen_segment.ident
                        ),
                    }),
                    ctxt::Named::Struct(struct_) => Ok(hlr::TyAnnotDef::Struct(struct_, Some(gen_args))),
                    ctxt::Named::Enum(enum_) => Ok(hlr::TyAnnotDef::Enum(enum_, Some(gen_args))),
                }
            }
            ast::PathSegment::Self_ => Ok(hlr::TyAnnotDef::Self_),
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
        let arg = self
            .ast
            .expr_slice(args)
            .iter()
            .map(|&arg| self.lower_expr(arg))
            .collect::<AstToHlrResult<_>>()?;

        let (method_name, gen_args) = match mthd {
            ast::PathSegment::Ident(name) => (name.clone(), None),
            ast::PathSegment::Generic(gen_path_segment) => {
                let method_name = gen_path_segment.ident.clone();
                let gen_args = gen_path_segment
                    .gen_args
                    .iter()
                    .map(|&arg| self.lower_ty_annot(arg))
                    .collect::<AstToHlrResult<_>>()?;
                (method_name, Some(gen_args))
            }
            ast::PathSegment::Self_ => {
                return Err(AstToHlrError {
                    msg: "Invalid use of 'Self' as method name".to_string(),
                });
            }
        };

        let expr = hlr::ExprDef::MethodCall {
            receiver: obj,
            method_name,
            gen_args,
            args: arg,
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
            gen_args,
        };
        Ok(self.hlr.new_expr(expr))
    }

    fn resolve_path_to_constructor(
        &mut self,
        ty_path: &ast::Path,
    ) -> AstToHlrResult<(hlr::Def, Option<Vec<hlr::TyAnnot>>)> {
        match ty_path.segments.as_slice() {
            [simple] => {
                let (struct_, gen_args) = match simple {
                    ast::PathSegment::Ident(ident) => {
                        let struct_ = self.ctxt.tys.get_struct_by_name(ident).ok_or_else(|| AstToHlrError {
                            msg: format!("Unknown struct name in struct literal: {}", ident),
                        })?;
                        (struct_, None)
                    }
                    ast::PathSegment::Generic(gen_segment) => {
                        let struct_ =
                            self.ctxt
                                .tys
                                .get_struct_by_name(&gen_segment.ident)
                                .ok_or_else(|| AstToHlrError {
                                    msg: format!("Unknown struct name in struct literal: {}", gen_segment.ident),
                                })?;
                        let gen_args = gen_segment
                            .gen_args
                            .iter()
                            .map(|&arg| self.lower_ty_annot(arg))
                            .collect::<AstToHlrResult<_>>()?;
                        (struct_, Some(gen_args))
                    }
                    ast::PathSegment::Self_ => {
                        return Err(AstToHlrError {
                            msg: "Invalid use of 'Self' as struct literal constructor".to_string(),
                        });
                    }
                };
                Ok((hlr::Def::Struct(struct_), gen_args))
            }
            [enum_, variant_name] => {
                let (enum_, gen_args) = match enum_ {
                    ast::PathSegment::Ident(ident) => {
                        let enum_ = self.ctxt.tys.get_enum_by_name(ident).ok_or_else(|| AstToHlrError {
                            msg: format!("Unknown enum name in struct literal: {}", ident),
                        })?;
                        (enum_, None)
                    }
                    ast::PathSegment::Generic(gen_segment) => {
                        let enum_ =
                            self.ctxt
                                .tys
                                .get_enum_by_name(&gen_segment.ident)
                                .ok_or_else(|| AstToHlrError {
                                    msg: format!("Unknown enum name in struct literal: {}", gen_segment.ident),
                                })?;
                        let gen_args = gen_segment
                            .gen_args
                            .iter()
                            .map(|&arg| self.lower_ty_annot(arg))
                            .collect::<AstToHlrResult<_>>()?;
                        (enum_, Some(gen_args))
                    }
                    ast::PathSegment::Self_ => {
                        return Err(AstToHlrError {
                            msg: "Invalid use of 'Self' as enum literal constructor".to_string(),
                        });
                    }
                };

                let variant_name = match variant_name {
                    ast::PathSegment::Ident(name) => name,
                    ast::PathSegment::Generic(_) => {
                        return Err(AstToHlrError {
                            msg: "Generic parameters are not allowed on enum variants in struct literals".to_string(),
                        });
                    }
                    ast::PathSegment::Self_ => {
                        return Err(AstToHlrError {
                            msg: "Invalid use of 'Self' as enum variant name in struct literal".to_string(),
                        });
                    }
                };

                let variant_index = self
                    .ctxt
                    .tys
                    .get_enum_def(enum_)
                    .unwrap()
                    .variants
                    .iter()
                    .position(|variant| &variant.name == variant_name)
                    .ok_or_else(|| AstToHlrError {
                        msg: format!(
                            "Unknown variant '{}' for enum '{}' in struct literal",
                            variant_name, enum_.0
                        ),
                    })?;

                Ok((hlr::Def::Variant(enum_, variant_index), gen_args))
            }
            _ => Err(AstToHlrError {
                msg: format!("Complex paths in struct literals are not supported yet: {}", ty_path),
            }),
        }
    }

    fn lower_field_access_expr(&mut self, obj: ast::Expr, field: &ast::FieldDescriptor) -> AstToHlrResult<hlr::Expr> {
        let obj = self.lower_expr(obj)?;

        let field_spec = match field {
            ast::FieldDescriptor::Named(path_segment) => match path_segment {
                ast::PathSegment::Ident(name) => hlr::FieldSpec::Name(name.clone()),
                _ => {
                    return Err(AstToHlrError {
                        msg: "Only identifiers are allowed as fields, got generic.".to_string(),
                    });
                }
            },
            ast::FieldDescriptor::Indexed(index) => hlr::FieldSpec::Index(*index),
        };

        let expr = hlr::ExprDef::FieldAccess {
            base: obj,
            field: field_spec,
        };
        Ok(self.hlr.new_expr(expr))
    }
}

#[derive(Default)]
struct Scope {
    bindings: HashMap<String, hlr::VarId>,
}
