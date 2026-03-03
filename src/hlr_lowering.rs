mod lowered_expr;

use std::collections::HashMap;

use lowered_expr::LoweredExpr;

use crate::{
    ctxt::{self, fns, language_items, ty},
    hlr,
    mlr::{self, builder::MlrBuilder},
    typeck::{ExprExtra, HlrTyping, MthdResolution},
};

pub fn hlr_to_mlr<'hlr, 'mlr>(
    ctxt: &mut ctxt::Ctxt,
    mlr: &'mlr mlr::Mlr<'mlr>,
    fn_: &'hlr hlr::Fn<'hlr>,
    typing: &'hlr HlrTyping,
) -> Vec<mlr::Fn<'mlr>> {
    let mut lowerer = HlrLowerer::new(ctxt, mlr, fn_.fn_, typing);
    lowerer.lower_fn(fn_)
}

struct HlrLowerer<'ctxt, 'hlr, 'mlr> {
    fn_: fns::Fn,
    builder: MlrBuilder<'ctxt, 'mlr>,
    typing: &'ctxt HlrTyping,
    var_locs: HashMap<hlr::VarId, mlr::Loc>,
    mlr_fns: Vec<mlr::Fn<'mlr>>,
    _hlr: std::marker::PhantomData<&'hlr hlr::Hlr<'hlr>>,
}

impl<'ctxt, 'hlr, 'mlr> HlrLowerer<'ctxt, 'hlr, 'mlr> {
    fn new(ctxt: &'ctxt mut ctxt::Ctxt, mlr: &'mlr mlr::Mlr<'mlr>, fn_: fns::Fn, typing: &'ctxt HlrTyping) -> Self {
        Self {
            fn_,
            builder: MlrBuilder::new(ctxt, mlr, fn_),
            typing,
            var_locs: HashMap::new(),
            mlr_fns: Vec::new(),
            _hlr: std::marker::PhantomData,
        }
    }

    fn lower_fn(&mut self, fn_: &'hlr hlr::Fn<'hlr>) -> Vec<mlr::Fn<'mlr>> {
        let mlr_fn = self.lower_body(&fn_.param_var_ids, None, fn_.body);
        self.mlr_fns.push(mlr_fn);
        std::mem::take(&mut self.mlr_fns)
    }

    fn lower_body(
        &mut self,
        param_var_ids: &[hlr::VarId],
        captured_vars: Option<&[hlr::VarId]>,
        body: hlr::Expr<'hlr>,
    ) -> mlr::Fn<'mlr> {
        let sig = self.builder.ctxt.fns.get_sig(self.fn_).unwrap().clone();
        let mut param_locs = Vec::new();

        // For closures, the first sig param is the captures struct (no VarId binding)
        let regular_params = if captured_vars.is_some() {
            let captures_loc = self.builder.insert_typed_loc(sig.params[0].ty);
            param_locs.push(captures_loc);
            &sig.params[1..]
        } else {
            &sig.params[..]
        };
        for (param, &var_id) in regular_params.iter().zip(param_var_ids) {
            let loc = self.builder.insert_typed_loc(param.ty);
            param_locs.push(loc);
            self.var_locs.insert(var_id, loc);
        }

        self.builder.start_block();

        // Extract captured vars from the captures struct into individual locals
        if let Some(captured_vars) = captured_vars {
            let captures_ty = sig.params[0].ty;
            let captures_place = self.builder.insert_loc_place(param_locs[0]);
            for (i, &var_id) in captured_vars.iter().enumerate() {
                let field_ty = self.builder.ctxt.tys.get_struct_field_ty(captures_ty, i).unwrap();
                let field_place = self.builder.insert_field_access_place(captures_place, i, field_ty);
                let use_val = self.builder.copy_val(field_place);
                let loc = self.builder.alloc_loc(field_ty);
                self.builder.insert_assign_to_loc_stmt(loc, use_val);
                self.var_locs.insert(var_id, loc);
            }
        }

        let body_val = self.lower_to_val(body);
        self.builder.insert_return_stmt(body_val);
        let body = self.builder.end_block();

        mlr::Fn {
            fn_: self.fn_,
            body,
            param_locs,
        }
    }

    fn lower_expr(&mut self, expr: hlr::Expr<'hlr>) -> LoweredExpr<'mlr> {
        let (expr_def, expr_id) = (expr.0, expr.1);

        use hlr::ExprDef::*;
        match expr_def {
            Lit(lit) => self.lower_lit(lit),
            Val(val) => self.lower_val(val, expr_id),
            BinaryOp { left, right, operator } => self.lower_binary_op(expr_id, *left, *right, *operator),
            UnaryOp { operand, operator } => self.lower_unary_op(expr_id, *operand, *operator),
            Call { callee, args } => self.lower_call(*callee, args),
            MthdCall { receiver, args, .. } => self.lower_mthd_call(expr_id, *receiver, args),
            Struct { constructor, fields } => self.lower_struct_expr(expr_id, constructor, fields),
            FieldAccess { base, .. } => self.lower_field_access(expr_id, *base),
            Tuple(exprs) => self.lower_tuple(exprs),
            Assign { target, value } => self.lower_assign(*target, *value),
            Deref(inner) => self.lower_deref(*inner),
            AddrOf(inner) => self.lower_addr_of(*inner),
            As { expr: inner, .. } => self.lower_as(expr_id, *inner),
            Closure { params, body, .. } => self.lower_closure(expr_id, params, *body),
            If { cond, then, else_ } => self.lower_if(expr_id, *cond, *then, *else_),
            Loop { body } => self.lower_loop(*body),
            Match { scrutinee, arms } => self.lower_match(expr_id, *scrutinee, arms),
            Block { stmts, trailing } => self.lower_block(stmts, *trailing),
            QualifiedMthd { .. } => self.lower_qualified_mthd(expr_id),
        }
    }

    fn lower_to_val(&mut self, expr: hlr::Expr<'hlr>) -> mlr::Val<'mlr> {
        self.lower_expr(expr).into_val(&mut self.builder)
    }

    fn lower_to_place(&mut self, expr: hlr::Expr<'hlr>) -> mlr::Place<'mlr> {
        self.lower_expr(expr).into_place(&mut self.builder)
    }

    fn lower_to_op(&mut self, expr: hlr::Expr<'hlr>) -> mlr::Op<'mlr> {
        self.lower_expr(expr).into_op(&mut self.builder)
    }

    fn lower_lit(&mut self, lit: &hlr::Lit) -> LoweredExpr<'mlr> {
        let (const_, ty) = match lit {
            hlr::Lit::Int(n) => {
                let ty = self.builder.ctxt.tys.primitive(ty::Primitive::Integer32);
                (mlr::Const::Int(*n), ty)
            }
            hlr::Lit::Bool(b) => {
                let ty = self.builder.ctxt.tys.primitive(ty::Primitive::Boolean);
                (mlr::Const::Bool(*b), ty)
            }
            hlr::Lit::CChar(c) => {
                let ty = self.builder.ctxt.tys.primitive(ty::Primitive::CChar);
                (mlr::Const::CChar(*c), ty)
            }
            hlr::Lit::CString(s) => {
                let c_char_ty = self.builder.ctxt.tys.primitive(ty::Primitive::CChar);
                let ty = self.builder.ctxt.tys.ptr(c_char_ty);
                (mlr::Const::CString(s.clone()), ty)
            }
        };
        self.builder.insert_const_op(const_, ty).into()
    }

    fn lower_val(&mut self, val: &hlr::Val<'hlr>, expr_id: hlr::ExprId) -> LoweredExpr<'mlr> {
        match val {
            hlr::Val::Var(var_id) => {
                let loc = self.var_locs[var_id];
                self.builder.insert_loc_place(loc).into()
            }
            hlr::Val::Fn(..) => {
                let fn_inst = match self.typing.expr_extra[&expr_id] {
                    ExprExtra::ValFn(fi) => fi,
                    _ => panic!("expected ValFn extra"),
                };
                self.builder.insert_fn_inst_op(fn_inst).into()
            }
            hlr::Val::Mthd(..) => {
                let resolution = match &self.typing.expr_extra[&expr_id] {
                    ExprExtra::ValMthd(r) => r.clone(),
                    _ => panic!("expected ValMthd extra"),
                };
                let op = self.lower_mthd_resolution_to_op(&resolution);
                op.into()
            }
            hlr::Val::Struct(..) | hlr::Val::Variant(..) => {
                unreachable!("Val::Struct/Variant only appear as constructors in Struct expressions")
            }
        }
    }

    fn lower_binary_op(
        &mut self,
        expr_id: hlr::ExprId,
        left: hlr::Expr<'hlr>,
        right: hlr::Expr<'hlr>,
        operator: hlr::BinaryOperator,
    ) -> LoweredExpr<'mlr> {
        let val = match operator {
            hlr::BinaryOperator::LogicalAnd => self.lower_logical_and(left, right),
            hlr::BinaryOperator::LogicalOr => self.lower_logical_or(left, right),
            _ => match self.typing.expr_extra[&expr_id] {
                ExprExtra::BinaryPrim(prim) => {
                    let result_ty = self.typing.expr_types[&expr_id];
                    let lhs = self.lower_to_op(left);
                    let rhs = self.lower_to_op(right);
                    self.builder.insert_binary_prim_val(prim, lhs, rhs, result_ty)
                }
                _ => panic!("expected BinaryOp or BinaryPrim extra"),
            },
        };
        val.into()
    }

    fn lower_logical_and(&mut self, left: hlr::Expr<'hlr>, right: hlr::Expr<'hlr>) -> mlr::Val<'mlr> {
        let bool_ty = self.builder.ctxt.tys.primitive(ty::Primitive::Boolean);
        let result_place = self.builder.alloc_place(bool_ty);

        let left_op = self.lower_to_op(left);

        self.builder.start_block();
        let right_op = self.lower_to_op(right);
        let right_val = self.builder.insert_use_val(right_op);
        self.builder.insert_assign_stmt(result_place, right_val);
        let then_block = self.builder.end_block();

        self.builder.start_block();
        let false_op = self.builder.insert_bool_const(false);
        let false_val = self.builder.insert_use_val(false_op);
        self.builder.insert_assign_stmt(result_place, false_val);
        let else_block = self.builder.end_block();

        self.builder.insert_if_stmt(left_op, then_block, else_block);
        self.builder.copy_val(result_place)
    }

    fn lower_logical_or(&mut self, left: hlr::Expr<'hlr>, right: hlr::Expr<'hlr>) -> mlr::Val<'mlr> {
        let bool_ty = self.builder.ctxt.tys.primitive(ty::Primitive::Boolean);
        let result_place = self.builder.alloc_place(bool_ty);

        let left_op = self.lower_to_op(left);

        self.builder.start_block();
        let true_op = self.builder.insert_bool_const(true);
        let true_val = self.builder.insert_use_val(true_op);
        self.builder.insert_assign_stmt(result_place, true_val);
        let then_block = self.builder.end_block();

        self.builder.start_block();
        let right_op = self.lower_to_op(right);
        let right_val = self.builder.insert_use_val(right_op);
        self.builder.insert_assign_stmt(result_place, right_val);
        let else_block = self.builder.end_block();

        self.builder.insert_if_stmt(left_op, then_block, else_block);
        self.builder.copy_val(result_place)
    }

    fn lower_unary_op(
        &mut self,
        expr_id: hlr::ExprId,
        operand: hlr::Expr<'hlr>,
        _operator: hlr::UnaryOperator,
    ) -> LoweredExpr<'mlr> {
        let prim = match self.typing.expr_extra[&expr_id] {
            ExprExtra::UnaryPrim(p) => p,
            _ => panic!("expected UnaryPrim extra"),
        };
        let result_ty = self.typing.expr_types[&expr_id];
        let operand_op = self.lower_to_op(operand);
        self.builder.insert_unary_prim_val(prim, operand_op, result_ty).into()
    }

    fn lower_call(&mut self, callee: hlr::Expr<'hlr>, args: hlr::ExprSlice<'hlr>) -> LoweredExpr<'mlr> {
        let callee_op = self.lower_to_op(callee);
        let mut call_args = Vec::new();
        for &arg in args {
            call_args.push(self.lower_to_op(arg));
        }
        self.builder.insert_call_val(callee_op, call_args).into()
    }

    fn lower_mthd_call(
        &mut self,
        expr_id: hlr::ExprId,
        receiver: hlr::Expr<'hlr>,
        args: hlr::ExprSlice<'hlr>,
    ) -> LoweredExpr<'mlr> {
        let resolution = match &self.typing.expr_extra[&expr_id] {
            ExprExtra::ValMthd(r) => r.clone(),
            _ => panic!("expected ValMthd extra for MthdCall"),
        };
        let (callee_op, by_ref) = self.mthd_resolution_to_op(&resolution);

        let receiver_place = self.lower_to_place(receiver);
        let receiver_op = if by_ref {
            let addr_val = self.builder.insert_addr_of_val(receiver_place);
            let addr_place = self.builder.store_val(addr_val);
            self.builder.insert_copy_op(addr_place)
        } else {
            self.builder.insert_copy_op(receiver_place)
        };

        let mut call_args = vec![receiver_op];
        for &arg in args {
            call_args.push(self.lower_to_op(arg));
        }
        self.builder.insert_call_val(callee_op, call_args).into()
    }

    fn lower_qualified_mthd(&mut self, expr_id: hlr::ExprId) -> LoweredExpr<'mlr> {
        let resolution = match &self.typing.expr_extra[&expr_id] {
            ExprExtra::ValMthd(r) => r.clone(),
            _ => panic!("expected ValMthd extra for QualifiedMthd"),
        };
        self.lower_mthd_resolution_to_op(&resolution).into()
    }

    fn lower_struct_expr(
        &mut self,
        expr_id: hlr::ExprId,
        constructor: &hlr::Val<'hlr>,
        fields: hlr::StructFields<'hlr>,
    ) -> LoweredExpr<'mlr> {
        let expr_ty = self.typing.expr_types[&expr_id];
        let val = match constructor {
            hlr::Val::Struct(..) => self.lower_struct_val(expr_ty, fields),
            hlr::Val::Variant(_, variant_idx, _) => self.lower_enum_val(expr_ty, *variant_idx, fields),
            _ => unreachable!(),
        };
        val.into()
    }

    fn lower_struct_val(&mut self, struct_ty: ty::Ty, fields: hlr::StructFields<'hlr>) -> mlr::Val<'mlr> {
        let struct_place = self.builder.alloc_place(struct_ty);

        for (field_spec, field_expr) in fields.iter() {
            let field_index = match field_spec {
                hlr::FieldSpec::Name(name) => self
                    .builder
                    .ctxt
                    .tys
                    .get_struct_field_index_by_name(struct_ty, name)
                    .unwrap_or_else(|_| panic!("struct field {name} should exist")),
                hlr::FieldSpec::Index(idx) => *idx,
            };
            let field_ty = self
                .builder
                .ctxt
                .tys
                .get_struct_field_ty(struct_ty, field_index)
                .unwrap();
            let field_place = self
                .builder
                .insert_field_access_place(struct_place, field_index, field_ty);
            let val = self.lower_to_val(*field_expr);
            self.builder.insert_assign_stmt(field_place, val);
        }

        self.builder.copy_val(struct_place)
    }

    fn lower_enum_val(
        &mut self,
        enum_ty: ty::Ty,
        variant_idx: usize,
        fields: hlr::StructFields<'hlr>,
    ) -> mlr::Val<'mlr> {
        let enum_place = self.builder.alloc_place(enum_ty);

        let discriminant_place = self.builder.insert_enum_discriminant_place(enum_place);
        let i32_ty = self.builder.ctxt.tys.primitive(ty::Primitive::Integer32);
        let disc_op = self
            .builder
            .insert_const_op(mlr::Const::Int(variant_idx as i64), i32_ty);
        let disc_val = self.builder.insert_use_val(disc_op);
        self.builder.insert_assign_stmt(discriminant_place, disc_val);

        let variant_ty = self.builder.ctxt.tys.get_enum_variant_ty(enum_ty, variant_idx).unwrap();
        let variant_place = self
            .builder
            .insert_project_to_variant_place(enum_place, variant_idx, variant_ty);

        for (field_spec, field_expr) in fields.iter() {
            let field_index = match field_spec {
                hlr::FieldSpec::Name(name) => self
                    .builder
                    .ctxt
                    .tys
                    .get_struct_field_index_by_name(variant_ty, name)
                    .unwrap_or_else(|_| panic!("variant field {name} should exist")),
                hlr::FieldSpec::Index(idx) => *idx,
            };
            let field_ty = self
                .builder
                .ctxt
                .tys
                .get_struct_field_ty(variant_ty, field_index)
                .unwrap();
            let field_place = self
                .builder
                .insert_field_access_place(variant_place, field_index, field_ty);
            let val = self.lower_to_val(*field_expr);
            self.builder.insert_assign_stmt(field_place, val);
        }

        self.builder.copy_val(enum_place)
    }

    fn lower_field_access(&mut self, expr_id: hlr::ExprId, base: hlr::Expr<'hlr>) -> LoweredExpr<'mlr> {
        let (derefs, field_index) = match self.typing.expr_extra[&expr_id] {
            ExprExtra::FieldAccess { derefs, index } => (derefs, index),
            _ => panic!("expected FieldAccess extra"),
        };

        let mut place = self.lower_to_place(base);

        for _ in 0..derefs {
            let op = self.builder.insert_copy_op(place);
            place = self.builder.insert_deref_place(op);
        }

        let field_ty = self.typing.expr_types[&expr_id];
        self.builder
            .insert_field_access_place(place, field_index, field_ty)
            .into()
    }

    fn lower_tuple(&mut self, exprs: hlr::ExprSlice<'hlr>) -> LoweredExpr<'mlr> {
        let exprs = exprs.to_vec();
        let elem_tys: Vec<ty::Ty> = exprs.iter().map(|e| self.typing.expr_types[&e.1]).collect();
        let tuple_ty = self.builder.ctxt.tys.tuple(&elem_tys);

        let tuple_place = self.builder.alloc_place(tuple_ty);

        for (i, &expr) in exprs.iter().enumerate() {
            let field_ty = elem_tys[i];
            let field_place = self.builder.insert_field_access_place(tuple_place, i, field_ty);
            let val = self.lower_to_val(expr);
            self.builder.insert_assign_stmt(field_place, val);
        }

        self.builder.copy_val(tuple_place).into()
    }

    fn lower_assign(&mut self, target: hlr::Expr<'hlr>, value: hlr::Expr<'hlr>) -> LoweredExpr<'mlr> {
        let target_place = self.lower_to_place(target);
        let value_val = self.lower_to_val(value);
        self.builder.insert_assign_stmt(target_place, value_val);
        self.builder.insert_unit_val().into()
    }

    fn lower_deref(&mut self, inner: hlr::Expr<'hlr>) -> LoweredExpr<'mlr> {
        let op = self.lower_to_op(inner);
        self.builder.insert_deref_place(op).into()
    }

    fn lower_addr_of(&mut self, inner: hlr::Expr<'hlr>) -> LoweredExpr<'mlr> {
        let place = self.lower_to_place(inner);
        self.builder.insert_addr_of_val(place).into()
    }

    fn lower_as(&mut self, expr_id: hlr::ExprId, inner: hlr::Expr<'hlr>) -> LoweredExpr<'mlr> {
        let op = self.lower_to_op(inner);
        let target_ty = self.typing.expr_types[&expr_id];
        self.builder.insert_as_val(op, target_ty).into()
    }

    fn lower_if(
        &mut self,
        expr_id: hlr::ExprId,
        cond: hlr::Expr<'hlr>,
        then: hlr::Expr<'hlr>,
        else_: Option<hlr::Expr<'hlr>>,
    ) -> LoweredExpr<'mlr> {
        let result_ty = self.typing.expr_types[&expr_id];

        let cond_op = self.lower_to_op(cond);

        let result_place = self.builder.alloc_place(result_ty);

        self.builder.start_block();
        let then_val = self.lower_to_val(then);
        self.builder.insert_assign_stmt(result_place, then_val);
        let then_block = self.builder.end_block();

        self.builder.start_block();
        let else_val = match else_ {
            Some(e) => self.lower_to_val(e),
            None => self.builder.insert_unit_val(),
        };
        self.builder.insert_assign_stmt(result_place, else_val);
        let else_block = self.builder.end_block();

        self.builder.insert_if_stmt(cond_op, then_block, else_block);

        self.builder.copy_val(result_place).into()
    }

    fn lower_loop(&mut self, body: hlr::Expr<'hlr>) -> LoweredExpr<'mlr> {
        self.builder.start_block();
        self.lower_to_val(body);
        let body_block = self.builder.end_block();
        self.builder.insert_loop_stmt(body_block);
        self.builder.insert_unit_val().into()
    }

    fn lower_match(
        &mut self,
        expr_id: hlr::ExprId,
        scrutinee: hlr::Expr<'hlr>,
        arms: &'hlr [hlr::MatchArm<'hlr>],
    ) -> LoweredExpr<'mlr> {
        let result_ty = self.typing.expr_types[&expr_id];
        let scrutinee_ty = self.typing.expr_types[&scrutinee.1];

        let scrutinee_place = self.lower_to_place(scrutinee);

        let (enum_ty, by_ref, scrutinee_place) = match self.builder.ctxt.tys.get_ty_def(scrutinee_ty) {
            ty::TyDef::Enum { .. } => (scrutinee_ty, false, scrutinee_place),
            &ty::TyDef::Ref(inner) => {
                let copy_op = self.builder.insert_copy_op(scrutinee_place);
                let deref_place = self.builder.insert_deref_place(copy_op);
                (inner, true, deref_place)
            }
            _ => panic!("match scrutinee must be an enum or ref-to-enum"),
        };

        let disc_place = self.builder.insert_enum_discriminant_place(scrutinee_place);
        let discriminant_op = self.builder.insert_copy_op(disc_place);

        let i32_ty = self.builder.ctxt.tys.primitive(ty::Primitive::Integer32);
        let result_place = self.builder.alloc_place(result_ty);

        self.lower_match_arms(
            enum_ty,
            by_ref,
            arms,
            discriminant_op,
            i32_ty,
            scrutinee_place,
            result_place,
        );

        self.builder.copy_val(result_place).into()
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_match_arms(
        &mut self,
        enum_ty: ty::Ty,
        by_ref: bool,
        arms: &'hlr [hlr::MatchArm<'hlr>],
        discriminant_op: mlr::Op<'mlr>,
        i32_ty: ty::Ty,
        scrutinee_place: mlr::Place<'mlr>,
        result_place: mlr::Place<'mlr>,
    ) {
        match arms {
            [] => panic!("match must have at least one arm"),
            [arm] => {
                let arm_val = self.lower_match_arm(enum_ty, by_ref, arm, scrutinee_place);
                self.builder.insert_assign_stmt(result_place, arm_val);
            }
            [first_arm, rest_arms @ ..] => {
                let hlr::Val::Variant(_, variant_idx, _) = &first_arm.pattern.variant else {
                    panic!("match arm pattern must be Val::Variant")
                };
                let variant_idx = *variant_idx;

                let variant_idx_op = self
                    .builder
                    .insert_const_op(mlr::Const::Int(variant_idx as i64), i32_ty);

                let bool_ty = self.builder.ctxt.tys.primitive(ty::Primitive::Boolean);
                let cond_val = self.builder.insert_binary_prim_val(
                    language_items::BinaryPrimOp::EqI32,
                    discriminant_op,
                    variant_idx_op,
                    bool_ty,
                );
                let cond_place = self.builder.alloc_place(bool_ty);
                self.builder.insert_assign_stmt(cond_place, cond_val);
                let cond_op = self.builder.insert_copy_op(cond_place);

                self.builder.start_block();
                let first_arm_val = self.lower_match_arm(enum_ty, by_ref, first_arm, scrutinee_place);
                self.builder.insert_assign_stmt(result_place, first_arm_val);
                let then_block = self.builder.end_block();

                self.builder.start_block();
                self.lower_match_arms(
                    enum_ty,
                    by_ref,
                    rest_arms,
                    discriminant_op,
                    i32_ty,
                    scrutinee_place,
                    result_place,
                );
                let else_block = self.builder.end_block();

                self.builder.insert_if_stmt(cond_op, then_block, else_block);
            }
        }
    }

    fn lower_match_arm(
        &mut self,
        enum_ty: ty::Ty,
        by_ref: bool,
        arm: &'hlr hlr::MatchArm<'hlr>,
        scrutinee_place: mlr::Place<'mlr>,
    ) -> mlr::Val<'mlr> {
        let hlr::Val::Variant(_, variant_idx, _) = &arm.pattern.variant else {
            panic!("match arm pattern must be Val::Variant")
        };
        let variant_idx = *variant_idx;

        let variant_ty = self.builder.ctxt.tys.get_enum_variant_ty(enum_ty, variant_idx).unwrap();
        let variant_place = self
            .builder
            .insert_project_to_variant_place(scrutinee_place, variant_idx, variant_ty);

        for field in arm.pattern.fields {
            let field_ty = self
                .builder
                .ctxt
                .tys
                .get_struct_field_ty(variant_ty, field.field_index)
                .unwrap();
            let field_place = self
                .builder
                .insert_field_access_place(variant_place, field.field_index, field_ty);

            let binding_ty = self.typing.var_types[&field.binding];
            let binding_loc = self.builder.alloc_loc(binding_ty);

            if by_ref {
                let addr_val = self.builder.insert_addr_of_val(field_place);
                self.builder.insert_assign_to_loc_stmt(binding_loc, addr_val);
            } else {
                let field_val = self.builder.copy_val(field_place);
                self.builder.insert_assign_to_loc_stmt(binding_loc, field_val);
            }

            self.var_locs.insert(field.binding, binding_loc);
        }

        self.lower_to_val(arm.body)
    }

    fn lower_closure(
        &mut self,
        expr_id: hlr::ExprId,
        params: hlr::ClosureParams<'hlr>,
        body: hlr::Expr<'hlr>,
    ) -> LoweredExpr<'mlr> {
        let (fn_inst, captured_vars) = match &self.typing.expr_extra[&expr_id] {
            ExprExtra::Closure { fn_inst, captured_vars } => (*fn_inst, captured_vars.clone()),
            _ => panic!("expected Closure extra"),
        };

        let closure_ty = self.typing.expr_types[&expr_id];
        let captures_ty = match self.builder.ctxt.tys.get_ty_def(closure_ty).clone() {
            ty::TyDef::Closure { captures_ty, .. } => captures_ty,
            _ => panic!("closure expr should have Closure type"),
        };

        self.builder.register_fn_call(fn_inst);

        let closure_place = self.builder.alloc_place(closure_ty);

        let captures_place = self.builder.insert_closure_captures_place(closure_place, captures_ty);
        self.builder.start_block();
        for (i, &var_id) in captured_vars.iter().enumerate() {
            let field_ty = self.builder.ctxt.tys.get_struct_field_ty(captures_ty, i).unwrap();
            let field_place = self.builder.insert_field_access_place(captures_place, i, field_ty);
            let var_loc = self.var_locs[&var_id];
            let var_place = self.builder.insert_loc_place(var_loc);
            let use_val = self.builder.copy_val(var_place);
            self.builder.insert_assign_stmt(field_place, use_val);
        }
        self.builder.end_and_push_block();

        let param_var_ids: Vec<_> = params.iter().map(|hlr::ClosureParam(v, _)| *v).collect();
        let (closure_mlr_fn, nested_mlr_fns) = {
            let mut closure_lowerer = HlrLowerer::new(self.builder.ctxt, self.builder.mlr, fn_inst.fn_, self.typing);
            let mlr_fn = closure_lowerer.lower_body(&param_var_ids, Some(&captured_vars), body);
            let nested_mlr_fns = closure_lowerer.mlr_fns;
            (mlr_fn, nested_mlr_fns)
        };
        self.mlr_fns.extend(nested_mlr_fns);
        self.mlr_fns.push(closure_mlr_fn);

        self.builder.copy_val(closure_place).into()
    }

    fn lower_block(&mut self, stmts: hlr::StmtSlice<'hlr>, trailing: hlr::Expr<'hlr>) -> LoweredExpr<'mlr> {
        for stmt in stmts {
            self.lower_stmt(stmt);
        }
        self.lower_to_val(trailing).into()
    }

    fn lower_stmt(&mut self, stmt: hlr::Stmt<'hlr>) {
        match stmt {
            hlr::StmtDef::Expr(expr) => {
                self.builder.start_block();
                let val = self.lower_to_val(*expr);
                self.builder.store_val(val);
                self.builder.end_and_push_block();
            }
            hlr::StmtDef::Let { var, init, .. } => {
                self.lower_let_stmt(*var, *init);
            }
            hlr::StmtDef::Break => {
                self.builder.insert_break_stmt();
            }
            hlr::StmtDef::Return(expr) => {
                self.lower_return_stmt(*expr);
            }
        }
    }

    fn lower_let_stmt(&mut self, var: hlr::VarId, init: hlr::Expr<'hlr>) {
        let var_ty = self.typing.var_types[&var];

        let loc = self.builder.alloc_loc(var_ty);

        self.builder.start_block();
        let val = self.lower_to_val(init);
        self.builder.insert_assign_to_loc_stmt(loc, val);
        self.builder.end_and_push_block();

        self.var_locs.insert(var, loc);
    }

    fn lower_return_stmt(&mut self, expr: Option<hlr::Expr<'hlr>>) {
        self.builder.start_block();
        let return_val = match expr {
            Some(e) => self.lower_to_val(e),
            None => self.builder.insert_unit_val(),
        };
        self.builder.insert_return_stmt(return_val);
        self.builder.end_and_push_block();
    }

    fn mthd_resolution_to_op(&mut self, resolution: &MthdResolution) -> (mlr::Op<'mlr>, bool) {
        match resolution {
            MthdResolution::Inherent(fn_inst) => {
                let by_ref = self
                    .builder
                    .ctxt
                    .fns
                    .get_sig(fn_inst.fn_)
                    .unwrap()
                    .params
                    .first()
                    .map(|p| p.kind == fns::FnParamKind::SelfByRef)
                    .unwrap_or(false);
                let op = self.builder.insert_fn_inst_op(*fn_inst);
                (op, by_ref)
            }
            MthdResolution::Trait(inst) => {
                let by_ref = self
                    .builder
                    .ctxt
                    .traits
                    .get_trait_mthd_sig(inst.trait_inst.trait_, inst.mthd_idx)
                    .params
                    .first()
                    .map(|p| p.kind == fns::FnParamKind::SelfByRef)
                    .unwrap_or(false);
                let op = self.builder.insert_trait_mthd_op(*inst);
                (op, by_ref)
            }
        }
    }

    fn lower_mthd_resolution_to_op(&mut self, resolution: &MthdResolution) -> mlr::Op<'mlr> {
        match resolution {
            MthdResolution::Inherent(fn_inst) => self.builder.insert_fn_inst_op(*fn_inst),
            MthdResolution::Trait(inst) => self.builder.insert_trait_mthd_op(*inst),
        }
    }
}
