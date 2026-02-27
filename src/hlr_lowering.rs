use std::collections::HashMap;

use crate::{
    ctxt::{self, fns, mlr, ty},
    hlr,
    typeck::{ExprExtra, HlrTyping, MthdResolution},
};

pub fn hlr_to_mlr<'hlr>(ctxt: &mut ctxt::Ctxt, fn_: &'hlr hlr::Fn<'hlr>, typing: &HlrTyping) {
    let fn_mlr = {
        let mut lowerer = HlrLowerer::new(ctxt, fn_.fn_, typing);
        lowerer.lower_fn(fn_)
    };
    ctxt.fns.add_fn_def(fn_.fn_, fn_mlr);
}

struct HlrLowerer<'a, 'hlr> {
    ctxt: &'a mut ctxt::Ctxt,
    fn_: fns::Fn,
    typing: &'a HlrTyping,
    var_locs: HashMap<hlr::VarId, mlr::Loc>,
    blocks: Vec<Vec<mlr::Stmt>>,
    _hlr: std::marker::PhantomData<&'hlr hlr::Hlr<'hlr>>,
}

enum Lowered {
    Val(mlr::Val),
    Place(mlr::Place),
    Op(mlr::Op),
}

impl<'a, 'hlr> HlrLowerer<'a, 'hlr> {
    fn new(ctxt: &'a mut ctxt::Ctxt, fn_: fns::Fn, typing: &'a HlrTyping) -> Self {
        Self {
            ctxt,
            fn_,
            typing,
            var_locs: HashMap::new(),
            blocks: Vec::new(),
            _hlr: std::marker::PhantomData,
        }
    }

    fn lower_fn(&mut self, fn_: &'hlr hlr::Fn<'hlr>) -> fns::FnMlr {
        self.lower_body(&fn_.param_var_ids, None, fn_.body)
    }

    fn lower_body(
        &mut self,
        param_var_ids: &[hlr::VarId],
        captured_vars: Option<&[hlr::VarId]>,
        body: hlr::Expr<'hlr>,
    ) -> fns::FnMlr {
        let sig = self.ctxt.fns.get_sig(self.fn_).unwrap().clone();
        let mut param_locs = Vec::new();

        // For closures, the first sig param is the captures struct (no VarId binding)
        let regular_params = if captured_vars.is_some() {
            let captures_loc = self.ctxt.mlr.insert_typed_loc(sig.params[0].ty);
            param_locs.push(captures_loc);
            &sig.params[1..]
        } else {
            &sig.params[..]
        };
        for (param, &var_id) in regular_params.iter().zip(param_var_ids) {
            let loc = self.ctxt.mlr.insert_typed_loc(param.ty);
            param_locs.push(loc);
            self.var_locs.insert(var_id, loc);
        }

        self.start_block();

        // Extract captured vars from the captures struct into individual locals
        if let Some(captured_vars) = captured_vars {
            let captures_ty = sig.params[0].ty;
            let captures_place = self.insert_loc_place(param_locs[0]);
            for (i, &var_id) in captured_vars.iter().enumerate() {
                let field_ty = self.ctxt.tys.get_struct_field_ty(captures_ty, i).unwrap();
                let field_place = self.insert_field_access_place(captures_place, i, field_ty);
                let copy_op = self.insert_copy_op(field_place);
                let use_val = self.insert_use_val(copy_op);
                let loc = self.ctxt.mlr.insert_typed_loc(field_ty);
                self.insert_alloc_stmt(loc);
                self.insert_assign_to_loc_stmt(loc, use_val);
                self.var_locs.insert(var_id, loc);
            }
        }

        let body_val = self.lower_to_val(body);
        self.insert_return_stmt(body_val);
        let body = self.end_block();

        fns::FnMlr { body, param_locs }
    }

    fn lower_expr(&mut self, expr: hlr::Expr<'hlr>) -> Lowered {
        let (expr_def, expr_id) = (expr.0, expr.1);

        match expr_def {
            hlr::ExprDef::Lit(lit) => Lowered::Op(self.lower_lit(lit)),
            hlr::ExprDef::Val(val) => self.lower_val(val, expr_id),
            hlr::ExprDef::BinaryOp { left, right, operator } => {
                Lowered::Val(self.lower_binary_op(expr_id, *left, *right, *operator))
            }
            hlr::ExprDef::UnaryOp { operand, operator } => {
                Lowered::Val(self.lower_unary_op(expr_id, *operand, *operator))
            }
            hlr::ExprDef::Call { callee, args } => Lowered::Val(self.lower_call(*callee, args)),
            hlr::ExprDef::MthdCall { receiver, args, .. } => {
                Lowered::Val(self.lower_mthd_call(expr_id, *receiver, args))
            }
            hlr::ExprDef::Struct { constructor, fields } => {
                Lowered::Val(self.lower_struct_expr(expr_id, constructor, fields))
            }
            hlr::ExprDef::FieldAccess { base, .. } => Lowered::Place(self.lower_field_access(expr_id, *base)),
            hlr::ExprDef::Tuple(exprs) => Lowered::Val(self.lower_tuple(exprs)),
            hlr::ExprDef::Assign { target, value } => Lowered::Val(self.lower_assign(*target, *value)),
            hlr::ExprDef::Deref(inner) => Lowered::Place(self.lower_deref(*inner)),
            hlr::ExprDef::AddrOf(inner) => Lowered::Val(self.lower_addr_of(*inner)),
            hlr::ExprDef::As { expr: inner, .. } => Lowered::Val(self.lower_as(expr_id, *inner)),
            hlr::ExprDef::Closure { params, body, .. } => Lowered::Val(self.lower_closure(expr_id, params, *body)),
            hlr::ExprDef::If { cond, then, else_ } => Lowered::Val(self.lower_if(expr_id, *cond, *then, *else_)),
            hlr::ExprDef::Loop { body } => Lowered::Val(self.lower_loop(*body)),
            hlr::ExprDef::Match { scrutinee, arms } => Lowered::Val(self.lower_match(expr_id, *scrutinee, arms)),
            hlr::ExprDef::Block { stmts, trailing } => Lowered::Val(self.lower_block(stmts, *trailing)),
            hlr::ExprDef::QualifiedMthd { .. } => Lowered::Op(self.lower_qualified_mthd(expr_id)),
        }
    }

    fn lower_to_val(&mut self, expr: hlr::Expr<'hlr>) -> mlr::Val {
        let lowered = self.lower_expr(expr);
        self.lowered_into_val(lowered)
    }

    fn lower_to_place(&mut self, expr: hlr::Expr<'hlr>) -> mlr::Place {
        let lowered = self.lower_expr(expr);
        self.lowered_into_place(lowered)
    }

    fn lower_to_op(&mut self, expr: hlr::Expr<'hlr>) -> mlr::Op {
        let lowered = self.lower_expr(expr);
        self.lowered_into_op(lowered)
    }

    fn lowered_into_val(&mut self, lowered: Lowered) -> mlr::Val {
        match lowered {
            Lowered::Val(v) => v,
            Lowered::Place(p) => {
                let op = self.insert_copy_op(p);
                self.insert_use_val(op)
            }
            Lowered::Op(op) => self.insert_use_val(op),
        }
    }

    fn lowered_into_place(&mut self, lowered: Lowered) -> mlr::Place {
        match lowered {
            Lowered::Place(p) => p,
            Lowered::Val(v) => {
                let ty = self.ctxt.mlr.get_val_ty(v);
                let loc = self.ctxt.mlr.insert_typed_loc(ty);
                self.insert_alloc_stmt(loc);
                let place = self.insert_loc_place(loc);
                self.insert_assign_stmt(place, v);
                place
            }
            Lowered::Op(op) => {
                let v = self.insert_use_val(op);
                let ty = self.ctxt.mlr.get_val_ty(v);
                let loc = self.ctxt.mlr.insert_typed_loc(ty);
                self.insert_alloc_stmt(loc);
                let place = self.insert_loc_place(loc);
                self.insert_assign_stmt(place, v);
                place
            }
        }
    }

    fn lowered_into_op(&mut self, lowered: Lowered) -> mlr::Op {
        match lowered {
            Lowered::Op(op) => op,
            Lowered::Place(p) => self.insert_copy_op(p),
            Lowered::Val(v) => {
                let ty = self.ctxt.mlr.get_val_ty(v);
                let loc = self.ctxt.mlr.insert_typed_loc(ty);
                self.insert_alloc_stmt(loc);
                let place = self.insert_loc_place(loc);
                self.insert_assign_stmt(place, v);
                self.insert_copy_op(place)
            }
        }
    }

    fn lower_lit(&mut self, lit: &hlr::Lit) -> mlr::Op {
        let (const_, ty) = match lit {
            hlr::Lit::Int(n) => {
                let ty = self.ctxt.tys.primitive(ty::Primitive::Integer32);
                (mlr::Const::Int(*n), ty)
            }
            hlr::Lit::Bool(b) => {
                let ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);
                (mlr::Const::Bool(*b), ty)
            }
            hlr::Lit::CChar(c) => {
                let ty = self.ctxt.tys.primitive(ty::Primitive::CChar);
                (mlr::Const::CChar(*c), ty)
            }
            hlr::Lit::CString(s) => {
                let c_char_ty = self.ctxt.tys.primitive(ty::Primitive::CChar);
                let ty = self.ctxt.tys.ptr(c_char_ty);
                (mlr::Const::CString(s.clone()), ty)
            }
        };
        let op = self.ctxt.mlr.insert_op(mlr::OpDef::Const(const_));
        self.ctxt.mlr.set_op_ty(op, ty);
        op
    }

    fn lower_val(&mut self, val: &hlr::Val<'hlr>, expr_id: hlr::ExprId) -> Lowered {
        match val {
            hlr::Val::Var(var_id) => {
                let loc = self.var_locs[var_id];
                Lowered::Place(self.insert_loc_place(loc))
            }
            hlr::Val::Fn(..) => {
                let fn_inst = match self.typing.expr_extra[&expr_id] {
                    ExprExtra::ValFn(fi) => fi,
                    _ => panic!("expected ValFn extra"),
                };
                let ty = self.typing.expr_types[&expr_id];
                self.ctxt.fns.register_fn_call(self.fn_, fn_inst);
                let op = self.ctxt.mlr.insert_op(mlr::OpDef::Fn(fn_inst));
                self.ctxt.mlr.set_op_ty(op, ty);
                Lowered::Op(op)
            }
            hlr::Val::Mthd(..) => {
                let resolution = match &self.typing.expr_extra[&expr_id] {
                    ExprExtra::ValMthd(r) => r.clone(),
                    _ => panic!("expected ValMthd extra"),
                };
                let ty = self.typing.expr_types[&expr_id];
                let op = self.lower_mthd_resolution_to_op(&resolution, ty);
                Lowered::Op(op)
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
    ) -> mlr::Val {
        match operator {
            hlr::BinaryOperator::LogicalAnd => self.lower_logical_and(left, right),
            hlr::BinaryOperator::LogicalOr => self.lower_logical_or(left, right),
            _ => {
                let fn_inst = match self.typing.expr_extra[&expr_id] {
                    ExprExtra::BinaryOp(fi) => fi,
                    _ => panic!("expected BinaryOp extra"),
                };
                let fn_op = self.insert_fn_inst_op(fn_inst);
                let left_op = self.lower_to_op(left);
                let right_op = self.lower_to_op(right);
                self.insert_call_val(fn_op, vec![left_op, right_op])
            }
        }
    }

    fn lower_logical_and(&mut self, left: hlr::Expr<'hlr>, right: hlr::Expr<'hlr>) -> mlr::Val {
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);
        let result_loc = self.ctxt.mlr.insert_typed_loc(bool_ty);
        self.insert_alloc_stmt(result_loc);
        let result_place = self.insert_loc_place(result_loc);

        let left_op = self.lower_to_op(left);

        self.start_block();
        let right_op = self.lower_to_op(right);
        let right_val = self.insert_use_val(right_op);
        self.insert_assign_stmt(result_place, right_val);
        let then_block = self.end_block();

        self.start_block();
        let false_op = self.insert_bool_const(false);
        let false_val = self.insert_use_val(false_op);
        self.insert_assign_stmt(result_place, false_val);
        let else_block = self.end_block();

        self.insert_if_stmt(left_op, then_block, else_block);
        let result_op = self.insert_copy_op(result_place);
        self.insert_use_val(result_op)
    }

    fn lower_logical_or(&mut self, left: hlr::Expr<'hlr>, right: hlr::Expr<'hlr>) -> mlr::Val {
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);
        let result_loc = self.ctxt.mlr.insert_typed_loc(bool_ty);
        self.insert_alloc_stmt(result_loc);
        let result_place = self.insert_loc_place(result_loc);

        let left_op = self.lower_to_op(left);

        self.start_block();
        let true_op = self.insert_bool_const(true);
        let true_val = self.insert_use_val(true_op);
        self.insert_assign_stmt(result_place, true_val);
        let then_block = self.end_block();

        self.start_block();
        let right_op = self.lower_to_op(right);
        let right_val = self.insert_use_val(right_op);
        self.insert_assign_stmt(result_place, right_val);
        let else_block = self.end_block();

        self.insert_if_stmt(left_op, then_block, else_block);
        let result_op = self.insert_copy_op(result_place);
        self.insert_use_val(result_op)
    }

    fn lower_unary_op(
        &mut self,
        expr_id: hlr::ExprId,
        operand: hlr::Expr<'hlr>,
        _operator: hlr::UnaryOperator,
    ) -> mlr::Val {
        let fn_inst = match self.typing.expr_extra[&expr_id] {
            ExprExtra::UnaryOp(fi) => fi,
            _ => panic!("expected UnaryOp extra"),
        };
        let fn_op = self.insert_fn_inst_op(fn_inst);
        let operand_op = self.lower_to_op(operand);
        self.insert_call_val(fn_op, vec![operand_op])
    }

    fn lower_call(&mut self, callee: hlr::Expr<'hlr>, args: hlr::ExprSlice<'hlr>) -> mlr::Val {
        let callee_op = self.lower_to_op(callee);
        let mut call_args = Vec::new();
        for &arg in args {
            call_args.push(self.lower_to_op(arg));
        }
        self.insert_call_val(callee_op, call_args)
    }

    fn lower_mthd_call(
        &mut self,
        expr_id: hlr::ExprId,
        receiver: hlr::Expr<'hlr>,
        args: hlr::ExprSlice<'hlr>,
    ) -> mlr::Val {
        let resolution = match &self.typing.expr_extra[&expr_id] {
            ExprExtra::ValMthd(r) => r.clone(),
            _ => panic!("expected ValMthd extra for MthdCall"),
        };
        let (callee_op, by_ref) = self.mthd_resolution_to_op(&resolution);

        let receiver_place = self.lower_to_place(receiver);
        let receiver_op = if by_ref {
            let addr_val = self.insert_addr_of_val(receiver_place);
            let addr_ty = self.ctxt.mlr.get_val_ty(addr_val);
            let addr_loc = self.ctxt.mlr.insert_typed_loc(addr_ty);
            self.insert_alloc_stmt(addr_loc);
            self.insert_assign_to_loc_stmt(addr_loc, addr_val);
            let addr_place = self.insert_loc_place(addr_loc);
            self.insert_copy_op(addr_place)
        } else {
            self.insert_copy_op(receiver_place)
        };

        let mut call_args = vec![receiver_op];
        for &arg in args {
            call_args.push(self.lower_to_op(arg));
        }
        self.insert_call_val(callee_op, call_args)
    }

    fn lower_qualified_mthd(&mut self, expr_id: hlr::ExprId) -> mlr::Op {
        let resolution = match &self.typing.expr_extra[&expr_id] {
            ExprExtra::ValMthd(r) => r.clone(),
            _ => panic!("expected ValMthd extra for QualifiedMthd"),
        };
        let ty = self.typing.expr_types[&expr_id];
        self.lower_mthd_resolution_to_op(&resolution, ty)
    }

    fn lower_struct_expr(
        &mut self,
        expr_id: hlr::ExprId,
        constructor: &hlr::Val<'hlr>,
        fields: hlr::StructFields<'hlr>,
    ) -> mlr::Val {
        let expr_ty = self.typing.expr_types[&expr_id];
        match constructor {
            hlr::Val::Struct(..) => self.lower_struct_val(expr_ty, fields),
            hlr::Val::Variant(_, variant_idx, _) => self.lower_enum_val(expr_ty, *variant_idx, fields),
            _ => unreachable!(),
        }
    }

    fn lower_struct_val(&mut self, struct_ty: ty::Ty, fields: hlr::StructFields<'hlr>) -> mlr::Val {
        let loc = self.ctxt.mlr.insert_typed_loc(struct_ty);
        self.insert_alloc_stmt(loc);
        let struct_place = self.insert_loc_place(loc);

        for (field_spec, field_expr) in fields.iter() {
            let field_index = match field_spec {
                hlr::FieldSpec::Name(name) => self
                    .ctxt
                    .tys
                    .get_struct_field_index_by_name(struct_ty, name)
                    .unwrap_or_else(|_| panic!("struct field {name} should exist")),
                hlr::FieldSpec::Index(idx) => *idx,
            };
            let field_ty = self.ctxt.tys.get_struct_field_ty(struct_ty, field_index).unwrap();
            let field_place = self.insert_field_access_place(struct_place, field_index, field_ty);
            let val = self.lower_to_val(*field_expr);
            self.insert_assign_stmt(field_place, val);
        }

        let op = self.insert_copy_op(struct_place);
        self.insert_use_val(op)
    }

    fn lower_enum_val(&mut self, enum_ty: ty::Ty, variant_idx: usize, fields: hlr::StructFields<'hlr>) -> mlr::Val {
        let loc = self.ctxt.mlr.insert_typed_loc(enum_ty);
        self.insert_alloc_stmt(loc);
        let enum_place = self.insert_loc_place(loc);

        let discriminant_place = self.insert_enum_discriminant_place(enum_place);
        let i32_ty = self.ctxt.tys.primitive(ty::Primitive::Integer32);
        let disc_op = self
            .ctxt
            .mlr
            .insert_op(mlr::OpDef::Const(mlr::Const::Int(variant_idx as i64)));
        self.ctxt.mlr.set_op_ty(disc_op, i32_ty);
        let disc_val = self.insert_use_val(disc_op);
        self.insert_assign_stmt(discriminant_place, disc_val);

        let variant_ty = self.ctxt.tys.get_enum_variant_ty(enum_ty, variant_idx).unwrap();
        let variant_place = self.insert_project_to_variant_place(enum_place, variant_idx, variant_ty);

        for (field_spec, field_expr) in fields.iter() {
            let field_index = match field_spec {
                hlr::FieldSpec::Name(name) => self
                    .ctxt
                    .tys
                    .get_struct_field_index_by_name(variant_ty, name)
                    .unwrap_or_else(|_| panic!("variant field {name} should exist")),
                hlr::FieldSpec::Index(idx) => *idx,
            };
            let field_ty = self.ctxt.tys.get_struct_field_ty(variant_ty, field_index).unwrap();
            let field_place = self.insert_field_access_place(variant_place, field_index, field_ty);
            let val = self.lower_to_val(*field_expr);
            self.insert_assign_stmt(field_place, val);
        }

        let op = self.insert_copy_op(enum_place);
        self.insert_use_val(op)
    }

    fn lower_field_access(&mut self, expr_id: hlr::ExprId, base: hlr::Expr<'hlr>) -> mlr::Place {
        let (derefs, field_index) = match self.typing.expr_extra[&expr_id] {
            ExprExtra::FieldAccess { derefs, index } => (derefs, index),
            _ => panic!("expected FieldAccess extra"),
        };

        let mut place = self.lower_to_place(base);

        for _ in 0..derefs {
            let op = self.insert_copy_op(place);
            place = self.insert_deref_place(op);
        }

        let field_ty = self.typing.expr_types[&expr_id];
        self.insert_field_access_place(place, field_index, field_ty)
    }

    fn lower_tuple(&mut self, exprs: hlr::ExprSlice<'hlr>) -> mlr::Val {
        let exprs = exprs.to_vec();
        let elem_tys: Vec<ty::Ty> = exprs.iter().map(|e| self.typing.expr_types[&e.1]).collect();
        let tuple_ty = self.ctxt.tys.tuple(&elem_tys);

        let loc = self.ctxt.mlr.insert_typed_loc(tuple_ty);
        self.insert_alloc_stmt(loc);
        let tuple_place = self.insert_loc_place(loc);

        for (i, &expr) in exprs.iter().enumerate() {
            let field_ty = elem_tys[i];
            let field_place = self.insert_field_access_place(tuple_place, i, field_ty);
            let val = self.lower_to_val(expr);
            self.insert_assign_stmt(field_place, val);
        }

        let op = self.insert_copy_op(tuple_place);
        self.insert_use_val(op)
    }

    fn lower_assign(&mut self, target: hlr::Expr<'hlr>, value: hlr::Expr<'hlr>) -> mlr::Val {
        let target_place = self.lower_to_place(target);
        let value_val = self.lower_to_val(value);
        self.insert_assign_stmt(target_place, value_val);
        self.insert_unit_val()
    }

    fn lower_deref(&mut self, inner: hlr::Expr<'hlr>) -> mlr::Place {
        let op = self.lower_to_op(inner);
        self.insert_deref_place(op)
    }

    fn lower_addr_of(&mut self, inner: hlr::Expr<'hlr>) -> mlr::Val {
        let place = self.lower_to_place(inner);
        self.insert_addr_of_val(place)
    }

    fn lower_as(&mut self, expr_id: hlr::ExprId, inner: hlr::Expr<'hlr>) -> mlr::Val {
        let op = self.lower_to_op(inner);
        let target_ty = self.typing.expr_types[&expr_id];
        self.insert_as_val(op, target_ty)
    }

    fn lower_if(
        &mut self,
        expr_id: hlr::ExprId,
        cond: hlr::Expr<'hlr>,
        then: hlr::Expr<'hlr>,
        else_: Option<hlr::Expr<'hlr>>,
    ) -> mlr::Val {
        let result_ty = self.typing.expr_types[&expr_id];

        let cond_op = self.lower_to_op(cond);

        let result_loc = self.ctxt.mlr.insert_typed_loc(result_ty);
        self.insert_alloc_stmt(result_loc);
        let result_place = self.insert_loc_place(result_loc);

        self.start_block();
        let then_val = self.lower_to_val(then);
        self.insert_assign_stmt(result_place, then_val);
        let then_block = self.end_block();

        self.start_block();
        let else_val = match else_ {
            Some(e) => self.lower_to_val(e),
            None => self.insert_unit_val(),
        };
        self.insert_assign_stmt(result_place, else_val);
        let else_block = self.end_block();

        self.insert_if_stmt(cond_op, then_block, else_block);

        let result_op = self.insert_copy_op(result_place);
        self.insert_use_val(result_op)
    }

    fn lower_loop(&mut self, body: hlr::Expr<'hlr>) -> mlr::Val {
        self.start_block();
        self.lower_to_val(body);
        let body_block = self.end_block();
        self.insert_loop_stmt(body_block);
        self.insert_unit_val()
    }

    fn lower_match(
        &mut self,
        expr_id: hlr::ExprId,
        scrutinee: hlr::Expr<'hlr>,
        arms: &'hlr [hlr::MatchArm<'hlr>],
    ) -> mlr::Val {
        let result_ty = self.typing.expr_types[&expr_id];
        let scrutinee_ty = self.typing.expr_types[&scrutinee.1];

        let scrutinee_place = self.lower_to_place(scrutinee);

        let (enum_ty, by_ref, scrutinee_place) = match self.ctxt.tys.get_ty_def(scrutinee_ty).cloned() {
            Some(ty::TyDef::Enum { .. }) => (scrutinee_ty, false, scrutinee_place),
            Some(ty::TyDef::Ref(inner)) => {
                let copy_op = self.insert_copy_op(scrutinee_place);
                let deref_place = self.insert_deref_place(copy_op);
                (inner, true, deref_place)
            }
            _ => panic!("match scrutinee must be an enum or ref-to-enum"),
        };

        let disc_place = self.insert_enum_discriminant_place(scrutinee_place);
        let discriminant_op = self.insert_copy_op(disc_place);

        let i32_ty = self.ctxt.tys.primitive(ty::Primitive::Integer32);
        let empty = self.ctxt.tys.ty_slice(&[]);
        let eq_fn = self
            .ctxt
            .fns
            .get_fn_by_name("eq::<i32>")
            .expect("eq::<i32> should be registered");
        let eq_fn_inst = fns::FnInst {
            fn_: eq_fn,
            gen_args: empty,
            env_gen_args: empty,
        };
        let eq_op = self.insert_fn_inst_op(eq_fn_inst);

        let result_loc = self.ctxt.mlr.insert_typed_loc(result_ty);
        self.insert_alloc_stmt(result_loc);
        let result_place = self.insert_loc_place(result_loc);

        self.lower_match_arms(
            enum_ty,
            by_ref,
            arms,
            eq_op,
            discriminant_op,
            i32_ty,
            scrutinee_place,
            result_place,
        );

        let result_op = self.insert_copy_op(result_place);
        self.insert_use_val(result_op)
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_match_arms(
        &mut self,
        enum_ty: ty::Ty,
        by_ref: bool,
        arms: &'hlr [hlr::MatchArm<'hlr>],
        eq_op: mlr::Op,
        discriminant_op: mlr::Op,
        i32_ty: ty::Ty,
        scrutinee_place: mlr::Place,
        result_place: mlr::Place,
    ) {
        match arms {
            [] => panic!("match must have at least one arm"),
            [arm] => {
                let arm_val = self.lower_match_arm(enum_ty, by_ref, arm, scrutinee_place);
                self.insert_assign_stmt(result_place, arm_val);
            }
            [first_arm, rest_arms @ ..] => {
                let hlr::Val::Variant(_, variant_idx, _) = &first_arm.pattern.variant else {
                    panic!("match arm pattern must be Val::Variant")
                };
                let variant_idx = *variant_idx;

                let variant_idx_op = {
                    let op = self
                        .ctxt
                        .mlr
                        .insert_op(mlr::OpDef::Const(mlr::Const::Int(variant_idx as i64)));
                    self.ctxt.mlr.set_op_ty(op, i32_ty);
                    op
                };

                let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);
                let cond_val = self.insert_call_val(eq_op, vec![discriminant_op, variant_idx_op]);
                let cond_loc = self.ctxt.mlr.insert_typed_loc(bool_ty);
                self.insert_alloc_stmt(cond_loc);
                self.insert_assign_to_loc_stmt(cond_loc, cond_val);
                let cond_place = self.insert_loc_place(cond_loc);
                let cond_op = self.insert_copy_op(cond_place);

                self.start_block();
                let first_arm_val = self.lower_match_arm(enum_ty, by_ref, first_arm, scrutinee_place);
                self.insert_assign_stmt(result_place, first_arm_val);
                let then_block = self.end_block();

                self.start_block();
                self.lower_match_arms(
                    enum_ty,
                    by_ref,
                    rest_arms,
                    eq_op,
                    discriminant_op,
                    i32_ty,
                    scrutinee_place,
                    result_place,
                );
                let else_block = self.end_block();

                self.insert_if_stmt(cond_op, then_block, else_block);
            }
        }
    }

    fn lower_match_arm(
        &mut self,
        enum_ty: ty::Ty,
        by_ref: bool,
        arm: &'hlr hlr::MatchArm<'hlr>,
        scrutinee_place: mlr::Place,
    ) -> mlr::Val {
        let hlr::Val::Variant(_, variant_idx, _) = &arm.pattern.variant else {
            panic!("match arm pattern must be Val::Variant")
        };
        let variant_idx = *variant_idx;

        let variant_ty = self.ctxt.tys.get_enum_variant_ty(enum_ty, variant_idx).unwrap();
        let variant_place = self.insert_project_to_variant_place(scrutinee_place, variant_idx, variant_ty);

        for field in arm.pattern.fields {
            let field_ty = self
                .ctxt
                .tys
                .get_struct_field_ty(variant_ty, field.field_index)
                .unwrap();
            let field_place = self.insert_field_access_place(variant_place, field.field_index, field_ty);

            let binding_ty = self.typing.var_types[&field.binding];
            let binding_loc = self.ctxt.mlr.insert_typed_loc(binding_ty);
            self.insert_alloc_stmt(binding_loc);

            if by_ref {
                let addr_val = self.insert_addr_of_val(field_place);
                self.insert_assign_to_loc_stmt(binding_loc, addr_val);
            } else {
                let field_op = self.insert_copy_op(field_place);
                let field_val = self.insert_use_val(field_op);
                self.insert_assign_to_loc_stmt(binding_loc, field_val);
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
    ) -> mlr::Val {
        let (fn_inst, captured_vars) = match &self.typing.expr_extra[&expr_id] {
            ExprExtra::Closure { fn_inst, captured_vars } => (*fn_inst, captured_vars.clone()),
            _ => panic!("expected Closure extra"),
        };

        let closure_ty = self.typing.expr_types[&expr_id];
        let captures_ty = match self.ctxt.tys.get_ty_def(closure_ty).cloned() {
            Some(ty::TyDef::Closure { captures_ty, .. }) => captures_ty,
            _ => panic!("closure expr should have Closure type"),
        };

        self.ctxt.fns.register_fn_call(self.fn_, fn_inst);

        let closure_loc = self.ctxt.mlr.insert_typed_loc(closure_ty);
        self.insert_alloc_stmt(closure_loc);
        let closure_place = self.insert_loc_place(closure_loc);

        let captures_place = self.insert_closure_captures_place(closure_place, captures_ty);
        self.start_block();
        for (i, &var_id) in captured_vars.iter().enumerate() {
            let field_ty = self.ctxt.tys.get_struct_field_ty(captures_ty, i).unwrap();

            let field_place = self.insert_field_access_place(captures_place, i, field_ty);

            let var_loc = self.var_locs[&var_id];
            let var_place = self.insert_loc_place(var_loc);
            let copy_op = self.insert_copy_op(var_place);
            let use_val = self.insert_use_val(copy_op);

            self.insert_assign_stmt(field_place, use_val);
        }
        self.end_and_push_block();

        let param_var_ids: Vec<_> = params.iter().map(|hlr::ClosureParam(v, _)| *v).collect();
        let mut closure_lowerer = HlrLowerer::new(self.ctxt, fn_inst.fn_, self.typing);
        let fn_mlr = closure_lowerer.lower_body(&param_var_ids, Some(&captured_vars), body);
        self.ctxt.fns.add_fn_def(fn_inst.fn_, fn_mlr);

        let copy_op = self.insert_copy_op(closure_place);
        self.insert_use_val(copy_op)
    }

    fn lower_block(&mut self, stmts: hlr::StmtSlice<'hlr>, trailing: hlr::Expr<'hlr>) -> mlr::Val {
        for stmt in stmts {
            self.lower_stmt(stmt);
        }
        self.lower_to_val(trailing)
    }

    fn lower_stmt(&mut self, stmt: hlr::Stmt<'hlr>) {
        match stmt {
            hlr::StmtDef::Expr(expr) => {
                self.start_block();
                let val = self.lower_to_val(*expr);
                let ty = self.ctxt.mlr.get_val_ty(val);
                let loc = self.ctxt.mlr.insert_typed_loc(ty);
                self.insert_alloc_stmt(loc);
                self.insert_assign_to_loc_stmt(loc, val);
                self.end_and_push_block();
            }
            hlr::StmtDef::Let { var, init, .. } => {
                self.lower_let_stmt(*var, *init);
            }
            hlr::StmtDef::Break => {
                self.insert_break_stmt();
            }
            hlr::StmtDef::Return(expr) => {
                self.lower_return_stmt(*expr);
            }
        }
    }

    fn lower_let_stmt(&mut self, var: hlr::VarId, init: hlr::Expr<'hlr>) {
        let var_ty = self.typing.var_types[&var];

        let loc = self.ctxt.mlr.insert_typed_loc(var_ty);
        self.insert_alloc_stmt(loc);

        self.start_block();
        let val = self.lower_to_val(init);
        self.insert_assign_to_loc_stmt(loc, val);
        self.end_and_push_block();

        self.var_locs.insert(var, loc);
    }

    fn lower_return_stmt(&mut self, expr: Option<hlr::Expr<'hlr>>) {
        self.start_block();
        let return_val = match expr {
            Some(e) => self.lower_to_val(e),
            None => self.insert_unit_val(),
        };
        self.insert_return_stmt(return_val);
        self.end_and_push_block();
    }

    fn mthd_resolution_to_op(&mut self, resolution: &MthdResolution) -> (mlr::Op, bool) {
        match resolution {
            MthdResolution::Inherent(fn_inst) => {
                let by_ref = self
                    .ctxt
                    .fns
                    .get_sig(fn_inst.fn_)
                    .unwrap()
                    .params
                    .first()
                    .map(|p| p.kind == fns::FnParamKind::SelfByRef)
                    .unwrap_or(false);
                let op = self.insert_fn_inst_op(*fn_inst);
                (op, by_ref)
            }
            MthdResolution::Trait(inst) => {
                let by_ref = self
                    .ctxt
                    .traits
                    .get_trait_mthd_sig(inst.trait_inst.trait_, inst.mthd_idx)
                    .params
                    .first()
                    .map(|p| p.kind == fns::FnParamKind::SelfByRef)
                    .unwrap_or(false);
                let op = self.insert_trait_mthd_op(*inst);
                (op, by_ref)
            }
        }
    }

    fn lower_mthd_resolution_to_op(&mut self, resolution: &MthdResolution, op_ty: ty::Ty) -> mlr::Op {
        match resolution {
            MthdResolution::Inherent(fn_inst) => {
                self.ctxt.fns.register_fn_call(self.fn_, *fn_inst);
                let op = self.ctxt.mlr.insert_op(mlr::OpDef::Fn(*fn_inst));
                self.ctxt.mlr.set_op_ty(op, op_ty);
                op
            }
            MthdResolution::Trait(inst) => {
                self.ctxt.fns.register_trait_mthd_call(self.fn_, *inst);
                let op = self.ctxt.mlr.insert_op(mlr::OpDef::TraitMthd(*inst));
                self.ctxt.mlr.set_op_ty(op, op_ty);
                op
            }
        }
    }

    fn fn_ty_of_fn_inst(&mut self, fn_inst: fns::FnInst) -> ty::Ty {
        let sig = self.ctxt.fns.get_sig(fn_inst.fn_).unwrap();
        let param_tys: Vec<_> = sig.params.iter().map(|p| p.ty).collect();
        let fn_ty = self.ctxt.tys.fn_(&param_tys, sig.return_ty, sig.var_args);
        let subst = self.ctxt.get_subst_for_fn_inst(fn_inst);
        self.ctxt.tys.substitute_gen_vars(fn_ty, &subst)
    }

    fn trait_mthd_fn_ty(&mut self, inst: fns::TraitMthdInst) -> ty::Ty {
        let sig = self
            .ctxt
            .traits
            .get_trait_mthd_sig(inst.trait_inst.trait_, inst.mthd_idx);
        let param_tys: Vec<_> = sig.params.iter().map(|p| p.ty).collect();
        let sig_gen_params = sig.gen_params.clone();
        let fn_ty = self.ctxt.tys.fn_(&param_tys, sig.return_ty, sig.var_args);

        let trait_def = self.ctxt.traits.get_trait_def(inst.trait_inst.trait_);
        let trait_gen_params = trait_def.gen_params.clone();

        let trait_subst =
            ty::GenVarSubst::new(&trait_gen_params, self.ctxt.tys.get_ty_slice(inst.trait_inst.gen_args)).unwrap();
        let mthd_subst = ty::GenVarSubst::new(&sig_gen_params, self.ctxt.tys.get_ty_slice(inst.gen_args)).unwrap();
        let all_subst = ty::GenVarSubst::compose(trait_subst, mthd_subst);

        let fn_ty = self.ctxt.tys.substitute_self_ty(fn_ty, inst.impl_ty);
        self.ctxt.tys.substitute_gen_vars(fn_ty, &all_subst)
    }

    fn start_block(&mut self) {
        self.blocks.push(Vec::new());
    }

    fn end_block(&mut self) -> mlr::Stmt {
        let stmts = self.blocks.pop().expect("block stack should not be empty");
        self.ctxt.mlr.insert_stmt(mlr::StmtDef::Block(stmts))
    }

    fn end_and_push_block(&mut self) {
        let block_stmt = self.end_block();
        self.push_stmt(block_stmt);
    }

    fn push_stmt(&mut self, stmt: mlr::Stmt) {
        self.blocks
            .last_mut()
            .expect("block stack should not be empty")
            .push(stmt);
    }

    fn insert_loc_place(&mut self, loc: mlr::Loc) -> mlr::Place {
        let ty = self.ctxt.mlr.get_loc_ty(loc);
        let place = self.ctxt.mlr.insert_place(mlr::PlaceDef::Loc(loc));
        self.ctxt.mlr.set_place_ty(place, ty);
        place
    }

    fn insert_field_access_place(&mut self, base: mlr::Place, field_index: usize, ty: ty::Ty) -> mlr::Place {
        let place = self
            .ctxt
            .mlr
            .insert_place(mlr::PlaceDef::FieldAccess { base, field_index });
        self.ctxt.mlr.set_place_ty(place, ty);
        place
    }

    fn insert_closure_captures_place(&mut self, base: mlr::Place, captures_ty: ty::Ty) -> mlr::Place {
        let place = self.ctxt.mlr.insert_place(mlr::PlaceDef::ClosureCaptures(base));
        self.ctxt.mlr.set_place_ty(place, captures_ty);
        place
    }

    fn insert_enum_discriminant_place(&mut self, base: mlr::Place) -> mlr::Place {
        let i32_ty = self.ctxt.tys.primitive(ty::Primitive::Integer32);
        let place = self.ctxt.mlr.insert_place(mlr::PlaceDef::EnumDiscriminant { base });
        self.ctxt.mlr.set_place_ty(place, i32_ty);
        place
    }

    fn insert_project_to_variant_place(
        &mut self,
        base: mlr::Place,
        variant_index: usize,
        variant_ty: ty::Ty,
    ) -> mlr::Place {
        let place = self
            .ctxt
            .mlr
            .insert_place(mlr::PlaceDef::ProjectToVariant { base, variant_index });
        self.ctxt.mlr.set_place_ty(place, variant_ty);
        place
    }

    fn insert_deref_place(&mut self, op: mlr::Op) -> mlr::Place {
        let op_ty = self.ctxt.mlr.get_op_ty(op);
        let inner_ty = match self.ctxt.tys.get_ty_def(op_ty).cloned() {
            Some(ty::TyDef::Ref(inner) | ty::TyDef::Ptr(inner)) => inner,
            _ => panic!("deref of non-ref/ptr type"),
        };
        let place = self.ctxt.mlr.insert_place(mlr::PlaceDef::Deref(op));
        self.ctxt.mlr.set_place_ty(place, inner_ty);
        place
    }

    fn insert_copy_op(&mut self, place: mlr::Place) -> mlr::Op {
        let ty = self.ctxt.mlr.get_place_ty(place);
        let op = self.ctxt.mlr.insert_op(mlr::OpDef::Copy(place));
        self.ctxt.mlr.set_op_ty(op, ty);
        op
    }

    fn insert_fn_inst_op(&mut self, fn_inst: fns::FnInst) -> mlr::Op {
        self.ctxt.fns.register_fn_call(self.fn_, fn_inst);
        let ty = self.fn_ty_of_fn_inst(fn_inst);
        let op = self.ctxt.mlr.insert_op(mlr::OpDef::Fn(fn_inst));
        self.ctxt.mlr.set_op_ty(op, ty);
        op
    }

    fn insert_trait_mthd_op(&mut self, inst: fns::TraitMthdInst) -> mlr::Op {
        self.ctxt.fns.register_trait_mthd_call(self.fn_, inst);
        let ty = self.trait_mthd_fn_ty(inst);
        let op = self.ctxt.mlr.insert_op(mlr::OpDef::TraitMthd(inst));
        self.ctxt.mlr.set_op_ty(op, ty);
        op
    }

    fn insert_use_val(&mut self, op: mlr::Op) -> mlr::Val {
        let ty = self.ctxt.mlr.get_op_ty(op);
        let val = self.ctxt.mlr.insert_val(mlr::ValDef::Use(op));
        self.ctxt.mlr.set_val_ty(val, ty);
        val
    }

    fn insert_call_val(&mut self, callable: mlr::Op, args: Vec<mlr::Op>) -> mlr::Val {
        let callable_ty = self.ctxt.mlr.get_op_ty(callable);
        let return_ty = self
            .ctxt
            .ty_is_callable(callable_ty)
            .map(|(_, return_ty, _)| return_ty)
            .expect("callable op should have a callable type");
        let val = self.ctxt.mlr.insert_val(mlr::ValDef::Call { callable, args });
        self.ctxt.mlr.set_val_ty(val, return_ty);
        val
    }

    fn insert_addr_of_val(&mut self, place: mlr::Place) -> mlr::Val {
        let place_ty = self.ctxt.mlr.get_place_ty(place);
        let ref_ty = self.ctxt.tys.ref_(place_ty);
        let val = self.ctxt.mlr.insert_val(mlr::ValDef::AddrOf(place));
        self.ctxt.mlr.set_val_ty(val, ref_ty);
        val
    }

    fn insert_as_val(&mut self, op: mlr::Op, target_ty: ty::Ty) -> mlr::Val {
        let val = self.ctxt.mlr.insert_val(mlr::ValDef::As { op, target_ty });
        self.ctxt.mlr.set_val_ty(val, target_ty);
        val
    }

    fn insert_alloc_stmt(&mut self, loc: mlr::Loc) {
        let stmt = self.ctxt.mlr.insert_stmt(mlr::StmtDef::Alloc { loc });
        self.push_stmt(stmt);
    }

    fn insert_assign_stmt(&mut self, place: mlr::Place, value: mlr::Val) {
        let stmt = self.ctxt.mlr.insert_stmt(mlr::StmtDef::Assign { place, value });
        self.push_stmt(stmt);
    }

    fn insert_assign_to_loc_stmt(&mut self, loc: mlr::Loc, value: mlr::Val) {
        let place = self.insert_loc_place(loc);
        self.insert_assign_stmt(place, value);
    }

    fn insert_return_stmt(&mut self, value: mlr::Val) {
        let stmt = self.ctxt.mlr.insert_stmt(mlr::StmtDef::Return { value });
        self.push_stmt(stmt);
    }

    fn insert_if_stmt(&mut self, cond: mlr::Op, then: mlr::Stmt, else_: mlr::Stmt) {
        let stmt = self
            .ctxt
            .mlr
            .insert_stmt(mlr::StmtDef::If(mlr::If { cond, then, else_ }));
        self.push_stmt(stmt);
    }

    fn insert_loop_stmt(&mut self, body: mlr::Stmt) {
        let stmt = self.ctxt.mlr.insert_stmt(mlr::StmtDef::Loop { body });
        self.push_stmt(stmt);
    }

    fn insert_break_stmt(&mut self) {
        let stmt = self.ctxt.mlr.insert_stmt(mlr::StmtDef::Break);
        self.push_stmt(stmt);
    }

    fn insert_unit_val(&mut self) -> mlr::Val {
        let unit_ty = self.ctxt.tys.unit();
        let loc = self.ctxt.mlr.insert_typed_loc(unit_ty);
        self.insert_alloc_stmt(loc);
        let place = self.insert_loc_place(loc);
        let op = self.insert_copy_op(place);
        self.insert_use_val(op)
    }

    fn insert_bool_const(&mut self, b: bool) -> mlr::Op {
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);
        let op = self.ctxt.mlr.insert_op(mlr::OpDef::Const(mlr::Const::Bool(b)));
        self.ctxt.mlr.set_op_ty(op, bool_ty);
        op
    }
}
