use crate::{
    ctxt::{self, fns, language_items, ty},
    mlr,
};

pub struct MlrBuilder<'ctxt, 'mlr> {
    pub ctxt: &'ctxt mut ctxt::Ctxt,
    pub mlr: &'mlr mut mlr::Mlr,
    fn_: fns::Fn,
    blocks: Vec<Vec<mlr::Stmt>>,
}

impl<'ctxt, 'mlr> MlrBuilder<'ctxt, 'mlr> {
    pub fn new(ctxt: &'ctxt mut ctxt::Ctxt, mlr: &'mlr mut mlr::Mlr, fn_: fns::Fn) -> Self {
        Self {
            ctxt,
            mlr,
            fn_,
            blocks: Vec::new(),
        }
    }

    pub fn register_fn_call(&mut self, fn_inst: fns::FnInst) {
        self.ctxt.fns.register_fn_call(self.fn_, fn_inst);
    }

    pub fn start_block(&mut self) {
        self.blocks.push(Vec::new());
    }

    pub fn end_block(&mut self) -> mlr::Stmt {
        let stmts = self.blocks.pop().expect("block stack should not be empty");
        self.mlr.insert_stmt(mlr::StmtDef::Block(stmts))
    }

    pub fn end_and_push_block(&mut self) {
        let block_stmt = self.end_block();
        self.push_stmt(block_stmt);
    }

    pub fn push_stmt(&mut self, stmt: mlr::Stmt) {
        self.blocks
            .last_mut()
            .expect("block stack should not be empty")
            .push(stmt);
    }

    pub fn insert_typed_loc(&mut self, ty: ty::Ty) -> mlr::Loc {
        self.mlr.insert_typed_loc(ty)
    }

    pub fn alloc_loc(&mut self, ty: ty::Ty) -> mlr::Loc {
        let loc = self.mlr.insert_typed_loc(ty);
        self.insert_alloc_stmt(loc);
        loc
    }

    pub fn alloc_place(&mut self, ty: ty::Ty) -> mlr::Place {
        let loc = self.alloc_loc(ty);
        self.insert_loc_place(loc)
    }

    pub fn store_val(&mut self, val: mlr::Val) -> mlr::Place {
        let ty = self.get_val_ty(val);
        let place = self.alloc_place(ty);
        self.insert_assign_stmt(place, val);
        place
    }

    pub fn insert_loc_place(&mut self, loc: mlr::Loc) -> mlr::Place {
        let ty = self.mlr.get_loc_ty(loc);
        let place = self.mlr.insert_place(mlr::PlaceDef::Loc(loc));
        self.mlr.set_place_ty(place, ty);
        place
    }

    pub fn insert_field_access_place(&mut self, base: mlr::Place, field_index: usize, ty: ty::Ty) -> mlr::Place {
        let place = self.mlr.insert_place(mlr::PlaceDef::FieldAccess { base, field_index });
        self.mlr.set_place_ty(place, ty);
        place
    }

    pub fn insert_closure_captures_place(&mut self, base: mlr::Place, captures_ty: ty::Ty) -> mlr::Place {
        let place = self.mlr.insert_place(mlr::PlaceDef::ClosureCaptures(base));
        self.mlr.set_place_ty(place, captures_ty);
        place
    }

    pub fn insert_enum_discriminant_place(&mut self, base: mlr::Place) -> mlr::Place {
        let i32_ty = self.ctxt.tys.primitive(ty::Primitive::Integer32);
        let place = self.mlr.insert_place(mlr::PlaceDef::EnumDiscriminant { base });
        self.mlr.set_place_ty(place, i32_ty);
        place
    }

    pub fn insert_project_to_variant_place(
        &mut self,
        base: mlr::Place,
        variant_index: usize,
        variant_ty: ty::Ty,
    ) -> mlr::Place {
        let place = self
            .mlr
            .insert_place(mlr::PlaceDef::ProjectToVariant { base, variant_index });
        self.mlr.set_place_ty(place, variant_ty);
        place
    }

    pub fn insert_deref_place(&mut self, op: mlr::Op) -> mlr::Place {
        let op_ty = self.mlr.get_op_ty(op);
        let inner_ty = match self.ctxt.tys.get_ty_def(op_ty) {
            &ty::TyDef::Ref(inner) | &ty::TyDef::Ptr(inner) => inner,
            _ => panic!("deref of non-ref/ptr type"),
        };
        let place = self.mlr.insert_place(mlr::PlaceDef::Deref(op));
        self.mlr.set_place_ty(place, inner_ty);
        place
    }

    pub fn insert_copy_op(&mut self, place: mlr::Place) -> mlr::Op {
        let ty = self.mlr.get_place_ty(place);
        let op = self.mlr.insert_op(mlr::OpDef::Copy(place));
        self.mlr.set_op_ty(op, ty);
        op
    }

    pub fn insert_fn_inst_op(&mut self, fn_inst: fns::FnInst) -> mlr::Op {
        self.ctxt.fns.register_fn_call(self.fn_, fn_inst);
        let ty = self.fn_ty_of_fn_inst(fn_inst);
        let op = self.mlr.insert_op(mlr::OpDef::Fn(fn_inst));
        self.mlr.set_op_ty(op, ty);
        op
    }

    pub fn insert_trait_mthd_op(&mut self, inst: fns::TraitMthdInst) -> mlr::Op {
        self.ctxt.fns.register_trait_mthd_call(self.fn_, inst);
        let ty = self.trait_mthd_fn_ty(inst);
        let op = self.mlr.insert_op(mlr::OpDef::TraitMthd(inst));
        self.mlr.set_op_ty(op, ty);
        op
    }

    pub fn insert_const_op(&mut self, const_: mlr::Const, ty: ty::Ty) -> mlr::Op {
        let op = self.mlr.insert_op(mlr::OpDef::Const(const_));
        self.mlr.set_op_ty(op, ty);
        op
    }

    pub fn insert_bool_const(&mut self, b: bool) -> mlr::Op {
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);
        let op = self.mlr.insert_op(mlr::OpDef::Const(mlr::Const::Bool(b)));
        self.mlr.set_op_ty(op, bool_ty);
        op
    }

    pub fn get_val_ty(&self, val: mlr::Val) -> ty::Ty {
        self.mlr.get_val_ty(val)
    }

    pub fn insert_use_val(&mut self, op: mlr::Op) -> mlr::Val {
        let ty = self.mlr.get_op_ty(op);
        let val = self.mlr.insert_val(mlr::ValDef::Use(op));
        self.mlr.set_val_ty(val, ty);
        val
    }

    pub fn copy_val(&mut self, place: mlr::Place) -> mlr::Val {
        let op = self.insert_copy_op(place);
        self.insert_use_val(op)
    }

    pub fn insert_call_val(&mut self, callable: mlr::Op, args: Vec<mlr::Op>) -> mlr::Val {
        let callable_ty = self.mlr.get_op_ty(callable);
        let return_ty = self
            .ctxt
            .ty_is_callable(callable_ty)
            .map(|(_, return_ty, _)| return_ty)
            .expect("callable op should have a callable type");
        let val = self.mlr.insert_val(mlr::ValDef::Call { callable, args });
        self.mlr.set_val_ty(val, return_ty);
        val
    }

    pub fn insert_addr_of_val(&mut self, place: mlr::Place) -> mlr::Val {
        let place_ty = self.mlr.get_place_ty(place);
        let ref_ty = self.ctxt.tys.ref_(place_ty);
        let val = self.mlr.insert_val(mlr::ValDef::AddrOf(place));
        self.mlr.set_val_ty(val, ref_ty);
        val
    }

    pub fn insert_as_val(&mut self, op: mlr::Op, target_ty: ty::Ty) -> mlr::Val {
        let val = self.mlr.insert_val(mlr::ValDef::As { op, target_ty });
        self.mlr.set_val_ty(val, target_ty);
        val
    }

    pub fn insert_binary_prim_val(
        &mut self,
        op: language_items::BinaryPrimOp,
        lhs: mlr::Op,
        rhs: mlr::Op,
        result_ty: ty::Ty,
    ) -> mlr::Val {
        let val = self.mlr.insert_val(mlr::ValDef::BinaryPrim { op, lhs, rhs });
        self.mlr.set_val_ty(val, result_ty);
        val
    }

    pub fn insert_unary_prim_val(
        &mut self,
        op: language_items::UnaryPrimOp,
        operand: mlr::Op,
        result_ty: ty::Ty,
    ) -> mlr::Val {
        let val = self.mlr.insert_val(mlr::ValDef::UnaryPrim { op, operand });
        self.mlr.set_val_ty(val, result_ty);
        val
    }

    pub fn insert_unit_val(&mut self) -> mlr::Val {
        let unit_ty = self.ctxt.tys.unit();
        let place = self.alloc_place(unit_ty);
        self.copy_val(place)
    }

    pub fn insert_alloc_stmt(&mut self, loc: mlr::Loc) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Alloc { loc });
        self.push_stmt(stmt);
    }

    pub fn insert_assign_stmt(&mut self, place: mlr::Place, value: mlr::Val) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Assign { place, value });
        self.push_stmt(stmt);
    }

    pub fn insert_assign_to_loc_stmt(&mut self, loc: mlr::Loc, value: mlr::Val) {
        let place = self.insert_loc_place(loc);
        self.insert_assign_stmt(place, value);
    }

    pub fn insert_return_stmt(&mut self, value: mlr::Val) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Return { value });
        self.push_stmt(stmt);
    }

    pub fn insert_if_stmt(&mut self, cond: mlr::Op, then: mlr::Stmt, else_: mlr::Stmt) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::If(mlr::If { cond, then, else_ }));
        self.push_stmt(stmt);
    }

    pub fn insert_loop_stmt(&mut self, body: mlr::Stmt) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Loop { body });
        self.push_stmt(stmt);
    }

    pub fn insert_break_stmt(&mut self) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Break);
        self.push_stmt(stmt);
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
}
