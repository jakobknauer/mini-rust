use crate::{
    ctxt::{self, fns, language_items, ty},
    mlr,
};

pub struct MlrBuilder<'a, 'ctxt: 'mlr, 'mlr: 'ctxt> {
    pub ctxt: &'a mut ctxt::Ctxt<'ctxt>,
    pub mlr: &'mlr mlr::Mlr<'mlr>,
    fn_: fns::Fn,
    blocks: Vec<Vec<mlr::Stmt<'mlr>>>,
}

impl<'a, 'ctxt: 'mlr, 'mlr: 'ctxt> MlrBuilder<'a, 'ctxt, 'mlr> {
    pub fn new(ctxt: &'a mut ctxt::Ctxt<'ctxt>, mlr: &'mlr mlr::Mlr<'mlr>, fn_: fns::Fn) -> Self {
        Self {
            ctxt,
            mlr,
            fn_,
            blocks: Vec::new(),
        }
    }

    pub fn register_fn_call(&mut self, fn_inst: fns::FnInst<'ctxt>) {
        self.ctxt.fns.register_fn_call(self.fn_, fn_inst);
    }

    pub fn start_block(&mut self) {
        self.blocks.push(Vec::new());
    }

    pub fn end_block(&mut self) -> mlr::Stmt<'mlr> {
        let stmts = self.blocks.pop().expect("block stack should not be empty");
        let stmts = self.mlr.insert_stmt_slice(&stmts);
        self.mlr.insert_stmt(mlr::StmtDef::Block(stmts))
    }

    pub fn end_and_push_block(&mut self) {
        let block_stmt = self.end_block();
        self.push_stmt(block_stmt);
    }

    pub fn push_stmt(&mut self, stmt: mlr::Stmt<'mlr>) {
        self.blocks
            .last_mut()
            .expect("block stack should not be empty")
            .push(stmt);
    }

    pub fn insert_typed_loc(&self, ty: ty::Ty<'ctxt>) -> mlr::Loc<'mlr> {
        self.mlr.insert_typed_loc(ty)
    }

    pub fn alloc_loc(&mut self, ty: ty::Ty<'ctxt>) -> mlr::Loc<'mlr> {
        let loc = self.mlr.insert_typed_loc(ty);
        self.insert_alloc_stmt(loc);
        loc
    }

    pub fn alloc_place(&mut self, ty: ty::Ty<'ctxt>) -> mlr::Place<'mlr> {
        let loc = self.alloc_loc(ty);
        self.insert_loc_place(loc)
    }

    pub fn store_val(&mut self, val: mlr::Val<'mlr>) -> mlr::Place<'mlr> {
        let place = self.alloc_place(val.1);
        self.insert_assign_stmt(place, val);
        place
    }

    pub fn insert_loc_place(&self, loc: mlr::Loc<'mlr>) -> mlr::Place<'mlr> {
        self.mlr.insert_place(mlr::PlaceDef::Loc(loc), loc.1)
    }

    pub fn insert_field_access_place(
        &self,
        base: mlr::Place<'mlr>,
        field_index: usize,
        ty: ty::Ty<'ctxt>,
    ) -> mlr::Place<'mlr> {
        self.mlr
            .insert_place(mlr::PlaceDef::FieldAccess { base, field_index }, ty)
    }

    pub fn insert_closure_captures_place(
        &self,
        base: mlr::Place<'mlr>,
        captures_ty: ty::Ty<'ctxt>,
    ) -> mlr::Place<'mlr> {
        self.mlr.insert_place(mlr::PlaceDef::ClosureCaptures(base), captures_ty)
    }

    pub fn insert_enum_discriminant_place(&mut self, base: mlr::Place<'mlr>) -> mlr::Place<'mlr> {
        let i32_ty = self.ctxt.tys.primitive(ty::Primitive::Integer32);
        self.mlr.insert_place(mlr::PlaceDef::EnumDiscriminant { base }, i32_ty)
    }

    pub fn insert_project_to_variant_place(
        &self,
        base: mlr::Place<'mlr>,
        variant_index: usize,
        variant_ty: ty::Ty<'ctxt>,
    ) -> mlr::Place<'mlr> {
        self.mlr
            .insert_place(mlr::PlaceDef::ProjectToVariant { base, variant_index }, variant_ty)
    }

    pub fn insert_deref_place(&self, op: mlr::Op<'mlr>) -> mlr::Place<'mlr> {
        let inner_ty = match op.1.0 {
            &ty::TyDef::Ref(inner) | &ty::TyDef::Ptr(inner) => inner,
            _ => panic!("deref of non-ref/ptr type"),
        };
        self.mlr.insert_place(mlr::PlaceDef::Deref(op), inner_ty)
    }

    pub fn insert_copy_op(&self, place: mlr::Place<'mlr>) -> mlr::Op<'mlr> {
        self.mlr.insert_op(mlr::OpDef::Copy(place), place.1)
    }

    pub fn insert_fn_inst_op(&mut self, fn_inst: fns::FnInst<'ctxt>) -> mlr::Op<'mlr> {
        self.ctxt.fns.register_fn_call(self.fn_, fn_inst);
        let ty = self.fn_ty_of_fn_inst(fn_inst);
        self.mlr.insert_op(mlr::OpDef::Fn(fn_inst), ty)
    }

    pub fn insert_trait_mthd_op(&mut self, inst: fns::TraitMthdInst<'ctxt>) -> mlr::Op<'mlr> {
        self.ctxt.fns.register_trait_mthd_call(self.fn_, inst);
        let ty = self.trait_mthd_fn_ty(inst);
        self.mlr.insert_op(mlr::OpDef::TraitMthd(inst), ty)
    }

    pub fn insert_const_op(&self, const_: mlr::Const, ty: ty::Ty<'ctxt>) -> mlr::Op<'mlr> {
        self.mlr.insert_op(mlr::OpDef::Const(const_), ty)
    }

    pub fn insert_bool_const(&mut self, b: bool) -> mlr::Op<'mlr> {
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);
        self.mlr.insert_op(mlr::OpDef::Const(mlr::Const::Bool(b)), bool_ty)
    }

    pub fn insert_use_val(&self, op: mlr::Op<'mlr>) -> mlr::Val<'mlr> {
        self.mlr.insert_val(mlr::ValDef::Use(op), op.1)
    }

    pub fn copy_val(&self, place: mlr::Place<'mlr>) -> mlr::Val<'mlr> {
        let op = self.insert_copy_op(place);
        self.insert_use_val(op)
    }

    pub fn insert_call_val(&mut self, callable: mlr::Op<'mlr>, args: Vec<mlr::Op<'mlr>>) -> mlr::Val<'mlr> {
        let constraints: Vec<_> = self
            .ctxt
            .fns
            .get_sig(self.fn_)
            .unwrap()
            .all_constraints()
            .cloned()
            .collect();
        let return_ty = self
            .ctxt
            .ty_is_callable(&constraints, callable.1)
            .map(|(_, return_ty, _)| return_ty)
            .expect("callable op should have a callable type");
        self.mlr.insert_val(mlr::ValDef::Call { callable, args }, return_ty)
    }

    pub fn insert_addr_of_val(&mut self, place: mlr::Place<'mlr>) -> mlr::Val<'mlr> {
        let ref_ty = self.ctxt.tys.ref_(place.1);
        self.mlr.insert_val(mlr::ValDef::AddrOf(place), ref_ty)
    }

    pub fn insert_as_val(&self, op: mlr::Op<'mlr>, target_ty: ty::Ty<'ctxt>) -> mlr::Val<'mlr> {
        self.mlr.insert_val(mlr::ValDef::As { op, target_ty }, target_ty)
    }

    pub fn insert_binary_prim_val(
        &self,
        op: language_items::BinaryPrimOp,
        lhs: mlr::Op<'mlr>,
        rhs: mlr::Op<'mlr>,
        result_ty: ty::Ty<'ctxt>,
    ) -> mlr::Val<'mlr> {
        self.mlr.insert_val(mlr::ValDef::BinaryPrim { op, lhs, rhs }, result_ty)
    }

    pub fn insert_unary_prim_val(
        &self,
        op: language_items::UnaryPrimOp,
        operand: mlr::Op<'mlr>,
        result_ty: ty::Ty<'ctxt>,
    ) -> mlr::Val<'mlr> {
        self.mlr.insert_val(mlr::ValDef::UnaryPrim { op, operand }, result_ty)
    }

    pub fn insert_unit_val(&mut self) -> mlr::Val<'mlr> {
        let unit_ty = self.ctxt.tys.unit();
        let place = self.alloc_place(unit_ty);
        self.copy_val(place)
    }

    pub fn insert_alloc_stmt(&mut self, loc: mlr::Loc<'mlr>) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Alloc { loc });
        self.push_stmt(stmt);
    }

    pub fn insert_assign_stmt(&mut self, place: mlr::Place<'mlr>, value: mlr::Val<'mlr>) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Assign { place, value });
        self.push_stmt(stmt);
    }

    pub fn insert_assign_to_loc_stmt(&mut self, loc: mlr::Loc<'mlr>, value: mlr::Val<'mlr>) {
        let place = self.insert_loc_place(loc);
        self.insert_assign_stmt(place, value);
    }

    pub fn insert_return_stmt(&mut self, value: mlr::Val<'mlr>) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Return { value });
        self.push_stmt(stmt);
    }

    pub fn insert_if_stmt(&mut self, cond: mlr::Op<'mlr>, then: mlr::Stmt<'mlr>, else_: mlr::Stmt<'mlr>) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::If(mlr::If { cond, then, else_ }));
        self.push_stmt(stmt);
    }

    pub fn insert_loop_stmt(&mut self, body: mlr::Stmt<'mlr>) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Loop { body });
        self.push_stmt(stmt);
    }

    pub fn insert_break_stmt(&mut self) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Break);
        self.push_stmt(stmt);
    }

    fn fn_ty_of_fn_inst(&mut self, fn_inst: fns::FnInst<'ctxt>) -> ty::Ty<'ctxt> {
        let sig = self.ctxt.fns.get_sig(fn_inst.fn_).unwrap();
        let param_tys: Vec<_> = sig.params.iter().map(|p| p.ty).collect();
        let return_ty = sig.return_ty;
        let var_args = sig.var_args;
        let _ = sig;
        let fn_ty = self.ctxt.tys.fn_(&param_tys, return_ty, var_args);
        let subst = self.ctxt.get_subst_for_fn_inst(fn_inst);
        self.ctxt.tys.substitute_gen_vars(fn_ty, &subst)
    }

    fn trait_mthd_fn_ty(&mut self, inst: fns::TraitMthdInst<'ctxt>) -> ty::Ty<'ctxt> {
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
            ty::GenVarSubst::new(&trait_gen_params, inst.trait_inst.gen_args).unwrap();
        let mthd_subst = ty::GenVarSubst::new(&sig_gen_params, inst.gen_args).unwrap();
        let all_subst = ty::GenVarSubst::compose(trait_subst, mthd_subst);

        self.ctxt.tys.substitute(fn_ty, &all_subst, Some(inst.impl_ty))
    }
}
