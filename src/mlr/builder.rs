use crate::{
    ctxt::{self, fns, language_items, ty},
    mlr,
};

pub struct MlrBuilder<'a, 'ctxt> {
    pub ctxt: &'a mut ctxt::Ctxt<'ctxt>,
    pub mlr: &'a mlr::Mlr<'ctxt>,
    fn_: fns::Fn<'ctxt>,
    blocks: Vec<Vec<mlr::Stmt<'ctxt>>>,
}

impl<'a, 'ctxt> MlrBuilder<'a, 'ctxt> {
    pub fn new(ctxt: &'a mut ctxt::Ctxt<'ctxt>, mlr: &'a mlr::Mlr<'ctxt>, fn_: fns::Fn<'ctxt>) -> Self {
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

    pub fn end_block(&mut self) -> mlr::Stmt<'ctxt> {
        let stmts = self.blocks.pop().expect("block stack should not be empty");
        let stmts = self.mlr.insert_stmt_slice(&stmts);
        self.mlr.insert_stmt(mlr::StmtDef::Block(stmts))
    }

    pub fn end_and_push_block(&mut self) {
        let block_stmt = self.end_block();
        self.push_stmt(block_stmt);
    }

    pub fn push_stmt(&mut self, stmt: mlr::Stmt<'ctxt>) {
        self.blocks
            .last_mut()
            .expect("block stack should not be empty")
            .push(stmt);
    }

    pub fn insert_typed_loc(&self, ty: ty::Ty<'ctxt>) -> mlr::Loc<'ctxt> {
        self.mlr.insert_typed_loc(ty)
    }

    pub fn alloc_loc(&mut self, ty: ty::Ty<'ctxt>) -> mlr::Loc<'ctxt> {
        let loc = self.mlr.insert_typed_loc(ty);
        self.insert_alloc_stmt(loc, false);
        loc
    }

    pub fn alloc_mut_loc(&mut self, ty: ty::Ty<'ctxt>) -> mlr::Loc<'ctxt> {
        let loc = self.mlr.insert_typed_loc(ty);
        self.insert_alloc_stmt(loc, true);
        loc
    }

    pub fn alloc_place(&mut self, ty: ty::Ty<'ctxt>) -> mlr::Place<'ctxt> {
        let loc = self.alloc_mut_loc(ty);
        self.insert_loc_place(loc)
    }

    pub fn store_val(&mut self, val: mlr::Val<'ctxt>) -> mlr::Place<'ctxt> {
        let place = self.alloc_place(val.1);
        self.insert_assign_stmt(place, val);
        place
    }

    pub fn insert_loc_place(&self, loc: mlr::Loc<'ctxt>) -> mlr::Place<'ctxt> {
        self.mlr.insert_place(mlr::PlaceDef::Loc(loc), loc.1)
    }

    pub fn insert_field_access_place(
        &self,
        base: mlr::Place<'ctxt>,
        field_index: usize,
        ty: ty::Ty<'ctxt>,
    ) -> mlr::Place<'ctxt> {
        self.mlr
            .insert_place(mlr::PlaceDef::FieldAccess { base, field_index }, ty)
    }

    pub fn insert_closure_captures_place(
        &self,
        base: mlr::Place<'ctxt>,
        captures_ty: ty::Ty<'ctxt>,
    ) -> mlr::Place<'ctxt> {
        self.mlr.insert_place(mlr::PlaceDef::ClosureCaptures(base), captures_ty)
    }

    pub fn insert_enum_discriminant_place(&mut self, base: mlr::Place<'ctxt>) -> mlr::Place<'ctxt> {
        let i32_ty = self.ctxt.tys.primitive(ty::Primitive::Integer32);
        self.mlr.insert_place(mlr::PlaceDef::EnumDiscriminant { base }, i32_ty)
    }

    pub fn insert_project_to_variant_place(
        &self,
        base: mlr::Place<'ctxt>,
        variant_index: usize,
        variant_ty: ty::Ty<'ctxt>,
    ) -> mlr::Place<'ctxt> {
        self.mlr
            .insert_place(mlr::PlaceDef::ProjectToVariant { base, variant_index }, variant_ty)
    }

    pub fn insert_deref_place(&self, op: mlr::Op<'ctxt>) -> mlr::Place<'ctxt> {
        let inner_ty = match op.1.0 {
            &ty::TyDef::Ref(inner) | &ty::TyDef::RefMut(inner) | &ty::TyDef::Ptr(inner) => {
                self.ctxt.normalize_ty(inner)
            }
            _ => panic!("deref of non-ref/ptr type"),
        };
        self.mlr.insert_place(mlr::PlaceDef::Deref(op), inner_ty)
    }

    pub fn insert_copy_op(&self, place: mlr::Place<'ctxt>) -> mlr::Op<'ctxt> {
        self.mlr.insert_op(mlr::OpDef::Copy(place), place.1)
    }

    pub fn insert_fn_inst_op(&mut self, fn_inst: fns::FnInst<'ctxt>) -> mlr::Op<'ctxt> {
        self.ctxt.fns.register_fn_call(self.fn_, fn_inst);
        let ty = self.fn_ty_of_fn_inst(fn_inst);
        self.mlr.insert_op(mlr::OpDef::Fn(fn_inst), ty)
    }

    pub fn insert_trait_mthd_op(&mut self, inst: fns::TraitMthdInst<'ctxt>) -> mlr::Op<'ctxt> {
        self.ctxt.fns.register_trait_mthd_call(self.fn_, inst);
        let ty = self.trait_mthd_fn_ty(inst);
        self.mlr.insert_op(mlr::OpDef::TraitMthdCall(inst), ty)
    }

    pub fn insert_const_op(&self, const_: mlr::Const, ty: ty::Ty<'ctxt>) -> mlr::Op<'ctxt> {
        self.mlr.insert_op(mlr::OpDef::Const(const_), ty)
    }

    pub fn insert_bool_const(&mut self, b: bool) -> mlr::Op<'ctxt> {
        let bool_ty = self.ctxt.tys.primitive(ty::Primitive::Boolean);
        self.mlr.insert_op(mlr::OpDef::Const(mlr::Const::Bool(b)), bool_ty)
    }

    pub fn insert_use_val(&self, op: mlr::Op<'ctxt>) -> mlr::Val<'ctxt> {
        self.mlr.insert_val(mlr::ValDef::Use(op), op.1)
    }

    pub fn copy_val(&self, place: mlr::Place<'ctxt>) -> mlr::Val<'ctxt> {
        let op = self.insert_copy_op(place);
        self.insert_use_val(op)
    }

    pub fn insert_call_val(&mut self, callable: mlr::Op<'ctxt>, args: Vec<mlr::Op<'ctxt>>) -> mlr::Val<'ctxt> {
        let constraints: Vec<_> = self.fn_.all_constraints().cloned().collect();
        let return_ty = self
            .ctxt
            .ty_is_callable(&constraints, callable.1)
            .map(|(_, return_ty, _)| return_ty)
            .expect("callable op should have a callable type");
        self.mlr.insert_val(mlr::ValDef::Call { callable, args }, return_ty)
    }

    pub fn insert_addr_of_val(&mut self, place: mlr::Place<'ctxt>) -> mlr::Val<'ctxt> {
        let ref_ty = self.ctxt.tys.ref_(place.1);
        self.mlr.insert_val(mlr::ValDef::AddrOf(place), ref_ty)
    }

    pub fn insert_addr_of_mut_val(&mut self, place: mlr::Place<'ctxt>) -> mlr::Val<'ctxt> {
        let ref_ty = self.ctxt.tys.ref_mut(place.1);
        self.mlr.insert_val(mlr::ValDef::AddrOf(place), ref_ty)
    }

    pub fn insert_as_val(&self, op: mlr::Op<'ctxt>, target_ty: ty::Ty<'ctxt>) -> mlr::Val<'ctxt> {
        self.mlr.insert_val(mlr::ValDef::As { op, target_ty }, target_ty)
    }

    pub fn insert_binary_prim_val(
        &self,
        op: language_items::BinaryPrimOp,
        lhs: mlr::Op<'ctxt>,
        rhs: mlr::Op<'ctxt>,
        result_ty: ty::Ty<'ctxt>,
    ) -> mlr::Val<'ctxt> {
        self.mlr.insert_val(mlr::ValDef::BinaryPrim { op, lhs, rhs }, result_ty)
    }

    pub fn insert_unary_prim_val(
        &self,
        op: language_items::UnaryPrimOp,
        operand: mlr::Op<'ctxt>,
        result_ty: ty::Ty<'ctxt>,
    ) -> mlr::Val<'ctxt> {
        self.mlr.insert_val(mlr::ValDef::UnaryPrim { op, operand }, result_ty)
    }

    pub fn insert_unit_val(&mut self) -> mlr::Val<'ctxt> {
        let unit_ty = self.ctxt.tys.unit();
        let place = self.alloc_place(unit_ty);
        self.copy_val(place)
    }

    pub fn insert_alloc_stmt(&mut self, loc: mlr::Loc<'ctxt>, mutable: bool) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Alloc { loc, mutable });
        self.push_stmt(stmt);
    }

    pub fn insert_assign_stmt(&mut self, place: mlr::Place<'ctxt>, value: mlr::Val<'ctxt>) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Assign { place, value });
        self.push_stmt(stmt);
    }

    pub fn insert_assign_to_loc_stmt(&mut self, loc: mlr::Loc<'ctxt>, value: mlr::Val<'ctxt>) {
        let place = self.insert_loc_place(loc);
        self.insert_assign_stmt(place, value);
    }

    pub fn insert_return_stmt(&mut self, value: mlr::Val<'ctxt>) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Return { value });
        self.push_stmt(stmt);
    }

    pub fn insert_if_stmt(&mut self, cond: mlr::Op<'ctxt>, then: mlr::Stmt<'ctxt>, else_: mlr::Stmt<'ctxt>) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::If(mlr::If { cond, then, else_ }));
        self.push_stmt(stmt);
    }

    pub fn insert_loop_stmt(&mut self, body: mlr::Stmt<'ctxt>) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Loop { body });
        self.push_stmt(stmt);
    }

    pub fn insert_break_stmt(&mut self) {
        let stmt = self.mlr.insert_stmt(mlr::StmtDef::Break);
        self.push_stmt(stmt);
    }

    fn fn_ty_of_fn_inst(&mut self, fn_inst: fns::FnInst<'ctxt>) -> ty::Ty<'ctxt> {
        let param_tys: Vec<_> = fn_inst.fn_.params.iter().map(|p| p.ty).collect();
        let return_ty = fn_inst.fn_.return_ty;
        let var_args = fn_inst.fn_.var_args;
        let fn_ty = self.ctxt.tys.fn_(&param_tys, return_ty, var_args);
        let subst = fn_inst.get_subst();
        self.ctxt.tys.substitute(fn_ty, &subst, fn_inst.self_ty)
    }

    fn trait_mthd_fn_ty(&mut self, inst: fns::TraitMthdInst<'ctxt>) -> ty::Ty<'ctxt> {
        let param_tys: Vec<_> = inst.mthd.fn_.params.iter().map(|p| p.ty).collect();
        let fn_ty = self
            .ctxt
            .tys
            .fn_(&param_tys, inst.mthd.fn_.return_ty, inst.mthd.fn_.var_args);

        let all_subst = inst.get_subst();

        self.ctxt.tys.substitute(fn_ty, &all_subst, Some(inst.impl_ty))
    }
}
