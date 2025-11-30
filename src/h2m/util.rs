use crate::{
    ctxt::{fns, mlr, ty},
    h2m::{H2MErr, H2MResult},
    hlr,
    typechecker::Typechecker,
};

impl<'a> super::H2M<'a> {
    pub(super) fn typechecker(&mut self) -> Typechecker<'_> {
        Typechecker::new(self.ctxt, self.target_fn)
    }

    fn insert_val(&mut self, val_def: mlr::ValDef) -> H2MResult<mlr::Val> {
        let val = self.ctxt.mlr.insert_val(val_def);
        self.typechecker().infer_val_ty(val)?;
        Ok(val)
    }

    pub fn insert_call_val(&mut self, callable: mlr::Op, args: Vec<mlr::Op>) -> H2MResult<mlr::Val> {
        let val = mlr::ValDef::Call { callable, args };
        self.insert_val(val)
    }

    pub fn insert_use_val(&mut self, op: mlr::Op) -> H2MResult<mlr::Val> {
        let val = mlr::ValDef::Use(op);
        self.insert_val(val)
    }

    pub fn insert_use_place_val(&mut self, place: mlr::Place) -> H2MResult<mlr::Val> {
        let op = self.insert_copy_op(place)?;
        self.insert_use_val(op)
    }

    pub fn insert_addr_of_val(&mut self, place: mlr::Place) -> H2MResult<mlr::Val> {
        let val = mlr::ValDef::AddrOf(place);
        self.insert_val(val)
    }

    fn insert_stmt(&mut self, stmt_def: mlr::StmtDef) -> H2MResult<mlr::Stmt> {
        let stmt = self.ctxt.mlr.insert_stmt(stmt_def);
        self.typechecker().check_stmt_ty(stmt)?;

        self.blocks
            .back_mut()
            .expect("self.blocks should not be empty")
            .push(stmt);
        Ok(stmt)
    }

    pub fn insert_if_stmt(&mut self, cond: mlr::Op, then_: mlr::Stmt, else_: mlr::Stmt) -> H2MResult<mlr::Stmt> {
        let if_ = mlr::StmtDef::If(mlr::If {
            cond,
            then: then_,
            else_,
        });
        self.insert_stmt(if_)
    }

    pub fn insert_loop_stmt(&mut self, body: mlr::Stmt) -> H2MResult<mlr::Stmt> {
        let val = mlr::StmtDef::Loop { body };
        self.insert_stmt(val)
    }

    pub fn insert_alloc_stmt(&mut self, loc: mlr::Loc) -> H2MResult<mlr::Stmt> {
        let stmt = mlr::StmtDef::Alloc { loc };
        self.insert_stmt(stmt)
    }

    pub fn insert_fresh_alloc(&mut self) -> H2MResult<mlr::Place> {
        let undef = self.ctxt.tys.new_undefined_ty();
        self.insert_alloc_with_ty(undef)
    }

    pub fn insert_alloc_with_ty(&mut self, ty: ty::Ty) -> H2MResult<mlr::Place> {
        let loc = self.ctxt.mlr.insert_typed_loc(ty);
        self.insert_alloc_stmt(loc)?;
        self.insert_loc_place(loc)
    }

    pub fn insert_break_stmt(&mut self) -> H2MResult<mlr::Stmt> {
        let stmt = mlr::StmtDef::Break;
        self.insert_stmt(stmt)
    }

    pub fn insert_assign_stmt(&mut self, place: mlr::Place, value: mlr::Val) -> H2MResult<mlr::Stmt> {
        let stmt = mlr::StmtDef::Assign { place, value };
        self.insert_stmt(stmt)
    }

    pub fn insert_assign_to_loc_stmt(&mut self, loc: mlr::Loc, value: mlr::Val) -> H2MResult<mlr::Stmt> {
        let place = mlr::PlaceDef::Loc(loc);
        let place = self.insert_place(place)?;
        self.insert_assign_stmt(place, value)
    }

    pub fn insert_return_stmt(&mut self, value: mlr::Val) -> H2MResult<mlr::Stmt> {
        let stmt = mlr::StmtDef::Return { value };
        self.insert_stmt(stmt)
    }

    fn insert_place(&mut self, place_def: mlr::PlaceDef) -> H2MResult<mlr::Place> {
        let place = self.ctxt.mlr.insert_place(place_def);
        self.typechecker().infer_place_ty(place)?;
        Ok(place)
    }

    pub fn insert_loc_place(&mut self, loc: mlr::Loc) -> H2MResult<mlr::Place> {
        let place = mlr::PlaceDef::Loc(loc);
        self.insert_place(place)
    }

    pub fn insert_enum_discriminant_place(&mut self, base: mlr::Place) -> H2MResult<mlr::Place> {
        let place = mlr::PlaceDef::EnumDiscriminant { base };
        self.insert_place(place)
    }

    pub fn insert_project_to_variant_place(&mut self, base: mlr::Place, variant_index: usize) -> H2MResult<mlr::Place> {
        let place = mlr::PlaceDef::ProjectToVariant { base, variant_index };
        self.insert_place(place)
    }

    pub fn insert_field_access_place(&mut self, base: mlr::Place, field_index: usize) -> H2MResult<mlr::Place> {
        let place = mlr::PlaceDef::FieldAccess { base, field_index };
        self.insert_place(place)
    }

    pub fn insert_deref_place(&mut self, op: mlr::Op) -> H2MResult<mlr::Place> {
        let place = mlr::PlaceDef::Deref(op);
        self.insert_place(place)
    }

    fn insert_op(&mut self, op_def: mlr::OpDef) -> H2MResult<mlr::Op> {
        let op = self.ctxt.mlr.insert_op(op_def);
        self.typechecker().infer_op_ty(op)?;
        Ok(op)
    }

    pub fn insert_fn_op(&mut self, fn_: fns::Fn) -> H2MResult<mlr::Op> {
        self.insert_gen_fn_op(fn_, Vec::new())
    }

    pub fn insert_gen_fn_op(&mut self, fn_: fns::Fn, gen_args: Vec<ty::Ty>) -> H2MResult<mlr::Op> {
        self.ctxt.fns.specialize_fn(&self.target_fn, &fn_, gen_args.clone());
        let op = mlr::OpDef::Fn(fns::FnSpecialization { fn_, gen_args });
        self.insert_op(op)
    }

    pub fn insert_int_op(&mut self, int: i64) -> H2MResult<mlr::Op> {
        let val = mlr::OpDef::Const(mlr::Const::Int(int));
        self.insert_op(val)
    }

    pub fn insert_bool_op(&mut self, boolean: bool) -> H2MResult<mlr::Op> {
        let op = mlr::OpDef::Const(mlr::Const::Bool(boolean));
        self.insert_op(op)
    }

    pub fn insert_unit_op(&mut self) -> H2MResult<mlr::Op> {
        let op = mlr::OpDef::Const(mlr::Const::Unit);
        self.insert_op(op)
    }

    pub fn insert_copy_op(&mut self, place: mlr::Place) -> H2MResult<mlr::Op> {
        let op = mlr::OpDef::Copy(place);
        self.insert_op(op)
    }

    /// TODO move resolution functionality to an impl block in another submodule,
    /// or create resolver submodule.
    pub fn resolve_name(&mut self, name: &str) -> H2MResult<mlr::Op> {
        if let Some(loc) = self.resolve_name_to_location(name) {
            let place = self.insert_loc_place(loc)?;
            self.insert_copy_op(place)
        } else if let Some(fn_) = self.ctxt.fns.get_fn_by_name(name) {
            self.insert_fn_op(fn_)
        } else {
            Err(H2MErr::UnresolvableSymbol { name: name.to_string() })
        }
    }

    pub fn resolve_name_to_location(&self, name: &str) -> Option<mlr::Loc> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|scope| scope.vars.get(name))
            .next()
            .cloned()
    }

    pub fn try_resolve_enum_variant(&self, variant_name: &str) -> Option<(ty::Ty, usize)> {
        for (ty, enum_def) in self.ctxt.tys.get_all_enums() {
            for (idx, variant) in enum_def.variants.iter().enumerate() {
                if variant.name == variant_name {
                    return Some((*ty, idx));
                }
            }
        }
        None
    }

    pub fn resolve_hlr_ty_annot(&mut self, annot: &hlr::TyAnnot) -> H2MResult<ty::Ty> {
        let gen_params = self.get_signature().gen_params.clone();
        let ty = self
            .ctxt
            .tys
            .get_ty_by_hlr_annot(annot, &gen_params)
            .ok_or(H2MErr::UnresolvableTyAnnot)?;
        Ok(ty)
    }
}
