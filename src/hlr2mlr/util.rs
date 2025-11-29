use std::collections::HashSet;

use crate::{
    ctxt::{fns, mlr, ty},
    hlr,
    hlr2mlr::{Hlr2MlrErr, Result, TyErr},
    typechecker::Typechecker,
};

impl<'a> super::Hlr2Mlr<'a> {
    pub fn insert_val(&mut self, val_def: mlr::ValDef) -> Result<mlr::Val> {
        let val = self.ctxt.mlr.insert_val(val_def);
        Typechecker::new(self.ctxt).infer_val_ty(val)?;
        Ok(val)
    }

    pub fn insert_call_val(&mut self, callable: mlr::Op, args: Vec<mlr::Op>) -> Result<mlr::Val> {
        let val = mlr::ValDef::Call { callable, args };
        self.insert_val(val)
    }

    pub fn insert_empty_val(&mut self, ty: ty::Ty) -> Result<mlr::Val> {
        let val = mlr::ValDef::Empty { ty };
        self.insert_val(val)
    }

    pub fn insert_use_val(&mut self, op: mlr::Op) -> Result<mlr::Val> {
        let val = mlr::ValDef::Use(op);
        self.insert_val(val)
    }

    pub fn insert_use_place_val(&mut self, place: mlr::Place) -> Result<mlr::Val> {
        let op = self.insert_copy_op(place)?;
        self.insert_use_val(op)
    }

    pub fn insert_addr_of_val(&mut self, place: mlr::Place) -> Result<mlr::Val> {
        let val = mlr::ValDef::AddrOf(place);
        self.insert_val(val)
    }

    pub fn insert_stmt(&mut self, stmt_def: mlr::StmtDef) -> Result<mlr::Stmt> {
        let stmt = self.ctxt.mlr.insert_stmt(stmt_def);
        self.blocks
            .back_mut()
            .expect("self.blocks should not be empty")
            .push(stmt);
        Ok(stmt)
    }

    pub fn insert_if_stmt(&mut self, cond: mlr::Op, then_: mlr::Stmt, else_: mlr::Stmt) -> Result<mlr::Stmt> {
        let if_ = mlr::StmtDef::If(mlr::If {
            cond,
            then: then_,
            else_,
        });
        self.insert_stmt(if_)
    }

    pub fn insert_loop_stmt(&mut self, body: mlr::Stmt) -> Result<mlr::Stmt> {
        let val = mlr::StmtDef::Loop { body };
        self.insert_stmt(val)
    }

    pub fn insert_alloc_stmt(&mut self, loc: mlr::Loc) -> Result<mlr::Stmt> {
        let stmt = mlr::StmtDef::Alloc { loc };
        self.insert_stmt(stmt)
    }

    pub fn insert_fresh_alloc(&mut self) -> Result<mlr::Place> {
        let undef = self.ctxt.tys.get_undef_ty();
        self.insert_alloc_with_ty(undef)
    }

    pub fn insert_alloc_with_ty(&mut self, ty: ty::Ty) -> Result<mlr::Place> {
        let loc = self.ctxt.mlr.insert_typed_loc(ty);
        self.insert_alloc_stmt(loc)?;
        self.insert_loc_place(loc)
    }

    pub fn insert_break_stmt(&mut self) -> Result<mlr::Stmt> {
        let stmt = mlr::StmtDef::Break;
        self.insert_stmt(stmt)
    }

    pub fn insert_assign_stmt(&mut self, place: mlr::Place, value: mlr::Val) -> Result<mlr::Stmt> {
        let place_ty = self.ctxt.mlr.get_place_ty(&place);
        let value_ty = self.ctxt.mlr.get_val_ty(&value);

        self.ctxt
            .tys
            .unify(&place_ty, &value_ty)
            .map_err(|_| TyErr::AssignStmtTyMismatch {
                place,
                expected: place_ty,
                actual: value_ty,
            })
            .map_err(Hlr2MlrErr::TyErr)?;

        let stmt = mlr::StmtDef::Assign { place, value };
        self.insert_stmt(stmt)
    }

    pub fn insert_assign_to_loc_stmt(&mut self, loc: mlr::Loc, value: mlr::Val) -> Result<mlr::Stmt> {
        let place = mlr::PlaceDef::Loc(loc);
        let place = self.insert_place(place)?;
        self.insert_assign_stmt(place, value)
    }

    pub fn insert_return_stmt(&mut self, value: mlr::Val) -> Result<mlr::Stmt> {
        let return_ty = self
            .ctxt
            .fns
            .get_sig(&self.target_fn)
            .expect("return stmt only valid in function")
            .return_ty;

        let val_ty = self.ctxt.mlr.get_val_ty(&value);

        self.ctxt
            .tys
            .unify(&return_ty, &val_ty)
            .map_err(|_| TyErr::ReturnTyMismatch {
                expected: return_ty,
                actual: val_ty,
            })
            .map_err(Hlr2MlrErr::TyErr)?;

        let stmt = mlr::StmtDef::Return { value };
        self.insert_stmt(stmt)
    }

    pub fn insert_place(&mut self, place_def: mlr::PlaceDef) -> Result<mlr::Place> {
        let place = self.ctxt.mlr.insert_place(place_def);
        Typechecker::new(self.ctxt).infer_place_ty(place)?;
        Ok(place)
    }

    pub fn insert_loc_place(&mut self, loc: mlr::Loc) -> Result<mlr::Place> {
        let place = mlr::PlaceDef::Loc(loc);
        self.insert_place(place)
    }

    pub fn insert_enum_discriminant_place(&mut self, base: mlr::Place) -> Result<mlr::Place> {
        let place = mlr::PlaceDef::EnumDiscriminant { base };
        self.insert_place(place)
    }

    pub fn insert_project_to_variant_place(&mut self, base: mlr::Place, variant_index: usize) -> Result<mlr::Place> {
        let place = mlr::PlaceDef::ProjectToVariant { base, variant_index };
        self.insert_place(place)
    }

    pub fn insert_field_access_place(&mut self, base: mlr::Place, field_index: usize) -> Result<mlr::Place> {
        let place = mlr::PlaceDef::FieldAccess { base, field_index };
        self.insert_place(place)
    }

    pub fn insert_deref_place(&mut self, op: mlr::Op) -> Result<mlr::Place> {
        let place = mlr::PlaceDef::Deref(op);
        self.insert_place(place)
    }

    pub fn insert_op(&mut self, op_def: mlr::OpDef) -> Result<mlr::Op> {
        let op = self.ctxt.mlr.insert_op(op_def);
        Typechecker::new(self.ctxt).infer_op_ty(op)?;
        Ok(op)
    }

    pub fn insert_fn_op(&mut self, fn_: fns::Fn) -> Result<mlr::Op> {
        self.insert_gen_fn_op(fn_, Vec::new())
    }

    pub fn insert_gen_fn_op(&mut self, fn_: fns::Fn, gen_args: Vec<ty::Ty>) -> Result<mlr::Op> {
        self.ctxt.fns.specialize_fn(&self.target_fn, &fn_, gen_args.clone());
        let op = mlr::OpDef::Fn(fns::FnSpecialization { fn_, gen_args });
        self.insert_op(op)
    }

    pub fn insert_int_op(&mut self, int: i64) -> Result<mlr::Op> {
        let val = mlr::OpDef::Const(mlr::Const::Int(int));
        self.insert_op(val)
    }

    pub fn insert_bool_op(&mut self, boolean: bool) -> Result<mlr::Op> {
        let op = mlr::OpDef::Const(mlr::Const::Bool(boolean));
        self.insert_op(op)
    }

    pub fn insert_unit_op(&mut self) -> Result<mlr::Op> {
        let op = mlr::OpDef::Const(mlr::Const::Unit);
        self.insert_op(op)
    }

    pub fn insert_copy_op(&mut self, place: mlr::Place) -> Result<mlr::Op> {
        let op = mlr::OpDef::Copy(place);
        self.insert_op(op)
    }

    /// TODO move resolution functionality to an impl block in another submodule,
    /// or create resolver submodule.
    pub fn resolve_name(&mut self, name: &str) -> Result<mlr::Op> {
        if let Some(loc) = self.resolve_name_to_location(name) {
            let place = self.insert_loc_place(loc)?;
            self.insert_copy_op(place)
        } else if let Some(fn_) = self.ctxt.fns.get_fn_by_name(name) {
            self.insert_fn_op(fn_)
        } else {
            Err(Hlr2MlrErr::UnresolvableSymbol { name: name.to_string() })
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
        for (enum_, enum_def) in self.ctxt.tys.get_all_enums() {
            for (idx, variant) in enum_def.variants.iter().enumerate() {
                if variant.name == variant_name {
                    let ty = self.ctxt.tys.get_ty_of_enum(enum_).expect("enum type should be known");
                    return Some((ty, idx));
                }
            }
        }
        None
    }

    pub fn build_struct_field_init_stmts(
        &mut self,
        ty: &ty::Ty,
        fields: &[(String, hlr::Expr)],
        base_place: &mlr::Place,
    ) -> Result<()> {
        let field_indices = self.compute_field_indices(ty, fields.iter().map(|(name, _)| name.as_str()))?;

        fields
            .iter()
            .zip(field_indices)
            .try_for_each(|((_, expr), field_index)| {
                let field_place = self.insert_field_access_place(*base_place, field_index)?;
                let field_value = self.lower_to_val(expr)?;
                self.insert_assign_stmt(field_place, field_value)?;
                Ok(())
            })
    }

    pub fn compute_field_indices<'b>(
        &self,
        ty: &ty::Ty,
        field_names: impl IntoIterator<Item = &'b str>,
    ) -> Result<Vec<usize>> {
        let field_names: Vec<&str> = field_names.into_iter().collect();

        let struct_def = self.ctxt.tys.get_struct_def_by_ty(ty).map_err(Hlr2MlrErr::TyErr)?;

        let expected: HashSet<&str> = struct_def.fields.iter().map(|field| field.name.as_str()).collect();
        let actual: HashSet<&str> = field_names.iter().cloned().collect();

        let missing_fields: Vec<&str> = expected.difference(&actual).cloned().collect();
        if !missing_fields.is_empty() {
            return TyErr::InitializerMissingFields {
                ty: *ty,
                missing_fields: missing_fields.iter().map(|s| s.to_string()).collect(),
            }
            .into();
        }

        let extra_fields: Vec<&str> = actual.difference(&expected).cloned().collect();
        if !extra_fields.is_empty() {
            return TyErr::InitializerExtraFields {
                ty: *ty,
                extra_fields: extra_fields.iter().map(|s| s.to_string()).collect(),
            }
            .into();
        }

        let field_indices = field_names
            .iter()
            .map(|field_name| {
                struct_def
                    .fields
                    .iter()
                    .position(|struct_field| &struct_field.name == field_name)
                    .ok_or(Hlr2MlrErr::TyErr(TyErr::NotAStructField {
                        ty: *ty,
                        field_name: field_name.to_string(),
                    }))
            })
            .collect::<Result<_>>()?;

        Ok(field_indices)
    }

    pub fn get_signature(&self) -> &fns::FnSig {
        self.ctxt
            .fns
            .get_sig(&self.target_fn)
            .expect("function signature should be registered")
    }

    pub fn resolve_hlr_ty_annot(&mut self, annot: &hlr::TyAnnot) -> Result<ty::Ty> {
        let gen_params = self.get_signature().gen_params.clone();
        let ty = self
            .ctxt
            .tys
            .get_ty_by_hlr_annot(annot, &gen_params)
            .ok_or(Hlr2MlrErr::TyErr(TyErr::UnresolvableTyAnnot))?;
        Ok(ty)
    }
}
