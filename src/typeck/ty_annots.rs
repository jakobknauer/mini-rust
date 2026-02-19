use crate::{
    ctxt::{traits, ty},
    hlr,
    typeck::{TypeckError, TypeckResult},
};

impl<'ctxt, 'hlr> super::Typeck<'ctxt, 'hlr> {
    pub(super) fn new_inf_var(&mut self) -> ty::Ty {
        self.ctxt.tys.inf_var()
    }

    pub(super) fn resolve_optional_gen_args(
        &mut self,
        ty_annots: Option<hlr::TyAnnotSlice<'hlr>>,
        n_expected: usize,
        f: impl FnOnce(usize) -> TypeckError,
    ) -> TypeckResult<Vec<ty::Ty>> {
        match ty_annots {
            Some(ty_annots) if ty_annots.len() == n_expected => self.resolve_ty_annots(ty_annots),
            Some(ty_annots) => Err(f(ty_annots.len())),
            None => Ok((0..n_expected).map(|_| self.ctxt.tys.inf_var()).collect()),
        }
    }

    pub(super) fn resolve_ty_annots(&mut self, ty_annots: hlr::TyAnnotSlice<'hlr>) -> TypeckResult<Vec<ty::Ty>> {
        ty_annots
            .iter()
            .map(|ty_annot| self.resolve_ty_annot(ty_annot))
            .collect()
    }

    pub(super) fn resolve_ty_annot(&mut self, ty_annot: hlr::TyAnnot<'hlr>) -> TypeckResult<ty::Ty> {
        match ty_annot {
            hlr::TyAnnotDef::Struct(struct_, ty_annot_defs) => self.resolve_struct_ty_annot(*struct_, *ty_annot_defs),
            hlr::TyAnnotDef::Enum(enum_, ty_annot_defs) => self.resolve_enum_ty_annot(*enum_, *ty_annot_defs),
            hlr::TyAnnotDef::Ty(ty) => Ok(*ty),
            hlr::TyAnnotDef::GenVar(gen_var) => self.resolve_gen_var_ty_annot(*gen_var),
            hlr::TyAnnotDef::AssocTy { base, trait_, name } => self.resolve_assoc_ty_annot(base, *trait_, name),
            hlr::TyAnnotDef::Ref(inner) => self.resolve_ref_ty_annot(inner),
            hlr::TyAnnotDef::Ptr(inner) => self.resolve_ptr_ty_annot(inner),
            hlr::TyAnnotDef::Fn { params, ret } => self.resolve_fn_ty_annot(params, *ret),
            hlr::TyAnnotDef::Tuple(elems) => self.resolve_tuple_ty_annot(elems),
            hlr::TyAnnotDef::Infer => self.resolve_infer_ty_annot(),
            hlr::TyAnnotDef::Self_ => self.resolve_self_ty_annot(),
        }
    }

    fn resolve_struct_ty_annot(
        &mut self,
        struct_: ty::Struct,
        ty_annot_defs: Option<hlr::TyAnnotSlice<'hlr>>,
    ) -> TypeckResult<ty::Ty> {
        let n_expected = self.ctxt.tys.get_struct_def(struct_).unwrap().gen_params.len();
        let gen_args = self.resolve_optional_gen_args(ty_annot_defs, n_expected, |actual| {
            TypeckError::StructGenArgCountMismatch {
                struct_,
                expected: n_expected,
                actual,
            }
        })?;
        self.ctxt
            .tys
            .inst_struct(struct_, &gen_args)
            .map_err(|_| TypeckError::StructGenArgCountMismatch {
                struct_,
                expected: n_expected,
                actual: gen_args.len(),
            })
    }

    fn resolve_enum_ty_annot(
        &mut self,
        enum_: ty::Enum,
        ty_annot_defs: Option<hlr::TyAnnotSlice<'hlr>>,
    ) -> TypeckResult<ty::Ty> {
        let n_expected = self.ctxt.tys.get_enum_def(enum_).unwrap().gen_params.len();
        let gen_args = self.resolve_optional_gen_args(ty_annot_defs, n_expected, |actual| {
            TypeckError::EnumGenArgCountMismatch {
                enum_,
                expected: n_expected,
                actual,
            }
        })?;
        self.ctxt
            .tys
            .inst_enum(enum_, &gen_args)
            .map_err(|_| TypeckError::EnumGenArgCountMismatch {
                enum_,
                expected: n_expected,
                actual: gen_args.len(),
            })
    }

    fn resolve_gen_var_ty_annot(&mut self, gen_var: ty::GenVar) -> TypeckResult<ty::Ty> {
        Ok(self.ctxt.tys.gen_var(gen_var))
    }

    fn resolve_assoc_ty_annot(
        &mut self,
        base: hlr::TyAnnot<'hlr>,
        trait_: Option<hlr::TyAnnot<'hlr>>,
        name: &str,
    ) -> TypeckResult<ty::Ty> {
        let base_ty = self.resolve_ty_annot(base)?;
        match trait_ {
            Some(_) => todo!("AssocTy with explicit trait disambiguation"),
            None => {
                let (trait_, assoc_ty_idx) =
                    self.ctxt
                        .traits
                        .get_trait_assoc_ty_with_name(name)
                        .next()
                        .ok_or_else(|| TypeckError::UnresolvableAssocTy {
                            base: base_ty,
                            name: name.to_string(),
                        })?;
                let trait_inst = traits::TraitInst {
                    trait_,
                    gen_args: self.ctxt.tys.ty_slice(&[]),
                };
                Ok(self.ctxt.tys.assoc_ty(base_ty, trait_inst, assoc_ty_idx))
            }
        }
    }

    fn resolve_ref_ty_annot(&mut self, inner: hlr::TyAnnot<'hlr>) -> TypeckResult<ty::Ty> {
        let inner_ty = self.resolve_ty_annot(inner)?;
        Ok(self.ctxt.tys.ref_(inner_ty))
    }

    fn resolve_ptr_ty_annot(&mut self, inner: hlr::TyAnnot<'hlr>) -> TypeckResult<ty::Ty> {
        let inner_ty = self.resolve_ty_annot(inner)?;
        Ok(self.ctxt.tys.ptr(inner_ty))
    }

    fn resolve_fn_ty_annot(
        &mut self,
        params: hlr::TyAnnotSlice<'hlr>,
        ret: Option<hlr::TyAnnot<'hlr>>,
    ) -> TypeckResult<ty::Ty> {
        let param_tys = self.resolve_ty_annots(params)?;
        let ret_ty = match ret {
            Some(ret) => self.resolve_ty_annot(ret)?,
            None => self.ctxt.tys.unit(),
        };
        Ok(self.ctxt.tys.fn_(&param_tys, ret_ty, false))
    }

    fn resolve_tuple_ty_annot(&mut self, elems: hlr::TyAnnotSlice<'hlr>) -> TypeckResult<ty::Ty> {
        let elem_tys = self.resolve_ty_annots(elems)?;
        Ok(self.ctxt.tys.tuple(&elem_tys))
    }

    fn resolve_infer_ty_annot(&mut self) -> TypeckResult<ty::Ty> {
        Ok(self.ctxt.tys.inf_var())
    }

    fn resolve_self_ty_annot(&mut self) -> TypeckResult<ty::Ty> {
        let self_ty = self.ctxt.fns.get_sig(self.fn_.fn_).unwrap().associated_ty;
        Ok(self_ty.expect("Self type not available in this context"))
    }
}
