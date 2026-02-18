use crate::{
    ctxt::ty,
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
            hlr::TyAnnotDef::Struct(struct_, ty_annot_defs) => todo!(),
            hlr::TyAnnotDef::Enum(_, ty_annot_defs) => todo!(),
            hlr::TyAnnotDef::Ty(ty) => todo!(),
            hlr::TyAnnotDef::GenVar(gen_var) => todo!(),
            hlr::TyAnnotDef::AssocTy { base, trait_, name } => todo!(),
            hlr::TyAnnotDef::Ref(ty_annot_def) => todo!(),
            hlr::TyAnnotDef::Ptr(ty_annot_def) => todo!(),
            hlr::TyAnnotDef::Fn { params, ret } => todo!(),
            hlr::TyAnnotDef::Tuple(ty_annot_defs) => todo!(),
            hlr::TyAnnotDef::Infer => todo!(),
            hlr::TyAnnotDef::Self_ => todo!(),
        }
    }
}
