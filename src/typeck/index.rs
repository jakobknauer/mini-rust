use crate::ctxt::traits::TraitInst;
use crate::ctxt::ty;
use crate::hlr;

use super::{ExprExtra, TypeckError, TypeckResult};

impl<'a, 'ctxt: 'a> super::Typeck<'a, 'ctxt> {
    pub(super) fn check_index(
        &mut self,
        expr_id: hlr::ExprId,
        obj: hlr::Expr<'ctxt>,
        index: hlr::Expr<'ctxt>,
    ) -> TypeckResult<'ctxt, ty::Ty<'ctxt>> {
        let obj_ty = self.check_expr(obj, None, false)?;
        let obj_ty = self.normalize(obj_ty);
        let index_ty = self.check_expr(index, None, false)?;
        let index_ty = self.normalize(index_ty);

        let index_trait = self
            .ctxt
            .language_items
            .index_trait
            .ok_or(TypeckError::IndexTraitNotImplemented { obj_ty, index_ty })?;

        let gen_args = self.ctxt.tys.ty_slice(&[index_ty]);
        let trait_inst = TraitInst::new(index_trait, gen_args).unwrap();

        let mut steps = vec![];
        let mut current = obj_ty;
        loop {
            if self
                .ctxt
                .ty_implements_trait_inst(&self.constraints, current, trait_inst)
            {
                let mthd = self.ctxt.traits.resolve_trait_method(index_trait, "index").unwrap();
                let found = super::mthd::FoundMthd::Trait { trait_inst, mthd };
                let resolution = self.instantiate_mthd(found, current, "index", None)?;
                self.typing
                    .expr_extra
                    .insert(expr_id, ExprExtra::Index { resolution, steps });
                let output_ty = self.ctxt.tys.assoc_ty(current, trait_inst, 0);
                return Ok(output_ty);
            }

            match self.try_deref_step(current) {
                Some((next_ty, step)) => {
                    steps.push(step);
                    current = next_ty;
                }
                None => return Err(TypeckError::IndexTraitNotImplemented { obj_ty, index_ty }),
            }
        }
    }
}
