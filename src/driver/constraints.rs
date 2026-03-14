use crate::{
    ast,
    ctxt::{traits, ty},
};

use super::{Driver, DriverError, ResCtxt};

impl Driver<'_, '_, '_, '_, '_> {
    pub(super) fn resolve_constraint(
        &mut self,
        constraint: &ast::Constraint,
        res_ctxt: ResCtxt,
    ) -> Result<Vec<ty::Constraint>, DriverError> {
        let subject = self
            .try_resolve_ast_ty_annot(constraint.subject, res_ctxt, false)
            .ok_or(DriverError::ContextBuild("Failed to resolve constraint subject type"))?;

        self.resolve_constraint_requirement(subject, &constraint.requirement, res_ctxt)
    }

    fn resolve_constraint_requirement(
        &mut self,
        subject: ty::Ty,
        requirement: &ast::ConstraintRequirement,
        res_ctxt: ResCtxt,
    ) -> Result<Vec<ty::Constraint>, DriverError> {
        match requirement {
            ast::ConstraintRequirement::Trait {
                trait_name,
                trait_args,
                assoc_bindings,
            } => {
                let trait_ = self
                    .ctxt
                    .traits
                    .resolve_trait_name(trait_name)
                    .ok_or(DriverError::ContextBuild("Unknown trait in constraint"))?;

                let trait_args: Vec<_> = trait_args
                    .iter()
                    .map(|&arg| {
                        self.try_resolve_ast_ty_annot(arg, res_ctxt, false)
                            .ok_or(DriverError::ContextBuild("Failed to resolve constraint trait argument"))
                    })
                    .collect::<Result<_, _>>()?;

                let gen_args = self.ctxt.tys.ty_slice(&trait_args);
                let trait_inst = self.ctxt.traits.inst_trait(trait_, gen_args).unwrap();

                let mut result = vec![ty::Constraint {
                    subject,
                    requirement: ty::ConstraintRequirement::Trait(trait_inst),
                }];

                result.extend(self.resolve_assoc_bindings(subject, trait_inst, assoc_bindings, res_ctxt)?);

                Ok(result)
            }
            ast::ConstraintRequirement::Callable { params, return_ty } => {
                let param_tys: Vec<_> = params
                    .iter()
                    .map(|&ty| {
                        self.try_resolve_ast_ty_annot(ty, res_ctxt, false)
                            .ok_or(DriverError::ContextBuild(
                                "Failed to resolve constraint callable parameter type",
                            ))
                    })
                    .collect::<Result<_, _>>()?;
                let param_tys = self.ctxt.tys.ty_slice(&param_tys);
                let return_ty =
                    match return_ty {
                        Some(return_ty) => self.try_resolve_ast_ty_annot(return_ty, res_ctxt, false).ok_or(
                            DriverError::ContextBuild("Failed to resolve constraint callable return type"),
                        )?,
                        None => self.ctxt.tys.unit(),
                    };

                Ok(vec![ty::Constraint {
                    subject,
                    requirement: ty::ConstraintRequirement::Callable { param_tys, return_ty },
                }])
            }
        }
    }

    fn resolve_assoc_bindings(
        &mut self,
        subject: ty::Ty,
        trait_inst: traits::TraitInst,
        bindings: &[ast::AssocBinding],
        res_ctxt: ResCtxt,
    ) -> Result<Vec<ty::Constraint>, DriverError> {
        let mut result = Vec::new();

        for binding in bindings {
            let (name, assoc_subject) = match binding {
                ast::AssocBinding::Eq { name, .. } | ast::AssocBinding::Bound { name, .. } => {
                    let assoc_ty_idx = self.ctxt.traits.resolve_assoc_ty_name(trait_inst.trait_, name).ok_or(
                        DriverError::ContextBuild("Unknown associated type in constraint binding"),
                    )?;
                    let assoc_subject = self.ctxt.tys.assoc_ty(subject, trait_inst, assoc_ty_idx);
                    (name, assoc_subject)
                }
            };

            match binding {
                ast::AssocBinding::Eq { ty, .. } => {
                    let eq_ty = self
                        .try_resolve_ast_ty_annot(ty, res_ctxt, false)
                        .ok_or(DriverError::ContextBuild(
                            "Failed to resolve associated type equality binding",
                        ))?;
                    result.push(ty::Constraint {
                        subject: assoc_subject,
                        requirement: ty::ConstraintRequirement::AssocTyEq(eq_ty),
                    });
                }
                ast::AssocBinding::Bound { requirement, .. } => {
                    result.extend(self.resolve_constraint_requirement(assoc_subject, requirement, res_ctxt)?);
                }
            }

            let _ = name;
        }

        Ok(result)
    }
}
