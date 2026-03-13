use crate::{
    ast,
    ctxt::{Named, ty},
};

use super::{Driver, ResCtxt};

impl Driver<'_, '_, '_, '_, '_> {
    pub(super) fn try_resolve_ast_ty_annot(
        &mut self,
        annot: ast::TyAnnot,
        res_ctxt: ResCtxt<'_>,
        allow_opaque: bool,
    ) -> Option<ty::Ty> {
        use ast::TyAnnotKind::*;

        match annot {
            Path(path) => match path.segments.as_slice() {
                [segment] => self.try_resolve_ast_path_segment_to_ty(segment, res_ctxt),
                [base_ty, assoc_ty] if !assoc_ty.is_self && assoc_ty.args.is_none() => {
                    let base_ty = self.try_resolve_ast_path_segment_to_ty(base_ty, res_ctxt)?;

                    let ty = self
                        .ctxt
                        .resolve_associated_ty_completely(res_ctxt.constraints, base_ty, &assoc_ty.ident)
                        .expect("associated type not found");
                    Some(ty)
                }
                _ => None,
            },
            &Ref(ty_annot) => self
                .try_resolve_ast_ty_annot(ty_annot, res_ctxt, false)
                .map(|inner| self.ctxt.tys.ref_(inner)),
            &Ptr(ty_annot) => self
                .try_resolve_ast_ty_annot(ty_annot, res_ctxt, false)
                .map(|inner| self.ctxt.tys.ptr(inner)),
            &Fn { param_tys, return_ty } => {
                let param_tys: Vec<ty::Ty> = param_tys
                    .iter()
                    .map(|&pt| self.try_resolve_ast_ty_annot(pt, res_ctxt, false))
                    .collect::<Option<Vec<_>>>()?;

                let return_ty = match return_ty {
                    Some(rt) => self.try_resolve_ast_ty_annot(rt, res_ctxt, false),
                    None => Some(self.ctxt.tys.unit()),
                }?;

                Some(self.ctxt.tys.fn_(&param_tys, return_ty, false))
            }
            &Tuple(ty_annots) => {
                let tys: Vec<ty::Ty> = ty_annots
                    .iter()
                    .map(|ty_annot| self.try_resolve_ast_ty_annot(ty_annot, res_ctxt, false))
                    .collect::<Option<Vec<_>>>()?;
                Some(self.ctxt.tys.tuple(&tys))
            }
            QualifiedPath(qual_path) => {
                let [assoc_seg] = qual_path.path.segments.as_slice() else {
                    return None;
                };
                let base_ty = self.try_resolve_ast_ty_annot(qual_path.ty, res_ctxt, false)?;
                let trait_annot = qual_path.trait_.as_ref()?;
                let trait_ = self.ctxt.traits.resolve_trait_name(&trait_annot.name)?;
                let gen_args: Vec<_> = match trait_annot.args {
                    Some(args) => args
                        .iter()
                        .map(|&a| self.try_resolve_ast_ty_annot(a, res_ctxt, false))
                        .collect::<Option<_>>()?,
                    None => vec![],
                };
                let gen_args = self.ctxt.tys.ty_slice(&gen_args);
                let trait_inst = self.ctxt.traits.inst_trait(trait_, gen_args).ok()?;
                if !self
                    .ctxt
                    .ty_implements_trait_inst(res_ctxt.constraints, base_ty, trait_inst)
                {
                    return None;
                }
                let assoc_ty_idx = self.ctxt.traits.get_trait_assoc_ty_index(trait_, &assoc_seg.ident);
                Some(self.ctxt.tys.assoc_ty(base_ty, trait_inst, assoc_ty_idx))
            }
            ImplTrait(req) => {
                if !allow_opaque {
                    return None;
                }
                let (id, opaque_ty) = self.ctxt.tys.opaque(res_ctxt.gen_vars);
                match req {
                    ast::ConstraintRequirement::Trait {
                        trait_name, trait_args, ..
                    } => {
                        let trait_ = self.ctxt.traits.resolve_trait_name(trait_name)?;
                        let gen_args: Vec<_> = trait_args
                            .iter()
                            .map(|&a| self.try_resolve_ast_ty_annot(a, res_ctxt, false))
                            .collect::<Option<_>>()?;
                        let gen_args = self.ctxt.tys.ty_slice(&gen_args);
                        let trait_inst = self.ctxt.traits.inst_trait(trait_, gen_args).unwrap();
                        self.ctxt
                            .tys
                            .add_opaque_constraint(id, ty::ConstraintRequirement::Trait(trait_inst));
                    }
                    ast::ConstraintRequirement::Callable { params, return_ty } => {
                        let param_tys: Vec<_> = params
                            .iter()
                            .map(|&p| self.try_resolve_ast_ty_annot(p, res_ctxt, false))
                            .collect::<Option<_>>()?;
                        let param_tys = self.ctxt.tys.ty_slice(&param_tys);
                        let return_ty = match return_ty {
                            Some(rt) => self.try_resolve_ast_ty_annot(rt, res_ctxt, false)?,
                            None => self.ctxt.tys.unit(),
                        };
                        self.ctxt
                            .tys
                            .add_opaque_constraint(id, ty::ConstraintRequirement::Callable { param_tys, return_ty });
                    }
                }
                Some(opaque_ty)
            }
            Wildcard => panic!("wildcard type annotation not supported at this position"),
        }
    }

    pub(super) fn try_resolve_ast_path_segment_to_ty(
        &mut self,
        path_segment: &ast::PathSegment,
        res_ctxt: ResCtxt<'_>,
    ) -> Option<ty::Ty> {
        match path_segment {
            ast::PathSegment { is_self: true, .. } => Some(res_ctxt.self_ty.expect("self type not available")),
            ast::PathSegment { ident, args: None, .. } => {
                if let Some(&gv) = res_ctxt
                    .gen_vars
                    .iter()
                    .find(|&&gv| self.ctxt.tys.get_gen_var_name(gv) == *ident)
                {
                    let ty = self.ctxt.tys.gen_var(gv);
                    return Some(ty);
                }

                match *self.ctxt.tys.get_ty_by_name(ident).ok()? {
                    Named::Ty(ty) => Some(ty),
                    Named::Struct(struct_) => self.ctxt.tys.inst_struct(struct_, &[]).ok(),
                    Named::Enum(enum_) => self.ctxt.tys.inst_enum(enum_, &[]).ok(),
                }
            }
            &ast::PathSegment {
                ref ident,
                args: Some(args),
                ..
            } => {
                let gen_args: Vec<ty::Ty> = args
                    .iter()
                    .map(|arg_annot| self.try_resolve_ast_ty_annot(arg_annot, res_ctxt, false))
                    .collect::<Option<Vec<_>>>()?;

                match *self.ctxt.tys.get_ty_by_name(ident).ok()? {
                    Named::Struct(struct_) => self.ctxt.tys.inst_struct(struct_, &gen_args).ok(),
                    Named::Enum(enum_) => self.ctxt.tys.inst_enum(enum_, &gen_args).ok(),
                    Named::Ty(..) => None,
                }
            }
        }
    }
}
