use crate::{
    ast,
    ast_lowering::{AstLoweringError, AstLoweringResult},
    ctxt, hlr,
};

impl<'a, 'ctxt> super::AstLowerer<'a, 'ctxt> {
    pub(super) fn resolve_path_to_constructor(&mut self, ty_path: &ast::Path) -> AstLoweringResult<hlr::Val<'ctxt>> {
        match ty_path.segments.as_slice() {
            [segment] => self.resolve_path_segment_to_struct(segment),
            [enum_seg, variant_seg] => self.resolve_path_segments_to_variant(enum_seg, variant_seg),
            _ => Err(AstLoweringError {
                msg: format!("Complex paths in struct literals are not supported yet: {:#?}", ty_path),
            }),
        }
    }

    fn resolve_path_segment_to_struct(&mut self, segment: &ast::PathSegment) -> AstLoweringResult<hlr::Val<'ctxt>> {
        if segment.is_self {
            return Err(AstLoweringError {
                msg: "Invalid use of 'Self' as struct literal constructor".to_string(),
            });
        }

        let struct_ = self
            .ctxt
            .tys
            .get_struct_by_name(&segment.ident)
            .ok_or_else(|| AstLoweringError {
                msg: format!("Unknown struct name in struct literal: {}", &segment.ident),
            })?;

        let args = segment.args.map(|args| self.lower_ty_annots(args)).transpose()?;

        Ok(hlr::Val::Struct(struct_, args))
    }

    pub(super) fn resolve_path_segments_to_variant(
        &mut self,
        enum_seg: &ast::PathSegment,
        variant_seg: &ast::PathSegment,
    ) -> AstLoweringResult<hlr::Val<'ctxt>> {
        if enum_seg.is_self || variant_seg.is_self {
            return Err(AstLoweringError {
                msg: "Invalid use of 'Self' in enum variant path".to_string(),
            });
        }

        if variant_seg.args.is_some() {
            return Err(AstLoweringError {
                msg: "Generic arguments on enum variants are not supported yet".to_string(),
            });
        }

        let enum_ = self
            .ctxt
            .tys
            .get_enum_by_name(&enum_seg.ident)
            .ok_or_else(|| AstLoweringError {
                msg: format!("Unknown enum name in enum variant path: {}", &enum_seg.ident),
            })?;

        let args = enum_seg.args.map(|args| self.lower_ty_annots(args)).transpose()?;

        let (variant_index, _) = enum_
            .resolve_variant_by_name(&variant_seg.ident)
            .ok_or_else(|| AstLoweringError {
                msg: format!(
                    "Unknown variant '{}' for enum '{}' in enum variant path",
                    variant_seg.ident, enum_.name
                ),
            })?;

        Ok(hlr::Val::Variant(enum_, variant_index, args))
    }

    pub(super) fn resolve_ident_to_ty(&self, ident: &str) -> AstLoweringResult<TyResolution<'ctxt>> {
        // Try to resolve to generic var
        for &gen_var in &self.fn_.gen_params {
            if gen_var.name() == ident {
                // Resolve to generic var
                return Ok(TyResolution::GenVar(gen_var));
            }
        }
        // Try to resolve to env generic var
        for &gen_var in &self.fn_.env_gen_params {
            if gen_var.name() == ident {
                // Resolve to generic var
                return Ok(TyResolution::GenVar(gen_var));
            }
        }

        // Resolve to named type (struct/enum/primitive)
        let named_ty = self.ctxt.tys.get_ty_by_name(ident).map_err(|_| AstLoweringError {
            msg: format!("Unknown type name in type annotation: {}", ident),
        })?;

        Ok(TyResolution::NamedTy(named_ty))
    }

    pub(super) fn resolve_ident_to_val_def(&mut self, name: &str) -> Option<hlr::Val<'ctxt>> {
        let fn_ = self.ctxt.fns.get_fn_by_name(name);
        self.resolve_ident_to_var(name)
            .map(hlr::Val::Var)
            .or_else(|| fn_.map(|fn_| hlr::Val::Fn(fn_, None)))
    }

    pub(super) fn resolve_ident_to_var(&mut self, name: &str) -> Option<hlr::VarId> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|scope| scope.bindings.get(name))
            .next()
            .cloned()
    }
}

pub(super) enum TyResolution<'ty> {
    NamedTy(ctxt::Named<'ty>),
    GenVar(ctxt::ty::GenVar<'ty>),
}
