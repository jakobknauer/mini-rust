use crate::{
    ast,
    ast_to_hlr::{AstToHlr, AstToHlrError, AstToHlrResult},
    ctxt::{self, fns},
    hlr,
};

impl<'a> AstToHlr<'a> {
    pub(super) fn resolve_path_to_constructor(&mut self, ty_path: &ast::Path) -> AstToHlrResult<hlr::Val> {
        match ty_path.segments.as_slice() {
            [segment] => self.resolve_path_segment_to_enum(segment),
            [enum_seg, variant_seg] => self.resolve_path_segments_to_variant(enum_seg, variant_seg),
            _ => Err(AstToHlrError {
                msg: format!("Complex paths in struct literals are not supported yet: {:#?}", ty_path),
            }),
        }
    }

    fn resolve_path_segment_to_enum(&mut self, segment: &ast::PathSegment) -> AstToHlrResult<hlr::Val> {
        if segment.is_self {
            return Err(AstToHlrError {
                msg: "Invalid use of 'Self' as struct literal constructor".to_string(),
            });
        }

        let struct_ = self
            .ctxt
            .tys
            .get_struct_by_name(&segment.ident)
            .ok_or_else(|| AstToHlrError {
                msg: format!("Unknown struct name in struct literal: {}", &segment.ident),
            })?;

        let args: Vec<_> = match segment.args {
            None => {
                let n_gen_params = self.ctxt.tys.get_struct_def(struct_).unwrap().gen_params.len();
                (0..n_gen_params)
                    .map(|_| self.hlr.new_ty_annot(hlr::TyAnnotDef::Infer))
                    .collect()
            }
            Some(args) => self
                .ast
                .ty_annot_slice(args)
                .iter()
                .map(|&annot| self.lower_ty_annot(annot))
                .collect::<AstToHlrResult<_>>()?,
        };

        Ok(hlr::Val::Struct(struct_, Some(args)))
    }

    fn resolve_path_segments_to_variant(
        &mut self,
        enum_seg: &ast::PathSegment,
        variant_seg: &ast::PathSegment,
    ) -> AstToHlrResult<hlr::Val> {
        if enum_seg.is_self || variant_seg.is_self {
            return Err(AstToHlrError {
                msg: "Invalid use of 'Self' in enum variant path".to_string(),
            });
        }

        let enum_ = self
            .ctxt
            .tys
            .get_enum_by_name(&enum_seg.ident)
            .ok_or_else(|| AstToHlrError {
                msg: format!("Unknown enum name in enum variant path: {}", &enum_seg.ident),
            })?;

        let enum_def = self.ctxt.tys.get_enum_def(enum_).unwrap();
        let variant_index = enum_def
            .variants
            .iter()
            .position(|variant| variant.name == variant_seg.ident)
            .ok_or_else(|| AstToHlrError {
                msg: format!(
                    "Unknown variant '{}' for enum '{}' in enum variant path",
                    variant_seg.ident, enum_.0
                ),
            })?;

        Ok(hlr::Val::Variant(enum_, variant_index, None))
    }

    pub(super) fn resolve_ident_to_ty(&mut self, ident: &str) -> AstToHlrResult<TyResolution> {
        let sig = self.get_signature();
        // Try to resolve to generic var
        for &gen_var in &sig.gen_params {
            if self.ctxt.tys.get_gen_var_name(gen_var) == ident {
                // Resolve to generic var
                return Ok(TyResolution::GenVar(gen_var));
            }
        }
        // Try to resolve to env generic var
        for &gen_var in &sig.env_gen_params {
            if self.ctxt.tys.get_gen_var_name(gen_var) == ident {
                // Resolve to generic var
                return Ok(TyResolution::GenVar(gen_var));
            }
        }

        // Resolve to named type (struct/enum/primitive)
        let named_ty = self.ctxt.tys.get_ty_by_name(ident).map_err(|_| AstToHlrError {
            msg: format!("Unknown type name in type annotation: {}", ident),
        })?;

        Ok(TyResolution::NamedTy(*named_ty))
    }

    pub(super) fn resolve_ident_to_val_def(&mut self, name: &str) -> Option<hlr::Val> {
        // First try to resolve to a local variable
        if let Some(var_id) = self.resolve_ident_to_var(name) {
            return Some(hlr::Val::Var(var_id));
        }

        // Then try to resolve to a function
        if let Some(fn_) = self.resolve_ident_to_fn(name) {
            return Some(hlr::Val::Fn(fn_, None));
        }

        None
    }

    pub(super) fn resolve_ident_to_var(&mut self, name: &str) -> Option<hlr::VarId> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|scope| scope.bindings.get(name))
            .next()
            .cloned()
    }

    pub(super) fn resolve_ident_to_fn(&mut self, name: &str) -> Option<fns::Fn> {
        self.ctxt.fns.get_fn_by_name(name)
    }
}

pub(super) enum TyResolution {
    NamedTy(ctxt::Named),
    GenVar(ctxt::ty::GenVar),
}
