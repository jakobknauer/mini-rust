use crate::{
    ast,
    ast_to_hlr::{AstToHlr, AstToHlrError, AstToHlrResult},
    ctxt::{self, fns},
    hlr,
};

impl<'a> AstToHlr<'a> {
    pub(super) fn resolve_path_to_constructor(
        &mut self,
        ty_path: &ast::Path,
    ) -> AstToHlrResult<(hlr::Def, Vec<hlr::TyAnnot>)> {
        match ty_path.segments.as_slice() {
            [segment] => {
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

                Ok((hlr::Def::Struct(struct_), args))
            }

            [enum_seg, variant_name] => {
                if enum_seg.is_self {
                    return Err(AstToHlrError {
                        msg: "Invalid use of 'Self' as enum literal constructor".to_string(),
                    });
                }

                let enum_ = self
                    .ctxt
                    .tys
                    .get_enum_by_name(&enum_seg.ident)
                    .ok_or_else(|| AstToHlrError {
                        msg: format!("Unknown enum name in struct literal: {}", &enum_seg.ident),
                    })?;

                let args: Vec<_> = match enum_seg.args {
                    None => {
                        let n_gen_params = self.ctxt.tys.get_enum_def(enum_).unwrap().gen_params.len();
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

                if variant_name.is_self || variant_name.args.is_some() {
                    return Err(AstToHlrError {
                        msg: "Invalid variant path".to_string(),
                    });
                }

                let variant_index = self
                    .ctxt
                    .tys
                    .get_enum_def(enum_)
                    .unwrap()
                    .variants
                    .iter()
                    .position(|variant| variant.name == variant_name.ident)
                    .ok_or_else(|| AstToHlrError {
                        msg: format!(
                            "Unknown variant '{}' for enum '{}' in struct literal",
                            variant_name.ident, enum_.0
                        ),
                    })?;

                Ok((hlr::Def::Variant(enum_, variant_index), args))
            }

            _ => Err(AstToHlrError {
                msg: format!("Complex paths in struct literals are not supported yet: {:#?}", ty_path),
            }),
        }
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

    pub fn resolve_ident_to_val_def(&mut self, name: &str) -> Option<hlr::Def> {
        // First try to resolve to a local variable
        if let Some(var_id) = self.resolve_ident_to_var(name) {
            return Some(hlr::Def::Var(var_id));
        }

        // Then try to resolve to a function
        if let Some(fn_) = self.resolve_ident_to_fn(name) {
            return Some(hlr::Def::Fn(fn_));
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
