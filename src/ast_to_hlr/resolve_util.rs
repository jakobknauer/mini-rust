use crate::{
    ast,
    ast_to_hlr::{AstToHlr, AstToHlrError, AstToHlrResult},
    ctxt, hlr,
};

impl<'a> AstToHlr<'a> {
    pub(super) fn resolve_path_to_constructor(
        &mut self,
        ty_path: &ast::Path,
    ) -> AstToHlrResult<(hlr::Def, Option<Vec<hlr::TyAnnot>>)> {
        match ty_path.segments.as_slice() {
            [simple] => {
                let (struct_, gen_args) = match simple {
                    ast::PathSegment::Ident(ident) => {
                        let struct_ = self.ctxt.tys.get_struct_by_name(ident).ok_or_else(|| AstToHlrError {
                            msg: format!("Unknown struct name in struct literal: {}", ident),
                        })?;
                        (struct_, None)
                    }
                    ast::PathSegment::Generic(gen_segment) => {
                        let struct_ =
                            self.ctxt
                                .tys
                                .get_struct_by_name(&gen_segment.ident)
                                .ok_or_else(|| AstToHlrError {
                                    msg: format!("Unknown struct name in struct literal: {}", gen_segment.ident),
                                })?;
                        let gen_args = gen_segment
                            .gen_args
                            .iter()
                            .map(|&arg| self.lower_ty_annot(arg))
                            .collect::<AstToHlrResult<_>>()?;
                        (struct_, Some(gen_args))
                    }
                    ast::PathSegment::Self_ => {
                        return Err(AstToHlrError {
                            msg: "Invalid use of 'Self' as struct literal constructor".to_string(),
                        });
                    }
                };
                Ok((hlr::Def::Struct(struct_), gen_args))
            }
            [enum_, variant_name] => {
                let (enum_, gen_args) = match enum_ {
                    ast::PathSegment::Ident(ident) => {
                        let enum_ = self.ctxt.tys.get_enum_by_name(ident).ok_or_else(|| AstToHlrError {
                            msg: format!("Unknown enum name in struct literal: {}", ident),
                        })?;
                        (enum_, None)
                    }
                    ast::PathSegment::Generic(gen_segment) => {
                        let enum_ =
                            self.ctxt
                                .tys
                                .get_enum_by_name(&gen_segment.ident)
                                .ok_or_else(|| AstToHlrError {
                                    msg: format!("Unknown enum name in struct literal: {}", gen_segment.ident),
                                })?;
                        let gen_args = gen_segment
                            .gen_args
                            .iter()
                            .map(|&arg| self.lower_ty_annot(arg))
                            .collect::<AstToHlrResult<_>>()?;
                        (enum_, Some(gen_args))
                    }
                    ast::PathSegment::Self_ => {
                        return Err(AstToHlrError {
                            msg: "Invalid use of 'Self' as enum literal constructor".to_string(),
                        });
                    }
                };

                let variant_name = match variant_name {
                    ast::PathSegment::Ident(name) => name,
                    ast::PathSegment::Generic(_) => {
                        return Err(AstToHlrError {
                            msg: "Generic parameters are not allowed on enum variants in struct literals".to_string(),
                        });
                    }
                    ast::PathSegment::Self_ => {
                        return Err(AstToHlrError {
                            msg: "Invalid use of 'Self' as enum variant name in struct literal".to_string(),
                        });
                    }
                };

                let variant_index = self
                    .ctxt
                    .tys
                    .get_enum_def(enum_)
                    .unwrap()
                    .variants
                    .iter()
                    .position(|variant| &variant.name == variant_name)
                    .ok_or_else(|| AstToHlrError {
                        msg: format!(
                            "Unknown variant '{}' for enum '{}' in struct literal",
                            variant_name, enum_.0
                        ),
                    })?;

                Ok((hlr::Def::Variant(enum_, variant_index), gen_args))
            }
            _ => Err(AstToHlrError {
                msg: format!("Complex paths in struct literals are not supported yet: {}", ty_path),
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
}

pub(super) enum TyResolution {
    NamedTy(ctxt::Named),
    GenVar(ctxt::ty::GenVar),
}
