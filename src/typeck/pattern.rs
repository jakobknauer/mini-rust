use crate::ctxt::ty;
use crate::hlr;

use super::{MatchBinding, TypeckError, TypeckResult};

impl<'a, 'ctxt: 'a> super::Typeck<'a, 'ctxt> {
    pub(super) fn check_pattern(
        &mut self,
        pattern: hlr::Pattern<'ctxt>,
        scrutinee_ty: ty::Ty<'ctxt>,
    ) -> TypeckResult<'ctxt, ()> {
        match pattern {
            hlr::PatternKind::Wildcard => Ok(()),
            hlr::PatternKind::Identifier { var_id, .. } => self.check_identifier_pattern(*var_id, scrutinee_ty),
            hlr::PatternKind::Variant(pattern) => self.check_variant_pattern(pattern, scrutinee_ty),
            hlr::PatternKind::Tuple(sub_patterns) => self.check_tuple_pattern(sub_patterns, scrutinee_ty),
            hlr::PatternKind::Lit(lit) => self.check_lit_pattern(lit, scrutinee_ty),
        }
    }

    fn check_identifier_pattern(&mut self, var_id: hlr::VarId, scrutinee_ty: ty::Ty<'ctxt>) -> TypeckResult<'ctxt, ()> {
        self.typing.var_types.insert(var_id, scrutinee_ty);
        Ok(())
    }

    fn check_variant_pattern(
        &mut self,
        pattern: &hlr::VariantPattern<'ctxt>,
        scrutinee_ty: ty::Ty<'ctxt>,
    ) -> TypeckResult<'ctxt, ()> {
        let (enum_ty, binding) = self.resolve_enum_scrutinee(scrutinee_ty)?;

        let hlr::Val::Variant(arm_enum, variant_idx, gen_args) = &pattern.variant else {
            unreachable!("match arm pattern must be Val::Variant");
        };

        let n_gen_params = arm_enum.gen_params.len();
        let arm_gen_args =
            self.resolve_optional_gen_args(*gen_args, n_gen_params, |actual| TypeckError::EnumGenArgCountMismatch {
                enum_: arm_enum,
                expected: n_gen_params,
                actual,
            })?;
        let arm_enum_ty = self.ctxt.tys.inst_enum(arm_enum, &arm_gen_args).unwrap();

        if !self.unify(arm_enum_ty, enum_ty) {
            return Err(TypeckError::MatchArmWrongEnum {
                expected: enum_ty,
                found: arm_enum_ty,
            });
        }

        let variant_ty = self.ctxt.tys.get_enum_variant_ty(enum_ty, *variant_idx).unwrap();

        for field in pattern.fields {
            let field_ty = self
                .ctxt
                .tys
                .get_struct_field_ty(variant_ty, field.field_index)
                .unwrap();
            let binding_ty = match binding {
                MatchBinding::Direct => field_ty,
                MatchBinding::ByRef => self.ctxt.tys.ref_(field_ty),
                MatchBinding::ByRefMut => self.ctxt.tys.ref_mut(field_ty),
            };
            self.check_pattern(field.pattern, binding_ty)?;
        }

        Ok(())
    }

    fn check_lit_pattern(&mut self, lit: &hlr::Lit, scrutinee_ty: ty::Ty<'ctxt>) -> TypeckResult<'ctxt, ()> {
        use ty::Primitive::*;
        let lit_ty = self.ctxt.tys.primitive(match lit {
            hlr::Lit::Int(_) => Integer32,
            hlr::Lit::Bool(_) => Boolean,
            hlr::Lit::CChar(_) => CChar,
            hlr::Lit::CString(_) => unreachable!("CString not supported in patterns"),
        });
        if self.unify(lit_ty, scrutinee_ty) {
            Ok(())
        } else {
            Err(TypeckError::NonMatchableScrutinee { ty: scrutinee_ty })
        }
    }

    fn check_tuple_pattern(
        &mut self,
        sub_patterns: &[hlr::Pattern<'ctxt>],
        scrutinee_ty: ty::Ty<'ctxt>,
    ) -> TypeckResult<'ctxt, ()> {
        let field_tys = scrutinee_ty
            .tuple_field_tys()
            .map_err(|()| TypeckError::NonMatchableScrutinee { ty: scrutinee_ty })?;

        if sub_patterns.len() != field_tys.len() {
            return Err(TypeckError::TuplePatternLenMismatch {
                expected: field_tys.len(),
                found: sub_patterns.len(),
            });
        }

        for (pattern, &field_ty) in sub_patterns.iter().zip(field_tys) {
            self.check_pattern(pattern, field_ty)?;
        }

        Ok(())
    }

    fn resolve_enum_scrutinee(
        &mut self,
        scrutinee_ty: ty::Ty<'ctxt>,
    ) -> TypeckResult<'ctxt, (ty::Ty<'ctxt>, MatchBinding)> {
        match scrutinee_ty.0 {
            ty::TyDef::Enum { .. } => Ok((scrutinee_ty, MatchBinding::Direct)),
            &ty::TyDef::Ref(inner) => {
                let inner = self.normalize(inner);
                match inner.0 {
                    ty::TyDef::Enum { .. } => Ok((inner, MatchBinding::ByRef)),
                    _ => Err(TypeckError::NonMatchableScrutinee { ty: scrutinee_ty }),
                }
            }
            &ty::TyDef::RefMut(inner) => {
                let inner = self.normalize(inner);
                match inner.0 {
                    ty::TyDef::Enum { .. } => Ok((inner, MatchBinding::ByRefMut)),
                    _ => Err(TypeckError::NonMatchableScrutinee { ty: scrutinee_ty }),
                }
            }
            _ => Err(TypeckError::NonMatchableScrutinee { ty: scrutinee_ty }),
        }
    }
}
