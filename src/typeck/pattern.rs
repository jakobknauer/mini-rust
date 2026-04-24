use crate::ctxt::ty;
use crate::hlr;

use super::{MatchBinding, TypeckError, TypeckResult};

impl<'a, 'ctxt: 'a> super::Typeck<'a, 'ctxt> {
    pub(super) fn check_pattern(
        &mut self,
        pattern: hlr::Pattern<'ctxt>,
        scrutinee_ty: ty::Ty<'ctxt>,
        binding: MatchBinding,
    ) -> TypeckResult<'ctxt, ()> {
        match pattern {
            hlr::PatternKind::Wildcard => Ok(()),
            hlr::PatternKind::Identifier { var_id, .. } => self.check_identifier_pattern(*var_id, scrutinee_ty),
            hlr::PatternKind::Variant(_) | hlr::PatternKind::Struct(_) | hlr::PatternKind::Tuple(_) => {
                let (_, new_binding) = self.peel_ref_scrutinee(scrutinee_ty);
                self.typing.match_bindings.insert(pattern as *const _, new_binding);
                match pattern {
                    hlr::PatternKind::Variant(p) => self.check_variant_pattern(p, scrutinee_ty, new_binding),
                    hlr::PatternKind::Struct(p) => self.check_struct_pattern(p, scrutinee_ty, new_binding),
                    hlr::PatternKind::Tuple(sub_patterns) => {
                        self.check_tuple_pattern(sub_patterns, scrutinee_ty, new_binding)
                    }
                    _ => unreachable!(),
                }
            }
            hlr::PatternKind::Lit(lit) => self.check_lit_pattern(lit, scrutinee_ty),
            hlr::PatternKind::Ref(inner) => self.check_ref_pattern(inner, scrutinee_ty, binding),
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
        binding: MatchBinding,
    ) -> TypeckResult<'ctxt, ()> {
        let (inner_ty, _) = self.peel_ref_scrutinee(scrutinee_ty);
        let enum_ty = match inner_ty.0 {
            ty::TyDef::Enum { .. } => inner_ty,
            _ => return Err(TypeckError::NonMatchableScrutinee { ty: scrutinee_ty }),
        };

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
            let binding_ty = self.apply_binding(binding, field_ty);
            self.check_pattern(field.pattern, binding_ty, binding)?;
        }

        Ok(())
    }

    fn check_struct_pattern(
        &mut self,
        pattern: &hlr::StructPattern<'ctxt>,
        scrutinee_ty: ty::Ty<'ctxt>,
        binding: MatchBinding,
    ) -> TypeckResult<'ctxt, ()> {
        let (inner_ty, _) = self.peel_ref_scrutinee(scrutinee_ty);
        let struct_scrutinee_ty = match inner_ty.0 {
            ty::TyDef::Struct { .. } => inner_ty,
            _ => return Err(TypeckError::NonMatchableScrutinee { ty: scrutinee_ty }),
        };

        let hlr::Val::Struct(struct_, gen_args) = &pattern.constructor else {
            unreachable!("struct pattern must have Val::Struct");
        };

        let n_gen_params = struct_.gen_params.len();
        let gen_args = self.resolve_optional_gen_args(*gen_args, n_gen_params, |actual| {
            TypeckError::StructGenArgCountMismatch {
                struct_,
                expected: n_gen_params,
                actual,
            }
        })?;
        let struct_ty = self.ctxt.tys.inst_struct(struct_, &gen_args).unwrap();

        if !self.unify(struct_ty, struct_scrutinee_ty) {
            return Err(TypeckError::MatchArmTypeMismatch {
                expected: struct_scrutinee_ty,
                actual: struct_ty,
            });
        }

        for field in pattern.fields {
            let field_ty = self.ctxt.tys.get_struct_field_ty(struct_ty, field.field_index).unwrap();
            let binding_ty = self.apply_binding(binding, field_ty);
            self.check_pattern(field.pattern, binding_ty, binding)?;
        }

        Ok(())
    }

    fn check_tuple_pattern(
        &mut self,
        sub_patterns: &[hlr::Pattern<'ctxt>],
        scrutinee_ty: ty::Ty<'ctxt>,
        binding: MatchBinding,
    ) -> TypeckResult<'ctxt, ()> {
        let (inner_ty, _) = self.peel_ref_scrutinee(scrutinee_ty);
        let field_tys = inner_ty
            .tuple_field_tys()
            .map_err(|()| TypeckError::NonMatchableScrutinee { ty: scrutinee_ty })?;

        if sub_patterns.len() != field_tys.len() {
            return Err(TypeckError::TuplePatternLenMismatch {
                expected: field_tys.len(),
                found: sub_patterns.len(),
            });
        }

        for (pattern, &field_ty) in sub_patterns.iter().zip(field_tys) {
            self.check_pattern(pattern, self.apply_binding(binding, field_ty), binding)?;
        }

        Ok(())
    }

    fn check_ref_pattern(
        &mut self,
        inner: hlr::Pattern<'ctxt>,
        scrutinee_ty: ty::Ty<'ctxt>,
        binding: MatchBinding,
    ) -> TypeckResult<'ctxt, ()> {
        // With Direct binding, scrutinee_ty is the real type — must be &T.
        // With ByRef/ByRefMut binding, scrutinee_ty = &field_ty (ergonomics-added &). For &p to
        // be valid here, field_ty must itself be a reference, i.e. scrutinee_ty = &&U. After
        // stripping the outer &, inner gets &U (the real reference), not a plain value.
        let ty::TyDef::Ref(inner_ty) = *scrutinee_ty.0 else {
            return Err(TypeckError::NonMatchableScrutinee { ty: scrutinee_ty });
        };
        let inner_ty = self.normalize(inner_ty);
        if binding != MatchBinding::Direct && !matches!(*inner_ty.0, ty::TyDef::Ref(_)) {
            return Err(TypeckError::NonMatchableScrutinee { ty: scrutinee_ty });
        }
        self.check_pattern(inner, inner_ty, MatchBinding::Direct)
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

    fn peel_ref_scrutinee(&mut self, scrutinee_ty: ty::Ty<'ctxt>) -> (ty::Ty<'ctxt>, MatchBinding) {
        match *scrutinee_ty.0 {
            ty::TyDef::Ref(inner) => (self.normalize(inner), MatchBinding::ByRef),
            ty::TyDef::RefMut(inner) => (self.normalize(inner), MatchBinding::ByRefMut),
            _ => (scrutinee_ty, MatchBinding::Direct),
        }
    }

    fn apply_binding(&self, binding: MatchBinding, ty: ty::Ty<'ctxt>) -> ty::Ty<'ctxt> {
        match binding {
            MatchBinding::Direct => ty,
            MatchBinding::ByRef => self.ctxt.tys.ref_(ty),
            MatchBinding::ByRefMut => self.ctxt.tys.ref_mut(ty),
        }
    }
}
