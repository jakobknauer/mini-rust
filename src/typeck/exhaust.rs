use std::collections::{HashMap, HashSet};

use crate::{
    ctxt::{self, ty},
    hlr,
    typeck::MatchBinding,
};

type Matrix = Vec<Vec<ExhPat>>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Ctor {
    Variant(usize),
    Tuple,
    Struct,
    Bool(bool),
    Int(i64),
    Ref,
}

#[derive(Debug, Clone)]
enum ExhPat {
    Wildcard,
    Ctor(Ctor, Vec<ExhPat>),
}

pub(super) fn is_exhaustive<'ctxt>(
    scrutinee_ty: ty::Ty<'ctxt>,
    arms: &[hlr::MatchArm<'ctxt>],
    match_bindings: &HashMap<*const hlr::PatternKind<'ctxt>, MatchBinding>,
    ctxt: &ctxt::Ctxt<'ctxt>,
) -> bool {
    let matrix: Matrix = arms
        .iter()
        .map(|arm| vec![lower_pattern(arm.pattern, match_bindings)])
        .collect();
    !is_useful(&matrix, &[ExhPat::Wildcard], &[scrutinee_ty], ctxt)
}

fn lower_pattern<'ctxt>(
    pattern: hlr::Pattern<'ctxt>,
    bindings: &HashMap<*const hlr::PatternKind<'ctxt>, MatchBinding>,
) -> ExhPat {
    match pattern {
        hlr::PatternKind::Wildcard | hlr::PatternKind::Identifier { .. } => ExhPat::Wildcard,
        hlr::PatternKind::Variant(vp) => {
            let hlr::Val::Variant(enum_, idx, _) = &vp.variant else {
                panic!()
            };
            let arity = enum_.get_variant(*idx).struct_.get_fields().len();
            let fields: Vec<ExhPat> = (0..arity)
                .map(|i| {
                    vp.fields
                        .iter()
                        .find(|f| f.field_index == i)
                        .map_or(ExhPat::Wildcard, |f| lower_pattern(f.pattern, bindings))
                })
                .collect();
            wrap_if_ergonomics(ExhPat::Ctor(Ctor::Variant(*idx), fields), pattern, bindings)
        }
        hlr::PatternKind::Struct(sp) => {
            let hlr::Val::Struct(struct_, _) = &sp.constructor else {
                panic!()
            };
            let arity = struct_.get_fields().len();
            let fields: Vec<ExhPat> = (0..arity)
                .map(|i| {
                    sp.fields
                        .iter()
                        .find(|f| f.field_index == i)
                        .map_or(ExhPat::Wildcard, |f| lower_pattern(f.pattern, bindings))
                })
                .collect();
            wrap_if_ergonomics(ExhPat::Ctor(Ctor::Struct, fields), pattern, bindings)
        }
        hlr::PatternKind::Tuple(ps) => {
            let fields: Vec<ExhPat> = ps.iter().map(|p| lower_pattern(p, bindings)).collect();
            wrap_if_ergonomics(ExhPat::Ctor(Ctor::Tuple, fields), pattern, bindings)
        }
        hlr::PatternKind::Lit(hlr::Lit::Bool(b)) => ExhPat::Ctor(Ctor::Bool(*b), vec![]),
        hlr::PatternKind::Lit(hlr::Lit::Int(n)) => ExhPat::Ctor(Ctor::Int(*n), vec![]),
        hlr::PatternKind::Lit(hlr::Lit::CChar(b)) => ExhPat::Ctor(Ctor::Int(*b as i64), vec![]),
        hlr::PatternKind::Lit(hlr::Lit::CString(_)) => ExhPat::Wildcard,
        hlr::PatternKind::Ref(inner) | hlr::PatternKind::RefMut(inner) => {
            ExhPat::Ctor(Ctor::Ref, vec![lower_pattern(inner, bindings)])
        }
    }
}

fn wrap_if_ergonomics<'ctxt>(
    inner: ExhPat,
    pattern: hlr::Pattern<'ctxt>,
    bindings: &HashMap<*const hlr::PatternKind<'ctxt>, MatchBinding>,
) -> ExhPat {
    match bindings.get(&(pattern as *const _)) {
        Some(MatchBinding::ByRef) | Some(MatchBinding::ByRefMut) => ExhPat::Ctor(Ctor::Ref, vec![inner]),
        _ => inner,
    }
}

fn is_useful<'ctxt>(matrix: &Matrix, row: &[ExhPat], tys: &[ty::Ty<'ctxt>], ctxt: &ctxt::Ctxt<'ctxt>) -> bool {
    if matrix.is_empty() {
        return true;
    }
    if row.is_empty() {
        return false;
    }

    match &row[0] {
        ExhPat::Ctor(ctor, _) => {
            let sub_tys = specialize_ty(tys[0], ctor, ctxt);
            let arity = sub_tys.len();
            let new_tys: Vec<_> = sub_tys.into_iter().chain(tys[1..].iter().copied()).collect();
            let new_matrix = specialize_matrix(matrix, ctor, arity);
            let new_row = specialize_row(row, ctor, arity).unwrap();
            is_useful(&new_matrix, &new_row, &new_tys, ctxt)
        }
        ExhPat::Wildcard => {
            let sigma: HashSet<Ctor> = matrix.iter().filter_map(|r| as_ctor(&r[0])).collect();
            let ctors = all_ctors(tys[0], ctxt);

            if !ctors.is_empty() && ctors.iter().all(|(c, _)| sigma.contains(c)) {
                ctors.iter().any(|(ctor, arity)| {
                    let sub_tys = specialize_ty(tys[0], ctor, ctxt);
                    let new_tys: Vec<_> = sub_tys.into_iter().chain(tys[1..].iter().copied()).collect();
                    let new_matrix = specialize_matrix(matrix, ctor, *arity);
                    let wildcard_row: Vec<ExhPat> = (0..*arity)
                        .map(|_| ExhPat::Wildcard)
                        .chain(row[1..].iter().cloned())
                        .collect();
                    is_useful(&new_matrix, &wildcard_row, &new_tys, ctxt)
                })
            } else {
                is_useful(&default_matrix(matrix), &row[1..], &tys[1..], ctxt)
            }
        }
    }
}

fn specialize_matrix(matrix: &Matrix, ctor: &Ctor, arity: usize) -> Matrix {
    matrix
        .iter()
        .filter_map(|row| specialize_row(row, ctor, arity))
        .collect()
}

fn specialize_row(row: &[ExhPat], ctor: &Ctor, arity: usize) -> Option<Vec<ExhPat>> {
    let [first, tail @ ..] = row else { panic!() };
    match first {
        ExhPat::Wildcard => Some(
            (0..arity)
                .map(|_| ExhPat::Wildcard)
                .chain(tail.iter().cloned())
                .collect(),
        ),
        ExhPat::Ctor(c, fields) if c == ctor => Some(fields.iter().cloned().chain(tail.iter().cloned()).collect()),
        ExhPat::Ctor(_, _) => None,
    }
}

fn default_matrix(matrix: &Matrix) -> Matrix {
    matrix
        .iter()
        .filter_map(|row| {
            if matches!(row[0], ExhPat::Wildcard) {
                Some(row[1..].to_vec())
            } else {
                None
            }
        })
        .collect()
}

fn as_ctor(pat: &ExhPat) -> Option<Ctor> {
    match pat {
        ExhPat::Wildcard => None,
        ExhPat::Ctor(c, _) => Some(c.clone()),
    }
}

fn all_ctors<'ctxt>(ty: ty::Ty<'ctxt>, ctxt: &ctxt::Ctxt<'ctxt>) -> Vec<(Ctor, usize)> {
    match *ty.0 {
        ty::TyDef::Enum { enum_, .. } => (0..enum_.get_variants().len())
            .map(|i| {
                let variant_ty = ctxt.tys.get_enum_variant_ty(ty, i).unwrap();
                let arity = ctxt.tys.get_struct_field_tys(variant_ty).unwrap().len();
                (Ctor::Variant(i), arity)
            })
            .collect(),
        ty::TyDef::Tuple(fields) => vec![(Ctor::Tuple, fields.len())],
        ty::TyDef::Struct { .. } => {
            let arity = ctxt.tys.get_struct_field_tys(ty).unwrap().len();
            vec![(Ctor::Struct, arity)]
        }
        ty::TyDef::Primitive(ty::Primitive::Boolean) => vec![(Ctor::Bool(false), 0), (Ctor::Bool(true), 0)],
        ty::TyDef::Ref(_) | ty::TyDef::RefMut(_) => vec![(Ctor::Ref, 1)],
        _ => vec![],
    }
}

fn specialize_ty<'ctxt>(ty: ty::Ty<'ctxt>, ctor: &Ctor, ctxt: &ctxt::Ctxt<'ctxt>) -> Vec<ty::Ty<'ctxt>> {
    match (ty.0, ctor) {
        (ty::TyDef::Enum { .. }, Ctor::Variant(i)) => {
            let variant_ty = ctxt.tys.get_enum_variant_ty(ty, *i).unwrap();
            ctxt.tys.get_struct_field_tys(variant_ty).unwrap()
        }
        (ty::TyDef::Tuple(fields), Ctor::Tuple) => fields.to_vec(),
        (ty::TyDef::Struct { .. }, Ctor::Struct) => ctxt.tys.get_struct_field_tys(ty).unwrap(),
        (ty::TyDef::Primitive(ty::Primitive::Boolean), Ctor::Bool(_)) => vec![],
        (_, Ctor::Int(_)) => vec![],
        (ty::TyDef::Ref(inner), Ctor::Ref) | (ty::TyDef::RefMut(inner), Ctor::Ref) => vec![*inner],
        _ => vec![],
    }
}
