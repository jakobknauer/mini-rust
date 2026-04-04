use std::collections::HashSet;

use crate::{
    ctxt::ty::TyDef,
    mlr::{self, Fn, Place, PlaceDef, StmtDef, Val, ValDef},
};

#[derive(Debug)]
pub enum MutckError {
    AssignToImmutablePlace,
    AddrOfMutOfImmutablePlace,
    AssignThroughImmutableRef,
}

pub fn mutck(fns: &[Fn]) -> Result<(), MutckError> {
    for fn_ in fns {
        Mutck::check_fn(fn_)?;
    }
    Ok(())
}

struct Mutck<'mlr> {
    mutable_locs: HashSet<mlr::Loc<'mlr>>,
    fresh_locs: HashSet<mlr::Loc<'mlr>>,
}

impl<'mlr> Mutck<'mlr> {
    fn check_fn(fn_: &'mlr Fn) -> Result<(), MutckError> {
        let mut mutck = Mutck {
            mutable_locs: HashSet::new(),
            fresh_locs: HashSet::new(),
        };

        // Params: mutability from FnDecl; closures have a captures struct as first param,
        // which is never mutable, so we skip it if present.
        let param_offset = fn_.param_locs.len().saturating_sub(fn_.fn_.params.len());
        for (loc, param) in fn_.param_locs[param_offset..].iter().zip(&fn_.fn_.params) {
            if param.mutable {
                mutck.mutable_locs.insert(*loc);
            }
        }

        mutck.check_stmt(fn_.body)
    }

    fn check_stmt(&mut self, stmt: mlr::Stmt<'mlr>) -> Result<(), MutckError> {
        match *stmt {
            StmtDef::Alloc { loc, mutable } => {
                if mutable {
                    self.mutable_locs.insert(loc);
                } else {
                    self.fresh_locs.insert(loc);
                }
                Ok(())
            }
            StmtDef::Assign { place, value } => {
                self.check_assign_place(place)?;
                self.check_val(value)
            }
            StmtDef::Return { value } => self.check_val(value),
            StmtDef::Block(stmts) => {
                for &s in stmts {
                    self.check_stmt(s)?;
                }
                Ok(())
            }
            StmtDef::If(if_) => {
                self.check_stmt(if_.then)?;
                self.check_stmt(if_.else_)
            }
            StmtDef::Loop { body } => self.check_stmt(body),
            StmtDef::Break => Ok(()),
        }
    }

    fn check_assign_place(&mut self, place: Place<'mlr>) -> Result<(), MutckError> {
        match *place.0 {
            PlaceDef::Loc(loc) => {
                if self.mutable_locs.contains(&loc) || self.fresh_locs.remove(&loc) {
                    Ok(())
                } else {
                    Err(MutckError::AssignToImmutablePlace)
                }
            }
            PlaceDef::Deref(op) => {
                if matches!(op.1.0, TyDef::Ref(_)) {
                    Err(MutckError::AssignThroughImmutableRef)
                } else {
                    Ok(())
                }
            }
            PlaceDef::FieldAccess { base, .. }
            | PlaceDef::EnumDiscriminant { base }
            | PlaceDef::ProjectToVariant { base, .. } => self.check_assign_place(base),
            PlaceDef::ClosureCaptures(_) => Ok(()),
        }
    }

    fn check_val(&mut self, val: Val<'mlr>) -> Result<(), MutckError> {
        match *val.0 {
            ValDef::AddrOf(place) => {
                if matches!(val.1.0, TyDef::RefMut(_)) && !self.place_is_mutable(place) {
                    Err(MutckError::AddrOfMutOfImmutablePlace)
                } else {
                    Ok(())
                }
            }
            ValDef::Call { .. }
            | ValDef::Use(..)
            | ValDef::As { .. }
            | ValDef::UnaryPrim { .. }
            | ValDef::BinaryPrim { .. } => Ok(()),
        }
    }

    fn place_is_mutable(&self, place: Place<'mlr>) -> bool {
        match *place.0 {
            PlaceDef::Loc(loc) => self.mutable_locs.contains(&loc) || self.fresh_locs.contains(&loc),
            PlaceDef::FieldAccess { base, .. }
            | PlaceDef::EnumDiscriminant { base }
            | PlaceDef::ProjectToVariant { base, .. } => self.place_is_mutable(base),
            PlaceDef::ClosureCaptures(_) => true,
            PlaceDef::Deref(op) => matches!(op.1.0, TyDef::RefMut(_) | TyDef::Ptr(_)),
        }
    }
}
