use std::collections::{HashMap, HashSet};

use crate::{
    ctxt::{
        fns,
        ty::{Ty, TyDef},
    },
    hlr,
    typeck::{DerefStep, ExprExtra, HlrTyping, MthdResolution},
};

#[derive(Debug)]
pub enum MutckError {
    AssignToImmutablePlace,
    AddrOfMutOfImmutablePlace,
    AssignThroughImmutableRef,
    MutReceiverNotMutRef,
    NoDerefMutImpl,
}

/// Why a place is not usable as a mutable place; mapped to a context-specific
/// [`MutckError`] at the use site (assignment, `&mut`, `&mut self` receiver).
enum PlaceImmutability {
    /// The place is (rooted in) an immutable binding.
    ImmutableVar,
    /// The place is behind a `&` (or a deref chain ending in one).
    ImmutableRef,
    /// The place is behind a `Deref` type that does not implement `DerefMut`.
    NoDerefMut,
}

/// For each expression whose deref steps reach a mutably used place (`FieldAccess` /
/// `MthdCall` chains and explicit `Deref` nodes), the index of the first step to lower
/// via `DerefMut::deref_mut`; all trait steps from that index on are `deref_mut` calls.
/// Explicit `Deref` nodes use index 0.
pub type DerefMutMarks = HashMap<hlr::ExprId, usize>;

#[allow(clippy::mutable_key_type)]
pub fn mutck<'ctxt>(
    hlr_fns: &[hlr::Fn<'ctxt>],
    typings: &HashMap<fns::Fn<'ctxt>, HlrTyping<'ctxt>>,
) -> Result<DerefMutMarks, MutckError> {
    let mut marks = DerefMutMarks::new();
    for hlr_fn in hlr_fns {
        if let Some(typing) = typings.get(&hlr_fn.fn_) {
            marks.extend(Mutck::check_fn(hlr_fn, typing)?);
        }
    }
    Ok(marks)
}

struct Mutck<'a, 'ctxt> {
    typing: &'a HlrTyping<'ctxt>,
    mutable_vars: HashSet<hlr::VarId>,
    deref_mut_marks: DerefMutMarks,
}

impl<'a, 'ctxt> Mutck<'a, 'ctxt> {
    fn check_fn(hlr_fn: &hlr::Fn<'ctxt>, typing: &'a HlrTyping<'ctxt>) -> Result<DerefMutMarks, MutckError> {
        let mut mutck = Mutck {
            typing,
            mutable_vars: HashSet::new(),
            deref_mut_marks: DerefMutMarks::new(),
        };

        for (param, &var_id) in hlr_fn.fn_.params.iter().zip(&hlr_fn.param_var_ids) {
            if param.mutable {
                mutck.mutable_vars.insert(var_id);
            }
        }

        mutck.check_expr(hlr_fn.body)?;
        Ok(mutck.deref_mut_marks)
    }

    fn check_expr(&mut self, expr: hlr::Expr<'ctxt>) -> Result<(), MutckError> {
        use hlr::ExprDef::*;
        match expr.0 {
            Lit(_) | Val(_) => Ok(()),
            BinaryOp { left, right, .. } => {
                self.check_expr(*left)?;
                self.check_expr(*right)
            }
            UnaryOp { operand, .. } => self.check_expr(*operand),
            Call { callee, args } => {
                self.check_expr(*callee)?;
                self.check_exprs(args)
            }
            MthdCall { receiver, args, .. } => {
                if self.mthd_call_takes_mut_receiver(expr.1) {
                    self.check_chain_mutable(expr.1, *receiver, self.mthd_call_steps(expr.1))
                        .map_err(|reason| match reason {
                            PlaceImmutability::NoDerefMut => MutckError::NoDerefMutImpl,
                            _ => MutckError::MutReceiverNotMutRef,
                        })?;
                }
                self.check_expr(*receiver)?;
                self.check_exprs(args)
            }
            Struct { fields, .. } => {
                for field in fields.iter() {
                    self.check_expr(field.expr)?;
                }
                Ok(())
            }
            FieldAccess { base, .. } => self.check_expr(*base),
            Tuple(exprs) => self.check_exprs(exprs),
            Assign { target, value } => {
                self.check_place_mutable(*target).map_err(|reason| match reason {
                    PlaceImmutability::ImmutableVar => MutckError::AssignToImmutablePlace,
                    PlaceImmutability::ImmutableRef => MutckError::AssignThroughImmutableRef,
                    PlaceImmutability::NoDerefMut => MutckError::NoDerefMutImpl,
                })?;
                self.check_expr(*target)?;
                self.check_expr(*value)
            }
            Deref(inner) => self.check_expr(*inner),
            AddrOf(inner) => self.check_expr(*inner),
            AddrOfMut(inner) => {
                self.check_place_mutable(*inner).map_err(|reason| match reason {
                    PlaceImmutability::NoDerefMut => MutckError::NoDerefMutImpl,
                    _ => MutckError::AddrOfMutOfImmutablePlace,
                })?;
                self.check_expr(*inner)
            }
            As { expr: inner, .. } => self.check_expr(*inner),
            // Closures capture by copy and closure params are always immutable, so the body
            // is checked as-is: captured vars keep the mutability of their original binding.
            Closure { body, .. } => self.check_expr(*body),
            If { cond, then, else_ } => {
                self.check_expr(*cond)?;
                self.check_expr(*then)?;
                if let Some(else_) = else_ {
                    self.check_expr(*else_)?;
                }
                Ok(())
            }
            Loop { body } => self.check_expr(*body),
            Match { scrutinee, arms } => {
                self.check_expr(*scrutinee)?;
                for arm in arms.iter() {
                    self.collect_pattern_bindings(arm.pattern);
                    self.check_expr(arm.body)?;
                }
                Ok(())
            }
            Block { stmts, trailing } => {
                for stmt in stmts.iter() {
                    self.check_stmt(stmt)?;
                }
                self.check_expr(*trailing)
            }
            QualifiedMthd { .. } => Ok(()),
        }
    }

    fn check_exprs(&mut self, exprs: hlr::ExprSlice<'ctxt>) -> Result<(), MutckError> {
        for &expr in exprs {
            self.check_expr(expr)?;
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: hlr::Stmt<'ctxt>) -> Result<(), MutckError> {
        match stmt {
            hlr::StmtDef::Expr(expr) => self.check_expr(*expr),
            &hlr::StmtDef::Let { var, mutable, init, .. } => {
                self.check_expr(init)?;
                if mutable {
                    self.mutable_vars.insert(var);
                }
                Ok(())
            }
            hlr::StmtDef::Break => Ok(()),
            hlr::StmtDef::Return(expr) => {
                if let Some(expr) = expr {
                    self.check_expr(*expr)?;
                }
                Ok(())
            }
        }
    }

    fn collect_pattern_bindings(&mut self, pattern: hlr::Pattern<'ctxt>) {
        use hlr::PatternKind::*;
        match pattern {
            Identifier { var_id, mutable } => {
                if *mutable {
                    self.mutable_vars.insert(*var_id);
                }
            }
            Or(alternatives) => alternatives.iter().for_each(|&p| self.collect_pattern_bindings(p)),
            Ref(inner) | RefMut(inner) => self.collect_pattern_bindings(inner),
            Variant(p) => p.fields.iter().for_each(|f| self.collect_pattern_bindings(f.pattern)),
            Struct(p) => p.fields.iter().for_each(|f| self.collect_pattern_bindings(f.pattern)),
            Tuple(sub) => sub.iter().for_each(|&p| self.collect_pattern_bindings(p)),
            Lit(_) | Wildcard => {}
        }
    }

    /// Checks that `place` is usable as a mutable place (assignment target, `&mut`
    /// operand, `&mut self` receiver). On success, records in `deref_mut_marks` which
    /// trait deref steps along the way must lower via `DerefMut::deref_mut`.
    fn check_place_mutable(&mut self, place: hlr::Expr<'ctxt>) -> Result<(), PlaceImmutability> {
        match place.0 {
            hlr::ExprDef::Val(hlr::Val::Var(var_id)) => {
                if self.mutable_vars.contains(var_id) {
                    Ok(())
                } else {
                    Err(PlaceImmutability::ImmutableVar)
                }
            }
            hlr::ExprDef::FieldAccess { base, .. } => {
                self.check_chain_mutable(place.1, *base, self.field_access_steps(place.1))
            }
            hlr::ExprDef::Deref(inner) => match self.typing.expr_extra.get(&place.1) {
                Some(ExprExtra::DerefMthd { mthd_mut, .. }) => {
                    if mthd_mut.is_none() {
                        return Err(PlaceImmutability::NoDerefMut);
                    }
                    // `deref_mut` takes `&mut self`, so the dereferenced place itself
                    // must be mutable.
                    self.check_place_mutable(*inner)?;
                    self.deref_mut_marks.insert(place.1, 0);
                    Ok(())
                }
                _ => {
                    if matches!(self.expr_ty(*inner).0, TyDef::RefMut(_) | TyDef::Ptr(_)) {
                        Ok(())
                    } else {
                        Err(PlaceImmutability::ImmutableRef)
                    }
                }
            },
            // Anything else is a temporary, which is mutable. (For assignments, typeck
            // already validated that the target is a place.)
            _ => Ok(()),
        }
    }

    /// Checks that the place reached by dereferencing `base` through `steps` is mutable.
    /// HLR field accesses and method receivers carry their auto-deref steps in
    /// `expr_extra` rather than as explicit `Deref` nodes, so place-mutability must walk
    /// them. The steps after the last builtin step form the `deref_mut` suffix: each of
    /// those trait steps must mutate its target, so it needs `DerefMut`. Steps before it
    /// only ever read (a builtin step merely copies the pointer out of its source place),
    /// so they stay `deref`, and the pointer dereferenced by the last builtin step must
    /// be `&mut`/`*`. If there is no builtin step, the suffix starts at `base`, whose
    /// place must itself be mutable.
    fn check_chain_mutable(
        &mut self,
        expr_id: hlr::ExprId,
        base: hlr::Expr<'ctxt>,
        steps: &[DerefStep<'ctxt>],
    ) -> Result<(), PlaceImmutability> {
        if steps.is_empty() {
            return self.check_place_mutable(base);
        }
        let mut_from = match steps.iter().rposition(|s| matches!(s, DerefStep::Builtin)) {
            Some(last_builtin) => {
                let mut ty = self.expr_ty(base);
                for step in &steps[..last_builtin] {
                    ty = match step {
                        DerefStep::Builtin => match ty.0 {
                            &TyDef::Ref(inner) | &TyDef::RefMut(inner) | &TyDef::Ptr(inner) => inner,
                            _ => return Err(PlaceImmutability::ImmutableRef),
                        },
                        DerefStep::Trait { target_ty, .. } => *target_ty,
                    };
                }
                if !matches!(ty.0, TyDef::RefMut(_) | TyDef::Ptr(_)) {
                    return Err(PlaceImmutability::ImmutableRef);
                }
                last_builtin + 1
            }
            None => {
                self.check_place_mutable(base)?;
                0
            }
        };
        if steps[mut_from..]
            .iter()
            .any(|s| matches!(s, DerefStep::Trait { mthd_mut: None, .. }))
        {
            return Err(PlaceImmutability::NoDerefMut);
        }
        if mut_from < steps.len() {
            self.deref_mut_marks.insert(expr_id, mut_from);
        }
        Ok(())
    }

    fn field_access_steps(&self, expr_id: hlr::ExprId) -> &'a [DerefStep<'ctxt>] {
        match self.typing.expr_extra.get(&expr_id) {
            Some(ExprExtra::FieldAccess { steps, .. }) => steps,
            _ => &[],
        }
    }

    fn mthd_call_steps(&self, expr_id: hlr::ExprId) -> &'a [DerefStep<'ctxt>] {
        match self.typing.expr_extra.get(&expr_id) {
            Some(ExprExtra::MthdCall { steps, .. }) => steps,
            _ => &[],
        }
    }

    fn mthd_call_takes_mut_receiver(&self, mthd_call_id: hlr::ExprId) -> bool {
        let Some(ExprExtra::MthdCall { resolution, .. }) = self.typing.expr_extra.get(&mthd_call_id) else {
            return false;
        };
        let params = match resolution {
            MthdResolution::Inherent(fn_inst) => &fn_inst.fn_.params,
            MthdResolution::Trait(inst) => &inst.mthd.fn_.params,
        };
        matches!(params.first().map(|p| &p.kind), Some(fns::FnParamKind::SelfByRefMut))
    }

    fn expr_ty(&self, expr: hlr::Expr<'ctxt>) -> Ty<'ctxt> {
        self.typing.expr_types[&expr.1]
    }
}
