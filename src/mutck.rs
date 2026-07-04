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
}

#[allow(clippy::mutable_key_type)]
pub fn mutck<'ctxt>(
    hlr_fns: &[hlr::Fn<'ctxt>],
    typings: &HashMap<fns::Fn<'ctxt>, HlrTyping<'ctxt>>,
) -> Result<(), MutckError> {
    for hlr_fn in hlr_fns {
        if let Some(typing) = typings.get(&hlr_fn.fn_) {
            Mutck::check_fn(hlr_fn, typing)?;
        }
    }
    Ok(())
}

struct Mutck<'a, 'ctxt> {
    typing: &'a HlrTyping<'ctxt>,
    mutable_vars: HashSet<hlr::VarId>,
}

impl<'a, 'ctxt> Mutck<'a, 'ctxt> {
    fn check_fn(hlr_fn: &hlr::Fn<'ctxt>, typing: &'a HlrTyping<'ctxt>) -> Result<(), MutckError> {
        let mut mutck = Mutck {
            typing,
            mutable_vars: HashSet::new(),
        };

        for (param, &var_id) in hlr_fn.fn_.params.iter().zip(&hlr_fn.param_var_ids) {
            if param.mutable {
                mutck.mutable_vars.insert(var_id);
            }
        }

        mutck.check_expr(hlr_fn.body)
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
                if self.mthd_call_takes_mut_receiver(expr.1) && !self.receiver_is_mutable(*receiver, expr.1) {
                    return Err(MutckError::MutReceiverNotMutRef);
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
                self.check_assign_place(*target)?;
                self.check_expr(*target)?;
                self.check_expr(*value)
            }
            Deref(inner) => self.check_expr(*inner),
            AddrOf(inner) => self.check_expr(*inner),
            AddrOfMut(inner) => {
                if !self.place_is_mutable(*inner) {
                    return Err(MutckError::AddrOfMutOfImmutablePlace);
                }
                self.check_expr(*inner)
            }
            As { expr: inner, .. } => self.check_expr(*inner),
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

    fn check_assign_place(&self, place: hlr::Expr<'ctxt>) -> Result<(), MutckError> {
        match place.0 {
            hlr::ExprDef::Val(hlr::Val::Var(var_id)) => {
                if self.mutable_vars.contains(var_id) {
                    Ok(())
                } else {
                    Err(MutckError::AssignToImmutablePlace)
                }
            }
            hlr::ExprDef::FieldAccess { base, .. } => {
                self.check_chain_assignable(*base, self.field_access_steps(place.1))
            }
            hlr::ExprDef::Deref(inner) => {
                if matches!(self.expr_ty(*inner).0, TyDef::RefMut(_) | TyDef::Ptr(_)) {
                    Ok(())
                } else {
                    Err(MutckError::AssignThroughImmutableRef)
                }
            }
            // typeck already validated that assignment targets are places.
            _ => Ok(()),
        }
    }

    fn place_is_mutable(&self, place: hlr::Expr<'ctxt>) -> bool {
        match place.0 {
            hlr::ExprDef::Val(hlr::Val::Var(var_id)) => self.mutable_vars.contains(var_id),
            hlr::ExprDef::FieldAccess { base, .. } => {
                self.chain_target_mutable(*base, self.field_access_steps(place.1))
            }
            hlr::ExprDef::Deref(inner) => matches!(self.expr_ty(*inner).0, TyDef::RefMut(_) | TyDef::Ptr(_)),
            // Anything else is a temporary, which is mutable.
            _ => true,
        }
    }

    /// Whether the place reached by dereferencing `base` through `steps` is mutable.
    /// HLR field accesses and method receivers carry their auto-deref steps in
    /// `expr_extra` rather than as explicit `Deref` nodes, so place-mutability must walk
    /// them: a custom (`Deref`-trait) step yields an immutable `&Target`, and an all-builtin
    /// chain is mutable iff the pointer dereferenced by the last step is `&mut`/`*`.
    fn chain_target_mutable(&self, base: hlr::Expr<'ctxt>, steps: &[DerefStep<'ctxt>]) -> bool {
        if steps.is_empty() {
            return self.place_is_mutable(base);
        }
        if steps.iter().any(|s| matches!(s, DerefStep::Trait(_))) {
            return false;
        }
        let mut ty = self.expr_ty(base);
        for _ in 0..steps.len() - 1 {
            ty = match ty.0 {
                &TyDef::Ref(inner) | &TyDef::RefMut(inner) | &TyDef::Ptr(inner) => inner,
                _ => return false,
            };
        }
        matches!(ty.0, TyDef::RefMut(_) | TyDef::Ptr(_))
    }

    /// Assignability counterpart of [`chain_target_mutable`], reporting the appropriate error.
    fn check_chain_assignable(&self, base: hlr::Expr<'ctxt>, steps: &[DerefStep<'ctxt>]) -> Result<(), MutckError> {
        if steps.is_empty() {
            return self.check_assign_place(base);
        }
        if steps.iter().any(|s| matches!(s, DerefStep::Trait(_))) {
            return Err(MutckError::AssignThroughImmutableRef);
        }
        let mut ty = self.expr_ty(base);
        for _ in 0..steps.len() - 1 {
            ty = match ty.0 {
                &TyDef::Ref(inner) | &TyDef::RefMut(inner) | &TyDef::Ptr(inner) => inner,
                _ => return Err(MutckError::AssignThroughImmutableRef),
            };
        }
        if matches!(ty.0, TyDef::RefMut(_) | TyDef::Ptr(_)) {
            Ok(())
        } else {
            Err(MutckError::AssignThroughImmutableRef)
        }
    }

    fn receiver_is_mutable(&self, receiver: hlr::Expr<'ctxt>, mthd_call_id: hlr::ExprId) -> bool {
        let steps = match self.typing.expr_extra.get(&mthd_call_id) {
            Some(ExprExtra::MthdCall { steps, .. }) => steps.as_slice(),
            _ => &[],
        };
        self.chain_target_mutable(receiver, steps)
    }

    fn field_access_steps(&self, expr_id: hlr::ExprId) -> &[DerefStep<'ctxt>] {
        match self.typing.expr_extra.get(&expr_id) {
            Some(ExprExtra::FieldAccess { steps, .. }) => steps,
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
