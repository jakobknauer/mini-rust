use std::collections::HashMap;

use crate::ctxt::{self, ty};

pub enum UnificationResult {
    Success(Vec<(ty::InfVar, ty::Ty)>),
    Failure,
}

impl<'ctxt, 'hlr> super::Typeck<'ctxt, 'hlr> {
    pub(super) fn unify(&mut self, ty1: ty::Ty, ty2: ty::Ty) -> bool {
        let unify = Unify::new(&self.ctxt.tys, &self.type_vars);
        let result = unify.run(ty1, ty2);
        match result {
            UnificationResult::Success(bindings) => {
                self.type_vars.extend(bindings);
                true
            }
            UnificationResult::Failure => false,
        }
    }
}

struct Unify<'a> {
    tys: &'a ctxt::TyReg,
    committed: &'a HashMap<ty::InfVar, ty::Ty>,
    pending: HashMap<ty::InfVar, ty::Ty>,
}

impl<'a> Unify<'a> {
    fn new(tys: &'a ctxt::TyReg, committed: &'a HashMap<ty::InfVar, ty::Ty>) -> Self {
        Unify {
            tys,
            committed,
            pending: HashMap::new(),
        }
    }

    fn run(mut self, ty1: ty::Ty, ty2: ty::Ty) -> UnificationResult {
        if self.unify(ty1, ty2) {
            UnificationResult::Success(self.pending.into_iter().collect())
        } else {
            UnificationResult::Failure
        }
    }

    fn resolve(&self, mut ty: ty::Ty) -> ty::Ty {
        while let Some(&ty::TyDef::InfVar(iv)) = self.tys.get_ty_def(ty) {
            let resolved = self.pending.get(&iv).or_else(|| self.committed.get(&iv));

            match resolved {
                Some(resolved) => ty = *resolved,
                None => break,
            }
        }
        ty
    }

    fn unify(&mut self, ty1: ty::Ty, ty2: ty::Ty) -> bool {
        let ty1 = self.resolve(ty1);
        let ty2 = self.resolve(ty2);

        if ty1 == ty2 {
            return true;
        }

        let def1 = self.tys.get_ty_def(ty1).unwrap();
        let def2 = self.tys.get_ty_def(ty2).unwrap();

        match (def1, def2) {
            (&ty::TyDef::InfVar(iv), _) => {
                self.pending.insert(iv, ty2);
                true
            }
            (_, &ty::TyDef::InfVar(iv)) => {
                self.pending.insert(iv, ty1);
                true
            }

            (
                &ty::TyDef::Fn {
                    param_tys: param_tys_1,
                    return_ty: return_ty_1,
                    var_args: var_args_1,
                },
                &ty::TyDef::Fn {
                    param_tys: param_tys_2,
                    return_ty: return_ty_2,
                    var_args: var_args_2,
                },
            ) => {
                if param_tys_1.len != param_tys_2.len || var_args_1 != var_args_2 {
                    return false;
                }
                let param_tys_1 = self.tys.get_ty_slice(param_tys_1);
                let param_tys_2 = self.tys.get_ty_slice(param_tys_2);
                param_tys_1.iter().zip(param_tys_2).all(|(t1, t2)| self.unify(*t1, *t2))
                    && self.unify(return_ty_1, return_ty_2)
            }

            (&ty::TyDef::Ref(ty_1), &ty::TyDef::Ref(ty_2)) => self.unify(ty_1, ty_2),
            (&ty::TyDef::Ptr(ty_1), &ty::TyDef::Ptr(ty_2)) => self.unify(ty_1, ty_2),

            (
                &ty::TyDef::Struct {
                    struct_: struct_1,
                    gen_args: gen_args_1,
                },
                &ty::TyDef::Struct {
                    struct_: struct_2,
                    gen_args: gen_args_2,
                },
            ) => {
                if struct_1 != struct_2 || gen_args_1.len != gen_args_2.len {
                    return false;
                }
                let gen_args_1 = self.tys.get_ty_slice(gen_args_1);
                let gen_args_2 = self.tys.get_ty_slice(gen_args_2);
                gen_args_1.iter().zip(gen_args_2).all(|(t1, t2)| self.unify(*t1, *t2))
            }

            (
                &ty::TyDef::Enum {
                    enum_: enum_1,
                    gen_args: gen_args_1,
                },
                &ty::TyDef::Enum {
                    enum_: enum_2,
                    gen_args: gen_args_2,
                },
            ) => {
                if enum_1 != enum_2 || gen_args_1.len != gen_args_2.len {
                    return false;
                }
                let gen_args_1 = self.tys.get_ty_slice(gen_args_1);
                let gen_args_2 = self.tys.get_ty_slice(gen_args_2);
                gen_args_1.iter().zip(gen_args_2).all(|(t1, t2)| self.unify(*t1, *t2))
            }

            (&ty::TyDef::Tuple(tys_1), &ty::TyDef::Tuple(tys_2)) => {
                if tys_1.len != tys_2.len {
                    return false;
                }
                let tys_1 = self.tys.get_ty_slice(tys_1);
                let tys_2 = self.tys.get_ty_slice(tys_2);
                tys_1.iter().zip(tys_2).all(|(t1, t2)| self.unify(*t1, *t2))
            }

            (&ty::TyDef::Closure { fn_inst: fn_inst_1, .. }, &ty::TyDef::Closure { fn_inst: fn_inst_2, .. }) => {
                if fn_inst_1.fn_ != fn_inst_2.fn_
                    || fn_inst_1.gen_args.len != fn_inst_2.gen_args.len
                    || fn_inst_1.env_gen_args.len != fn_inst_2.env_gen_args.len
                {
                    return false;
                }
                let gen_args_1 = self.tys.get_ty_slice(fn_inst_1.gen_args);
                let gen_args_2 = self.tys.get_ty_slice(fn_inst_2.gen_args);
                let env_gen_args_1 = self.tys.get_ty_slice(fn_inst_1.env_gen_args);
                let env_gen_args_2 = self.tys.get_ty_slice(fn_inst_2.env_gen_args);
                gen_args_1.iter().zip(gen_args_2).all(|(t1, t2)| self.unify(*t1, *t2))
                    && env_gen_args_1
                        .iter()
                        .zip(env_gen_args_2)
                        .all(|(t1, t2)| self.unify(*t1, *t2))
            }

            (
                &ty::TyDef::AssocTy {
                    base_ty: base_ty_1,
                    trait_inst: trait_inst_1,
                    assoc_ty_idx: assoc_ty_idx_1,
                },
                &ty::TyDef::AssocTy {
                    base_ty: base_ty_2,
                    trait_inst: trait_inst_2,
                    assoc_ty_idx: assoc_ty_idx_2,
                },
            ) => {
                if trait_inst_1.trait_ != trait_inst_2.trait_
                    || assoc_ty_idx_1 != assoc_ty_idx_2
                    || trait_inst_1.gen_args.len != trait_inst_2.gen_args.len
                {
                    return false;
                }
                let gen_args_1 = self.tys.get_ty_slice(trait_inst_1.gen_args);
                let gen_args_2 = self.tys.get_ty_slice(trait_inst_2.gen_args);
                self.unify(base_ty_1, base_ty_2)
                    && gen_args_1.iter().zip(gen_args_2).all(|(t1, t2)| self.unify(*t1, *t2))
            }

            _ => false,
        }
    }
}
