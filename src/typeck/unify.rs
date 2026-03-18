use std::collections::HashMap;

use crate::ctxt::ty;

pub enum UnificationResult<'ty> {
    Success(Vec<(ty::InfVar, ty::Ty<'ty>)>),
    Failure,
}

impl<'a, 'ctxt: 'a + 'hlr, 'hlr: 'ctxt> super::Typeck<'a, 'ctxt, 'hlr> {
    pub(super) fn unify(&mut self, ty1: ty::Ty<'ctxt>, ty2: ty::Ty<'ctxt>) -> bool {
        let ty1 = self.normalize(ty1);
        let ty2 = self.normalize(ty2);
        let result = {
            let unify = Unify::new(&self.type_vars);
            unify.run(ty1, ty2)
        };
        match result {
            UnificationResult::Success(bindings) => {
                self.type_vars.extend(bindings);
                true
            }
            UnificationResult::Failure => false,
        }
    }
}

struct Unify<'borrow, 'ty> {
    committed: &'borrow HashMap<ty::InfVar, ty::Ty<'ty>>,
    pending: HashMap<ty::InfVar, ty::Ty<'ty>>,
}

impl<'borrow, 'ty> Unify<'borrow, 'ty> {
    fn new(committed: &'borrow HashMap<ty::InfVar, ty::Ty<'ty>>) -> Self {
        Unify {
            committed,
            pending: HashMap::new(),
        }
    }

    fn run(mut self, ty1: ty::Ty<'ty>, ty2: ty::Ty<'ty>) -> UnificationResult<'ty> {
        if self.unify(ty1, ty2) {
            UnificationResult::Success(self.pending.into_iter().collect())
        } else {
            UnificationResult::Failure
        }
    }

    fn resolve(&self, mut ty: ty::Ty<'ty>) -> ty::Ty<'ty> {
        while let &ty::TyDef::InfVar(iv) = ty.0 {
            let resolved = self.pending.get(&iv).or_else(|| self.committed.get(&iv));

            match resolved {
                Some(resolved) => ty = *resolved,
                None => break,
            }
        }
        ty
    }

    fn unify(&mut self, ty1: ty::Ty<'ty>, ty2: ty::Ty<'ty>) -> bool {
        let ty1 = self.resolve(ty1);
        let ty2 = self.resolve(ty2);

        if ty1 == ty2 {
            return true;
        }

        match (ty1.0, ty2.0) {
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
                param_tys_1.len() == param_tys_2.len()
                    && var_args_1 == var_args_2
                    && param_tys_1.iter().zip(param_tys_2).all(|(t1, t2)| self.unify(*t1, *t2))
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
                struct_1 == struct_2
                    && gen_args_1.len() == gen_args_2.len()
                    && gen_args_1.iter().zip(gen_args_2).all(|(t1, t2)| self.unify(*t1, *t2))
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
                enum_1 == enum_2
                    && gen_args_1.len() == gen_args_2.len()
                    && gen_args_1.iter().zip(gen_args_2).all(|(t1, t2)| self.unify(*t1, *t2))
            }

            (&ty::TyDef::Tuple(tys_1), &ty::TyDef::Tuple(tys_2)) => {
                tys_1.len() == tys_2.len() && tys_1.iter().zip(tys_2).all(|(t1, t2)| self.unify(*t1, *t2))
            }

            (ty::TyDef::Closure { name: name_1, .. }, ty::TyDef::Closure { name: name_2, .. }) => name_1 == name_2,

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
                trait_inst_1.trait_ == trait_inst_2.trait_
                    && assoc_ty_idx_1 == assoc_ty_idx_2
                    && trait_inst_1.gen_args.len() == trait_inst_2.gen_args.len()
                    && self.unify(base_ty_1, base_ty_2)
                    && trait_inst_1
                        .gen_args
                        .iter()
                        .zip(trait_inst_2.gen_args)
                        .all(|(t1, t2)| self.unify(*t1, *t2))
            }

            (
                &ty::TyDef::Opaque {
                    opaque: opaque1,
                    gen_args: gen_args1,
                },
                &ty::TyDef::Opaque {
                    opaque: opaque2,
                    gen_args: gen_args2,
                },
            ) => {
                opaque1 == opaque2
                    && gen_args1.len() == gen_args2.len()
                    && gen_args1.iter().zip(gen_args2).all(|(t1, t2)| self.unify(*t1, *t2))
            }

            _ => false,
        }
    }
}
