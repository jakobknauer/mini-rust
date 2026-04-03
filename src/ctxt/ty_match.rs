use std::collections::HashMap;

use crate::ctxt::ty::{GenVar, Ty, TyDef};

/// Finds a substitution of `gen_vars` that unifies `generic` with `target`.
/// Returns bindings in the same order as `gen_vars`, or `Err` if no match exists.
pub fn try_find_instantiation<'ty>(
    target: Ty<'ty>,
    generic: Ty<'ty>,
    gen_vars: &[GenVar<'ty>],
) -> Result<Vec<Ty<'ty>>, ()> {
    try_find_instantiation_from_pairs(&[(target, generic)], gen_vars)
}

/// Like [`try_find_instantiation`], but unifies multiple `(target, generic)` pairs simultaneously.
/// Useful when gen vars only appear in some pairs (e.g. in trait gen args but not the `for` type).
pub fn try_find_instantiation_from_pairs<'ty>(
    pairs: &[(Ty<'ty>, Ty<'ty>)],
    gen_vars: &[GenVar<'ty>],
) -> Result<Vec<Ty<'ty>>, ()> {
    let mut instantiations = HashMap::new();
    for gen_param in gen_vars {
        instantiations.insert(*gen_param, None);
    }

    for &(target, generic) in pairs {
        if !try_find_instantiation_internal(target, generic, &mut instantiations) {
            return Err(());
        }
    }

    gen_vars
        .iter()
        .map(|gen_var| instantiations[gen_var])
        .collect::<Option<_>>()
        .ok_or(())
}

fn try_find_instantiation_internal<'ty>(
    target: Ty<'ty>,
    generic: Ty<'ty>,
    instantiation: &mut HashMap<GenVar<'ty>, Option<Ty<'ty>>>,
) -> bool {
    use TyDef::*;

    if let &GenVar(gen_var) = generic.0
        && instantiation.contains_key(&gen_var)
    {
        let substitute = instantiation.get(&gen_var).unwrap();
        if let Some(substitute) = substitute {
            return *substitute == target;
        } else {
            instantiation.insert(gen_var, Some(target));
            return true;
        }
    }

    match (target.0, generic.0) {
        (GenVar(var1), GenVar(var2)) => var1 == var2,

        (&Primitive(a), &Primitive(b)) => a == b,

        (
            &Fn {
                param_tys: params1,
                return_ty: ret1,
                var_args: var_args1,
            },
            &Fn {
                param_tys: params2,
                return_ty: ret2,
                var_args: var_args2,
            },
        ) => {
            params1.len() == params2.len()
                && params1
                    .iter()
                    .zip(params2.iter())
                    .all(|(&ty1, &ty2)| try_find_instantiation_internal(ty1, ty2, instantiation))
                && try_find_instantiation_internal(ret1, ret2, instantiation)
                && var_args1 == var_args2
        }

        (&Ref(inner1), &Ref(inner2)) | (&RefMut(inner1), &RefMut(inner2)) | (&Ptr(inner1), &Ptr(inner2)) => {
            try_find_instantiation_internal(inner1, inner2, instantiation)
        }

        (
            &Struct {
                struct_: struct1,
                gen_args: gen_args1,
            },
            &Struct {
                struct_: struct2,
                gen_args: gen_args2,
            },
        ) => {
            struct1 == struct2
                && gen_args1.len() == gen_args2.len()
                && gen_args1
                    .iter()
                    .zip(gen_args2.iter())
                    .all(|(&ty1, &ty2)| try_find_instantiation_internal(ty1, ty2, instantiation))
        }

        (
            &Enum {
                enum_: enum1,
                gen_args: gen_args1,
            },
            &Enum {
                enum_: enum2,
                gen_args: gen_args2,
            },
        ) => {
            enum1 == enum2
                && gen_args1.len() == gen_args2.len()
                && gen_args1
                    .iter()
                    .zip(gen_args2.iter())
                    .all(|(&ty1, &ty2)| try_find_instantiation_internal(ty1, ty2, instantiation))
        }

        (Closure { .. }, Closure { .. }) => target == generic,

        (&Tuple(items1), &Tuple(items2)) => {
            items1.len() == items2.len()
                && items1
                    .iter()
                    .zip(items2.iter())
                    .all(|(&ty1, &ty2)| try_find_instantiation_internal(ty1, ty2, instantiation))
        }

        (
            &AssocTy {
                base_ty: base_ty1,
                trait_inst: trait_inst1,
                assoc_ty_idx: assoc_ty_idx1,
            },
            &AssocTy {
                base_ty: base_ty2,
                trait_inst: trait_inst2,
                assoc_ty_idx: assoc_ty_idx2,
            },
        ) => {
            try_find_instantiation_internal(base_ty1, base_ty2, instantiation)
                && trait_inst1.trait_ == trait_inst2.trait_
                && trait_inst1.gen_args.len() == trait_inst2.gen_args.len()
                && trait_inst1
                    .gen_args
                    .iter()
                    .zip(trait_inst2.gen_args.iter())
                    .all(|(&ty1, &ty2)| try_find_instantiation_internal(ty1, ty2, instantiation))
                && assoc_ty_idx1 == assoc_ty_idx2
        }

        (
            &Opaque {
                opaque: opaque1,
                gen_args: gen_args1,
            },
            &Opaque {
                opaque: opaque2,
                gen_args: gen_args2,
            },
        ) => {
            opaque1 == opaque2
                && gen_args1.len() == gen_args2.len()
                && gen_args1
                    .iter()
                    .zip(gen_args2.iter())
                    .all(|(&ty1, &ty2)| try_find_instantiation_internal(ty1, ty2, instantiation))
        }

        (_, _) => false,
    }
}
