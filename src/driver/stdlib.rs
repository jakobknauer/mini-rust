use std::collections::HashMap;

use crate::ctxt::{self, fns, traits, ty};

pub fn register_fns<'ctxt>(ctxt: &mut ctxt::Ctxt<'ctxt>) -> Result<(), ()> {
    register_size_of(ctxt)
}

fn register_arith_trait<'ctxt>(
    ctxt: &mut ctxt::Ctxt<'ctxt>,
    trait_name: &str,
    mthd_name: &str,
) -> traits::Trait<'ctxt> {
    let rhs_var = ctxt.tys.register_gen_var("Rhs");
    let trait_ = ctxt
        .traits
        .register_trait(trait_name, vec![rhs_var], vec!["Output".to_string()]);

    let self_ty = ctxt.tys.trait_self(trait_);
    let rhs_ty = ctxt.tys.gen_var(rhs_var);
    let gen_args = ctxt.tys.ty_slice(&[rhs_ty]);
    let trait_inst = ctxt.traits.inst_trait(trait_, gen_args).unwrap();
    let output_ty = ctxt.tys.assoc_ty(self_ty, trait_inst, 0);

    let fn_ = ctxt
        .fns
        .register_fn(
            fns::FnDecl {
                id: fns::FnId::default(),
                name: mthd_name.to_string(),
                associated_ty: None,
                associated_trait_inst: Some(ctxt.traits.inst_trait(trait_, ctxt.tys.ty_slice(&[rhs_ty])).unwrap()),
                gen_params: vec![],
                env_gen_params: vec![rhs_var],
                env_constraints: Vec::new(),
                params: vec![
                    fns::FnParam {
                        kind: fns::FnParamKind::Self_,
                        ty: self_ty,
                    },
                    fns::FnParam {
                        kind: fns::FnParamKind::Regular("rhs".to_string()),
                        ty: rhs_ty,
                    },
                ],
                var_args: false,
                return_ty: output_ty,
                constraints: Vec::new(),
            },
            false,
        )
        .unwrap();
    ctxt.traits.register_mthd(trait_, fn_);

    trait_
}

pub fn register_add_trait<'ctxt>(ctxt: &mut ctxt::Ctxt<'ctxt>) {
    ctxt.language_items.add_trait = Some(register_arith_trait(ctxt, "Add", "add"));
}

pub fn register_sub_trait<'ctxt>(ctxt: &mut ctxt::Ctxt<'ctxt>) {
    ctxt.language_items.sub_trait = Some(register_arith_trait(ctxt, "Sub", "sub"));
}

pub fn register_mul_trait<'ctxt>(ctxt: &mut ctxt::Ctxt<'ctxt>) {
    ctxt.language_items.mul_trait = Some(register_arith_trait(ctxt, "Mul", "mul"));
}

pub fn register_div_trait<'ctxt>(ctxt: &mut ctxt::Ctxt<'ctxt>) {
    ctxt.language_items.div_trait = Some(register_arith_trait(ctxt, "Div", "div"));
}

pub fn register_bit_or_trait<'ctxt>(ctxt: &mut ctxt::Ctxt<'ctxt>) {
    ctxt.language_items.bit_or_trait = Some(register_arith_trait(ctxt, "BitOr", "bitor"));
}

pub fn register_bit_and_trait<'ctxt>(ctxt: &mut ctxt::Ctxt<'ctxt>) {
    ctxt.language_items.bit_and_trait = Some(register_arith_trait(ctxt, "BitAnd", "bitand"));
}

pub fn register_rem_trait<'ctxt>(ctxt: &mut ctxt::Ctxt<'ctxt>) {
    ctxt.language_items.rem_trait = Some(register_arith_trait(ctxt, "Rem", "rem"));
}

fn register_size_of<'ctxt>(ctxt: &mut ctxt::Ctxt<'ctxt>) -> Result<(), ()> {
    let fn_ = ctxt.fns.register_fn(
        fns::FnDecl {
            id: fns::FnId::default(),
            name: "size_of".to_string(),
            associated_ty: None,
            associated_trait_inst: None,
            gen_params: vec![ctxt.tys.register_gen_var("T")],
            env_gen_params: vec![],
            env_constraints: Vec::new(),
            params: vec![],
            var_args: false,
            return_ty: ctxt.tys.primitive(ty::Primitive::Integer32),
            constraints: Vec::new(),
        },
        true,
    )?;
    ctxt.language_items.size_of = Some(fn_);
    Ok(())
}

pub fn register_impl_for_ptr<'ctxt>(ctxt: &mut ctxt::Ctxt<'ctxt>) -> Result<(), ()> {
    let var = ctxt.tys.register_gen_var("T");
    let var_ty = ctxt.tys.gen_var(var);
    let ptr_ty = ctxt.tys.ptr(var_ty);
    let ref_ptr_ty = ctxt.tys.ref_(ptr_ty);

    let impl_ = ctxt
        .impls
        .register_impl(ptr_ty, vec![var], None, Vec::new(), HashMap::new());
    let fn_ = ctxt.fns.register_fn(
        fns::FnDecl {
            id: fns::FnId::default(),
            name: "offset".to_string(),
            associated_ty: Some(ptr_ty),
            associated_trait_inst: None,
            gen_params: vec![],
            env_gen_params: vec![var],
            env_constraints: Vec::new(),
            params: vec![
                ctxt::fns::FnParam {
                    kind: ctxt::fns::FnParamKind::SelfByRef,
                    ty: ref_ptr_ty,
                },
                ctxt::fns::FnParam {
                    kind: ctxt::fns::FnParamKind::Regular("offset".to_string()),
                    ty: ctxt.tys.primitive(ty::Primitive::Integer32),
                },
            ],
            var_args: false,
            return_ty: ptr_ty,
            constraints: Vec::new(),
        },
        true,
    )?;
    ctxt.impls.register_mthd(impl_, fn_, "offset");

    ctxt.language_items.ptr_offset = Some(fn_);

    Ok(())
}
