use crate::ctxt::{self, fns, traits, ty};

pub fn register_fns(ctxt: &mut ctxt::Ctxt) -> Result<(), ()> {
    register_size_of(ctxt)
}

pub fn register_add_trait(ctxt: &mut ctxt::Ctxt) {
    let rhs_var = ctxt.tys.register_gen_var("Rhs");
    let trait_ = ctxt.traits.register_trait("Add", vec![rhs_var]);
    ctxt.traits.register_assoc_ty(trait_, "Output");

    let self_ty = ctxt.tys.trait_self(trait_);
    let rhs_ty = ctxt.tys.gen_var(rhs_var);
    let trait_inst = traits::TraitInst {
        trait_,
        gen_args: ctxt.tys.ty_slice(&[rhs_ty]),
    };
    let output_ty = ctxt.tys.assoc_ty(self_ty, trait_inst, 0);

    ctxt.traits.register_mthd(
        trait_,
        fns::FnSig {
            name: "add".to_string(),
            associated_ty: None,
            associated_trait_inst: Some(traits::TraitInst {
                trait_,
                gen_args: ctxt.tys.ty_slice(&[]),
            }),
            gen_params: vec![],
            env_gen_params: vec![rhs_var],
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
        },
    );

    ctxt.language_items.add_trait = Some(trait_);
}

fn register_size_of(ctxt: &mut ctxt::Ctxt) -> Result<(), ()> {
    let fn_ = ctxt.fns.register_fn(
        fns::FnSig {
            name: "size_of".to_string(),
            associated_ty: None,
            associated_trait_inst: None,
            gen_params: vec![ctxt.tys.register_gen_var("T")],
            env_gen_params: vec![],
            params: vec![],
            var_args: false,
            return_ty: ctxt.tys.primitive(ty::Primitive::Integer32),
        },
        true,
    )?;
    ctxt.language_items.size_of = Some(fn_);
    Ok(())
}

pub fn register_impl_for_ptr(ctxt: &mut ctxt::Ctxt) -> Result<(), ()> {
    let var = ctxt.tys.register_gen_var("T");
    let var_ty = ctxt.tys.gen_var(var);
    let ptr_ty = ctxt.tys.ptr(var_ty);
    let ref_ptr_ty = ctxt.tys.ref_(ptr_ty);

    let impl_ = ctxt.impls.register_impl(ptr_ty, vec![var], None);
    let fn_ = ctxt.fns.register_fn(
        fns::FnSig {
            name: "offset".to_string(),
            associated_ty: Some(ptr_ty),
            associated_trait_inst: None,
            gen_params: vec![],
            env_gen_params: vec![var],
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
        },
        true,
    )?;
    ctxt.impls.register_mthd(impl_, fn_, "offset");

    ctxt.language_items.ptr_offset = Some(fn_);

    Ok(())
}
