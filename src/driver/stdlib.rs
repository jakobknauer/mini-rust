use crate::ctxt::{self, fns, ty};

pub fn register_fns(ctxt: &mut ctxt::Ctxt) -> Result<(), ()> {
    register_size_of(ctxt)
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
