use crate::ctxt::{self, fns, ty};

macro_rules! register_fn {
    ($fn_reg:expr, $name:expr, ( $( $param_name:ident : $param_ty:ident ),* ) -> $return_ty:expr ) => {
        $fn_reg.register_fn(
            fns::FnSig {
                name: $name.to_string(),
                associated_ty: None,
                associated_trait_inst: None,
                gen_params: vec![],
                env_gen_params: vec![],
                params: vec![
                    $(
                        fns::FnParam {
                            kind: fns::FnParamKind::Regular(stringify!($param_name).to_string()),
                            ty: $param_ty,
                        },
                    )*
                ],
                var_args: false,
                return_ty: $return_ty,
            },
            true)?;
    };
}

pub fn register_fns(ctxt: &mut ctxt::Ctxt) -> Result<(), ()> {
    let tys = &mut ctxt.tys;
    let i32 = tys.primitive(ty::Primitive::Integer32);
    let bool = tys.primitive(ty::Primitive::Boolean);
    let unit = tys.unit();

    let fns = &mut ctxt.fns;

    register_fn!(fns, "add::<i32>", (a: i32, b: i32) -> i32);
    register_fn!(fns, "sub::<i32>", (a: i32, b: i32) -> i32);
    register_fn!(fns, "mul::<i32>", (a: i32, b: i32) -> i32);
    register_fn!(fns, "div::<i32>", (a: i32, b: i32) -> i32);
    register_fn!(fns, "rem::<i32>", (a: i32, b: i32) -> i32);

    register_fn!(fns, "eq::<i32>", (a: i32, b: i32) -> bool);
    register_fn!(fns, "ne::<i32>", (a: i32, b: i32) -> bool);
    register_fn!(fns, "eq::<bool>", (a: bool, b: bool) -> bool);
    register_fn!(fns, "ne::<bool>", (a: bool, b: bool) -> bool);
    register_fn!(fns, "eq::<()>", (a: unit, b: unit) -> bool);
    register_fn!(fns, "ne::<()>", (a: unit, b: unit) -> bool);

    register_fn!(fns, "bitor::<bool>", (a: bool, b: bool) -> bool);
    register_fn!(fns, "bitand::<bool>", (a: bool, b: bool) -> bool);

    register_fn!(fns, "lt::<i32>", (a: i32, b: i32) -> bool);
    register_fn!(fns, "gt::<i32>", (a: i32, b: i32) -> bool);
    register_fn!(fns, "le::<i32>", (a: i32, b: i32) -> bool);
    register_fn!(fns, "ge::<i32>", (a: i32, b: i32) -> bool);

    register_fn!(fns, "not::<bool>", (a: bool) -> bool);
    register_fn!(fns, "neg::<i32>", (a: i32) -> i32);

    register_size_of(ctxt)?;

    Ok(())
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
