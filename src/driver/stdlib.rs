use crate::{
    ctxt::{
        self,
        fns::{self},
        ty,
    },
    util::mlr_builder::MlrBuilder,
};

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

pub fn register_fns(tys: &mut ctxt::TyReg, fns: &mut ctxt::FnReg) -> Result<(), ()> {
    let i32 = tys.get_primitive_ty(ty::Primitive::Integer32);
    let bool = tys.get_primitive_ty(ty::Primitive::Boolean);
    let unit = tys.register_unit_ty();

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

    register_size_of(tys, fns)?;

    Ok(())
}

fn register_size_of(tys: &mut ctxt::TyReg, fns: &mut ctxt::FnReg) -> Result<(), ()> {
    fns.register_fn(
        fns::FnSig {
            name: "size_of".to_string(),
            associated_ty: None,
            associated_trait_inst: None,
            gen_params: vec![tys.register_gen_var("T")],
            env_gen_params: vec![],
            params: vec![],
            var_args: false,
            return_ty: tys.get_primitive_ty(ty::Primitive::Integer32),
        },
        true,
    )?;
    Ok(())
}

pub fn define_size_of(ctxt: &mut ctxt::Ctxt) -> Result<(), String> {
    let size_of_fn = ctxt
        .fns
        .get_fn_by_name("size_of")
        .ok_or("function size_of not registered")?;
    let mut builder = MlrBuilder::new(size_of_fn, ctxt);

    let body = {
        builder.start_new_block();

        let gen_var = builder.get_signature().gen_params[0];
        let gen_var_ty = builder.tys().register_gen_var_ty(gen_var);
        let size_of_val = builder.insert_size_of_val(gen_var_ty).unwrap();
        builder.insert_return_stmt(size_of_val).unwrap();

        builder.release_current_block()
    };

    let mlr = fns::FnMlr {
        body,
        param_locs: vec![],
    };

    ctxt.fns.add_fn_def(size_of_fn, mlr);
    Ok(())
}
