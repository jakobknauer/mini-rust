use crate::ctxt::{self, fns, ty::Primitive};

macro_rules! register_fn {
    ($fn_reg:expr, $name:expr, ( $( $param_name:ident : $param_ty:ident ),* ) -> $return_ty:expr ) => {
        $fn_reg.register_fn(fns::FnSig {
            name: $name.to_string(),
            return_ty: $return_ty,
            parameters: vec![
                $(
                    fns::FnParam {
                        name: stringify!($param_name).to_string(),
                        ty: $param_ty,
                    },
                )*
            ],
        })?;
    };
}

pub fn register_fns(tys: &ctxt::TyReg, fns: &mut ctxt::FnReg) -> Result<(), ()> {
    use Primitive::*;

    let i32 = tys.get_primitive_ty(Integer32).ok_or(())?;
    let bool = tys.get_primitive_ty(Boolean).ok_or(())?;
    let unit = tys.get_primitive_ty(Unit).ok_or(())?;

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

    Ok(())
}
