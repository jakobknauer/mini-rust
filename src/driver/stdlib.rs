use crate::ctxt::{self, fns, types::PrimitiveType};

macro_rules! register_fn {
    ($fn_reg:expr, $name:expr, ( $( $param_name:ident : $param_type:ident ),* ) -> $return_type:expr ) => {
        $fn_reg.register_fn(fns::FnSig {
            name: $name.to_string(),
            return_type: $return_type,
            parameters: vec![
                $(
                    fns::FnParam {
                        name: stringify!($param_name).to_string(),
                        type_: $param_type,
                    },
                )*
            ],
        })?;
    };
}

pub fn register_fns(
    type_registry: &ctxt::TypeRegistry,
    fns: &mut ctxt::FnReg,
) -> Result<(), ()> {
    use PrimitiveType::*;

    let i32 = type_registry.get_primitive_type_id(Integer32).ok_or(())?;
    let bool = type_registry.get_primitive_type_id(Boolean).ok_or(())?;
    let unit = type_registry.get_primitive_type_id(Unit).ok_or(())?;

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
