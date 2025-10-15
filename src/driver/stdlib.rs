use crate::ctxt::{self, functions, types::PrimitiveType};

macro_rules! register_function {
    ($function_registry:expr, $name:expr, ( $( $param_name:ident : $param_type:ident ),* ) -> $return_type:expr ) => {
        $function_registry.register_function(functions::FunctionSignature {
            name: $name.to_string(),
            return_type: $return_type,
            parameters: vec![
                $(
                    functions::FunctionParameter {
                        name: stringify!($param_name).to_string(),
                        type_: $param_type,
                    },
                )*
            ],
        })?;
    };
}

pub fn register_functions(
    type_registry: &ctxt::TypeRegistry,
    function_registry: &mut ctxt::FunctionRegistry,
) -> Result<(), ()> {
    use PrimitiveType::*;

    let i32 = type_registry.get_primitive_type_id(Integer32).ok_or(())?;
    let bool = type_registry.get_primitive_type_id(Boolean).ok_or(())?;
    let unit = type_registry.get_primitive_type_id(Unit).ok_or(())?;

    register_function!(function_registry, "add::<i32>", (a: i32, b: i32) -> i32);
    register_function!(function_registry, "sub::<i32>", (a: i32, b: i32) -> i32);
    register_function!(function_registry, "mul::<i32>", (a: i32, b: i32) -> i32);
    register_function!(function_registry, "div::<i32>", (a: i32, b: i32) -> i32);
    register_function!(function_registry, "rem::<i32>", (a: i32, b: i32) -> i32);

    register_function!(function_registry, "eq::<i32>", (a: i32, b: i32) -> bool);
    register_function!(function_registry, "eq::<bool>", (a: bool, b: bool) -> bool);
    register_function!(function_registry, "eq::<()>", (a: unit, b: unit) -> bool);

    Ok(())
}
