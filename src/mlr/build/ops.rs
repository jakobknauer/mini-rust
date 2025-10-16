use crate::{
    ctxt::{functions::FnId, types::*},
    hlr,
    mlr::{self, MlrBuilderError, TypeError},
};

macro_rules! op_match {
    (
        $operator:expr, $left:expr, $right:expr,
        $(
            ($op:pat, $left_ty:ident, $right_ty:ident) => $fn_name:expr
        ),* $(,)?
    ) => {
        match ($operator, $left, $right) {
            $(
                ($op, l, r) if l == $left_ty && r == $right_ty => $fn_name,
            )*
            _ => {
                return TypeError::OperatorResolutionFailed {
                    operator: format!("{{$operator:?}}"),
                    operand_types: ($left, $right),
                }
                .into();
            }
        }
    };
}

impl<'a> mlr::MlrBuilder<'a> {
    pub fn resolve_operator(
        &self,
        operator: &hlr::BinaryOperator,
        (left, right): (TypeId, TypeId),
    ) -> Result<FnId, mlr::build::MlrBuilderError> {
        use MlrBuilderError::UnknownPrimitiveType;
        use PrimitiveType::*;
        use hlr::BinaryOperator::*;

        let tr = &self.ctxt.type_registry;
        let i32 = tr.get_primitive_type_id(Integer32).ok_or(UnknownPrimitiveType)?;
        let bool = tr.get_primitive_type_id(Boolean).ok_or(UnknownPrimitiveType)?;
        let unit = tr.get_primitive_type_id(Unit).ok_or(UnknownPrimitiveType)?;

        let fn_name = op_match!(operator, left, right,
            (Add,       i32, i32) => "add::<i32>",
            (Subtract,  i32, i32) => "sub::<i32>",
            (Multiply,  i32, i32) => "mul::<i32>",
            (Divide,    i32, i32) => "div::<i32>",
            (Remainder, i32, i32) => "rem::<i32>",

            (Equal,     i32, i32) => "eq::<i32>",
            (NotEqual,  i32, i32) => "ne::<i32>",
            (Equal,     bool, bool) => "eq::<bool>",
            (NotEqual,  bool, bool) => "ne::<bool>",
            (Equal,     unit, unit) => "eq::<()>",
            (NotEqual,  unit, unit) => "ne::<()>",

            (BitOr,     bool, bool) => "bitor::<bool>",
            (BitAnd,    bool, bool) => "bitand::<bool>",

            (LessThan,           i32, i32) => "lt::<i32>",
            (GreaterThan,        i32, i32) => "gt::<i32>",
            (LessThanOrEqual,    i32, i32) => "le::<i32>",
            (GreaterThanOrEqual, i32, i32) => "ge::<i32>",
        );

        self.ctxt
            .function_registry
            .get_function_by_name(fn_name)
            .ok_or(MlrBuilderError::MissingOperatorImpl {
                name: fn_name.to_string(),
            })
    }
}
