use crate::{
    ctxt::{functions::FnId, types::*},
    hlr,
    mlr::{self, MlrBuilderError, TypeError},
};

impl<'a> mlr::MlrBuilder<'a> {
    pub fn resolve_operator(
        &self,
        operator: &hlr::BinaryOperator,
        (left, right): (TypeId, TypeId),
    ) -> Result<FnId, mlr::build::MlrBuilderError> {
        use hlr::BinaryOperator::*;

        let i32_t = self
            .ctxt
            .type_registry
            .get_primitive_type_id(PrimitiveType::Integer32)
            .ok_or(MlrBuilderError::UnknownPrimitiveType)?;
        let bool_t = self
            .ctxt
            .type_registry
            .get_primitive_type_id(PrimitiveType::Boolean)
            .ok_or(MlrBuilderError::UnknownPrimitiveType)?;
        let unit_t = self
            .ctxt
            .type_registry
            .get_primitive_type_id(PrimitiveType::Unit)
            .ok_or(MlrBuilderError::UnknownPrimitiveType)?;

        let fn_name = match (operator, left, right) {
            (Add, l, r) if l == i32_t && r == i32_t => "add::<i32>",
            (Multiply, l, r) if l == i32_t && r == i32_t => "mul::<i32>",
            (Equal, l, r) if l == i32_t && r == i32_t => "eq::<i32>",
            (Equal, l, r) if l == bool_t && r == bool_t => "eq::<bool>",
            (Equal, l, r) if l == unit_t && r == unit_t => "eq::<()>",
            _ => {
                return TypeError::OperatorResolutionFailed {
                    operator: format!("{operator:?}"),
                    operand_types: (left, right),
                }
                .into();
            }
        };

        self.ctxt
            .function_registry
            .get_function_by_name(fn_name)
            .ok_or(mlr::MlrBuilderError::MissingOperatorImpl {
                name: fn_name.to_string(),
            })
    }
}
