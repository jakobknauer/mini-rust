use crate::{
    ctxt::{fns, ty},
    h2m::{self, H2MError, H2MResult},
    hlr,
};

macro_rules! op_match {
    (
        $ty_reg:expr, $operator:expr,  $left:expr, $right:expr,
        $(
            ($op:pat, $left_ty:ident, $right_ty:ident) => $fn_name:expr
        ),* $(,)?
    ) => {{
        let ty_reg = $ty_reg;
        let left = ty_reg.canonicalize($left);
        let right = ty_reg.canonicalize($right);

        match ($operator, left, right) {
            $(
                ($op, l, r) if l == $left_ty && r == $right_ty => $fn_name,
            )*
            _ => {
                return H2MError::OperatorResolutionFailed {
                    operator: format!("{{$operator:?}}"),
                    operand_tys: ($left, $right),
                }
                .into();
            }
        }
    }};
}

impl<'a> h2m::H2M<'a> {
    pub fn resolve_operator(
        &mut self,
        operator: &hlr::BinaryOperator,
        (left, right): (ty::Ty, ty::Ty),
    ) -> H2MResult<fns::Fn> {
        use hlr::BinaryOperator::*;

        let tys = &self.tys();

        let i32 = tys.get_primitive_ty(ty::Primitive::Integer32);
        let bool = tys.get_primitive_ty(ty::Primitive::Boolean);
        let unit = tys.get_primitive_ty(ty::Primitive::Unit);

        let fn_name = op_match!(tys, operator, left, right,
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

        self.fns().get_fn_by_name(fn_name).ok_or(H2MError::MissingOperatorImpl {
            name: fn_name.to_string(),
        })
    }
}
