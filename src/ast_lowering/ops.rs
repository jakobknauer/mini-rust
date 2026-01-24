use crate::{
    ast,
    ast_lowering::{self, AstLoweringError, AstLoweringResult},
    ctxt::{fns, ty},
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
                return AstLoweringError::OperatorResolutionFailed {
                    operator: format!("{:?}", $operator),
                    operand_tys: ($left, $right),
                }
                .into();
            }
        }
    }};
}

impl<'a> ast_lowering::AstLowerer<'a> {
    pub fn resolve_binary_operator(
        &mut self,
        operator: &ast::BinaryOperator,
        (left, right): (ty::Ty, ty::Ty),
    ) -> AstLoweringResult<fns::Fn> {
        use ast::BinaryOperator::*;

        let tys = &mut self.tys();

        let i32 = tys.primitive(ty::Primitive::Integer32);
        let bool = tys.primitive(ty::Primitive::Boolean);
        let unit = tys.unit();

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

        self.fns()
            .get_fn_by_name(fn_name)
            .ok_or(AstLoweringError::MissingOperatorImpl {
                name: fn_name.to_string(),
            })
    }

    pub fn resolve_unary_operator(
        &mut self,
        operator: &ast::UnaryOperator,
        operand: ty::Ty,
    ) -> AstLoweringResult<fns::Fn> {
        use ast::UnaryOperator::*;

        let tys = &mut self.tys();

        let i32 = tys.primitive(ty::Primitive::Integer32);
        let bool = tys.primitive(ty::Primitive::Boolean);

        let operand = tys.canonicalize(operand);

        let fn_name = match operator {
            Negative if operand == i32 => "neg::<i32>",
            Not if operand == bool => "not::<bool>",
            _ => {
                return AstLoweringError::OperatorResolutionFailed {
                    operator: format!("{:?}", operator),
                    operand_tys: (operand, operand),
                }
                .into();
            }
        };

        self.fns()
            .get_fn_by_name(fn_name)
            .ok_or(AstLoweringError::MissingOperatorImpl {
                name: fn_name.to_string(),
            })
    }
}
