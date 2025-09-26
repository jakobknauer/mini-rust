use crate::mlr;

use crate::ctxt::types::*;

impl<'a> mlr::MlrBuilder<'a> {
    pub fn infer_type(&mut self, expr: mlr::ExprId) -> mlr::build::Result<TypeId> {
        let expr = self
            .output
            .expressions
            .get(&expr)
            .expect("infer_type should only be called with a valid ExprId");

        match expr {
            mlr::Expression::Block(block) => todo!(),
            mlr::Expression::Constant(constant) => todo!(),
            mlr::Expression::Var(loc_id) => todo!(),
            mlr::Expression::AddressOf(loc_id) => todo!(),
            mlr::Expression::Call { callable, args } => todo!(),
            mlr::Expression::Function(fn_id) => todo!(),
            mlr::Expression::If {
                condition,
                then_block,
                else_block,
            } => todo!(),
            mlr::Expression::Loop { body } => todo!(),
        }
    }
}
