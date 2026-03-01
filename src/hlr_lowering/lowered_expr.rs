use crate::mlr::{self, builder::MlrBuilder};

pub(super) struct LoweredExpr<'mlr>(LoweredExprKind<'mlr>);

enum LoweredExprKind<'mlr> {
    Val(mlr::Val<'mlr>),
    Place(mlr::Place<'mlr>),
    Op(mlr::Op<'mlr>),
}

impl<'mlr> From<mlr::Val<'mlr>> for LoweredExpr<'mlr> {
    fn from(val: mlr::Val<'mlr>) -> Self {
        LoweredExpr(LoweredExprKind::Val(val))
    }
}

impl<'mlr> From<mlr::Place<'mlr>> for LoweredExpr<'mlr> {
    fn from(place: mlr::Place<'mlr>) -> Self {
        LoweredExpr(LoweredExprKind::Place(place))
    }
}

impl<'mlr> From<mlr::Op<'mlr>> for LoweredExpr<'mlr> {
    fn from(op: mlr::Op<'mlr>) -> Self {
        LoweredExpr(LoweredExprKind::Op(op))
    }
}

impl<'mlr> LoweredExpr<'mlr> {
    pub(super) fn into_val(self, builder: &mut MlrBuilder<'_, 'mlr>) -> mlr::Val<'mlr> {
        match self.0 {
            LoweredExprKind::Val(val) => val,
            LoweredExprKind::Place(place) => builder.copy_val(place),
            LoweredExprKind::Op(op) => builder.insert_use_val(op),
        }
    }

    pub(super) fn into_place(self, builder: &mut MlrBuilder<'_, 'mlr>) -> mlr::Place<'mlr> {
        match self.0 {
            LoweredExprKind::Place(place) => place,
            LoweredExprKind::Val(val) => builder.store_val(val),
            LoweredExprKind::Op(op) => {
                let val = builder.insert_use_val(op);
                builder.store_val(val)
            }
        }
    }

    pub(super) fn into_op(self, builder: &mut MlrBuilder<'_, 'mlr>) -> mlr::Op<'mlr> {
        match self.0 {
            LoweredExprKind::Op(op) => op,
            LoweredExprKind::Place(place) => builder.insert_copy_op(place),
            LoweredExprKind::Val(val) => {
                let place = builder.store_val(val);
                builder.insert_copy_op(place)
            }
        }
    }
}
