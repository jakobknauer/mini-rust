use crate::mlr::{self, builder::MlrBuilder};

pub(super) struct LoweredExpr<'ctxt>(LoweredExprKind<'ctxt>);

enum LoweredExprKind<'ctxt> {
    Val(mlr::Val<'ctxt>),
    Place(mlr::Place<'ctxt>),
    Op(mlr::Op<'ctxt>),
}

impl<'ctxt> From<mlr::Val<'ctxt>> for LoweredExpr<'ctxt> {
    fn from(val: mlr::Val<'ctxt>) -> Self {
        LoweredExpr(LoweredExprKind::Val(val))
    }
}

impl<'ctxt> From<mlr::Place<'ctxt>> for LoweredExpr<'ctxt> {
    fn from(place: mlr::Place<'ctxt>) -> Self {
        LoweredExpr(LoweredExprKind::Place(place))
    }
}

impl<'ctxt> From<mlr::Op<'ctxt>> for LoweredExpr<'ctxt> {
    fn from(op: mlr::Op<'ctxt>) -> Self {
        LoweredExpr(LoweredExprKind::Op(op))
    }
}

impl<'ctxt> LoweredExpr<'ctxt> {
    pub(super) fn into_val(self, builder: &mut MlrBuilder<'_, 'ctxt>) -> mlr::Val<'ctxt> {
        match self.0 {
            LoweredExprKind::Val(val) => val,
            LoweredExprKind::Place(place) => builder.copy_val(place),
            LoweredExprKind::Op(op) => builder.insert_use_val(op),
        }
    }

    pub(super) fn into_place(self, builder: &mut MlrBuilder<'_, 'ctxt>) -> mlr::Place<'ctxt> {
        match self.0 {
            LoweredExprKind::Place(place) => place,
            LoweredExprKind::Val(val) => builder.store_val(val),
            LoweredExprKind::Op(op) => {
                let val = builder.insert_use_val(op);
                builder.store_val(val)
            }
        }
    }

    pub(super) fn into_op(self, builder: &mut MlrBuilder<'_, 'ctxt>) -> mlr::Op<'ctxt> {
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
