use crate::{ctxt::mlr, ctxt::mlr::builder::MlrBuilder};

pub(super) struct LoweredExpr(LoweredExprKind);

enum LoweredExprKind {
    Val(mlr::Val),
    Place(mlr::Place),
    Op(mlr::Op),
}

impl From<mlr::Val> for LoweredExpr {
    fn from(val: mlr::Val) -> Self {
        LoweredExpr(LoweredExprKind::Val(val))
    }
}

impl From<mlr::Place> for LoweredExpr {
    fn from(place: mlr::Place) -> Self {
        LoweredExpr(LoweredExprKind::Place(place))
    }
}

impl From<mlr::Op> for LoweredExpr {
    fn from(op: mlr::Op) -> Self {
        LoweredExpr(LoweredExprKind::Op(op))
    }
}

impl LoweredExpr {
    pub(super) fn into_val(self, builder: &mut MlrBuilder<'_>) -> mlr::Val {
        match self.0 {
            LoweredExprKind::Val(val) => val,
            LoweredExprKind::Place(place) => builder.copy_val(place),
            LoweredExprKind::Op(op) => builder.insert_use_val(op),
        }
    }

    pub(super) fn into_place(self, builder: &mut MlrBuilder<'_>) -> mlr::Place {
        match self.0 {
            LoweredExprKind::Place(place) => place,
            LoweredExprKind::Val(val) => builder.store_val(val),
            LoweredExprKind::Op(op) => {
                let val = builder.insert_use_val(op);
                builder.store_val(val)
            }
        }
    }

    pub(super) fn into_op(self, builder: &mut MlrBuilder<'_>) -> mlr::Op {
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
