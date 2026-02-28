use crate::{ctxt::mlr, ctxt::mlr::builder::MlrBuilder};

pub(super) struct LoweredExpr(LoweredExprKind);

enum LoweredExprKind {
    Val(mlr::Val),
    Place(mlr::Place),
    Op(mlr::Op),
}

impl From<mlr::Val> for LoweredExpr {
    fn from(v: mlr::Val) -> Self {
        LoweredExpr(LoweredExprKind::Val(v))
    }
}

impl From<mlr::Place> for LoweredExpr {
    fn from(p: mlr::Place) -> Self {
        LoweredExpr(LoweredExprKind::Place(p))
    }
}

impl From<mlr::Op> for LoweredExpr {
    fn from(op: mlr::Op) -> Self {
        LoweredExpr(LoweredExprKind::Op(op))
    }
}

impl LoweredExpr {
    pub(super) fn into_val(self, b: &mut MlrBuilder<'_>) -> mlr::Val {
        match self.0 {
            LoweredExprKind::Val(v) => v,
            LoweredExprKind::Place(p) => {
                let op = b.insert_copy_op(p);
                b.insert_use_val(op)
            }
            LoweredExprKind::Op(op) => b.insert_use_val(op),
        }
    }

    pub(super) fn into_place(self, b: &mut MlrBuilder<'_>) -> mlr::Place {
        match self.0 {
            LoweredExprKind::Place(p) => p,
            LoweredExprKind::Val(v) => {
                let ty = b.get_val_ty(v);
                let loc = b.insert_typed_loc(ty);
                b.insert_alloc_stmt(loc);
                let place = b.insert_loc_place(loc);
                b.insert_assign_stmt(place, v);
                place
            }
            LoweredExprKind::Op(op) => {
                let v = b.insert_use_val(op);
                let ty = b.get_val_ty(v);
                let loc = b.insert_typed_loc(ty);
                b.insert_alloc_stmt(loc);
                let place = b.insert_loc_place(loc);
                b.insert_assign_stmt(place, v);
                place
            }
        }
    }

    pub(super) fn into_op(self, b: &mut MlrBuilder<'_>) -> mlr::Op {
        match self.0 {
            LoweredExprKind::Op(op) => op,
            LoweredExprKind::Place(p) => b.insert_copy_op(p),
            LoweredExprKind::Val(v) => {
                let ty = b.get_val_ty(v);
                let loc = b.insert_typed_loc(ty);
                b.insert_alloc_stmt(loc);
                let place = b.insert_loc_place(loc);
                b.insert_assign_stmt(place, v);
                b.insert_copy_op(place)
            }
        }
    }
}
