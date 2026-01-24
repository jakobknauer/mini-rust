use crate::{ctxt::mlr, ast_lowering::AstLoweringResult, util::mlr_builder::MlrBuilder};

pub enum Lowered {
    Op(mlr::Op),
    Val(mlr::Val),
    Place(mlr::Place),
}

impl Lowered {
    pub fn into_op(self, mlr_builder: &mut MlrBuilder) -> AstLoweringResult<mlr::Op> {
        match self {
            Lowered::Op(op) => Ok(op),
            Lowered::Val(val) => {
                let place = mlr_builder.insert_fresh_alloc()?;
                mlr_builder.insert_assign_stmt(place, val)?;
                mlr_builder.insert_copy_op(place)
            }
            Lowered::Place(place) => mlr_builder.insert_copy_op(place),
        }
    }

    pub fn into_val(self, mlr_builder: &mut MlrBuilder) -> AstLoweringResult<mlr::Val> {
        match self {
            Lowered::Op(op) => mlr_builder.insert_use_val(op),
            Lowered::Val(val) => Ok(val),
            Lowered::Place(place) => mlr_builder.insert_use_place_val(place),
        }
    }

    pub fn into_place(self, mlr_builder: &mut MlrBuilder) -> AstLoweringResult<mlr::Place> {
        match self {
            Lowered::Op(op) => {
                let place = mlr_builder.insert_fresh_alloc()?;
                let val = mlr_builder.insert_use_val(op)?;
                mlr_builder.insert_assign_stmt(place, val)?;
                Ok(place)
            }
            Lowered::Val(val) => {
                let place = mlr_builder.insert_fresh_alloc()?;
                mlr_builder.insert_assign_stmt(place, val)?;
                Ok(place)
            }
            Lowered::Place(place) => Ok(place),
        }
    }
}

impl From<mlr::Op> for Lowered {
    fn from(op: mlr::Op) -> Self {
        Lowered::Op(op)
    }
}

impl From<mlr::Val> for Lowered {
    fn from(val: mlr::Val) -> Self {
        Lowered::Val(val)
    }
}

impl From<mlr::Place> for Lowered {
    fn from(place: mlr::Place) -> Self {
        Lowered::Place(place)
    }
}
