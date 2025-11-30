use crate::{ctxt::ty::Ty, typechecker::TyErr};

pub type H2MResult<T> = Result<T, H2MErr>;

#[derive(Debug)]
pub enum H2MErr {
    MissingOperatorImpl { name: String },
    UnresolvableSymbol { name: String },
    NotAPlace,
    TyErr(TyErr),
    OperatorResolutionFailed { operator: String, operand_tys: (Ty, Ty) },
    UnresolvableStructOrEnum { ty_name: String },
    UnresolvableTyAnnot,
}

impl<T> From<H2MErr> for H2MResult<T> {
    fn from(val: H2MErr) -> Self {
        Err(val)
    }
}

impl<E> From<E> for H2MErr
where
    TyErr: From<E>,
{
    fn from(val: E) -> Self {
        H2MErr::TyErr(TyErr::from(val))
    }
}
