use crate::typechecker::TyErr;

#[derive(Debug)]
pub enum Hlr2MlrErr {
    MissingOperatorImpl { name: String },
    UnresolvableSymbol { name: String },
    UnknownPrimitiveTy,
    NotAPlace,
    TyErr(TyErr),
}

pub type Result<T> = std::result::Result<T, Hlr2MlrErr>;

impl<T> From<Hlr2MlrErr> for Result<T> {
    fn from(val: Hlr2MlrErr) -> Self {
        Err(val)
    }
}

impl From<TyErr> for Hlr2MlrErr {
    fn from(val: TyErr) -> Self {
        Hlr2MlrErr::TyErr(val)
    }
}
