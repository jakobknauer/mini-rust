use crate::typechecker::TyErr;

#[derive(Debug)]
pub enum H2MErr {
    MissingOperatorImpl { name: String },
    UnresolvableSymbol { name: String },
    UnknownPrimitiveTy,
    NotAPlace,
    TyErr(TyErr),
}

pub type Result<T> = std::result::Result<T, H2MErr>;

impl<T> From<H2MErr> for Result<T> {
    fn from(val: H2MErr) -> Self {
        Err(val)
    }
}

impl From<TyErr> for H2MErr {
    fn from(val: TyErr) -> Self {
        H2MErr::TyErr(val)
    }
}
