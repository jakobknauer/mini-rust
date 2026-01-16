use crate::{ctxt::ty::Ty, hlr::Path, typechecker::TyError};

pub type H2MResult<T> = Result<T, H2MError>;

#[derive(Debug)]
pub enum H2MError {
    MissingOperatorImpl { name: String },
    UnresolvableSymbol { name: String },
    NotAPlace,
    TyErr(TyError),
    OperatorResolutionFailed { operator: String, operand_tys: (Ty, Ty) },
    UnresolvableStructOrEnum { path: Path },
    UnresolvablePath { path: Path },
    UnresolvableTyAnnot,
    VarArgsNotSupported,
    NonMatchableScrutinee { ty: Ty },
    NoSelfOutsideOfMethod,
    UnresolvableTraitAnnot { trait_name: String },
}

impl<T> From<H2MError> for H2MResult<T> {
    fn from(val: H2MError) -> Self {
        Err(val)
    }
}

impl<E> From<E> for H2MError
where
    TyError: From<E>,
{
    fn from(val: E) -> Self {
        H2MError::TyErr(TyError::from(val))
    }
}
