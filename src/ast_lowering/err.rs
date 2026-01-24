use crate::{
    ctxt::{NotATypeName, ty::Ty},
    ast::Path,
    typechecker::TyError,
};

pub type AstLoweringResult<T> = Result<T, AstLoweringError>;

#[derive(Debug)]
pub enum AstLoweringError {
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
    NotATypeName(String),
    NotAGenericType(Ty),
}

impl<T> From<AstLoweringError> for AstLoweringResult<T> {
    fn from(val: AstLoweringError) -> Self {
        Err(val)
    }
}

impl<E> From<E> for AstLoweringError
where
    TyError: From<E>,
{
    fn from(val: E) -> Self {
        AstLoweringError::TyErr(TyError::from(val))
    }
}

impl From<NotATypeName> for AstLoweringError {
    fn from(val: NotATypeName) -> Self {
        AstLoweringError::NotATypeName(val.0)
    }
}
