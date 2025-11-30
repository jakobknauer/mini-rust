use crate::{
    ctxt::{NotAStructErr, NotAnEnumErr, ty::Ty},
    typechecker::TyErr,
};

#[derive(Debug)]
pub enum H2MErr {
    MissingOperatorImpl { name: String },
    UnresolvableSymbol { name: String },
    UnknownPrimitiveTy,
    NotAPlace,
    TyErr(TyErr),
    OperatorResolutionFailed { operator: String, operand_tys: (Ty, Ty) },
    UnresolvableStructOrEnum { ty_name: String },
    UnresolvableTyAnnot,
}

pub type H2MResult<T> = std::result::Result<T, H2MErr>;

impl<T> From<H2MErr> for H2MResult<T> {
    fn from(val: H2MErr) -> Self {
        Err(val)
    }
}

impl From<TyErr> for H2MErr {
    fn from(val: TyErr) -> Self {
        H2MErr::TyErr(val)
    }
}

impl From<NotAStructErr> for H2MErr {
    fn from(val: NotAStructErr) -> Self {
        H2MErr::TyErr(TyErr::NotAStruct { ty: val.ty })
    }
}

impl From<NotAnEnumErr> for H2MErr {
    fn from(val: NotAnEnumErr) -> Self {
        H2MErr::TyErr(TyErr::NotAnEnum { ty: val.ty })
    }
}

pub fn into_h2m_err<E>(err: E) -> H2MErr
where
    H2MErr: From<E>,
{
    From::from(err)
}
