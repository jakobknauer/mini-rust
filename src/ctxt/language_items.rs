use crate::ctxt::{fns::Fn, traits::Trait};

#[derive(Default)]
pub struct LanguageItems<'ctxt> {
    pub size_of: Option<Fn<'ctxt>>,
    pub ptr_offset: Option<Fn<'ctxt>>,
    pub add_trait: Option<Trait<'ctxt>>,
    pub sub_trait: Option<Trait<'ctxt>>,
    pub mul_trait: Option<Trait<'ctxt>>,
    pub div_trait: Option<Trait<'ctxt>>,
    pub bit_or_trait: Option<Trait<'ctxt>>,
    pub bit_and_trait: Option<Trait<'ctxt>>,
    pub rem_trait: Option<Trait<'ctxt>>,
    pub deref_trait: Option<Trait<'ctxt>>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum UnaryPrimOp {
    NegI32,
    NotBool,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BinaryPrimOp {
    AddI32,
    SubI32,
    MulI32,
    DivI32,
    RemI32,
    EqI32,
    NeI32,
    EqBool,
    NeBool,
    EqUnit,
    NeUnit,
    BitOrBool,
    BitAndBool,
    BitOrI32,
    BitAndI32,
    LtI32,
    GtI32,
    LeI32,
    GeI32,
}
