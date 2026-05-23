use crate::ctxt::{fns::Fn, traits::Trait};

#[derive(Default)]
pub struct LanguageItems<'ctxt> {
    pub size_of: Option<Fn<'ctxt>>,
    pub ptr_offset: Option<Fn<'ctxt>>,
    pub panic_fn: Option<Fn<'ctxt>>,
    pub add_trait: Option<Trait<'ctxt>>,
    pub sub_trait: Option<Trait<'ctxt>>,
    pub mul_trait: Option<Trait<'ctxt>>,
    pub div_trait: Option<Trait<'ctxt>>,
    pub bit_or_trait: Option<Trait<'ctxt>>,
    pub bit_and_trait: Option<Trait<'ctxt>>,
    pub rem_trait: Option<Trait<'ctxt>>,
    pub deref_trait: Option<Trait<'ctxt>>,
    pub eq_trait: Option<Trait<'ctxt>>,
    pub into_iterator_trait: Option<Trait<'ctxt>>,
    pub iterator_trait: Option<Trait<'ctxt>>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum UnaryPrimOp {
    NegInt,
    NegF64,
    NotBool,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BinaryPrimOp {
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    RemInt,
    EqInt,
    NeInt,
    EqBool,
    NeBool,
    EqCChar,
    NeCChar,
    EqUnit,
    NeUnit,
    BitOrBool,
    BitAndBool,
    BitOrInt,
    BitAndInt,
    LtInt,
    GtInt,
    LeInt,
    GeInt,
    AddF64,
    SubF64,
    MulF64,
    DivF64,
    RemF64,
    EqF64,
    NeF64,
    LtF64,
    GtF64,
    LeF64,
    GeF64,
}
