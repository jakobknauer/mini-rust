use crate::ctxt::{fns::Fn, traits::Trait, ty::Struct};

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
    pub bit_xor_trait: Option<Trait<'ctxt>>,
    pub rem_trait: Option<Trait<'ctxt>>,
    pub neg_trait: Option<Trait<'ctxt>>,
    pub not_trait: Option<Trait<'ctxt>>,
    pub deref_trait: Option<Trait<'ctxt>>,
    pub eq_trait: Option<Trait<'ctxt>>,
    pub into_iterator_trait: Option<Trait<'ctxt>>,
    pub iterator_trait: Option<Trait<'ctxt>>,
    pub range_struct: Option<Struct<'ctxt>>,
    pub inclusive_range_struct: Option<Struct<'ctxt>>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum UnaryPrimOp {
    NegInt,
    NegFloat,
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
    BitXorBool,
    BitOrInt,
    BitAndInt,
    BitXorInt,
    LtInt,
    GtInt,
    LeInt,
    GeInt,
    AddFloat,
    SubFloat,
    MulFloat,
    DivFloat,
    RemFloat,
    EqFloat,
    NeFloat,
    LtFloat,
    GtFloat,
    LeFloat,
    GeFloat,
}
