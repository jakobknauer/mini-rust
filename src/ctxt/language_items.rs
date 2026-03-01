use crate::ctxt::fns::Fn;

#[derive(Default)]
pub struct LanguageItems {
    pub size_of: Option<Fn>,
    pub ptr_offset: Option<Fn>,
}

#[derive(Debug, Clone, Copy)]
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
    LtI32,
    GtI32,
    LeI32,
    GeI32,
}
