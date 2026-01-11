use crate::ctxt::fns::Fn;

#[derive(Default)]
pub struct LanguageItems {
    pub size_of: Option<Fn>,
    pub ptr_offset: Option<Fn>,
}
