use crate::ctxt::{traits, ty};

pub type TyAnnot<'hlr> = &'hlr TyAnnotDef<'hlr>;
pub type TyAnnotSlice<'hlr> = &'hlr [TyAnnot<'hlr>];

#[derive(Debug)]
pub enum TyAnnotDef<'hlr> {
    Struct(ty::Struct, Option<TyAnnotSlice<'hlr>>),
    Enum(ty::Enum, Option<TyAnnotSlice<'hlr>>),
    Ty(ty::Ty),
    GenVar(ty::GenVar),

    AssocTy {
        base: TyAnnot<'hlr>,
        trait_: Option<(traits::Trait, Option<TyAnnotSlice<'hlr>>)>,
        name: String,
    },

    Ref(TyAnnot<'hlr>),
    Ptr(TyAnnot<'hlr>),

    Fn {
        params: TyAnnotSlice<'hlr>,
        ret: Option<TyAnnot<'hlr>>,
    },

    Tuple(TyAnnotSlice<'hlr>),

    Infer,
    Self_,
}
