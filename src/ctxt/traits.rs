use crate::ctxt::{
    fns::FnSig,
    ty::{GenVar, TySlice},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Trait(pub(in crate::ctxt) usize);

#[derive(Clone)]
pub struct TraitDef<'traits> {
    pub name: String,
    pub gen_params: Vec<GenVar<'traits>>,
    pub mthds: Vec<FnSig<'traits>>,
    pub assoc_tys: Vec<String>,
}

#[derive(Debug)]
pub struct TraitInstError {
    #[allow(unused)]
    pub trait_: Trait,
    #[allow(unused)]
    pub expected: usize,
    #[allow(unused)]
    pub actual: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TraitInst<'traits> {
    pub trait_: Trait,
    pub gen_args: TySlice<'traits>,
    pub(in crate::ctxt) _private: (),
    pub(in crate::ctxt) _phantom: std::marker::PhantomData<&'traits ()>,
}

impl<'traits> TraitInst<'traits> {
    pub fn with_gen_args(self, gen_args: TySlice<'traits>) -> Result<TraitInst<'traits>, TraitInstError> {
        if gen_args.len() != self.gen_args.len() {
            return Err(TraitInstError {
                trait_: self.trait_,
                expected: self.gen_args.len(),
                actual: gen_args.len(),
            });
        }
        Ok(TraitInst {
            trait_: self.trait_,
            gen_args,
            _private: (),
            _phantom: std::marker::PhantomData,
        })
    }
}
