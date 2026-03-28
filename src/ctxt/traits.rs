use crate::ctxt::{
    fns::Fn,
    ty::{GenVar, TySlice},
};

pub type Trait<'traits> = &'traits TraitDef<'traits>;
pub type TraitMthd<'traits> = &'traits TraitMthdDef<'traits>;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Default)]
pub struct TraitId(pub(in crate::ctxt) usize);

#[derive(Debug)]
pub struct TraitDef<'traits> {
    pub(in crate::ctxt) id: TraitId,
    pub name: String,
    pub gen_params: Vec<GenVar<'traits>>,
    pub assoc_tys: Vec<String>,
}

impl PartialEq for TraitDef<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for TraitDef<'_> {}

impl std::hash::Hash for TraitDef<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Debug)]
pub struct TraitInstError<'traits> {
    #[allow(unused)]
    pub trait_: Trait<'traits>,
    #[allow(unused)]
    pub expected: usize,
    #[allow(unused)]
    pub actual: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct TraitInst<'traits> {
    pub trait_: Trait<'traits>,
    pub gen_args: TySlice<'traits>,
    pub(in crate::ctxt) _private: (),
}

pub struct TraitMthdDef<'traits> {
    pub trait_: Trait<'traits>,
    pub fn_: Fn<'traits>,
    pub has_default_body: bool,
}

impl std::fmt::Debug for TraitMthdDef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TraitMthdDef({}::{})", self.trait_.name, self.fn_.name)
    }
}

impl PartialEq for TraitMthdDef<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for TraitMthdDef<'_> {}

impl std::hash::Hash for TraitMthdDef<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(self, state);
    }
}

impl<'traits> TraitInst<'traits> {
    pub fn with_gen_args(self, gen_args: TySlice<'traits>) -> Result<TraitInst<'traits>, TraitInstError<'traits>> {
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
        })
    }
}
