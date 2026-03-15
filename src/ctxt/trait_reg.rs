use crate::ctxt::{
    fns::{FnSig, TraitMthdInst, TraitMthdInstError},
    traits::{Trait, TraitDef, TraitInst},
    ty::{GenVar, Ty, TySlice},
};

pub use crate::ctxt::traits::TraitInstError;

#[derive(Default)]
pub struct TraitReg<'traits> {
    _phantom: std::marker::PhantomData<&'traits ()>,
    traits: Vec<TraitDef<'traits>>,
}

impl<'traits> TraitReg<'traits> {
    pub fn inst_trait_mthd(
        &self,
        trait_inst: TraitInst<'traits>,
        mthd_idx: usize,
        impl_ty: Ty<'traits>,
        gen_args: TySlice<'traits>,
    ) -> Result<TraitMthdInst<'traits>, TraitMthdInstError> {
        let trait_def = self.traits.get(trait_inst.trait_.0).unwrap();
        if mthd_idx >= trait_def.mthds.len() {
            return Err(TraitMthdInstError::MthdIdxOutOfRange {
                trait_: trait_inst.trait_,
                mthd_count: trait_def.mthds.len(),
                actual: mthd_idx,
            });
        }
        if trait_def.mthds[mthd_idx].gen_params.len() != gen_args.len() {
            return Err(TraitMthdInstError::GenArgCountMismatch {
                trait_: trait_inst.trait_,
                mthd_idx,
                expected: trait_def.mthds[mthd_idx].gen_params.len(),
                actual: gen_args.len(),
            });
        }
        Ok(TraitMthdInst {
            trait_inst,
            mthd_idx,
            impl_ty,
            gen_args,
            _private: (),
            _phantom: std::marker::PhantomData,
        })
    }

    pub fn inst_trait(&self, trait_: Trait, gen_args: TySlice<'traits>) -> Result<TraitInst<'traits>, TraitInstError> {
        let trait_def = self.traits.get(trait_.0).unwrap();
        if trait_def.gen_params.len() != gen_args.len() {
            return Err(TraitInstError {
                trait_,
                expected: trait_def.gen_params.len(),
                actual: gen_args.len(),
            });
        }
        Ok(TraitInst {
            trait_,
            gen_args,
            _private: (),
            _phantom: std::marker::PhantomData,
        })
    }

    pub fn register_trait(&mut self, name: &str, gen_params: Vec<GenVar<'traits>>) -> Trait {
        let trait_ = Trait(self.traits.len());
        self.traits.push(TraitDef {
            name: name.to_string(),
            gen_params,
            mthds: Vec::new(),
            assoc_tys: Vec::new(),
        });
        trait_
    }

    pub fn register_mthd(&mut self, trait_: Trait, sig: FnSig<'traits>) {
        self.traits[trait_.0].mthds.push(sig);
    }

    pub fn register_assoc_ty(&mut self, trait_: Trait, name: &str) {
        self.traits[trait_.0].assoc_tys.push(name.to_string());
    }

    pub fn resolve_trait_name(&self, trait_name: &str) -> Option<Trait> {
        self.traits
            .iter()
            .position(|trait_| trait_.name == trait_name)
            .map(Trait)
    }

    pub fn get_trait_def(&self, trait_: Trait) -> &TraitDef<'traits> {
        self.traits.get(trait_.0).unwrap()
    }

    pub fn get_trait_name(&self, trait_: Trait) -> &str {
        self.traits.get(trait_.0).unwrap().name.as_str()
    }

    pub fn get_trait_mthd_name(&self, trait_: Trait, mthd_idx: usize) -> &str {
        self.traits.get(trait_.0).unwrap().mthds[mthd_idx].name.as_str()
    }

    pub fn get_trait_mthd_with_name(
        &self,
        mthd_name: &str,
        must_have_receiver: bool,
    ) -> impl Iterator<Item = (Trait, usize)> {
        self.traits
            .iter()
            .enumerate()
            .filter_map(move |(trait_idx, trait_def)| {
                trait_def
                    .mthds
                    .iter()
                    .position(|mthd| mthd.name == mthd_name && (!must_have_receiver || mthd.has_receiver()))
                    .map(|mthd_idx| (Trait(trait_idx), mthd_idx))
            })
    }

    pub fn get_trait_mthd_sig(&self, trait_: Trait, mthd_idx: usize) -> &FnSig<'traits> {
        &self.traits[trait_.0].mthds[mthd_idx]
    }

    pub fn resolve_trait_method(&self, trait_: Trait, ident: &str) -> Option<usize> {
        self.traits
            .get(trait_.0)?
            .mthds
            .iter()
            .position(|mthd| mthd.name == ident)
    }

    pub fn get_trait_assoc_ty_with_name(&self, ident: &str) -> impl Iterator<Item = (Trait, usize)> {
        self.traits
            .iter()
            .enumerate()
            .filter_map(move |(trait_idx, trait_def)| {
                trait_def
                    .assoc_tys
                    .iter()
                    .position(|assoc_ty| assoc_ty == ident)
                    .map(|assoc_ty_idx| (Trait(trait_idx), assoc_ty_idx))
            })
    }

    pub fn resolve_assoc_ty_name(&self, trait_: Trait, name: &str) -> Option<usize> {
        self.traits[trait_.0].assoc_tys.iter().position(|n| n == name)
    }

    pub fn get_trait_assoc_ty_index(&self, trait_: Trait, name: &str) -> usize {
        self.traits[trait_.0]
            .assoc_tys
            .iter()
            .position(|assoc_ty| assoc_ty == name)
            .unwrap()
    }
}
