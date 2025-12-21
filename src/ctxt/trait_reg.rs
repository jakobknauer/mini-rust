use crate::ctxt::{
    fns::FnSig,
    traits::{Trait, TraitDef},
};

#[derive(Default)]
pub struct TraitReg {
    traits: Vec<TraitDef>,
}

impl TraitReg {
    pub fn register_trait(&mut self, name: &str) -> Trait {
        let trait_ = Trait(self.traits.len());
        self.traits.push(TraitDef {
            name: name.to_string(),
            methods: Vec::new(),
        });
        trait_
    }

    pub fn register_method(&mut self, trait_: Trait, sig: FnSig) {
        self.traits[trait_.0].methods.push(sig);
    }
}
