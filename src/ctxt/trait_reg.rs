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

    pub fn resolve_trait_name(&self, trait_name: &str) -> Option<Trait> {
        self.traits
            .iter()
            .position(|trait_| trait_.name == trait_name)
            .map(Trait)
    }

    pub fn get_trait_def(&self, trait_: Trait) -> &TraitDef {
        self.traits.get(trait_.0).unwrap()
    }

    pub fn get_trait_name(&self, trait_: Trait) -> &str {
        self.traits.get(trait_.0).unwrap().name.as_str()
    }

    pub fn get_all_traits(&self) -> Vec<Trait> {
        (0..self.traits.len()).map(Trait).collect()
    }
}
