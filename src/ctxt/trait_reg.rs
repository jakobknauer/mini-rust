use crate::ctxt::{
    fns::FnSig,
    traits::{Trait, TraitDef},
    ty::GenVar,
};

#[derive(Default)]
pub struct TraitReg {
    traits: Vec<TraitDef>,
}

impl TraitReg {
    pub fn register_trait(&mut self, name: &str, gen_params: Vec<GenVar>) -> Trait {
        let trait_ = Trait(self.traits.len());
        self.traits.push(TraitDef {
            name: name.to_string(),
            gen_params,
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

    pub fn get_trait_method_name(&self, trait_: Trait, method_index: usize) -> &str {
        self.traits.get(trait_.0).unwrap().methods[method_index].name.as_str()
    }

    pub fn get_trait_methods_with_receiver_and_name(&self, method_name: &str) -> impl Iterator<Item = (Trait, usize)> {
        self.traits
            .iter()
            .enumerate()
            .filter_map(move |(trait_idx, trait_def)| {
                trait_def
                    .methods
                    .iter()
                    .position(|method| method.name == method_name && method.has_receiver())
                    .map(|method_idx| (Trait(trait_idx), method_idx))
            })
    }

    pub fn get_trait_method_sig(&self, trait_: Trait, method_idx: usize) -> &FnSig {
        &self.traits[trait_.0].methods[method_idx]
    }
}
