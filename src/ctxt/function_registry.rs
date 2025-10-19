use std::collections::{BTreeMap, HashMap};

use crate::{
    ctxt::functions::{FnId, FunctionSignature},
    mlr::Mlr,
};

pub struct FunctionRegistry {
    function_names: BTreeMap<String, FnId>,
    signatures: HashMap<FnId, FunctionSignature>,
    next_function_id: FnId,
    defs: HashMap<FnId, Mlr>,
}

impl FunctionRegistry {
    pub fn new() -> FunctionRegistry {
        FunctionRegistry {
            function_names: BTreeMap::new(),
            signatures: HashMap::new(),
            next_function_id: FnId(0),
            defs: HashMap::new(),
        }
    }

    pub fn register_function(&mut self, signature: FunctionSignature) -> Result<FnId, ()> {
        if self.function_names.contains_key(&signature.name) {
            return Err(());
        }

        let fn_id = self.next_function_id;
        self.next_function_id.0 += 1;

        self.function_names.insert(signature.name.to_string(), fn_id);
        self.signatures.insert(fn_id, signature);

        Ok(fn_id)
    }

    pub fn get_signature_by_id(&self, fn_id: &FnId) -> Option<&FunctionSignature> {
        self.signatures.get(fn_id)
    }

    pub fn get_function_name_by_id(&self, fn_id: &FnId) -> Option<&str> {
        self.signatures.get(fn_id).map(|sig| sig.name.as_str())
    }

    pub fn get_function_by_name(&self, name: &str) -> Option<FnId> {
        self.function_names.get(name).cloned()
    }

    pub fn add_function_def(&mut self, name: &str, mlr: Mlr) {
        if let Some(&fn_id) = self.function_names.get(name) {
            self.defs.insert(fn_id, mlr);
        }
    }

    pub fn is_function_defined(&self, fn_id: &FnId) -> bool {
        self.defs.contains_key(fn_id)
    }

    pub fn get_function_mlr(&self, fn_id: &FnId) -> Option<&Mlr> {
        self.defs.get(fn_id)
    }

    pub fn get_all_functions(&self) -> impl Iterator<Item = &FnId> {
        self.function_names.values()
    }
}
