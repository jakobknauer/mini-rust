use std::collections::HashMap;

use crate::context::functions::{FnId, FunctionSignature};

pub struct FunctionRegistry {
    function_names: HashMap<String, FnId>,
    signatures: HashMap<FnId, FunctionSignature>,
    next_function_id: FnId,
    mlrs: HashMap<FnId, crate::mlr::Mlr>,
}

impl FunctionRegistry {
    pub fn new() -> FunctionRegistry {
        FunctionRegistry {
            function_names: HashMap::new(),
            signatures: HashMap::new(),
            next_function_id: FnId(0),
            mlrs: HashMap::new(),
        }
    }

    pub fn register_function(&mut self, name: &str, signature: FunctionSignature) -> Result<FnId, ()> {
        if self.function_names.contains_key(name) {
            return Err(());
        }

        let fn_id = self.next_function_id;
        self.next_function_id.0 += 1;

        self.function_names.insert(name.to_string(), fn_id);
        self.signatures.insert(fn_id, signature);

        Ok(fn_id)
    }

    pub fn get_function_by_name(&self, name: &str) -> Option<FnId> {
        self.function_names.get(name).cloned()
    }

    pub fn register_function_mlr(&mut self, name: &str, mlr: crate::mlr::Mlr) {
        if let Some(&fn_id) = self.function_names.get(name) {
            self.mlrs.insert(fn_id, mlr);
        }
    }
}
