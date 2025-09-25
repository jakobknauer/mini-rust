mod function_registry;
pub mod functions;
mod type_registry;
pub mod types;

pub use function_registry::FunctionRegistry;
pub use type_registry::TypeRegistry;

pub struct Ctxt {
    pub type_registry: TypeRegistry,
    pub function_registry: FunctionRegistry,
}

impl Ctxt {
    pub fn new() -> Self {
        Self {
            type_registry: TypeRegistry::new(),
            function_registry: FunctionRegistry::new(),
        }
    }
}
