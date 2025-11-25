use crate::ctxt::ty::Ty;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Fn(pub usize);

#[derive(Clone)]
pub struct FnSig {
    pub name: String,
    pub gen_params: Vec<GenParam>,
    pub params: Vec<FnParam>,
    pub return_ty: Ty,
}

impl FnSig {
    pub fn build_substitutions(&self, gen_args: &[Ty]) -> std::collections::HashMap<&str, Ty> {
        self.gen_params
            .iter()
            .zip(gen_args)
            .map(|(gen_param, gen_arg)| (gen_param.name.as_str(), *gen_arg))
            .collect()
    }
}

#[derive(Clone)]
pub struct GenParam {
    pub name: String,
    pub ty: Ty,
}

#[derive(Clone)]
pub struct FnParam {
    pub name: String,
    pub ty: Ty,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct InstantiatedFn {
    pub fn_: Fn,
    pub gen_args: Vec<Ty>,
}
