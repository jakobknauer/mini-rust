#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Tuple(Vec<Type>),
    Option(Box<Type>),
}

impl Type {
    pub fn option(t: Type) -> Type {
        Type::Option(Box::new(t))
    }

    pub fn get_constructors(&self) -> Vec<Constructor> {
        match self {
            Type::Int => vec![],
            Type::Tuple(items) => vec![Constructor::Tuple(items.len())],
            Type::Option(_) => vec![Constructor::Some, Constructor::None],
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
pub enum Constructor {
    Some,
    None,
    Tuple(usize),
}
