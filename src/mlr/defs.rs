use std::{collections::HashMap, fmt::Display};

use crate::ctxt::{functions::FnId, types::TypeId};

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct StmtId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct ValId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct LocId(pub usize);

#[derive(Debug)]
pub struct Mlr {
    pub vals: HashMap<ValId, Value>,
    pub statements: HashMap<StmtId, Statement>,
    pub loc_types: HashMap<LocId, TypeId>,
    pub val_types: HashMap<ValId, TypeId>,
    pub body: Block,
    pub param_locs: Vec<LocId>,
}

impl Mlr {
    pub fn new() -> Self {
        Self {
            vals: HashMap::new(),
            statements: HashMap::new(),
            loc_types: HashMap::new(),
            val_types: HashMap::new(),
            body: Block {
                statements: Vec::new(),
                output: LocId(0),
            },
            param_locs: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Assign { place: Place, value: ValId },
    Return { value: LocId },
    Break,
}

#[derive(Debug)]
pub enum Value {
    Block(Block),
    Constant(Constant),
    Use(LocId),
    AddressOf(LocId),
    Call {
        callable: LocId,
        args: Vec<LocId>,
    },
    Function(FnId),
    If(If),
    Loop {
        body: Block,
    },
    Struct {
        type_id: TypeId,
        field_initializers: Vec<(String, LocId)>,
    },
}

#[derive(Debug)]
pub enum Place {
    Local(LocId),
}

#[derive(Debug)]
pub struct Block {
    pub statements: Vec<StmtId>,
    pub output: LocId,
}

#[derive(Debug)]
pub enum Constant {
    Int(i64),
    Bool(bool),
    Unit,
}

#[derive(Debug)]
pub struct If {
    pub condition: LocId,
    pub then_block: Block,
    pub else_block: Block,
}

impl Display for LocId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}", self.0)
    }
}
