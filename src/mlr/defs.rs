use std::{collections::HashMap, fmt::Display};

use crate::ctxt::{functions::FnId, types::TypeId};

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct StmtId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct ExprId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct LocId(pub usize);

#[derive(Debug)]
pub struct Mlr {
    pub expressions: HashMap<ExprId, Expression>,
    pub statements: HashMap<StmtId, Statement>,
    pub loc_types: HashMap<LocId, TypeId>,
    pub expr_types: HashMap<ExprId, TypeId>,
    pub body: Block,
    pub param_locs: Vec<LocId>,
}

impl Mlr {
    pub fn new() -> Self {
        Self {
            expressions: HashMap::new(),
            statements: HashMap::new(),
            loc_types: HashMap::new(),
            expr_types: HashMap::new(),
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
    Assign { loc: LocId, value: ExprId },
    Return { value: LocId },
}

#[derive(Debug)]
pub enum Expression {
    Block(Block),
    Constant(Constant),
    Var(LocId),
    AddressOf(LocId),
    Call { callable: LocId, args: Vec<LocId> },
    Function(FnId),
    If(If),
    Loop { body: ExprId },
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
