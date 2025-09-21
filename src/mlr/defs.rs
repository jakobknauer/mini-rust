use std::collections::HashMap;

use crate::context::{functions::FnId, types::TypeId};

#[derive(Debug)]
pub struct Mlr {
    pub expressions: HashMap<ExprId, Expression>,
    pub statements: HashMap<StmtId, Statement>,
    pub types: HashMap<LocId, TypeId>,
    pub body: Block,
}

impl Mlr {
    pub fn new() -> Self {
        Self {
            expressions: HashMap::new(),
            statements: HashMap::new(),
            types: HashMap::new(),
            body: Block {
                statements: vec![],
                output: LocId(0),
            },
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct StmtId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct ExprId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct LocId(pub usize);

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
    Call {
        callable: LocId,
        args: Vec<LocId>,
    },
    Function(FnId),
    If {
        condition: LocId,
        then_block: Block,
        else_block: Block,
    },
    Loop {
        body: ExprId,
    },
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
