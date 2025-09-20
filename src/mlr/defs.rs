use std::collections::HashMap;

use crate::context::{functions::FnId, types::TypeId};

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

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct StmtId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ExprId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct LocId(pub usize);

const RETURN_VALUE: ExprId = ExprId(0);

pub enum Statement {
    Assign { loc: LocId, value: ExprId },
    Return { value: LocId },
}

pub enum Expression {
    Block(Block),
    Constant(Constant),
    Var(LocId),
    Call {
        callable: LocId,
        args: Vec<LocId>,
    },
    Function(FnId),
    If {
        condition: LocId,
        then_block: ExprId,
        else_block: Option<ExprId>,
    },
    Loop {
        body: ExprId,
    },
}

pub struct Block {
    pub statements: Vec<StmtId>,
    pub output: LocId,
}

pub enum Constant {
    Int(i64),
    Bool(bool),
}
