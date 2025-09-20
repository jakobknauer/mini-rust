use std::collections::HashMap;

use crate::context::{functions::FnId, types::TypeId};

pub struct Mlr {
    pub expressions: HashMap<ExprId, Expression>,
    pub statements: HashMap<StmtId, Statement>,
    pub types: HashMap<ExprId, TypeId>,
    pub body: Block,
}

pub struct StmtId(usize);
pub struct ExprId(usize);

const RETURN_VALUE: ExprId = ExprId(0);

pub enum Statement {
    Assign { loc: ExprId, value: ExprId },
    Return { value: ExprId, from: ExprId },
}

pub struct Expression {
    pub type_: TypeId,
    pub val: ExprValue,
}

pub enum ExprValue {
    Block(Block),
    Constant(Constant),
    Var(ExprId),
    Call {
        callable: ExprId,
        args: Vec<ExprId>,
    },
    Function(FnId),
    If {
        condition: ExprId,
        then_block: ExprId,
        else_block: Option<ExprId>,
    },
    Loop {
        body: ExprId,
    },
}

pub struct Block {
    statements: Vec<StmtId>,
    output: ExprId,
}

pub enum Constant {
    Integer(i32),
    Boolean(bool),
}
