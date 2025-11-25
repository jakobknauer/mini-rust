use std::{collections::HashMap, fmt::Display};

use crate::ctxt::{fns::InstantiatedFn, ty::Ty};

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Stmt(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Val(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Place(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Loc(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Op(pub usize);

#[derive(Debug, Clone)]
pub struct Mlr {
    pub vals: HashMap<Val, ValDef>,
    pub stmts: HashMap<Stmt, StmtDef>,
    pub places: HashMap<Place, PlaceDef>,
    pub ops: HashMap<Op, OpDef>,

    pub loc_tys: HashMap<Loc, Ty>,
    pub val_tys: HashMap<Val, Ty>,
    pub place_tys: HashMap<Place, Ty>,
    pub op_tys: HashMap<Op, Ty>,
    pub body: Stmt,
    pub param_locs: Vec<Loc>,
}

impl Mlr {
    pub fn new() -> Self {
        Self {
            vals: HashMap::new(),
            stmts: HashMap::new(),
            places: HashMap::new(),
            ops: HashMap::new(),

            loc_tys: HashMap::new(),
            val_tys: HashMap::new(),
            place_tys: HashMap::new(),
            op_tys: HashMap::new(),
            body: Stmt(0),
            param_locs: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum StmtDef {
    Alloc { loc: Loc },
    Assign { place: Place, value: Val },
    Return { value: Val },
    Block(Vec<Stmt>),
    If(If),
    Loop { body: Stmt },
    Break,
}

#[derive(Debug, Clone)]
pub enum ValDef {
    Call { callable: Op, args: Vec<Op> },
    Empty { ty: Ty },
    Use(Op),
    AddrOf(Place),
}

#[derive(Debug, Clone)]
pub enum OpDef {
    Fn(InstantiatedFn),
    Const(Const),
    Copy(Place),
}

#[derive(Debug, Clone)]
pub enum PlaceDef {
    Loc(Loc),
    FieldAccess { base: Place, field_index: usize },
    EnumDiscriminant { base: Place },
    ProjectToVariant { base: Place, variant_index: usize },
    Deref(Op),
}

#[derive(Debug, Clone)]
pub enum Const {
    Int(i64),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Op,
    pub then: Stmt,
    pub else_: Stmt,
}

impl Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}", self.0)
    }
}
