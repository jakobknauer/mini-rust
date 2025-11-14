use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use crate::ctxt::{functions::FnId, types::TypeId};

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct StmtId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct ValId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct PlaceId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct LocId(pub usize);

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct OpId(pub usize);

#[derive(Debug)]
pub struct Mlr {
    pub vals: HashMap<ValId, Val>,
    pub stmts: HashMap<StmtId, Stmt>,
    pub places: HashMap<PlaceId, Place>,
    pub ops: HashMap<OpId, Operand>,
    pub allocated_locs: HashSet<LocId>,

    pub loc_types: HashMap<LocId, TypeId>,
    pub val_types: HashMap<ValId, TypeId>,
    pub place_types: HashMap<PlaceId, TypeId>,
    pub op_types: HashMap<OpId, TypeId>,
    pub body: StmtId,
    pub param_locs: Vec<LocId>,
}

impl Mlr {
    pub fn new() -> Self {
        Self {
            vals: HashMap::new(),
            stmts: HashMap::new(),
            places: HashMap::new(),
            ops: HashMap::new(),
            allocated_locs: HashSet::new(),

            loc_types: HashMap::new(),
            val_types: HashMap::new(),
            place_types: HashMap::new(),
            op_types: HashMap::new(),
            body: StmtId(0),
            param_locs: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Alloc { loc: LocId },
    Assign { place: PlaceId, value: ValId },
    Return { value: ValId },
    Block(Vec<StmtId>),
    If(If),
    Loop { body: StmtId },
    Break,
}

#[derive(Debug, Clone)]
pub enum Val {
    Call { callable: OpId, args: Vec<OpId> },
    Empty { type_id: TypeId },
    Use(OpId),
}

#[derive(Debug, Clone)]
pub enum Operand {
    Function(FnId),
    Constant(Constant),
    Copy(PlaceId),
}

#[derive(Debug, Clone)]
pub enum Place {
    Local(LocId),
    FieldAccess { base: PlaceId, field_index: usize },
    EnumDiscriminant { base: PlaceId },
    ProjectToVariant { base: PlaceId, variant_index: usize },
}

#[derive(Debug, Clone)]
pub enum Constant {
    Int(i64),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: OpId,
    pub then_block: StmtId,
    pub else_block: StmtId,
}

impl Display for LocId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}", self.0)
    }
}
