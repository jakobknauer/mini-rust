use std::collections::HashMap;

use crate::ctxt::{fns::FnSpecialization, ty::Ty};

pub struct Mlr {
    vals: HashMap<Val, ValDef>,
    stmts: HashMap<Stmt, StmtDef>,
    places: HashMap<Place, PlaceDef>,
    ops: HashMap<Op, OpDef>,

    val_tys: HashMap<Val, Ty>,
    place_tys: HashMap<Place, Ty>,
    op_tys: HashMap<Op, Ty>,
    loc_tys: HashMap<Loc, Ty>,

    next_val: Val,
    next_stmt: Stmt,
    next_place: Place,
    next_op: Op,
    next_loc: Loc,
}

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
    Fn(FnSpecialization),
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

#[derive(Debug, Clone, Copy)]
pub struct If {
    pub cond: Op,
    pub then: Stmt,
    pub else_: Stmt,
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}", self.0)
    }
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

            next_val: Val(0),
            next_place: Place(0),
            next_stmt: Stmt(0),
            next_loc: Loc(0),
            next_op: Op(0),
        }
    }

    pub fn insert_val(&mut self, val_def: ValDef) -> Val {
        let val = self.get_next_val();
        self.vals.insert(val, val_def);
        val
    }

    pub fn insert_stmt(&mut self, stmt_def: StmtDef) -> Stmt {
        let stmt = self.get_next_stmt();
        self.stmts.insert(stmt, stmt_def);
        stmt
    }

    pub fn insert_place(&mut self, place_def: PlaceDef) -> Place {
        let place = self.get_next_place();
        self.places.insert(place, place_def);
        place
    }

    pub fn insert_op(&mut self, op_def: OpDef) -> Op {
        let op = self.get_next_op();
        self.ops.insert(op, op_def);
        op
    }

    pub fn insert_loc(&mut self) -> Loc {
        self.get_next_loc()
    }

    pub fn insert_typed_loc(&mut self, ty: Ty) -> Loc {
        let loc = self.get_next_loc();
        self.loc_tys.insert(loc, ty);
        loc
    }

    fn get_next_val(&mut self) -> Val {
        let val = self.next_val;
        self.next_val.0 += 1;
        val
    }

    fn get_next_stmt(&mut self) -> Stmt {
        let stmt = self.next_stmt;
        self.next_stmt.0 += 1;
        stmt
    }

    fn get_next_loc(&mut self) -> Loc {
        let loc = self.next_loc;
        self.next_loc.0 += 1;
        loc
    }

    fn get_next_place(&mut self) -> Place {
        let place = self.next_place;
        self.next_place.0 += 1;
        place
    }

    fn get_next_op(&mut self) -> Op {
        let op = self.next_op;
        self.next_op.0 += 1;
        op
    }

    pub fn get_val_def(&self, val: &Val) -> &ValDef {
        self.vals.get(val).expect("val should be known")
    }

    pub fn try_get_val_def(&self, val: &Val) -> Option<&ValDef> {
        self.vals.get(val)
    }

    pub fn get_stmt_def(&self, stmt: &Stmt) -> &StmtDef {
        self.stmts.get(stmt).expect("stmt should be known")
    }

    pub fn try_get_stmt_def(&self, stmt: Stmt) -> Option<&StmtDef> {
        self.stmts.get(&stmt)
    }

    pub fn get_place_def(&self, place: Place) -> &PlaceDef {
        self.places.get(&place).expect("place should be known")
    }

    pub fn try_get_place_def(&self, place: &Place) -> Option<&PlaceDef> {
        self.places.get(place)
    }

    pub fn get_op_def(&self, op: &Op) -> &OpDef {
        self.ops.get(op).expect("op should be known")
    }

    pub fn try_get_op_def(&self, op: &Op) -> Option<&OpDef> {
        self.ops.get(op)
    }

    pub fn get_loc_ty(&self, loc: &Loc) -> Ty {
        *self.loc_tys.get(loc).expect("type of loc should be known")
    }

    pub fn get_place_ty(&self, place: &Place) -> Ty {
        *self.place_tys.get(place).expect("type of place should be known")
    }

    pub fn get_val_ty(&self, val: &Val) -> Ty {
        *self.val_tys.get(val).expect("type of val should be known")
    }

    pub fn get_op_ty(&self, op: &Op) -> Ty {
        *self.op_tys.get(op).expect("type of op should be known")
    }

    pub fn get_all_types_mut(&mut self) -> impl Iterator<Item = &mut Ty> {
        self.loc_tys
            .values_mut()
            .chain(self.val_tys.values_mut())
            .chain(self.place_tys.values_mut())
            .chain(self.op_tys.values_mut())
            .chain(self.vals.values_mut().filter_map(|val_def| match val_def {
                ValDef::Empty { ty } => Some(ty),
                _ => None,
            }))
            .chain(
                self.ops
                    .values_mut()
                    .filter_map(|op_def| match op_def {
                        OpDef::Fn(FnSpecialization { gen_args, .. }) => Some(gen_args),
                        _ => None,
                    })
                    .flatten(),
            )
    }

    pub fn set_loc_ty(&mut self, loc: Loc, ty: Ty) {
        self.loc_tys.insert(loc, ty);
    }

    pub fn set_val_ty(&mut self, val: Val, ty: Ty) {
        self.val_tys.insert(val, ty);
    }

    pub fn set_place_ty(&mut self, place: Place, ty: Ty) {
        self.place_tys.insert(place, ty);
    }

    pub fn set_op_ty(&mut self, op: Op, ty: Ty) {
        self.op_tys.insert(op, ty);
    }
}
