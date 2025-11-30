use std::collections::HashMap;

use crate::ctxt::{fns::FnSpecialization, ty::Ty};

pub struct Mlr {
    stmts: Vec<StmtDef>,
    vals: Vec<ValDef>,
    places: Vec<PlaceDef>,
    ops: Vec<OpDef>,

    val_tys: HashMap<Val, Ty>,
    place_tys: HashMap<Place, Ty>,
    op_tys: HashMap<Op, Ty>,
    loc_tys: HashMap<Loc, Ty>,

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
            stmts: Vec::new(),
            vals: Vec::new(),
            places: Vec::new(),
            ops: Vec::new(),

            val_tys: HashMap::new(),
            place_tys: HashMap::new(),
            op_tys: HashMap::new(),
            loc_tys: HashMap::new(),

            next_loc: Loc(0),
        }
    }

    pub fn insert_stmt(&mut self, stmt_def: StmtDef) -> Stmt {
        let stmt = Stmt(self.stmts.len());
        self.stmts.push(stmt_def);
        stmt
    }

    pub fn insert_val(&mut self, val_def: ValDef) -> Val {
        let val = Val(self.vals.len());
        self.vals.push(val_def);
        val
    }

    pub fn insert_place(&mut self, place_def: PlaceDef) -> Place {
        let place = Place(self.places.len());
        self.places.push(place_def);
        place
    }

    pub fn insert_op(&mut self, op_def: OpDef) -> Op {
        let op = Op(self.ops.len());
        self.ops.push(op_def);
        op
    }

    pub fn insert_typed_loc(&mut self, ty: Ty) -> Loc {
        let loc = self.get_next_loc();
        self.loc_tys.insert(loc, ty);
        loc
    }

    fn get_next_loc(&mut self) -> Loc {
        let loc = self.next_loc;
        self.next_loc.0 += 1;
        loc
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

    pub fn get_stmt_def(&self, stmt: &Stmt) -> &StmtDef {
        self.stmts.get(stmt.0).expect("stmt should be known")
    }

    pub fn try_get_stmt_def(&self, stmt: Stmt) -> Option<&StmtDef> {
        self.stmts.get(stmt.0)
    }

    pub fn get_val_def(&self, val: &Val) -> &ValDef {
        self.vals.get(val.0).expect("val should be known")
    }

    pub fn try_get_val_def(&self, val: &Val) -> Option<&ValDef> {
        self.vals.get(val.0)
    }

    pub fn get_place_def(&self, place: Place) -> &PlaceDef {
        self.places.get(place.0).expect("place should be known")
    }

    pub fn try_get_place_def(&self, place: &Place) -> Option<&PlaceDef> {
        self.places.get(place.0)
    }

    pub fn get_op_def(&self, op: &Op) -> &OpDef {
        self.ops.get(op.0).expect("op should be known")
    }

    pub fn try_get_op_def(&self, op: &Op) -> Option<&OpDef> {
        self.ops.get(op.0)
    }

    pub fn get_val_ty(&self, val: &Val) -> Ty {
        *self.val_tys.get(val).expect("type of val should be known")
    }

    pub fn get_place_ty(&self, place: &Place) -> Ty {
        *self.place_tys.get(place).expect("type of place should be known")
    }

    pub fn get_op_ty(&self, op: &Op) -> Ty {
        *self.op_tys.get(op).expect("type of op should be known")
    }

    pub fn get_loc_ty(&self, loc: &Loc) -> Ty {
        *self.loc_tys.get(loc).expect("type of loc should be known")
    }

    pub fn get_all_types_mut(&mut self) -> impl Iterator<Item = &mut Ty> {
        self.loc_tys
            .values_mut()
            .chain(self.val_tys.values_mut())
            .chain(self.place_tys.values_mut())
            .chain(self.op_tys.values_mut())
            .chain(
                self.ops
                    .iter_mut()
                    .filter_map(|op_def| match op_def {
                        OpDef::Fn(FnSpecialization { gen_args, .. }) => Some(gen_args),
                        _ => None,
                    })
                    .flatten(),
            )
    }
}
