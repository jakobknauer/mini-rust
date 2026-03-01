pub mod builder;

use std::{cell::RefCell, collections::HashMap};

use crate::ctxt::{
    fns::{FnInst, TraitMthdInst},
    language_items::{BinaryPrimOp, UnaryPrimOp},
    ty::Ty,
};

#[derive(Default)]
pub struct Mlr<'mlr> {
    val_tys: RefCell<HashMap<Val<'mlr>, Ty>>,
    place_tys: RefCell<HashMap<Place<'mlr>, Ty>>,
    op_tys: RefCell<HashMap<Op<'mlr>, Ty>>,
    loc_tys: RefCell<HashMap<Loc, Ty>>,

    next_loc_id: RefCell<Loc>,

    _marker: std::marker::PhantomData<&'mlr ()>,
    arena: bumpalo::Bump,
}

pub type Stmt<'mlr> = &'mlr StmtDef<'mlr>;
pub type Val<'mlr> = &'mlr ValDef<'mlr>;
pub type Place<'mlr> = &'mlr PlaceDef<'mlr>;
pub type Op<'mlr> = &'mlr OpDef<'mlr>;

#[derive(Default, Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub struct Loc(pub usize);

#[derive(Debug, Clone)]
pub enum StmtDef<'mlr> {
    Alloc { loc: Loc },
    Assign { place: Place<'mlr>, value: Val<'mlr> },
    Return { value: Val<'mlr> },
    Block(&'mlr [Stmt<'mlr>]),
    If(If<'mlr>),
    Loop { body: Stmt<'mlr> },
    Break,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ValDef<'mlr> {
    Call {
        callable: Op<'mlr>,
        args: Vec<Op<'mlr>>,
    },
    Use(Op<'mlr>),
    AddrOf(Place<'mlr>),
    As {
        op: Op<'mlr>,
        target_ty: Ty,
    },
    BinaryPrim {
        op: BinaryPrimOp,
        lhs: Op<'mlr>,
        rhs: Op<'mlr>,
    },
    UnaryPrim {
        op: UnaryPrimOp,
        operand: Op<'mlr>,
    },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum OpDef<'mlr> {
    Fn(FnInst),
    TraitMthd(TraitMthdInst),
    Const(Const),
    Copy(Place<'mlr>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum PlaceDef<'mlr> {
    Loc(Loc),
    FieldAccess { base: Place<'mlr>, field_index: usize },
    EnumDiscriminant { base: Place<'mlr> },
    ProjectToVariant { base: Place<'mlr>, variant_index: usize },
    ClosureCaptures(Place<'mlr>),
    Deref(Op<'mlr>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Const {
    Int(i64),
    Bool(bool),
    CChar(u8),
    CString(Vec<u8>),
}

#[derive(Debug, Clone, Copy)]
pub struct If<'mlr> {
    pub cond: Op<'mlr>,
    pub then: Stmt<'mlr>,
    pub else_: Stmt<'mlr>,
}

#[derive(Debug, Clone)]
pub struct Fn<'mlr> {
    pub body: Stmt<'mlr>,
    pub param_locs: Vec<Loc>,
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}", self.0)
    }
}

impl<'mlr> Mlr<'mlr> {
    pub fn insert_stmt(&'mlr self, stmt_def: StmtDef<'mlr>) -> Stmt<'mlr> {
        self.arena.alloc(stmt_def)
    }

    pub fn insert_stmt_slice(&'mlr self, stmts: &[Stmt<'mlr>]) -> &'mlr [Stmt<'mlr>] {
        self.arena.alloc_slice_copy(stmts)
    }

    pub fn insert_val(&'mlr self, val_def: ValDef<'mlr>) -> Val<'mlr> {
        self.arena.alloc(val_def)
    }

    pub fn insert_place(&'mlr self, place_def: PlaceDef<'mlr>) -> Place<'mlr> {
        self.arena.alloc(place_def)
    }

    pub fn insert_op(&'mlr self, op_def: OpDef<'mlr>) -> Op<'mlr> {
        self.arena.alloc(op_def)
    }

    pub fn insert_typed_loc(&self, ty: Ty) -> Loc {
        let loc = self.get_next_loc();
        self.loc_tys.borrow_mut().insert(loc, ty);
        loc
    }

    fn get_next_loc(&self) -> Loc {
        self.next_loc_id.replace_with(|Loc(id)| Loc(*id + 1))
    }

    pub fn set_val_ty(&self, val: Val<'mlr>, ty: Ty) {
        self.val_tys.borrow_mut().insert(val, ty);
    }

    pub fn set_place_ty(&self, place: Place<'mlr>, ty: Ty) {
        self.place_tys.borrow_mut().insert(place, ty);
    }

    pub fn set_op_ty(&self, op: Op<'mlr>, ty: Ty) {
        self.op_tys.borrow_mut().insert(op, ty);
    }

    pub fn get_val_ty(&self, val: Val) -> Ty {
        *self.val_tys.borrow().get(&val).expect("type of val should be known")
    }

    pub fn get_place_ty(&self, place: Place) -> Ty {
        *self
            .place_tys
            .borrow()
            .get(&place)
            .expect("type of place should be known")
    }

    pub fn get_op_ty(&self, op: Op) -> Ty {
        *self.op_tys.borrow().get(&op).expect("type of op should be known")
    }

    pub fn get_loc_ty(&self, loc: Loc) -> Ty {
        *self.loc_tys.borrow().get(&loc).expect("type of loc should be known")
    }
}
