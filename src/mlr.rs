pub mod builder;

use std::cell::Cell;

use crate::ctxt::{
    fns::{FnInst, TraitMthdInst},
    language_items::{BinaryPrimOp, UnaryPrimOp},
    ty::Ty,
};

#[derive(Default)]
pub struct Mlr<'mlr> {
    next_loc_id: Cell<usize>,

    _marker: std::marker::PhantomData<&'mlr ()>,
    arena: bumpalo::Bump,
}

pub type Stmt<'mlr> = &'mlr StmtDef<'mlr>;

#[derive(Clone, Copy, Debug)]
pub struct Val<'mlr>(pub &'mlr ValDef<'mlr>, pub Ty);

#[derive(Clone, Copy, Debug)]
pub struct Place<'mlr>(pub &'mlr PlaceDef<'mlr>, pub Ty);

#[derive(Clone, Copy, Debug)]
pub struct Op<'mlr>(pub &'mlr OpDef<'mlr>, pub Ty);

impl<'mlr> std::ops::Deref for Val<'mlr> {
    type Target = ValDef<'mlr>;
    fn deref(&self) -> &ValDef<'mlr> {
        self.0
    }
}

impl<'mlr> std::ops::Deref for Place<'mlr> {
    type Target = PlaceDef<'mlr>;
    fn deref(&self) -> &PlaceDef<'mlr> {
        self.0
    }
}

impl<'mlr> std::ops::Deref for Op<'mlr> {
    type Target = OpDef<'mlr>;
    fn deref(&self) -> &OpDef<'mlr> {
        self.0
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Loc(usize, pub Ty);

impl PartialEq for Loc {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for Loc {}

impl std::hash::Hash for Loc {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}", self.0)
    }
}

#[derive(Debug)]
pub enum StmtDef<'mlr> {
    Alloc { loc: Loc },
    Assign { place: Place<'mlr>, value: Val<'mlr> },
    Return { value: Val<'mlr> },
    Block(&'mlr [Stmt<'mlr>]),
    If(If<'mlr>),
    Loop { body: Stmt<'mlr> },
    Break,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum OpDef<'mlr> {
    Fn(FnInst),
    TraitMthd(TraitMthdInst),
    Const(Const),
    Copy(Place<'mlr>),
}

#[derive(Debug)]
pub enum PlaceDef<'mlr> {
    Loc(Loc),
    FieldAccess { base: Place<'mlr>, field_index: usize },
    EnumDiscriminant { base: Place<'mlr> },
    ProjectToVariant { base: Place<'mlr>, variant_index: usize },
    ClosureCaptures(Place<'mlr>),
    Deref(Op<'mlr>),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Fn<'mlr> {
    pub body: Stmt<'mlr>,
    pub param_locs: Vec<Loc>,
}

impl<'mlr> Mlr<'mlr> {
    pub fn insert_stmt(&'mlr self, stmt_def: StmtDef<'mlr>) -> Stmt<'mlr> {
        self.arena.alloc(stmt_def)
    }

    pub fn insert_stmt_slice(&'mlr self, stmts: &[Stmt<'mlr>]) -> &'mlr [Stmt<'mlr>] {
        self.arena.alloc_slice_copy(stmts)
    }

    pub fn insert_val(&'mlr self, val_def: ValDef<'mlr>, ty: Ty) -> Val<'mlr> {
        Val(self.arena.alloc(val_def), ty)
    }

    pub fn insert_place(&'mlr self, place_def: PlaceDef<'mlr>, ty: Ty) -> Place<'mlr> {
        Place(self.arena.alloc(place_def), ty)
    }

    pub fn insert_op(&'mlr self, op_def: OpDef<'mlr>, ty: Ty) -> Op<'mlr> {
        Op(self.arena.alloc(op_def), ty)
    }

    pub fn insert_typed_loc(&self, ty: Ty) -> Loc {
        let id = self.next_loc_id.get();
        self.next_loc_id.set(id + 1);
        Loc(id, ty)
    }
}
