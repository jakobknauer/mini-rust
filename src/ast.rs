use std::{
    cell::{Ref, RefCell},
    marker::PhantomData,
};

pub mod builder;

#[derive(Debug, Default)]
pub struct Ast<'ast> {
    free_fns: RefCell<Vec<Fn<'ast>>>,
    structs: RefCell<Vec<&'ast Struct<'ast>>>,
    enums: RefCell<Vec<&'ast Enum<'ast>>>,
    impls: RefCell<Vec<&'ast Impl<'ast>>>,
    traits: RefCell<Vec<&'ast Trait<'ast>>>,

    arena: bumpalo::Bump,
    _marker: PhantomData<&'ast ()>,
}

impl<'ast> Ast<'ast> {
    pub fn expr(&'ast self, expr: ExprKind<'ast>) -> Expr<'ast> {
        self.arena.alloc(expr)
    }

    pub fn expr_slice(&'ast self, exprs: &[Expr<'ast>]) -> ExprSlice<'ast> {
        self.arena.alloc_slice_copy(exprs)
    }

    pub fn stmt(&'ast self, stmt: StmtKind<'ast>) -> Stmt<'ast> {
        self.arena.alloc(stmt)
    }

    pub fn stmt_slice(&'ast self, stmts: &[Stmt<'ast>]) -> StmtSlice<'ast> {
        self.arena.alloc_slice_copy(stmts)
    }

    pub fn ty_annot(&'ast self, annot: TyAnnotKind<'ast>) -> TyAnnot<'ast> {
        self.arena.alloc(annot)
    }

    pub fn ty_annot_slice(&'ast self, ty_annots: &[TyAnnot<'ast>]) -> TyAnnotSlice<'ast> {
        self.arena.alloc_slice_copy(ty_annots)
    }

    pub fn fn_(&'ast self, fn_def: FnDef<'ast>) -> Fn<'ast> {
        self.arena.alloc(fn_def)
    }

    pub fn structs(&self) -> Ref<'_, [&'ast Struct<'ast>]> {
        Ref::map(self.structs.borrow(), |v| v.as_slice())
    }

    pub fn enums(&self) -> Ref<'_, [&'ast Enum<'ast>]> {
        Ref::map(self.enums.borrow(), |v| v.as_slice())
    }

    pub fn impls(&self) -> Ref<'_, [&'ast Impl<'ast>]> {
        Ref::map(self.impls.borrow(), |v| v.as_slice())
    }

    pub fn traits(&self) -> Ref<'_, [&'ast Trait<'ast>]> {
        Ref::map(self.traits.borrow(), |v| v.as_slice())
    }

    pub fn free_fns(&self) -> Ref<'_, [Fn<'ast>]> {
        Ref::map(self.free_fns.borrow(), |v| v.as_slice())
    }
}

pub type Fn<'ast> = &'ast FnDef<'ast>;

#[derive(Debug, PartialEq, Eq)]
pub struct FnDef<'ast> {
    pub name: String,
    pub gen_params: Vec<String>,
    pub params: Vec<Param<'ast>>,
    pub var_args: bool,
    pub return_ty: Option<TyAnnot<'ast>>,
    pub constraints: Vec<Constraint<'ast>>,
    pub body: Option<Block<'ast>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Param<'ast> {
    Regular { name: String, ty: TyAnnot<'ast> },
    Receiver,
    ReceiverByRef,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Constraint<'ast> {
    pub subject: String,
    pub requirement: ConstraintRequirement<'ast>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ConstraintRequirement<'ast> {
    Trait {
        trait_name: String,
        trait_args: TyAnnotSlice<'ast>,
    },
    Callable {
        params: TyAnnotSlice<'ast>,
        return_ty: Option<TyAnnot<'ast>>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Struct<'ast> {
    pub name: String,
    pub gen_params: Vec<String>,
    pub fields: Vec<StructField<'ast>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructField<'ast> {
    pub name: String,
    pub ty: TyAnnot<'ast>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Enum<'ast> {
    pub name: String,
    pub gen_params: Vec<String>,
    pub variants: Vec<EnumVariant<'ast>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumVariant<'ast> {
    pub name: String,
    pub fields: Vec<StructField<'ast>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Impl<'ast> {
    pub gen_params: Vec<String>,
    pub trait_annot: Option<TraitAnnot<'ast>>,
    pub ty: TyAnnot<'ast>,
    pub mthds: Vec<Fn<'ast>>,
    pub assoc_tys: Vec<AssocTy<'ast>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssocTy<'ast> {
    pub name: String,
    pub ty: TyAnnot<'ast>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TraitAnnot<'ast> {
    pub name: String,
    pub args: Option<TyAnnotSlice<'ast>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Trait<'ast> {
    pub name: String,
    pub gen_params: Vec<String>,
    pub mthds: Vec<Fn<'ast>>,
    pub assoc_ty_names: Vec<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Block<'ast> {
    pub stmts: StmtSlice<'ast>,
    pub return_expr: Option<Expr<'ast>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Path<'ast> {
    pub segments: Vec<PathSegment<'ast>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QualifiedPath<'ast> {
    pub ty: TyAnnot<'ast>,
    pub trait_: Option<TraitAnnot<'ast>>,
    pub path: Path<'ast>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PathSegment<'ast> {
    pub ident: String,
    pub args: Option<TyAnnotSlice<'ast>>,
    pub is_self: bool,
}

pub type Stmt<'ast> = &'ast StmtKind<'ast>;
pub type StmtSlice<'ast> = &'ast [Stmt<'ast>];

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StmtKind<'ast> {
    Let {
        name: String,
        ty_annot: Option<TyAnnot<'ast>>,
        value: Expr<'ast>,
    },
    Expr(Expr<'ast>),
    Return(Option<Expr<'ast>>),
    Break,
}

pub type Expr<'ast> = &'ast ExprKind<'ast>;
pub type ExprSlice<'ast> = &'ast [Expr<'ast>];

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind<'ast> {
    Lit(Lit),
    Path(Path<'ast>),
    QualifiedPath(QualifiedPath<'ast>),
    Tuple(ExprSlice<'ast>),
    BinaryOp {
        left: Expr<'ast>,
        operator: BinaryOperator,
        right: Expr<'ast>,
    },
    UnaryOp {
        operator: UnaryOperator,
        operand: Expr<'ast>,
    },
    Assign {
        target: Expr<'ast>,
        value: Expr<'ast>,
    },
    Call {
        callee: Expr<'ast>,
        args: ExprSlice<'ast>,
    },
    MthdCall {
        obj: Expr<'ast>,
        mthd: PathSegment<'ast>,
        args: ExprSlice<'ast>,
    },
    Struct {
        ty_path: Path<'ast>,
        fields: Vec<(String, Expr<'ast>)>,
    },
    FieldAccess {
        obj: Expr<'ast>,
        field: FieldDescriptor<'ast>,
    },
    Block(Block<'ast>),
    If {
        cond: Expr<'ast>,
        then: Block<'ast>,
        else_: Option<Block<'ast>>,
    },
    Loop {
        body: Block<'ast>,
    },
    While {
        cond: Expr<'ast>,
        body: Block<'ast>,
    },
    Match {
        scrutinee: Expr<'ast>,
        arms: Vec<MatchArm<'ast>>,
    },
    Deref {
        base: Expr<'ast>,
    },
    AddrOf {
        base: Expr<'ast>,
    },
    As {
        expr: Expr<'ast>,
        target_ty: TyAnnot<'ast>,
    },
    Self_,
    Closure {
        params: Vec<ClosureParam<'ast>>,
        return_ty: Option<TyAnnot<'ast>>,
        body: Block<'ast>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FieldDescriptor<'ast> {
    Named(PathSegment<'ast>),
    Indexed(usize),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClosureParam<'ast> {
    pub name: String,
    pub ty: Option<TyAnnot<'ast>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    CChar(u8),
    CString(Vec<u8>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    Remainder,
    BitOr,
    BitAnd,
    LogicalAnd,
    LogicalOr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    Not,
    Negative,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatchArm<'ast> {
    pub pattern: Pattern<'ast>,
    pub value: Expr<'ast>,
}

type Pattern<'ast> = VariantPattern<'ast>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariantPattern<'ast> {
    pub variant: Path<'ast>,
    pub fields: Vec<VariantPatternField>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariantPatternField {
    pub field_name: String,
    pub binding_name: String,
}

pub type TyAnnot<'ast> = &'ast TyAnnotKind<'ast>;
pub type TyAnnotSlice<'ast> = &'ast [TyAnnot<'ast>];

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TyAnnotKind<'ast> {
    Path(Path<'ast>),
    Tuple(TyAnnotSlice<'ast>),
    Ref(TyAnnot<'ast>),
    Ptr(TyAnnot<'ast>),
    Fn {
        param_tys: TyAnnotSlice<'ast>,
        return_ty: Option<TyAnnot<'ast>>,
    },
    Wildcard,
}
