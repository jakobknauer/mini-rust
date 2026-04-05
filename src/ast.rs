use std::{
    cell::{Cell, Ref, RefCell},
    ops::Deref,
};

pub mod builder;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct StructId(usize);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct EnumId(usize);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct TraitId(usize);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct ImplId(usize);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub struct FnId(usize);

#[derive(Debug)]
pub struct Ast<'ast> {
    free_fns: RefCell<Vec<Fn<'ast>>>,
    structs: RefCell<Vec<Struct<'ast>>>,
    enums: RefCell<Vec<Enum<'ast>>>,
    impls: RefCell<Vec<Impl<'ast>>>,
    traits: RefCell<Vec<Trait<'ast>>>,

    next_struct_id: Cell<StructId>,
    next_enum_id: Cell<EnumId>,
    next_trait_id: Cell<TraitId>,
    next_impl_id: Cell<ImplId>,
    next_fn_id: Cell<FnId>,

    arena: &'ast bumpalo::Bump,
}

impl<'ast> Ast<'ast> {
    pub fn new(arena: &'ast bumpalo::Bump) -> Self {
        Self {
            free_fns: RefCell::new(Vec::new()),
            structs: RefCell::new(Vec::new()),
            enums: RefCell::new(Vec::new()),
            impls: RefCell::new(Vec::new()),
            traits: RefCell::new(Vec::new()),
            next_struct_id: Cell::new(StructId(0)),
            next_enum_id: Cell::new(EnumId(0)),
            next_trait_id: Cell::new(TraitId(0)),
            next_impl_id: Cell::new(ImplId(0)),
            next_fn_id: Cell::new(FnId(0)),
            arena,
        }
    }
}

impl<'ast> Ast<'ast> {
    pub fn expr(&self, expr: ExprKind<'ast>) -> Expr<'ast> {
        self.arena.alloc(expr)
    }

    pub fn expr_slice(&self, exprs: &[Expr<'ast>]) -> ExprSlice<'ast> {
        self.arena.alloc_slice_copy(exprs)
    }

    pub fn stmt(&self, stmt: StmtKind<'ast>) -> Stmt<'ast> {
        self.arena.alloc(stmt)
    }

    pub fn stmt_slice(&self, stmts: &[Stmt<'ast>]) -> StmtSlice<'ast> {
        self.arena.alloc_slice_copy(stmts)
    }

    pub fn ty_annot(&self, annot: TyAnnotKind<'ast>) -> TyAnnot<'ast> {
        self.arena.alloc(annot)
    }

    pub fn ty_annot_slice(&self, ty_annots: &[TyAnnot<'ast>]) -> TyAnnotSlice<'ast> {
        self.arena.alloc_slice_copy(ty_annots)
    }

    pub fn assoc_binding_slice(&self, bindings: &[AssocBinding<'ast>]) -> &'ast [AssocBinding<'ast>] {
        self.arena.alloc_slice_clone(bindings)
    }

    pub fn fn_(&self, fn_def: FnDef<'ast>) -> Fn<'ast> {
        let id = self.next_fn_id.get();
        self.next_fn_id.set(FnId(id.0 + 1));
        Fn(self.arena.alloc(fn_def), id)
    }

    pub fn fn_slice(&self, fns: &[Fn<'ast>]) -> FnSlice<'ast> {
        self.arena.alloc_slice_copy(fns)
    }

    pub fn add_struct(&self, def: StructDef<'ast>) {
        let id = self.next_struct_id.get();
        self.next_struct_id.set(StructId(id.0 + 1));
        self.structs.borrow_mut().push(Struct(self.arena.alloc(def), id));
    }

    pub fn add_enum(&self, def: EnumDef<'ast>) {
        let id = self.next_enum_id.get();
        self.next_enum_id.set(EnumId(id.0 + 1));
        self.enums.borrow_mut().push(Enum(self.arena.alloc(def), id));
    }

    pub fn add_trait(&self, def: TraitDef<'ast>) {
        let id = self.next_trait_id.get();
        self.next_trait_id.set(TraitId(id.0 + 1));
        self.traits.borrow_mut().push(Trait(self.arena.alloc(def), id));
    }

    pub fn add_impl(&self, def: ImplDef<'ast>) {
        let id = self.next_impl_id.get();
        self.next_impl_id.set(ImplId(id.0 + 1));
        self.impls.borrow_mut().push(Impl(self.arena.alloc(def), id));
    }

    pub fn add_free_fn(&self, fn_: Fn<'ast>) {
        self.free_fns.borrow_mut().push(fn_);
    }

    pub fn structs(&self) -> Ref<'_, [Struct<'ast>]> {
        Ref::map(self.structs.borrow(), |v| v.as_slice())
    }

    pub fn enums(&self) -> Ref<'_, [Enum<'ast>]> {
        Ref::map(self.enums.borrow(), |v| v.as_slice())
    }

    pub fn impls(&self) -> Ref<'_, [Impl<'ast>]> {
        Ref::map(self.impls.borrow(), |v| v.as_slice())
    }

    pub fn traits(&self) -> Ref<'_, [Trait<'ast>]> {
        Ref::map(self.traits.borrow(), |v| v.as_slice())
    }

    pub fn free_fns(&self) -> Ref<'_, [Fn<'ast>]> {
        Ref::map(self.free_fns.borrow(), |v| v.as_slice())
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Fn<'ast>(pub &'ast FnDef<'ast>, pub FnId);
pub type FnSlice<'ast> = &'ast [Fn<'ast>];

impl<'ast> std::ops::Deref for Fn<'ast> {
    type Target = FnDef<'ast>;
    fn deref(&self) -> &FnDef<'ast> {
        self.0
    }
}

#[derive(Debug)]
pub struct FnDef<'ast> {
    pub name: String,
    pub gen_params: Vec<String>,
    pub params: Vec<Param<'ast>>,
    pub var_args: bool,
    pub return_ty: Option<TyAnnot<'ast>>,
    pub constraints: Vec<Constraint<'ast>>,
    pub body: Option<Block<'ast>>,
}

#[derive(Debug)]
pub enum Param<'ast> {
    Regular {
        name: String,
        ty: TyAnnot<'ast>,
        mutable: bool,
    },
    Receiver,
    ReceiverMut,
    ReceiverByRef,
    ReceiverByRefMut,
}

#[derive(Debug)]
pub struct Constraint<'ast> {
    pub subject: TyAnnot<'ast>,
    pub requirement: ConstraintRequirement<'ast>,
}

#[derive(Debug, Clone)]
pub enum ConstraintRequirement<'ast> {
    Trait {
        trait_name: String,
        trait_args: TyAnnotSlice<'ast>,
        assoc_bindings: &'ast [AssocBinding<'ast>],
    },
    Callable {
        params: TyAnnotSlice<'ast>,
        return_ty: Option<TyAnnot<'ast>>,
    },
}

#[derive(Debug, Clone)]
pub enum AssocBinding<'ast> {
    Eq {
        name: String,
        ty: TyAnnot<'ast>,
    },
    Bound {
        name: String,
        requirement: ConstraintRequirement<'ast>,
    },
}

#[derive(Debug)]
pub struct StructDef<'ast> {
    pub name: String,
    pub gen_params: Vec<String>,
    pub fields: Vec<StructField<'ast>>,
}

#[derive(Clone, Copy, Debug)]
pub struct Struct<'ast>(pub &'ast StructDef<'ast>, pub StructId);

impl<'ast> Deref for Struct<'ast> {
    type Target = StructDef<'ast>;
    fn deref(&self) -> &StructDef<'ast> {
        self.0
    }
}

#[derive(Debug)]
pub struct StructField<'ast> {
    pub name: String,
    pub ty: TyAnnot<'ast>,
}

#[derive(Debug)]
pub struct EnumDef<'ast> {
    pub name: String,
    pub gen_params: Vec<String>,
    pub variants: Vec<EnumVariant<'ast>>,
}

#[derive(Clone, Copy, Debug)]
pub struct Enum<'ast>(pub &'ast EnumDef<'ast>, pub EnumId);

impl<'ast> Deref for Enum<'ast> {
    type Target = EnumDef<'ast>;
    fn deref(&self) -> &EnumDef<'ast> {
        self.0
    }
}

#[derive(Debug)]
pub struct EnumVariant<'ast> {
    pub name: String,
    pub fields: Vec<StructField<'ast>>,
}

#[derive(Debug)]
pub struct ImplDef<'ast> {
    pub gen_params: Vec<String>,
    pub trait_annot: Option<TraitAnnot<'ast>>,
    pub ty: TyAnnot<'ast>,
    pub constraints: Vec<Constraint<'ast>>,
    pub mthds: FnSlice<'ast>,
    pub assoc_tys: Vec<AssocTy<'ast>>,
}

#[derive(Clone, Copy, Debug)]
pub struct Impl<'ast>(pub &'ast ImplDef<'ast>, pub ImplId);

impl<'ast> Deref for Impl<'ast> {
    type Target = ImplDef<'ast>;
    fn deref(&self) -> &ImplDef<'ast> {
        self.0
    }
}

#[derive(Debug)]
pub struct AssocTy<'ast> {
    pub name: String,
    pub ty: TyAnnot<'ast>,
}

#[derive(Debug)]
pub struct TraitAnnot<'ast> {
    pub name: String,
    pub args: Option<TyAnnotSlice<'ast>>,
}

#[derive(Debug)]
pub struct AssocTyDef<'ast> {
    pub name: String,
    pub bounds: Vec<ConstraintRequirement<'ast>>,
}

#[derive(Debug)]
pub struct TraitDef<'ast> {
    pub name: String,
    pub gen_params: Vec<String>,
    pub mthds: FnSlice<'ast>,
    pub assoc_tys: Vec<AssocTyDef<'ast>>,
}

#[derive(Clone, Copy, Debug)]
pub struct Trait<'ast>(pub &'ast TraitDef<'ast>, pub TraitId);

impl<'ast> Deref for Trait<'ast> {
    type Target = TraitDef<'ast>;
    fn deref(&self) -> &TraitDef<'ast> {
        self.0
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Block<'ast> {
    pub stmts: StmtSlice<'ast>,
    pub return_expr: Option<Expr<'ast>>,
}

#[derive(Debug)]
pub struct Path<'ast> {
    pub segments: Vec<PathSegment<'ast>>,
}

#[derive(Debug)]
pub struct QualifiedPath<'ast> {
    pub ty: TyAnnot<'ast>,
    pub trait_: Option<TraitAnnot<'ast>>,
    pub path: Path<'ast>,
}

#[derive(Debug)]
pub struct PathSegment<'ast> {
    pub ident: String,
    pub args: Option<TyAnnotSlice<'ast>>,
    pub is_self: bool,
}

pub type Stmt<'ast> = &'ast StmtKind<'ast>;
pub type StmtSlice<'ast> = &'ast [Stmt<'ast>];

#[derive(Debug)]
pub enum StmtKind<'ast> {
    Let {
        name: String,
        mutable: bool,
        ty_annot: Option<TyAnnot<'ast>>,
        value: Expr<'ast>,
    },
    Expr(Expr<'ast>),
    Return(Option<Expr<'ast>>),
    Break,
}

pub type Expr<'ast> = &'ast ExprKind<'ast>;
pub type ExprSlice<'ast> = &'ast [Expr<'ast>];

#[derive(Debug)]
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
    For {
        binding: String,
        mutable: bool,
        iter: Expr<'ast>,
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
    AddrOfMut {
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

#[derive(Debug)]
pub enum FieldDescriptor<'ast> {
    Named(PathSegment<'ast>),
    Indexed(usize),
}

#[derive(Debug)]
pub struct ClosureParam<'ast> {
    pub name: String,
    pub ty: Option<TyAnnot<'ast>>,
}

#[derive(Debug)]
pub enum Lit {
    Int(i64),
    Bool(bool),
    CChar(u8),
    CString(Vec<u8>),
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Copy, Debug)]
pub enum UnaryOperator {
    Not,
    Negative,
}

#[derive(Debug)]
pub struct MatchArm<'ast> {
    pub pattern: Pattern<'ast>,
    pub value: Expr<'ast>,
}

type Pattern<'ast> = VariantPattern<'ast>;

#[derive(Debug)]
pub struct VariantPattern<'ast> {
    pub variant: Path<'ast>,
    pub fields: Vec<VariantPatternField>,
}

#[derive(Debug)]
pub struct VariantPatternField {
    pub field_name: String,
    pub binding_name: String,
}

pub type TyAnnot<'ast> = &'ast TyAnnotKind<'ast>;
pub type TyAnnotSlice<'ast> = &'ast [TyAnnot<'ast>];

#[derive(Debug)]
pub enum TyAnnotKind<'ast> {
    Path(Path<'ast>),
    QualifiedPath(QualifiedPath<'ast>),
    Tuple(TyAnnotSlice<'ast>),
    Ref(TyAnnot<'ast>),
    RefMut(#[allow(unused)] TyAnnot<'ast>),
    Ptr(TyAnnot<'ast>),
    Fn {
        param_tys: TyAnnotSlice<'ast>,
        return_ty: Option<TyAnnot<'ast>>,
    },
    ImplTrait(ConstraintRequirement<'ast>),
    Wildcard,
}
