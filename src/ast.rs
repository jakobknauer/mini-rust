pub mod builder;

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Ast {
    pub fns: Vec<Fn>,
    pub structs: Vec<Struct>,
    pub enums: Vec<Enum>,
    pub impls: Vec<Impl>,
    pub traits: Vec<Trait>,

    pub exprs: Vec<ExprKind>,
    pub expr_slices: Vec<Expr>,

    pub ty_annots: Vec<TyAnnotKind>,
    pub ty_annot_slices: Vec<TyAnnot>,
}

impl Ast {
    pub fn new_expr(&mut self, expr: ExprKind) -> Expr {
        self.exprs.push(expr);
        Expr(self.exprs.len() - 1)
    }

    pub fn expr(&self, expr: Expr) -> &ExprKind {
        &self.exprs[expr.0]
    }

    pub fn new_expr_slice(&mut self, exprs: &[Expr]) -> ExprSlice {
        self.expr_slices.extend_from_slice(exprs);
        ExprSlice(self.expr_slices.len() - exprs.len(), exprs.len())
    }

    pub fn expr_slice(&self, ExprSlice(start, len): ExprSlice) -> &[Expr] {
        &self.expr_slices[start..start + len]
    }

    pub fn new_ty_annot(&mut self, annot: TyAnnotKind) -> TyAnnot {
        self.ty_annots.push(annot);
        TyAnnot(self.ty_annots.len() - 1)
    }

    pub fn ty_annot(&self, ty_annot: TyAnnot) -> &TyAnnotKind {
        &self.ty_annots[ty_annot.0]
    }

    pub fn new_ty_annot_slice(&mut self, ty_annots: &[TyAnnot]) -> TyAnnotSlice {
        self.ty_annot_slices.extend_from_slice(ty_annots);
        TyAnnotSlice(self.ty_annot_slices.len() - ty_annots.len(), ty_annots.len())
    }

    pub fn ty_annot_slice(&self, TyAnnotSlice(start, len): TyAnnotSlice) -> &[TyAnnot] {
        &self.ty_annot_slices[start..start + len]
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Fn {
    pub name: String,
    pub gen_params: Vec<String>,
    pub params: Vec<Param>,
    pub var_args: bool,
    pub return_ty: Option<TyAnnot>,
    pub constraints: Vec<Constraint>,
    pub body: Option<Block>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Param {
    Regular { name: String, ty: TyAnnot },
    Receiver,
    ReceiverByRef,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Constraint {
    pub subject: String,
    pub requirement: ConstraintRequirement,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ConstraintRequirement {
    Trait {
        trait_name: String,
        trait_args: TyAnnotSlice,
    },
    Callable {
        params: TyAnnotSlice,
        return_ty: Option<TyAnnot>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Struct {
    pub name: String,
    pub gen_params: Vec<String>,
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub ty: TyAnnot,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Enum {
    pub name: String,
    pub gen_params: Vec<String>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Vec<StructField>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Impl {
    pub gen_params: Vec<String>,
    pub trait_annot: Option<TraitAnnot>,
    pub ty: TyAnnot,
    pub mthds: Vec<Fn>,
    pub assoc_tys: Vec<AssocTy>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct AssocTy {
    pub name: String,
    pub ty: TyAnnot,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TraitAnnot {
    pub name: String,
    pub args: TyAnnotSlice,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Trait {
    pub name: String,
    pub gen_params: Vec<String>,
    pub mthds: Vec<Fn>,
    pub assoc_ty_names: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub return_expr: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

impl std::fmt::Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.segments
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join("::")
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct QualifiedPath {
    pub ty: TyAnnot,
    pub trait_: Option<TraitAnnot>,
    pub path: Path,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PathSegment {
    Ident(String),
    Generic(GenPathSegment),
    Self_,
}

impl std::fmt::Display for PathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PathSegment::Ident(ident) => write!(f, "{}", ident),
            PathSegment::Generic(segment) => write!(f, "{}", segment),
            PathSegment::Self_ => write!(f, "Self"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenPathSegment {
    pub ident: String,
    pub gen_args: Vec<TyAnnot>,
}

impl std::fmt::Display for GenPathSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}<{}>",
            self.ident,
            self.gen_args
                .iter()
                .map(|annot| format!("{}", annot.0))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    Let {
        name: String,
        ty_annot: Option<TyAnnot>,
        value: Expr,
    },
    Expr(Expr),
    Return(Option<Expr>),
    Break,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Expr(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ExprSlice(usize, usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind {
    Lit(Lit),
    Path(Path),
    QualifiedPath(QualifiedPath),
    Tuple(ExprSlice),
    BinaryOp {
        left: Expr,
        operator: BinaryOperator,
        right: Expr,
    },
    UnaryOp {
        operator: UnaryOperator,
        operand: Expr,
    },
    Assign {
        target: Expr,
        value: Expr,
    },
    Call {
        callee: Expr,
        args: ExprSlice,
    },
    MthdCall {
        obj: Expr,
        mthd: PathSegment,
        args: ExprSlice,
    },
    Struct {
        ty_path: Path,
        fields: Vec<(String, Expr)>,
    },
    FieldAccess {
        obj: Expr,
        field: FieldDescriptor,
    },
    Block(Block),
    If {
        cond: Expr,
        then: Block,
        else_: Option<Block>,
    },
    Loop {
        body: Block,
    },
    While {
        cond: Expr,
        body: Block,
    },
    Match {
        scrutinee: Expr,
        arms: Vec<MatchArm>,
    },
    Deref {
        base: Expr,
    },
    AddrOf {
        base: Expr,
    },
    As {
        expr: Expr,
        target_ty: TyAnnot,
    },
    Self_,
    Closure {
        params: Vec<ClosureParam>,
        return_ty: Option<TyAnnot>,
        body: Block,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FieldDescriptor {
    Named(PathSegment),
    Indexed(usize),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ClosureParam {
    pub name: String,
    pub ty: Option<TyAnnot>,
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
pub struct MatchArm {
    pub pattern: Pattern,
    pub value: Expr,
}

type Pattern = StructPattern;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructPattern {
    pub variant: String,
    pub fields: Vec<StructPatternField>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructPatternField {
    pub field_name: String,
    pub binding_name: String,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TyAnnot(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TyAnnotSlice(usize, usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TyAnnotKind {
    Path(Path),
    Tuple(TyAnnotSlice),
    Ref(TyAnnot),
    Ptr(TyAnnot),
    Fn {
        param_tys: TyAnnotSlice,
        return_ty: Option<TyAnnot>,
    },
    Wildcard,
}
