#[derive(Debug, PartialEq, Eq, Default)]
pub struct Program {
    pub fns: Vec<Fn>,
    pub structs: Vec<Struct>,
    pub enums: Vec<Enum>,
    pub impls: Vec<Impl>,
    pub traits: Vec<Trait>,
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
        trait_args: Vec<TyAnnot>,
    },
    Callable {
        params: Vec<TyAnnot>,
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
    pub args: Vec<TyAnnot>,
}

impl std::fmt::Display for TraitAnnot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.args.is_empty() {
            write!(f, "{}", self.name)
        } else {
            write!(
                f,
                "{}<{}>",
                self.name,
                self.args
                    .iter()
                    .map(|annot| format!("{}", annot))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        }
    }
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
    pub return_expr: Option<Box<ExprKind>>,
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

impl std::fmt::Display for QualifiedPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(trait_) = &self.trait_ {
            write!(f, "<{} as {}>::{}", self.ty, trait_, self.path)
        } else {
            write!(f, "<{}>::{}", self.ty, self.path)
        }
    }
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
                .map(|annot| format!("{}", annot))
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
        value: ExprKind,
    },
    Expr(ExprKind),
    Return(Option<ExprKind>),
    Break,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExprKind {
    Lit(Lit),
    Path(Path),
    QualifiedPath(QualifiedPath),
    Tuple(Vec<ExprKind>),
    BinaryOp {
        left: Box<ExprKind>,
        operator: BinaryOperator,
        right: Box<ExprKind>,
    },
    UnaryOp {
        operator: UnaryOperator,
        operand: Box<ExprKind>,
    },
    Assign {
        target: Box<ExprKind>,
        value: Box<ExprKind>,
    },
    Call {
        callee: Box<ExprKind>,
        arguments: Vec<ExprKind>,
    },
    MthdCall {
        obj: Box<ExprKind>,
        mthd: PathSegment,
        arguments: Vec<ExprKind>,
    },
    Struct {
        ty_path: Path,
        fields: Vec<(String, ExprKind)>,
    },
    FieldAccess {
        obj: Box<ExprKind>,
        field: FieldDescriptor,
    },
    Block(Block),
    If {
        cond: Box<ExprKind>,
        then: Block,
        else_: Option<Block>,
    },
    Loop {
        body: Block,
    },
    While {
        condition: Box<ExprKind>,
        body: Block,
    },
    Match {
        scrutinee: Box<ExprKind>,
        arms: Vec<MatchArm>,
    },
    Deref {
        base: Box<ExprKind>,
    },
    AddrOf {
        base: Box<ExprKind>,
    },
    As {
        expr: Box<ExprKind>,
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

#[derive(Clone, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    Not,
    Negative,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub value: Box<ExprKind>,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TyAnnot {
    Path(Path),
    Tuple(Vec<TyAnnot>),
    Ref(Box<TyAnnot>),
    Ptr(Box<TyAnnot>),
    Fn {
        param_tys: Vec<TyAnnot>,
        return_ty: Option<Box<TyAnnot>>,
    },
    Wildcard,
}

impl std::fmt::Display for TyAnnot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TyAnnot::Path(path) => write!(f, "{}", path),
            TyAnnot::Tuple(ty_annots) => write!(
                f,
                "({})",
                ty_annots.iter().map(|ty| ty.to_string()).collect::<Vec<_>>().join(", ")
            ),
            TyAnnot::Ref(ty_annot) => write!(f, "&{}", ty_annot),
            TyAnnot::Ptr(ty_annot) => write!(f, "*{}", ty_annot),
            TyAnnot::Fn { param_tys, return_ty } => write!(
                f,
                "fn({}) -> {}",
                param_tys.iter().map(|ty| ty.to_string()).collect::<Vec<_>>().join(", "),
                match return_ty {
                    Some(ty) => ty.to_string(),
                    None => "()".to_string(),
                }
            ),
            TyAnnot::Wildcard => write!(f, "_"),
        }
    }
}
