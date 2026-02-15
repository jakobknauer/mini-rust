use crate::hlr::{Expr, TyAnnot, VarId};

pub type Stmt<'hlr> = &'hlr StmtDef<'hlr>;

pub type StmtSlice<'hlr> = &'hlr [Stmt<'hlr>];

#[derive(Clone, Debug)]
pub enum StmtDef<'hlr> {
    Expr(Expr<'hlr>),

    Let {
        var: VarId,
        ty: Option<TyAnnot<'hlr>>,
        init: Expr<'hlr>,
    },

    Break,
    Return(Option<Expr<'hlr>>),
}
