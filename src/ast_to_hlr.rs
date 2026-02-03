#![allow(unused)]

use std::collections::{HashMap, VecDeque};

use crate::{
    ast,
    ctxt::{self, fns},
    hlr,
};

pub fn ast_to_hlr(
    ctxt: &ctxt::Ctxt,
    fn_: fns::Fn,
    ast: &ast::Ast,
    ast_body: &ast::Block,
) -> AstToHlrResult<(hlr::Hlr, hlr::Expr)> {
    let converter = AstToHlr::new(ctxt, fn_, ast);
    converter.lower_block(ast_body)
}

struct AstToHlr<'a> {
    ctxt: &'a ctxt::Ctxt,
    fn_: fns::Fn,
    ast: &'a ast::Ast,
    hlr: hlr::Hlr,

    scopes: VecDeque<Scope>,
    blocks: VecDeque<Vec<hlr::Stmt>>,

    next_var_id: hlr::VarId,
    self_var_id: Option<hlr::VarId>,
}

pub type AstToHlrResult<T> = Result<T, AstToHlrError>;
pub struct AstToHlrError {
    msg: String,
}

impl<'a> AstToHlr<'a> {
    fn new(ctxt: &'a ctxt::Ctxt, fn_: fns::Fn, ast: &'a ast::Ast) -> Self {
        Self {
            ctxt,
            fn_,
            ast,
            hlr: hlr::Hlr::new(),

            scopes: VecDeque::new(),
            blocks: VecDeque::new(),

            next_var_id: hlr::VarId(0),
            self_var_id: None,
        }
    }

    pub fn lower_block(mut self, block: &ast::Block) -> AstToHlrResult<(hlr::Hlr, hlr::Expr)> {
        let signature = self.get_signature();
        if signature.var_args {
            return Err(AstToHlrError {
                msg: "Varargs functions are not supported in HLR".to_string(),
            });
        }

        let params = signature.params.clone();

        self.scopes.push_back(Scope::default());
        for fns::FnParam { kind: name, ty } in params {
            let var_id = self.get_next_var_id();

            match name {
                fns::FnParamKind::Regular(name) => {
                    self.scopes
                        .back_mut()
                        .unwrap()
                        .bindings
                        .insert(name.to_string(), var_id);
                }
                fns::FnParamKind::Self_ | fns::FnParamKind::SelfByRef => {
                    self.self_var_id = Some(var_id);
                }
            }
        }

        self.start_new_block();

        let return_val = self.build_block(block)?;
        let body = self.release_current_block(return_val);

        Ok((self.hlr, body))
    }

    fn get_next_var_id(&mut self) -> hlr::VarId {
        let id = self.next_var_id.0;
        self.next_var_id.0 += 1;
        hlr::VarId(id)
    }

    fn get_signature(&self) -> &fns::FnSig {
        self.ctxt.fns.get_sig(self.fn_).unwrap()
    }

    fn start_new_block(&mut self) {
        self.blocks.push_back(Vec::new());
    }

    fn release_current_block(&mut self, trailing: hlr::Expr) -> hlr::Expr {
        let stmts = self.blocks.pop_back().expect("self.blocks should never be empty");
        let block = hlr::ExprDef::Block { stmts, trailing };
        self.hlr.new_expr(block)
    }

    fn build_block(&mut self, body: &ast::Block) -> AstToHlrResult<hlr::Expr> {
        self.scopes.push_back(Scope::default());

        for &stmt in self.ast.stmt_slice(body.stmts) {
            self.lower_stmt(stmt)?;
        }

        let output = match body.return_expr {
            Some(expr) => self.lower_expr(expr)?,
            None => {
                let unit_expr = hlr::ExprDef::Tuple(vec![]);
                self.hlr.new_expr(unit_expr)
            }
        };

        self.scopes.pop_back();
        Ok(output)
    }

    fn lower_stmt(&mut self, stmt: ast::Stmt) -> AstToHlrResult<()> {
        use ast::StmtKind::*;

        let stmt = self.ast.stmt(stmt);

        match *stmt {
            Let {
                ref name,
                ty_annot,
                value,
            } => self.lower_let_stmt(name, ty_annot, value),
            Expr(expr) => self.lower_expr_stmt(expr),
            Return(expr) => self.lower_return_stmt(expr),
            Break => self.lower_break_stmt(),
        }
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> AstToHlrResult<hlr::Expr> {
        todo!()
    }

    fn lower_let_stmt(&mut self, name: &str, ty_annot: Option<ast::TyAnnot>, value: ast::Expr) -> AstToHlrResult<()> {
        let init = self.lower_expr(value)?;

        let var = self.get_next_var_id();
        self.scopes.back_mut().unwrap().bindings.insert(name.to_string(), var);

        let ty = ty_annot.map(|ty_annot| self.lower_ty_annot(ty_annot)).transpose()?;

        let stmt = hlr::StmtDef::Let { var, ty, init };
        self.push_stmt(stmt)
    }

    fn lower_expr_stmt(&mut self, expr: ast::Expr) -> AstToHlrResult<()> {
        let expr = self.lower_expr(expr)?;
        let stmt = hlr::StmtDef::Expr(expr);
        self.push_stmt(stmt)
    }

    fn lower_return_stmt(&mut self, expr: Option<ast::Expr>) -> AstToHlrResult<()> {
        let return_expr = expr.map(|e| self.lower_expr(e)).transpose()?;
        let stmt = hlr::StmtDef::Return(return_expr);
        self.push_stmt(stmt)
    }

    fn lower_break_stmt(&mut self) -> AstToHlrResult<()> {
        let stmt = hlr::StmtDef::Break;
        self.push_stmt(stmt)
    }

    fn push_stmt(&mut self, stmt: hlr::StmtDef) -> AstToHlrResult<()> {
        let stmt = self.hlr.new_stmt(stmt);
        self.blocks.back_mut().unwrap().push(stmt);
        Ok(())
    }

    fn lower_ty_annot(&self, ty_annot: ast::TyAnnot) -> AstToHlrResult<hlr::TyAnnot> {
        todo!()
    }
}

#[derive(Default)]
struct Scope {
    bindings: HashMap<String, hlr::VarId>,
}
