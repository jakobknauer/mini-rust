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
    let mut converter = AstToHlr::new(ctxt, fn_, ast);
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

    fn build_block(&self, body: &ast::Block) -> AstToHlrResult<hlr::Expr> {
        todo!()
    }
}

#[derive(Default)]
struct Scope {
    bindings: HashMap<String, hlr::VarId>,
}
