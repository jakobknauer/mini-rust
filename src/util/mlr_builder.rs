use std::collections::{HashMap, VecDeque};

use crate::{
    ctxt::{
        self, fns,
        mlr::{self, Mlr},
        ty,
    },
    h2m::{H2MError, H2MResult},
    hlr,
    typechecker::Typechecker,
};

pub struct MlrBuilder<'a> {
    target_fn: fns::Fn,
    ctxt: &'a mut ctxt::Ctxt,

    scopes: VecDeque<Scope>,
    blocks: VecDeque<Vec<mlr::Stmt>>,
    receiver_loc: Option<mlr::Loc>,
}

struct Scope {
    bindings: HashMap<String, mlr::Loc>,
}

impl Scope {
    fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }
}

impl<'a> MlrBuilder<'a> {
    pub fn new(target_fn: fns::Fn, ctxt: &'a mut ctxt::Ctxt) -> Self {
        Self {
            target_fn,
            ctxt,

            scopes: VecDeque::new(),
            blocks: VecDeque::new(),
            receiver_loc: None,
        }
    }

    pub fn get_signature(&self) -> &fns::FnSig {
        self.ctxt
            .fns
            .get_sig(self.target_fn)
            .expect("function signature should be registered")
    }

    fn current_scope(&mut self) -> &mut Scope {
        self.scopes.back_mut().expect("self.scopes should never be empty")
    }

    pub fn push_scope(&mut self) {
        self.scopes.push_back(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop_back();
    }

    pub fn add_binding(&mut self, name: &str, loc: mlr::Loc) {
        self.current_scope().bindings.insert(name.to_string(), loc);
    }

    pub fn start_new_block(&mut self) {
        self.blocks.push_back(Vec::new());
    }

    pub fn release_current_block(&mut self) -> mlr::Stmt {
        let stmts = self.blocks.pop_back().expect("self.blocks should never be empty");
        let block = mlr::StmtDef::Block(stmts);
        self.ctxt.mlr.insert_stmt(block)
    }

    pub fn end_and_insert_current_block(&mut self) {
        let block_stmt = self.release_current_block();
        self.blocks
            .back_mut()
            .expect("self.blocks should never be empty")
            .push(block_stmt);
    }

    pub fn typechecker(&mut self) -> Typechecker<'_> {
        Typechecker::new(self.ctxt, self.target_fn)
    }

    fn insert_val(&mut self, val_def: mlr::ValDef) -> H2MResult<mlr::Val> {
        let val = self.ctxt.mlr.insert_val(val_def);
        self.typechecker().infer_val_ty(val)?;
        Ok(val)
    }

    pub fn insert_call_val(&mut self, callable: mlr::Op, args: Vec<mlr::Op>) -> H2MResult<mlr::Val> {
        let val = mlr::ValDef::Call { callable, args };
        self.insert_val(val)
    }

    pub fn insert_use_val(&mut self, op: mlr::Op) -> H2MResult<mlr::Val> {
        let val = mlr::ValDef::Use(op);
        self.insert_val(val)
    }

    pub fn insert_use_place_val(&mut self, place: mlr::Place) -> H2MResult<mlr::Val> {
        let op = self.insert_copy_op(place)?;
        self.insert_use_val(op)
    }

    pub fn insert_addr_of_val(&mut self, place: mlr::Place) -> H2MResult<mlr::Val> {
        let val = mlr::ValDef::AddrOf(place);
        self.insert_val(val)
    }

    pub fn insert_as_val(&mut self, op: mlr::Op, target_ty: ty::Ty) -> H2MResult<mlr::Val> {
        let val_def = mlr::ValDef::As { op, target_ty };
        self.insert_val(val_def)
    }

    pub fn insert_size_of_val(&mut self, ty: ty::Ty) -> H2MResult<mlr::Val> {
        let val_def = mlr::ValDef::SizeOf(ty);
        self.insert_val(val_def)
    }

    fn insert_stmt(&mut self, stmt_def: mlr::StmtDef) -> H2MResult<mlr::Stmt> {
        let stmt = self.ctxt.mlr.insert_stmt(stmt_def);
        self.typechecker().check_stmt_ty(stmt)?;

        self.blocks
            .back_mut()
            .expect("self.blocks should not be empty")
            .push(stmt);
        Ok(stmt)
    }

    pub fn insert_if_stmt(&mut self, cond: mlr::Op, then_: mlr::Stmt, else_: mlr::Stmt) -> H2MResult<mlr::Stmt> {
        let if_ = mlr::StmtDef::If(mlr::If {
            cond,
            then: then_,
            else_,
        });
        self.insert_stmt(if_)
    }

    pub fn insert_loop_stmt(&mut self, body: mlr::Stmt) -> H2MResult<mlr::Stmt> {
        let val = mlr::StmtDef::Loop { body };
        self.insert_stmt(val)
    }

    pub fn insert_alloc_stmt(&mut self, loc: mlr::Loc) -> H2MResult<mlr::Stmt> {
        let stmt = mlr::StmtDef::Alloc { loc };
        self.insert_stmt(stmt)
    }

    pub fn insert_fresh_alloc(&mut self) -> H2MResult<mlr::Place> {
        let undef = self.ctxt.tys.new_undefined_ty();
        self.insert_alloc_with_ty(undef)
    }

    pub fn insert_alloc_with_ty(&mut self, ty: ty::Ty) -> H2MResult<mlr::Place> {
        let loc = self.ctxt.mlr.insert_typed_loc(ty);
        self.insert_alloc_stmt(loc)?;
        self.insert_loc_place(loc)
    }

    pub fn insert_break_stmt(&mut self) -> H2MResult<mlr::Stmt> {
        let stmt = mlr::StmtDef::Break;
        self.insert_stmt(stmt)
    }

    pub fn insert_assign_stmt(&mut self, place: mlr::Place, value: mlr::Val) -> H2MResult<mlr::Stmt> {
        let stmt = mlr::StmtDef::Assign { place, value };
        self.insert_stmt(stmt)
    }

    pub fn insert_assign_to_loc_stmt(&mut self, loc: mlr::Loc, value: mlr::Val) -> H2MResult<mlr::Stmt> {
        let place = mlr::PlaceDef::Loc(loc);
        let place = self.insert_place(place)?;
        self.insert_assign_stmt(place, value)
    }

    pub fn insert_return_stmt(&mut self, value: mlr::Val) -> H2MResult<mlr::Stmt> {
        let stmt = mlr::StmtDef::Return { value };
        self.insert_stmt(stmt)
    }

    fn insert_place(&mut self, place_def: mlr::PlaceDef) -> H2MResult<mlr::Place> {
        let place = self.ctxt.mlr.insert_place(place_def);
        self.typechecker().infer_place_ty(place)?;
        Ok(place)
    }

    pub fn insert_loc_place(&mut self, loc: mlr::Loc) -> H2MResult<mlr::Place> {
        let place = mlr::PlaceDef::Loc(loc);
        self.insert_place(place)
    }

    pub fn insert_enum_discriminant_place(&mut self, base: mlr::Place) -> H2MResult<mlr::Place> {
        let place = mlr::PlaceDef::EnumDiscriminant { base };
        self.insert_place(place)
    }

    pub fn insert_project_to_variant_place(&mut self, base: mlr::Place, variant_index: usize) -> H2MResult<mlr::Place> {
        let place = mlr::PlaceDef::ProjectToVariant { base, variant_index };
        self.insert_place(place)
    }

    pub fn insert_field_access_place(&mut self, base: mlr::Place, field_index: usize) -> H2MResult<mlr::Place> {
        let place = mlr::PlaceDef::FieldAccess { base, field_index };
        self.insert_place(place)
    }

    pub fn insert_deref_place(&mut self, op: mlr::Op) -> H2MResult<mlr::Place> {
        let place = mlr::PlaceDef::Deref(op);
        self.insert_place(place)
    }

    fn insert_op(&mut self, op_def: mlr::OpDef) -> H2MResult<mlr::Op> {
        let op = self.ctxt.mlr.insert_op(op_def);
        self.typechecker().infer_op_ty(op)?;
        Ok(op)
    }

    pub fn insert_fn_op(&mut self, fn_: fns::Fn) -> H2MResult<mlr::Op> {
        self.insert_gen_fn_op(fn_, Vec::new(), Vec::new())
    }

    pub fn insert_gen_fn_op(
        &mut self,
        fn_: fns::Fn,
        gen_args: Vec<ty::Ty>,
        env_gen_args: Vec<ty::Ty>,
    ) -> H2MResult<mlr::Op> {
        let fn_spec = fns::FnSpecialization {
            fn_,
            gen_args,
            env_gen_args,
        };
        self.insert_fn_spec_op(fn_spec)
    }

    pub fn insert_fn_spec_op(&mut self, fn_spec: fns::FnSpecialization) -> H2MResult<mlr::Op> {
        self.ctxt.fns.specialize_fn(self.target_fn, fn_spec.clone());
        let op = mlr::OpDef::Fn(fn_spec);
        self.insert_op(op)
    }

    pub fn insert_trait_method_op(&mut self, trait_method: fns::TraitMethod) -> Result<mlr::Op, H2MError> {
        self.ctxt
            .fns
            .specialize_trait_method(self.target_fn, trait_method.clone());
        let op = mlr::OpDef::TraitMethod(trait_method);
        self.insert_op(op)
    }

    pub fn insert_int_op(&mut self, int: i64) -> H2MResult<mlr::Op> {
        let val = mlr::OpDef::Const(mlr::Const::Int(int));
        self.insert_op(val)
    }

    pub fn insert_bool_op(&mut self, boolean: bool) -> H2MResult<mlr::Op> {
        let op = mlr::OpDef::Const(mlr::Const::Bool(boolean));
        self.insert_op(op)
    }

    pub fn insert_unit_op(&mut self) -> H2MResult<mlr::Op> {
        let op = mlr::OpDef::Const(mlr::Const::Unit);
        self.insert_op(op)
    }

    pub fn insert_c_char_op(&mut self, c_char: u8) -> H2MResult<mlr::Op> {
        let op = mlr::OpDef::Const(mlr::Const::CChar(c_char));
        self.insert_op(op)
    }

    pub fn insert_c_string_op(&mut self, c_string: &[u8]) -> H2MResult<mlr::Op> {
        let op = mlr::OpDef::Const(mlr::Const::CString(c_string.to_vec()));
        self.insert_op(op)
    }

    pub fn insert_copy_op(&mut self, place: mlr::Place) -> H2MResult<mlr::Op> {
        let op = mlr::OpDef::Copy(place);
        self.insert_op(op)
    }

    pub fn insert_typed_loc(&mut self, ty: ty::Ty) -> H2MResult<mlr::Loc> {
        let loc = self.ctxt.mlr.insert_typed_loc(ty);
        Ok(loc)
    }

    /// TODO move resolution functionality to an impl block in another submodule,
    /// or create resolver submodule.
    pub fn resolve_name(&mut self, name: &str) -> H2MResult<mlr::Op> {
        if let Some(loc) = self.resolve_name_to_location(name) {
            let place = self.insert_loc_place(loc)?;
            self.insert_copy_op(place)
        } else if let Some(fn_) = self.ctxt.fns.get_fn_by_name(name) {
            self.insert_fn_op(fn_)
        } else {
            Err(H2MError::UnresolvableSymbol { name: name.to_string() })
        }
    }

    pub fn resolve_name_to_location(&self, name: &str) -> Option<mlr::Loc> {
        self.scopes
            .iter()
            .rev()
            .filter_map(|scope| scope.bindings.get(name))
            .next()
            .cloned()
    }

    pub fn try_resolve_enum_variant(&self, variant_name: &str) -> Option<(ty::Enum, usize)> {
        for (enum_, enum_def) in self.ctxt.tys.get_all_enums() {
            for (idx, variant) in enum_def.variants.iter().enumerate() {
                if variant.name == variant_name {
                    return Some((enum_, idx));
                }
            }
        }
        None
    }

    pub fn resolve_hlr_ty_annot(&mut self, annot: &hlr::TyAnnot) -> H2MResult<ty::Ty> {
        let gen_params = &self.get_signature().gen_params;
        let env_gen_params = &self.get_signature().env_gen_params;
        let all_gen_params: Vec<_> = gen_params.iter().chain(env_gen_params.iter()).cloned().collect();

        let ty = self
            .ctxt
            .tys
            .try_resolve_hlr_annot(annot, &all_gen_params, None)
            .ok_or(H2MError::UnresolvableTyAnnot)?;
        Ok(ty)
    }

    pub fn resolve_name_to_fn(&self, name: &str) -> H2MResult<fns::Fn> {
        if let Some(fn_) = self.ctxt.fns.get_fn_by_name(name) {
            Ok(fn_)
        } else {
            Err(H2MError::UnresolvableSymbol { name: name.to_string() })
        }
    }

    pub fn tys(&mut self) -> &mut ctxt::TyReg {
        &mut self.ctxt.tys
    }

    pub fn fns(&mut self) -> &mut ctxt::FnReg {
        &mut self.ctxt.fns
    }

    pub fn mlr(&mut self) -> &mut Mlr {
        &mut self.ctxt.mlr
    }

    pub fn traits(&mut self) -> &mut ctxt::TraitReg {
        &mut self.ctxt.traits
    }

    pub fn register_receiver_loc(&mut self, receiver_loc: mlr::Loc) {
        self.receiver_loc = Some(receiver_loc);
    }

    pub fn get_receiver_loc(&self) -> Option<mlr::Loc> {
        self.receiver_loc
    }
}
