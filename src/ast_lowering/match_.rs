use std::collections::{HashMap, HashSet};

use crate::{
    ast,
    ast_lowering::{AstLoweringError, AstLoweringResult, Scope},
    ctxt::ty,
    hlr,
};

struct PatternLoweringCtxt {
    /// Names already bound in this pattern; maps each to its VarId.
    /// Binding a name already present is a duplicate error.
    previously_bound: HashMap<String, hlr::VarId>,
    /// Inside the 2nd+ alternative of an Or pattern: the canonical name->VarId
    /// established by the first alternative. Identifiers must draw from here.
    /// If None, we are not in an Or pattern and there is no canonical set of bindings, so new
    /// bindings are created as we go and added to previously_bound.
    bound_in_siblings: Option<HashMap<String, hlr::VarId>>,
}

impl PatternLoweringCtxt {
    fn new() -> Self {
        Self {
            previously_bound: HashMap::new(),
            bound_in_siblings: None,
        }
    }

    fn with_bound_in_sibling(bound_in_sibling: HashMap<String, hlr::VarId>) -> Self {
        Self {
            previously_bound: HashMap::new(),
            bound_in_siblings: Some(bound_in_sibling),
        }
    }
}

impl<'a, 'ctxt, 'ast> super::AstLowerer<'a, 'ctxt> {
    pub(super) fn lower_match_expr(
        &mut self,
        scrutinee: ast::Expr<'ast>,
        arms: &[ast::MatchArm<'ast>],
    ) -> AstLoweringResult<hlr::Expr<'ctxt>> {
        let scrutinee = self.lower_expr(scrutinee)?;

        let hlr_arms: Vec<_> = arms
            .iter()
            .map(|arm| {
                self.scopes.push_back(Scope::default());
                let pattern = self.lower_pattern_inner(arm.pattern, &mut PatternLoweringCtxt::new())?;
                let body = self.lower_expr(arm.value)?;
                self.scopes.pop_back();
                Ok(hlr::MatchArm { pattern, body })
            })
            .collect::<AstLoweringResult<_>>()?;
        let hlr_arms = self.hlr.match_arms(hlr_arms);

        let expr = hlr::ExprDef::Match {
            scrutinee,
            arms: hlr_arms,
        };
        Ok(self.hlr.expr(expr))
    }

    fn lower_pattern_inner(
        &mut self,
        pattern: ast::Pattern,
        ctxt: &mut PatternLoweringCtxt,
    ) -> AstLoweringResult<hlr::Pattern<'ctxt>> {
        match pattern {
            ast::PatternKind::Or(alternatives) => self.lower_or_pattern(alternatives, ctxt),
            ast::PatternKind::Ref(inner) => {
                let inner = self.lower_pattern_inner(inner, ctxt)?;
                Ok(self.hlr.pattern(hlr::PatternKind::Ref(inner)))
            }
            ast::PatternKind::RefMut(inner) => {
                let inner = self.lower_pattern_inner(inner, ctxt)?;
                Ok(self.hlr.pattern(hlr::PatternKind::RefMut(inner)))
            }
            ast::PatternKind::Path(path) => self.lower_path_pattern(path, ctxt),
            ast::PatternKind::Struct(pattern) => self.lower_struct_pattern(pattern, ctxt),
            ast::PatternKind::Lit(lit) => self.lower_lit_pattern(lit),
            ast::PatternKind::Tuple(sub_patterns) => self.lower_tuple_pattern(sub_patterns, ctxt),
            ast::PatternKind::MutBinding(name) => self.lower_identifier_pattern(name, true, ctxt),
            ast::PatternKind::Wildcard => Ok(self.hlr.pattern(hlr::PatternKind::Wildcard)),
        }
    }

    fn lower_path_pattern(
        &mut self,
        path: &ast::Path,
        ctxt: &mut PatternLoweringCtxt,
    ) -> AstLoweringResult<hlr::Pattern<'ctxt>> {
        if let Ok(constructor) = self.resolve_path_to_constructor(path) {
            let fields = self.hlr.pattern_fields(vec![]);
            return match constructor {
                hlr::Val::Variant(..) => Ok(self.hlr.pattern(hlr::PatternKind::Variant(hlr::VariantPattern {
                    variant: constructor,
                    fields,
                }))),
                hlr::Val::Struct(..) => Ok(self
                    .hlr
                    .pattern(hlr::PatternKind::Struct(hlr::StructPattern { constructor, fields }))),
                _ => Err(AstLoweringError::ExpectedConstructor),
            };
        }

        match path.segments.as_slice() {
            [segment] if !segment.is_self && segment.args.is_none() => {
                self.lower_identifier_pattern(&segment.ident, false, ctxt)
            }
            _ => Err(AstLoweringError::UnresolvedPath {
                path: path
                    .segments
                    .iter()
                    .map(|s| s.ident.as_str())
                    .collect::<Vec<_>>()
                    .join("::"),
            }),
        }
    }

    fn lower_lit_pattern(&mut self, lit: &ast::Lit) -> AstLoweringResult<hlr::Pattern<'ctxt>> {
        let lit = match lit {
            ast::Lit::Int(i, suffix) => hlr::Lit::Int(
                *i,
                suffix.map(ast::IntSuffix::into_int_width).unwrap_or(ty::IntWidth::I32),
            ),
            ast::Lit::Float(_, _) => {
                return Err(AstLoweringError::FloatLiteralInPattern);
            }
            ast::Lit::Bool(b) => hlr::Lit::Bool(*b),
            ast::Lit::CChar(c) => hlr::Lit::CChar(*c),
            ast::Lit::CString(_) => {
                return Err(AstLoweringError::StringLiteralInPattern);
            }
        };
        Ok(self.hlr.pattern(hlr::PatternKind::Lit(lit)))
    }

    fn lower_tuple_pattern(
        &mut self,
        sub_patterns: &[ast::Pattern],
        ctxt: &mut PatternLoweringCtxt,
    ) -> AstLoweringResult<hlr::Pattern<'ctxt>> {
        let lowered: Vec<_> = sub_patterns
            .iter()
            .map(|&p| self.lower_pattern_inner(p, ctxt))
            .collect::<AstLoweringResult<_>>()?;
        let lowered = self.hlr.patterns(lowered);
        Ok(self.hlr.pattern(hlr::PatternKind::Tuple(lowered)))
    }

    fn lower_identifier_pattern(
        &mut self,
        name: &str,
        mutable: bool,
        ctxt: &mut PatternLoweringCtxt,
    ) -> AstLoweringResult<hlr::Pattern<'ctxt>> {
        if ctxt.previously_bound.contains_key(name) {
            return Err(AstLoweringError::IdentifierBoundMoreThanOnce { name: name.to_owned() });
        }
        let var_id = match &ctxt.bound_in_siblings {
            Some(bound_in_siblings) => match bound_in_siblings.get(name) {
                Some(&id) => id,
                None => {
                    return Err(AstLoweringError::VariableNotBoundInAllAlternatives { name: name.to_owned() });
                }
            },
            None => self.hlr.named_var_id(name),
        };
        ctxt.previously_bound.insert(name.to_owned(), var_id);
        self.scopes.back_mut().unwrap().bindings.insert(name.to_owned(), var_id);
        Ok(self.hlr.pattern(hlr::PatternKind::Identifier { var_id, mutable }))
    }

    fn lower_or_pattern(
        &mut self,
        alternatives: &[ast::Pattern],
        outer_ctxt: &mut PatternLoweringCtxt,
    ) -> AstLoweringResult<hlr::Pattern<'ctxt>> {
        let [first_pattern, other_patterns @ ..] = alternatives else {
            todo!()
        };

        let mut first_ctxt = PatternLoweringCtxt {
            // We use an empty previously_bound here in order to collect the newly introduced bindings in
            // the first pattern, which then become the 'bound_in_siblings' bindings for the other patterns.
            // Later, we check against the outer context to ensure no introduced name is already
            // disallowed there.
            previously_bound: HashMap::new(),
            bound_in_siblings: outer_ctxt.bound_in_siblings.clone(),
        };

        let first = self.lower_pattern_inner(first_pattern, &mut first_ctxt)?;
        let canonical = first_ctxt.previously_bound;

        let mut lowered = vec![first];
        for &alt in other_patterns {
            let mut alt_ctxt = PatternLoweringCtxt::with_bound_in_sibling(canonical.clone());
            let hlr_alt = self.lower_pattern_inner(alt, &mut alt_ctxt)?;
            for name in canonical.keys() {
                if !alt_ctxt.previously_bound.contains_key(name) {
                    return Err(AstLoweringError::VariableNotBoundInAllAlternatives { name: name.clone() });
                }
            }
            lowered.push(hlr_alt);
        }

        for (name, &var_id) in &canonical {
            if outer_ctxt.previously_bound.contains_key(name) {
                return Err(AstLoweringError::IdentifierBoundMoreThanOnce { name: name.clone() });
            }
            outer_ctxt.previously_bound.insert(name.clone(), var_id);
        }

        let lowered = self.hlr.patterns(lowered);
        Ok(self.hlr.pattern(hlr::PatternKind::Or(lowered)))
    }

    fn lower_struct_pattern(
        &mut self,
        pattern: &ast::StructPattern,
        ctxt: &mut PatternLoweringCtxt,
    ) -> AstLoweringResult<hlr::Pattern<'ctxt>> {
        let constructor = self.resolve_path_to_constructor(&pattern.path)?;
        match constructor {
            hlr::Val::Variant(enum_, variant_index, ..) => {
                let struct_ = enum_.get_variant(variant_index).struct_;
                let fields = self.lower_pattern_fields(&pattern.fields, struct_.get_fields(), &enum_.name, ctxt)?;
                let fields = self.hlr.pattern_fields(fields);
                Ok(self.hlr.pattern(hlr::PatternKind::Variant(hlr::VariantPattern {
                    variant: constructor,
                    fields,
                })))
            }
            hlr::Val::Struct(struct_, ..) => {
                let fields = self.lower_pattern_fields(&pattern.fields, struct_.get_fields(), &struct_.name, ctxt)?;
                let fields = self.hlr.pattern_fields(fields);
                Ok(self
                    .hlr
                    .pattern(hlr::PatternKind::Struct(hlr::StructPattern { constructor, fields })))
            }
            _ => Err(AstLoweringError::ExpectedConstructor),
        }
    }

    fn lower_pattern_fields(
        &mut self,
        ast_fields: &[ast::StructPatternField],
        def_fields: &[ty::StructField],
        type_name: &str,
        ctxt: &mut PatternLoweringCtxt,
    ) -> AstLoweringResult<Vec<hlr::PatternField<'ctxt>>> {
        let expected_fields: HashSet<&str> = def_fields.iter().map(|f| f.name.as_str()).collect();
        let provided_fields: HashSet<&str> = ast_fields.iter().map(|f| f.field_name.as_str()).collect();

        let mut missing: Vec<&str> = expected_fields.difference(&provided_fields).copied().collect();
        if !missing.is_empty() {
            missing.sort_unstable();
            return Err(AstLoweringError::MissingFields {
                type_name: type_name.to_owned(),
                fields: missing.iter().map(|s| s.to_string()).collect(),
            });
        }

        let mut extra: Vec<&str> = provided_fields.difference(&expected_fields).copied().collect();
        if !extra.is_empty() {
            extra.sort_unstable();
            return Err(AstLoweringError::ExtraFields {
                type_name: type_name.to_owned(),
                fields: extra.iter().map(|s| s.to_string()).collect(),
            });
        }

        ast_fields
            .iter()
            .map(|field| {
                let field_index = def_fields.iter().position(|f| f.name == field.field_name).unwrap();
                let pattern = self.lower_pattern_inner(field.pattern, ctxt)?;
                Ok(hlr::PatternField { field_index, pattern })
            })
            .collect()
    }
}
