use crate::mlr::defs::*;

pub fn simplify(mlr: &mut Mlr) {
    let mut simplifier = Simplify { mlr };
    simplifier.run();
}

struct Simplify<'a> {
    mlr: &'a mut Mlr,
}

impl<'a> Simplify<'a> {
    fn run(&mut self) {
        self.simplify_val(self.mlr.body);
    }

    fn simplify_val(&mut self, val_id: ValId) {
        let mut val = self.mlr.vals.remove(&val_id).unwrap();

        match &mut val {
            Val::Block { statements, output } => {
                for stmt_id in statements.iter() {
                    self.simplify_stmt(*stmt_id);
                }
                self.simplify_val(*output);

                // If the output is itself a block, we can inline its statements
                if let Val::Block {
                    statements: inner_statements,
                    output: inner_output,
                } = self.mlr.vals.get(output).unwrap()
                {
                    statements.extend(inner_statements);
                    *output = *inner_output;
                }

                // If there are no statements, we can replace the block with its output
                if statements.is_empty() {
                    // The following assumes that the output ValId is not used elsewhere in the MLR
                    val = self.mlr.vals.remove(output).unwrap();
                }
            }
            Val::If(if_) => {
                self.simplify_val(if_.then_block);
                self.simplify_val(if_.else_block);
            }
            Val::Loop { body } => self.simplify_val(*body),

            Val::Use(..) | Val::Call { .. } | Val::Empty { .. } => (),
        }

        self.mlr.vals.insert(val_id, val);
    }

    fn simplify_stmt(&mut self, stmt_id: StmtId) {
        match self.mlr.stmts.get(&stmt_id).unwrap() {
            Statement::Assign { value, .. } => self.simplify_val(*value),
            Statement::Return { .. } | Statement::Break => (),
        }
    }
}
