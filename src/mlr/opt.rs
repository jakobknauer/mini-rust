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

    fn simplify_val(&mut self, val: ValId) {
        let val = unsafe { &mut *(self.mlr.vals.get_mut(&val).unwrap() as *mut Value) };

        match val {
            Value::Block { statements, output } => {
                for stmt_id in statements.iter() {
                    self.simplify_stmt(*stmt_id);
                }
                self.simplify_val(*output);

                // If the output is itself a block, we can inline its statements
                if let Value::Block {
                    statements: inner_statements,
                    output: inner_output,
                } = self.mlr.vals.get(output).unwrap()
                {
                    statements.extend(inner_statements);
                    *output = *inner_output;
                }

                if statements.is_empty() {
                    // If there are no statements, we can replace the block with its output
                    let output_val = self.mlr.vals.get(output).unwrap().clone();
                    *val = output_val;
                }
            }
            Value::If(if_) => {
                self.simplify_val(if_.then_block);
                self.simplify_val(if_.else_block);
            }
            Value::Loop { body } => self.simplify_val(*body),

            Value::Constant(..) | Value::Use(..) | Value::Call { .. } | Value::Function(..) | Value::Empty { .. } => (),
        }
    }

    fn simplify_stmt(&mut self, stmt_id: StmtId) {
        let stmt = unsafe { &mut *(self.mlr.stmts.get_mut(&stmt_id).unwrap() as *mut Statement) };

        match stmt {
            Statement::Assign { value, .. } => self.simplify_val(*value),
            Statement::Return { .. } | Statement::Break => (),
        }
    }
}
