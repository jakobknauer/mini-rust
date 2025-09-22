use crate::{
    context::{function_registry::FunctionRegistry, type_registry::TypeRegistry},
    mlr::*,
};

pub fn print_mlr(mlr: &Mlr, type_registry: &TypeRegistry, function_registry: &FunctionRegistry) {
    let mut printer = MlrPrinter {
        mlr,
        type_registry,
        function_registry,
        indent_level: 0,
    };
    printer.print_mlr();
}

struct MlrPrinter<'a> {
    mlr: &'a Mlr,
    type_registry: &'a TypeRegistry,
    function_registry: &'a FunctionRegistry,
    indent_level: usize,
}

impl<'a> MlrPrinter<'a> {
    fn print_mlr(&mut self) {
        self.print_block(&self.mlr.body);
    }

    fn print_block(&mut self, block: &Block) {
        println!("{{");
        self.indent_level += 1;

        for stmt_id in &block.statements {
            let stmt = &self.mlr.statements[stmt_id];
            self.print_statement(stmt);
        }

        self.indent();
        println!("_{}", block.output.0);

        self.indent_level -= 1;
        self.indent();
        print!("}}");
    }

    fn print_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Assign { loc, value } => {
                self.indent();
                print!("let _{} = ", loc.0);
                self.print_expression(*value);
                println!(";");
            }
            Statement::Return { value } => {
                self.indent();
                println!("return _{};", value.0);
            }
        }
    }

    fn print_expression(&mut self, expr_id: ExprId) {
        let expr = &self.mlr.expressions[&expr_id];
        match expr {
            Expression::Block(block) => {
                self.print_block(block);
            }
            Expression::Constant(constant) => match constant {
                Constant::Int(i) => print!("const {}", i),
                Constant::Bool(b) => print!("const {}", b),
                Constant::Unit => print!("const ()"),
            },
            Expression::Var(loc) => {
                print!("_{}", loc.0);
            }
            Expression::AddressOf(loc) => {
                print!("&_{}", loc.0);
            }
            Expression::Call { callable, args } => {
                print!("call _{}(", callable.0);
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    print!("_{}", arg.0);
                }
                print!(")");
            }
            Expression::Function(fn_id) => {
                if let Some(func) = self.function_registry.get_signature_by_id(*fn_id) {
                    print!("fn {}()", func.name);
                } else {
                    print!("fn<unknown>()");
                }
            }
            Expression::If {
                condition,
                then_block,
                else_block,
            } => {
                print!("if _{} ", condition.0);
                self.print_block(then_block);
                self.indent();
                print!("else ");
                self.print_block(else_block);
            }
            Expression::Loop { body } => {
                print!("loop ");
                self.print_expression(*body);
            }
        }
    }
    fn indent(&self) {
        for _ in 0..self.indent_level {
            print!("    ");
        }
    }
}
