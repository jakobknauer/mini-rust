macro_rules! parse_left_associative {
    (
        $name:ident,
        $next_fn:ident,
        [ $( $token:pat => $op:expr ),+ $(,)? ]
    ) => {
        fn $name(&mut self, allow_struct: bool) -> Result<Expr, ParserErr> {
            let mut acc: Expr = self.$next_fn(allow_struct)?;

            while let Some(token) = self.current() {
                let operator = match token {
                    $(
                        $token => $op,
                    )+
                    _ => break,
                };
                self.position += 1; // consume operator
                let right = self.$next_fn(allow_struct)?;
                acc = self.builder.binary_op(acc, operator, right);
            }

            Ok(acc)
        }
    };
}
