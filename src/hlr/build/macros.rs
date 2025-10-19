macro_rules! parse_left_associative {
    (
        $name:ident,
        $next_fn:ident,
        [ $( $token:pat => $op:expr ),+ $(,)? ]
    ) => {
        fn $name(&mut self, allow_struct: bool) -> Result<Expression, ParserError> {
            let mut acc = self.$next_fn(allow_struct)?;

            while let Some(token) = self.current() {
                let operator = match token {
                    $(
                        $token => $op,
                    )+
                    _ => break,
                };
                self.position += 1; // consume operator
                let right = self.$next_fn(allow_struct)?;
                acc = Expression::BinaryOp {
                    left: Box::new(acc),
                    operator,
                    right: Box::new(right),
                };
            }

            Ok(acc)
        }
    };
}
