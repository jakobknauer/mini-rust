#[derive(Debug, PartialEq, Eq)]
pub enum Pattern {
    Wildcard,
    Alternative(Vec<Pattern>),

    Constant(i32),

    Tuple(Vec<Pattern>),

    Some(Box<Pattern>),
    None,
}

impl Pattern {
    pub fn some(p: Pattern) -> Pattern {
        Pattern::Some(Box::new(p))
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Tuple(Vec<Type>),
    Option(Box<Type>),
}

impl Type {
    pub fn option(t: Type) -> Type {
        Type::Option(Box::new(t))
    }
}

fn valid_pattern(pattern: &Pattern, type_: &Type) -> bool {
    match (pattern, type_) {
        (Pattern::Wildcard, _) => true,
        (Pattern::Alternative(patterns), type_) => patterns.iter().all(|p| valid_pattern(p, type_)),
        (Pattern::Constant(_), Type::Int) => true,
        (Pattern::Tuple(ps), Type::Tuple(ts)) => {
            ps.len() == ts.len() && ps.iter().zip(ts).all(|(p, t)| valid_pattern(p, t))
        }
        (Pattern::Some(p), Type::Option(t)) => valid_pattern(p, t),
        (Pattern::None, Type::Option(_)) => true,

        (_, _) => false,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_valid_pattern() {
        let test_cases = [
            (&Pattern::Constant(3), &Type::Int, true),
            (&Pattern::Constant(3), &Type::Tuple(vec![]), false),
            (
                &Pattern::some(Pattern::some(Pattern::Wildcard)),
                &Type::option(Type::option(Type::Int)),
                true,
            ),
            (
                &Pattern::some(Pattern::some(Pattern::Wildcard)),
                &Type::option(Type::Int),
                false,
            ),
        ];

        for (p, t, expected) in test_cases {
            assert_eq!(expected, valid_pattern(p, t))
        }
    }
}
