use crate::Constructor;
use crate::Pattern;
use crate::Type;

pub fn valid_pattern(pattern: &Pattern, type_: &Type) -> bool {
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

pub fn matches(pattern: &Pattern, constructor: &Constructor) -> bool {
    match (pattern, constructor) {
        (Pattern::Alternative(..), _) => panic!(),

        (Pattern::Wildcard, _) => true,
        (Pattern::None, Constructor::None) => true,
        (Pattern::Some(_), Constructor::Some) => true,
        (Pattern::Tuple(ts), Constructor::Tuple(n)) => ts.len() == *n,

        (_, _) => false,
    }
}

pub fn as_constructor(pattern: &Pattern) -> Option<Constructor> {
    match pattern {
        Pattern::Wildcard => None,
        Pattern::Alternative(patterns) => unreachable!(),
        Pattern::Constant(_) => None,
        Pattern::Tuple(patterns) => Some(Constructor::Tuple(patterns.len())),
        Pattern::Some(pattern) => Some(Constructor::Some),
        Pattern::None => Some(Constructor::None),
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
