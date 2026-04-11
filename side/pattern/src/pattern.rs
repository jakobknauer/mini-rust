use itertools::Itertools;

#[derive(Clone, Debug, PartialEq, Eq)]
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

    pub fn expand_alternatives(&self) -> Vec<Pattern> {
        match self {
            Pattern::Wildcard => vec![Pattern::Wildcard],
            Pattern::Alternative(patterns) => patterns.iter().flat_map(|p| p.expand_alternatives()).collect(),
            Pattern::Constant(n) => vec![Pattern::Constant(*n)],
            Pattern::Tuple(patterns) => patterns
                .iter()
                .map(|p| p.expand_alternatives())
                .multi_cartesian_product()
                .map(Pattern::Tuple)
                .collect(),
            Pattern::Some(pattern) => pattern.expand_alternatives().into_iter().map(Pattern::some).collect(),
            Pattern::None => vec![Pattern::None],
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_expand_alternatives() {
        let test_cases = [
            (
                Pattern::Alternative(vec![Pattern::Constant(1), Pattern::Constant(2)]),
                vec![Pattern::Constant(1), Pattern::Constant(2)],
            ),
            (
                Pattern::some(Pattern::Alternative(vec![Pattern::Constant(1), Pattern::Constant(2)])),
                vec![Pattern::some(Pattern::Constant(1)), Pattern::some(Pattern::Constant(2))],
            ),
            (
                Pattern::Tuple(vec![
                    Pattern::Alternative(vec![Pattern::Constant(1), Pattern::Constant(2)]),
                    Pattern::Alternative(vec![Pattern::Constant(3), Pattern::Constant(4)]),
                ]),
                vec![
                    Pattern::Tuple(vec![Pattern::Constant(1), Pattern::Constant(3)]),
                    Pattern::Tuple(vec![Pattern::Constant(1), Pattern::Constant(4)]),
                    Pattern::Tuple(vec![Pattern::Constant(2), Pattern::Constant(3)]),
                    Pattern::Tuple(vec![Pattern::Constant(2), Pattern::Constant(4)]),
                ],
            ),
        ];

        for (pattern, expected) in test_cases {
            assert_eq!(expected, pattern.expand_alternatives());
        }
    }
}
