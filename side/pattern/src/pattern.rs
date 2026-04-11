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
