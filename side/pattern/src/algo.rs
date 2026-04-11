use std::collections::HashSet;

use crate::{Constructor, Pattern, Type, as_constructor, pattern, valid_pattern};

type Matrix = Vec<Vec<Pattern>>;

fn check_exhaustive(type_: &Type, patterns: &[Pattern]) -> bool {
    assert!(patterns.iter().all(|p| valid_pattern(p, type_)));

    let patterns: Vec<_> = patterns.iter().flat_map(|p| p.expand_alternatives()).collect();
    let matrix = patterns.into_iter().map(|p| vec![p]).collect();

    !is_useful(&matrix, &[Pattern::Wildcard], std::slice::from_ref(type_))
}

fn is_useful(matrix: &Matrix, row: &[Pattern], types: &[Type]) -> bool {
    if matrix.is_empty() {
        return true;
    }

    if row.is_empty() {
        return false;
    }

    match row[0] {
        Pattern::Some(_) => is_useful(
            &specialize_matrix(matrix, &Constructor::Some),
            &specialize_row(row, &Constructor::Some).unwrap(),
            &specialize_types(types, &Constructor::Some),
        ),
        Pattern::None => is_useful(
            &specialize_matrix(matrix, &Constructor::None),
            &specialize_row(row, &Constructor::None).unwrap(),
            &specialize_types(types, &Constructor::None),
        ),
        Pattern::Tuple(ref ps) => is_useful(
            &specialize_matrix(matrix, &Constructor::Tuple(ps.len())),
            &specialize_row(row, &Constructor::Tuple(ps.len())).unwrap(),
            &specialize_types(types, &Constructor::Tuple(ps.len())),
        ),
        Pattern::Wildcard => {
            let sigma: Vec<_> = matrix
                .iter()
                .flat_map(|row| as_constructor(&row[0]))
                .collect::<HashSet<_>>()
                .into_iter()
                .collect();

            if is_complete_signature(&types[0], &sigma) {
                sigma.iter().any(|c| {
                    is_useful(
                        &specialize_matrix(matrix, c),
                        &specialize_row(row, c).unwrap(),
                        &specialize_types(types, c),
                    )
                })
            } else {
                is_useful(&default_matrix(matrix), &row[1..], &types[1..])
            }
        }
        Pattern::Alternative(_) => unreachable!(),
        Pattern::Constant(_) => todo!(),
    }
}

fn specialize_matrix(matrix: &Matrix, constructor: &Constructor) -> Matrix {
    matrix
        .iter()
        .filter_map(|row| specialize_row(row, constructor))
        .collect()
}

fn specialize_row(row: &[Pattern], constructor: &Constructor) -> Option<Vec<Pattern>> {
    let [first, tail @ ..] = row else { panic!() };

    match constructor {
        Constructor::Some => match first {
            Pattern::Wildcard => Some(std::iter::once(Pattern::Wildcard).chain(tail.iter().cloned()).collect()),
            Pattern::Some(inner) => Some(std::iter::once((**inner).clone()).chain(tail.iter().cloned()).collect()),
            _ => None,
        },
        Constructor::None => match first {
            Pattern::Wildcard | Pattern::None => Some(tail.to_vec()),
            _ => None,
        },
        Constructor::Tuple(n) => match first {
            Pattern::Wildcard => Some(
                std::iter::repeat_n(Pattern::Wildcard, *n)
                    .chain(tail.iter().cloned())
                    .collect(),
            ),
            Pattern::Tuple(ts) if ts.len() == *n => Some(ts.iter().cloned().chain(tail.iter().cloned()).collect()),
            _ => None,
        },
    }
}

fn specialize_types(types: &[Type], constructor: &Constructor) -> Vec<Type> {
    let [first, tail @ ..] = types else { panic!() };
    match constructor {
        Constructor::Some => {
            let Type::Option(inner) = first else { panic!() };
            std::iter::once((**inner).clone()).chain(tail.iter().cloned()).collect()
        }
        Constructor::None => tail.to_vec(),
        Constructor::Tuple(n) => {
            let Type::Tuple(ts) = first else { panic!() };
            ts.iter().cloned().chain(tail.iter().cloned()).collect()
        }
    }
}

fn default_matrix(matrix: &Matrix) -> Matrix {
    matrix
        .iter()
        .filter_map(|row| {
            if let [Pattern::Wildcard, tail @ ..] = &row[..] {
                Some(tail.to_vec())
            } else {
                None
            }
        })
        .collect()
}

fn is_complete_signature(type_: &Type, constructors: &[Constructor]) -> bool {
    match type_ {
        Type::Int => false,
        Type::Tuple(items) => constructors.contains(&Constructor::Tuple(items.len())),
        Type::Option(_) => constructors.contains(&Constructor::None) && constructors.contains(&Constructor::Some),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_exhaustive() {
        let test_cases = [
            // match x: Int { 1 => .., 2 => .. }
            (Type::Int, vec![Pattern::Constant(1), Pattern::Constant(2)], false),
            // match x: Int { 1 => .., 2 => .., _ => .. }
            (
                Type::Int,
                vec![Pattern::Constant(1), Pattern::Constant(2), Pattern::Wildcard],
                true,
            ),
            // match x: Option<Int> { None => .., Some(_) => .. }
            (
                Type::option(Type::Int),
                vec![Pattern::None, Pattern::some(Pattern::Wildcard)],
                true,
            ),
            // match x: Option<Int> { Some(_) => .. }
            (Type::option(Type::Int), vec![Pattern::some(Pattern::Wildcard)], false),
            // match x: Option<Int> { None => .. }
            (Type::option(Type::Int), vec![Pattern::None], false),
            // match x: Option<Option<Int>> { None => .., Some(None) => .., Some(Some(_)) => .. }
            (
                Type::option(Type::option(Type::Int)),
                vec![
                    Pattern::None,
                    Pattern::some(Pattern::None),
                    Pattern::some(Pattern::some(Pattern::Wildcard)),
                ],
                true,
            ),
            // match x: Option<Option<Int>> { None => .., Some(Some(_)) => .. }
            (
                Type::option(Type::option(Type::Int)),
                vec![Pattern::None, Pattern::some(Pattern::some(Pattern::Wildcard))],
                false,
            ),
            // match (x, y): (Option<Int>, Option<Int>) { (Some(_), Some(_)) => .., (Some(_), None) => .., (None, Some(_)) => .., (None, None) => .. }
            (
                Type::Tuple(vec![Type::option(Type::Int), Type::option(Type::Int)]),
                vec![
                    Pattern::Tuple(vec![Pattern::some(Pattern::Wildcard), Pattern::some(Pattern::Wildcard)]),
                    Pattern::Tuple(vec![Pattern::some(Pattern::Wildcard), Pattern::None]),
                    Pattern::Tuple(vec![Pattern::None, Pattern::some(Pattern::Wildcard)]),
                    Pattern::Tuple(vec![Pattern::None, Pattern::None]),
                ],
                true,
            ),
            // match (x, y): (Option<Int>, Option<Int>) { (Some(_), Some(_)) => .., (Some(_), None) => .., (None, _) => .. }
            (
                Type::Tuple(vec![Type::option(Type::Int), Type::option(Type::Int)]),
                vec![
                    Pattern::Tuple(vec![Pattern::some(Pattern::Wildcard), Pattern::some(Pattern::Wildcard)]),
                    Pattern::Tuple(vec![Pattern::some(Pattern::Wildcard), Pattern::None]),
                    Pattern::Tuple(vec![Pattern::None, Pattern::Wildcard]),
                ],
                true,
            ),
            // match (x, y): (Option<Int>, Option<Int>) { (Some(_), Some(_)) => .., (Some(_), None) => .., (None, None) => .. }
            (
                Type::Tuple(vec![Type::option(Type::Int), Type::option(Type::Int)]),
                vec![
                    Pattern::Tuple(vec![Pattern::some(Pattern::Wildcard), Pattern::some(Pattern::Wildcard)]),
                    Pattern::Tuple(vec![Pattern::some(Pattern::Wildcard), Pattern::None]),
                    Pattern::Tuple(vec![Pattern::None, Pattern::None]),
                ],
                false,
            ),
        ];

        for (type_, patterns, expected) in test_cases {
            assert_eq!(check_exhaustive(&type_, &patterns), expected);
        }
    }
}
