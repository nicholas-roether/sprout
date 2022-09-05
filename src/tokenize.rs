
use std::{vec, slice};

use crate::token_match::TokenMatcher;

#[derive(Debug)]
struct TokenDefinition<N: Eq> {
    name: N,
    matcher: TokenMatcher
}

impl<N: Eq> TokenDefinition<N> {
    fn new(name: N, match_expr: &str) -> Self {
        TokenDefinition { name, matcher: TokenMatcher::new(match_expr) }
    }
}

pub struct Alphabet<N: Eq> {
    token_defs: Vec<TokenDefinition<N>>
}

impl<N: Eq> Alphabet<N> {
    pub fn new() -> Self {
        Alphabet { token_defs: vec![] }
    }

    pub fn register_token(&mut self, name: N, match_expr: &str) {
        self.token_defs.push(TokenDefinition::new(name, match_expr));
    }

    fn iter(&self) -> slice::Iter<'_, TokenDefinition<N>> {
        self.token_defs.iter()
    }
}

#[macro_export]
macro_rules! alphabet {
    ($($match_expr:literal=>$name:expr),*) => {
        {
            let mut alphabet = Alphabet::new();
            $(alphabet.register_token($name, $match_expr));*;
            alphabet
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_register_tokens() {
        let abc = alphabet! {
            "a" => 0,
            "b" => 1
        };

        let mut defs = abc.iter();

        let first = defs.next().expect("Should register the first token definition");
        assert_eq!(first.name, 0);
        assert_eq!(first.matcher.compare("a"), Some(1));

        let second = defs.next().expect("Should register the second token definition");
        assert_eq!(second.name, 1);
        assert_eq!(second.matcher.compare("b"), Some(1));

        assert_eq!(matches!(defs.next(), None), true, "Should have exactly two definitions");
    }

    #[test]
    fn allows_duplicates() {
        let abc = alphabet! {
            "a" => 0,
            "b" => 0
        };

        let mut defs = abc.iter();

        let first = defs.next().expect("Should register the second token definition");
        assert_eq!(first.name, 0);
        assert_eq!(first.matcher.compare("a"), Some(1));

        let second = defs.next().expect("Should register the second token definition");
        assert_eq!(second.name, 0);
        assert_eq!(second.matcher.compare("b"), Some(1));
        
        assert_eq!(matches!(defs.next(), None), true, "Should have exactly two definitions");
    }
}