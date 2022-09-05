
use std::{vec, slice};

use crate::token_match::TokenMatcher;

#[derive(Debug)]
struct TokenDefinition<K: Eq> {
    id: K,
    matcher: TokenMatcher
}

impl<K: Eq> TokenDefinition<K> {
    fn new(id: K, match_expr: &str) -> Self {
        TokenDefinition { id, matcher: TokenMatcher::new(match_expr) }
    }
}

pub struct Alphabet<K: Eq> {
    token_defs: Vec<TokenDefinition<K>>
}

impl<K: Eq> Alphabet<K> {
    fn new() -> Self {
        Alphabet { token_defs: vec![] }
    }

    fn register_token(&mut self, id: K, match_expr: &str) {
        self.token_defs.push(TokenDefinition::new(id, match_expr));
    }

    fn iter(&self) -> slice::Iter<'_, TokenDefinition<K>> {
        self.token_defs.iter()
    }
}

#[macro_export]
macro_rules! alphabet {
    ($($match_expr:literal=>$id:expr),*) => {
        {
            let mut alphabet = Alphabet::new();
            $(alphabet.register_token($id, $match_expr));*;
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
        assert_eq!(first.id, 0);
        assert_eq!(first.matcher.compare("a"), Some(1));

        let second = defs.next().expect("Should register the second token definition");
        assert_eq!(second.id, 1);
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
        assert_eq!(first.id, 0);
        assert_eq!(first.matcher.compare("a"), Some(1));

        let second = defs.next().expect("Should register the second token definition");
        assert_eq!(second.id, 0);
        assert_eq!(second.matcher.compare("b"), Some(1));
        
        assert_eq!(matches!(defs.next(), None), true, "Should have exactly two definitions");
    }
}