use std::{vec, slice, fmt::{self, Debug}};

use crate::token_match::TokenMatcher;

#[derive(Debug)]
struct TokenDefinition<N: Eq + Copy> {
    name: N,
    matcher: TokenMatcher
}

impl<N: Eq + Copy> TokenDefinition<N> {
    fn new(name: N, match_expr: &str) -> Self {
        TokenDefinition { name, matcher: TokenMatcher::new(match_expr) }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TokenPosition {
    pub line: usize,
    pub char: usize
}

impl TokenPosition {
    pub fn new(line: usize, char: usize) -> Self {
        TokenPosition { line, char }
    }

    fn advance(&mut self, str: &str) {
        for char in str.chars() {
            match char {
                '\n' => {
                    self.line += 1;
                    self.char = 0;
                }
                _ => {
                    self.char += 1;
                }
            }
        }
    }
}

impl fmt::Display for TokenPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.char)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a, N: Eq + Copy + Debug> {
    pub name: N,
    pub str: &'a str,
    pub pos: TokenPosition
}

impl<'a, N: Eq + Copy + Debug> Token<'a, N> {
    fn new(name: N, str: &'a str, pos: TokenPosition) -> Self {
        return Token { name, str, pos }
    }
}

impl<'a, N: Eq + Copy + Debug> fmt::Display for Token<'a, N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}(\"{}\") at {}", self.name, self.str, self.pos)
    }
}

pub struct Alphabet<N: Eq + Copy + Debug> {
    token_defs: Vec<TokenDefinition<N>>
}

impl<N: Eq + Copy + Debug> Alphabet<N> {
    pub fn new() -> Self {
        Alphabet { token_defs: vec![] }
    }

    pub fn register_token(&mut self, name: N, match_expr: &str) {
        self.token_defs.push(TokenDefinition::new(name, match_expr));
    }

    fn iter(&self) -> slice::Iter<'_, TokenDefinition<N>> {
        self.token_defs.iter()
    }

    pub fn tokenize<'a>(&self, string: &'a str) -> Vec<Token<'a, N>> {
        let mut tokens: Vec<Token<'a, N>> = vec![];
        let mut index = 0;
        let mut pos = TokenPosition::new(1, 0);

        loop {
            let mut match_found = false;
            for token_def in &self.token_defs {
                if let Some(len) = token_def.matcher.compare(&string[index..]) {
                    let slice = &string[index .. index + len];
                    tokens.push(Token::new(token_def.name, slice, pos.clone()));
                    pos.advance(slice);
                    index += len;
                    match_found = true;
                    break;
                }
            }
            if !match_found {
                panic!("Unexpected character '{}' ({})", string.chars().nth(index).unwrap(), pos);
            }
            if index == string.len() {
                break;
            }
        }
        
        tokens
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

    #[test]
    fn tokenization_works() {
        #[derive(PartialEq, Eq, Clone, Copy, Debug)]
        enum TokenName {
            A,
            B,
            AX,
            EOL
        }

        let abc = alphabet! {
            "ax" => TokenName::AX,
            "a"  => TokenName::A,
            "b"  => TokenName::B,
            "\n" => TokenName::EOL
        };
        let tokens = abc.tokenize("aaxb\na");
        let mut token_iter = tokens.iter();
        
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::A, "a", TokenPosition::new(1, 0))));
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::AX, "ax", TokenPosition::new(1, 1))));
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::B, "b", TokenPosition::new(1, 3))));
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::EOL, "\n", TokenPosition::new(1, 4))));
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::A, "a", TokenPosition::new(2, 0))));
        assert_eq!(token_iter.next(), None);
    }
}