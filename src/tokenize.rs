use std::{vec, slice, fmt::{self, Debug}};

use crate::token_match::{TokenMatcher, ExprParseError};

struct TokenDefinition<N> {
    name: N,
    matcher: TokenMatcher
}

impl<N> TokenDefinition<N> {
    fn new(name: N, matcher: TokenMatcher) -> Self {
        TokenDefinition { name, matcher }
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<N> {
    pub name: N,
    pub str: String,
    pub pos: TokenPosition
}

impl<N> Token<N> {
    pub fn new(name: N, str: String, pos: TokenPosition) -> Self {
        return Token { name, str, pos }
    }
}

impl<N: fmt::Display> fmt::Display for Token<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} \"{}\" at {}", self.name, self.str, self.pos)
    }
}

pub struct Alphabet<N> {
    token_defs: Vec<TokenDefinition<N>>
}

impl<N: Copy> Alphabet<N> {
    pub fn new() -> Self {
        Alphabet { token_defs: vec![] }
    }

    pub fn register_token(&mut self, name: N, match_expr: &str) -> Result<(), ExprParseError> {
        Ok(self.token_defs.push(TokenDefinition::new(name, TokenMatcher::expr(match_expr)?)))
    }

    fn iter(&self) -> slice::Iter<'_, TokenDefinition<N>> {
        self.token_defs.iter()
    }

    pub fn tokenize(&self, string: String) -> Result<Vec<Token<N>>, String> {
        let mut tokens: Vec<Token<N>> = vec![];
        let mut index = 0;
        let mut pos = TokenPosition::new(1, 0);

        loop {
            let mut match_found = false;
            for token_def in &self.token_defs {
                if let Ok(token_str) = token_def.matcher.compare(&string[index..]) {
                    tokens.push(Token::new(token_def.name, token_str.clone(), pos.clone()));
                    pos.advance(&token_str);
                    index += token_str.len();
                    match_found = true;
                    break;
                }
            }
            if !match_found {
                return Err(format!("Unexpected character '{}' ({})", string.chars().nth(index).unwrap(), pos));
            }
            if index == string.len() {
                break;
            }
        }
        
        Ok(tokens)
    }
}

#[macro_export]
macro_rules! alphabet {
    ($($match_expr:literal=>$name:expr),*) => {
        {
            let mut alphabet = crate::tokenize::Alphabet::new();
            $({
                let res = alphabet.register_token($name, $match_expr);
                if let Err(err) = res {
                    panic!("{}", err)
                }
                res.unwrap()
            });*;
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
        assert_eq!(first.matcher.compare("a"), Ok(String::from("a")));

        let second = defs.next().expect("Should register the second token definition");
        assert_eq!(second.name, 1);
        assert_eq!(second.matcher.compare("b"), Ok(String::from("b")));

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
        assert_eq!(first.matcher.compare("a"), Ok(String::from("a")));

        let second = defs.next().expect("Should register the second token definition");
        assert_eq!(second.name, 0);
        assert_eq!(second.matcher.compare("b"), Ok(String::from("b")));
        
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
        let tokens = abc.tokenize(String::from("aaxb\na"));
        assert!(tokens.is_ok(), "Should tokenize \"axb\\na\"");
        let tokens = tokens.unwrap();
        let mut token_iter = tokens.iter(); 
        
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::A, String::from("a"), TokenPosition::new(1, 0))));
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::AX, String::from("ax"), TokenPosition::new(1, 1))));
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::B, String::from("b"), TokenPosition::new(1, 3))));
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::EOL, String::from("\n"), TokenPosition::new(1, 4))));
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::A, String::from("a"), TokenPosition::new(2, 0))));
        assert_eq!(token_iter.next(), None);
    }
}