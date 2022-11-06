use std::{vec, slice, fmt::{self, Debug}};

use colored::Colorize;

use crate::{token_match::{TokenMatcher, ExprParseError}, ParsingError, TextPosition};

#[derive(Debug)]
struct TokenDefinition<N> {
    name: N,
    matcher: TokenMatcher
}

impl<N> TokenDefinition<N> {
    fn new(name: N, matcher: TokenMatcher) -> Self {
        TokenDefinition { name, matcher }
    }
}

impl TextPosition {
    fn advance(&mut self, str: &str) {
        self.index += str.len();
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

/// A specific occurence of a token in the text that was tokenized.
/// 
/// Contains the `name` of the token type, `str`, a [`String`] that contains the
/// text the token matched, and `pos`, the [`TextPosition`] of the token in the text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<N> {
    pub name: N,
    pub str: String,
    pub pos: TextPosition
}

impl<N> Token<N> {
    /// Creates a new token.
    /// 
    /// # Examples
    /// ```
    /// # use sprout::prelude::*;
    /// #
    /// use sprout::tokenize::Token;
    /// 
    /// enum TokenName { ABC }
    /// 
    /// Token::new(TokenName::ABC, "abc".to_string(), TextPosition::new(4, 20, 69));
    /// ```
    pub fn new(name: N, str: String, pos: TextPosition) -> Self {
        return Token { name, str, pos }
    }
}

impl<N: fmt::Display> fmt::Display for Token<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} \"{}\" at {}", self.name.to_string().bold(), self.str, self.pos)
    }
}

/// An `Alphabet` contains definitions for token types, and can parse a `String` into a `Vec` of [`Token`]s
/// via the `tokenize` function.
/// 
/// Token definitions are given via a subset of regular expressions, namely one that contains groups (`[abc]`),
/// ranges (`[a-z]`), and repeats (`*`, `+`, `?`).
/// 
/// The easiest way to create an alphabet is using the [`alphabet`] macro.
/// 
/// # Examples
/// Creating an alphabet using the [`alphabet`] macro
/// ```
/// # use sprout::{prelude::*};
/// #
/// #[derive(Clone, Copy)]
/// enum TokenName {
///     Word,
///     Number
/// }
/// 
/// use TokenName::*;
/// 
/// let alphabet = alphabet! {
///     Word => "[A-Za-z]+";
///     Number => "[0-9]+(.[0-9]+)?";
/// };
/// ```
/// 
/// Creating an alphabet manually
/// ```
/// # use sprout::prelude::*;
/// #
/// use sprout::tokenize::Alphabet;
/// 
/// #[derive(Clone, Copy)]
/// enum TokenName {
///     Word,
///     Number
/// }
/// 
/// use TokenName::*;
/// 
/// let mut alphabet = Alphabet::new();
/// alphabet.register_token(Word, "[A-Za-z]+");
/// alphabet.register_token(Number, "[0-9]+(.[0-9]+)?");
/// ```
#[derive(Debug)]
pub struct Alphabet<N> {
    token_defs: Vec<TokenDefinition<N>>
}

impl<N: Copy> Alphabet<N> {
    /// Create a new `Alphabet`.
    pub fn new() -> Self {
        Alphabet { token_defs: vec![] }
    }

    /// Add a new token definition to this alphabet. `match_expr` is an expression in a subset of regex.
    /// 
    /// If your use case just involves creating a static alphabet whithout any dynamic changes made to it,
    /// consider using the [`alphabet`] macro instead of this function.
    /// 
    /// # Examples
    /// Registering a token
    /// ```
    /// # use sprout::{prelude::*, tokenize::Alphabet};
    /// # #[derive(Clone, Copy)]
    /// # enum TokenName { Word }
    /// #
    /// # let mut alphabet = Alphabet::new();
    /// #
    /// alphabet.register_token(TokenName::Word, "[A-Za-z]+");
    /// ```
    pub fn register_token(&mut self, name: N, match_expr: &str) -> Result<(), ExprParseError> {
        Ok(self.token_defs.push(TokenDefinition::new(name, TokenMatcher::expr(match_expr)?)))
    }

    fn iter(&self) -> slice::Iter<'_, TokenDefinition<N>> {
        self.token_defs.iter()
    }

    /// Parse a `String` into a `Vec` of [`Token`]s.
    /// 
    /// Returns a `Result`, with a [`ParsingError`] as the error type.
    pub fn tokenize(&self, string: String) -> Result<Vec<Token<N>>, ParsingError> {
        let mut tokens: Vec<Token<N>> = vec![];
        let mut index = 0;
        let mut pos = TextPosition::new(1, 0, 0);

        loop {
            if index == string.len() {
                break;
            }

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
                if index == string.len() {
                    return Err(ParsingError::new("Unexpected end of input".to_string(), pos, Some(string)));
                }
                return Err(ParsingError::new(format!("Unexpected character '{}'", string.chars().nth(index).unwrap()), pos, Some(string)))
            }
        }
        
        Ok(tokens)
    }
}

/// Create an [`Alphabet`] by providing definitions for all token types.
/// 
/// Token definitions are given via a subset of regular expressions, namely one that contains groups (`[abc]`),
/// ranges (`[a-z]`), and repeats (`*`, `+`, `?`). For more information.
/// 
/// # Examples
/// Create a simple alphabet
/// ```
/// # use sprout::{prelude::*};
/// #
/// #[derive(Clone, Copy)]
/// enum TokenName {
///     Word,
///     Number
/// }
/// 
/// use TokenName::*;
/// 
/// let alphabet = alphabet! {
///     Word => "[A-Za-z]+";
///     Number => "[0-9]+(.[0-9]+)?";
/// };
/// ```
#[macro_export]
macro_rules! alphabet {
    ($($name:expr=>$match_expr:expr);*$(;)?) => {
        {
            let mut __alphabet = $crate::tokenize::Alphabet::new();
            $({
                let res = __alphabet.register_token($name, $match_expr);
                if let Err(err) = res {
                    panic!("{}", err)
                }
                res.unwrap()
            });*;
            __alphabet
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_register_tokens() {
        let abc = alphabet! {
            0 => "a";
            1 => "b";
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
            0 => "a";
            0 => "b";
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
            TokenName::AX => "ax";
            TokenName::A => "a";
            TokenName::B => "b";
            TokenName::EOL => "\n";
        };
        let tokens = abc.tokenize(String::from("aaxb\na"));
        assert!(tokens.is_ok(), "Should tokenize \"aaxb\\na\"");
        let tokens = tokens.unwrap();
        let mut token_iter = tokens.iter(); 
        
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::A, String::from("a"), TextPosition::new(1, 0, 0))));
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::AX, String::from("ax"), TextPosition::new(1, 1, 1))));
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::B, String::from("b"), TextPosition::new(1, 3, 3))));
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::EOL, String::from("\n"), TextPosition::new(1, 4, 4))));
        assert_eq!(token_iter.next(), Some(&Token::new(TokenName::A, String::from("a"), TextPosition::new(2, 0, 5))));
        assert_eq!(token_iter.next(), None);
    }
}