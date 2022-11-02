use std::fmt;

use crate::compare::{Matcher, SequenceView, MatchGraph, MatchGraphBuilder, MatchError};

#[derive(Debug, Clone)]
enum CharMatcher {
    Exact(char),
    Range(char, char)
}

impl Matcher for CharMatcher {
    type Item = char;
    type Accumulator = String;

    fn compare(
        &self,
        sequence: &mut SequenceView<char>,
        accumulator: &mut String
    ) -> Result<(), crate::compare::MatchError> {
        match self {
            Self::Exact(char) => {
                let error = Err(MatchError::simple(format!("'{}'", char), sequence.index));

                if sequence.items().is_empty() {
                    return error;
                }
                let actual_char = *sequence.items().first().unwrap();
                if actual_char != *char {
                    return error
                }
                sequence.index += 1;
                accumulator.push(actual_char);
                Ok(())
            },
            Self::Range(from, to) => {
                let error = Err(MatchError::simple(format!("char in range '{}' to '{}'", from, to), sequence.index));

                if sequence.items().is_empty() {
                    return error;
                }
                let actual_char = *sequence.items().first().unwrap();
                if actual_char < *from || actual_char > *to {
                    return error;
                }
                sequence.index += 1;
                accumulator.push(actual_char);
                Ok(())
            }
        }
    }
}

#[derive(PartialEq, Eq)]
enum ExprParseState {
    Default,
    Choice,
    Range
}

#[derive(Debug, PartialEq, Eq)]
enum ChoiceParseState {
    Default,
    Range
}

#[derive(Debug)]
pub struct ExprParseError {
    pub character: char,
    pub index: usize,
    pub message: Option<String>
}

impl ExprParseError {
    fn new(character: char, index: usize, message: Option<String>) -> Self {
       ExprParseError { character, index, message: message }
    }
}

impl fmt::Display for ExprParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut char_string: String = format!("'{}'", self.character);
        if char_string == "\0" {
            char_string = "end of expression".to_string();
        }
        if self.message.is_some() {
            write!(f, "Unexpected {} at index {}: {}", char_string, self.index, self.message.as_ref().unwrap())
        } else {
            write!(f, "Unexpected {} at index {}", char_string, self.index)
        }
    }
}

#[derive(Debug)]
pub struct TokenMatcher {
    pattern: MatchGraph<CharMatcher>
}

impl TokenMatcher {
    pub fn expr(expr: &str) -> Result<Self, ExprParseError> {
        Ok(TokenMatcher {
            pattern: Self::parse_expr(&expr.chars().collect::<Vec<char>>())?
        })
    }

    pub fn compare(&self, string: &str) -> Result<String, String> {
        let cmp_chars: Vec<char> = string.chars().collect();
        let mut result_str = String::new();
        let result = self.pattern.compare(&cmp_chars, &mut result_str);
        if let Err(err) = result {
            return Err(format!("(expr:{}) {}", err.index, err));
        }
        Ok(result_str)
    }

    fn parse_expr(expr: &[char]) -> Result<MatchGraph<CharMatcher>, ExprParseError> {
        let mut builder = MatchGraphBuilder::new();
        let mut has_expression = false;
        let mut mode = ExprParseState::Default;
        let mut buffered_char: Option<char> = None;
        let mut escape = false;
        let mut index: usize = 0;
        let mut paren_depth: u32 = 0;

        for char in expr {
            index += 1;

            if !escape && *char == '\\' {
                escape = true;
                continue;
            }

            match mode {
                ExprParseState::Default => {
                    match (char, escape) {
                        ('*', false) => {
                            if !has_expression {
                                return Err(ExprParseError::new(*char, index, "'*' must follow a repeatable expression".to_string().into()))
                            }
                            builder.pop_repeat();
                        }
                        ('+', false) => {
                            if !has_expression {
                                return Err(ExprParseError::new(*char, index, "'+' must follow a repeatable expression".to_string().into()))
                            }
                            builder.duplicate(1);
                            builder.pop_repeat();
                        }
                        ('?', false) => {
                            if !has_expression {
                                return Err(ExprParseError::new(*char, index, "'?' must follow a repeatable expression".to_string().into()))
                            }
                            builder.pop_optional();
                        }
                        ('(', false) => {
                            builder.pop_return();
                            has_expression = false;
                            builder.push_return();
                            paren_depth += 1;
                        }
                        (')', false) => {
                            if paren_depth == 0 {
                                return Err(ExprParseError::new(*char, index, "unmatched closing parenthesis".to_string().into()));
                            }
                            paren_depth -= 1;
                            has_expression = true;
                            builder.pop_return();
                        },
                        ('[', false) => {
                            has_expression = false;
                            builder.pop_return();
                            builder.push_return();
                            mode = ExprParseState::Choice;
                        },
                        (']', false) => {
                            return Err(ExprParseError::new(*char, index, "unmatched closing square bracket".to_string().into()));
                        }
                        (_, _) => {
                            if has_expression {
                                builder.pop_return();
                            }
                            builder.push_return();
                            builder.append(CharMatcher::Exact(*char), None);
                            has_expression = true;
                        }
                    }
                    
                }
                ExprParseState::Choice => {
                    match (char, escape) {
                        ('-', false) => {
                            mode = ExprParseState::Range;
                        }
                        (']', false) => {
                            if let Some(b_char) = buffered_char {
                                builder.append(CharMatcher::Exact(b_char), None);
                                builder.end_choice_path();
                            }
                            
                            builder.pop_choice();
                            has_expression = true;
                            mode = ExprParseState::Default;
                        }
                        (_, _) => {
                            if let Some(b_char) = buffered_char {
                                builder.append(CharMatcher::Exact(b_char), None);
                                builder.end_choice_path();
                            }
                            buffered_char = Some(*char);
                        }
                    }
                },
                ExprParseState::Range => {
                    match (char, escape) {
                        (']', false) => {
                            return Err(ExprParseError::new(*char, index, "Unexpected end of choice after '-'".to_string().into()));
                        }
                        (_, _) => {
                            builder.append(CharMatcher::Range(buffered_char.expect("No start of range found"), *char), None);
                            buffered_char = None;
                            builder.end_choice_path();
                            mode = ExprParseState::Choice;
                        }
                    }
                }
            }

            if escape {
                escape = false;
            }
        }

        if escape {
            return Err(ExprParseError::new('\0', index, "incomplete escape sequence".to_string().into()));
        }
        if mode == ExprParseState::Choice || mode == ExprParseState::Range {
            return Err(ExprParseError::new('\0', index, "Unmatched opening square bracket".to_string().into()));
        }
        if paren_depth != 0 {
            return Err(ExprParseError::new('\0', index, "Unmatched opening parenthesis".to_string().into()));
        }

        builder.pop_return();
        Ok(builder.complete())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn seq_view<'a>(str: &str, buffer: &'a mut Vec<char>) -> SequenceView<'a, char> {
		*buffer = str.chars().collect();
        SequenceView::new(buffer.as_slice())
    }

    #[test]
    fn exact_char_matcher_works() {
        let matcher = CharMatcher::Exact('a');
        
        let mut strbuf: Vec<char> = vec![];

        let mut str = String::new();
        assert_eq!(matcher.compare(&mut seq_view("a", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("a"));

        let mut str = String::new();
        assert_eq!(matcher.compare(&mut seq_view("abc", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("a"));

        let mut str = String::new();
        assert_eq!(matcher.compare(&mut seq_view("fsfd", &mut strbuf), &mut str), Err(MatchError::simple("'a'".to_string(), 0)));


        let mut str = String::new();
        assert_eq!(matcher.compare(&mut seq_view("", &mut strbuf), &mut str), Err(MatchError::simple("'a'".to_string(), 0)));
    }

    #[test]
    fn range_char_matcher_works() {
        let matcher = CharMatcher::Range('1', '3');

        let mut strbuf: Vec<char> = vec![];

        let mut str = String::new();
        assert_eq!(matcher.compare(&mut seq_view("0", &mut strbuf), &mut str), Err(MatchError::simple("char in range '1' to '3'".to_string(), 0)));

        let mut str = String::new();
        assert_eq!(matcher.compare(&mut seq_view("1", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("1"));

        let mut str = String::new();
        assert_eq!(matcher.compare(&mut seq_view("2", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("2"));

        let mut str = String::new();
        assert_eq!(matcher.compare(&mut seq_view("3", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("3"));

        let mut str = String::new();
        assert_eq!(matcher.compare(&mut seq_view("4", &mut strbuf), &mut str), Err(MatchError::simple("char in range '1' to '3'".to_string(), 0)));
    }

    #[test]
    fn can_match_exact() {
        let matcher = TokenMatcher::expr("abc").unwrap();

        assert_eq!(matcher.compare("abc"), Ok(String::from("abc")), "'abc' should match first 3 chars in 'abc'");
        assert_eq!(matcher.compare("abcdefg"), Ok(String::from("abc")), "'abc' should match first 3 chars in 'abcdefg'");
        assert_eq!(matcher.compare("abedefg"), Err(String::from("(expr:2) Expected 'c'")), "'abc' should not match in 'abedefg'");
    }

    #[test]
    fn can_repeat_single() {
        let matcher = TokenMatcher::expr("ab*").unwrap();
        
        assert_eq!(matcher.compare("abbbbbc"), Ok(String::from("abbbbb")), "'ab*' should match first 6 chars in 'abbbbbc'");
        assert_eq!(matcher.compare("acfds"), Ok(String::from("a")), "'ab*' should match first char in 'acfds'");
        assert_eq!(matcher.compare("cgfds"), Err(String::from("(expr:0) Expected 'a'")), "'ab*' should not match in 'cgfds'");
    }

    #[test]
    fn can_repeat_multiple() {
        let matcher = TokenMatcher::expr("a(bc)*").unwrap();

        assert_eq!(matcher.compare("abcbcbcbcefdfs"), Ok(String::from("abcbcbcbc")), "'a(bc)*' should match first 9 chars in 'abcbcbcbcefdfs'");
        assert_eq!(matcher.compare("bcbcbcbcefdfs"), Err(String::from("(expr:0) Expected 'a'")), "'a(bc)*' should not match 'bcbcbcbcefdfs'");
    }

    #[test]
    fn can_repeat_multiple_plus() {
        let matcher = TokenMatcher::expr("a(bc)+").unwrap();

        assert_eq!(matcher.compare("abcbcbcbcefdfs"), Ok(String::from("abcbcbcbc")), "'a(bc)+' should match first 9 chars in 'abcbcbcbcefdfs'");
        assert_eq!(matcher.compare("bcbcbcbcefdfs"), Err(String::from("(expr:0) Expected 'a'")), "'a(bc)+' not match 'bcbcbcbcefdfs'");
        assert_eq!(matcher.compare("aefdfs"), Err(String::from("(expr:1) Expected 'b'")), "'a(bc)+' should not match 'aefdfs'");
    }

    #[test]
    fn optional_works() {
        let matcher = TokenMatcher::expr("a(bc)?").unwrap();

        assert_eq!(matcher.compare("abcbcbcbcefdfs"), Ok(String::from("abc")), "'a(bc)?' should match first 3 chars in 'abcbcbcbcefdfs'");
        assert_eq!(matcher.compare("adbcbcbcbcefdfs"), Ok(String::from("a")), "'a(bc)?' should match the first character of 'adbcbcbcbcefdfs'");
        assert_eq!(matcher.compare("gdfefdfs"), Err(String::from("(expr:0) Expected 'a'")), "'a(bc)?' not match 'gdfefdfs'");
    }

    #[test]
    fn choice_works() {
        let matcher = TokenMatcher::expr("a[bc]").unwrap();

        assert_eq!(matcher.compare("ab534"), Ok(String::from("ab")), "'a[bc]' should match the first 2 chars in 'ab534'");
        assert_eq!(matcher.compare("ac534"), Ok(String::from("ac")), "'a[bc]' should match the first 2 chars in 'ac534'");
        assert_eq!(
            matcher.compare("a534"),
            Err(String::from("(expr:1) Expected one of: 'b', 'c'")),
            "'a[bc]' should not match 'a534'"
        );
    }

    #[test]
    fn repeated_choice_works() {
        let matcher = TokenMatcher::expr("a[bc]*").unwrap();

        assert_eq!(matcher.compare("abbcbccccbbc453"), Ok(String::from("abbcbccccbbc")), "'a[bc]*' should match the first 12 chars in 'abbcbccccbbc453'");
    }

    #[test]
    fn escape_works() {
        let matcher = TokenMatcher::expr("\\(\\(\\[\\*").unwrap();

        assert_eq!(matcher.compare("(([*"), Ok(String::from("(([*")), "'\\(\\(\\[\\*' should match the first 4 chars in '(([*'");
        assert_eq!(matcher.compare("(([a"), Err(String::from("(expr:3) Expected '*'")), "'\\(\\(\\[\\*' should not match '(([a'");
    }

    #[test]
    fn escape_in_choice_works() {
        let matcher = TokenMatcher::expr("[a\\-\\]]").unwrap();

        assert_eq!(matcher.compare("a"), Ok(String::from("a")), "'[a\\-\\]]' should match 'a'");
        assert_eq!(matcher.compare("]"), Ok(String::from("]")), "'[a\\-\\]]' should match ']'");
        assert_eq!(matcher.compare("-"), Ok(String::from("-")), "'[a\\-\\]]' should match '-'");
        assert_eq!
            (matcher.compare("bet46r"),
            Err(String::from("(expr:0) Expected one of: 'a', '-', ']'")),
            "'[a\\-\\]]' should not match 'bet46r'"
        );
    }

    #[test]
    fn ranges_work() {
        let matcher = TokenMatcher::expr("[a-z]").unwrap();

        assert_eq!(matcher.compare("v"), Ok(String::from("v")), "'[a-z]' should match 'v'");
        assert_eq!(matcher.compare("s"), Ok(String::from("s")), "'[a-z]' should match 's'");
        assert_eq!(matcher.compare("b"), Ok(String::from("b")), "'[a-z]' should match 'b'");
        assert_eq!(matcher.compare("A"), Err(String::from("(expr:0) Expected char in range 'a' to 'z'")), "'[a-z]' should not match 'A'");
        assert_eq!(matcher.compare("-"), Err(String::from("(expr:0) Expected char in range 'a' to 'z'")), "'[a-z]' should not match '-'");
    }

    #[test]
    fn multiple_ranges_work() {
        let matcher = TokenMatcher::expr("[1-24-5]").unwrap();

        assert_eq!(matcher.compare("1"), Ok(String::from("1")), "'[1-24-5]' should match '1'");
        assert_eq!(matcher.compare("4"), Ok(String::from("4")), "'[1-24-5]' should match '4'");
        assert_eq!(
            matcher.compare("3"),
            Err(String::from("(expr:0) Expected one of: char in range '1' to '2', char in range '4' to '5'")),
            "'[1-24-5]' should not match '3'"
        );
    }
}