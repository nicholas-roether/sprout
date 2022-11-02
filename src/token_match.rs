use std::fmt;

use crate::compare::{Matcher, SequenceView, MatchGraph, MatchGraphBuilder, MatchError};


// use crate::fragments::{Fragment, SequenceFragment, SequenceView, RepeatFragment, ChoiceFragment};


// #[derive(Debug)]
// struct CharFragment {
//     char: char
// }

// impl CharFragment {
//     fn new(char: char) -> Self {
//         CharFragment { char }
//     }
// }

// impl Fragment<char, String, ()> for CharFragment {
//     fn compare(
//         &self,
//         view: &mut SequenceView<char>,
//         acc: &mut String,
//         _context: &()
//     ) -> Result<(), String> {
//         if view.items().is_empty() {
//             return Err(format!("Unexpected end of expression; expected '{}'", self.char));
//         }
//         let actual_char = *view.items().first().unwrap();
//         if actual_char != self.char {
//             return Err(format!(
//                 "Expected '{}', but got '{}'",
//                 self.char,
//                 actual_char
//             ))
//         }
//         view.advance(1);
//         acc.push(actual_char);
//         Ok(())
//     }
// }

// #[derive(Debug)]
// pub struct RangeFragment {
//     from: char,
//     to: char
// }

// impl RangeFragment {
//     pub fn new(from: char, to: char) -> Self {
//         RangeFragment { from, to }
//     }
// }

// impl Fragment<char, String, ()> for RangeFragment {
//     fn compare(
//         &self,
//         view: &mut SequenceView<char>,
//         acc: &mut String,
//         _context: &()
//     ) -> Result<(), String> {
//         if view.items().is_empty() {
//             return Err(format!("Unexpected end of expression; expected char in range '{}'-'{}'", self.from, self.to))
//         }
//         let actual_char = *view.items().first().unwrap();
//         if actual_char < self.from || actual_char > self.to {
//             return Err(format!("Expected char in range '{}'-'{}', but got '{actual_char}'", self.from, self.to))
//         }
//         view.advance(1);
//         acc.push(actual_char);
//         Ok(())
//     }
// }

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
        if self.message.is_some() {
            write!(f, "Unexpected '{}' at index {}: {}", self.character, self.index, self.message.as_ref().unwrap())
        } else {
            write!(f, "Unexpected '{}' at index {}", self.character, self.index)
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
        let mut index = 0;

        for char in expr {
            index += 1;

            if !escape && *char == '\\' {
                escape = true;
                continue;
            }

            match mode {
                ExprParseState::Default => {
                    if !escape {
                        match char {
                            '*' => {
                                if !has_expression {
                                    return Err(ExprParseError::new(*char, index, None))
                                }
                                builder.pop_repeat();
                                continue;
                            }
                            '+' => {
                                if !has_expression {
                                    return Err(ExprParseError::new(*char, index, None))
                                }
                                builder.duplicate(1);
                                builder.pop_repeat();
                                continue;
                            }
                            '?' => {
                                if !has_expression {
                                    return Err(ExprParseError::new(*char, index, None))
                                }
                                builder.pop_optional();
                                continue;
                            }
                            '(' => {
                                builder.pop_return();
                                has_expression = false;
                                builder.push_return();
                                continue;
                            }
                            ')' => {
                                has_expression = true;
                                builder.pop_return();
                                continue;
                            },
                            '[' => {
                                builder.pop_return();
                                has_expression = false;
                                builder.push_return();
                                builder.push_return();
                                mode = ExprParseState::Choice;
                                continue;
                            },
                            ']' => {
                                panic!("Unexpected character ']' in expression")
                            }
                            _ => ()
                        }
                    }
                    if has_expression {
                        builder.pop_return();
                    }
                    builder.append(CharMatcher::Exact(*char), None);
                    builder.push_return();
                }
                ExprParseState::Choice => {
                    if !escape {
                        match char {
                            '-' => {
                                mode = ExprParseState::Range;
                                continue;
                            }
                            ']' => {
                                if let Some(b_char) = buffered_char {
                                    builder.append(CharMatcher::Exact(b_char), None);
                                }
                                builder.pop_choice();
                                has_expression = true;
                                mode = ExprParseState::Default;
                            }
                            _ => ()
                        }
                        
                    }
                    if let Some(b_char) = buffered_char {
                        builder.append(CharMatcher::Exact(b_char), None);
                    }
                    buffered_char = Some(*char);
                },
                ExprParseState::Range => {
                    if !escape {
                        match char {
                            ']' => {
                                return Err(ExprParseError::new(*char, index, Some(String::from("Unexpected end of choice after '-'"))));
                            }
                            _ => ()
                        }
                    }
                    builder.append(CharMatcher::Range(buffered_char.expect("No start of range found"), *char), None)
                }
            }

            if escape {
                escape = false;
            }
        }

        if escape {
            panic!("Incomplete escape sequence");
        }
        if mode == ExprParseState::Choice || mode == ExprParseState::Range {
            panic!("Unmatched opening square bracket");
        }

        builder.pop_return();
        Ok(builder.complete())
    }

    // fn parse_choice(expr: &[char]) -> Box<dyn Fragment<char, String, ()>> {
    //     let mut choices: Vec<Box<dyn Fragment<char, String, ()>>> = vec![];
    //     let mut state = ChoiceParseState::Default;
    //     let mut last: Option<char> = None;
    //     let mut escape = false;

    //     for char in expr {
    //         if !escape && *char == '\\' {
    //             escape = true;
    //             continue;
    //         }

    //         match state {
    //             ChoiceParseState::Default => {
    //                 if !escape {
    //                     match char {
    //                         '-' => {
    //                             choices.pop();
    //                             state = ChoiceParseState::Range;
    //                             continue;
    //                         }
    //                         _ => ()
    //                     }
    //                 }
    //                 last = Some(*char);
    //                 choices.push(Box::new(CharFragment::new(*char)))
    //             }
    //             ChoiceParseState::Range => {
    //                 choices.push(Box::new(
    //                     RangeFragment::new(
    //                         last.expect("Missing start of range"),
    //                         *char
    //                     )
    //                 ));
    //                 last = None;
    //                 state = ChoiceParseState::Default;
    //             }
    //         }

    //         if escape {
    //             escape = false;
    //         }
    //     }

    //     if escape {
    //         panic!("Incomplete escape sequence");
    //     }
    //     if state == ChoiceParseState::Range {
    //         panic!("Missing end of range");
    //     }

    //     if choices.len() == 1 {
    //         return choices.into_iter().next().unwrap();
    //     }

    //     Box::new(ChoiceFragment::new(choices))
    // }
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
        assert_eq!(matcher.compare("bcbcbcbcefdfs"), Err(String::from("(expr:0) Expected 'a', but got 'b'")), "'a(bc)+' not match 'bcbcbcbcefdfs'");
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
            Err(String::from("(expr:0) Expected one of: 'a', '[', '-'")),
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