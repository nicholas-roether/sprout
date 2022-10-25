use std::fmt;

use crate::fragment::{Fragment, SequenceFragment, SequenceView, RepeatFragment, ChoiceFragment};


struct CharFragment {
    char: char
}

impl CharFragment {
    fn new(char: char) -> Self {
        CharFragment { char }
    }
}

impl Fragment<char> for CharFragment {
    type Acc = String;

    fn compare(
        &self,
        view: &mut crate::fragment::SequenceView<char>,
        acc: &mut Self::Acc
    ) -> Result<(), String> {
        if view.items().is_empty() {
            return Err(format!("Unexpected end of expression; expected '{}'", self.char));
        }
        let actual_char = *view.items().first().unwrap();
        if actual_char != self.char {
            return Err(format!(
                "Expected '{}', but got '{}'",
                self.char,
                actual_char
            ))
        }
        view.advance(1);
        acc.push(actual_char);
        Ok(())
    }
}

pub struct RangeFragment {
    from: char,
    to: char
}

impl RangeFragment {
    pub fn new(from: char, to: char) -> Self {
        RangeFragment { from, to }
    }
}

impl Fragment<char> for RangeFragment {
    type Acc = String;

    fn compare(
        &self,
        view: &mut SequenceView<char>,
        acc: &mut Self::Acc
    ) -> Result<(), String> {
        if view.items().is_empty() {
            return Err(format!("Unexpected end of expression; expected char in range '{}'-'{}'", self.from, self.to))
        }
        let actual_char = *view.items().first().unwrap();
        if actual_char < self.from || actual_char > self.to {
            return Err(format!("Expected char in range '{}'-'{}', but got '{actual_char}'", self.from, self.to))
        }
        view.advance(1);
        acc.push(actual_char);
        Ok(())
    }
}

#[derive(PartialEq, Eq)]
enum ExprParseState {
    Default,
    SubExpr,
    Choice
}

#[derive(PartialEq, Eq)]
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

pub struct TokenMatcher {
    root_fragment: SequenceFragment<char, String>
}

impl TokenMatcher {
    pub fn expr(expr: &str) -> Result<Self, ExprParseError> {
        Ok(TokenMatcher {
            root_fragment: Self::parse_expr(&expr.chars().collect::<Vec<char>>())?
        })
    }

    pub fn compare(&self, string: &str) -> Result<String, String> {
        let cmp_chars: Vec<char> = string.chars().collect();
        let mut seq_view = SequenceView::new(&cmp_chars);
        let mut result_str = String::new();
        let result = self.root_fragment.compare(&mut seq_view, &mut result_str);
        if let Err(msg) = result {
            return Err(format!("(expr:{}) {}", result_str.len(), msg));
        }
        Ok(result_str)
    }

    fn parse_expr(expr: &[char]) -> Result<SequenceFragment<char, String>, ExprParseError> {
        let mut items: Vec<Box<dyn Fragment<char, Acc = String>>> = vec![];
        let mut current_item: Option<Box<dyn Fragment<char, Acc = String>>> = None;
        let mut mode = ExprParseState::Default;
        let mut buffer: Vec<char> = vec![];
        let mut depth = 0;
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
                                if current_item.is_none() {
                                    return Err(ExprParseError::new(*char, index, None))
                                }
                                current_item = Some(Box::new(RepeatFragment::new(
                                    current_item.unwrap().into(),
                                    0,
                                    None
                                )));
                                continue;
                            }
                            '+' => {
                                if current_item.is_none() {
                                    return Err(ExprParseError::new(*char, index, None))
                                }
                                current_item = Some(Box::new(RepeatFragment::new(
                                    current_item.unwrap().into(),
                                    1,
                                    None
                                )));
                                continue;
                            }
                            '?' => {
                                if current_item.is_none() {
                                    return Err(ExprParseError::new(*char, index, None))
                                }
                                current_item = Some(Box::new(RepeatFragment::new(
                                    current_item.unwrap().into(),
                                    0,
                                    Some(1)
                                )));
                                continue;
                            }
                            '(' => {
                                if let Some(item) = current_item {
                                    items.push(item);
                                }
                                current_item = None;
                                mode = ExprParseState::SubExpr;
                                continue;
                            }
                            ')' => {
                                return Err(ExprParseError::new(*char, index, Some(String::from("Unmatched closing parenthesis"))))
                            },
                            '[' => {
                                if let Some(item) = current_item {
                                    items.push(item);
                                }
                                current_item = None;
                                mode = ExprParseState::Choice;
                                continue;
                            },
                            ']' => {
                                return Err(ExprParseError::new(*char, index, Some(String::from("Unmatched closing square bracket"))))
                            }
                            _ => ()
                        }
                    }
                    if let Some(item) = current_item {
                        items.push(item);
                    }
                    current_item = Some(Box::new(CharFragment::new(*char)))
                }
                ExprParseState::SubExpr => {
                    if escape { buffer.push('\\'); }
                    match char {
                        '(' => depth += 1,
                        ')' => {
                            if depth > 0 {
                                depth -= 1;
                            } else {
                                let parsed_expr = Self::parse_expr(&buffer);
                                if let Err(err) = parsed_expr {
                                    return Err(ExprParseError::new(
                                        err.character,
                                        index + err.index,
                                        err.message
                                    ));
                                }
                                current_item = Some(Box::new(Self::parse_expr(&buffer)?));
                                buffer.clear();
                                mode = ExprParseState::Default;
                                continue;
                            }
                        }
                        _ => ()
                    }
                    buffer.push(*char)
                }
                ExprParseState::Choice => {
                    if escape {
                        if *char != ']' { buffer.push('\\'); }
                    } else {
                        if *char == ']' {
                            current_item = Some(Box::new(Self::parse_choice(&buffer)));
                            buffer.clear();
                            mode = ExprParseState::Default;
                            continue;
                        }
                    }
                    buffer.push(*char);
                }
            }

            if escape {
                escape = false;
            }
        }

        if escape {
            panic!("Incomplete escape sequence");
        }
        if mode == ExprParseState::SubExpr {
            panic!("Unmatched opening parenthesis");
        }
        if mode == ExprParseState::Choice {
            panic!("Unmatched opening square bracket");
        }

        if let Some(item) = current_item {
            items.push(item);
        }

        Ok(SequenceFragment::new(items))
    }

    fn parse_choice(expr: &[char]) -> ChoiceFragment<char, String> {
        let mut choices: Vec<Box<dyn Fragment<char, Acc = String>>> = vec![];
        let mut state = ChoiceParseState::Default;
        let mut last: Option<char> = None;
        let mut escape = false;

        for char in expr {
            if !escape && *char == '\\' {
                escape = true;
                continue;
            }

            match state {
                ChoiceParseState::Default => {
                    if !escape {
                        match char {
                            '-' => {
                                choices.clear();
                                state = ChoiceParseState::Range;
                                continue;
                            }
                            _ => ()
                        }
                    }
                    last = Some(*char);
                    choices.push(Box::new(CharFragment::new(*char)))
                }
                ChoiceParseState::Range => {
                    choices.push(Box::new(
                        RangeFragment::new(
                            last.expect("Missing start of range"),
                            *char
                        )
                    ));
                    last = None;
                    state = ChoiceParseState::Default;
                }
            }

            if escape {
                escape = false;
            }
        }

        if escape {
            panic!("Incomplete escape sequence");
        }
        if state == ChoiceParseState::Range {
            panic!("Missing end of range");
        }

        ChoiceFragment::new(choices)
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
    fn char_fragment_works() {
        let fragment = CharFragment::new('a');
        
        let mut strbuf: Vec<char> = vec![];

        let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("a", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("a"));

        let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("abc", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("a"));

        let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("fsfd", &mut strbuf), &mut str), Err(String::from("Expected 'a', but got 'f'")));


        let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("", &mut strbuf), &mut str), Err(String::from("Unexpected end of expression; expected 'a'")));
    }

    #[test]
    fn range_fragment_works() {
        let fragment = RangeFragment::new('1', '3');

        let mut strbuf: Vec<char> = vec![];

        let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("0", &mut strbuf), &mut str), Err(String::from("Expected char in range '1'-'3', but got '0'")));

        let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("1", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("1"));

        let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("2", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("2"));

        let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("3", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("3"));

        let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("4", &mut strbuf), &mut str), Err(String::from("Expected char in range '1'-'3', but got '4'")));
    }

    #[test]
    fn can_match_exact() {
        let matcher = TokenMatcher::expr("abc").unwrap();

        assert_eq!(matcher.compare("abc"), Ok(String::from("abc")), "'abc' should match first 3 chars in 'abc'");
        assert_eq!(matcher.compare("abcdefg"), Ok(String::from("abc")), "'abc' should match first 3 chars in 'abcdefg'");
        assert_eq!(matcher.compare("abedefg"), Err(String::from("(expr:2) Expected 'c', but got 'e'")), "'abc' should not match in 'abedefg'");
    }

    #[test]
    fn can_repeat_single() {
        let matcher = TokenMatcher::expr("ab*").unwrap();
        
        assert_eq!(matcher.compare("abbbbbc"), Ok(String::from("abbbbb")), "'ab*' should match first 6 chars in 'abbbbbc'");
        assert_eq!(matcher.compare("acfds"), Ok(String::from("a")), "'ab*' should match first char in 'acfds'");
        assert_eq!(matcher.compare("cgfds"), Err(String::from("(expr:0) Expected 'a', but got 'c'")), "'ab*' should not match in 'cgfds'");
    }

    #[test]
    fn can_repeat_multiple() {
        let matcher = TokenMatcher::expr("a(bc)*").unwrap();

        assert_eq!(matcher.compare("abcbcbcbcefdfs"), Ok(String::from("abcbcbcbc")), "'a(bc)*' should match first 9 chars in 'abcbcbcbcefdfs'");
        assert_eq!(matcher.compare("bcbcbcbcefdfs"), Err(String::from("(expr:0) Expected 'a', but got 'b'")), "'a(bc)*' should not match 'bcbcbcbcefdfs'");
    }

    #[test]
    fn can_repeat_multiple_plus() {
        let matcher = TokenMatcher::expr("a(bc)+").unwrap();

        assert_eq!(matcher.compare("abcbcbcbcefdfs"), Ok(String::from("abcbcbcbc")), "'a(bc)+' should match first 9 chars in 'abcbcbcbcefdfs'");
        assert_eq!(matcher.compare("bcbcbcbcefdfs"), Err(String::from("(expr:0) Expected 'a', but got 'b'")), "'a(bc)+' not match 'bcbcbcbcefdfs'");
        assert_eq!(matcher.compare("aefdfs"), Err(String::from("(expr:1) Expected 'b', but got 'e'")), "'a(bc)+' should not match 'aefdfs'");
    }

    #[test]
    fn optional_works() {
        let matcher = TokenMatcher::expr("a(bc)?").unwrap();

        assert_eq!(matcher.compare("abcbcbcbcefdfs"), Ok(String::from("abc")), "'a(bc)?' should match first 3 chars in 'abcbcbcbcefdfs'");
        assert_eq!(matcher.compare("adbcbcbcbcefdfs"), Ok(String::from("a")), "'a(bc)?' should match the first character of 'adbcbcbcbcefdfs'");
        assert_eq!(matcher.compare("gdfefdfs"), Err(String::from("(expr:0) Expected 'a', but got 'g'")), "'a(bc)?' not match 'gdfefdfs'");
    }

    #[test]
    fn choice_works() {
        let matcher = TokenMatcher::expr("a[bc]").unwrap();

        assert_eq!(matcher.compare("ab534"), Ok(String::from("ab")), "'a[bc]' should match the first 2 chars in 'ab534'");
        assert_eq!(matcher.compare("ac534"), Ok(String::from("ac")), "'a[bc]' should match the first 2 chars in 'ac534'");
        assert_eq!(matcher.compare("a534"), Err(String::from("(expr:1) Expected 'b', but got '5'")), "'a[bc]' should not match 'a534'");
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
        assert_eq!(matcher.compare("(([a"), Err(String::from("(expr:3) Expected '*', but got 'a'")), "'\\(\\(\\[\\*' should not match '(([a'");
    }

    #[test]
    fn escape_in_choice_works() {
        let matcher = TokenMatcher::expr("[a\\-\\]]").unwrap();

        assert_eq!(matcher.compare("a"), Ok(String::from("a")), "'[a\\-\\]]' should match 'a'");
        assert_eq!(matcher.compare("]"), Ok(String::from("]")), "'[a\\-\\]]' should match ']'");
        assert_eq!(matcher.compare("-"), Ok(String::from("-")), "'[a\\-\\]]' should match '-'");
        assert_eq!(matcher.compare("bet46r"), Err(String::from("(expr:0) Expected 'a', but got 'b'")), "'[a\\-\\]]' should not match 'bet46r'");
    }

    #[test]
    fn ranges_work() {
        let matcher = TokenMatcher::expr("[a-z]").unwrap();

        assert_eq!(matcher.compare("v"), Ok(String::from("v")), "'[a-z]' should match 'v'");
        assert_eq!(matcher.compare("s"), Ok(String::from("s")), "'[a-z]' should match 's'");
        assert_eq!(matcher.compare("b"), Ok(String::from("b")), "'[a-z]' should match 'b'");
        assert_eq!(matcher.compare("A"), Err(String::from("(expr:0) Expected char in range 'a'-'z', but got 'A'")), "'[a-z]' should not match 'A'");
        assert_eq!(matcher.compare("-"), Err(String::from("(expr:0) Expected char in range 'a'-'z', but got '-'")), "'[a-z]' should not match '-'");
    }
}