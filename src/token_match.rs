use crate::fragment::{SequenceFragment, Fragment, RepeatFragment, ExactFragment, ChoiceFragment, RangeFragment};

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
pub struct TokenMatcher {
    root_fragment: SequenceFragment<char>
}

impl TokenMatcher {
    pub fn new(expr: &str) -> Self {
        TokenMatcher { root_fragment: Self::parse_expr(&expr.chars().collect::<Vec<char>>()) }
    }

    pub fn compare(&self, string: &str) -> Option<usize> {
        let cmp_chars: Vec<char> = string.chars().collect();
        self.root_fragment.compare(&cmp_chars)
    }

    fn parse_expr(expr: &[char]) -> SequenceFragment<char> {
        let mut items: Vec<Box<dyn Fragment<char>>> = vec![];
        let mut current_item: Option<Box<dyn Fragment<char>>> = None;
        let mut mode = ExprParseState::Default;
        let mut buffer: Vec<char> = vec![];
        let mut depth = 0;
        let mut escape = false;

        for char in expr {
            if !escape && *char == '\\' {
                escape = true;
                continue;
            }

            match mode {
                ExprParseState::Default => {
                    if !escape {
                        match char {
                            '*' => {
                                current_item = Some(Box::new(RepeatFragment::new(
                                    current_item.expect("Invalid placement for '*'"),
                                    0,
                                    None
                                )));
                                continue;
                            }
                            '+' => {
                                current_item = Some(Box::new(RepeatFragment::new(
                                    current_item.expect("Invalid placement for '*'"),
                                    1,
                                    None
                                )));
                                continue;
                            }
                            '?' => {
                                current_item = Some(Box::new(RepeatFragment::new(
                                    current_item.expect("Invalid placement for '*'"),
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
                                panic!("Unmatched closing parenthesis");
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
                                panic!("Unmatched closing square bracket");
                            }
                            _ => ()
                        }
                    }
                    if let Some(item) = current_item {
                        items.push(item);
                    }
                    current_item = Some(Box::new(ExactFragment::new(*char)))
                }
                ExprParseState::SubExpr => {
                    if escape { buffer.push('\\'); }
                    match char {
                        '(' => depth += 1,
                        ')' => {
                            if depth > 0 {
                                depth -= 1;
                            } else {
                                current_item = Some(Box::new(Self::parse_expr(&buffer)));
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

        SequenceFragment::new(items)
    }

    fn parse_choice(expr: &[char]) -> ChoiceFragment<char> {
        let mut choices: Vec<Box<dyn Fragment<char>>> = vec![];
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
                                state = ChoiceParseState::Range;
                                continue;
                            }
                            _ => ()
                        }
                    }
                    last = Some(*char);
                    choices.push(Box::new(ExactFragment::new(*char)))
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
    use super::TokenMatcher;

    #[test]
    fn can_match_exact() {
        let matcher = TokenMatcher::new("abc");

        assert_eq!(matcher.compare("abc"), Some(3), "'abc' should match first 3 chars in 'abc'");
        assert_eq!(matcher.compare("abcdefg"), Some(3), "'abc' should match first 3 chars in 'abcdefg'");
        assert_eq!(matcher.compare("abedefg"), None, "'abc' should not match in 'abedefg'");
    }

    #[test]
    fn can_repeat_single() {
        let matcher = TokenMatcher::new("ab*");
        
        assert_eq!(matcher.compare("abbbbbc"), Some(6), "'ab*' should match first 6 chars in 'abbbbbc'");
        assert_eq!(matcher.compare("acfds"), Some(1), "'ab*' should match first char in 'acfds'");
        assert_eq!(matcher.compare("cgfds"), None, "'ab*' should not match in 'cgfds'");
    }

    #[test]
    fn can_repeat_multiple() {
        let matcher = TokenMatcher::new("a(bc)*");

        assert_eq!(matcher.compare("abcbcbcbcefdfs"), Some(9), "'a(bc)*' should match first 9 chars in 'abcbcbcbcefdfs'");
        assert_eq!(matcher.compare("bcbcbcbcefdfs"), None, "'a(bc)*' should not match 'bcbcbcbcefdfs'");
    }

    #[test]
    fn can_repeat_multiple_plus() {
        let matcher = TokenMatcher::new("a(bc)+");

        assert_eq!(matcher.compare("abcbcbcbcefdfs"), Some(9), "'a(bc)+' should match first 9 chars in 'abcbcbcbcefdfs'");
        assert_eq!(matcher.compare("bcbcbcbcefdfs"), None, "'a(bc)+' not match 'bcbcbcbcefdfs'");
        assert_eq!(matcher.compare("aefdfs"), None, "'a(bc)+' should not match 'aefdfs'");
    }

    #[test]
    fn optional_works() {
        let matcher = TokenMatcher::new("a(bc)?");

        assert_eq!(matcher.compare("abcbcbcbcefdfs"), Some(3), "'a(bc)?' should match first 3 chars in 'abcbcbcbcefdfs'");
        assert_eq!(matcher.compare("adbcbcbcbcefdfs"), Some(1), "'a(bc)?' should match the first character of 'adbcbcbcbcefdfs'");
        assert_eq!(matcher.compare("gdfefdfs"), None, "'a(bc)?' not match 'gdfefdfs'");
    }

    #[test]
    fn choice_works() {
        let matcher = TokenMatcher::new("a[bc]");

        assert_eq!(matcher.compare("ab534"), Some(2), "'a[bc]' should match the first 2 chars in 'ab534'");
        assert_eq!(matcher.compare("ac534"), Some(2), "'a[bc]' should match the first 2 chars in 'ac534'");
        assert_eq!(matcher.compare("a534"), None, "'a[bc]' should not match 'a534'");
    }

    #[test]
    fn repeated_choice_works() {
        let matcher = TokenMatcher::new("a[bc]*");

        assert_eq!(matcher.compare("abbcbccccbbc453"), Some(12), "'a[bc]*' should match the first 12 chars in 'abbcbccccbbc453'");
    }

    #[test]
    fn escape_works() {
        let matcher = TokenMatcher::new("\\(\\(\\[\\*");

        assert_eq!(matcher.compare("(([*"), Some(4), "'\\(\\(\\[\\*' should match the first 4 chars in '(([*'");
        assert_eq!(matcher.compare("(([a"), None, "'\\(\\(\\[\\*' should not match '(([a'");
    }

    #[test]
    fn escape_in_choice_works() {
        let matcher = TokenMatcher::new("[a\\-\\]]");

        assert_eq!(matcher.compare("a"), Some(1), "'[a\\-\\]]' should match 'a'");
        assert_eq!(matcher.compare("]"), Some(1), "'[a\\-\\]]' should match ']'");
        assert_eq!(matcher.compare("-"), Some(1), "'[a\\-\\]]' should match '-'");
        assert_eq!(matcher.compare("bet46r"), None, "'[a\\-\\]]' should not match 'bet46r'");
    }

    #[test]
    fn ranges_work() {
        let matcher = TokenMatcher::new("[a-z]");

        assert_eq!(matcher.compare("v"), Some(1), "'[a-z]' should match 'v'");
        assert_eq!(matcher.compare("s"), Some(1), "'[a-z]' should match 's'");
        assert_eq!(matcher.compare("b"), Some(1), "'[a-z]' should match 'b'");
        assert_eq!(matcher.compare("A"), None, "'[a-z]' should not match 'A'");
        assert_eq!(matcher.compare("-"), None, "'[a-z]' should not match '-'");
    }
}