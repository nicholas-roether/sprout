use std::fmt::Debug;

trait MatchItem where Self: Debug {
    fn compare_part(&self, string: &str) -> Option<usize>;
}

#[derive(Debug)]
struct ExactMatchItem {
    exact: String
}

impl ExactMatchItem {
    fn new(exact: &str) -> Self {
        ExactMatchItem { exact: String::from(exact) }
    }

    fn single(char: char) -> Self {
        ExactMatchItem { exact: char.to_string() }
    }
}

impl MatchItem for ExactMatchItem {
    fn compare_part(&self, string: &str) -> Option<usize> {
        if string.starts_with(self.exact.as_str()) {
            return Some(self.exact.len());
        }
        None
    }
}

#[derive(Debug)]
struct RepeatMatchItem {
    item: Box<dyn MatchItem>,
    min_reps: u32,
    max_reps: Option<u32>
}

impl RepeatMatchItem {
    fn new(item: Box<dyn MatchItem>) -> Self {
        RepeatMatchItem { item, min_reps: 0, max_reps: None }
    }
}

impl MatchItem for RepeatMatchItem {
    fn compare_part(&self, string: &str) -> Option<usize> {
        let mut num_reps: u32 = 0;
        let mut size: usize = 0;
        loop {
            if let Some(max) = self.max_reps {
                if num_reps >= max { break; } 
            }
            if let Some(part_length) = self.item.compare_part(&string[size..]) {
                size += part_length
            } else {
                break;
            }
            num_reps += 1;
        }
        if num_reps < self.min_reps {
            return None;
        }
        Some(size)
    }
}

trait Choice {
    fn check(&self, char: char) -> bool;
}

struct ExactChoice {
    char: char
}

impl ExactChoice {
    fn new(char: char) -> Self {
        ExactChoice { char }
    }
}

impl Choice for ExactChoice {
    fn check(&self, char: char) -> bool {
        char == self.char
    }
}

#[derive(Debug)]
struct ChoiceMatcher {
    choices: Vec<char>
}

impl ChoiceMatcher {
    fn new(expr: &str) -> Self {
        ChoiceMatcher { choices: expr.chars().collect() }
    }
}

impl MatchItem for ChoiceMatcher {
    fn compare_part(&self, string: &str) -> Option<usize> {
        for choice in &self.choices {
            if string.starts_with(&[*choice]) {
                return Some(1);
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct TokenMatcher {
    items: Vec<Box<dyn MatchItem>>
}

#[derive(PartialEq, Eq)]
enum TokenParseMode {
    Default,
    SubExpr,
    Choice
}

impl TokenMatcher {
    pub fn new(expr: &str) -> Self {
        TokenMatcher {
            items: Self::parse(expr)
        }
    }

    fn group(items: Vec<Box<dyn MatchItem>>) -> Self {
        TokenMatcher { items }
    }

    fn parse(expr: &str) -> Vec<Box<dyn MatchItem>> {
        const SPECIAL_CHARS: &str = "*";

        let mut items: Vec<Box<dyn MatchItem>> = vec![];
        let mut current_item: Option<Box<dyn MatchItem>> = None;
        let mut mode = TokenParseMode::Default;
        let mut buffer = String::new();
        let mut depth = 0;
        let mut escape = false;

        for char in expr.chars() {
            if mode != TokenParseMode::SubExpr && !escape && char == '\\' {
                escape = true;
                continue;
            }

            match mode {
                TokenParseMode::Default => {
                    if !escape {
                        match char {
                            '*' => {
                                current_item = Some(Box::new(RepeatMatchItem::new(
                                    current_item.expect("Invalid placement for '*'")
                                )));
                                continue;
                            }
                            '+' => {
                                let mut repeat_item = RepeatMatchItem::new(
                                    current_item.expect("Invalid placement for '*'")
                                );
                                repeat_item.min_reps = 1;
                                current_item = Some(Box::new(repeat_item));
                                continue;
                            }
                            '?' => {
                                let mut repeat_item = RepeatMatchItem::new(
                                    current_item.expect("Invalid placement for '*'")
                                );
                                repeat_item.max_reps = Some(1);
                                current_item = Some(Box::new(repeat_item));
                                continue;
                            }
                            '(' => {
                                if let Some(item) = current_item {
                                    items.push(item);
                                }
                                current_item = None;
                                mode = TokenParseMode::SubExpr;
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
                                mode = TokenParseMode::Choice;
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
                    current_item = Some(Box::new(ExactMatchItem::single(char)))
                }
                TokenParseMode::SubExpr => {
                    match char {
                        '(' => depth += 1,
                        ')' => {
                            if depth > 0 {
                                depth -= 1;
                            } else {
                                current_item = Some(Box::new(TokenMatcher::new(buffer.as_str())));
                                buffer.clear();
                                mode = TokenParseMode::Default;
                                continue;
                            }
                        }
                        _ => ()
                    }
                    buffer += char.to_string().as_str()
                }
                TokenParseMode::Choice => {
                    if !escape && char == ']' {
                        current_item = Some(Box::new(ChoiceMatcher::new(buffer.as_str())));
                        buffer.clear();
                        mode = TokenParseMode::Default;
                        continue;
                    }
                    buffer += char.to_string().as_str()
                }
            }

            if escape {
                escape = false;
            }
        }

        if mode == TokenParseMode::SubExpr {
            panic!("Unmatched opening parenthesis");
        }
        if mode == TokenParseMode::Choice {
            panic!("Unmatched opening square bracket");
        }

        if let Some(item) = current_item {
            items.push(item);
        }

        println!("{items:?}");
        items
    }

    fn compare(&self, string: &str) -> Option<usize> {
        return self.compare_part(string);
    }
}

impl MatchItem for TokenMatcher {
    fn compare_part(&self, string: &str) -> Option<usize> {
        let mut size: usize = 0;
        for item in &self.items {
            if let Some(part_length) = item.compare_part(&string[size..]) {
                size += part_length;
            } else {
                return None;
            }
        }
        Some(size)
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
        assert_eq!(matcher.compare("A"), Some(1), "'[a-z]' should not match 'A'");
        assert_eq!(matcher.compare("-"), Some(1), "'[a-z]' should match '-'");
    }
}