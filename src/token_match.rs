struct TokenMatcherState {
    eval_index: usize,
    cmp_index: usize,
    loop_stack: Vec<usize>,
    escape: bool,
    optional: bool,
    token_length: usize,
}

impl TokenMatcherState {
    fn new() -> Self {
        TokenMatcherState {
            eval_index: 0,
            cmp_index: 0,
            loop_stack: vec![],
            escape: false,
            optional: false,
            token_length: 0,
        }
    }
}

struct TokenMatcher {
    state: TokenMatcherState,
    expr: String,
    depth: u32
}

impl TokenMatcher {
    fn new(expr: &str) -> Self {
        TokenMatcher {
            state: TokenMatcherState::new(),
            expr: String::from(expr),
            depth: 0
        }
    }

    fn compare(&mut self, string: &str) -> Option<usize> {
        let mut char: char;
        let mut eval_char: char;

        loop {
            eval_char = self.expr.chars().nth(self.state.eval_index).unwrap_or('\0');
            char = string.chars().nth(self.state.cmp_index).unwrap_or('\0');
            println!("{}: {eval_char} -> {char}", self.depth);

            if eval_char == '\0' {
                if self.depth != 0 { panic!("Unmatched opening parenthesis"); }
                break;
            }
            if eval_char == ')' {
                if self.depth == 0 { panic!("Unmatced closing parenthesis"); }
                break;
            }
            if char == '\0' { return None; }
            
            match eval_char {
                '*' => {
                    self.state.loop_stack.push(self.state.eval_index);
                }
                '(' => {
                    let mut child_matcher = TokenMatcher::new(&self.expr[self.state.eval_index + 1 ..]);
                    child_matcher.depth = self.depth + 1;
                    if let Some(result) = child_matcher.compare(&string[self.state.cmp_index..]) {
                        self.state.token_length += result;
                        self.state.cmp_index += result;
                        self.state.eval_index += child_matcher.state.eval_index + 1;
                    } else {
                        return None;
                    }
                }
                _ => {
                    if eval_char == char {
                        self.state.token_length += 1;
                        self.state.cmp_index += 1;
                    } else {
                        if self.state.optional {
                            self.state.optional = false
                        } else if self.state.loop_stack.len() > 0 {
                            self.state.loop_stack.pop();
                        } else {
                            return None;
                        }
                    }

                    if let Some(next_eval) = self.state.loop_stack.pop() {
                        self.state.eval_index = next_eval - 1;
                    }
                }
            }
            
            self.state.eval_index += 1;
        }

        Some(self.state.token_length)
    }
}

#[cfg(test)]
mod tests {
    use super::TokenMatcher;

    #[test]
    fn can_match_exact() {
        let mut matcher = TokenMatcher::new("abc");
        assert_eq!(matcher.compare("abc"), Some(3), "'abc' should match first 3 chars in 'abc'");

        let mut matcher = TokenMatcher::new("abc");
        assert_eq!(matcher.compare("abcdefg"), Some(3), "'abc' should match first 3 chars in 'abcdefg'");

        let mut matcher = TokenMatcher::new("abc");
        assert_eq!(matcher.compare("abedefg"), None, "'abc' should not match in 'abedefg'");
    }

    #[test]
    fn can_repeat_single() {
        let mut matcher = TokenMatcher::new("a*b");
        assert_eq!(matcher.compare("abbbbbc"), Some(6), "'a*b' should match first 6 chars in 'abbbbbc'");

        let mut matcher = TokenMatcher::new("a*b");
        assert_eq!(matcher.compare("acfds"), Some(1), "'a*b' should match first char in 'acfds'");

        let mut matcher = TokenMatcher::new("a*b");
        assert_eq!(matcher.compare("cgfds"), None, "'a*b' should not match in 'cgfds'");
    }

    #[test]
    fn can_repeat_multiple() {
        let mut matcher = TokenMatcher::new("a*(bc)");
        assert_eq!(matcher.compare("abcbcbcbcefdfs"), Some(9), "'a*(bc)' should match first 6 chars in 'abcbcbcbcefdfs'");

        let mut matcher = TokenMatcher::new("a*(bc)");
        assert_eq!(matcher.compare("bcbcbcbcefdfs"), None, "'a*(bc)' not match 'bcbcbcbcefdfs'");
    }
}