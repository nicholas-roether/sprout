use std::{collections::{HashMap}};

#[derive(Clone)]
struct TokenMatcherState {
    eval_index: usize,
    cmp_index: usize,
    loop_stack: Vec<usize>,
    skip: bool
}

impl TokenMatcherState {
    fn new() -> Self {
        TokenMatcherState {
            eval_index: 0,
            cmp_index: 0,
            loop_stack: vec![],
            skip: false
        }
    }
}

struct TokenMatcherBranches {
    branches: HashMap<u32, TokenMatcherState>,
    next_branch_id: u32
}

impl TokenMatcherBranches {
    fn new() -> Self {
        TokenMatcherBranches { branches: HashMap::new(), next_branch_id: 0 }
    }

    fn spawn_branch(&mut self, state: TokenMatcherState) {
        self.branches.insert(self.next_branch_id, state);
        self.next_branch_id += 1;
    }

    fn get_branch_state(&mut self, branch: u32) -> Option<&TokenMatcherState> {
        self.branches.get(&branch)
    }

    fn terminate_branch(&mut self, branch: u32) {
        self.branches.remove(&branch);
    }

    fn running(&self) -> bool {
        self.branches.len() > 0
    }

    fn branch_ids(&self) -> Vec<u32> {
        self.branches.keys().map(|b| *b).collect()
    }

    fn state(&self, branch: u32) -> Option<&TokenMatcherState> {
        self.branches.get(&branch)
    }

    fn state_mut(&mut self, branch: u32) -> Option<&mut TokenMatcherState> {
        self.branches.get_mut(&branch)
    }
}

pub struct TokenMatcher {
    pub expr: String,
}

impl TokenMatcher {
    pub fn new(expr: &str) -> Self {
        TokenMatcher {
            expr: String::from(expr),
        }
    }

    pub fn compare(&mut self, string: &str) -> Option<usize> {
        let mut branches = TokenMatcherBranches::new();
        branches.spawn_branch(TokenMatcherState::new());

        while branches.running() {
            for branch in branches.branch_ids() {
                let mut state = branches.state_mut(branch).unwrap();
                let eval_char = self.expr.chars().nth(state.eval_index).unwrap_or('\0');
                let cmp_char = string.chars().nth(state.eval_index).unwrap_or('\0');
                if eval_char == '\0' { return Some(state.cmp_index) }
                if cmp_char == '\0' {
                    branches.terminate_branch(branch);
                    continue;
                }

                match eval_char {
                    '*' => {
                        let mut loop_branch_state = state.clone();
                        loop_branch_state.loop_stack.push(state.eval_index);
                        state.skip = true;

                        branches.spawn_branch(loop_branch_state);

                    }
                    _ => {
                        if eval_char != cmp_char {
                            branches.terminate_branch(branch);
                            continue;
                        } else {
                            state.cmp_index += 1;
                        }
                    }
                }

                branches.state_mut(branch).unwrap().eval_index += 1;
            }
        }

        None
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