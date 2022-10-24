use std::{fmt};

pub trait Fragment where Self: fmt::Debug {
    type Item;

    fn compare(&self, seq: &[Self::Item]) -> Option<usize>;
}

pub trait Check where Self: fmt::Debug {
    type Type;

    fn check(&self, other: &Self::Type) -> bool;
}

#[derive(Debug)]
pub struct CheckFragment<C: Check> {
    checker: C
}

impl<C: Check> CheckFragment<C> {
    pub fn new(checker: C) -> Self {
        CheckFragment { checker }
    }
}

impl<U, C: Check<Type = U>> Fragment for CheckFragment<C> {
    type Item = U;

    fn compare(&self, seq: &[U]) -> Option<usize> {
        if let Some(item) = seq.iter().next() {
            if self.checker.check(item) {
                return Some(1)
            }
        }
        return None;
    }
}

#[derive(Debug)]
pub struct ExactFragment<U: Eq + fmt::Debug> {
   exact: U
}

impl<U: Eq + fmt::Debug> ExactFragment<U> {
    pub fn new(exact: U) -> Self {
        ExactFragment { exact }
    }
}

impl<U: Eq + fmt::Debug> Fragment for ExactFragment<U> {
    type Item = U;

    fn compare(&self, seq: &[U]) -> Option<usize> {
        if seq.iter().next() == Some(&self.exact) {
            return Some(1)
        }
        None
    }
}

#[derive(Debug)]
pub struct RepeatFragment<U> {
    item: Box<dyn Fragment<Item = U>>,
    min_reps: u32,
    max_reps: Option<u32>
}

impl<U> RepeatFragment<U> {
    pub fn new(item: Box<dyn Fragment<Item = U>>, min_reps: u32, max_reps: Option<u32>) -> Self {
        RepeatFragment { item, min_reps, max_reps }
    }
}

impl<U: fmt::Debug> Fragment for RepeatFragment<U> {
    type Item = U;

    fn compare(&self, seq: &[U]) -> Option<usize> {
        let mut num_reps: u32 = 0;
        let mut size: usize = 0;
        loop {
            if let Some(max) = self.max_reps {
                if num_reps >= max { break; }
            }
            if let Some(part_length) = self.item.compare(&seq[size..]) {
                size += part_length;
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

#[derive(Debug)]
pub struct ChoiceFragment<U> {
    choices: Vec<Box<dyn Fragment<Item = U>>>
}

impl<U> ChoiceFragment<U> {
    pub fn new(choices: Vec<Box<dyn Fragment<Item = U>>>) -> Self {
        ChoiceFragment { choices }
    }
}

impl<U: fmt::Debug> Fragment for ChoiceFragment<U> {
    type Item = U;

    fn compare(&self, seq: &[U]) -> Option<usize> {
        for choice in &self.choices {
            if let Some(len) = choice.compare(seq) {
                return Some(len)
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct RangeFragment<U: Eq + Ord + fmt::Debug> {
    from: U,
    to: U
}

impl<U: Eq + Ord + fmt::Debug> RangeFragment<U> {
    pub fn new(from: U, to: U) -> Self {
        RangeFragment { from, to }
    }
}

impl<U: Eq + Ord + fmt::Debug> Fragment for RangeFragment<U> {
    type Item = U;

    fn compare(&self, seq: &[U]) -> Option<usize> {
        let first = seq.iter().next();
        if matches!(first, None) { return None }
        let first = first.unwrap();
        if !(&self.from <= first && first <= &self.to) { return None }
        Some(1)
    }
}

#[derive(Debug)]
pub struct SequenceFragment<U> {
    items: Vec<Box<dyn Fragment<Item = U>>>
}

impl<U> SequenceFragment<U> {
    pub fn new(items: Vec<Box<dyn Fragment<Item = U>>>) -> Self {
        SequenceFragment { items }
    }
}

impl<U: fmt::Debug> Fragment for SequenceFragment<U> {
    type Item = U;

    fn compare(&self, seq: &[U]) -> Option<usize> {
        let mut size: usize = 0;
        for item in &self.items {
            if let Some(part_length) = item.compare(&seq[size..]) {
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
    use super::*;

    fn stov(str: &str) -> Vec<char> {
        str.chars().collect()
    }

    #[test]
    fn check_fragment_works() {
        #[derive(Debug)]
        struct TestChecker;

        impl Check for TestChecker {
            type Type = char;

            fn check(&self, other: &char) -> bool {
                *other == 'x'
            }
        }

        let fragment = CheckFragment::new(TestChecker {});

        assert_eq!(fragment.compare(&stov("a")), None);
        assert_eq!(fragment.compare(&stov("x")), Some(1));
        assert_eq!(fragment.compare(&stov("xxa")), Some(1));
        assert_eq!(fragment.compare(&stov("sx")), None);
    }

    #[test]
    fn exact_fragment_works() {
        let fragment = ExactFragment::new('a');

        assert_eq!(fragment.compare(&stov("a")), Some(1));
        assert_eq!(fragment.compare(&stov("abc")), Some(1));
        assert_eq!(fragment.compare(&stov("fsfd")), None);
        assert_eq!(fragment.compare(&stov("")), None);
    }

    #[test]
    fn repeat_fragment_works() {
        let fragment = RepeatFragment::new(
            Box::new(ExactFragment::new('x')),
            0,
            None
        );
        
        assert_eq!(fragment.compare(&stov("")), Some(0));
        assert_eq!(fragment.compare(&stov("fhg")), Some(0));
        assert_eq!(fragment.compare(&stov("xfgh")), Some(1));
        assert_eq!(fragment.compare(&stov("xxxxxxxxzut")), Some(8));
    }

    #[test]
    fn repeat_fragment_min_reps_works() {
        let fragment = RepeatFragment::new(
            Box::new(ExactFragment::new('x')),
            2,
            None
        );

        assert_eq!(fragment.compare(&stov("")), None);
        assert_eq!(fragment.compare(&stov("x")), None);
        assert_eq!(fragment.compare(&stov("xx")), Some(2));
    }

    #[test]
    fn repeat_fragment_max_reps_works() {
        let fragment = RepeatFragment::new(
            Box::new(ExactFragment::new('x')),
            0,
            Some(4)
        );

        assert_eq!(fragment.compare(&stov("xxxx")), Some(4));
        assert_eq!(fragment.compare(&stov("xxxxx")), Some(4));
    }

    #[test]
    fn choice_fragment_works() {
        let fragment = ChoiceFragment::new(vec![
            Box::new(ExactFragment::new('a')),
            Box::new(ExactFragment::new('b'))
        ]);

        assert_eq!(fragment.compare(&stov("a")), Some(1));
        assert_eq!(fragment.compare(&stov("b")), Some(1));
        assert_eq!(fragment.compare(&stov("c")), None);
    }

    #[test]
    fn range_fragment_works() {
        let fragment = RangeFragment::new('1', '3');

        assert_eq!(fragment.compare(&stov("0")), None);
        assert_eq!(fragment.compare(&stov("1")), Some(1));
        assert_eq!(fragment.compare(&stov("2")), Some(1));
        assert_eq!(fragment.compare(&stov("3")), Some(1));
        assert_eq!(fragment.compare(&stov("4")), None);
    }

    #[test]
    fn sequence_fragment_works() {
        let fragment = SequenceFragment::new(vec![
            Box::new(ExactFragment::new('a')),
            Box::new(ExactFragment::new('b'))
        ]);

        assert_eq!(fragment.compare(&stov("agfgdh")), None);
        assert_eq!(fragment.compare(&stov("abgfgdh")), Some(2));
        assert_eq!(fragment.compare(&stov("abbgfgdh")), Some(2));
    }
}