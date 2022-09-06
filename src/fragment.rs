use std::{fmt};

pub trait Fragment<U: Eq> where Self: fmt::Debug {
    fn compare(&self, seq: &[U]) -> Option<usize>;
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

impl<U: Eq + fmt::Debug> Fragment<U> for ExactFragment<U> {
    fn compare(&self, seq: &[U]) -> Option<usize> {
        if seq.iter().next() == Some(&self.exact) {
            return Some(1)
        }
        None
    }
}

#[derive(Debug)]
pub struct RepeatFragment<U: Eq + fmt::Debug> {
    item: Box<dyn Fragment<U>>,
    min_reps: u32,
    max_reps: Option<u32>
}

impl<U: Eq + fmt::Debug> RepeatFragment<U> {
    pub fn new(item: Box<dyn Fragment<U>>, min_reps: u32, max_reps: Option<u32>) -> Self {
        RepeatFragment { item, min_reps, max_reps }
    }
}

impl<U: Eq + fmt::Debug + fmt::Debug> Fragment<U> for RepeatFragment<U> {
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
pub struct ChoiceFragment<U: Eq + fmt::Debug> {
    choices: Vec<Box<dyn Fragment<U>>>
}

impl<U: Eq + fmt::Debug> ChoiceFragment<U> {
    pub fn new(choices: Vec<Box<dyn Fragment<U>>>) -> Self {
        ChoiceFragment { choices }
    }
}

impl<U: Eq + fmt::Debug + fmt::Debug> Fragment<U> for ChoiceFragment<U> {
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

impl<U: Eq + Ord + fmt::Debug> Fragment<U> for RangeFragment<U> {
    fn compare(&self, seq: &[U]) -> Option<usize> {
        let first = seq.iter().next();
        if matches!(first, None) { return None }
        let first = first.unwrap();
        if !(&self.from <= first && first <= &self.to) { return None }
        Some(1)
    }
}

#[derive(Debug)]
pub struct SequenceFragment<U: Eq + fmt::Debug> {
    items: Vec<Box<dyn Fragment<U>>>
}

impl<U: Eq + fmt::Debug> SequenceFragment<U> {
    pub fn new(items: Vec<Box<dyn Fragment<U>>>) -> Self {
        SequenceFragment { items }
    }
}

impl<U: Eq + fmt::Debug + fmt::Debug> Fragment<U> for SequenceFragment<U> {
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