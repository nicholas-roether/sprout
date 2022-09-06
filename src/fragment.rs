use std::fmt;

trait Fragment<P: Eq> where Self: fmt::Debug {
    fn compare(&self, seq: &[P]) -> Option<usize>;
}

#[derive(Debug)]
struct ExactFragment<P: Eq + fmt::Debug> {
   exact: P
}

impl<P: Eq + fmt::Debug> ExactFragment<P> {
    fn new(exact: P) -> Self {
        ExactFragment { exact }
    }
}

impl<P: Eq + fmt::Debug> Fragment<P> for ExactFragment<P> {
    fn compare(&self, seq: &[P]) -> Option<usize> {
        if seq.iter().next() == Some(&self.exact) {
            return Some(1)
        }
        None
    }
}

#[derive(Debug)]
struct RepeatFragment<P: Eq + fmt::Debug> {
    item: Box<dyn Fragment<P>>,
    min_reps: u32,
    max_reps: Option<u32>
}

impl<P: Eq + fmt::Debug> RepeatFragment<P> {
    fn new(item: Box<dyn Fragment<P>>, min_reps: u32, max_reps: Option<u32>) -> Self {
        RepeatFragment { item, min_reps, max_reps }
    }
}

impl<P: Eq + fmt::Debug> Fragment<P> for RepeatFragment<P> {
    fn compare(&self, seq: &[P]) -> Option<usize> {
        let mut num_reps: u32 = 0;
        let mut size: usize = 0;
        loop {
            if let Some(max) = self.max_reps {
                if max >= num_reps { break; }
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
struct ChoiceFragment<P: Eq + fmt::Debug> {
    choices: Vec<Box<dyn Fragment<P>>>
}

impl<P: Eq + fmt::Debug> ChoiceFragment<P> {
    fn new(choices: Vec<Box<dyn Fragment<P>>>) -> Self {
        ChoiceFragment { choices }
    }
}

impl<P: Eq + fmt::Debug> Fragment<P> for ChoiceFragment<P> {
    fn compare(&self, seq: &[P]) -> Option<usize> {
        for choice in &self.choices {
            if let Some(len) = choice.compare(seq) {
                return Some(len)
            }
        }
        None
    }
}

#[derive(Debug)]
struct RangeFragment<P: Eq + Ord + fmt::Debug> {
    from: P,
    to: P
}

impl<P: Eq + Ord + fmt::Debug> RangeFragment<P> {
    fn new(from: P, to: P) -> Self {
        RangeFragment { from, to }
    }
}

impl<P: Eq + Ord + fmt::Debug> Fragment<P> for RangeFragment<P> {
    fn compare(&self, seq: &[P]) -> Option<usize> {
        let first = seq.iter().next();
        if matches!(first, None) { return None }
        let first = first.unwrap();
        if !(&self.from <= first && first <= &self.to) { return None }
        Some(1)
    }
}

#[derive(Debug)]
struct SequenceFragment<P: Eq + fmt::Debug> {
    items: Vec<Box<dyn Fragment<P>>>
}

impl<P: Eq + fmt::Debug> SequenceFragment<P> {
    fn new(items: Vec<Box<dyn Fragment<P>>>) -> Self {
        SequenceFragment { items }
    }
}

impl<P: Eq + fmt::Debug> Fragment<P> for SequenceFragment<P> {
    fn compare(&self, seq: &[P]) -> Option<usize> {
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

    #[test]
    fn exact_fragment_works() {
        let fragment = ExactFragment::new('a');

        assert_eq!(fragment.compare((&[char]) &("a".chars())), Some(1));
        assert_eq!(fragment.compare("abc"), Some(1));
        assert_eq!(fragment.compare("fsfd"), None);
        assert_eq!(fragment.compare(""), None);
    }
}