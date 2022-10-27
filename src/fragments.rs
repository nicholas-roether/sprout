use std::fmt::Debug;

pub struct SequenceView<'a, I> {
	index: usize,
	items: &'a [I]
}

impl<'a, I> SequenceView<'a, I> {
	pub fn new(items: &'a [I]) -> Self {
		SequenceView { items, index: 0 }
	}

	pub fn advance(&mut self, steps: usize) {
		self.items = &self.items[steps..];
	}

	pub fn set_index(&mut self, index: usize) {
		self.index = index;
	}

	pub fn items(&self) -> &'a [I] {
		&self.items[self.index..]
	}

	pub fn index(&self) -> usize {
		self.index
	}
}

impl<'a, I> Clone for SequenceView<'a, I> {
	fn clone(&self) -> Self {
		SequenceView { index: self.index, items: self.items }
	}
}


pub trait Fragment<I, A, C> where Self: Debug {
	fn compare(
		&self,
		view: &mut SequenceView<I>,
		acc: &mut A,
		context: &C
	) -> Result<(), String>;
}

#[derive(Debug)]
pub struct RepeatFragment<I, A, C> {
	item: Box<dyn Fragment<I, A, C>>,
	min_reps: u32,
	max_reps: Option<u32>
}

impl<I, A, C> RepeatFragment<I, A, C> {
	pub fn new(item: Box<dyn Fragment<I, A, C>>, min_reps: u32, max_reps: Option<u32>) -> Self {
		RepeatFragment { item, min_reps, max_reps }
	}
}

impl<I: Debug, A: Debug, C: Debug> Fragment<I, A, C> for RepeatFragment<I, A, C> {
	fn compare(
		&self,
		view: &mut SequenceView<I>,
		acc: &mut A,
		context: &C
	) -> Result<(), String> {
		let mut num_reps: u32 = 0;
		let mut err = String::from("Unexpected parsing error");
		loop {
			if let Some(max) = self.max_reps{
				if num_reps >= max { break; }
			}
			if let Err(message) = self.item.compare(view, acc, context) {
				err = message;
				break;
			}
			num_reps += 1;
		}
		if num_reps < self.min_reps {
			return Err(err);
		}
		Ok(())
	}
}

#[derive(Debug)]
pub struct ChoiceFragment<I, A, C> {
	choices: Vec<Box<dyn Fragment<I, A, C>>>
}

impl<I, A, C> ChoiceFragment<I, A, C> {
	pub fn new(choices: Vec<Box<dyn Fragment<I, A, C>>>) -> Self {
		ChoiceFragment { choices }
	}
}

impl<I: Debug, A: Debug, C: Debug> Fragment<I, A, C> for ChoiceFragment<I, A, C> {
	fn compare(
		&self,
		view: &mut SequenceView<I>,
		acc: &mut A,
		context: &C
	) -> Result<(), String> {
		let mut first_err: Option<String> = None;
		for choice in &self.choices {
			if let Err(message) = choice.compare(view, acc, context) {
				if first_err.is_none() {
					first_err = Some(message);
				}
			} else {
				return Ok(())
			}
		}
		return Err(first_err.unwrap_or(String::from("Unexpected parsing error")))
	}
}

#[derive(Debug)]
pub struct SequenceFragment<I, A, C> {
	items: Vec<Box<dyn Fragment<I, A, C>>>
}

impl<I, A, C> SequenceFragment<I, A, C> {
	pub fn new(items: Vec<Box<dyn Fragment<I, A, C>>>) -> Self {
		SequenceFragment { items }
	}
}

impl<I: Debug, A: Debug + Clone, C: Debug> Fragment<I, A, C> for SequenceFragment<I, A, C> {
	fn compare(
		&self,
		view: &mut SequenceView<I>,
		acc: &mut A,
		context: &C
	) -> Result<(), String> {
		let mut seq_view_clone = view.clone();
		let mut acc_clone = acc.clone();

		for item in &self.items {
			item.compare(&mut seq_view_clone, &mut acc_clone, context)?;
		}

		*view = seq_view_clone;
		*acc = acc_clone;
		Ok(())
	}
}

#[cfg(test)]
mod tests {
    use crate::{repeat, choice};

    use super::*;

	#[derive(Debug)]
	struct TestCharFragment {
		char: char
	}

	impl TestCharFragment {
		fn new(char: char) -> Self {
			TestCharFragment { char }
		}
	}

	impl Fragment<char, String, ()> for TestCharFragment {
		fn compare(
			&self,
			view: &mut SequenceView<char>,
			acc: &mut String,
			_context: &()
		) -> Result<(), String> {
			if let Some(char) = view.items().first() {
				if *char == self.char {
					view.advance(1);
					acc.push(*char);
					return Ok(())
				}
				return Err(format!("Expected '{}'; got '{char}'.", self.char))
			}
			Err(format!("Expected '{}'; got nothing.", self.char))
		}
	}

    fn seq_view<'a>(str: &str, buffer: &'a mut Vec<char>) -> SequenceView<'a, char> {
		*buffer = str.chars().collect();
        SequenceView::new(buffer.as_slice())
    }

    #[test]
    fn repeat_fragment_works() {
        let fragment = repeat!(TestCharFragment::new('x'));

		let mut strbuf: Vec<char> = vec![];

        let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("", &mut strbuf), &mut str, &()), Ok(()));
		assert_eq!(str, String::from(""));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("fhg", &mut strbuf), &mut str, &()), Ok(()));
		assert_eq!(str, String::from(""));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("xfhg", &mut strbuf), &mut str, &()), Ok(()));
		assert_eq!(str, String::from("x"));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("xxxxxxxxzut", &mut strbuf), &mut str, &()), Ok(()));
		assert_eq!(str, String::from("xxxxxxxx"));
    }

    #[test]
    fn repeat_fragment_min_reps_works() {
        let fragment = repeat!(TestCharFragment::new('x'); 2);

		let mut strbuf: Vec<char> = vec![];

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("", &mut strbuf), &mut str, &()), Err(String::from("Expected 'x'; got nothing.")));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("x", &mut strbuf), &mut str, &()), Err(String::from("Expected 'x'; got nothing.")));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("xx", &mut strbuf), &mut str, &()), Ok(()));
		assert_eq!(str, String::from("xx"));
    }

    #[test]
    fn repeat_fragment_max_reps_works() {
        let fragment = repeat!(TestCharFragment::new('x'); 0; 4);

		let mut strbuf: Vec<char> = vec![];

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("xxxx", &mut strbuf), &mut str, &()), Ok(()));
		assert_eq!(str, String::from("xxxx"));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("xxxxx", &mut strbuf), &mut str, &()), Ok(()));
		assert_eq!(str, String::from("xxxx"));
    }

    #[test]
    fn choice_fragment_works() {
        let fragment = choice!(TestCharFragment::new('a'); TestCharFragment::new('b'));

		let mut strbuf: Vec<char> = vec![];

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("a", &mut strbuf), &mut str, &()), Ok(()));
		assert_eq!(str, String::from("a"));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("b", &mut strbuf), &mut str, &()), Ok(()));
		assert_eq!(str, String::from("b"));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("c", &mut strbuf), &mut str, &()), Err(String::from("Expected 'a'; got 'c'.")));
    }

    #[test]
    fn sequence_fragment_works() {
        let fragment = SequenceFragment::new(vec![
            Box::new(TestCharFragment::new('a')),
            Box::new(TestCharFragment::new('b'))
        ]);

		let mut strbuf: Vec<char> = vec![];

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("abgfgdh", &mut strbuf), &mut str, &()), Ok(()));
		assert_eq!(str, String::from("ab"));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("abbgfgdh", &mut strbuf), &mut str, &()), Ok(()));
		assert_eq!(str, String::from("ab"));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("agfgdh", &mut strbuf), &mut str, &()), Err(String::from("Expected 'b'; got 'g'.")));
    }
}