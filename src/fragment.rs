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

	pub fn items(&self) -> &'a [I] {
		&self.items[self.index..]
	}

	pub fn index(&self) -> usize {
		self.index
	}
}


pub trait Fragment<I, A> {
	fn compare(
		&self,
		view: &mut SequenceView<I>,
		acc: &mut A
	) -> Result<(), String>;
}

pub struct RepeatFragment<I, A> {
	item: Box<dyn Fragment<I, A>>,
	min_reps: u32,
	max_reps: Option<u32>
}

impl<I, A> RepeatFragment<I, A> {
	pub fn new(item: Box<dyn Fragment<I, A>>, min_reps: u32, max_reps: Option<u32>) -> Self {
		RepeatFragment { item, min_reps, max_reps }
	}
}

impl<I, A> Fragment<I, A> for RepeatFragment<I, A> {
	fn compare(
		&self,
		view: &mut SequenceView<I>,
		acc: &mut A
	) -> Result<(), String> {
		let mut num_reps: u32 = 0;
		let mut err = String::from("Unexpected parsing error");
		loop {
			if let Some(max) = self.max_reps{
				if num_reps >= max { break; }
			}
			if let Err(message) = self.item.compare(view, acc) {
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

pub struct ChoiceFragment<I, A> {
	choices: Vec<Box<dyn Fragment<I, A>>>
}

impl<I, A> ChoiceFragment<I, A> {
	pub fn new(choices: Vec<Box<dyn Fragment<I, A>>>) -> Self {
		ChoiceFragment { choices }
	}
}

impl<I, A> Fragment<I, A> for ChoiceFragment<I, A> {
	fn compare(
		&self,
		view: &mut SequenceView<I>,
		acc: &mut A
	) -> Result<(), String> {
		let mut first_err: Option<String> = None;
		for choice in &self.choices {
			if let Err(message) = choice.compare(view, acc) {
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

pub struct SequenceFragment<I, A> {
	items: Vec<Box<dyn Fragment<I, A>>>
}

impl<I, A> SequenceFragment<I, A> {
	pub fn new(items: Vec<Box<dyn Fragment<I, A>>>) -> Self {
		SequenceFragment { items }
	}
}

impl<I, A> Fragment<I, A> for SequenceFragment<I, A> {
	fn compare(
		&self,
		view: &mut SequenceView<I>,
		acc: &mut A
	) -> Result<(), String> {
		for item in &self.items {
			item.compare(view, acc)?;
		}
		Ok(())
	}
}

#[cfg(test)]
mod tests {
    use super::*;

	struct TestCharFragment {
		char: char
	}

	impl TestCharFragment {
		fn new(char: char) -> Self {
			TestCharFragment { char }
		}
	}

	impl Fragment<char, String> for TestCharFragment {
		fn compare(
			&self,
			view: &mut SequenceView<char>,
			acc: &mut String
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
        let fragment = RepeatFragment::new(
            Box::new(TestCharFragment::new('x')),
            0,
            None
        );

		let mut strbuf: Vec<char> = vec![];

        let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from(""));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("fhg", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from(""));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("xfhg", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("x"));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("xxxxxxxxzut", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("xxxxxxxx"));
    }

    #[test]
    fn repeat_fragment_min_reps_works() {
        let fragment = RepeatFragment::new(
            Box::new(TestCharFragment::new('x')),
            2,
            None
        );

		let mut strbuf: Vec<char> = vec![];

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("", &mut strbuf), &mut str), Err(String::from("Expected 'x'; got nothing.")));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("x", &mut strbuf), &mut str), Err(String::from("Expected 'x'; got nothing.")));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("xx", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("xx"));
    }

    #[test]
    fn repeat_fragment_max_reps_works() {
        let fragment = RepeatFragment::new(
            Box::new(TestCharFragment::new('x')),
            0,
            Some(4)
        );

		let mut strbuf: Vec<char> = vec![];

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("xxxx", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("xxxx"));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("xxxxx", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("xxxx"));
    }

    #[test]
    fn choice_fragment_works() {
        let fragment = ChoiceFragment::new(vec![
            Box::new(TestCharFragment::new('a')),
            Box::new(TestCharFragment::new('b'))
        ]);

		let mut strbuf: Vec<char> = vec![];

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("a", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("a"));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("b", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("b"));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("c", &mut strbuf), &mut str), Err(String::from("Expected 'a'; got 'c'.")));
    }

    #[test]
    fn sequence_fragment_works() {
        let fragment = SequenceFragment::new(vec![
            Box::new(TestCharFragment::new('a')),
            Box::new(TestCharFragment::new('b'))
        ]);

		let mut strbuf: Vec<char> = vec![];

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("abgfgdh", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("ab"));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("abbgfgdh", &mut strbuf), &mut str), Ok(()));
		assert_eq!(str, String::from("ab"));

		let mut str = String::new();
        assert_eq!(fragment.compare(&mut seq_view("agfgdh", &mut strbuf), &mut str), Err(String::from("Expected 'b'; got 'g'.")));
    }
}