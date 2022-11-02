#[derive(Debug)]
pub struct SequenceView<'a, T> {
	pub index: usize,
	items: &'a [T]
}

impl<'a, T> SequenceView<'a, T> {
	pub fn new(items: &'a [T]) -> Self {
		SequenceView { index: 0, items }
	}

	pub fn items(&self) -> &'a [T] {
		&self.items[self.index ..]
	}
}

impl<'a, T> Clone for SequenceView<'a, T> {
	fn clone(&self) -> Self {
		SequenceView { index: self.index, items: self.items }
	}
}