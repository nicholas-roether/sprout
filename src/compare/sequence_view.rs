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
		if self.is_empty() {
			return &self.items[self.items.len()..];
		}
		&self.items[self.index ..]
	}

	pub fn is_empty(&self) -> bool {
		self.index == self.items.len()
	}
}

impl<'a, T> Clone for SequenceView<'a, T> {
	fn clone(&self) -> Self {
		SequenceView { index: self.index, items: self.items }
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn should_construct_correctly() {
		let items = &[1, 2, 3];
		let seq_view = SequenceView::new(items);

		assert_eq!(seq_view.index, 0);
	}

	fn items_function_should_return_correclty() {
		let items = &[1, 2, 3];
		let seq_view = SequenceView::new(items);

		assert_eq!(seq_view.items(), &[1, 2, 3]);
	}

	fn items_function_should_account_for_index() {
		let items = &[1, 2, 3];
		let mut seq_view = SequenceView::new(items);
		seq_view.index = 2;

		assert_eq!(seq_view.items(), &[3]);
	}
}