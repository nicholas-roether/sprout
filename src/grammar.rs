use std::fmt::Debug;

use crate::fragment::Fragment;

#[derive(Debug)]
pub struct GrammarItem<N: Eq + Debug, TN: Eq + Debug> {
	name: N,
	fragment: Box<dyn Fragment<TN>>
}

impl<N: Eq + Debug, TN: Eq + Debug> GrammarItem<N, TN> {
	pub fn new(name: N, fragment: Box<dyn Fragment<TN>>) -> Self {
		return GrammarItem { name, fragment }
	}
}

#[derive(Debug)]
struct Grammar<N: Eq + Debug, TN: Eq + Debug> {
	items: Vec<GrammarItem<N, TN>>
}