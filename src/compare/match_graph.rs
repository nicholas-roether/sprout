use std::fmt;

use petgraph::{graph::{DiGraph, NodeIndex, EdgeReference}, visit::EdgeRef};

use super::SequenceView;

#[derive(Debug, PartialEq, Eq)]
pub struct MatchError {
	pub expectations: Vec<String>,
	pub depth: u32,
	pub index: usize
}

impl MatchError {
	pub fn new(expectations: Vec<String>, depth: u32, index: usize) -> Self {
		MatchError { expectations, depth, index }
	}

	pub fn simple(expectation: String, index: usize) -> Self {
		Self::new(vec![expectation], 0, index)
	}
}

impl fmt::Display for MatchError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if self.expectations.is_empty() {
			return write!(f, "Invalid syntax");
		}
		if self.expectations.len() == 1 {
			return write!(f, "Expected {}", self.expectations.first().unwrap());
		}
		write!(f, "Expected one of: {}", self.expectations.join(", "))
	}
}

pub trait Matcher {
	type Item;
	type Accumulator: Clone;

	fn compare(&self, sequence: &mut SequenceView<Self::Item>, accumulator: &mut Self::Accumulator) -> Result<(), MatchError>;
}

#[derive(Debug, Clone)]
pub struct MatchNodeData<M: Matcher> {
	matcher: Option<M>,
	name: Option<String>
}

impl<M: Matcher> MatchNodeData<M> {
	pub fn new(matcher: Option<M>, name: Option<String>) -> Self {
		MatchNodeData { matcher, name }
	}

	pub fn empty() -> Self {
		MatchNodeData { matcher: None, name: None }
	}
}

#[derive(Debug)]
pub struct MatchGraph<M: Matcher> {
	graph: DiGraph<MatchNodeData<M>, u32>,
	root: NodeIndex
}

impl<M: Matcher> MatchGraph<M> {
	pub fn new(graph: DiGraph<MatchNodeData<M>, u32>, root: NodeIndex) -> Self {
		MatchGraph { graph, root }
	}

	pub fn compare(&self, sequence: &[M::Item], accumulator: &mut M::Accumulator) -> Result<(), MatchError> {
		let mut sequence_view = SequenceView::new(sequence);
		self.compare_node(self.root, &mut sequence_view, accumulator)
	}

	fn compare_node(
		&self,
		index: NodeIndex,
		sequence: &mut SequenceView<M::Item>,
		accumulator: &mut M::Accumulator
	) -> Result<(), MatchError> {
		let node = &self.graph[index];

		if let Some(matcher) = &node.matcher {
			matcher.compare(sequence, accumulator)?;
		}

		let mut errors: Vec<MatchError> = vec![];
		let mut edges: Vec<EdgeReference<u32>> = self.graph.edges(index).collect();
		edges.sort_by(|e1, e2| e1.weight().cmp(e2.weight()));
		for edge in edges {
			let mut seq_clone = sequence.clone();
			let mut acc_clone = accumulator.clone();
			match self.compare_node(edge.target(), &mut seq_clone, &mut acc_clone) {
				Err(error) => {
					errors.push(
						match &node.name {
							Some(name) => MatchError::simple(name.clone(), error.index),
							None => MatchError::new(error.expectations, error.depth + 1, error.index)
						}
					)
				},
				Ok(_) => {
					*sequence = seq_clone;
					*accumulator = acc_clone;
					return Ok(())
				}
			}
		}

		if errors.len() == 0 {
			return Ok(());
		}

		if errors.len() == 1 {
			return Err(errors.into_iter().next().unwrap());
		}

		let mut deepest_expectations: Vec<String> = vec![];
		let mut deepest_error_depth = 0;
		for mut error in errors {
			if error.depth > deepest_error_depth {
				deepest_error_depth = error.depth;
				deepest_expectations.clear();
				deepest_expectations.append(&mut error.expectations);
			} else if error.depth == deepest_error_depth {
				deepest_expectations.append(&mut error.expectations);
			}
		}
		
		Err(MatchError::new(deepest_expectations, deepest_error_depth, sequence.index))
	}
}