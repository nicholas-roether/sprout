use std::fmt;

use colored::Colorize;
use petgraph::{graph::{DiGraph, NodeIndex, EdgeReference}, visit::EdgeRef};

use super::SequenceView;

#[derive(Debug, PartialEq, Eq)]
pub struct MatchError {
	pub expectations: Vec<String>,
	pub index: usize,
	pub signature: bool
}

impl MatchError {
	pub fn new(expectations: Vec<String>, index: usize, signature: bool) -> Self {
		MatchError { expectations, index, signature }
	}

	pub fn simple(expectation: String, index: usize) -> Self {
		Self::new(vec![expectation], index, false)
	}

	pub fn signature(expectation: String, index: usize) -> Self {
		Self::new(vec![expectation], index, true)
	}
}

impl fmt::Display for MatchError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		if self.expectations.is_empty() {
			return write!(f, "Invalid syntax");
		}
		if self.expectations.len() == 1 {
			return write!(f, "Expected {}", self.expectations.first().unwrap().bold());
		}
		write!(f, "Expected one of: ")?;
		for i in 0 .. self.expectations.len() {
			if i != 0 { write!(f, ", ")?; }
			write!(f, "{}", self.expectations[i].bold())?;
		}
		Ok(())
	}
}

#[derive(Debug)]
pub struct MatcherContext<'a, D> {
	pub data: &'a D,
	exhaustive: bool
}

impl<'a, D> MatcherContext<'a, D> {
	pub fn new(data: &'a D, exhaustive: bool) -> Self {
		MatcherContext { data, exhaustive }
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum MatchResult {
	Match,
	Signature
}

pub trait Matcher {
	type Item;
	type Accumulator: Clone;
	type ContextData<'a> where Self: 'a;

	fn compare<'a>(&self, sequence: &mut SequenceView<Self::Item>, accumulator: &mut Self::Accumulator, context: &MatcherContext<Self::ContextData<'a>>) -> Result<MatchResult, MatchError>;
}

#[derive(Debug)]
pub struct MatchGraph<M: Matcher> {
	graph: DiGraph<Option<M>, u32>,
	root: NodeIndex
}

impl<M: Matcher> MatchGraph<M> {
	pub fn new(graph: DiGraph<Option<M>, u32>, root: NodeIndex) -> Self {
		MatchGraph { graph, root }
	}

	pub fn compare<'a>(&self, sequence_view: &mut SequenceView<M::Item>, accumulator: &mut M::Accumulator, context: &MatcherContext<M::ContextData<'a>>) -> Result<(), MatchError> {
		self.compare_node(self.root, sequence_view, accumulator, context)
	}

	fn compare_node<'a>(
		&self,
		index: NodeIndex,
		sequence: &mut SequenceView<M::Item>,
		accumulator: &mut M::Accumulator,
		context: &MatcherContext<M::ContextData<'a>>
	) -> Result<(), MatchError> {
		let node = &self.graph[index];
		let mut signature = false;

		if let Some(matcher) = &node {
			let result = matcher.compare(sequence, accumulator, context)?;
			if result == MatchResult::Signature {
				signature = true;
			}
		}

		let mut errors: Vec<MatchError> = vec![];
		let mut edges: Vec<EdgeReference<u32>> = self.graph.edges(index).collect();
		edges.sort_by(|e1, e2| e1.weight().cmp(e2.weight()));
		let mut success = true;

		for edge in edges {
			let mut seq_clone = sequence.clone();
			let mut acc_clone = accumulator.clone();
			match self.compare_node(edge.target(), &mut seq_clone, &mut acc_clone, context) {
				Err(error) => {
					if error.signature {
						return Err(error);
					}
					errors.push(MatchError::new(error.expectations, error.index, signature));
					success = false;
				},
				Ok(_) => {
					*sequence = seq_clone;
					*accumulator = acc_clone;
					success = true;
					break;
				}
			}
		}

		if success {
			if context.exhaustive && !sequence.is_empty() {
				return Err(MatchError::new(vec!["end of input".to_string()], sequence.index, signature))
			} 
			return Ok(());
		}

		if errors.len() == 1 {
			return Err(errors.into_iter().next().unwrap());
		}

		let mut deepest_expectations: Vec<String> = vec![];
		let mut deepest_error_index = 0;
		for mut error in errors {
			if error.index > deepest_error_index {
				deepest_error_index = error.index;
				deepest_expectations.clear();
				deepest_expectations.append(&mut error.expectations);
			} else if error.index == deepest_error_index {
				deepest_expectations.append(&mut error.expectations);
			}
		}
		
		Err(MatchError::new(deepest_expectations, deepest_error_index, signature))
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	impl Matcher for char {
		type Item = char;
		type Accumulator = String;
		type ContextData<'a> = ();

		fn compare(&self, sequence: &mut SequenceView<Self::Item>, accumulator: &mut Self::Accumulator, _context: &MatcherContext<()>) -> Result<MatchResult, MatchError> {
			if sequence.items().is_empty() || sequence.items().first().unwrap() != self {
				return Err(MatchError::simple(self.to_string(), sequence.index))
			}
			sequence.index += 1;
			accumulator.push(*self);
			Ok(if *self == '@' { MatchResult::Signature } else { MatchResult::Match })
		}
	}

	#[test]
	fn match_error_should_construct_correctly() {
		let err = MatchError::new(vec!["abc".to_string()], 420, false);
		assert_eq!(err.expectations, vec!["abc".to_string()]);
		assert_eq!(err.index, 420);

		let err = MatchError::simple("xyz".to_string(), 123);
		assert_eq!(err.expectations, vec!["xyz".to_string()]);
		assert_eq!(err.index, 123);
	}

	#[test]
	fn match_error_with_single_expectation_should_display_correctly() {
		let err = MatchError::simple("something cool".to_string(), 12);
		assert_eq!(format!("{err}"), format!("Expected {}", "something cool".bold()));
	}

	#[test]
	fn match_error_with_multiple_expectations_should_display_correctly() {
		let err = MatchError::new(vec!["123".to_string(), "456".to_string(), "789".to_string()], 0, false);
		assert_eq!(format!("{err}"), format!("Expected one of: {}, {}, {}", "123".bold(), "456".bold(), "789".bold()));
	}

	#[test]
	fn match_graph_with_single_matcher_node_should_work() {
		let mut di_graph: DiGraph<Option<char>, u32> = DiGraph::new();
		let node = di_graph.add_node(Some('x'));
		let graph = MatchGraph::new(di_graph, node);

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['x', 'a', 'b']), &mut acc, &MatcherContext::new(&(), false)), Ok(()));
		assert_eq!(acc, "x".to_string());

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['a', 'b', 'c']), &mut acc, &MatcherContext::new(&(), false)), Err(MatchError::new(vec!["x".to_string()], 0, false)));
	}

	#[test]
	fn match_graph_with_single_matcher_node_should_work_in_exhaustive_mode() {
		let mut di_graph: DiGraph<Option<char>, u32> = DiGraph::new();
		let node = di_graph.add_node(Some('x'));
		let graph = MatchGraph::new(di_graph, node);

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['x']), &mut acc, &MatcherContext::new(&(), true)), Ok(()));
		assert_eq!(acc, "x".to_string());

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['x', 'b', 'c']), &mut acc, &MatcherContext::new(&(), true)), Err(MatchError::new(vec!["end of input".to_string()], 1, false)));
	}

	#[test]
	fn match_graph_should_ignore_empty_matcher_nodes() {
		let mut di_graph: DiGraph<Option<char>, u32> = DiGraph::new();
		let root = di_graph.add_node(None);
		let node = di_graph.add_node(Some('x'));
		di_graph.add_edge(root, node, 2);
		let graph = MatchGraph::new(di_graph, root);

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['x', 'a', 'b']), &mut acc, &MatcherContext::new(&(), false)), Ok(()));
		assert_eq!(acc, "x".to_string());

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['a', 'b', 'c']), &mut acc, &MatcherContext::new(&(), false)), Err(MatchError::new(vec!["x".to_string()], 0, false)));
	}

	#[test]
	fn match_graph_should_handle_splits() {
		let mut di_graph: DiGraph<Option<char>, u32> = DiGraph::new();
		let root = di_graph.add_node(None);
		let opt1_node = di_graph.add_node(Some('x'));
		let opt2_node = di_graph.add_node(Some('y'));
		di_graph.add_edge(root, opt1_node, 0);
		di_graph.add_edge(root, opt2_node, 1);
		let graph = MatchGraph::new(di_graph, root);

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['x', 'a', 'b']), &mut acc, &MatcherContext::new(&(), false)), Ok(()));
		assert_eq!(acc, "x".to_string());

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['y', 'a', 'b']), &mut acc, &MatcherContext::new(&(), false)), Ok(()));
		assert_eq!(acc, "y".to_string());

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['z', 'b', 'c']), &mut acc, &MatcherContext::new(&(), false)), Err(MatchError::new(vec!["x".to_string(), "y".to_string()], 0, false)));
	}

	#[test]
	fn match_graph_should_handle_splits_in_exhaustive_mode() {
		let mut di_graph: DiGraph<Option<char>, u32> = DiGraph::new();
		let root = di_graph.add_node(None);
		let opt1_node = di_graph.add_node(Some('x'));
		let opt2_node = di_graph.add_node(Some('y'));
		di_graph.add_edge(root, opt1_node, 0);
		di_graph.add_edge(root, opt2_node, 1);
		let graph = MatchGraph::new(di_graph, root);

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['x']), &mut acc, &MatcherContext::new(&(), true)), Ok(()));
		assert_eq!(acc, "x".to_string());

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['y']), &mut acc, &MatcherContext::new(&(), true)), Ok(()));
		assert_eq!(acc, "y".to_string());

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['x', 'b', 'c']), &mut acc, &MatcherContext::new(&(), true)), Err(MatchError::new(vec!["end of input".to_string()], 1, false)));
	}

	#[test]
	fn match_graph_should_handle_splits_with_partial_match() {
		let mut di_graph: DiGraph<Option<char>, u32> = DiGraph::new();
		let root = di_graph.add_node(None);
		let opt1_node1 = di_graph.add_node(Some('a'));
		let opt1_node2 = di_graph.add_node(Some('x'));
		let opt2_node1 = di_graph.add_node(Some('a'));
		let opt2_node2 = di_graph.add_node(Some('y'));
		di_graph.add_edge(root, opt1_node1, 0);
		di_graph.add_edge(opt1_node1, opt1_node2, 0);
		di_graph.add_edge(root, opt2_node1, 1);
		di_graph.add_edge(opt2_node1, opt2_node2, 0);
		let graph = MatchGraph::new(di_graph, root);

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['a', 'x', 'b']), &mut acc, &MatcherContext::new(&(), false)), Ok(()));
		assert_eq!(acc, "ax".to_string());

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['a', 'y', 'b']), &mut acc, &MatcherContext::new(&(), false)), Ok(()));
		assert_eq!(acc, "ay".to_string());

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['a', 'z', 'c']), &mut acc, &MatcherContext::new(&(), false)), Err(MatchError::new(vec!["x".to_string(), "y".to_string()], 1, false)));
	}

	#[test]
	fn match_graph_should_handle_splits_with_signature_match() {
		let mut di_graph: DiGraph<Option<char>, u32> = DiGraph::new();
		let root = di_graph.add_node(None);
		let opt1_node1 = di_graph.add_node(Some('@'));
		let opt1_node2 = di_graph.add_node(Some('x'));
		let opt2_node1 = di_graph.add_node(Some('@'));
		let opt2_node2 = di_graph.add_node(Some('y'));
		di_graph.add_edge(root, opt1_node1, 0);
		di_graph.add_edge(opt1_node1, opt1_node2, 0);
		di_graph.add_edge(root, opt2_node1, 1);
		di_graph.add_edge(opt2_node1, opt2_node2, 0);
		let graph = MatchGraph::new(di_graph, root);

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['@', 'x', 'b']), &mut acc, &MatcherContext::new(&(), false)), Ok(()));
		assert_eq!(acc, "@x".to_string());

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['@', 'y', 'c']), &mut acc, &MatcherContext::new(&(), false)), Err(MatchError::new(vec!["x".to_string()], 1, true)));
	}

	#[test]
	fn match_graph_should_handle_splits_with_partial_matches_of_different_depths() {
		let mut di_graph: DiGraph<Option<char>, u32> = DiGraph::new();
		let root = di_graph.add_node(None);
		let opt1_node1 = di_graph.add_node(Some('a'));
		let opt1_node2 = di_graph.add_node(Some('x'));
		let opt1_node3 = di_graph.add_node(Some('x'));
		let opt2_node1 = di_graph.add_node(Some('a'));
		let opt2_node2 = di_graph.add_node(Some('y'));
		let opt2_node3 = di_graph.add_node(Some('x'));
		di_graph.add_edge(root, opt1_node1, 0);
		di_graph.add_edge(opt1_node1, opt1_node2, 0);
		di_graph.add_edge(opt1_node2, opt1_node3, 0);
		di_graph.add_edge(root, opt2_node1, 1);
		di_graph.add_edge(opt2_node1, opt2_node2, 0);
		di_graph.add_edge(opt2_node2, opt2_node3, 0);
		let graph = MatchGraph::new(di_graph, root);

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['a', 'x', 'x']), &mut acc, &MatcherContext::new(&(), false)), Ok(()));
		assert_eq!(acc, "axx".to_string());

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['a', 'y', 'x']), &mut acc, &MatcherContext::new(&(), false)), Ok(()));
		assert_eq!(acc, "ayx".to_string());

		let mut acc = String::new();
		assert_eq!(graph.compare(&mut SequenceView::new(&['a', 'x', 'c']), &mut acc, &MatcherContext::new(&(), false)), Err(MatchError::new(vec!["x".to_string()], 2, false)));
	}
}