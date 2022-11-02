use std::fmt;

use petgraph::{graph::{DiGraph, NodeIndex, EdgeReference}, visit::EdgeRef};

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

#[derive(Debug, PartialEq, Eq)]
enum MatchGraphBuilderState {
	Choice(NodeIndex, Vec<NodeIndex>),
	Repeat(NodeIndex),
	Optional(NodeIndex)
}

fn clone_graph_segment<N: Clone, E: Clone>(graph: &mut DiGraph<N, E>, start: NodeIndex) -> (NodeIndex, NodeIndex) {
	let clone = graph.add_node(graph[start].clone());
	let mut end: NodeIndex = clone;
	let mut neighbors = graph.neighbors(start).detach();

	while let Some((edge_index, neighbor_index)) = neighbors.next(graph) {
		let (clone_neighbor, neighbor_end) = clone_graph_segment(graph, neighbor_index);
		graph.add_edge(clone, clone_neighbor, graph[edge_index].clone());
		end = neighbor_end;
	}

	(clone, end)
}

fn move_all_connections<N, E: Clone>(graph: &mut DiGraph<N, E>, from: NodeIndex, to: NodeIndex) {
	let mut neigbors = graph.neighbors(from).detach();
	while let Some((edge_index, node_index)) = neigbors.next(graph) {
		graph.add_edge(to, node_index, graph[edge_index].clone());
		graph.remove_edge(edge_index);
	}
}

pub struct MatchGraphBuilder<M: Matcher> {
	graph: DiGraph<MatchNodeData<M>, u32>,
	root: NodeIndex,
	return_stack: Vec<NodeIndex>,
	end_buffer: Vec<NodeIndex>,
	current_node: NodeIndex
}

impl<M: Matcher> MatchGraphBuilder<M> {
	pub fn new() -> Self {
		let mut graph = DiGraph::new();
		let root = graph.add_node(MatchNodeData::empty());
		MatchGraphBuilder { graph, root, return_stack: vec![], end_buffer: vec![], current_node: root }
	}

	pub fn append(&mut self, matcher: M, name: Option<String>) {
		let appended_node = self.graph.add_node(MatchNodeData::new(Some(matcher), name));
		self.connect(self.current_node, appended_node);
		self.current_node = appended_node;
	}

	pub fn push_return(&mut self) {
		self.return_stack.push(self.current_node);
	}

	pub fn end_choice_path(&mut self) {
		self.end_buffer.push(self.current_node);
		self.current_node = self.get_return();
	}

	pub fn pop_choice(&mut self) {
		let rejoin_node = self.add_buffer_node();
		for end in self.end_buffer.clone() {
			self.connect(end, rejoin_node);
		}
		self.end_buffer.clear();
		self.current_node = rejoin_node;
	}

	pub fn pop_repeat(&mut self) {
		let ret = self.pop_return();
		let loop_start = self.add_buffer_node();
		let next = self.add_buffer_node();
		move_all_connections(&mut self.graph, ret, loop_start);
		self.connect(ret, loop_start);
		self.connect(self.current_node, loop_start);
		self.connect(loop_start, next);
		self.current_node = next;
	}


	pub fn pop_optional(&mut self) {
		let ret = self.pop_return();
		let next = self.add_buffer_node();
		self.connect(ret, next);
		self.connect(self.current_node, next);
		self.current_node = next;
	}

	pub fn get_return(&self) -> NodeIndex {
		*self.return_stack.last().unwrap_or(&self.root)
	}

	pub fn pop_return(&mut self) -> NodeIndex {
		self.return_stack.pop().unwrap_or(self.root)
	}

	pub fn complete(self) -> MatchGraph<M> {
		if !self.return_stack.is_empty() {
			panic!("Match graph return stack was not empty!");
		}
		MatchGraph::new(self.graph, self.root)
	}

	fn connect(&mut self, start: NodeIndex, end: NodeIndex) {
		self.graph.add_edge(
			start,
			end,
			self.graph.edges(start).count() as u32
		);
	}

	fn add_buffer_node(&mut self) -> NodeIndex {
		self.graph.add_node(MatchNodeData::empty())
	}
}

impl<M: Matcher + Clone> MatchGraphBuilder<M> {
	pub fn duplicate(&mut self, times: u32) {
		if times == 0 {
			return;
		}

		let ret = self.pop_return();
		let (clone, _end) = clone_graph_segment(&mut self.graph, ret);
		move_all_connections(&mut self.graph, clone, self.current_node);
		self.graph.remove_node(clone);
		// end is the last node, and so moves to the empty space
		let end = clone;
		self.push_return();
		self.current_node = end;

		self.duplicate(times - 1);
	}
}