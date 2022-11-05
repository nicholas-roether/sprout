use petgraph::graph::{DiGraph, NodeIndex};

use super::{Matcher, MatchNodeData, MatchGraph};


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

#[derive(Debug)]
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
		self.pop_return();
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

#[cfg(test)]
mod tests {
    use crate::compare::{Matcher, SequenceView, MatchError, MatcherContext};

    use super::*;

	impl Matcher for u32 {
	    type Item = u32;
	    type Accumulator = Vec<u32>;
		type ContextData<'a> = ();

	    fn compare(&self, sequence: &mut SequenceView<u32>, accumulator: &mut Vec<u32>, _context: &MatcherContext<()>) -> Result<(), MatchError> {
	        if sequence.items().is_empty() || sequence.items().first().unwrap() != self {
				return Err(MatchError::simple(self.to_string(), sequence.index));
			}
			sequence.index += 1;
			accumulator.push(*self);
			Ok(())
	    }
	}

	#[test]
	fn append_works() {
		let mut builder = MatchGraphBuilder::<u32>::new();
		builder.append(6, None);
		builder.append(9, None);
		let graph = builder.complete();

		let mut acc: Vec<u32> = vec![];
		assert!(graph.compare(&mut SequenceView::new(&[6, 9, 0]), &mut acc, &MatcherContext::new(&(), false)).is_ok());
		assert_eq!(acc, vec![6, 9]);
		
		assert!(graph.compare(&mut SequenceView::new(&[6]), &mut vec![], &MatcherContext::new(&(), false)).is_err());
		assert!(graph.compare(&mut SequenceView::new(&[1, 2, 3]), &mut vec![], &MatcherContext::new(&(), false)).is_err());
	}

	#[test]
	fn push_get_and_pop_return_works() {
		let mut builder = MatchGraphBuilder::<u32>::new();
		builder.append(420, None);
		builder.push_return();
		builder.append(6, None);
		builder.append(6, None);
		builder.push_return();

		assert_eq!(builder.get_return(), NodeIndex::new(3));
		assert_eq!(builder.pop_return(), NodeIndex::new(3));
		assert_eq!(builder.get_return(), NodeIndex::new(1));
		assert_eq!(builder.pop_return(), NodeIndex::new(1));
	}

	#[test]
	fn choice_construction_works() {
		let mut builder = MatchGraphBuilder::<u32>::new();
		builder.push_return();
		builder.append(420, None);
		builder.end_choice_path();
		builder.append(69, None);
		builder.end_choice_path();
		builder.pop_choice();
		let graph = builder.complete();

		let mut acc: Vec<u32> = vec![];
		assert!(graph.compare(&mut SequenceView::new(&[420, 0]), &mut acc, &MatcherContext::new(&(), false)).is_ok());
		assert_eq!(acc, vec![420]);

		let mut acc: Vec<u32> = vec![];
		assert!(graph.compare(&mut SequenceView::new(&[69, 0]), &mut acc, &MatcherContext::new(&(), false)).is_ok());
		assert_eq!(acc, vec![69]);

		assert!(graph.compare(&mut SequenceView::new(&[1, 2, 3]), &mut vec![], &MatcherContext::new(&(), false)).is_err());
	}

	#[test]
	fn repeat_construction_works() {
		let mut builder = MatchGraphBuilder::<u32>::new();
		builder.push_return();
		builder.append(420, None);
		builder.pop_repeat();
		let graph = builder.complete();

		let mut acc: Vec<u32> = vec![];
		assert!(graph.compare(&mut SequenceView::new(&[420, 0]), &mut acc, &MatcherContext::new(&(), false)).is_ok());
		assert_eq!(acc, vec![420]);

		let mut acc: Vec<u32> = vec![];
		assert!(graph.compare(&mut SequenceView::new(&[420, 420, 420, 0]), &mut acc, &MatcherContext::new(&(), false)).is_ok());
		assert_eq!(acc, vec![420, 420, 420]);

		let mut acc: Vec<u32> = vec![];
		assert!(graph.compare(&mut SequenceView::new(&[1, 2, 3]), &mut acc, &MatcherContext::new(&(), false)).is_ok());
		assert_eq!(acc, vec![]);
	}

	#[test]
	fn optional_construction_works() {
		let mut builder = MatchGraphBuilder::<u32>::new();
		builder.push_return();
		builder.append(420, None);
		builder.pop_optional();
		let graph = builder.complete();

		let mut acc: Vec<u32> = vec![];
		assert!(graph.compare(&mut SequenceView::new(&[420, 0]), &mut acc, &MatcherContext::new(&(), false)).is_ok());
		assert_eq!(acc, vec![420]);

		let mut acc: Vec<u32> = vec![];
		assert!(graph.compare(&mut SequenceView::new(&[420, 420, 420, 0]), &mut acc, &MatcherContext::new(&(), false)).is_ok());
		assert_eq!(acc, vec![420]);


		let mut acc: Vec<u32> = vec![];
		assert!(graph.compare(&mut SequenceView::new(&[1, 2, 3]), &mut acc, &MatcherContext::new(&(), false)).is_ok());
		assert_eq!(acc, vec![]);
	}

	#[test]
	fn duplicate_works() {
		let mut builder = MatchGraphBuilder::<u32>::new();
		builder.push_return();
		builder.append(420, None);
		builder.duplicate(2);
		builder.pop_return();
		let graph = builder.complete();

		assert!(graph.compare(&mut SequenceView::new(&[420, 420, 0]), &mut vec![], &MatcherContext::new(&(), false)).is_err());

		let mut acc: Vec<u32> = vec![];
		assert!(graph.compare(&mut SequenceView::new(&[420, 420, 420, 0]), &mut acc, &MatcherContext::new(&(), false)).is_ok());
		assert_eq!(acc, vec![420, 420, 420]);

		let mut acc: Vec<u32> = vec![];
		assert!(graph.compare(&mut SequenceView::new(&[420, 420, 420, 420]), &mut acc, &MatcherContext::new(&(), false)).is_ok());
		assert_eq!(acc, vec![420, 420, 420]);		
	}
}