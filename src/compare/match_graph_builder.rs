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