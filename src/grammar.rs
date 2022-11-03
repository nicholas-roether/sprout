use std::fmt;

use trees::{Tree, tr};

use crate::{tokenize::{Token, TokenPosition}, compare::{Matcher, SequenceView, MatchGraph, MatchError, MatcherContext}, ParsingError};

#[derive(Debug)]
pub enum GrammarItemName<PN, TN> {
	Terminal(TN),
	NonTerminal(PN)
}

impl<PN: fmt::Display, TN: fmt::Display> fmt::Display for GrammarItemName<PN, TN> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Terminal(token_name) => write!(f, "{}", token_name),
			Self::NonTerminal(proc_name) => write!(f, "{}", proc_name)
		}
	}
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum GrammarTreeNode<PN, TN> {
	Proc(PN),
	Token(Token<TN>)
}

impl<PN: Copy, TN> GrammarTreeNode<PN, TN> {
	pub fn as_proc(&self) -> Option<PN> {
		if let GrammarTreeNode::Proc(proc_name) = self {
			return Some(*proc_name);
		}
		None
	}

	pub fn as_token(&self) -> Option<&Token<TN>> {
		if let GrammarTreeNode::Token(token) = self {
			return Some(token);
		}
		None
	}
}

#[derive(Debug, Clone)]
pub struct GrammarTreeBuilder<PN: Clone, TN: Clone> {
	tree_stack: Vec<Tree<GrammarTreeNode<PN, TN>>>,
	result: Result<Tree<GrammarTreeNode<PN, TN>>, String>
}

impl<PN: Clone, TN: Clone> GrammarTreeBuilder<PN, TN> {
	fn new() -> Self {
		GrammarTreeBuilder { tree_stack: vec![], result: Err(String::from("Syntax tree is incomplete!")) }
	}

	fn current_proc_mut<'b>(&'b mut self) -> &'b mut Tree<GrammarTreeNode<PN, TN>> {
		self.tree_stack.last_mut().expect("Tried to push token, but no proc was pushed")
	}

	fn push_token(&mut self, token: Token<TN>) {
		self.current_proc_mut().push_back(tr(GrammarTreeNode::Token(token)));
	}

	fn push_proc(&mut self, name: PN) {
		self.tree_stack.push(tr(GrammarTreeNode::Proc(name)));
	}

	fn pop_proc(&mut self) {
		if self.tree_stack.is_empty() { return; }
		let popped_proc = self.tree_stack.pop().unwrap();
		if self.tree_stack.is_empty() {
			self.result = Ok(popped_proc);
		} else {
			self.current_proc_mut().push_back(popped_proc);
		}
	}
}

impl<PN: PartialEq + Copy + fmt::Display, TN: PartialEq + Copy + fmt::Display> Matcher for GrammarItemName<PN, TN> {
    type Item = Token<TN>;
    type Accumulator = GrammarTreeBuilder<PN, TN>;
	type ContextData = Grammar<PN, TN>;

    fn compare(
		&self, sequence: &mut SequenceView<Token<TN>>,
		accumulator: &mut GrammarTreeBuilder<PN, TN>,
		context: &MatcherContext<Grammar<PN, TN>>
	) -> Result<(), crate::compare::MatchError> {
        match self {
			GrammarItemName::Terminal(token_name) => {
				let error = Err(MatchError::simple(format!("{token_name}"), sequence.index));
				if sequence.items().is_empty() {
					return error;
				}
				let next_token = sequence.items().first().unwrap();
				if next_token.name != *token_name {
					return error;
				}
				sequence.index += 1;
				accumulator.push_token(next_token.clone());
				Ok(())
			},
			GrammarItemName::NonTerminal(proc_name) => {
				if sequence.items().is_empty() {
					return Err(MatchError::simple(format!("{proc_name}"), sequence.index));
				}
				context.data.compare_proc(*proc_name, sequence, accumulator)?;
				Ok(())
			}
		}
    }
}

#[derive(Debug)]
pub struct GrammarProc<PN: PartialEq + Copy + fmt::Display, TN: PartialEq + Copy + fmt::Display> {
	name: PN,
	graph: MatchGraph<GrammarItemName<PN, TN>>
}

impl<PN: PartialEq + Copy + fmt::Display, TN: PartialEq + Copy + fmt::Display> GrammarProc<PN, TN> {
	pub fn new(name: PN, graph: MatchGraph<GrammarItemName<PN, TN>>) -> Self {
		GrammarProc { name, graph }
	}
}

#[derive(Debug)]
pub struct Grammar<PN: PartialEq + Copy + fmt::Display, TN: PartialEq + Copy + fmt::Display> {
	procs: Vec<GrammarProc<PN, TN>>
}

impl<PN: PartialEq + Copy + fmt::Display, TN: PartialEq + Copy + fmt::Display> Grammar<PN, TN> {
	pub fn new(procs: Vec<GrammarProc<PN, TN>>) -> Self {
		Grammar { procs }
	}

	pub fn parse(&self, proc: PN, tokens: &[Token<TN>]) -> Result<Tree<GrammarTreeNode<PN, TN>>, ParsingError> {
		let mut tree_builder: GrammarTreeBuilder<PN, TN> = GrammarTreeBuilder::new();
		let mut seq_view = SequenceView::new(tokens);
		match self.compare_proc(proc, &mut seq_view, &mut tree_builder) {
			Ok(_) => Ok(tree_builder.result.expect("Unexpected error occurred during compilation")),
			Err(error) => {
				let position = if error.index == tokens.len() {
					match tokens.last() {
						None => TokenPosition::new(0, 0),
						Some(last_token) => TokenPosition::new(last_token.pos.line, last_token.pos.char + last_token.str.len())
					}
				} else {
					tokens[error.index].pos.clone()
				};
				Err(ParsingError::new(format!("{error}"), position))
			}
		}
		
	}

	fn compare_proc(
		&self,
		proc: PN,
		tokens: &mut SequenceView<Token<TN>>,
		acc: &mut GrammarTreeBuilder<PN, TN>,
	) -> Result<(), MatchError> {
		let mut error: Option<MatchError> = None;
		acc.push_proc(proc);
		for proc in self.procs.iter().filter(|p| p.name == proc) {
			if let Err(new_err) = proc.graph.compare(tokens, acc, &MatcherContext::new(&self, true)) {
				if error.is_none() || new_err.depth > error.as_ref().unwrap().depth {
					error = Some(new_err);
				}
			} else {
				acc.pop_proc();
				return Ok(())
			}
		}
		
		Err(error.expect("Unexpected error occurred during parsing"))
	}
}

#[macro_export]
macro_rules! token {
	($name:expr) => {
		|graph_builder: &mut $crate::compare::MatchGraphBuilder<_>| {
			graph_builder.append(GrammarItemName::Terminal($name), None);
		}
	};
}

#[macro_export]
macro_rules! proc {
	($name:expr) => {
		|graph_builder: &mut $crate::compare::MatchGraphBuilder<_>| {
			graph_builder.append(GrammarItemName::NonTerminal($name), None);
		}
	};
}

#[macro_export]
macro_rules! grammar_proc {
	($proc_name:expr; $($part:expr),+) => {
		{
			let mut graph_builder = $crate::compare::MatchGraphBuilder::new();

			$(
				$part(&mut graph_builder);
			)+

			GrammarProc::new($proc_name, graph_builder.complete())
		}
	}
}

#[macro_export]
macro_rules! grammar {
	($($proc_name:expr => $($part:expr),+);*$(;)?) => {
		Grammar::new(vec![
			$(
				$crate::grammar_proc!($proc_name; $($part),+)
			),+
		])
	};
}

#[cfg(test)]
mod tests {
    use trees::tr;

    use crate::tokenize::{Token, TokenPosition};

    use super::*;

	#[test]
	fn correctly_parses_single_token() {
		let grammar = grammar!(
			'a' => token!('b');
		);

		let result = grammar.parse('a', &[Token::new('b', String::from("123"), TokenPosition::new(6, 9))]);
		assert_eq!(result, Ok(tr(GrammarTreeNode::Proc('a')) / tr(GrammarTreeNode::Token(Token::new('b', String::from("123"), TokenPosition::new(6, 9))))));

		let result2 = grammar.parse('a', &[Token::new('c', String::from("123"), TokenPosition::new(6, 9))]);
		assert_eq!(result2, Err(ParsingError::new("Expected b".to_string(), TokenPosition::new(6, 9))));
	}

	#[test]
	fn correctly_parses_token_sequence() {
		let grammar = grammar!(
			'a' => token!('b'), token!('c');
		);

		let result = grammar.parse('a', &[
			Token::new('b', String::from("123"), TokenPosition::new(6, 9)),
			Token::new('c', String::from("456"), TokenPosition::new(4, 20)),
		]);
		assert_eq!(result, Ok(
			tr(GrammarTreeNode::Proc('a'))
				/ tr(GrammarTreeNode::Token(Token::new('b', String::from("123"), TokenPosition::new(6, 9))))
				/ tr(GrammarTreeNode::Token(Token::new('c', String::from("456"), TokenPosition::new(4, 20))))
		));

		let result2 = grammar.parse('a', &[
			Token::new('b', String::from("123"), TokenPosition::new(6, 9)),
			Token::new('e', String::from("breaks here"), TokenPosition::new(4, 20)),
		]);
		assert_eq!(result2, Err(ParsingError::new("Expected c".to_string(), TokenPosition::new(4, 20))));
	}

	#[test]
	fn correctly_parses_single_proc() {
		let grammar = grammar!(
			'a' => proc!('b');
			'b' => token!('c');
		);

		let result = grammar.parse('a', &[Token::new('c', String::from("123"), TokenPosition::new(6, 9))]);
		assert_eq!(result, Ok(
			tr(GrammarTreeNode::Proc('a'))
				/ (
					tr(GrammarTreeNode::Proc('b'))
						/ tr(GrammarTreeNode::Token(Token::new('c', String::from("123"), TokenPosition::new(6, 9))))
				)
		));

		let result2 = grammar.parse('a', &[Token::new('x', String::from("123"), TokenPosition::new(6, 9))]);
		assert_eq!(result2, Err(ParsingError::new("Expected c".to_string(), TokenPosition::new(6, 9))));
	}

	fn correctly_parses_mixed_sequence() {
		let grammar = grammar!(
			'a' => token!('x'), proc!('b');
			'b' => token!('y');
		);

		let result = grammar.parse('a', &[
			Token::new('x', String::from("123"), TokenPosition::new(6, 9)),
			Token::new('y', String::from("456"), TokenPosition::new(4, 20)),
		]);
		assert_eq!(result, Ok(
			tr(GrammarTreeNode::Proc('a'))
				/ tr(GrammarTreeNode::Token(Token::new('x', String::from("123"), TokenPosition::new(6, 9))))
				/ (
					tr(GrammarTreeNode::Proc('b'))
						/ tr(GrammarTreeNode::Token(Token::new('c', String::from("456"), TokenPosition::new(4, 20))))
				)
		));

		let result2 = grammar.parse('a', &[
			Token::new('x', String::from("123"), TokenPosition::new(6, 9)),
			Token::new('z', String::from("breaks here"), TokenPosition::new(4, 20)),
		]);
		assert_eq!(result2, Err(ParsingError::new("Expected y".to_string(), TokenPosition::new(4, 20))));
	}
}