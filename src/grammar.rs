use std::fmt;

use trees::{Tree, tr};

use crate::{tokenize::Token, fragments::{Fragment, SequenceView}};

enum GrammarItemName<PN, TN> {
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

#[derive(Debug)]
pub struct GrammarTokenFragment<TN> {
	name: TN
}

impl<TN> GrammarTokenFragment<TN> {
	pub fn new(name: TN) -> Self {
		GrammarTokenFragment { name }
	}
}

impl<'a, PN: Copy, TN: Eq + Copy + fmt::Debug + fmt::Display> Fragment<Token<TN>, GrammarTreeBuilder<PN, TN>, Grammar<PN, TN>> for GrammarTokenFragment<TN> {
	fn compare(
		&self,
		view: &mut SequenceView<Token<TN>>,
		acc: &mut GrammarTreeBuilder<PN, TN>,
		_context: &Grammar<PN, TN>
	) -> Result<(), String> {
		if view.items().is_empty() {
			return Err(format!("Unexpected end of file; expected {}", self.name))
		}
		let next_token = view.items().first().unwrap();
		if next_token.name != self.name {
			return Err(format!("Expected {}, but got {}", self.name, next_token));
		}
		view.advance(1);
		acc.push_token(next_token.clone());
		Ok(())
	}
}

#[derive(Debug)]
pub struct GrammarProcFragment<PN> {
	name: PN
}

impl<PN> GrammarProcFragment<PN> {
	pub fn new(name: PN) -> Self {
		GrammarProcFragment { name }
	}
}

impl<'a, PN: Eq + Copy + fmt::Debug + fmt::Display, TN: Copy> Fragment<Token<TN>, GrammarTreeBuilder<PN, TN>, Grammar<PN, TN>> for GrammarProcFragment<PN> {
	fn compare(
		&self,
		view: &mut SequenceView<Token<TN>>,
		acc: &mut GrammarTreeBuilder<PN, TN>,
		context: &Grammar<PN, TN>
	) -> Result<(), String> {
		if view.items().is_empty() {
			return Err(format!("Unexpected end of file; expected {}", self.name))
		}
		context.compare_proc(self.name, view, acc)?;
		Ok(())
	}
}

#[derive(Debug)]
pub struct GrammarProc<PN: Clone, TN: Clone> {
	name: PN,
	fragment: Box<dyn Fragment<Token<TN>, GrammarTreeBuilder<PN, TN>, Grammar<PN, TN>>>
}

impl<PN: Clone, TN: Clone> GrammarProc<PN, TN> {
	pub fn new(name: PN, fragment: Box<dyn Fragment<Token<TN>, GrammarTreeBuilder<PN, TN>, Grammar<PN, TN>>>) -> Self {
		GrammarProc { name, fragment }
	}
}

#[derive(Debug)]
pub struct Grammar<PN: Clone, TN: Clone> {
	procs: Vec<GrammarProc<PN, TN>>
}

impl<PN: Eq + Copy, TN: Copy> Grammar<PN, TN> {
	pub fn new(procs: Vec<GrammarProc<PN, TN>>) -> Self {
		Grammar { procs }
	}

	pub fn parse(&self, proc: PN, tokens: Vec<Token<TN>>) -> Result<Tree<GrammarTreeNode<PN, TN>>, String> {
		let mut seq_view = SequenceView::new(&tokens);
		let mut tree_builder: GrammarTreeBuilder<PN, TN> = GrammarTreeBuilder::new();
		self.compare_proc(proc, &mut seq_view, &mut tree_builder)?;
		tree_builder.result
	}

	fn compare_proc(
		&self,
		proc: PN,
		view: &mut SequenceView<Token<TN>>,
		acc: &mut GrammarTreeBuilder<PN, TN>,
	) -> Result<(), String> {
		let mut first_error: Option<String> = None;
		acc.push_proc(proc);
		for proc in self.procs.iter().filter(|p| p.name == proc) {
			if let Err(msg) = proc.fragment.compare(view, acc, &self) {
				if first_error.is_none() {
					first_error = Some(msg);
				}
			} else {
				acc.pop_proc();
				return Ok(())
			}
		}
		
		Err(first_error.unwrap_or(String::from("Unexpected error during parsing")))
	}
}

#[macro_export]
macro_rules! grammar {
	($($proc_name:expr => $($part:expr),+);*$(;)?) => {
		$crate::grammar::Grammar::new(vec![
			$($crate::grammar::GrammarProc::new($proc_name, Box::new($crate::sequence!($($part),+)))),*
		])
	};
}

#[macro_export]
macro_rules! token {
	($name:expr) => {
		$crate::grammar::GrammarTokenFragment::new($name)
	};
}

#[macro_export]
macro_rules! proc {
	($name:expr) => {
		$crate::grammar::GrammarProcFragment::new($name)
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

		let result = grammar.parse('a', vec![Token::new('b', String::from("123"), TokenPosition::new(6, 9))]);
		assert_eq!(result, Ok(tr(GrammarTreeNode::Proc('a')) / tr(GrammarTreeNode::Token(Token::new('b', String::from("123"), TokenPosition::new(6, 9))))));

		let result2 = grammar.parse('a', vec![Token::new('c', String::from("123"), TokenPosition::new(6, 9))]);
		assert_eq!(result2, Err(String::from("Expected b, but got c \"123\" at 6:9")));
	}

	#[test]
	fn correctly_parses_token_sequence() {
		let grammar = grammar!(
			'a' => token!('b'), token!('c');
		);

		let result = grammar.parse('a', vec![
			Token::new('b', String::from("123"), TokenPosition::new(6, 9)),
			Token::new('c', String::from("456"), TokenPosition::new(4, 20)),
		]);
		assert_eq!(result, Ok(
			tr(GrammarTreeNode::Proc('a'))
				/ tr(GrammarTreeNode::Token(Token::new('b', String::from("123"), TokenPosition::new(6, 9))))
				/ tr(GrammarTreeNode::Token(Token::new('c', String::from("456"), TokenPosition::new(4, 20))))
		));

		let result2 = grammar.parse('a', vec![
			Token::new('b', String::from("123"), TokenPosition::new(6, 9)),
			Token::new('e', String::from("breaks here"), TokenPosition::new(4, 20)),
		]);
		assert_eq!(result2, Err(String::from("Expected c, but got e \"breaks here\" at 4:20")));
	}

	fn correctly_parses_single_proc() {
		let grammar = grammar!(
			'a' => proc!('b');
			'b' => token!('c');
		);

		let result = grammar.parse('a', vec![Token::new('c', String::from("123"), TokenPosition::new(6, 9))]);
		assert_eq!(result, Ok(
			tr(GrammarTreeNode::Proc('a'))
				/ (
					tr(GrammarTreeNode::Proc('b'))
						/ tr(GrammarTreeNode::Token(Token::new('b', String::from("123"), TokenPosition::new(6, 9))))
				)
		));

		let result2 = grammar.parse('a', vec![Token::new('x', String::from("123"), TokenPosition::new(6, 9))]);
		assert_eq!(result2, Err(String::from("Expected c, but got x \"123\" at 6:9")));
	}

	fn correctly_parses_mixed_sequence() {
		let grammar = grammar!(
			'a' => token!('x'), proc!('b');
			'b' => token!('y');
		);

		let result = grammar.parse('a', vec![
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

		let result2 = grammar.parse('a', vec![
			Token::new('x', String::from("123"), TokenPosition::new(6, 9)),
			Token::new('z', String::from("breaks here"), TokenPosition::new(4, 20)),
		]);
		assert_eq!(result2, Err(String::from("Expected y, but got z \"breaks here\" at 4:20")));
	}
}