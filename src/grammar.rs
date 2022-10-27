use std::fmt;

use trees::Tree;

use crate::{tokenize::Token, fragment::{Fragment, SequenceView}};

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

enum GrammarTreeNode<PN, TN> {
	Proc(PN),
	Token(Token<TN>)
}

struct GrammarTreeBuilder<PN, TN> {
	tree_stack: Vec<Tree<GrammarTreeNode<PN, TN>>>,
	result: Result<Tree<GrammarTreeNode<PN, TN>>, String>
}

impl<PN, TN> GrammarTreeBuilder<PN, TN> {
	fn new() -> Self {
		GrammarTreeBuilder { tree_stack: vec![], result: Err(String::from("Syntax tree is incomplete!")) }
	}

	fn current_proc_mut<'b>(&'b mut self) -> &'b mut Tree<GrammarTreeNode<PN, TN>> {
		self.tree_stack.last_mut().expect("Tried to push token, but no proc was pushed")
	}

	fn push_token(&mut self, token: Token<TN>) {
		self.current_proc_mut().push_back(Tree::new(GrammarTreeNode::Token(token)));
	}

	fn push_proc(&mut self, name: PN) {
		self.tree_stack.push(Tree::new(GrammarTreeNode::Proc(name)));
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

pub struct GrammarTokenFragment<TN> {
	name: TN
}

impl<TN> GrammarTokenFragment<TN> {
	pub fn new(name: TN) -> Self {
		GrammarTokenFragment { name }
	}
}

impl<'a, PN, TN: Eq + Copy + fmt::Display> Fragment<Token<TN>, GrammarTreeBuilder<PN, TN>, Grammar<PN, TN>> for GrammarTokenFragment<TN> {
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
			return Err(format!("Expected {}, got \"{}\"", self.name, next_token.str));
		}
		view.advance(1);
		acc.push_token(next_token.clone());
		Ok(())
	}
}

pub struct GrammarProcFragment<PN> {
	name: PN
}

impl<PN> GrammarProcFragment<PN> {
	pub fn new(name: PN) -> Self {
		GrammarProcFragment { name }
	}
}

impl<'a, PN: Eq + Copy + fmt::Display, TN> Fragment<Token<TN>, GrammarTreeBuilder<PN, TN>, Grammar<PN, TN>> for GrammarProcFragment<PN> {
	fn compare(
		&self,
		view: &mut SequenceView<Token<TN>>,
		acc: &mut GrammarTreeBuilder<PN, TN>,
		context: &Grammar<PN, TN>
	) -> Result<(), String> {
		if view.items().is_empty() {
			return Err(format!("Unexpected end of file; expected {}", self.name))
		}
		acc.push_proc(self.name);
		context.compare_proc(self.name, view, acc)?;
		acc.pop_proc();
		Ok(())
	}
}

struct GrammarProc<PN, TN> {
	name: PN,
	fragment: Box<dyn Fragment<Token<TN>, GrammarTreeBuilder<PN, TN>, Grammar<PN, TN>>>
}

impl<PN, TN> GrammarProc<PN, TN> {
	fn new(name: PN, fragment: Box<dyn Fragment<Token<TN>, GrammarTreeBuilder<PN, TN>, Grammar<PN, TN>>>) -> Self {
		GrammarProc { name, fragment }
	}
}

struct Grammar<PN, TN> {
	procs: Vec<GrammarProc<PN, TN>>
}

impl<PN: Eq, TN> Grammar<PN, TN> {
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
		for proc in self.procs.iter().filter(|p| p.name == proc) {
			if let Err(msg) = proc.fragment.compare(view, acc, &self) {
				if first_error.is_none() {
					first_error = Some(msg);
				}
			} else {
				return Ok(())
			}
		}
		Err(first_error.unwrap_or(String::from("Unexpected error during parsing")))
	}
}

#[macro_export]
macro_rules! grammar_part_sequence {
	($($part:tt),+) => {
		$crate::fragment::SequenceFragment::new(vec![
			$(Box::new($part)),+
		])
	};
	($part:expr) => {
		$part
	};
}

#[macro_export]
macro_rules! grammar {
	($($proc_name:expr => $($name:expr),+;)*) => {
		$crate::grammar::Grammar::new(vec![
			$($crate::grammar::GrammarProc::new($proc_name, Box::new(grammar_part_sequence!($(($name)),+)))),*
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