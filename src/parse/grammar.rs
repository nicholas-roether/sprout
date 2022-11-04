use std::fmt;

use trees::{Forest, Tree};

use crate::{tokenize::Token, ASTNode, TextPosition, compare::{SequenceView, MatcherContext, MatchError, MatchGraph, Matcher}, ParsingError};

#[derive(Debug, Clone)]
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
struct ASTNodeBuilder<PN: Clone + fmt::Debug + Copy + PartialEq> {
	name: PN,
	children: Forest<ASTNode<PN>>,
	text: String,
	pos: Option<TextPosition>
}

impl<PN: Clone + fmt::Debug + Copy + PartialEq> ASTNodeBuilder<PN> {
	fn new(name: PN) -> Self {
		ASTNodeBuilder { name, children: Forest::new(), text: String::new(), pos: None }
	}

	fn push_token<TN>(&mut self, token: Token<TN>) {
		self.text += &token.str;
		if self.pos.is_none() {
			self.pos = Some(token.pos);
		}
	}

	fn push_child(&mut self, child: Tree<ASTNode<PN>>) {
		self.text += &child.root().data().text;
		if self.pos.is_none() {
			self.pos = Some(child.root().data().pos.clone());
		}
		self.children.push_back(child);
	}

	fn build(self) -> Tree<ASTNode<PN>> {
		let node = ASTNode::new(self.name, self.text, self.pos.unwrap_or(TextPosition::new(0, 1)));
		let mut tree = Tree::new(node);
		tree.append(self.children);
		tree
	}
}

#[derive(Debug, Clone)]
pub struct ASTBuilder<PN: Clone + fmt::Debug + Copy + PartialEq> {
	build_stack: Vec<ASTNodeBuilder<PN>>,
	result: Result<Tree<ASTNode<PN>>, String>
}

impl<PN: Clone + fmt::Debug + Copy + PartialEq> ASTBuilder<PN> {
	fn new() -> Self {
		ASTBuilder { build_stack: vec![], result: Err(String::from("Syntax tree is incomplete!")) }
	}

	fn current_proc_builder_mut<'b>(&'b mut self) -> &'b mut ASTNodeBuilder<PN> {
		self.build_stack.last_mut().expect("Tried to push to AST builder, but no proc was pushed")
	}

	fn push_token<TN>(&mut self, token: Token<TN>) {
		self.current_proc_builder_mut().push_token(token);
	}

	fn push_proc(&mut self, name: PN) {
		self.build_stack.push(ASTNodeBuilder::new(name));
	}

	fn pop_proc(&mut self) {
		if self.build_stack.is_empty() { return; }
		let popped_builder = self.build_stack.pop().unwrap();
		if self.build_stack.is_empty() {
			self.result = Ok(popped_builder.build());
		} else {
			self.current_proc_builder_mut().push_child(popped_builder.build());
		}
	}
}

impl<PN: PartialEq + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Display> Matcher for GrammarItemName<PN, TN> {
	type Item = Token<TN>;
	type Accumulator = ASTBuilder<PN>;
  type ContextData = Grammar<PN, TN>;

	fn compare(
	  &self, sequence: &mut SequenceView<Token<TN>>,
	  accumulator: &mut ASTBuilder<PN>,
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
			  context.data.compare_proc(*proc_name, sequence, accumulator, false)?;
			  Ok(())
		  }
	  }
	}
}

#[derive(Debug)]
pub struct GrammarProc<PN: PartialEq + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Display> {
  name: PN,
  graph: MatchGraph<GrammarItemName<PN, TN>>
}

impl<PN: PartialEq + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Display> GrammarProc<PN, TN> {
  pub fn new(name: PN, graph: MatchGraph<GrammarItemName<PN, TN>>) -> Self {
	  GrammarProc { name, graph }
  }
}

#[derive(Debug)]
pub struct Grammar<PN: PartialEq + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Display> {
  procs: Vec<GrammarProc<PN, TN>>
}

impl<PN: PartialEq + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Display> Grammar<PN, TN> {
	pub fn new(procs: Vec<GrammarProc<PN, TN>>) -> Self {
	  Grammar { procs }
	}

	pub fn parse(&self, proc: PN, tokens: &[Token<TN>]) -> Result<Tree<ASTNode<PN>>, ParsingError> {
	  let mut tree_builder: ASTBuilder<PN> = ASTBuilder::new();
	  let mut seq_view = SequenceView::new(tokens);
	  match self.compare_proc(proc, &mut seq_view, &mut tree_builder, true) {
		  Ok(_) => Ok(tree_builder.result.expect("Unexpected error occurred during compilation")),
		  Err(error) => {
			  let position = if error.index == tokens.len() {
				  match tokens.last() {
					  None => TextPosition::new(0, 0),
					  Some(last_token) => TextPosition::new(last_token.pos.line, last_token.pos.char + last_token.str.len())
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
	  acc: &mut ASTBuilder<PN>,
	  exhaustive: bool,
	) -> Result<(), MatchError> {
	  let mut error: Option<MatchError> = None;
	  acc.push_proc(proc);
	  for proc in self.procs.iter().filter(|p| p.name == proc) {
		  if let Err(new_err) = proc.graph.compare(tokens, acc, &MatcherContext::new(&self, exhaustive)) {
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

#[cfg(test)]
mod tests {
    use trees::tr;

    use crate::{tokenize::Token, token, proc, grammar};

    use super::*;

	#[test]
	fn correctly_parses_single_token() {
		let grammar = grammar!(
			'a' => token!('b');
		);

		let result = grammar.parse('a', &[Token::new('b', String::from("123"), TextPosition::new(6, 9))]);
		assert_eq!(result, Ok(tr(ASTNode::new('a', "123".to_string(), TextPosition::new(6, 9)))));

		let result2 = grammar.parse('a', &[Token::new('c', String::from("123"), TextPosition::new(6, 9))]);
		assert_eq!(result2, Err(ParsingError::new("Expected b".to_string(), TextPosition::new(6, 9))));
	}

	#[test]
	fn correctly_parses_token_sequence() {
		let grammar = grammar!(
			'a' => token!('b'), token!('c');
		);

		let result = grammar.parse('a', &[
			Token::new('b', String::from("123"), TextPosition::new(6, 9)),
			Token::new('c', String::from("456"), TextPosition::new(4, 20)),
		]);
		assert_eq!(result, Ok(tr(ASTNode::new('a', "123456".to_string(), TextPosition::new(6, 9)))));

		let result2 = grammar.parse('a', &[
			Token::new('b', String::from("123"), TextPosition::new(6, 9)),
			Token::new('e', String::from("breaks here"), TextPosition::new(4, 20)),
		]);
		assert_eq!(result2, Err(ParsingError::new("Expected c".to_string(), TextPosition::new(4, 20))));
	}

	#[test]
	fn correctly_parses_single_proc() {
		let grammar = grammar!(
			'a' => proc!('b');
			'b' => token!('c');
		);

		let result = grammar.parse('a', &[Token::new('c', String::from("123"), TextPosition::new(6, 9))]);
		assert_eq!(result, Ok(
			tr(ASTNode::new('a', "123".to_string(), TextPosition::new(6, 9)))
				/ tr(ASTNode::new('b', "123".to_string(), TextPosition::new(6, 9)))
		));

		let result2 = grammar.parse('a', &[Token::new('x', String::from("123"), TextPosition::new(6, 9))]);
		assert_eq!(result2, Err(ParsingError::new("Expected c".to_string(), TextPosition::new(6, 9))));
	}

	#[test]
	fn correctly_parses_mixed_sequence() {
		let grammar = grammar!(
			'a' => token!('x'), proc!('b'), token!('z');
			'b' => token!('y');
		);

		let result = grammar.parse('a', &[
			Token::new('x', String::from("123"), TextPosition::new(6, 9)),
			Token::new('y', String::from("456"), TextPosition::new(4, 20)),
			Token::new('z', String::from("789"), TextPosition::new(1, 2)),
		]);
		assert_eq!(result, Ok(
			tr(ASTNode::new('a', "123456789".to_string(), TextPosition::new(6, 9)))
				/ tr(ASTNode::new('b', "456".to_string(), TextPosition::new(4, 20)))
		));

		let result2 = grammar.parse('a', &[
			Token::new('x', String::from("123"), TextPosition::new(6, 9)),
			Token::new('ö', String::from("breaks here"), TextPosition::new(4, 20)),
		]);
		assert_eq!(result2, Err(ParsingError::new("Expected y".to_string(), TextPosition::new(4, 20))));
	}
}