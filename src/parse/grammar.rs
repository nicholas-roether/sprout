use std::{fmt, collections::HashMap, hash::Hash};

use trees::{Forest, Tree};

use crate::{tokenize::Token, ASTNode, TextPosition, compare::{SequenceView, MatcherContext, MatchError, MatchGraph, Matcher, MatchResult}, ParsingError, AST};

/// The name of a grammar item, meaning a token or a procedure
#[derive(Debug, Clone)]
pub enum GrammarItemName<PN, TN> {
	/// The name of a terminal grammar item (a token).
	Terminal(TN),
	/// The name of a signature grammar item, one that gives this procedure priority over others
	/// even if it only matches up to this point
	Signature(TN),
	/// The name of a non-terminal grammar item (a procedure).
	NonTerminal(PN)
}

impl<PN: fmt::Display, TN: fmt::Display> fmt::Display for GrammarItemName<PN, TN> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Terminal(token_name) | Self::Signature(token_name) => write!(f, "{}", token_name),
			Self::NonTerminal(proc_name) => write!(f, "{}", proc_name)
		}
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

	fn push_child(&mut self, child: AST<PN>) {
		self.text += &child.root().data().text;
		if self.pos.is_none() {
			self.pos = Some(child.root().data().pos.clone());
		}
		self.children.push_back(child);
	}

	fn build(self) -> AST<PN> {
		let node = ASTNode::new(self.name, self.text, self.pos.unwrap_or(TextPosition::new(1, 0, 0)));
		let mut tree = Tree::new(node);
		tree.append(self.children);
		tree
	}
}

/// A utility for dynamically constructing [`AST`]s.
#[derive(Debug, Clone)]
pub struct ASTBuilder<PN: Clone + fmt::Debug + Copy + PartialEq> {
	build_stack: Vec<ASTNodeBuilder<PN>>,
	pub result: Result<AST<PN>, String>
}

impl<PN: Clone + fmt::Debug + Copy + PartialEq> ASTBuilder<PN> {
	fn new() -> Self {
		ASTBuilder { build_stack: vec![], result: Err(String::from("Syntax tree is incomplete!")) }
	}

	fn current_proc_builder_mut<'b>(&'b mut self) -> &'b mut ASTNodeBuilder<PN> {
		self.build_stack.last_mut().expect("Tried to push to AST builder, but no proc was pushed")
	}

	/// Append a token to below the proc node on top of the build stack
	pub fn push_token<TN>(&mut self, token: Token<TN>) {
		self.current_proc_builder_mut().push_token(token);
	}

	/// Push a new token on top of the build stack
	pub fn push_proc(&mut self, name: PN) {
		self.build_stack.push(ASTNodeBuilder::new(name));
	}

	/// Pop the current token off of the build stack, completing it.
	pub fn pop_proc(&mut self) {
		if self.build_stack.is_empty() { return; }
		let popped_builder = self.build_stack.pop().unwrap();
		if self.build_stack.is_empty() {
			self.result = Ok(popped_builder.build());
		} else {
			self.current_proc_builder_mut().push_child(popped_builder.build());
		}
	}
}

/// Settings for the parsing process.
/// 
/// It currently has the following fields:
/// 
/// | Name         | Default | Description                                                                                                                                        |
/// |--------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
/// | `whitespace` | `None`  | A token that should be ignored as meaningless whitespace when encountered, but not expected. You can still use this token in your grammar as well. |
/// 
/// While you can create parsing settings directly, you will usually obtain them by calling
/// `configure()` on a [`Grammar`].
/// 
/// You can set the fields of the struct directly, but the most comfortable way is to use the setter method with the same name
/// field you want to set.
/// 
/// # Examples
/// Configuring a whitespace token
/// ```
/// # use sprout::prelude::*;
/// use sprout::parse::ParsingSettings;
/// #
/// # #[derive(Clone, Copy)]
/// # enum Token { Whitespace }
/// 
/// let mut settings = ParsingSettings::new();
/// settings.whitespace(Token::Whitespace);
/// ```
#[derive(Debug)]
pub struct ParsingSettings<TN: Copy> {
	pub whitespace: Option<TN>
}


impl<TN: Copy> ParsingSettings<TN> {
	pub fn new() -> Self {
		ParsingSettings { whitespace: None }
	}

	/// A token that should be ignored as meaningless whitespace. The default value is `None`.
	/// 
	/// You can still use this token in your grammar; if you define it as part of a procedure,
	/// it will still be matched just like any other token, but if it is encountered in the text
	/// even though it wasn't expected, it will simply be ignored.
	/// 
	/// This setting allows you to specify which characters your parser considers to be meaningless in most
	/// contexts, so that you don't have to litter optional whitespace tokens all over your procedure definitions.
	pub fn whitespace(&mut self, whitespace: TN) -> &mut Self {
		self.whitespace = Some(whitespace);
		self
	}
}

/// A context object that is passed to grammar item matchers during the parsing process.
pub struct ParsingContext<'a, PN: Eq + Hash + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Display> {
	grammar: &'a Grammar<PN, TN>,
	settings: &'a ParsingSettings<TN>
}

impl<'a, PN: Eq + Hash + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Display> ParsingContext<'a, PN, TN> {
	fn new(grammar: &'a Grammar<PN, TN>) -> Self {
		ParsingContext { grammar, settings: &grammar.settings }
	}
}

impl<PN: Eq + Hash + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Display> GrammarItemName<PN, TN> {
	fn compare_as_token(
		token_name: TN,
		sequence: &mut SequenceView<Token<TN>>,
		accumulator: &mut ASTBuilder<PN>,
		signature: bool
	) -> Result<MatchResult, MatchError> {
		let error = Err(MatchError::simple(format!("{token_name}"), sequence.index));
		if sequence.items().is_empty() {
			return error;
		}
		let next_token = sequence.items().first().unwrap();
		if next_token.name != token_name {
			return error;
		}
		sequence.index += 1;
		accumulator.push_token(next_token.clone());
		Ok(if signature { MatchResult::Signature } else { MatchResult::Match })
	}

	fn compare_as_proc(
		proc_name: PN,
		sequence: &mut SequenceView<Token<TN>>,
		accumulator: &mut ASTBuilder<PN>,
		context: &MatcherContext<ParsingContext<PN, TN>>
	) -> Result<MatchResult, MatchError> {
		if sequence.items().is_empty() {
			return Err(MatchError::simple(proc_name.to_string(), sequence.index));
		}
		context.data.grammar.compare_proc(
			proc_name,
			sequence,
			accumulator,
			&MatcherContext::new(context.data, false)
		)?;
		Ok(MatchResult::Match)
	}

	fn next_is_whitespace(sequence: &mut SequenceView<Token<TN>>, context: &MatcherContext<ParsingContext<PN, TN>>) -> bool {
		let Some(whitespace_token) = context.data.settings.whitespace else {
			return false;
		};
		if sequence.is_empty() {
			return false;
		}
		if sequence.items().first().unwrap().name == whitespace_token {
			return true;
		}
		false
	}
}

impl<PN: Eq + Hash + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Display> Matcher for GrammarItemName<PN, TN> {
	type Item = Token<TN>;
	type Accumulator = ASTBuilder<PN>;
	type ContextData<'a> = ParsingContext<'a, PN, TN> where Self: 'a;

	fn compare(
		&self, sequence: &mut SequenceView<Token<TN>>,
		accumulator: &mut ASTBuilder<PN>,
		context: &MatcherContext<ParsingContext<PN, TN>>
	) -> Result<MatchResult, MatchError> {
		loop {
			let result = match self {
				GrammarItemName::Terminal(token_name) => Self::compare_as_token(*token_name, sequence, accumulator, false),
				GrammarItemName::Signature(token_name) => Self::compare_as_token(*token_name, sequence, accumulator, true),
				GrammarItemName::NonTerminal(proc_name) => Self::compare_as_proc(*proc_name, sequence, accumulator, context)
			};
			if result.is_ok() {
				return result;
			}
			if Self::next_is_whitespace(sequence, context) {
				sequence.index += 1;
				continue;
			}
			return result;
		}
	}
}

/// A procedure definition in a grammar.
/// 
/// `GrammarProc`s shouldn't be created independently from a [`Grammar`]. If you want to create a [`Grammar`]
/// dynamically instead of using the [`crate::grammar`] macro, use [`crate::parse::GrammarBuilder`] instead. 
#[derive(Debug)]
pub struct GrammarProc<PN: Eq + Hash + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Display> {
	name: PN,
	graph: MatchGraph<GrammarItemName<PN, TN>>
}

impl<PN: Eq + Hash + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Display> GrammarProc<PN, TN> {
	pub fn new(name: PN, graph: MatchGraph<GrammarItemName<PN, TN>>) -> Self {
		GrammarProc { name, graph }
	}
}

/// The error behavior of the proc. The following options are available:
/// 
/// | Name      | Description                                                          |
/// |-----------|----------------------------------------------------------------------|
/// | Default   | The default error behavior                                           |
/// | Primitive | Errors will always appear at the level of this proc instead of below |
/// | Hidden    | Errors will never appear at the level of this proc; always below     |
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ProcErrorBehavior {
	Default,
	Primitive,
	Hidden
}

impl fmt::Display for ProcErrorBehavior {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Default => write!(f, "default"),
			Self::Primitive => write!(f, "primitive"),
			Self::Hidden => write!(f, "hidden")
		}
	}
}

/// A `Grammar` defines a set of rules, "procedures", that define the structure of the language that should be interpreted.
/// The grammar can then parse a `Vec` of [`Token`]s to produce an [`AST`].
/// 
/// The best way to create a `Grammar` is using the [`crate::grammar`] macro. If want to create a `Grammar` dynamically,
/// you can use a [`crate::parse::GrammarBuilder`].
/// 
/// # Examples
/// 
/// creating a `Grammar` with the [`grammar`] macro (see its documentation for more info)
/// ```
/// # use sprout::prelude::*;
/// #
/// #[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// enum Token {
/// 	Word,
/// 	Space,
/// 	Dot
/// }
/// 
/// #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
/// enum Proc {
/// 	Sentence,
/// 	Text
/// }
/// 
/// // Though not shown here, both Token an Proc need to implement std::fmt::Display so that
/// // human-readable error messages can be generated
/// # use std::fmt;
/// # impl fmt::Display for Token { fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() } }
/// # impl fmt::Display for Proc { fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { todo!() } }
/// 
/// use Token::*;
/// use Proc::*;
/// 
/// let grammar = grammar! {
/// 	#Sentence => (Word){Space}+, Dot;
/// 	#Text => (#Sentence){Space}+;
/// };
/// 
/// ```
#[derive(Debug)]
pub struct Grammar<PN: Eq + Hash + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Display> {
	procs: Vec<GrammarProc<PN, TN>>,
	error_behaviors: HashMap<PN, ProcErrorBehavior>,
	settings: ParsingSettings<TN>
}

impl<PN: Eq + Hash + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Display> Grammar<PN, TN> {
	pub fn new(procs: Vec<GrammarProc<PN, TN>>, error_behaviors: HashMap<PN, ProcErrorBehavior>) -> Self {
		Grammar { procs, settings: ParsingSettings::new(), error_behaviors }
	}

	/// Get the [`ParsingSettings`] for this `Grammar`. See the [`ParsingSettings`] documentation for more
	/// info.
	pub fn configure(&mut self) -> &mut ParsingSettings<TN> {
		&mut self.settings
	}
	
	/// Parse a slice of [`Token`]s into an [`AST`] according to some procedure `proc` defined in this grammar.
	/// 
	/// There aren't many cases where this function needs to be used directly; manually generating
	/// tokens is cumbersome, so in most cases it is best to combine your `Grammar` with an [`crate::Alphabet`]
	/// and create a [`crate::Parser`].
	/// 
	/// Returns a `Result` with [`ParsingError`] as the error type.
	/// 
	/// # Examples
	/// 
	/// Parsing a slice of [`Token`]s
	/// ```
	/// # use sprout::prelude::*;
	/// # use std::fmt;
	/// #
	/// # #[derive(Clone, Copy, Debug, Eq, PartialEq)]
	/// # enum TokenName { Word }
	/// # #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
	/// # enum Proc { Sentence }
	/// #
	/// # impl fmt::Display for TokenName { fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "") } }
	/// # impl fmt::Display for Proc { fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "") } }
	/// #
	/// # let grammar = grammar! { #Proc::Sentence => TokenName::Word; };
	/// #
	/// use sprout::{parse::Grammar, tokenize::Token};
	/// 
	/// grammar.parse(Proc::Sentence, &[
	/// 	Token::new(TokenName::Word, "abc".to_string(), TextPosition::new(1, 0, 0)),
	/// 	Token::new(TokenName::Word, "xyz".to_string(), TextPosition::new(1, 3, 3))
	/// ]);
	/// ```
	pub fn parse(&self, proc: PN, tokens: &[Token<TN>]) -> Result<AST<PN>, ParsingError> {
		let mut tree_builder: ASTBuilder<PN> = ASTBuilder::new();
		let mut seq_view = SequenceView::new(tokens);
		match self.compare_proc(proc, &mut seq_view, &mut tree_builder, &MatcherContext::new(&ParsingContext::new(&self), true)) {
			Ok(_) => Ok(tree_builder.result.expect("Unexpected error occurred during compilation")),
			Err(error) => {
				let position = if error.index == tokens.len() {
				match tokens.last() {
					None => TextPosition::new(1, 0, 0),
					Some(last_token) => TextPosition::new(
						last_token.pos.line,
						last_token.pos.char + last_token.str.len(),
						last_token.pos.index + last_token.str.len()
					)
				}
				} else {
					tokens[error.index].pos.clone()
				};
				Err(ParsingError::new(format!("{error}"), position))
			}
		}
	}

	/// Get the [`ProcErrorBehavior`] for a given proc name
	pub fn get_error_behavior(&self, proc: PN) -> ProcErrorBehavior {
		*self.error_behaviors.get(&proc).unwrap_or(&ProcErrorBehavior::Default)
	}

	fn compare_proc(
		&self,
		proc: PN,
		tokens: &mut SequenceView<Token<TN>>,
		acc: &mut ASTBuilder<PN>,
		context: &MatcherContext<ParsingContext<PN, TN>>
	) -> Result<(), MatchError> {
		let start_index = tokens.index;
		let mut error: Option<MatchError> = None;
		if self.get_error_behavior(proc) != ProcErrorBehavior::Hidden {
			error = Some(MatchError::simple(proc.to_string(), start_index));
		}
		acc.push_proc(proc);
		for proc in self.procs.iter().filter(|p| p.name == proc) {
			if let Err(new_err) = proc.graph.compare(tokens, acc, context) {
				if error.is_none() || new_err.index > error.as_ref().unwrap().index {
					error = Some(new_err);
				}
			} else {
				acc.pop_proc();
				return Ok(())
			}
		}

		if self.get_error_behavior(proc) == ProcErrorBehavior::Primitive {
			return Err(MatchError::simple(proc.to_string(), start_index));
		}

		Err(error.expect(&format!("No definitions were found for proc {proc}")))
	}
}

#[cfg(test)]
mod tests {
    use trees::tr;

    use crate::{tokenize::Token, grammar};

    use super::*;

	#[test]
	fn correctly_parses_single_token() {
		let grammar = grammar!(
			#'a' => 'b';
		);

		let result = grammar.parse('a', &[Token::new('b', String::from("123"), TextPosition::new(6, 9, 420))]);
		assert_eq!(result, Ok(tr(ASTNode::new('a', "123".to_string(), TextPosition::new(6, 9, 420)))));

		let result2 = grammar.parse('a', &[Token::new('c', String::from("123"), TextPosition::new(6, 9, 420))]);
		assert_eq!(result2, Err(ParsingError::new("Expected a".to_string(), TextPosition::new(6, 9, 420))));
	}

	#[test]
	fn correctly_parses_token_sequence() {
		let grammar = grammar!(
			#'a' => 'b', 'c';
		);

		let result = grammar.parse('a', &[
			Token::new('b', String::from("123"), TextPosition::new(6, 9, 420)),
			Token::new('c', String::from("456"), TextPosition::new(4, 20, 69)),
		]);
		assert_eq!(result, Ok(tr(ASTNode::new('a', "123456".to_string(), TextPosition::new(6, 9, 420)))));

		let result2 = grammar.parse('a', &[
			Token::new('b', String::from("123"), TextPosition::new(6, 9, 420)),
			Token::new('e', String::from("breaks here"), TextPosition::new(4, 20, 69)),
		]);
		assert_eq!(result2, Err(ParsingError::new("Expected c".to_string(), TextPosition::new(4, 20, 69))));
	}

	#[test]
	fn correctly_parses_single_proc() {
		let grammar = grammar!(
			#'a' => #'b';
			#'b' => 'c';
		);

		let result = grammar.parse('a', &[Token::new('c', String::from("123"), TextPosition::new(6, 9, 420))]);
		assert_eq!(result, Ok(
			tr(ASTNode::new('a', "123".to_string(), TextPosition::new(6, 9, 420)))
				/ tr(ASTNode::new('b', "123".to_string(), TextPosition::new(6, 9, 420)))
		));

		let result2 = grammar.parse('a', &[Token::new('x', String::from("123"), TextPosition::new(6, 9, 420))]);
		assert_eq!(result2, Err(ParsingError::new("Expected a".to_string(), TextPosition::new(6, 9, 420))));
	}

	#[test]
	fn correctly_parses_mixed_sequence() {
		let grammar = grammar!(
			#'a' => 'x', #'b', 'z';
			#'b' => 'y';
		);

		let result = grammar.parse('a', &[
			Token::new('x', String::from("123"), TextPosition::new(6, 9, 420)),
			Token::new('y', String::from("456"), TextPosition::new(4, 20, 69)),
			Token::new('z', String::from("789"), TextPosition::new(1, 2, 3)),
		]);
		assert_eq!(result, Ok(
			tr(ASTNode::new('a', "123456789".to_string(), TextPosition::new(6, 9, 420)))
				/ tr(ASTNode::new('b', "456".to_string(), TextPosition::new(4, 20, 69)))
		));

		let result2 = grammar.parse('a', &[
			Token::new('x', String::from("123"), TextPosition::new(6, 9, 420)),
			Token::new('ö', String::from("breaks here"), TextPosition::new(4, 20, 69)),
		]);
		assert_eq!(result2, Err(ParsingError::new("Expected b".to_string(), TextPosition::new(4, 20, 69))));
	}

	#[test]
	fn should_handle_non_primitive_proc_errors() {
		let grammar = grammar! {
			#'a' => 'x', 'y';
		};

		let res = grammar.parse('a', &[
			Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
			Token::new('z', "123".to_string(), TextPosition::new(1, 5, 5)),
		]);
		assert!(res.is_err());
		assert_eq!(res.as_ref().unwrap_err().message, "Expected y");
		assert_eq!(res.as_ref().unwrap_err().pos.index, 5);
	}

	#[test]
	fn should_handle_primitive_proc_errors() {
		let grammar = grammar! {
			#!'a' => 'x', 'y';
		};

		let res = grammar.parse('a', &[
			Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
			Token::new('z', "123".to_string(), TextPosition::new(5, 0, 0)),
		]);
		assert!(res.is_err());
		assert_eq!(res.as_ref().unwrap_err().message, "Expected a");
		assert_eq!(res.as_ref().unwrap_err().pos.index, 0);
	}

	#[test]
	fn should_handle_nested_primitive_proc_errors() {
		let grammar = grammar! {
			#'b' => '_', #'a';
			#!'a' => 'x', 'y';
		};

		let res = grammar.parse('b', &[
			Token::new('_', "123".to_string(), TextPosition::new(1, 0, 0)),
			Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
			Token::new('z', "123".to_string(), TextPosition::new(5, 0, 0)),
		]);
		assert!(res.is_err());
		assert_eq!(res.as_ref().unwrap_err().message, "Expected a");
		assert_eq!(res.as_ref().unwrap_err().pos.index, 0);
	}

	#[test]
	fn should_handle_hidden_proc_errors() {
		let grammar = grammar! {
			#?'a' => 'x', 'y';
		};

		let res = grammar.parse('a', &[
			Token::new('2', "123".to_string(), TextPosition::new(1, 0, 0)),
		]);
		assert!(res.is_err());
		assert_eq!(res.as_ref().unwrap_err().message, "Expected x");
		assert_eq!(res.as_ref().unwrap_err().pos.index, 0);
	}

	#[test]
	fn should_handle_split_proc_errors() {
		let grammar = grammar! {
			#'a' => ['x'; 'y'];
		};

		let res = grammar.parse('a', &[
			Token::new('z', "123".to_string(), TextPosition::new(5, 0, 0)),
		]);
		assert!(res.is_err());
		assert_eq!(res.as_ref().unwrap_err().message, "Expected a");
		assert_eq!(res.as_ref().unwrap_err().pos.index, 0);
	}

	#[test]
	fn should_handle_partial_split_proc_errors() {
		let grammar = grammar! {
			#'a' => ['x'; 'y', 'z'];
		};

		let res = grammar.parse('a', &[
			Token::new('y', "123".to_string(), TextPosition::new(5, 0, 0)),
		]);
		assert!(res.is_err());
		assert_eq!(res.as_ref().unwrap_err().message, "Expected z");
		assert_eq!(res.as_ref().unwrap_err().pos.index, 3);
	}

	#[test]
	fn should_handle_sub_proc_errors() {
		let grammar = grammar! {
			#'a' => #'b';
			#'b' => 'y', 'z'
		};

		let res = grammar.parse('a', &[
			Token::new('y', "123".to_string(), TextPosition::new(5, 0, 0)),
			Token::new('ä', "123".to_string(), TextPosition::new(5, 0, 6)),
		]);
		assert!(res.is_err());
		assert_eq!(res.as_ref().unwrap_err().message, "Expected z");
		assert_eq!(res.as_ref().unwrap_err().pos.index, 6);
	}

	#[test]
	fn should_handle_completely_unmatched_sub_proc_errors() {
		let grammar = grammar! {
			#'a' => #'b';
			#'b' => 'z'
		};

		let res = grammar.parse('a', &[
			Token::new('y', "123".to_string(), TextPosition::new(5, 0, 0)),
		]);
		assert!(res.is_err());
		assert_eq!(res.as_ref().unwrap_err().message, "Expected a");
		assert_eq!(res.as_ref().unwrap_err().pos.index, 0);
	}

	#[test]
	fn should_handle_partial_split_proc_errors_at_same_depth() {
		let grammar = grammar! {
			#'a' => ['y', 'x'; 'y', 'z'];
		};

		let res = grammar.parse('a', &[
			Token::new('y', "123".to_string(), TextPosition::new(5, 0, 0)),
		]);
		assert!(res.is_err());
		assert_eq!(res.as_ref().unwrap_err().message, "Expected one of: x, z");
		assert_eq!(res.as_ref().unwrap_err().pos.index, 3);
	}

	#[test]
	fn should_handle_split_with_signature() {
		let grammar = grammar! {
			#'a' => [@'y', 'x'; 'y', 'z'];
		};

		let res = grammar.parse('a', &[
			Token::new('y', "123".to_string(), TextPosition::new(5, 0, 0)),
		]);
		assert!(res.is_err());
		assert_eq!(res.as_ref().unwrap_err().message, "Expected x");
		assert_eq!(res.as_ref().unwrap_err().pos.index, 3);
	}

	#[test]
	fn configuring_whitespace_works() {
		let mut grammar = grammar!(
			#'a' => 'x', #'b', 'z';
			#'b' => 'y';
		);

		grammar.configure().whitespace(' ');

		let result = grammar.parse('a', &[
			Token::new('x', String::from("123"), TextPosition::new(6, 9, 420)),
			Token::new(' ', String::from("   "), TextPosition::new(6, 9, 421)),
			Token::new('y', String::from("456"), TextPosition::new(4, 20, 69)),
			Token::new(' ', String::from("    "), TextPosition::new(4, 20, 70)),
			Token::new('z', String::from("789"), TextPosition::new(1, 2, 3)),
		]);
		assert_eq!(result, Ok(
			tr(ASTNode::new('a', "123456789".to_string(), TextPosition::new(6, 9, 420)))
				/ tr(ASTNode::new('b', "456".to_string(), TextPosition::new(4, 20, 69)))
		));

		let result = grammar.parse('a', &[
			Token::new('x', String::from("123"), TextPosition::new(6, 9, 420)),
			Token::new('y', String::from("456"), TextPosition::new(4, 20, 69)),
			Token::new('z', String::from("789"), TextPosition::new(1, 2, 3)),
		]);
		assert_eq!(result, Ok(
			tr(ASTNode::new('a', "123456789".to_string(), TextPosition::new(6, 9, 420)))
				/ tr(ASTNode::new('b', "456".to_string(), TextPosition::new(4, 20, 69)))
		));

		let result2 = grammar.parse('a', &[
			Token::new('x', String::from("123"), TextPosition::new(6, 9, 420)),
			Token::new('ö', String::from("breaks here"), TextPosition::new(4, 20, 69)),
		]);
		assert_eq!(result2, Err(ParsingError::new("Expected b".to_string(), TextPosition::new(4, 20, 69))));
	}

	#[test]
	fn should_correctly_handle_whitespace_in_proc_def_when_whitespace_is_configured() {
		let mut grammar = grammar!(
			#'a' => 'x', ' ', 'y';
		);

		grammar.configure().whitespace(' ');

		let result = grammar.parse('a', &[
			Token::new('x', String::from("123"), TextPosition::new(6, 9, 420)),
			Token::new(' ', String::from("_"), TextPosition::new(6, 9, 421)),
			Token::new(' ', String::from("   "), TextPosition::new(6, 9, 422)),
			Token::new('y', String::from("456"), TextPosition::new(4, 20, 69)),
		]);
		assert_eq!(result, Ok(
			tr(ASTNode::new('a', "123_456".to_string(), TextPosition::new(6, 9, 420)))
		));


		let result2 = grammar.parse('a', &[
			Token::new('x', String::from("123"), TextPosition::new(6, 9, 420)),
			Token::new('y', String::from("breaks here"), TextPosition::new(4, 20, 69)),
		]);
		assert_eq!(result2, Err(ParsingError::new("Expected  ".to_string(), TextPosition::new(4, 20, 69))));
	}
}