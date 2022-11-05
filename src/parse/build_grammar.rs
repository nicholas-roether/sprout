use std::fmt;

use crate::compare::MatchGraphBuilder;

use super::{GrammarProc, Grammar, GrammarItemName};

#[derive(Debug, PartialEq, Eq)]
enum GrammarBuilderState {
	Repeat(u32),
	Optional,
	Choice
}

/// A utility for dynamically building grammars.
/// 
/// If you only want to create a static grammar that won't change over the runtime of your program,
/// consider using the [`grammar`] macro instead.
/// 
/// # Examples
/// Creating a simple grammar
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
/// #[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
/// use sprout::parse::GrammarBuilder;
/// 
/// use Token::*;
/// use Proc::*;
/// 
/// let mut grammar_builder = GrammarBuilder::new();
/// 
/// // Start a primtive procedure definition
/// // Primitive procedures will not reference their composite parts in error messages
/// grammar_builder.define(Sentence, true);
/// 
/// // Add a token to the current procedure
/// grammar_builder.token(Word);
/// 
/// // Start a repeat block with a minimum of 0 repetitions
/// grammar_builder.start_repeat(0);
/// 
/// grammar_builder.token(Space);
/// grammar_builder.token(Word);
/// 
/// // end the repeat block
/// grammar_builder.end();
/// 
/// grammar_builder.token(Dot);
/// 
/// // Start the next (non-primitive) procedure definition
/// grammar_builder.define(Text, false);
/// 
/// // Add a reference to another procedure to the current procedure
/// grammar_builder.proc(Sentence);
/// 
/// grammar_builder.start_repeat(0);
/// grammar_builder.token(Space);
/// grammar_builder.proc(Sentence);
/// grammar_builder.end();
/// 
/// // Extract the finished grammar
/// let grammar = grammar_builder.complete();
/// ```
/// 
/// See method documentation for more options.
#[derive(Debug)]
pub struct GrammarBuilder<PN: PartialEq + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Debug + fmt::Display> {
	procs: Vec<GrammarProc<PN, TN>>,
	current_proc: Option<(PN, MatchGraphBuilder<GrammarItemName<PN, TN>>)>,
	primitive_procs: Vec<PN>,
	defined_procs: Vec<PN>,
	state_stack: Vec<GrammarBuilderState>
}

impl<PN: PartialEq + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Debug + fmt::Display> GrammarBuilder<PN, TN> {
	pub fn new() -> Self {
		GrammarBuilder { procs: vec![], current_proc: None, state_stack: vec![], primitive_procs: vec![], defined_procs: vec![] }
	}

	/// Start a new procedure definition.
	/// 
	/// Setting `primitive` to true will make the parser consider this proc a
	/// "basic building block" of the language you are parsing, which means that
	/// error messages will always refer to this proc by name rather than it's constituent
	/// components.
	/// 
	/// Will panic if there are any unfinished repeats, optionals, or choices.
	pub fn define(&mut self, proc: PN, primitive: bool) {
		self.complete_proc();
		if primitive {
			if !self.primitive_procs.contains(&proc) {
				if self.defined_procs.contains(&proc) {
					panic!("Ambiguous proc primitivity: {proc:?} is defined as both primitive and non-primitive");
				}
				self.primitive_procs.push(proc);
			}
		} else {
			if self.primitive_procs.contains(&proc) {
				panic!("Ambiguous proc primitivity: {proc:?} is defined as both primitive and non-primitive");
			}
		}
		self.current_proc = Some((proc, MatchGraphBuilder::new()))
	}
	
	/// Add a token to the current procedure definition.
	/// 
	/// Will panic if no procedure definition was started yet.
	pub fn token(&mut self, token: TN) {
		self.get_proc_builder().append(GrammarItemName::Terminal(token));
	}

	/// Add a procedure to the current procedure definition.
	/// 
	/// Will panic if no procedure definition was started yet.
	pub fn proc(&mut self, proc: PN) {
		self.get_proc_builder().append(GrammarItemName::NonTerminal(proc));
	}

	/// Start a repeat block with a minimum of `min` repeats.
	/// 
	/// Will panic if no procedure definition was started yet.
	pub fn start_repeat(&mut self, min: u32) {
		self.get_proc_builder().push_return();
		self.state_stack.push(GrammarBuilderState::Repeat(min));
	}

	/// Start an optional block.
	/// 
	/// Will panic if no procedure definition was started yet.
	pub fn start_optional(&mut self) {
		self.get_proc_builder().push_return();
		self.state_stack.push(GrammarBuilderState::Optional);
	}

	/// Start a choice block.
	/// 
	/// Will panic if no procedure definition was started yet.
	pub fn start_choice(&mut self) {
		self.get_proc_builder().push_return();
		self.state_stack.push(GrammarBuilderState::Choice);
	}
	
	/// Within a choice block, finish the current choice path and start defining
	/// the next one.
	/// 
	/// Will panic when not within a choice block or if no procedure definition was started yet.
	pub fn next_choice_path(&mut self) {
		if self.state_stack.last() != Some(&GrammarBuilderState::Choice) {
			panic!("Invalid operation; not within a choice definition");
		}
		self.get_proc_builder().end_choice_path();
	}

	/// End the current block (repeat/optional/choice).
	/// 
	/// Will panic when not within a block or if no procedure definition was started yet.
	pub fn end(&mut self) {
		match self.state_stack.pop() {
			None => panic!("Invalid operation; no start definition matches this end call"),
			Some(GrammarBuilderState::Choice) => {
				self.get_proc_builder().end_choice_path();
				self.get_proc_builder().pop_choice();
			}
			Some(GrammarBuilderState::Optional) => {
				self.get_proc_builder().pop_optional();
			}
			Some(GrammarBuilderState::Repeat(min)) => {
				self.get_proc_builder().duplicate(min);
				self.get_proc_builder().pop_repeat()
			}
		}
	}

	/// Complete the grammar build and extract the constructed [`Grammar`].
	pub fn complete(mut self) -> Grammar<PN, TN> {
		self.complete_proc();
		Grammar::new(self.procs, self.primitive_procs)
	}

	fn get_proc_builder(&mut self) -> &mut MatchGraphBuilder<GrammarItemName<PN, TN>> {
		if let Some((_, builder)) = &mut self.current_proc {
			return builder;
		}
		panic!("Invalid operation; no proc is being defined");
	}

	fn complete_proc(&mut self) {
		if let Some(leftover_state) = self.state_stack.last() {
			match leftover_state {
				GrammarBuilderState::Choice => panic!("Unfinished choice in graph builder"),
				GrammarBuilderState::Optional => panic!("Unfinished optional in graph builder"),
				GrammarBuilderState::Repeat(_) => panic!("Unfinished repeat in graph builder"),
			}
		}

		if let Some((name, graph_builder)) = self.current_proc.take() {
			self.procs.push(GrammarProc::new(name, graph_builder.complete()));
		}
	}
}

/// Internal implementation of [`grammar`]. Potentially useful because it
/// uses an external [`GrammarBuilder`] that is passed as a parameter. 
#[macro_export]
macro_rules! build_grammar {
	// [...], ...
	((proc, $ret:tt) $builder:expr; [$($items:tt)+], $($tail:tt)+) => {
		$builder.start_choice();
		$crate::build_grammar!((choice) $builder; $($items)+);
		$builder.end();
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
	// [...];
	((proc, $ret:tt) $builder:expr; [$($items:tt)+] $(; $($tail:tt)*)?) => {
		$builder.start_choice();
		$crate::build_grammar!((choice) $builder; $($items)+);
		$builder.end();
		$($crate::build_grammar!(($ret) $builder; $($tail)*);)?
	};
	// (...){...}*, ...
	((proc, $ret:tt) $builder:expr; ($($items:tt)+){$($delim:tt)+}*, $($tail:tt)+) => {
		$builder.start_optional();
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.start_repeat(0);
		$crate::build_grammar!((proc, $ret) $builder; $($delim)+);
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$builder.end();
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
	// (...){...}*;
	((proc, $ret:tt) $builder:expr; ($($items:tt)+){$($delim:tt)+}* $(; $($tail:tt)*)?) => {
		$builder.start_optional();
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.start_repeat(0);
		$crate::build_grammar!((proc, $ret) $builder; $($delim)+);
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$builder.end();
		$($crate::build_grammar!(($ret) $builder; $($tail)*);)?
	};
	// (...){...}+, ...
	((proc, $ret:tt) $builder:expr; ($($items:tt)+){$($delim:tt)+}+, $($tail:tt)+) => {
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.start_repeat(0);
		$crate::build_grammar!((proc, $ret) $builder; $($delim)+);
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
	// (...){...}+;
	((proc, $ret:tt) $builder:expr; ($($items:tt)+){$($delim:tt)+}+ $(; $($tail:tt)*)?) => {
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.start_repeat(0);
		$crate::build_grammar!((proc, $ret) $builder; $($delim)+);
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$($crate::build_grammar!(($ret) $builder; $($tail)*);)?
	};
	// (...)*, ...
	((proc, $ret:tt) $builder:expr; ($($items:tt)+)*, $($tail:tt)+) => {
		$builder.start_repeat(0);
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
	// (...)*;
	((proc, $ret:tt) $builder:expr; ($($items:tt)+)* $(; $($tail:tt)*)?) => {
		$builder.start_repeat(0);
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$($crate::build_grammar!(($ret) $builder; $($tail)*);)?
	};
	// (...)+, ...
	((proc, $ret:tt) $builder:expr; ($($items:tt)+)+, $($tail:tt)+) => {
		$builder.start_repeat(1);
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
	// (...)+;
	((proc, $ret:tt) $builder:expr; ($($items:tt)+)+ $(; $($tail:tt)*)?) => {
		$builder.start_repeat(1);
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$($crate::build_grammar!(($ret) $builder; $($tail)*);)?
	};
	// (...)?, ...
	((proc, $ret:tt) $builder:expr; ($($items:tt)+)?, $($tail:tt)+) => {
		$builder.start_optional();
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
	// (...)?;
	((proc, $ret:tt) $builder:expr; ($($items:tt)+)? $(; $($tail:tt)*)?) => {
		$builder.start_optional();
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$($crate::build_grammar!(($ret) $builder; $($tail)*);)?
	};
	// #x, ...
	((proc, $ret:tt) $builder:expr; #$proc_name:expr, $($tail:tt)+) => {
		$builder.proc($proc_name);
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
	// #x;
	((proc, $ret:tt) $builder:expr; #$proc_name:expr $(; $($tail:tt)*)?) => {
		$builder.proc($proc_name);
		$($crate::build_grammar!(($ret) $builder; $($tail)+);)?
	};
	// x, ...
	((proc, $ret:tt) $builder:expr; $token_name:expr, $($tail:tt)+) => {
		$builder.token($token_name);
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
	// x;
	((proc, $ret:tt) $builder:expr; $token_name:expr $(; $($tail:tt)*)?) => {
		$builder.token($token_name);
		$($crate::build_grammar!(($ret) $builder; $($tail)*);)?
	};
	
	((choice) $builder:expr; $($tail:tt)+) => {
		$builder.start_choice();
		$crate::build_grammar!((proc, choice_end) $builder; $($tail)+);
		$builder.end();
	};
	((choice_end) $builder:expr; $($tail:tt)+) => {
		$builder.next_choice_path();
		$crate::build_grammar!((proc, choice_end) $builder; $($tail)+)
	};

	((grammar) $builder:expr;) => {};
	// #!x => ...
	((grammar) $builder:expr; #!$proc_name:expr => $($tail:tt)+) => {
		$builder.define($proc_name, true);
		$crate::build_grammar!((proc, grammar) $builder; $($tail)+);
	};
	// #x => ...
	((grammar) $builder:expr; #$proc_name:expr => $($tail:tt)+) => {
		$builder.define($proc_name, false);
		$crate::build_grammar!((proc, grammar) $builder; $($tail)+);
	};

	($builder:expr; $($tail:tt)*) => {
		{
			$crate::build_grammar!((grammar) $builder; $($tail)*);
		}
	};
}

/// Create a [`Grammar`] by providing definitions for its procedures.
/// 
/// This macro uses the syntax
/// ```
/// # use sprout::prelude::*;
/// # use std::fmt;
/// #
/// # #[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// # enum Proc { Name }
/// # #[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// # enum Token { Name }
/// #
/// # impl fmt::Display for Token { fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "") } }
/// # impl fmt::Display for Proc { fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "") } }
/// #
/// grammar! {
/// 	#Proc::Name => Token::Name, #Proc::Name /*, ... */;
/// 	#!Proc::Name => Token::Name /*, ... */;
/// 	//...
/// };
/// ```
/// where procedure names are always prefixed with a `#`. 
/// 
/// Putting the `#!` prefix on a procedure definition means that that procedure is _primitive_.
/// _Primitive_ procedures are considered to be the **simple building blocks** of your language,
/// which means that error messages will refer to them by their name rather than their composite parts.
/// 
/// Beyond that, for more complex structures, you can use these special expressions in the procedure definitions:
/// 
/// | Syntax            | Description                                         |
/// |-------------------|-----------------------------------------------------|
/// | `(<A>)*`          | Repeat `<A>` zero or more times                     |
/// | `(<A>)+`          | Repeat `<A>` one or more times                      |
/// | `(<A>){<B>}*`     | Repeat `<A>` zero or more times, delimited by `<B>` |
/// | `(<A>){<B>}+`     | Repeat `<A>` one or more times, delimited by `<B>`  |
/// | `(<A>)?`          | `<A>` is optional                                   |
/// | `[<A>; <B>; ...]` | Select one of `<A>`, `<B>`, etc.                    |
/// 
/// # Examples
/// 
/// Defining a simple grammar
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
/// #[derive(Clone, Copy, Debug, Eq, PartialEq)]
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
#[macro_export]
macro_rules! grammar {
	($($tokens:tt)*) => {
		{
			let mut builder = $crate::parse::GrammarBuilder::new();
			$crate::build_grammar!(builder; $($tokens)*);
			builder.complete()
		}
	};
}

#[cfg(test)]
mod tests {
    use trees::tr;

    use crate::{tokenize::Token, TextPosition, ASTNode};

	#[test]
	fn should_allow_defining_procs() {
		let grammar = grammar! {
			#'a' => 'x', 'y'
		};

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
				Token::new('y', "456".to_string(), TextPosition::new(1, 3, 3))
			]),
			Ok(tr(ASTNode::new('a', "123456".to_string(), TextPosition::new(1, 0, 0))))
		);
	}

	#[test]
	#[should_panic(expected="Ambiguous proc primitivity: 'a' is defined as both primitive and non-primitive")]
	fn should_panic_when_defining_proc_as_primitive_and_non_primitive() {
		grammar! {
			#!'a' => 'x';
			#'a' => 'x';
		};
	}

	#[test]
	fn should_allow_defining_multiple_procs() {
		let grammar = grammar! {
			#'a' => 'x', 'y';
			#'b' => 'z', 'w';
		};

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
				Token::new('y', "456".to_string(), TextPosition::new(1, 3, 3))
			]),
			Ok(tr(ASTNode::new('a', "123456".to_string(), TextPosition::new(1, 0, 0))))
		);

		assert_eq!(
			grammar.parse('b', &[
				Token::new('z', "123".to_string(), TextPosition::new(1, 0, 0)),
				Token::new('w', "456".to_string(), TextPosition::new(1, 3, 3))
			]),
			Ok(tr(ASTNode::new('b', "123456".to_string(), TextPosition::new(1, 0, 0))))
		);
	}

	#[test]
	fn should_allow_defining_procs_with_procs() {
		let grammar = grammar! {
			#'a' => 'x', #'b';
			#'b' => 'y';
		};

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
				Token::new('y', "456".to_string(), TextPosition::new(1, 3, 3))
			]),
			Ok(
				tr(ASTNode::new('a', "123456".to_string(), TextPosition::new(1, 0, 0)))
					/ tr(ASTNode::new('b', "456".to_string(), TextPosition::new(1, 3, 3)))
			)
		);
	}

	#[test]
	fn should_allow_defining_repeats() {
		let grammar = grammar! {
			#'a' => ('x')*;
		};

		assert_eq!(
			grammar.parse('a', &[]),
			Ok(tr(ASTNode::new('a', "".to_string(), TextPosition::new(1, 0, 0))))
		);

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
				Token::new('x', "456".to_string(), TextPosition::new(1, 3, 3)),
				Token::new('x', "789".to_string(), TextPosition::new(1, 6, 6)),
			]),
			Ok(tr(ASTNode::new('a', "123456789".to_string(), TextPosition::new(1, 0, 0))))
		);
	}

	#[test]
	fn should_allow_defining_min_1_repeats() {
		let grammar = grammar! {
			#'a' => ('x')+;
		};

		assert!(grammar.parse('a', &[]).is_err());

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
				Token::new('x', "456".to_string(), TextPosition::new(1, 3, 3)),
				Token::new('x', "789".to_string(), TextPosition::new(1, 6, 6)),
			]),
			Ok(tr(ASTNode::new('a', "123456789".to_string(), TextPosition::new(1, 0, 0))))
		);
	}

	#[test]
	fn should_allow_defining_delimited_repeats() {
		let grammar = grammar! {
			#'a' => ('x'){' '}*;
		};

		assert_eq!(
			grammar.parse('a', &[]),
			Ok(tr(ASTNode::new('a', "".to_string(), TextPosition::new(1, 0, 0))))
		);

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
				Token::new(' ', "456".to_string(), TextPosition::new(1, 3, 3)),
				Token::new('x', "789".to_string(), TextPosition::new(1, 6, 6)),
			]),
			Ok(tr(ASTNode::new('a', "123456789".to_string(), TextPosition::new(1, 0, 0))))
		);
	}

	#[test]
	fn should_allow_defining_delimited_min_1_repeats() {
		let grammar = grammar! {
			#'a' => ('x'){' '}+;
		};

		assert!(grammar.parse('a', &[]).is_err());

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
				Token::new(' ', "456".to_string(), TextPosition::new(1, 3, 3)),
				Token::new('x', "789".to_string(), TextPosition::new(1, 6, 6)),
			]),
			Ok(tr(ASTNode::new('a', "123456789".to_string(), TextPosition::new(1, 0, 0))))
		);
	}

	#[test]
	fn should_allow_defining_optionals() {
		let grammar = grammar! {
			#'a' => 'x', ('y')?;
		};

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
			]),
			Ok(tr(ASTNode::new('a', "123".to_string(), TextPosition::new(1, 0, 0))))
		);

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
				Token::new('y', "456".to_string(), TextPosition::new(1, 3, 3)),
			]),
			Ok(tr(ASTNode::new('a', "123456".to_string(), TextPosition::new(1, 0, 0))))
		);
	}

	#[test]
	fn should_allow_defining_choices() {
		let grammar = grammar! {
			#'a' => ['x'; 'y'];
		};

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
			]),
			Ok(tr(ASTNode::new('a', "123".to_string(), TextPosition::new(1, 0, 0))))
		);

		assert_eq!(
			grammar.parse('a', &[
				Token::new('y', "123".to_string(), TextPosition::new(1, 0, 0)),
			]),
			Ok(tr(ASTNode::new('a', "123".to_string(), TextPosition::new(1, 0, 0))))
		);

		assert!(
			grammar.parse('a', &[
				Token::new('z', "123".to_string(), TextPosition::new(1, 0, 0)),
			]).is_err()
		);

		assert!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
				Token::new('x', "123".to_string(), TextPosition::new(1, 0, 0)),
			]).is_err()
		);
	}
}