use std::fmt;

use crate::compare::MatchGraphBuilder;

use super::{GrammarProc, Grammar, GrammarItemName};

#[derive(Debug, PartialEq, Eq)]
enum GrammarBuilderState {
	Repeat(u32),
	Optional,
	Choice
}

#[derive(Debug)]
pub struct GrammarBuilder<PN: PartialEq + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Debug + fmt::Display> {
	procs: Vec<GrammarProc<PN, TN>>,
	current_proc: Option<(PN, MatchGraphBuilder<GrammarItemName<PN, TN>>)>,
	state_stack: Vec<GrammarBuilderState>
}

impl<PN: PartialEq + Copy + fmt::Debug + fmt::Display, TN: PartialEq + Copy + fmt::Debug + fmt::Display> GrammarBuilder<PN, TN> {
	pub fn new() -> Self {
		GrammarBuilder { procs: vec![], current_proc: None, state_stack: vec![] }
	}

	pub fn define(&mut self, proc: PN) {
		self.complete_proc();
		self.current_proc = Some((proc, MatchGraphBuilder::new()))
	}
	
	pub fn token(&mut self, token: TN) {
		self.get_proc_builder().append(GrammarItemName::Terminal(token), None);
	}

	pub fn proc(&mut self, proc: PN) {
		self.get_proc_builder().append(GrammarItemName::NonTerminal(proc), None);
	}

	pub fn start_repeat(&mut self, min: u32) {
		self.get_proc_builder().push_return();
		self.state_stack.push(GrammarBuilderState::Repeat(min));
	}

	pub fn start_optional(&mut self) {
		self.get_proc_builder().push_return();
		self.state_stack.push(GrammarBuilderState::Optional);
	}

	pub fn start_choice(&mut self) {
		self.get_proc_builder().push_return();
		self.state_stack.push(GrammarBuilderState::Choice);
	}

	pub fn next_choice_path(&mut self) {
		if self.state_stack.last() != Some(&GrammarBuilderState::Choice) {
			panic!("Invalid operation; not within a choice definition");
		}
		self.get_proc_builder().end_choice_path();
	}

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

	pub fn complete(mut self) -> Grammar<PN, TN> {
		self.complete_proc();
		Grammar::new(self.procs)
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

#[macro_export]
macro_rules! build_grammar {
	((proc, $ret:tt) $builder:expr; [$($items:tt)+], $($tail:tt)+) => {
		$builder.start_choice();
		$crate::build_grammar!((choice) $builder; $($items)+);
		$builder.end();
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
	((proc, $ret:tt) $builder:expr; [$($items:tt)+] $(; $($tail:tt)*)?) => {
		$builder.start_repeat(0);
		$crate::build_grammar!((choice) $builder; $($items)+);
		$builder.end();
		$($crate::build_grammar!(($ret) $builder; $($tail)*);)?
	};
	((proc, $ret:tt) $builder:expr; ($($items:tt)+)*, $($tail:tt)+) => {
		$builder.start_repeat(0);
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
	((proc, $ret:tt) $builder:expr; ($($items:tt)+)* $(; $($tail:tt)*)?) => {
		$builder.start_repeat(0);
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$($crate::build_grammar!(($ret) $builder; $($tail)*);)?
	};
	((proc, $ret:tt) $builder:expr; ($($items:tt)+)+, $($tail:tt)+) => {
		$builder.start_repeat(1);
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
	((proc, $ret:tt) $builder:expr; ($($items:tt)+)+ $(; $($tail:tt)*)?) => {
		$builder.start_repeat(1);
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$($crate::build_grammar!(($ret) $builder; $($tail)*);)?
	};
	((proc, $ret:tt) $builder:expr; ($($items:tt)+)?, $($tail:tt)+) => {
		$builder.start_optional();
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
	((proc, $ret:tt) $builder:expr; ($($items:tt)+)? $(; $($tail:tt)*)?) => {
		$builder.start_optional();
		$crate::build_grammar!((proc, $ret) $builder; $($items)+);
		$builder.end();
		$($crate::build_grammar!(($ret) $builder; $($tail)*);)?
	};
	((proc, $ret:tt) $builder:expr; #$proc_name:expr, $($tail:tt)+) => {
		$builder.proc($proc_name);
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
	((proc, $ret:tt) $builder:expr; #$proc_name:expr $(; $($tail:tt)*)?) => {
		$builder.proc($proc_name);
		$($crate::build_grammar!(($ret) $builder; $($tail)+);)?
	};
	((proc, $ret:tt) $builder:expr; $token_name:expr, $($tail:tt)+) => {
		$builder.token($token_name);
		$crate::build_grammar!((proc, $ret) $builder; $($tail)+);
	};
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
	((grammar) $builder:expr; #$proc_name:expr => $($tail:tt)+) => {
		$builder.define($proc_name);
		$crate::build_grammar!((proc, grammar) $builder; $($tail)+);
	};

	($builder:expr; $($tail:tt)*) => {
		{
			$crate::build_grammar!((grammar) $builder; $($tail)*);
		}
	};
}

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
				Token::new('x', "123".to_string(), TextPosition::new(1, 0)),
				Token::new('y', "456".to_string(), TextPosition::new(1, 3))
			]),
			Ok(tr(ASTNode::new('a', "123456".to_string(), TextPosition::new(1, 0))))
		);
	}

	#[test]
	fn should_allow_defining_multiple_procs() {
		let grammar = grammar! {
			#'a' => 'x', 'y';
			#'b' => 'z', 'w';
		};

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0)),
				Token::new('y', "456".to_string(), TextPosition::new(1, 3))
			]),
			Ok(tr(ASTNode::new('a', "123456".to_string(), TextPosition::new(1, 0))))
		);

		assert_eq!(
			grammar.parse('b', &[
				Token::new('z', "123".to_string(), TextPosition::new(1, 0)),
				Token::new('w', "456".to_string(), TextPosition::new(1, 3))
			]),
			Ok(tr(ASTNode::new('b', "123456".to_string(), TextPosition::new(1, 0))))
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
				Token::new('x', "123".to_string(), TextPosition::new(1, 0)),
				Token::new('y', "456".to_string(), TextPosition::new(1, 3))
			]),
			Ok(
				tr(ASTNode::new('a', "123456".to_string(), TextPosition::new(1, 0)))
					/ tr(ASTNode::new('b', "456".to_string(), TextPosition::new(1, 3)))
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
			Ok(tr(ASTNode::new('a', "".to_string(), TextPosition::new(1, 0))))
		);

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0)),
				Token::new('x', "456".to_string(), TextPosition::new(1, 3)),
				Token::new('x', "789".to_string(), TextPosition::new(1, 6)),
			]),
			Ok(tr(ASTNode::new('a', "123456789".to_string(), TextPosition::new(1, 0))))
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
				Token::new('x', "123".to_string(), TextPosition::new(1, 0)),
				Token::new('x', "456".to_string(), TextPosition::new(1, 3)),
				Token::new('x', "789".to_string(), TextPosition::new(1, 6)),
			]),
			Ok(tr(ASTNode::new('a', "123456789".to_string(), TextPosition::new(1, 0))))
		);
	}

	#[test]
	fn should_allow_defining_optionals() {
		let grammar = grammar! {
			#'a' => 'x', ('y')?;
		};

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0)),
			]),
			Ok(tr(ASTNode::new('a', "123".to_string(), TextPosition::new(1, 0))))
		);

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0)),
				Token::new('y', "456".to_string(), TextPosition::new(1, 3)),
			]),
			Ok(tr(ASTNode::new('a', "123456".to_string(), TextPosition::new(1, 0))))
		);
	}

	#[test]
	fn should_allow_defining_choices() {
		let grammar = grammar! {
			#'a' => ['x'; 'y'];
		};

		assert_eq!(
			grammar.parse('a', &[
				Token::new('x', "123".to_string(), TextPosition::new(1, 0)),
			]),
			Ok(tr(ASTNode::new('a', "123".to_string(), TextPosition::new(1, 0))))
		);

		assert_eq!(
			grammar.parse('a', &[
				Token::new('y', "123".to_string(), TextPosition::new(1, 0)),
			]),
			Ok(tr(ASTNode::new('a', "123".to_string(), TextPosition::new(1, 0))))
		);

		assert!(
			grammar.parse('a', &[
				Token::new('z', "123".to_string(), TextPosition::new(1, 0)),
			]).is_err()
		);
	}
}