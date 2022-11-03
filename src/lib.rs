#![allow(dead_code)]

pub mod compare;
pub mod fragments;
pub mod fragment_macros;
mod token_match;
pub mod tokenize;
pub mod grammar;

use std::fmt;
// use grammar::{Grammar, GrammarTreeNode};
use tokenize::{ /* Alphabet, */ TokenPosition};
// use trees::Tree;


#[derive(Debug, PartialEq, Eq)]
pub struct ParsingError {
	pub message: String,
	pub pos: TokenPosition
}

impl ParsingError {
	pub fn new(message: String, pos: TokenPosition) -> Self {
		ParsingError { message, pos }
	}
}

impl fmt::Display for ParsingError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "Parsing error: {} ({})", self.message, self.pos)
	}
}

// #[derive(Debug)]
// pub struct Parser<PN: Eq + Copy + fmt::Display + fmt::Debug, TN: Eq + Copy + fmt::Display + fmt::Debug> {
// 	alphabet: Alphabet<TN>,
// 	grammar: Grammar<PN, TN>
// }

// impl<PN: Eq + Copy + fmt::Display + fmt::Debug, TN: Eq + Copy + fmt::Display + fmt::Debug> Parser<PN, TN> {
// 	pub fn new(alphabet: Alphabet<TN>, grammar: Grammar<PN, TN>) -> Self {
// 		Parser { alphabet, grammar }
// 	}

// 	pub fn parse(&self, proc: PN, text: String) -> Result<Tree<GrammarTreeNode<PN, TN>>, ParsingError> {
// 		let tokens = self.alphabet.tokenize(text)?;
// 		self.grammar.parse(proc, &tokens)
// 	}
// }