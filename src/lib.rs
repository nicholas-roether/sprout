#![allow(dead_code)]

pub mod compare;
pub mod fragments;
pub mod fragment_macros;
pub mod tokenize;
pub mod grammar;

mod token_match;

use std::fmt;
use grammar::{Grammar, GrammarTreeNode};
use tokenize::Alphabet;
use trees::Tree;

#[derive(Debug)]
pub struct Parser<PN: Eq + Copy + fmt::Display + fmt::Debug, TN: Eq + Copy + fmt::Display + fmt::Debug> {
	alphabet: Alphabet<TN>,
	grammar: Grammar<PN, TN>
}

impl<PN: Eq + Copy + fmt::Display + fmt::Debug, TN: Eq + Copy + fmt::Display + fmt::Debug> Parser<PN, TN> {
	pub fn new(alphabet: Alphabet<TN>, grammar: Grammar<PN, TN>) -> Self {
		Parser { alphabet, grammar }
	}

	pub fn parse(&self, proc: PN, text: String) -> Result<Tree<GrammarTreeNode<PN, TN>>, String> {
		let tokens = self.alphabet.tokenize(text)?;
		self.grammar.parse(proc, tokens)
	}
}