#![allow(dead_code)]

use std::fmt;

use grammar::{Grammar, GrammarTreeNode};
use tokenize::Alphabet;
use trees::Tree;

mod fragments;

pub mod fragment_macros;

mod token_match;

pub mod tokenize;

pub mod grammar;

pub struct Parser<PN: Eq + Copy + fmt::Display, TN: Eq + Copy + fmt::Display> {
	alphabet: Alphabet<TN>,
	grammar: Grammar<PN, TN>
}

impl<PN: Eq + Copy + fmt::Display, TN: Eq + Copy + fmt::Display> Parser<PN, TN> {
	pub fn new(alphabet: Alphabet<TN>, grammar: Grammar<PN, TN>) -> Self {
		Parser { alphabet, grammar }
	}

	pub fn parse(&self, proc: PN, text: String) -> Result<Tree<GrammarTreeNode<PN, TN>>, String> {
		let tokens = self.alphabet.tokenize(text)?;
		self.grammar.parse(proc, tokens)
	}
}