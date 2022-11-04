#![allow(dead_code)]

mod token_match;
mod compare;

pub mod tokenize;
pub mod parse;

use std::fmt;
use parse::Grammar;
use tokenize::Alphabet;
use trees::Tree;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextPosition {
    pub line: usize,
    pub char: usize
}

impl TextPosition {
    pub fn new(line: usize, char: usize) -> Self {
        TextPosition { line, char }
    }
}

impl fmt::Display for TextPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.char)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ASTNode<PN: fmt::Debug + PartialEq + Copy> {
	pub proc: PN,
	pub text: String,
	pub pos: TextPosition
}

impl<PN: fmt::Debug + PartialEq + Copy> ASTNode<PN> {
	pub fn new(proc: PN, text: String, pos: TextPosition) -> Self {
		ASTNode { proc, text, pos }
	}
}

pub type AST<PN> = Tree<ASTNode<PN>>;

#[derive(Debug, PartialEq, Eq)]
pub struct ParsingError {
	pub message: String,
	pub pos: TextPosition
}

impl ParsingError {
	pub fn new(message: String, pos: TextPosition) -> Self {
		ParsingError { message, pos }
	}
}

impl fmt::Display for ParsingError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "Parsing error: {} ({})", self.message, self.pos)
	}
}

#[derive(Debug)]
pub struct Parser<PN: Eq + Copy + fmt::Display + fmt::Debug, TN: Eq + Copy + fmt::Display + fmt::Debug> {
	alphabet: Alphabet<TN>,
	grammar: Grammar<PN, TN>
}

impl<PN: Eq + Copy + fmt::Display + fmt::Debug, TN: Eq + Copy + fmt::Display + fmt::Debug> Parser<PN, TN> {
	pub fn new(alphabet: Alphabet<TN>, grammar: Grammar<PN, TN>) -> Self {
		Parser { alphabet, grammar }
	}

	pub fn parse(&self, proc: PN, text: String) -> Result<AST<PN>, ParsingError> {
		let tokens = self.alphabet.tokenize(text)?;
		self.grammar.parse(proc, &tokens)
	}
}