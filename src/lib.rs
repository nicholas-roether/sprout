#![allow(dead_code)]

mod token_match;
mod compare;

pub mod tokenize;
pub mod parse;
pub mod prelude;

use std::fmt;
use std::hash::Hash;
use parse::Grammar;
use tokenize::Alphabet;
use trees::Tree;

/// A position in the text.
/// 
/// Points to a specific `line` and `char` (index in that line).
/// `line` starts at 1, and `char` at 0.
/// 
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TextPosition {
	/// The line of text this position points to.
	/// 
	/// **Important**: The first line has line number 1, _not_ 0.
    pub line: usize,
	/// The index of the character this position points to within the line.
    pub char: usize,
	/// The global index of the character this position points to within the text.
	pub index: usize
}

impl TextPosition {
	/// Constructs a new `TextPosition`.
	/// 
	/// **Important**: While this constructor won't prevent you from doing it,
	/// you shouldn't enter 0 as a value for `line`. The `line` value of a text position
	/// is usually assumed to start at 1.
    pub fn new(line: usize, char: usize, index: usize) -> Self {
        TextPosition { line, char, index }
    }
}

impl fmt::Display for TextPosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.char)
    }
}

/// A node in the [`AST`]. It represents an occurrence of one of the procedures
/// from the grammar in the text. As such, it has three properties:
/// 
/// - `proc`: The procedure that this node represents
/// - `text`: The exact string that was matched in the text to produce the node ([`String`])
/// - `pos`: The position of the beginning of this node's text segment in the text ([`TextPosition`])
/// 
#[derive(Debug, PartialEq, Clone)]
pub struct ASTNode<PN: fmt::Debug + PartialEq + Copy> {
	/// The procedure that this node represents
	pub proc: PN,
	/// The exact string that was matched in the text to produce this node
	pub text: String,
	/// The position of the beginning of this node's text segment in the text
	pub pos: TextPosition
}

impl<PN: fmt::Debug + PartialEq + Copy> ASTNode<PN> {
	/// Constructs a new [`ASTNode`].
	/// 
	/// See also: [`TextPosition`]
	/// 
	/// # Examples
	/// 
	/// Basic usage
	/// ```
	/// # use sprout::prelude::*;
	/// #
	/// # #[derive(std::fmt::Debug, PartialEq, Clone, Copy)]
	/// # enum Proc { SomeProcName }
	/// #
	/// ASTNode::new(Proc::SomeProcName, "abc".to_string(), TextPosition::new(6, 9, 123));
	/// ```
	pub fn new(proc: PN, text: String, pos: TextPosition) -> Self {
		ASTNode { proc, text, pos }
	}
}

impl<PN: fmt::Debug + fmt::Display + PartialEq + Copy> fmt::Display for ASTNode<PN> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{:?}[\"{}\"]", self.proc, self.text)
	}
}

/// An AST (abstract syntax tree). Its nodes and represent occurrences of grammar procedures
/// in the parsed text.
/// 
/// `AST` is a type alias for a [`trees::Tree`] of [`ASTNode`]s.
pub type AST<PN> = Tree<ASTNode<PN>>;

/// An error object returned when a part of the parsing process failed.
/// It contains `message`, a [`String`] that describes what went wrong, 
/// and `pos`, a [`TextPosition`] that points to the problematic location in the text
/// whose parsing was attempted.
#[derive(Debug, PartialEq, Eq)]
pub struct ParsingError {
	pub message: String,
	pub pos: TextPosition,
	pub source: Option<String>
}

impl ParsingError {
	/// Constructs a new [`ParsingError`].
	/// 
	/// See also: [`TextPosition`]
	/// 
	/// # Examples
	/// 
	/// Basic usage
	/// ```
	/// # use sprout::prelude::*;
	/// ParsingError::new("something went wrong!".to_string(), TextPosition::new(2, 3, 5), None);
	/// ```
	pub fn new(message: String, pos: TextPosition, source: Option<String>) -> Self {
		ParsingError { message, pos, source }
	}

	fn find_src_offset_index(&self, source: &String, start: isize, incr: isize, max: isize) -> (usize, bool) {
		let mut offset = start;
		let mut ellipsis = false;
		loop {
			let char = source.chars().nth((self.pos.index as isize + offset) as usize);
			if char.is_none() || char == Some('\n') {
				offset -= incr;
				break;
			}

			offset += incr;
			if offset == max {
				ellipsis = true;
				break;
			}
		}
		((self.pos.index as isize + offset) as usize, ellipsis)
	}

	fn format_source_pointer(&self, f: &mut fmt::Formatter<'_>, source: &String) -> fmt::Result {
		let (first_index, start_ellipsis) = self.find_src_offset_index(source, -1, -1, -20);
		let (last_index, end_ellipsis) = self.find_src_offset_index(source, 0, 1, 19);

		if start_ellipsis { write!(f, "...")?; }
		write!(f, "{}", &source[first_index .. last_index + 1])?;
		if end_ellipsis { write!(f, "...")?; }
		writeln!(f)?;

		let mut pointer_offset = self.pos.index - first_index;
		if start_ellipsis { pointer_offset += 3 }

		write!(f, "{}", " ".repeat(pointer_offset))?;
		writeln!(f, "^")
	}
}

impl fmt::Display for ParsingError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "Parsing error: {} ({})", self.message, self.pos)?;
		if let Some(source) = &self.source {
			writeln!(f)?;
			self.format_source_pointer(f, source)?;
		}
		Ok(())
	}
}

/// The `Parser` is instantiated with an [`Alphabet`] and [`Grammar`], and provides the
/// `parse` method to directly compile a [`String`] to an [`AST`].
/// 
/// # Examples
/// 
/// Simple example
/// ```
/// # use sprout::prelude::*;
/// #
/// // Define an enum for our tokens
/// #[derive(Clone, Copy, Debug, Eq, PartialEq)]
/// enum Token { X }
///
/// // Define an enum for our procedures
/// #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
/// enum Proc { AllTheX }
/// 
/// // Implement std::fmt::Display for our two enums for error message generation
/// impl std::fmt::Display for Token {
/// 	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
/// 		match self {
/// 			Self::X => write!(f, "x")
/// 		}
/// 	}
/// }
/// 
/// impl std::fmt::Display for Proc {
/// 	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
/// 		match self {
/// 			Self::AllTheX => write!(f, "all the x!")
/// 		}
/// 	}
/// }
///
/// // import our tokens for convenience (this is not required)
/// use Token::*;
/// use Proc::*;
/// 
/// // Define alphabet and grammar and create parser
/// let parser = Parser::new(
/// 	alphabet! {
/// 		X => "x"
/// 	},
/// 	grammar! {
/// 		#AllTheX => (X)*
/// 	}
/// );
/// 
/// // Parse a test string
/// let result = parser.parse(AllTheX, "xxxxxxxx".to_string());
/// assert!(result.is_ok())
/// 
/// ```
/// 
/// For a detailed explanation on how to define alphabets and grammars, see [`alphabet`] and [`grammar`].
#[derive(Debug)]
pub struct Parser<PN: Eq + Hash + Copy + fmt::Display + fmt::Debug, TN: Eq + Copy + fmt::Display + fmt::Debug> {
	alphabet: Alphabet<TN>,
	grammar: Grammar<PN, TN>
}

impl<PN: Eq + Hash + Copy + fmt::Display + fmt::Debug, TN: Eq + Copy + fmt::Display + fmt::Debug> Parser<PN, TN> {
	/// Constructs a new parser from an [`Alphabet`] and a [`Grammar`].
	/// 
	/// For a complete usage example, see [`Parser`].
	/// 
	/// See also: [`Alphabet`], [`Grammar`].
	pub fn new(alphabet: Alphabet<TN>, grammar: Grammar<PN, TN>) -> Self {
		Parser { alphabet, grammar }
	}

	/// Parse a [`String`], according to some `proc` defined in the grammar of this parser,
	/// to an [`AST`].
	/// 
	/// Returns a `Result` with [`ParsingError`] as the error type.
	/// 
	/// # Examples
	/// 
	/// Parsing a string
	/// ```
	/// # use sprout::prelude::*;
	/// # use std::fmt::{Display, Formatter, Result};
	/// #
	/// # #[derive(Clone, Copy, Debug, Eq, PartialEq)]
	/// # enum Token { Number }
	/// #
	/// # impl Display for Token {
	/// # 	fn fmt(&self, f: &mut Formatter<'_>) -> Result { write!(f, "number") }
	/// # }
	/// # #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
	/// # enum Proc { Number }
	/// #
	/// # impl Display for Proc {
	/// # 	fn fmt(&self, f: &mut Formatter<'_>) -> Result { write!(f, "number") }
	/// # }
	/// #
	/// # let parser = Parser::new(alphabet!(Token::Number => "[0-9]+"), grammar!(#Proc::Number => Token::Number));
	/// #
	/// // Given the grammar contains a procedure Proc::Number
	/// let result = parser.parse(Proc::Number, "3453".to_string());
	/// ```
	/// 
	/// For more details, see [`Parser`]
	pub fn parse(&self, proc: PN, text: String) -> Result<AST<PN>, ParsingError> {
		let result = self.alphabet.tokenize(text.clone()).and_then(|tokens| {
			self.grammar.parse(proc, &tokens)
		});
		match result {
			Ok(ast) => Ok(ast),
			Err(mut error) => {
				error.source = Some(text);
				Err(error)
			}
		}
	}
}

#[cfg(test)]
mod tests {
    use crate::{ParsingError, TextPosition};

	#[test]
	fn parsing_error_with_small_char_should_format_properly() {
		let parsing_error = ParsingError::new(
			"Error happened here".to_string(),
			TextPosition::new(2, 1, 5),
			Some("123\n456789\nsgfde".to_string())
		);

		assert_eq!(parsing_error.to_string(), "Parsing error: Error happened here (2:1)\n\n456789\n ^\n");
	}

	#[test]
	fn parsing_error_should_format_properly() {
		let parsing_error = ParsingError::new(
			"Error happened here".to_string(),
			TextPosition::new(2, 5, 9),
			Some("123\n456789\nsgfde".to_string())
		);

		assert_eq!(parsing_error.to_string(), "Parsing error: Error happened here (2:5)\n\n456789\n     ^\n");
	}

	#[test]
	fn parsing_error_should_format_ellipses_properly() {
		let parsing_error = ParsingError::new(
			"Error happened here".to_string(),
			TextPosition::new(2, 43, 48),
			Some("123\naaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa456789aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\nsgfde".to_string())
		);

		assert_eq!(parsing_error.to_string(), "Parsing error: Error happened here (2:43)\n\n...aaaaaaaaaaaaaaaa456789aaaaaaaaaaaaaaaaaa...\n                       ^\n");
	}
}