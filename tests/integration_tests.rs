use std::fmt;

use sprout::{alphabet, grammar, ASTNode, token, option, choice, repeat, proc, Parser, TextPosition};
use trees::tr;

#[test]
pub fn example_from_readme() {
	#[derive(Debug, PartialEq, Eq, Clone, Copy)]
	pub enum Token {
		Number,
		Word,
		Space
	}
	
	impl fmt::Display for Token {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			match self {
				Self::Number => write!(f, "number")?,
				Self::Word => write!(f, "word")?,
				Self::Space => write!(f, "space")?
			}
			Ok(())
		}
	}

	#[derive(Debug, PartialEq, Eq, Clone, Copy)]
	pub enum Proc {
		TwoOrThreeWords,
		WordOrNumber,
		Sequence
	}

	impl fmt::Display for Proc {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			match self {
				Self::TwoOrThreeWords => write!(f, "two or three words")?,
				Self::WordOrNumber => write!(f, "word or number")?,
				Self::Sequence => write!(f, "word/number sequence")?
			}
			Ok(())
		}
	}

	let alphabet = alphabet! {
		Token::Number => "[0-9]+";
		Token::Word => "[a-z]+";
		Token::Space => " "
	};

	let grammar = grammar! {
		Proc::TwoOrThreeWords =>
			token!(Token::Word),
			token!(Token::Space),
			token!(Token::Word),
			option!(
				token!(Token::Space),
				token!(Token::Word)
			);
		Proc::WordOrNumber =>
			choice!(
				token!(Token::Word);
				token!(Token::Number)
			);
		Proc::Sequence =>
			repeat!(
				choice!(
					proc!(Proc::TwoOrThreeWords);
					proc!(Proc::WordOrNumber)
				),
				token!(Token::Space)
			),
			choice!(
				proc!(Proc::TwoOrThreeWords);
				proc!(Proc::WordOrNumber)
			);
	};

	let parser = Parser::new(alphabet, grammar);

	let tree = parser.parse(Proc::Sequence, "abc ab 123 xyz 69".to_string());

	assert_eq!(
		tree,
		Ok(
			tr(ASTNode::new(Proc::Sequence, "abc ab 123 xyz 69".to_string(), TextPosition::new(1, 0)))
				/ tr(ASTNode::new(Proc::TwoOrThreeWords, "abc ab".to_string(), TextPosition::new(1, 0)))
				/ tr(ASTNode::new(Proc::WordOrNumber, "123".to_string(), TextPosition::new(1, 7)))
				/ tr(ASTNode::new(Proc::WordOrNumber, "xyz".to_string(), TextPosition::new(1, 11)))
				/ tr(ASTNode::new(Proc::WordOrNumber, "69".to_string(), TextPosition::new(1, 15)))
		)
	)
}