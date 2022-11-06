use std::fmt;

use sprout::prelude::*;

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
				Self::Number => write!(f, "number"),
				Self::Word => write!(f, "word"),
				Self::Space => write!(f, "space")
			}
		}
	}

	#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
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

	use Token::*;
	use Proc::*;

	let alphabet = alphabet! {
		Number => "[0-9]+";
		Word => "[a-z]+";
		Space => " "
	};

	let grammar = grammar! {
		#TwoOrThreeWords => Word, Space, Word, (Space, Word)?;
		#WordOrNumber => [Word; Number];
		#Sequence => ([#TwoOrThreeWords; #WordOrNumber]){Space}+;
	};

	let parser = Parser::new(alphabet, grammar);

	let tree = parser.parse(Proc::Sequence, "abc ab 123 xyz 69".to_string());

	println!("{}", tree.as_ref().unwrap());

	assert_eq!(
		tree.unwrap().to_string(),
		r#"Sequence["abc ab 123 xyz 69"]( TwoOrThreeWords["abc ab"] WordOrNumber["123"] WordOrNumber["xyz"] WordOrNumber["69"] )"#
	)
}