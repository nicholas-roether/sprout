use std::fmt::Debug;

use trees::Tree;

use crate::{fragment::Fragment, tokenize::{Token, TokenPosition}};

enum GrammarItemName<ProcName, TokenName> {
	Terminal(TokenName),
	NonTerminal(ProcName)
}

trait GrammarPart {
	
}

struct GrammarProdedure {
	
}