use std::fmt;

use crate::compare::MatchGraphBuilder;

use super::{GrammarItemName, GrammarProc, Grammar};

pub enum GrammarProcTemplate<PN, TN> {
	Token(TN),
	Proc(PN),
	Sequence(Vec<GrammarProcTemplate<PN, TN>>),
	Choice(Vec<GrammarProcTemplate<PN, TN>>),
	Repeat(u32, Box<GrammarProcTemplate<PN, TN>>),
	Option(Box<GrammarProcTemplate<PN, TN>>)
}

impl<PN: PartialEq + Copy + fmt::Display, TN: PartialEq + Copy + fmt::Display> GrammarProcTemplate<PN, TN> {
	pub fn resolve(&self, name: PN) -> GrammarProc<PN, TN> {
		let mut graph_builder = MatchGraphBuilder::new();
		self.resolve_part(&mut graph_builder);
		GrammarProc::new(name, graph_builder.complete())
	}

	fn resolve_part(&self, graph_builder: &mut MatchGraphBuilder<GrammarItemName<PN, TN>>){
		match self {
			Self::Token(token_name) => graph_builder.append(GrammarItemName::Terminal(*token_name), None),
			Self::Proc(proc_name) => graph_builder.append(GrammarItemName::NonTerminal(*proc_name), None),
			Self::Sequence(items) => {
				for item in items { item.resolve_part(graph_builder); }
			},
			Self::Choice(options) => {
				graph_builder.push_return();
				for option in options {
					option.resolve_part(graph_builder);
					graph_builder.end_choice_path();
				}
				graph_builder.pop_choice();
			},
			Self::Repeat(min, item) => {
				graph_builder.push_return();
				item.resolve_part(graph_builder);
				graph_builder.duplicate(*min);
				graph_builder.pop_repeat();
			},
			Self::Option(item) => {
				graph_builder.push_return();
				item.resolve_part(graph_builder);
				graph_builder.pop_optional();
			}
		}
	}
}

pub struct GrammarTemplate<PN, TN> {
	proc_templates: Vec<(PN, GrammarProcTemplate<PN, TN>)>
}

impl<PN: PartialEq + Copy + fmt::Display, TN: PartialEq + Copy + fmt::Display> GrammarTemplate<PN, TN> {
	pub fn new(proc_templates: Vec<(PN, GrammarProcTemplate<PN, TN>)>) -> Self {
		GrammarTemplate { proc_templates }
	}

	pub fn resolve(&self) -> Grammar<PN, TN> {
		Grammar::new(self.proc_templates.iter().map(|(name, template)| template.resolve(*name)).collect())
	}
}