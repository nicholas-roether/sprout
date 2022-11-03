#[macro_export]
macro_rules! token {
	($name:expr) => {
		|graph_builder: &mut $crate::compare::MatchGraphBuilder<_>| {
			graph_builder.append(GrammarItemName::Terminal($name), None);
		}
	};
}

#[macro_export]
macro_rules! proc {
	($name:expr) => {
		|graph_builder: &mut $crate::compare::MatchGraphBuilder<_>| {
			graph_builder.append(GrammarItemName::NonTerminal($name), None);
		}
	};
}

#[macro_export]
macro_rules! grammar_proc {
	($proc_name:expr; $($part:expr),+) => {
		{
			let mut graph_builder = $crate::compare::MatchGraphBuilder::new();

			$(
				$part(&mut graph_builder);
			)+

			GrammarProc::new($proc_name, graph_builder.complete())
		}
	}
}

#[macro_export]
macro_rules! grammar {
	($($proc_name:expr => $($part:expr),+);*$(;)?) => {
		Grammar::new(vec![
			$(
				$crate::grammar_proc!($proc_name; $($part),+)
			),+
		])
	};
}
