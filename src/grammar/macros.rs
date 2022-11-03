#[macro_export]
macro_rules! token {
	($name:expr) => {
		GrammarProcTemplate::Token($name)
	};
}

#[macro_export]
macro_rules! proc {
	($name:expr) => {
		GrammarProcTemplate::Proc($name)
	};
}

#[macro_export]
macro_rules! sequence {
	($part:expr) => {
		$part
	};
	($($part:expr),+) => {
		GrammarProcTemplate::Sequence(vec![$($part),+])
	};
}

#[macro_export]
macro_rules! grammar {
	($($proc_name:expr => $($part:expr),+);*$(;)?) => {
		{
			let template = $crate::grammar::GrammarTemplate::new(vec![
				$(($proc_name, $crate::sequence!($($part),+))),+
			]);
			template.resolve()
		}
	};
}
