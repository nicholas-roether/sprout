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
macro_rules! choice {
	($($($part:expr),+);+) => {
		GrammarProcTemplate::Choice(vec![
			$($crate::sequence!($($part),+)),+
		])
	};
}

#[macro_export]
macro_rules! option {
	($($part:expr),+) => {
		GrammarProcTemplate::Choice(Box::new(
			$crate::sequence!($($part),+)
		))
	};
}

#[macro_export]
macro_rules! repeat {
	($($part:expr),+; $min:expr) => {
		GrammarProcTemplate::Repeat($min, Box::new(
			$($crate::sequence!($($part),+)),+
		))
	};
	($($part:expr),+) => {
		GrammarProcTemplate::Repeat(0, Box::new(
			$crate::sequence!($($part),+)
		))
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
