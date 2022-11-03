#[macro_export]
macro_rules! token {
	($name:expr) => {
		$crate::grammar::GrammarProcTemplate::Token($name)
	};
}

#[macro_export]
macro_rules! proc {
	($name:expr) => {
		$crate::grammar::GrammarProcTemplate::Proc($name)
	};
}

#[macro_export]
macro_rules! choice {
	($($($part:expr),+);+) => {
		$crate::grammar::GrammarProcTemplate::Choice(vec![
			$($crate::sequence!($($part),+)),+
		])
	};
}

#[macro_export]
macro_rules! option {
	($($part:expr),+) => {
		$crate::grammar::GrammarProcTemplate::Option(Box::new(
			$crate::sequence!($($part),+)
		))
	};
}

#[macro_export]
macro_rules! repeat {
	($($part:expr),+; $min:expr) => {
		$crate::grammar::GrammarProcTemplate::Repeat($min, Box::new(
			$($crate::sequence!($($part),+)),+
		))
	};
	($($part:expr),+) => {
		$crate::grammar::GrammarProcTemplate::Repeat(0, Box::new(
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
		$crate::grammar::GrammarProcTemplate::Sequence(vec![$($part),+])
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
