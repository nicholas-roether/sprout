#[macro_export]
macro_rules! token {
	($name:expr) => {
		$crate::parse::GrammarProcTemplate::Token($name)
	};
}

#[macro_export]
macro_rules! proc {
	($name:expr) => {
		$crate::parse::GrammarProcTemplate::Proc($name)
	};
}

#[macro_export]
macro_rules! choice {
	($($($part:expr),+);+) => {
		$crate::parse::GrammarProcTemplate::Choice(vec![
			$($crate::sequence!($($part),+)),+
		])
	};
}

#[macro_export]
macro_rules! option {
	($($part:expr),+) => {
		$crate::parse::GrammarProcTemplate::Option(Box::new(
			$crate::sequence!($($part),+)
		))
	};
}

#[macro_export]
macro_rules! repeat {
	($($part:expr),+; $min:expr) => {
		$crate::parse::GrammarProcTemplate::Repeat($min, Box::new(
			$($crate::sequence!($($part),+)),+
		))
	};
	($($part:expr),+) => {
		$crate::parse::GrammarProcTemplate::Repeat(0, Box::new(
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
		$crate::parse::GrammarProcTemplate::Sequence(vec![$($part),+])
	};
}

#[macro_export]
macro_rules! grammar {
	($($proc_name:expr => $($part:expr),+);*$(;)?) => {
		{
			let template = $crate::parse::GrammarTemplate::new(vec![
				$(($proc_name, $crate::sequence!($($part),+))),+
			]);
			template.resolve()
		}
	};
}
