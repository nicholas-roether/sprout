#[macro_export]
macro_rules! repeat {
	($($part:expr),+) => {
		$crate::fragments::RepeatFragment::new(Box::new($crate::sequence!($($part),+)), 0, None)
	};
	($($part:expr),+; $min:expr) => {
		$crate::fragments::RepeatFragment::new(Box::new($crate::sequence!($($part),+)), $min, None)
	};
	($($part:expr),+; $min:expr; $max:expr) => {
		$crate::fragments::RepeatFragment::new(Box::new($crate::sequence!($($part),+)), $min, Some($max))
	};
}

#[macro_export]
macro_rules! choice {
	($($($part:expr),+);*) => {
		$crate::fragments::ChoiceFragment::new(
			vec![$(Box::new($crate::sequence!($($part),+))),*]
		)
	};
}

#[macro_export]
macro_rules! optional {
	($($part:expr),+) => {
		$crate::fragments::RepeatFragment::new(Box::new($crate::sequence!($($part),+)), 0, Some(1))
	};
}

#[macro_export]
macro_rules! sequence {
	($part:expr) => {
		$part
	};
	($($part:expr),+) => {
		$crate::fragments::SequenceFragment::new(vec![
			$(Box::new($part)),+
		])
	}
}