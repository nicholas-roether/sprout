pub enum GrammarProcTemplate<PN, TN> {
	Token(TN),
	Proc(PN),
	Sequence(Vec<GrammarProcTemplate<PN, TN>>),
	Choice(Vec<GrammarProcTemplate<PN, TN>>),
	Repeat(u32, Option<u32>, Vec<GrammarProcTemplate<PN, TN>>),
	Option(GrammarProcTemplate<PN, TN>)
}

pub struct GrammarBuilder {
	
}