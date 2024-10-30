use lexer::Word;

pub mod expr;
pub mod lexer;
pub mod parser;

pub type GrammarVar = u8;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ChainingBehavior {
    pub var: GrammarVar,
    pub chain_with: PredicateChaining,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PredicateChaining {
    Sharing,
    Equivalence,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Exposure {
    Standard,
    Transparent,
    Modified(Vec<GrammarVar>),
    Explicit(Vec<Word>),
}
