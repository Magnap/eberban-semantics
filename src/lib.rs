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
    Explicit(Vec<(String, PredicateChaining)>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Negation {
    None,
    Short,
    Long,
    Both,
}
impl Negation {
    pub fn new(short: bool, long: bool) -> Self {
        match (short, long) {
            (false, false) => Negation::None,
            (true, false) => Negation::Short,
            (false, true) => Negation::Long,
            (true, true) => Negation::Both,
        }
    }
    pub fn short(&self) -> bool {
        matches!(self, Negation::Short | Negation::Both)
    }
    pub fn long(&self) -> bool {
        matches!(self, Negation::Long | Negation::Both)
    }
}
impl std::ops::BitXor for Negation {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        let short = self.short() ^ rhs.short();
        let long = self.long() ^ rhs.long();
        Self::new(short, long)
    }
}
