use std::collections::BTreeSet;

use chumsky::{
    prelude::{choice, end, filter, just, recursive},
    Error, Parser,
};

use crate::{
    lexer::{FiVar, ParticleFamily, Word, WordClass},
    ChainingBehavior, Exposure, PredicateChaining,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PredicateTree {
    Leaf(Word),
    Binding {
        chaining: ChainingBehavior,
        root: Box<PredicateTree>,
        exposure: Exposure,
        sharers: Vec<BTreeSet<(PredicateChaining, PredicateTree)>>,
    },
}

impl PredicateTree {
    pub fn chaining_behavior(&self) -> ChainingBehavior {
        match self {
            PredicateTree::Leaf(w) => match w.class {
                WordClass::Predicate(_, chaining_behavior) => chaining_behavior,
                WordClass::Particle(ParticleFamily::Ki) => ChainingBehavior {
                    var: 0,
                    chain_with: PredicateChaining::Sharing,
                },
                WordClass::Particle(
                    ParticleFamily::Gi(chaining) | ParticleFamily::Mi(chaining),
                ) => chaining,
                _ => unreachable!(),
            },
            PredicateTree::Binding { chaining, .. } => *chaining,
        }
    }
    pub fn to_binding(self) -> Self {
        match self {
            b @ PredicateTree::Binding { .. } => b,
            l @ PredicateTree::Leaf(_) => PredicateTree::Binding {
                chaining: l.chaining_behavior(),
                root: Box::new(l),
                exposure: Exposure::Standard,
                sharers: Vec::new(),
            },
        }
    }
}

pub fn parser<E: Error<Word> + 'static>() -> impl Parser<Word, PredicateTree, Error = E> {
    let predicate = filter(|w: &Word| {
        matches!(
            w.class,
            WordClass::Predicate(_, _)
                | WordClass::Particle(
                    ParticleFamily::Ki | ParticleFamily::Gi(_) | ParticleFamily::Mi(_)
                )
        )
    });
    let predicate_tree = recursive(|predicate_tree| {
        let leaf = predicate.map(PredicateTree::Leaf);
        let pe_pei = predicate_tree.clone().delimited_by(
            just(Word {
                word: "pe".to_string(),
                class: WordClass::Particle(ParticleFamily::Other),
            }),
            just(Word {
                word: "pei".to_string(),
                class: WordClass::Particle(ParticleFamily::Other),
            })
            .or_not(),
        );

        let si =
            filter(|w: &Word| matches!(w.class, WordClass::Particle(ParticleFamily::Si { .. })))
                .map(|s| match s.class {
                    WordClass::Particle(ParticleFamily::Si { exposure, chaining }) => {
                        (exposure, chaining)
                    }
                    _ => unreachable!(),
                });
        let element = si
            .or_not()
            .then(choice((leaf, pe_pei.clone())))
            .map(|(s, p)| match s {
                None => p,
                Some((exposure, chaining)) => {
                    let mut e = p.to_binding();
                    match &mut e {
                        PredicateTree::Binding {
                            chaining: old_chaining,
                            exposure: old_exposure,
                            ..
                        } => {
                            *old_chaining = chaining;
                            *old_exposure = exposure;
                        }
                        _ => unreachable!(),
                    }
                    e
                }
            });

        let vi =
            filter(|w: &Word| matches!(w.class, WordClass::Particle(ParticleFamily::Vi { .. })))
                .map(|w| match w.class {
                    WordClass::Particle(f) => f,
                    _ => unreachable!(),
                });
        let fi =
            filter(|w: &Word| matches!(w.class, WordClass::Particle(ParticleFamily::Fi { .. })))
                .map(|w| match w.class {
                    WordClass::Particle(f) => f,
                    _ => unreachable!(),
                });
        let vei = filter(|w: &Word| matches!(w.class, WordClass::Particle(ParticleFamily::Vei)));
        let binding = element
            .clone()
            .then(
                vi.then(predicate_tree.clone())
                    .chain(fi.then(predicate_tree.clone()).repeated())
                    .then_ignore(vei.or_not())
                    .repeated(),
            )
            .then(predicate_tree.or_not())
            .map(|((l, b), r)| {
                if matches!(l, PredicateTree::Leaf(_)) && b.is_empty() && r.is_none() {
                    l
                } else {
                    let (chaining, root, exposure, mut sharers) = match l.to_binding() {
                        PredicateTree::Binding {
                            chaining,
                            root,
                            exposure,
                            sharers,
                        } => (chaining, root, exposure, sharers),
                        _ => unreachable!(),
                    };

                    let children = r
                        .into_iter()
                        .map(|r| {
                            let binding = (FiVar::Var(chaining.var), chaining.chain_with);
                            (binding, r)
                        })
                        .chain(b.into_iter().flat_map(|b| {
                            b.into_iter().map(|(pf, p)| {
                                let binding = match pf {
                                    ParticleFamily::Vi { var, chain_with } => {
                                        (FiVar::Var(var), chain_with)
                                    }
                                    ParticleFamily::Fi { var, chain_with } => (var, chain_with),
                                    _ => unreachable!(),
                                };
                                (binding, p)
                            })
                        }));
                    let mut v = 0;
                    for ((var, chain_with), p) in children {
                        v = match var {
                            FiVar::Var(v) => v,
                            FiVar::Same => v,
                            FiVar::Next => v + 1,
                        };
                        while sharers.len() <= v as usize {
                            sharers.push(BTreeSet::new());
                        }
                        sharers[v as usize].insert((chain_with, p));
                    }

                    PredicateTree::Binding {
                        chaining,
                        root,
                        exposure,
                        sharers,
                    }
                }
            });

        binding
    });
    predicate_tree.then_ignore(end())
}
