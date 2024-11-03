use std::collections::BTreeSet;

use chumsky::{
    prelude::{choice, end, filter, just, recursive},
    Error, Parser,
};

use crate::{
    lexer::{FiVar, ParticleFamily, PredicateWord, Word},
    ChainingBehavior, Exposure, Negation, PredicateChaining,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PredicateTree {
    Leaf(PredicateWord),
    Negation {
        negation: Negation,
        pred: Box<PredicateTree>,
    },
    Binding {
        chaining: ChainingBehavior,
        root: Box<PredicateTree>,
        negation: Negation,
        exposure: Exposure,
        sharers: Vec<BTreeSet<(PredicateChaining, PredicateTree)>>,
        and: BTreeSet<PredicateTree>,
    },
}

impl PredicateTree {
    pub fn chaining_behavior(&self) -> ChainingBehavior {
        match self {
            PredicateTree::Leaf(PredicateWord { chaining, .. }) => *chaining,
            PredicateTree::Negation { pred, .. } => pred.chaining_behavior(),
            PredicateTree::Binding { chaining, .. } => *chaining,
        }
    }
    pub fn to_binding(self) -> Self {
        match self {
            b @ PredicateTree::Binding { .. } => b,
            PredicateTree::Negation { negation, pred } => pred.to_binding().negate(negation),
            l @ PredicateTree::Leaf(_) => PredicateTree::Binding {
                chaining: l.chaining_behavior(),
                root: Box::new(l),
                negation: Negation::None,
                exposure: Exposure::Standard,
                sharers: Vec::new(),
                and: BTreeSet::new(),
            },
        }
    }
    pub fn negate(self, orig_negation: Negation) -> Self {
        if orig_negation == Negation::None {
            self
        } else {
            match self {
                l @ PredicateTree::Leaf(_) => PredicateTree::Negation {
                    negation: orig_negation,
                    pred: Box::new(l),
                },
                PredicateTree::Negation { negation, pred } => {
                    let negation = orig_negation ^ negation;
                    match negation {
                        Negation::None => *pred,
                        negation => PredicateTree::Negation { negation, pred },
                    }
                }
                PredicateTree::Binding {
                    chaining,
                    root,
                    negation,
                    exposure,
                    sharers,
                    and,
                } => PredicateTree::Binding {
                    chaining,
                    root,
                    negation: orig_negation ^ negation,
                    exposure,
                    sharers,
                    and,
                },
            }
        }
    }
}

pub fn parser<E: Error<Word> + 'static>() -> impl Parser<Word, PredicateTree, Error = E> {
    let predicate = filter(|w: &Word| {
        matches!(
            w,
            Word::Predicate { .. }
                | Word::Particle(
                    ParticleFamily::Ki(_) | ParticleFamily::Gi(_) | ParticleFamily::Mi(_)
                )
        )
    })
    .map(|w| match w {
        Word::Predicate(pw, _) => pw,
        Word::Particle(ParticleFamily::Ki(word)) => PredicateWord {
            word,
            chaining: ChainingBehavior {
                var: 0,
                chain_with: PredicateChaining::Sharing,
            },
        },
        Word::Particle(ParticleFamily::Gi(pw) | ParticleFamily::Mi(pw)) => pw,
        _ => unreachable!(),
    });
    let predicate_tree =
        recursive(|predicate_tree| {
            let leaf = predicate.map(PredicateTree::Leaf);
            let pe_pei = predicate_tree.clone().delimited_by(
                just(Word::Particle(ParticleFamily::Pe)),
                just(Word::Particle(ParticleFamily::Pei)).or_not(),
            );

            let si = filter(|w: &Word| matches!(w, Word::Particle(ParticleFamily::Si { .. }))).map(
                |s| match s {
                    Word::Particle(ParticleFamily::Si { exposure, chaining }) => {
                        (exposure, chaining)
                    }
                    _ => unreachable!(),
                },
            );
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

            let vi = filter(|w: &Word| matches!(w, Word::Particle(ParticleFamily::Vi { .. }))).map(
                |w| match w {
                    Word::Particle(f) => f,
                    _ => unreachable!(),
                },
            );
            let fi = filter(|w: &Word| matches!(w, Word::Particle(ParticleFamily::Fi { .. }))).map(
                |w| match w {
                    Word::Particle(f) => f,
                    _ => unreachable!(),
                },
            );
            let vei = just(Word::Particle(ParticleFamily::Vei));

            let argument = choice((
                filter(|w: &Word| matches!(w, Word::Particle(ParticleFamily::Ki(_)))),
                filter(|w: &Word| matches!(w, Word::Particle(ParticleFamily::Gi { .. }))),
            ))
            .map(|w| match w {
                Word::Particle(ParticleFamily::Ki(word)) => (word, PredicateChaining::Sharing),
                Word::Particle(ParticleFamily::Gi(pw)) => (pw.word, PredicateChaining::Equivalence),
                _ => unreachable!(),
            });
            let be = just(Word::Particle(ParticleFamily::Be));
            let argument_list = argument.repeated().then_ignore(be);

            let bi = just(Word::Particle(ParticleFamily::Bi)).to(Negation::Long);
            let zi = just(Word::Particle(ParticleFamily::Zi("zi".to_string()))).to(Negation::Short);

            let binding = bi
                .ignored()
                .repeated()
                .then(zi.ignored().repeated())
                .then(element.clone())
                .then(
                    vi.then(argument_list.clone().or_not())
                        .then(predicate_tree.clone())
                        .chain(
                            fi.then(argument_list.or_not())
                                .then(predicate_tree.clone())
                                .repeated(),
                        )
                        .then_ignore(vei.or_not())
                        .repeated(),
                )
                .then(predicate_tree.or_not())
                .map(|((((bi, zi), l), b), r)| {
                    let n = Negation::new(zi.len() % 2 == 1, bi.len() % 2 == 1);
                    let no_binding = b.is_empty() && r.is_none();
                    if no_binding {
                        l.negate(n)
                    } else {
                        let (chaining, root, negation, exposure, mut sharers, mut and) =
                            match l.to_binding().negate(n) {
                                PredicateTree::Binding {
                                    chaining,
                                    root,
                                    negation,
                                    exposure,
                                    sharers,
                                    and,
                                } => (chaining, root, negation, exposure, sharers, and),
                                _ => unreachable!(),
                            };

                        let children = r
                            .into_iter()
                            .map(|r| {
                                let binding = (FiVar::Var(chaining.var), chaining.chain_with);
                                (binding, r)
                            })
                            .chain(b.into_iter().flat_map(|b| {
                                b.into_iter().map(|((pf, args), p)| {
                                    let binding = match pf {
                                        ParticleFamily::Vi { var, chain_with } => {
                                            let var = match var {
                                                Some(var) => FiVar::Var(var),
                                                None => FiVar::None,
                                            };
                                            (var, chain_with)
                                        }
                                        ParticleFamily::Fi { var, chain_with } => (var, chain_with),
                                        _ => unreachable!(),
                                    };
                                    let p = if let Some(args) = args {
                                        match p.to_binding() {
                                            PredicateTree::Binding {
                                                chaining,
                                                root,
                                                negation: negated,
                                                sharers,
                                                and,
                                                ..
                                            } => PredicateTree::Binding {
                                                exposure: Exposure::Explicit(args),
                                                chaining,
                                                root,
                                                negation: negated,
                                                sharers,
                                                and,
                                            },
                                            _ => unreachable!(),
                                        }
                                    } else {
                                        p
                                    };
                                    (binding, p)
                                })
                            }));
                        let mut v = 0;
                        for ((var, chain_with), p) in children {
                            v = match var {
                                FiVar::Same | FiVar::None => v,
                                FiVar::Next => v + 1,
                                FiVar::Var(v) => v,
                            };
                            if let FiVar::None = var {
                                and.insert(p);
                            } else {
                                while sharers.len() <= v as usize {
                                    sharers.push(BTreeSet::new());
                                }
                                sharers[v as usize].insert((chain_with, p));
                            }
                        }

                        PredicateTree::Binding {
                            chaining,
                            root,
                            negation,
                            exposure,
                            sharers,
                            and,
                        }
                    }
                });

            binding
        });
    predicate_tree.then_ignore(end())
}
