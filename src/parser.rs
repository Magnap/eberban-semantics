use std::collections::BTreeSet;

use chumsky::{
    prelude::{choice, end, filter, just, recursive},
    Error, Parser,
};

use crate::{
    lexer::{FiVar, ParticleFamily, PredicateWord, Word},
    ChainingBehavior, Exposure, PredicateChaining,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PredicateTree {
    Leaf(PredicateWord),
    Binding {
        chaining: ChainingBehavior,
        root: Box<PredicateTree>,
        negated: bool,
        exposure: Exposure,
        sharers: Vec<BTreeSet<(PredicateChaining, PredicateTree)>>,
        and: BTreeSet<PredicateTree>,
    },
}

impl PredicateTree {
    pub fn chaining_behavior(&self) -> ChainingBehavior {
        match self {
            PredicateTree::Leaf(PredicateWord { chaining, .. }) => *chaining,
            PredicateTree::Binding { chaining, .. } => *chaining,
        }
    }
    pub fn to_binding(self) -> Self {
        match self {
            b @ PredicateTree::Binding { .. } => b,
            l @ PredicateTree::Leaf(_) => PredicateTree::Binding {
                chaining: l.chaining_behavior(),
                root: Box::new(l),
                negated: false,
                exposure: Exposure::Standard,
                sharers: Vec::new(),
                and: BTreeSet::new(),
            },
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

            let bi = just(Word::Particle(ParticleFamily::Bi)).to(true);
            let zi = just(Word::Particle(ParticleFamily::Zi("zi".to_string()))).to(false);
            let negation = choice((bi, zi));

            let binding = negation
                .or_not()
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
                .map(|(((n, l), b), r)| {
                    let no_binding = n.is_none() && b.is_empty() && r.is_none();
                    if no_binding {
                        l
                    } else {
                        let (chaining, root, negated, exposure, mut sharers, mut and) =
                            match l.to_binding() {
                                PredicateTree::Binding {
                                    chaining,
                                    root,
                                    negated,
                                    exposure,
                                    sharers,
                                    and,
                                } => {
                                    let root = if let Some(false) = n {
                                        Box::new(match root.to_binding() {
                                            PredicateTree::Binding {
                                                chaining,
                                                root,
                                                negated,
                                                exposure,
                                                sharers,
                                                and,
                                            } => PredicateTree::Binding {
                                                chaining,
                                                root,
                                                negated: !negated,
                                                exposure,
                                                sharers,
                                                and,
                                            },
                                            _ => unreachable!(),
                                        })
                                    } else {
                                        root
                                    };
                                    (chaining, root, negated, exposure, sharers, and)
                                }
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
                                                negated,
                                                sharers,
                                                and,
                                                ..
                                            } => PredicateTree::Binding {
                                                exposure: Exposure::Explicit(args),
                                                chaining,
                                                root,
                                                negated,
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
                            negated: negated ^ n.unwrap_or(false),
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
