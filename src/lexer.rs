use std::{collections::HashMap, iter};

use crate::{ChainingBehavior, Exposure, GrammarVar, PredicateChaining};
use chumsky::{
    prelude::{choice, end, filter, just},
    Error, Parser,
};

pub const VOWELS: [char; 5] = ['i', 'e', 'a', 'o', 'u'];
pub const NON_SONORANT: [char; 13] = [
    'm', 'p', 'b', 'f', 'v', 't', 'd', 's', 'z', 'c', 'j', 'k', 'g',
];
pub const SONORANT: [char; 3] = ['n', 'r', 'l'];
pub const INITIAL_PAIRS: [(char, char); 69] = [
    ('b', 'z'),
    ('b', 'j'),
    ('b', 'r'),
    ('b', 'l'),
    ('d', 'z'),
    ('d', 'j'),
    ('d', 'r'),
    ('g', 'z'),
    ('g', 'j'),
    ('g', 'n'),
    ('g', 'r'),
    ('v', 'l'),
    ('v', 'z'),
    ('v', 'j'),
    ('v', 'n'),
    ('v', 'r'),
    ('v', 'l'),
    ('z', 'b'),
    ('z', 'd'),
    ('z', 'g'),
    ('z', 'v'),
    ('z', 'm'),
    ('z', 'n'),
    ('z', 'r'),
    ('z', 'l'),
    ('j', 'b'),
    ('j', 'd'),
    ('j', 'g'),
    ('j', 'v'),
    ('j', 'm'),
    ('j', 'n'),
    ('j', 'r'),
    ('j', 'l'),
    ('c', 'f'),
    ('c', 'k'),
    ('c', 't'),
    ('c', 'p'),
    ('c', 'm'),
    ('c', 'n'),
    ('c', 'r'),
    ('c', 'l'),
    ('s', 'f'),
    ('s', 'k'),
    ('s', 't'),
    ('s', 'p'),
    ('s', 'm'),
    ('s', 'n'),
    ('s', 'r'),
    ('s', 'l'),
    ('f', 'c'),
    ('f', 's'),
    ('f', 'n'),
    ('f', 'r'),
    ('f', 'l'),
    ('k', 'c'),
    ('k', 's'),
    ('k', 'n'),
    ('k', 'r'),
    ('k', 'l'),
    ('t', 'c'),
    ('t', 's'),
    ('t', 'r'),
    ('p', 'c'),
    ('p', 's'),
    ('p', 'r'),
    ('p', 'l'),
    ('m', 'n'),
    ('m', 'r'),
    ('m', 'l'),
];
pub const MEDIAL_PAIRS: [(char, char); 36] = [
    ('b', 'd'),
    ('b', 'g'),
    ('b', 'v'),
    ('b', 'm'),
    ('d', 'b'),
    ('d', 'g'),
    ('d', 'v'),
    ('d', 'm'),
    ('g', 'b'),
    ('g', 'd'),
    ('g', 'v'),
    ('g', 'm'),
    ('v', 'b'),
    ('v', 'd'),
    ('v', 'g'),
    ('v', 'm'),
    ('f', 'k'),
    ('f', 't'),
    ('f', 'p'),
    ('f', 'm'),
    ('k', 'f'),
    ('k', 't'),
    ('k', 'p'),
    ('k', 'm'),
    ('t', 'f'),
    ('t', 'k'),
    ('t', 'p'),
    ('t', 'm'),
    ('p', 'f'),
    ('p', 'k'),
    ('p', 't'),
    ('p', 'm'),
    ('n', 'r'),
    ('n', 'l'),
    ('r', 'n'),
    ('l', 'n'),
];

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Word {
    pub word: String,
    pub class: WordClass,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum WordClass {
    Particle(ParticleFamily),
    Predicate(PredicateFamily, ChainingBehavior),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PredicateFamily {
    Root,
    Borrowing,
    Freeform,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParticleFamily {
    Pe,
    Pei,
    Vi {
        var: Option<GrammarVar>,
        chain_with: PredicateChaining,
    },
    Fi {
        var: FiVar,
        chain_with: PredicateChaining,
    },
    Vei,
    Ki,
    Gi(ChainingBehavior),
    Be,
    Mi(ChainingBehavior),
    Si {
        exposure: Exposure,
        chaining: ChainingBehavior,
    },
    Other,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FiVar {
    None,
    Var(GrammarVar),
    Same,
    Next,
}

pub fn lexer<E: Error<char>>() -> impl Parser<char, Vec<Word>, Error = E> {
    let letter: HashMap<_, _> = "hnrlmpbfvtdszcjkgieaou"
        .chars()
        .map(|c| {
            (
                c,
                choice((just::<_, _, E>(c), just::<_, _, E>(c.to_ascii_uppercase())))
                    .repeated()
                    .at_least(1)
                    .map(move |_| c),
            )
        })
        .collect();
    let pause = filter::<_, _, E>(|c: &char| c.is_whitespace() || *c == '\'').repeated();

    let vowel = choice::<_, E>(VOWELS.map(|c| letter[&c]));
    let non_sonorant = choice::<_, E>(NON_SONORANT.map(|c| letter[&c]));
    let sonorant = choice::<_, E>(SONORANT.map(|c| letter[&c]));

    let initial_pair = choice::<_, E>(INITIAL_PAIRS.map(|(a, b)| just(a).then(just::<_, _, E>(b))));
    let medial_pair = choice::<_, E>(MEDIAL_PAIRS.map(|(a, b)| just(a).then(just::<_, _, E>(b))));

    let nonsonorant_particle = pause.ignore_then(
        non_sonorant
            .then(
                vowel
                    .repeated()
                    .at_least(1)
                    .then(
                        just('h')
                            .then(vowel.repeated().at_least(1))
                            .map(|(h, vowels)| iter::once(h).chain(vowels))
                            .repeated(),
                    )
                    .map(|(vowel, vhowels)| vowel.into_iter().chain(vhowels.into_iter().flatten())),
            )
            .map(|(c, vh)| {
                let w: String = iter::once(c).chain(vh).collect();
                let f = match c {
                    'k' => ParticleFamily::Ki,
                    'g' => ParticleFamily::Gi(if c == 'i' {
                        ChainingBehavior {
                            var: 0,
                            chain_with: PredicateChaining::Sharing,
                        }
                    } else if w.ends_with('i') {
                        ChainingBehavior {
                            var: 1,
                            chain_with: PredicateChaining::Equivalence,
                        }
                    } else {
                        ChainingBehavior {
                            var: 1,
                            chain_with: PredicateChaining::Sharing,
                        }
                    }),
                    _ => ParticleFamily::Other,
                };
                (w, f)
            }),
    );
    let sonorant_or_vowel_particle = pause.at_least(1).ignore_then(
        choice::<_, E>((
            sonorant.map(Some).then(vowel),
            vowel.map(|v| (None::<char>, v)),
        ))
        .then(
            choice((just('h'), sonorant))
                .then(vowel.repeated().at_least(1))
                .map(|(h, vowels)| iter::once(h).chain(vowels))
                .repeated(),
        )
        .then(sonorant.or_not())
        .map(|(((a, b), c), d)| {
            (
                a.into_iter()
                    .chain(iter::once(b))
                    .chain(c.into_iter().flatten())
                    .chain(d)
                    .collect(),
                ParticleFamily::Other,
            )
        }),
    );

    let mi = pause
        .ignore_then(choice(
            [
                "mai", "mao", "mui", "mue", "mua", "mio", "mie", "moe", "ma", "mi", "mo", "me",
            ]
            .map(just),
        ))
        .map(|w| {
            (
                w.to_string(),
                ParticleFamily::Mi(if w == "mua" {
                    ChainingBehavior {
                        var: 1,
                        chain_with: PredicateChaining::Equivalence,
                    }
                } else {
                    ChainingBehavior {
                        var: 0,
                        chain_with: PredicateChaining::Sharing,
                    }
                }),
            )
        });

    let arg_vowel = choice(['e', 'a', 'o', 'u'].map(just)).map(|v| {
        (
            v,
            match v {
                'e' => 0,
                'a' => 1,
                'o' => 2,
                'u' => 3,
                _ => unreachable!(),
            },
        )
    });
    let si = pause.ignore_then(
        just('s').ignore_then(choice((
            just('i').ignore_then(arg_vowel).map(|(v, i)| {
                (
                    ['s', 'i', v].into_iter().collect(),
                    ParticleFamily::Si {
                        exposure: Exposure::Transparent,
                        chaining: ChainingBehavior {
                            var: i,
                            chain_with: PredicateChaining::Equivalence,
                        },
                    },
                )
            }),
            choice((
                just('i')
                    .to(Vec::new())
                    .then(just('h').ignore_then(arg_vowel).map(Some))
                    .then(just('i').map(Some)),
                arg_vowel
                    .repeated()
                    .at_least(1)
                    .then(just('h').ignore_then(arg_vowel).or_not())
                    .then(just('i').or_not()),
            ))
            .map(|((vs, b), i)| {
                (
                    iter::once('s')
                        .chain(vs.iter().map(|(v, _)| *v).chain(b.map(|(v, _)| v)).chain(i))
                        .collect(),
                    ParticleFamily::Si {
                        exposure: Exposure::Modified(vs.iter().map(|(_, i)| *i).collect()),
                        chaining: ChainingBehavior {
                            var: if let Some((_, i)) = b {
                                i
                            } else {
                                vs.last().unwrap().1
                            },
                            chain_with: if i.is_some() {
                                PredicateChaining::Equivalence
                            } else {
                                PredicateChaining::Sharing
                            },
                        },
                    },
                )
            }),
        ))),
    );

    let vi = pause.ignore_then(choice((
        just('v')
            .then(just('i').or_not().then(arg_vowel))
            .map(|(v, (i, (var_v, var_i)))| {
                (
                    iter::once(v).chain(i).chain(iter::once(var_v)).collect(),
                    ParticleFamily::Vi {
                        var: Some(var_i),
                        chain_with: if i.is_some() {
                            PredicateChaining::Equivalence
                        } else {
                            PredicateChaining::Sharing
                        },
                    },
                )
            }),
        just("vi").map(|w| {
            (
                w.to_string(),
                ParticleFamily::Vi {
                    var: None,
                    chain_with: PredicateChaining::Sharing,
                },
            )
        }),
    )));
    let fi = pause.ignore_then(choice((
        just("feu").map(|w| {
            (
                w.to_string(),
                ParticleFamily::Fi {
                    var: FiVar::Same,
                    chain_with: PredicateChaining::Sharing,
                },
            )
        }),
        just("fau").map(|w| {
            (
                w.to_string(),
                ParticleFamily::Fi {
                    var: FiVar::Next,
                    chain_with: PredicateChaining::Sharing,
                },
            )
        }),
        just("fei").map(|w| {
            (
                w.to_string(),
                ParticleFamily::Fi {
                    var: FiVar::Same,
                    chain_with: PredicateChaining::Equivalence,
                },
            )
        }),
        just("fai").map(|w| {
            (
                w.to_string(),
                ParticleFamily::Fi {
                    var: FiVar::Next,
                    chain_with: PredicateChaining::Equivalence,
                },
            )
        }),
        just('f')
            .then(just('i').or_not().then(arg_vowel))
            .map(|(v, (i, (var_v, var_i)))| {
                (
                    iter::once(v).chain(i).chain(iter::once(var_v)).collect(),
                    ParticleFamily::Fi {
                        var: FiVar::Var(var_i),
                        chain_with: if i.is_some() {
                            PredicateChaining::Equivalence
                        } else {
                            PredicateChaining::Sharing
                        },
                    },
                )
            }),
        just("fi").map(|w| {
            (
                w.to_string(),
                ParticleFamily::Fi {
                    var: FiVar::None,
                    chain_with: PredicateChaining::Sharing,
                },
            )
        }),
    )));
    let vei = pause
        .ignore_then(just("vei"))
        .map(|w| (w.to_string(), ParticleFamily::Vei));
    let be = pause
        .ignore_then(just("be"))
        .map(|w| (w.to_string(), ParticleFamily::Be));

    let pe = pause
        .ignore_then(just("pe"))
        .map(|w| (w.to_string(), ParticleFamily::Pe));
    let pei = pause
        .ignore_then(just("pei"))
        .map(|w| (w.to_string(), ParticleFamily::Pei));

    let specific_particle = choice((pei, pe, be, vei, vi, fi, mi, si));

    let particle = choice((
        specific_particle,
        nonsonorant_particle,
        sonorant_or_vowel_particle,
    ))
    .map(|(w, f)| Word {
        word: w,
        class: WordClass::Particle(f),
    });

    let root_mix = choice((
        medial_pair.map(|(a, b)| (a, Some(b))),
        choice((just('h'), sonorant)).map(|c| (c, None)),
    ))
    .then(vowel.repeated().at_least(1))
    .map(|((a, b), vowels)| iter::once(a).chain(b).chain(vowels).collect::<Vec<_>>());
    let required_string = choice((
        medial_pair.map(|(a, b)| (a, Some(b))),
        sonorant.map(|c| (c, None)),
    ))
    .then(vowel.repeated().at_least(1))
    .map(|((a, b), vowels)| iter::once(a).chain(b).chain(vowels).collect::<Vec<_>>());
    let nonsonorant_root = non_sonorant
        .chain(vowel.repeated().at_least(1))
        .then(choice((
            choice((
                required_string,
                root_mix
                    .repeated()
                    .chain::<Vec<char>, _, _>(required_string)
                    .flatten(),
            ))
            .chain::<Vec<char>, _, _>(root_mix.repeated())
            .flatten()
            .then(sonorant.or_not()),
            sonorant.map(|s| (Vec::new(), Some(s))),
        )))
        .map(|(a, (b, c))| a.into_iter().chain(b).chain(c).collect())
        .map(|w: String| {
            let last = w.chars().last().unwrap();
            let chaining = if last == 'i' {
                ChainingBehavior {
                    var: 1,
                    chain_with: PredicateChaining::Equivalence,
                }
            } else if VOWELS.contains(&last) {
                ChainingBehavior {
                    var: 1,
                    chain_with: PredicateChaining::Sharing,
                }
            } else {
                ChainingBehavior {
                    var: 0,
                    chain_with: PredicateChaining::Sharing,
                }
            };
            (w, chaining)
        });
    let initial_pair_root = initial_pair
        .chain(vowel.repeated().at_least(1))
        .then(root_mix.repeated())
        .then(sonorant.or_not())
        .map(|((a, b), c)| {
            a.into_iter()
                .chain(b.into_iter().flatten())
                .chain(c)
                .collect()
        })
        .map(|w: String| {
            let last = w.chars().last().unwrap();
            let chaining = if last == 'i' || w.len() == 3 {
                ChainingBehavior {
                    var: 1,
                    chain_with: PredicateChaining::Equivalence,
                }
            } else if VOWELS.contains(&last) {
                ChainingBehavior {
                    var: 1,
                    chain_with: PredicateChaining::Sharing,
                }
            } else {
                ChainingBehavior {
                    var: 0,
                    chain_with: PredicateChaining::Sharing,
                }
            };
            (w, chaining)
        });
    let root = pause.ignore_then(
        choice((nonsonorant_root, initial_pair_root)).map(|(w, c)| (w, c, PredicateFamily::Root)),
    );
    let predicate = choice([root]).map(|(w, c, f)| Word {
        word: w,
        class: WordClass::Predicate(f, c),
    });

    let word = choice((predicate, particle));

    word.repeated().then_ignore(pause.then(end()))
}
