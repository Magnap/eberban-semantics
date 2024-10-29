use std::collections::{BTreeMap, BTreeSet};

use crate::{parser::PredicateTree, Exposure, GrammarVar, PredicateChaining};

pub type Var = usize;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Predicate {
    Leaf {
        word: String,
        apply_to: BTreeMap<GrammarVar, Var>,
    },
    And {
        preds: Vec<Predicate>,
    },
    Exists {
        vars: Vec<Var>,
        pred: Box<Predicate>,
    },
    Equivalent {
        var: Var,
        pred: Box<Predicate>,
    },
    Lambda {
        vars: Vec<Var>,
        pred: Box<Predicate>,
    },
}

pub fn to_expr(tree: PredicateTree) -> (Predicate, Vec<Var>) {
    let mut preds = Vec::new();
    let mut max_var = 0;
    let mut new_vars = Vec::new();
    to_expr_(
        tree,
        PredicateChaining::Equivalence,
        BTreeMap::new(),
        &mut new_vars,
        &mut max_var,
        &mut preds,
    );
    (Predicate::And { preds }, new_vars)
}

fn to_expr_(
    tree: PredicateTree,
    chaining_with: PredicateChaining,
    mut vars: BTreeMap<GrammarVar, Var>,
    orig_new_vars: &mut Vec<Var>,
    max_var: &mut Var,
    orig_preds: &mut Vec<Predicate>,
) {
    match tree {
        PredicateTree::Leaf(word) => orig_preds.push(Predicate::Leaf {
            word: word.word,
            apply_to: vars,
        }),
        PredicateTree::Binding {
            root,
            exposure,
            sharers,
            ..
        } => {
            let exposed_places: BTreeSet<_>;
            let chain_place;
            let transparent;
            match exposure {
                Exposure::Standard => {
                    transparent = false;
                    chain_place = 0;
                    exposed_places = match chaining_with {
                        PredicateChaining::Sharing => BTreeSet::from([0]),
                        PredicateChaining::Equivalence => {
                            (0..sharers.len()).map(|i| i as u8).collect()
                        }
                    };
                }
                Exposure::Transparent => {
                    transparent = true;
                    chain_place = 0;
                    exposed_places = (0..sharers.len()).map(|i| i as u8).collect();
                }
                Exposure::Modified(vec) => {
                    transparent = false;
                    chain_place = vec.first().copied().unwrap_or(0);
                    exposed_places = vec.iter().copied().collect();
                }
            };
            if matches!(chaining_with, PredicateChaining::Sharing) {
                let chain_var = vars.remove(&0).unwrap_or_else(|| {
                    let v = *max_var;
                    orig_new_vars.push(v);
                    *max_var += 1;
                    v
                });
                vars.clear();
                vars.insert(chain_place, chain_var);
            }

            let closure_needed = sharers
                .iter()
                .enumerate()
                .any(|(i, s)| !exposed_places.contains(&(i as u8)) && !s.is_empty());
            let mut new_new_vars = Vec::new();
            let mut new_preds = Vec::new();
            let (new_vars, preds) = if closure_needed {
                (&mut new_new_vars, &mut new_preds)
            } else {
                (&mut *orig_new_vars, &mut *orig_preds)
            };

            for (i, set) in sharers.into_iter().enumerate() {
                let i = i as GrammarVar;
                let var = *vars.entry(i).or_insert_with(|| {
                    let v = *max_var;
                    new_vars.push(v);
                    *max_var += 1;
                    v
                });
                for (chaining, pred_tree) in set {
                    match chaining {
                        PredicateChaining::Sharing => to_expr_(
                            pred_tree,
                            chaining,
                            [(0, var)].into_iter().collect(),
                            new_vars,
                            max_var,
                            preds,
                        ),
                        PredicateChaining::Equivalence => {
                            let mut equiv_preds = Vec::new();

                            if transparent {
                                to_expr_(
                                    pred_tree,
                                    chaining,
                                    BTreeMap::new(),
                                    new_vars,
                                    max_var,
                                    &mut equiv_preds,
                                );
                                let p = if equiv_preds.len() == 1 {
                                    equiv_preds.pop().unwrap()
                                } else {
                                    Predicate::And { preds: equiv_preds }
                                };
                                preds.push(Predicate::Equivalent {
                                    var,
                                    pred: Box::new(p),
                                });
                            } else {
                                let mut new_vars = Vec::new();
                                to_expr_(
                                    pred_tree,
                                    chaining,
                                    BTreeMap::new(),
                                    &mut new_vars,
                                    max_var,
                                    &mut equiv_preds,
                                );
                                let p = if equiv_preds.len() == 1 {
                                    equiv_preds.pop().unwrap()
                                } else {
                                    Predicate::And { preds: equiv_preds }
                                };
                                preds.push(Predicate::Equivalent {
                                    var,
                                    pred: if new_vars.is_empty() {
                                        Box::new(p)
                                    } else {
                                        Box::new(Predicate::Lambda {
                                            vars: new_vars,
                                            pred: Box::new(p),
                                        })
                                    },
                                });
                            }
                        }
                    }
                }
            }

            to_expr_(
                *root,
                PredicateChaining::Equivalence,
                vars,
                new_vars,
                max_var,
                preds,
            );

            if closure_needed {
                orig_preds.push(Predicate::Exists {
                    vars: new_new_vars,
                    pred: Box::new(Predicate::And { preds: new_preds }),
                });
            }
        }
    }
}
