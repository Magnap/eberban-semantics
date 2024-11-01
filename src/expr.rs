use std::collections::{BTreeMap, BTreeSet};

use crate::{parser::PredicateTree, Exposure, GrammarVar, PredicateChaining};

pub type Var = usize;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Predicate {
    Leaf {
        word: String,
        id: usize,
        apply_to: Vec<Var>,
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
impl std::fmt::Display for Predicate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Predicate::Leaf { word, id, apply_to } => {
                write!(f, "{word}{id}")?;
                if apply_to.is_empty() {
                    Ok(())
                } else {
                    write!(f, "(")?;
                    let mut first = true;
                    for v in apply_to {
                        if first {
                            write!(f, "{v}")?;
                        } else {
                            write!(f, ", {v}")?;
                        }
                        first = false;
                    }
                    write!(f, ")")
                }
            }
            Predicate::And { preds } => {
                let mut first = true;
                for p in preds {
                    if first {
                        write!(f, "{p}")?;
                    } else {
                        write!(f, " ∧ {p}")?;
                    }
                    first = false;
                }
                Ok(())
            }
            Predicate::Exists { vars, pred } => {
                if vars.is_empty() {
                    write!(f, "{pred}")
                } else {
                    write!(f, "(∃ ")?;
                    let mut first = true;
                    for v in vars {
                        if first {
                            write!(f, "{v}")?;
                        } else {
                            write!(f, ",{v}")?;
                        }
                        first = false;
                    }
                    write!(f, ". ")?;
                    write!(f, "{pred})")
                }
            }
            Predicate::Equivalent { var, pred } => write!(f, "({var} = {pred})"),
            Predicate::Lambda { vars, pred } => {
                if vars.is_empty() {
                    write!(f, "{pred}")
                } else {
                    write!(f, "(λ ")?;
                    let mut first = true;
                    for v in vars {
                        if first {
                            write!(f, "{v}")?;
                        } else {
                            write!(f, ",{v}")?;
                        }
                        first = false;
                    }
                    write!(f, ". ")?;
                    write!(f, "{pred})")
                }
            }
        }
    }
}

pub fn to_expr(tree: PredicateTree) -> (Predicate, Vec<Var>) {
    let mut preds = Vec::new();
    let mut max_var = 0;
    let mut max_id = 0;
    let mut symbol_table = BTreeMap::new();
    let mut new_vars = Vec::new();
    to_expr_(
        tree,
        PredicateChaining::Equivalence,
        BTreeMap::new(),
        &mut new_vars,
        &mut max_var,
        &mut max_id,
        &mut symbol_table,
        &mut preds,
    );
    (
        if preds.len() == 1 {
            preds.pop().unwrap()
        } else {
            Predicate::And { preds }
        },
        new_vars,
    )
}

fn to_expr_(
    tree: PredicateTree,
    chaining_with: PredicateChaining,
    mut vars: BTreeMap<GrammarVar, Var>,
    orig_new_vars: &mut Vec<Var>,
    max_var: &mut Var,
    max_id: &mut usize,
    symbol_table: &mut BTreeMap<String, Vec<usize>>,
    orig_preds: &mut Vec<Predicate>,
) {
    match tree {
        PredicateTree::Leaf(word) => orig_preds.push(Predicate::Leaf {
            word: word.word.clone(),
            id: *symbol_table
                .entry(word.word)
                .or_insert_with(|| {
                    let i = *max_id;
                    *max_id += 1;
                    vec![i]
                })
                .last()
                .unwrap(),
            apply_to: vars.values().copied().collect(),
        }),
        PredicateTree::Binding {
            chaining: _,
            root,
            exposure,
            sharers,
            and,
        } => {
            let exposed_places: BTreeSet<_>;
            let chain_place;
            match &exposure {
                Exposure::Standard => {
                    chain_place = 0;
                    exposed_places = match chaining_with {
                        PredicateChaining::Sharing => BTreeSet::from([0]),
                        PredicateChaining::Equivalence => {
                            (0..sharers.len()).map(|i| i as u8).collect()
                        }
                    };
                }
                Exposure::Transparent => {
                    chain_place = 0;
                    exposed_places = (0..sharers.len()).map(|i| i as u8).collect();
                }
                Exposure::Modified(vec) => {
                    chain_place = vec.first().copied().unwrap_or(0);
                    exposed_places = vec.iter().copied().collect();
                }
                Exposure::Explicit(_) => {
                    chain_place = 0;
                    exposed_places = BTreeSet::new();
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
            if let Exposure::Explicit(vec) = &exposure {
                for (i, (word, chain_with)) in vec.iter().enumerate() {
                    let var = vars.remove(&(i as u8)).unwrap_or_else(|| {
                        let v = *max_var;
                        orig_new_vars.push(v);
                        *max_var += 1;
                        v
                    });
                    let id = *max_id;
                    *max_id += 1;
                    symbol_table.entry(word.clone()).or_default().push(id);
                    match chain_with {
                        PredicateChaining::Sharing => orig_preds.push(Predicate::Leaf {
                            word: word.clone(),
                            id,
                            apply_to: vec![var],
                        }),
                        PredicateChaining::Equivalence => orig_preds.push(Predicate::Equivalent {
                            var,
                            pred: Box::new(Predicate::Leaf {
                                word: word.clone(),
                                id,
                                apply_to: Vec::new(),
                            }),
                        }),
                    }
                }
                vars.clear();
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

            for (i, set) in sharers.into_iter().enumerate().rev() {
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
                            max_id,
                            symbol_table,
                            preds,
                        ),
                        PredicateChaining::Equivalence => {
                            let mut equiv_preds = Vec::new();

                            if matches!(exposure, Exposure::Transparent) {
                                to_expr_(
                                    pred_tree,
                                    chaining,
                                    BTreeMap::new(),
                                    new_vars,
                                    max_var,
                                    max_id,
                                    symbol_table,
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
                                    max_id,
                                    symbol_table,
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
                max_id,
                symbol_table,
                preds,
            );

            if let Exposure::Explicit(vec) = &exposure {
                for (word, _) in vec.iter() {
                    symbol_table.get_mut(word).unwrap().pop();
                }
            }

            if closure_needed {
                let p = if new_preds.len() == 1 {
                    new_preds.pop().unwrap()
                } else {
                    Predicate::And { preds: new_preds }
                };
                orig_preds.push(Predicate::Exists {
                    vars: new_new_vars,
                    pred: Box::new(p),
                });
            }

            for p in and {
                let mut new_vars = Vec::new();
                let mut preds = Vec::new();
                to_expr_(
                    p,
                    PredicateChaining::Equivalence,
                    BTreeMap::new(),
                    &mut new_vars,
                    max_var,
                    max_id,
                    symbol_table,
                    &mut preds,
                );

                let p = if preds.len() == 1 {
                    preds.pop().unwrap()
                } else {
                    Predicate::And { preds }
                };
                orig_preds.push(Predicate::Exists {
                    vars: new_vars,
                    pred: Box::new(p),
                });
            }
        }
    }
}
