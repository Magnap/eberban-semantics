use std::{
    collections::{BTreeMap, BTreeSet},
    mem,
};

use crate::{parser::PredicateTree, Exposure, GrammarVar, PredicateChaining};

pub type Var = usize;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Predicate {
    Leaf {
        word: String,
        id: usize,
        apply_to: Vec<Var>,
    },
    Not(Box<Predicate>),
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
            Predicate::Not(pred) => {
                if matches!(**pred, Predicate::And { .. }) {
                    write!(f, "¬({pred})")
                } else {
                    write!(f, "¬{pred}")
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
            Predicate::Equivalent { var, pred } => {
                if matches!(**pred, Predicate::And { .. }) {
                    write!(f, "({var} = ({pred}))")
                } else {
                    write!(f, "({var} = {pred})")
                }
            }
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
        Vec::new(),
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
    mut vars: Vec<Var>,
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
            apply_to: vars,
        }),
        PredicateTree::Binding {
            chaining: _,
            root,
            negated,
            exposure,
            sharers,
            and,
        } => {
            let mut close_over = Vec::new();
            let chain_place = match &exposure {
                Exposure::Standard | Exposure::Transparent | Exposure::Explicit(_) => 0,
                Exposure::Modified(vec) => vec.first().copied().unwrap_or(0),
            };
            if matches!(chaining_with, PredicateChaining::Sharing) {
                let chain_var = vars.first().copied().unwrap_or_else(|| {
                    let v = *max_var;
                    close_over.push(v);
                    *max_var += 1;
                    v
                });
                vars.clear();
                vars = (0..(sharers.len() as u8))
                    .map(|i| {
                        if i == chain_place {
                            chain_var
                        } else {
                            let v = *max_var;
                            close_over.push(v);
                            *max_var += 1;
                            v
                        }
                    })
                    .collect();
            } else {
                for _ in 0..(sharers.len().saturating_sub(vars.len())) {
                    let v = *max_var;
                    orig_new_vars.push(v);
                    *max_var += 1;
                    vars.push(v);
                }
            }
            if let Exposure::Explicit(vec) = &exposure {
                for (i, (word, chain_with)) in vec.iter().enumerate() {
                    let mut var = *max_var;
                    *max_var += 1;
                    if let Some(v) = vars.get_mut(i) {
                        close_over.push(var);
                        mem::swap(v, &mut var);
                    } else {
                        orig_new_vars.push(var);
                    }
                    let var = var;

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
            }

            let closure_needed = !close_over.is_empty();
            let mut new_new_vars = close_over;
            let mut new_preds = Vec::new();
            let (new_vars, preds) = if closure_needed || negated {
                (&mut new_new_vars, &mut new_preds)
            } else {
                (&mut *orig_new_vars, &mut *orig_preds)
            };

            to_expr_(
                *root,
                PredicateChaining::Equivalence,
                vars.clone(),
                new_vars,
                max_var,
                max_id,
                symbol_table,
                preds,
            );

            for (set, var) in sharers.into_iter().zip(vars) {
                for (chaining, pred_tree) in set {
                    match chaining {
                        PredicateChaining::Sharing => to_expr_(
                            pred_tree,
                            chaining,
                            vec![var],
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
                                    Vec::new(),
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
                                    Vec::new(),
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

            let preds = if negated {
                &mut new_preds
            } else {
                &mut *orig_preds
            };
            for p in and {
                let mut new_vars = Vec::new();
                let mut new_preds = Vec::new();
                to_expr_(
                    p,
                    PredicateChaining::Equivalence,
                    Vec::new(),
                    &mut new_vars,
                    max_var,
                    max_id,
                    symbol_table,
                    &mut new_preds,
                );

                let p = if new_preds.len() == 1 {
                    new_preds.pop().unwrap()
                } else {
                    Predicate::And { preds: new_preds }
                };
                if new_vars.is_empty() {
                    preds.push(p)
                } else {
                    preds.push(Predicate::Exists {
                        vars: new_vars,
                        pred: Box::new(p),
                    })
                }
            }

            if let Exposure::Explicit(vec) = &exposure {
                for (word, _) in vec.iter() {
                    symbol_table.get_mut(word).unwrap().pop();
                }
            }

            if closure_needed || negated {
                let p = if new_preds.len() == 1 {
                    new_preds.pop().unwrap()
                } else {
                    Predicate::And { preds: new_preds }
                };

                let p = if closure_needed {
                    Predicate::Exists {
                        vars: new_new_vars,
                        pred: Box::new(p),
                    }
                } else {
                    p
                };
                let p = if negated {
                    Predicate::Not(Box::new(p))
                } else {
                    p
                };
                orig_preds.push(p);
            }
        }
    }
}
