use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    ops::Deref,
    rc::Rc,
};

use thiserror::Error;

use crate::{
    callable::{Builtin, BuiltinBodyFn, FunctionBody, Intrinsic, IntrinsicBody, IntrinsicBodyFn},
    expression::{EvalError, Expr},
    value::Value,
};

#[derive(Error, Debug)]
pub enum ScopeError {
    #[error("No parent scope")]
    NoParent,
}

#[derive(Clone, Debug)]
pub struct Scope {
    builtins: HashMap<String, Rc<Builtin>>,
    intrinsics: HashMap<String, Rc<Intrinsic>>,
    parent: Option<Rc<RefCell<Scope>>>,
    values: Rc<RefCell<HashMap<String, Value>>>,
}

impl Scope {
    pub fn new(
        builtins: Vec<(&str, Box<dyn BuiltinBodyFn>)>,
        intrinsics: Vec<(&str, Box<dyn IntrinsicBodyFn>)>,
    ) -> Self {
        let mut builtins_ = HashMap::new();
        for builtin in builtins {
            builtins_.insert(
                builtin.0.to_string(),
                Rc::new(Builtin::new(builtin.0, FunctionBody::new(builtin.1))),
            );
        }

        let mut intrinsics_ = HashMap::new();
        for intrinsic in intrinsics {
            intrinsics_.insert(
                intrinsic.0.to_string(),
                Rc::new(Intrinsic::new(intrinsic.0, IntrinsicBody::new(intrinsic.1))),
            );
        }

        Self {
            builtins: builtins_,
            intrinsics: intrinsics_,
            parent: None,
            values: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn from(
        builtins_and_intrinsics: (HashMap<String, Rc<Builtin>>, HashMap<String, Rc<Intrinsic>>),
    ) -> Self {
        let (builtins, intrinsics) = builtins_and_intrinsics;
        Self {
            builtins,
            intrinsics,
            parent: None,
            values: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn child(parent: Scope) -> Scope {
        Scope {
            builtins: parent.builtins.clone(),
            intrinsics: parent.intrinsics.clone(),
            parent: Some(Rc::new(RefCell::new(parent))),
            values: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    pub fn get_builtin(&self, name: &str) -> Option<Rc<Builtin>> {
        self.builtins.get(name).cloned()
    }

    pub fn get_intrinsic(&self, name: &str) -> Option<Rc<Intrinsic>> {
        self.intrinsics.get(name).cloned()
    }

    pub fn get_value(&self, name: &str) -> Result<Value, EvalError> {
        if let Some(value) = self.values.borrow().get(name).cloned() {
            Ok(value)
        } else if let Some(parent) = &self.parent
            && let Ok(value) = parent.borrow().get_value(name)
        {
            Ok(value)
        } else {
            Err(EvalError::UndefinedVariable {
                name: name.to_string(),
            })
        }
    }

    pub fn set_value(&mut self, name: &str, value: Value) {
        self.values.borrow_mut().insert(name.to_string(), value);
    }

    pub fn set_value_on_parent(&mut self, name: &str, value: Value) -> Result<(), ScopeError> {
        if let Some(parent) = &self.parent {
            parent.borrow_mut().set_value(name, value);
            println!("{:?}", parent.borrow().get_value(name));
            Ok(())
        } else {
            Err(ScopeError::NoParent)
        }
    }

    // TODO: this will be more complicated later
    pub fn pattern_match_assign(&mut self, left: Expr, right: Value) -> Result<(), EvalError> {
        match (left.clone(), right.clone()) {
            (Expr::Int(l), r) => {
                if let Value::Int(r) = r
                    && l == r
                {
                    Ok(())
                } else {
                    Err(EvalError::PatternMatchDoesNotMatch { left, right })
                }
            }
            (Expr::Symbol(l), r) => {
                self.set_value(&l, r);
                Ok(())
            }
            (Expr::Bool(l), r) => {
                if let Value::Bool(r) = r
                    && l == r
                {
                    Ok(())
                } else {
                    Err(EvalError::PatternMatchDoesNotMatch { left, right })
                }
            }
            (Expr::Unit, r) => {
                if let Value::Unit = r {
                    Ok(())
                } else {
                    Err(EvalError::PatternMatchDoesNotMatch { left, right })
                }
            }
            (Expr::String(l), r) => {
                if let Value::String(r) = r
                    && l == r
                {
                    Ok(())
                } else {
                    Err(EvalError::PatternMatchDoesNotMatch { left, right })
                }
            }
            // TODO: consider allowing pattern matching on marker pairs
            // That would make this branch much simpler, but would change semantics
            // of marker pairs
            (Expr::List(exprs), value) => {
                let values = value.clone().as_list()?;

                let mut values_iter = values.iter();
                for expr in exprs.iter() {
                    if let Ok(expr) = expr.clone().as_list_tail() {
                        let remaining_values: Vec<Value> = values_iter.by_ref().cloned().collect();
                        self.pattern_match_assign(Expr::new_symbol(&expr), Value::new_list(remaining_values))?;
                        break;
                    }

                    let next_value = values_iter.next().ok_or(EvalError::PatternMatchDoesNotMatch {
                        left: Expr::List(exprs.clone()),
                        right: value.clone(),
                    })?;

                    let pair = if let Ok(name) = expr.clone().as_symbol() {
                        Some((Rc::new(name.clone()), Expr::new_symbol(&name)))
                    } else if let Ok((name, expr)) = expr.clone().as_marker_pair() {
                        Some((name, *expr))
                    } else { None };

                    if let Some((lookup_name, set_name)) = pair {
                        if let Some(value) = values.get_field(lookup_name) {
                            self.pattern_match_assign(set_name, value.clone())?;
                            continue;
                        }
                    }

                    self.pattern_match_assign(expr.clone(), next_value.clone())?;
                }
                if values_iter.next().is_some() {
                    return Err(EvalError::PatternMatchDoesNotMatch {
                        left: Expr::List(exprs),
                        right: value.clone(),
                    });
                }

                Ok(())
            },
            (Expr::MarkerPair(left_marker, expr), r) => {
                let (right_marker, value) = r.as_marker_pair()?;
                if left_marker == right_marker {
                    self.pattern_match_assign(*expr, *value)?;
                    Ok(())
                } else {
                    Err(EvalError::PatternMatchDoesNotMatch {
                        right: *value,
                        left,
                    })
                }
            }
            (left, _) => Err(EvalError::TypeError {
                expected: "Symbol, Int, String, Bool, Unit, List, or MarkerPair".to_string(),
                got: left.type_name(),
            }),
        }
    }
}
