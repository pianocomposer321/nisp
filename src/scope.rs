use std::{
    cell::RefCell, collections::{HashMap, HashSet}, ops::Deref, rc::Rc
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
            (Expr::List(exprs), values) => {
                if let Value::List(values) = values {
                    let mut found_rest = false;
                    for (ind, expr) in exprs.deref().clone().into_iter().enumerate() {
                        // left: [:marker expr] right: [:marker value]
                        if let Ok((marker, expr)) = expr.clone().as_marker_pair() {
                            if let Some(value) = values.get_field(marker.clone()) {
                                self.pattern_match_assign(*expr, value.clone())?;
                                continue;
                            } else {
                                return Err(EvalError::PatternMatchDoesNotMatch {
                                    right: Value::List(values),
                                    left: Expr::List(exprs),
                                });
                            }
                        }

                        // left: [marker] right: [:marker value]
                        if let Ok(name) = expr.clone().as_symbol() {
                            if let Some(value) = values.get_field(Rc::new(name.clone())) {
                                self.pattern_match_assign(Expr::new_symbol(&name), value.clone())?;
                                continue;
                            }

                            // left: [last-name] right: [:first-name "John"]
                            if let Ok(_) = values.get(ind).ok_or(EvalError::PatternMatchDoesNotMatch { left: Expr::List(exprs.clone()), right: Value::List(values.clone()) })?.clone().as_marker_pair() {
                                return Err(EvalError::PatternMatchDoesNotMatch {
                                    right: Value::List(values),
                                    left: Expr::List(exprs),
                                });
                            }
                        }

                        // left: [&rest] right: [1 2 3]
                        if let Ok(rest_name) = expr.clone().as_rest_op() {
                            let mut rest_values = Vec::new();

                            let mut not_in_rest = HashSet::new();

                            for expr in exprs.iter().skip(ind + 1) {
                                if let Ok(name) = expr.clone().as_symbol() && let Some((value, ind)) = values.get_field_with_ind(Rc::new(name)) {
                                    self.pattern_match_assign(expr.clone(), value)?;
                                    not_in_rest.insert(ind);
                                    continue;
                                }

                                if let Ok((marker, expr)) = expr.clone().as_marker_pair() && let Some((value, ind)) = values.get_field_with_ind(marker) {
                                    self.pattern_match_assign(*expr, value)?;
                                    not_in_rest.insert(ind);
                                    continue;
                                }
                            }

                            for (ind, value) in values.iter().enumerate().skip(ind) {
                                if not_in_rest.contains(&ind) {
                                    continue;
                                }
                                rest_values.push(value.clone());
                            }

                            self.pattern_match_assign(Expr::new_symbol(&rest_name), Value::new_list(rest_values))?;
                            found_rest = true;
                            break;
                        }
                        
                        // left: [expr] right: [value]
                        self.pattern_match_assign(expr, values[ind].clone())?;
                    }

                    if (found_rest && exprs.len() > values.len() + 1) || (!found_rest && exprs.len() != values.len()) {
                        return Err(EvalError::PatternMatchDoesNotMatch { left, right });
                    }

                    Ok(())
                } else {
                    Err(EvalError::PatternMatchDoesNotMatch { left, right })
                }
            }
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
