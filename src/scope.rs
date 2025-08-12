use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    rc::Rc,
};

use thiserror::Error;

use crate::{
    callable::{Builtin, BuiltinBodyFn, FunctionBody, Intrinsic, IntrinsicBody, IntrinsicBodyFn},
    expression::{EvalError, Expr, Value},
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

    pub fn from(builtins_and_intrinsics: (HashMap<String, Rc<Builtin>>, HashMap<String, Rc<Intrinsic>>)) -> Self {
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
            Err(EvalError::UndefinedVariable { name: name.to_string() })
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
    pub fn pattern_match_assign(&mut self, left: &[Expr], right: Vec<Expr>) -> Result<(), EvalError> {
        if left.len() != right.len() {
            return Err(EvalError::PatternMatchLengthMismatch {
                expected: left.len(),
                got: right.len(),
            });
        } else {
            for (l, r) in left.into_iter().zip(right.into_iter()) {
                match (l, r) {
                    (Expr::Symbol(l), r) => {
                        self.set_value(&l, r.eval(self.clone())?);
                    }
                    _ => todo!(),
                }
            }
            Ok(())
        }
    }
}
