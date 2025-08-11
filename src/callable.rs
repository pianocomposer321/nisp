use std::{
    fmt,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::{
    expression::{EvalError, Expr, Value},
    scope::Scope,
};

#[derive(Debug)]
pub struct Intrinsic {
    pub name: String,
    body: IntrinsicBody,
}

impl Intrinsic {
    pub fn new(name: &str, body: IntrinsicBody) -> Self {
        Self {
            name: name.to_string(),
            body,
        }
    }

    pub fn call(&self, scope: Scope, args: Vec<Expr>) -> Result<Value, EvalError> {
        (self.body.0)(scope, args)
    }
}

pub trait IntrinsicBodyFn: Fn(Scope, Vec<Expr>) -> Result<Value, EvalError> + 'static {}
impl<T> IntrinsicBodyFn for T where T: Fn(Scope, Vec<Expr>) -> Result<Value, EvalError> + 'static {}

pub struct IntrinsicBody(Box<dyn IntrinsicBodyFn>);

impl IntrinsicBody {
    pub fn new(f: impl IntrinsicBodyFn) -> Self {
        Self(Box::new(f))
    }
}

impl fmt::Debug for IntrinsicBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "IntrinsicBody")
    }
}

#[derive(Debug)]
pub struct Builtin {
    pub name: String,
    body: FunctionBody,
}

impl Builtin {
    pub fn new(name: &str, body: FunctionBody) -> Self {
        Self {
            name: name.to_string(),
            body,
        }
    }

    pub fn call(&self, scope: Scope, args: Vec<Value>) -> Result<Value, EvalError> {
        (self.body.0)(scope, args)
    }
}

static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
pub struct FunctionDefn {
    args: Expr,
    body: FunctionBody,
    id: usize,
}

impl FunctionDefn {
    pub fn new(args: Expr, body: FunctionBody) -> Self {
        Self {
            args,
            body,
            id: NEXT_ID.fetch_add(1, Ordering::SeqCst),
        }
    }

    pub fn call(&self, scope: Scope, args: Vec<Value>) -> Result<Value, EvalError> {
        (self.body.0)(scope, args)
    }
}

impl PartialEq for FunctionDefn {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

pub trait FunctionBodyFn: Fn(Scope, Vec<Value>) -> Result<Value, EvalError> + 'static {}
impl<T> FunctionBodyFn for T where T: Fn(Scope, Vec<Value>) -> Result<Value, EvalError> + 'static {}

pub struct FunctionBody(Box<dyn FunctionBodyFn>);

impl FunctionBody {
    pub fn new(f: impl FunctionBodyFn) -> Self {
        Self(Box::new(f))
    }
}

impl fmt::Debug for FunctionBody {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FunctionBody")
    }
}

pub mod default_builtins {
    use std::{collections::HashMap, rc::Rc};

    use super::*;

    fn add(_: Scope, vals: Vec<Value>) -> Result<Value, EvalError> {
        let mut sum: i64 = 0;
        for v in vals {
            sum += v.as_int()? as i64;
        }
        Ok(Value::Int(sum))
    }

    fn make_builtin(name: &str, body: impl FunctionBodyFn) -> (String, Rc<Builtin>) {
        (name.to_string(), Rc::new(Builtin::new(name, FunctionBody::new(body))))
    }

    pub fn builtins() -> HashMap<String, Rc<Builtin>> {
        HashMap::from([
            make_builtin("+", add),
        ])
    }
}

pub mod default_intrinsics {
    use std::{collections::HashMap, rc::Rc};

    use super::*;

    fn set(mut scope: Scope, exprs: Vec<Expr>) -> Result<Value, EvalError> {
        let mut iter = exprs.into_iter();
        let variable_name: String = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 2,
                got: 0,
            })?
            .as_symbol()?;
        let value = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 2,
                got: 1,
            })?
            .eval(scope.clone())?;
        scope.set_value(&variable_name, value);

        Ok(Value::Unit)
    }

    fn make_intrinsic(name: &str, body: impl IntrinsicBodyFn) -> (String, Rc<Intrinsic>) {
        (name.to_string(), Rc::new(Intrinsic::new(name, IntrinsicBody::new(body))))
    }

    pub fn intrinsics() -> HashMap<String, Rc<Intrinsic>> {
        HashMap::from([
            make_intrinsic("set", set),
        ])
    }
}
