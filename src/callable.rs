use std::{
    fmt, rc::Rc, sync::atomic::{AtomicUsize, Ordering}
};

use crate::expression::{EvalError, Expr, Scope, Value};

#[derive(Debug)]
pub struct Builtin {
    pub name: String,
    body: FunctionBody,
}

impl Builtin {
    pub fn new(name: &str, body: FunctionBody) -> Self {
        Self { name: name.to_string(), body }
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
