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

    pub fn call(&self, args: Vec<Value>) -> Result<Value, EvalError> {
        (self.body.0)(args)
    }
}

static NEXT_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
pub struct FunctionDefn {
    args: Vec<Expr>,
    body: Vec<Expr>,
    id: usize,
}

impl ToString for FunctionDefn {
    fn to_string(&self) -> String {
        format!("<Function {}>", self.id)
    }
}

impl FunctionDefn {
    pub fn new(args: Vec<Expr>, body: Vec<Expr>) -> Self {
        Self {
            args,
            body,
            id: NEXT_ID.fetch_add(1, Ordering::SeqCst),
        }
    }

    pub fn args(&self) -> &[Expr] {
        &self.args
    }

    pub fn call(&self, scope: Scope) -> Result<Value, EvalError> {
        let values = Expr::values(scope.clone(), self.body.clone())?;
        Ok(values.into_iter().last().unwrap_or(Value::Unit).clone())
    }
}

impl PartialEq for FunctionDefn {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

pub trait BuiltinBodyFn: Fn(Vec<Value>) -> Result<Value, EvalError> + 'static {}
impl<T> BuiltinBodyFn for T where T: Fn(Vec<Value>) -> Result<Value, EvalError> + 'static {}

pub struct FunctionBody(Box<dyn BuiltinBodyFn>);

impl FunctionBody {
    pub fn new(f: impl BuiltinBodyFn) -> Self {
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

    fn add(vals: Vec<Value>) -> Result<Value, EvalError> {
        let mut sum: i64 = 0;
        for v in vals {
            sum += v.as_int()? as i64;
        }
        Ok(Value::Int(sum))
    }

    fn print(vals: Vec<Value>) -> Result<Value, EvalError> {
        let mut iter = vals.into_iter();
        let value = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 1,
                got: 0,
            })?
            .as_string()?;
        print!("{}", value);

        for value in iter {
            print!(" {}", value.as_string()?);
        }
        println!();

        Ok(Value::Unit)
    }

    fn eq(vals: Vec<Value>) -> Result<Value, EvalError> {
        let mut iter = vals.into_iter();
        let left = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 2,
                got: 0,
            })?;
        let right = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 2,
                got: 1,
            })?;
        Ok(Value::Bool(left == right))
    }

    fn make_builtin(name: &str, body: impl BuiltinBodyFn) -> (String, Rc<Builtin>) {
        (name.to_string(), Rc::new(Builtin::new(name, FunctionBody::new(body))))
    }

    pub fn builtins() -> HashMap<String, Rc<Builtin>> {
        HashMap::from([
            make_builtin("+", add),
            make_builtin("print", print),
            make_builtin("=", eq),
            make_builtin("eq", eq),
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

    fn defn(mut scope: Scope, exprs: Vec<Expr>) -> Result<Value, EvalError> {
        let mut iter = exprs.into_iter();
        let function_name: String = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 3,
                got: 0,
            })?
            .as_symbol()?;
        let args = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 3,
                got: 1,
            })?
            .as_list()?;
        let body: Vec<Expr> = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 3,
                got: 2,
            })?
            .as_block()?;

        let function = FunctionDefn::new(args, body);
        scope.set_value(&function_name, Value::FunctionDefn(Rc::new(function)));

        Ok(Value::Unit)
    }

    fn if_(scope: Scope, exprs: Vec<Expr>) -> Result<Value, EvalError> {
        let mut iter = exprs.into_iter();
        let condition = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 3,
                got: 0,
            })?
            .eval(scope.clone())?;
        let left = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 3,
                got: 1,
            })?;
        let right = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 3,
                got: 2,
            })?;

        if condition.as_bool()? {
            left.eval(scope.clone())
        } else {
            right.eval(scope.clone())
        }
    }

    fn make_intrinsic(name: &str, body: impl IntrinsicBodyFn) -> (String, Rc<Intrinsic>) {
        (name.to_string(), Rc::new(Intrinsic::new(name, IntrinsicBody::new(body))))
    }

    pub fn intrinsics() -> HashMap<String, Rc<Intrinsic>> {
        HashMap::from([
            make_intrinsic("set", set),
            make_intrinsic("defn", defn),
            make_intrinsic("if", if_),
        ])
    }
}
