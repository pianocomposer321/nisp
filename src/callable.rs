use std::{
    fmt, rc::Rc, sync::atomic::{AtomicUsize, Ordering}
};

use crate::{
    expression::{eval_block, EvalError, Expr, Value},
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

pub struct FunctionDefn {
    args: Expr,
    body: Vec<Expr>,
    id: usize,
}

impl ToString for FunctionDefn {
    fn to_string(&self) -> String {
        format!("<Function {}>", self.id)
    }
}

impl FunctionDefn {
    pub fn new(args: Expr, body: Vec<Expr>) -> Self {
        Self {
            args,
            body,
            id: NEXT_ID.fetch_add(1, Ordering::SeqCst),
        }
    }

    pub fn args(&self) -> Result<Vec<Expr>, EvalError> {
        self.args.clone().as_list()
    }

    pub fn call(&self, scope: Scope) -> Result<Value, EvalError> {
        eval_block(scope, self.body.clone())
    }
}

impl fmt::Debug for FunctionDefn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FunctionDefn({:?})", self.args)
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

    fn sub(vals: Vec<Value>) -> Result<Value, EvalError> {
        let mut iter = vals.into_iter();

        let mut diff = iter.next().ok_or(EvalError::NotEnoughArgs {
            expected: 2,
            got: 0,
        })?.as_int()?;

        for value in iter {
            diff -= value.as_int()?;
        }

        Ok(Value::Int(diff))
    }

    fn geq(vals: Vec<Value>) -> Result<Value, EvalError> {
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
        Ok(Value::Bool(left.as_int()? >= right.as_int()?))
    }

    fn leq(vals: Vec<Value>) -> Result<Value, EvalError> {
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
        Ok(Value::Bool(left.as_int()? <= right.as_int()?))
    }

    fn gt(vals: Vec<Value>) -> Result<Value, EvalError> {
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
        Ok(Value::Bool(left.as_int()? > right.as_int()?))
    }

    fn lt(vals: Vec<Value>) -> Result<Value, EvalError> {
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
        Ok(Value::Bool(left.as_int()? < right.as_int()?))
    }

    fn assert(vals: Vec<Value>) -> Result<Value, EvalError> {
        let iter = vals.into_iter();

        for value in iter {
            if !value.as_bool()? {
                return Err(EvalError::AssertionFailed);
            }
        }

        Ok(Value::Unit)
    }

    fn string(vals: Vec<Value>) -> Result<Value, EvalError> {
        let mut iter = vals.into_iter();
        let value = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 1,
                got: 0,
            })?;

        Ok(Value::String(Rc::new(value.to_string())))
    }

    // TODO: make this return any value rather than just a bool
    fn and(vals: Vec<Value>) -> Result<Value, EvalError> {
        let mut res = true;
        for v in vals {
            res = res && v.as_bool()?;
            if !res {
                break;
            }
        }
        Ok(Value::Bool(res))
    }

    fn or(vals: Vec<Value>) -> Result<Value, EvalError> {
        let mut res = false;
        for v in vals {
            res = res || v.as_bool()?;
            if res {
                break;
            }
        }
        Ok(Value::Bool(res))
    }

    fn make_builtin(name: &str, body: impl BuiltinBodyFn) -> (String, Rc<Builtin>) {
        (name.to_string(), Rc::new(Builtin::new(name, FunctionBody::new(body))))
    }

    pub fn builtins() -> HashMap<String, Rc<Builtin>> {
        HashMap::from([
            make_builtin("+", add),
            make_builtin("add", add),
            make_builtin("-", sub),
            make_builtin("sub", sub),
            make_builtin("=", eq),
            make_builtin("eq", eq),
            make_builtin(">=", geq),
            make_builtin("geq", geq),
            make_builtin("<=", leq),
            make_builtin("leq", leq),
            make_builtin(">", gt),
            make_builtin("gt", gt),
            make_builtin("<", lt),
            make_builtin("lt", lt),
            make_builtin("and", and),
            make_builtin("or", or),
            make_builtin("print", print),
            make_builtin("assert", assert),
            make_builtin("string", string),
        ])
    }
}

pub mod default_intrinsics {
    use std::{collections::HashMap, rc::Rc};

    use super::*;

    fn set(mut scope: Scope, exprs: Vec<Expr>) -> Result<Value, EvalError> {
        let mut iter = exprs.into_iter();
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
            })?
            .eval(scope.clone())?;
        scope.pattern_match_assign(left, right)?;

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
            })?;
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

    fn match_(mut scope: Scope, exprs: Vec<Expr>) -> Result<Value, EvalError> {

        let mut iter = exprs.into_iter();
        let value = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 2,
                got: 0,
            })?
            .eval(scope.clone())?;
        let body = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 2,
                got: 1,
            })?
            .as_list()?;

        for window in body.chunks(2) {
            let (pattern, body) = (window[0].clone(), window[1].clone());

            match scope.pattern_match_assign(pattern, value.clone()) {
                Ok(_) => {
                    let child_scope = Scope::child(scope.clone());
                    let value = eval_block(child_scope.clone(), body.as_block()?)?;
                    dbg!(&value);
                    return Ok(value);
                },
                Err(EvalError::PatternMatchDoesNotMatch { left: _, right: _ }) => {
                    continue;
                },
                Err(e) => {
                    return Err(e);
                }
            }
        }

        Err(EvalError::NoMatchingPattern { right: value })
    }

    fn cond(scope: Scope, exprs: Vec<Expr>) -> Result<Value, EvalError> {
        let mut iter = exprs.into_iter();
        let conditions = iter
            .next()
            .ok_or(EvalError::NotEnoughArgs {
                expected: 2,
                got: 0,
            })?
            .as_list()?;

        for chunk in conditions.chunks(2) {
            let (condition, body) = (chunk[0].clone(), chunk[1].clone());

            let condition = condition.eval(scope.clone())?;
            if condition.as_bool()? {
                return body.eval(scope);
            }
        }

        Ok(Value::Unit)
    }

    fn make_intrinsic(name: &str, body: impl IntrinsicBodyFn) -> (String, Rc<Intrinsic>) {
        (name.to_string(), Rc::new(Intrinsic::new(name, IntrinsicBody::new(body))))
    }

    pub fn intrinsics() -> HashMap<String, Rc<Intrinsic>> {
        HashMap::from([
            make_intrinsic("set", set),
            make_intrinsic("defn", defn),
            make_intrinsic("if", if_),
            make_intrinsic("match", match_),
            make_intrinsic("cond", cond),
        ])
    }
}
