use std::{cell::RefMut, rc::Rc};

use thiserror::Error;

use crate::{callable::FunctionDefn, scope::{Scope, ScopeError}};

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("Type error: expected {expected:?} but got {got:?}")]
    TypeError { expected: String, got: String },
    #[error("Not enough arguments: expected {expected:?} but got {got:?}")]
    NotEnoughArgs { expected: usize, got: usize },
    #[error(transparent)]
    ScopeError(#[from] ScopeError),
    #[error("Pattern match length mismatch: expected {expected:?} but got {got:?}")]
    PatternMatchLengthMismatch { expected: usize, got: usize },

    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Call(String, Vec<Expr>),
    List(Vec<Expr>),
    Block(Vec<Expr>),
    Int(i64),
    Bool(bool),
    String(String),
    Symbol(String),
    Unit,
}

impl Expr {
    pub fn eval(self, scope: Scope) -> Result<Value, EvalError> {
        match self {
            Expr::Int(i) => Ok(Value::Int(i)),
            Expr::String(s) => Ok(Value::String(Rc::new(s))),
            Expr::Call(name, args) => {
                let mut child_scope = Scope::child(scope.clone());
                if let Some(builtin) = scope.get_builtin(&name) {
                    let values = Expr::values(child_scope.clone(), args)?;
                    return builtin.call(values);
                }
                if let Some(intrinsic) = scope.get_intrinsic(&name) {
                    return intrinsic.call(scope, args);
                }
                let function = scope.get_value(&name)?.as_function_defn()?;
                child_scope.pattern_match_assign(function.args(), args)?;
                function.call(child_scope.clone())
            }
            Expr::Symbol(name) => {
                if let Ok(value) = scope.get_value(&name) {
                    return Ok(value);
                }
                todo!()
            }
            Expr::Bool(b) => Ok(Value::Bool(b)),
            Expr::Block(b) => {
                let child_scope = Scope::child(scope.clone());
                let values = Expr::values(child_scope.clone(), b)?;

                Ok(values.into_iter().last().unwrap_or(Value::Unit))
            }
            _ => todo!(),
        }
    }

    pub fn values(scope: Scope, exprs: Vec<Expr>) -> Result<Vec<Value>, EvalError> {
        exprs
            .into_iter()
            .map(|e| e.eval(scope.clone()))
            .collect()
    }

    pub fn type_name(&self) -> String {
        match self {
            Expr::Int(_) => "Int".to_string(),
            Expr::String(_) => "String".to_string(),
            Expr::Call(_, _) => "Call".to_string(),
            Expr::List(_) => "List".to_string(),
            Expr::Block(_) => "Block".to_string(),
            Expr::Symbol(_) => "Symbol".to_string(),
            Expr::Bool(_) => "Bool".to_string(),
            Expr::Unit => "Unit".to_string(),
        }
    }

    pub fn as_int(self) -> Result<i64, EvalError> {
        match self {
            Expr::Int(i) => Ok(i),
            _ => Err(EvalError::TypeError {
                expected: "Int".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_string(self) -> Result<String, EvalError> {
        match self {
            Expr::String(s) => Ok(s),
            _ => Err(EvalError::TypeError {
                expected: "String".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_symbol(self) -> Result<String, EvalError> {
        match self {
            Expr::Symbol(s) => Ok(s),
            _ => Err(EvalError::TypeError {
                expected: "Symbol".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_list(self) -> Result<Vec<Expr>, EvalError> {
        match self {
            Expr::List(l) => Ok(l),
            _ => Err(EvalError::TypeError {
                expected: "List".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_block(self) -> Result<Vec<Expr>, EvalError> {
        match self {
            Expr::Block(b) => Ok(b),
            Expr::Call(_, _) => Ok(vec![self]),
            _ => Err(EvalError::TypeError {
                expected: "Block".to_string(),
                got: self.type_name(),
            }),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i64),
    String(Rc<String>),
    FunctionDefn(Rc<FunctionDefn>),
    Bool(bool),
    Unit,
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::String(s) => s.to_string(),
            Value::FunctionDefn(d) => d.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Unit => "()".to_string(),
        }
    }
}

impl Value {
    pub fn new_string(s: &str) -> Self {
        Value::String(Rc::new(s.to_string()))
    }

    pub fn as_int(self) -> Result<i64, EvalError> {
        match self {
            Value::Int(i) => Ok(i),
            _ => Err(EvalError::TypeError {
                expected: "Int".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_string(self) -> Result<Rc<String>, EvalError> {
        match self {
            Value::String(s) => Ok(s),
            _ => Err(EvalError::TypeError {
                expected: "String | Int | Unit".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_function_defn(self) -> Result<Rc<FunctionDefn>, EvalError> {
        match self {
            Value::FunctionDefn(f) => Ok(f),
            _ => Err(EvalError::TypeError {
                expected: "FunctionDefn".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_bool(self) -> Result<bool, EvalError> {
        match self {
            Value::Bool(b) => Ok(b),
            _ => Err(EvalError::TypeError {
                expected: "Bool".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn type_name(&self) -> String {
        match self {
            Value::Int(_) => "Int".to_string(),
            Value::String(_) => "String".to_string(),
            Value::FunctionDefn(_) => "FunctionDefn".to_string(),
            Value::Bool(_) => "Bool".to_string(),
            Value::Unit => "Unit".to_string(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::callable::{default_builtins, default_intrinsics};

    use super::*;

    #[derive(Error, Debug)]
    enum ExprTestError {
        #[error(transparent)]
        EvalError(#[from] EvalError),
        #[error(transparent)]
        ParserError(#[from] crate::parser::ParsingError),
        #[error(transparent)]
        LexerError(#[from] crate::lexer::LexingError),
    }

    type ExprTestResult<T> = Result<T, ExprTestError>;

    macro_rules! assert_next_value_eq {
        ($values:expr, $value:expr) => {
            let value = $values.next().unwrap();
            assert_eq!(value, $value);
        };
    }

    fn scope() -> Scope {
        Scope::from((
            default_builtins::builtins(),
            default_intrinsics::intrinsics(),
        ))
    }

    fn eval(scope: Scope, input: &str) -> ExprTestResult<Vec<Value>> {
        let mut lexer = crate::lexer::Lexer::from_string(input);
        let mut parser = crate::parser::Parser::new(lexer.iter().collect());
        let exprs = parser.parse_all()?;
        exprs
            .into_iter()
            .map(|e| e.eval(scope.clone()).map_err(|e| e.into()))
            .collect()
    }

    #[test]
    fn eval_int() -> ExprTestResult<()> {
        let scope = scope();
        let mut values = eval(scope, "42")?.into_iter();
        assert_next_value_eq!(values, Value::Int(42));

        Ok(())
    }

    #[test]
    fn eval_string() -> ExprTestResult<()> {
        let scope = scope();
        let mut values = eval(scope, "\"hello, world\"")?.into_iter();
        assert_next_value_eq!(values, Value::new_string("hello, world"));

        Ok(())
    }

    #[test]
    fn eval_call() -> ExprTestResult<()> {
        let scope = scope();
        let mut values = eval(scope, "(+ 1 2)")?.into_iter();
        assert_next_value_eq!(values, Value::Int(3));

        Ok(())
    }

    #[test]
    fn eval_multiple_args() -> ExprTestResult<()> {
        let scope = scope();
        let mut values = eval(scope, "(+ 1 2 3)")?.into_iter();
        assert_next_value_eq!(values, Value::Int(6));

        Ok(())
    }

    #[test]
    fn eval_variable() -> ExprTestResult<()> {
        let scope = scope();
        let mut values = eval(scope, "(set x 10) x")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);
        assert_next_value_eq!(values, Value::Int(10));

        Ok(())
    }

    #[test]
    fn eval_print() -> ExprTestResult<()> {
        let scope = scope();
        let mut values = eval(scope.clone(), "(print \"hello\")")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);

        let mut values = eval(scope.clone(), "(print \"hello\" \"world\")")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);

        Ok(())
    }

    #[test]
    fn eval_defn() -> ExprTestResult<()> {
        let scope = scope();
        let mut values = eval(scope.clone(), "(defn f [x] (+ x 1))")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);

        let mut values = eval(scope.clone(), "(f 1)")?.into_iter();
        assert_next_value_eq!(values, Value::Int(2));

        Ok(())
    }

    #[test]
    fn eval_defn_with_block() -> ExprTestResult<()> {
        let scope = scope();
        let mut values = eval(scope.clone(), "(defn f [x] { (set y (+ x 1)) (set z (+ x 2)) (+ y z) })")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);

        let mut values = eval(scope.clone(), "(f 1)")?.into_iter();
        assert_next_value_eq!(values, Value::Int(5));

        Ok(())
    }

    #[test]
    fn eval_if() -> ExprTestResult<()> {
        let scope = scope();
        let mut values = eval(scope.clone(), "(if true 1 2)")?.into_iter();
        assert_next_value_eq!(values, Value::Int(1));

        let mut values = eval(scope.clone(), "(if false 1 2)")?.into_iter();
        assert_next_value_eq!(values, Value::Int(2));

        Ok(())
    }

    #[test]
    fn eval_eq() -> ExprTestResult<()> {
        let scope = scope();
        let mut values = eval(scope.clone(), "(= 1 1)")?.into_iter();
        assert_next_value_eq!(values, Value::Bool(true));

        let mut values = eval(scope.clone(), "(= 1 2)")?.into_iter();
        assert_next_value_eq!(values, Value::Bool(false));

        Ok(())
    }

    #[test]
    fn eval_recursive_function() -> ExprTestResult<()> {
        let scope = scope();
        let mut values = eval(scope.clone(), "(defn f [x] (if (= x 10) 10 (f (+ x 1)))) (f 1)")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);
        assert_next_value_eq!(values, Value::Int(10));

        Ok(())
    }

    #[test]
    fn eval_block() -> ExprTestResult<()> {
        let scope = scope();
        let mut values = eval(scope.clone(), "(set x { 1 2 3 }) x")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);
        assert_next_value_eq!(values, Value::Int(3));

        Ok(())
    }
}
