use std::{cell::RefMut, rc::Rc};

use thiserror::Error;

use crate::{callable::Function, scope::{Scope, ScopeError}};

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("Type error: expected {expected:?} but got {got:?}")]
    TypeError { expected: String, got: String },
    #[error("Not enough arguments: expected {expected:?} but got {got:?}")]
    NotEnoughArgs { expected: usize, got: usize },
    #[error(transparent)]
    ScopeError(#[from] ScopeError),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Call(String, Vec<Expr>),
    List(Vec<Expr>),
    Block(Vec<Expr>),
    Int(i64),
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
                let child_scope = Scope::child(scope.clone());
                if let Some(builtin) = scope.get_builtin(&name) {
                    let values = args
                        .into_iter()
                        .map(|a| a.eval(child_scope.clone()))
                        .collect::<Result<Vec<_>, _>>()?;
                    return builtin.call(scope, values);
                }
                if let Some(intrinsic) = scope.get_intrinsic(&name) {
                    return intrinsic.call(scope, args);
                }
                todo!()
            }
            Expr::Symbol(name) => {
                if let Some(value) = scope.get_value(&name) {
                    return Ok(value);
                }
                todo!()
            }
            _ => todo!(),
        }
    }

    pub fn type_name(&self) -> String {
        match self {
            Expr::Int(_) => "Int".to_string(),
            Expr::String(_) => "String".to_string(),
            Expr::Call(_, _) => "Call".to_string(),
            Expr::List(_) => "List".to_string(),
            Expr::Block(_) => "Block".to_string(),
            Expr::Symbol(_) => "Symbol".to_string(),
            Expr::Unit => "Unit".to_string(),
        }
    }

    pub fn as_int(&self) -> Result<i64, EvalError> {
        match self {
            Expr::Int(i) => Ok(*i),
            _ => Err(EvalError::TypeError {
                expected: "Int".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_string(&self) -> Result<String, EvalError> {
        match self {
            Expr::String(s) => Ok(s.clone()),
            _ => Err(EvalError::TypeError {
                expected: "String".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_symbol(&self) -> Result<String, EvalError> {
        match self {
            Expr::Symbol(s) => Ok(s.clone()),
            _ => Err(EvalError::TypeError {
                expected: "Symbol".to_string(),
                got: self.type_name(),
            }),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i64),
    String(Rc<String>),
    FunctionDefn(Rc<Function>),
    Unit,
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::String(s) => s.to_string(),
            Value::FunctionDefn(d) => d.to_string(),
            Value::Unit => "()".to_string(),
        }
    }
}

impl Value {
    pub fn new_string(s: &str) -> Self {
        Value::String(Rc::new(s.to_string()))
    }

    pub fn as_int(&self) -> Result<i64, EvalError> {
        match self {
            Value::Int(i) => Ok(*i),
            _ => Err(EvalError::TypeError {
                expected: "Int".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_string(&self) -> Result<String, EvalError> {
        match self {
            Value::String(s) => Ok(s.to_string()),
            _ => Err(EvalError::TypeError {
                expected: "String | Int | Unit".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn type_name(&self) -> String {
        match self {
            Value::Int(_) => "Int".to_string(),
            Value::String(_) => "String".to_string(),
            Value::FunctionDefn(_) => "FunctionDefn".to_string(),
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
    fn eval_print() -> Result<(), ()> {
        let scope = scope();
        let mut values = eval(scope.clone(), "(print \"hello\")").unwrap().into_iter();
        assert_next_value_eq!(values, Value::Unit);

        let mut values = eval(scope.clone(), "(print \"hello\" \"world\")").unwrap().into_iter();
        assert_next_value_eq!(values, Value::Unit);

        Ok(())
    }
}
