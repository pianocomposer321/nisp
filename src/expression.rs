use std::{collections::HashMap, rc::Rc};

use thiserror::Error;

use crate::callable::{Builtin, FunctionDefn};

#[derive(Clone, Debug)]
pub struct Scope {
    builtins: Rc<HashMap<String, Builtin>>,
    parent: Option<Rc<Scope>>,
    values: Rc<HashMap<String, Value>>,
}

impl Scope {
    pub fn new(builtins: Vec<Builtin>) -> Self {
        Self {
            builtins: Rc::new(builtins.into_iter().map(|b| (b.name.clone(), b)).collect()),
            parent: None,
            values: Rc::new(HashMap::new()),
        }
    }

    pub fn get_builtin(&self, name: &str) -> Option<&Builtin> {
        self.builtins.get(name)
    }

    pub fn get_value(&self, name: &str) -> Option<&Value> {
        self.values.get(name)
    }
}

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("Type error: expected {expected:?} but got {got:?}")]
    TypeError { expected: String, got: String },
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
            Expr::String(s) => Ok(Value::String(s)),
            Expr::Call(name, args) => {
                if let Some(builtin) = scope.get_builtin(&name) {
                    let values = args
                        .into_iter()
                        .map(|a| a.eval(scope.clone()))
                        .collect::<Result<Vec<_>, _>>()?;
                    return builtin.call(scope.clone(), values);
                }
                todo!()
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Int(i64),
    String(String),
    FunctionDefn(FunctionDefn),
    Unit,
}

impl Value {
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
            Value::String(s) => Ok(s.clone()),
            _ => Err(EvalError::TypeError {
                expected: "String".to_string(),
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
    use crate::callable::FunctionBody;

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
        let mut builtins = Vec::new();
        builtins.push(Builtin::new(
            "+",
            FunctionBody::new(|_, vals| {
                let mut sum = 0;
                for v in vals {
                    sum += v.as_int()?;
                }
                Ok(Value::Int(sum))
            }),
        ));
        Scope::new(builtins)
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
        assert_next_value_eq!(values, Value::String("hello, world".to_string()));

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
}
