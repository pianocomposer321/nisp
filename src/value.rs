use std::rc::Rc;

use crate::{callable::FunctionDefn, expression::EvalError};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Int(i64),
    String(Rc<String>),
    FunctionDefn(Rc<FunctionDefn>),
    Bool(bool),
    List(Rc<Vec<Value>>),
    MarkerPair(Rc<String>, Box<Value>),
    Unit,
}

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::String(s) => s.to_string(),
            Value::FunctionDefn(d) => d.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::List(l) => {
                let mut s = String::new();
                s.push('[');
                let mut iter = l.iter();
                if let Some(first) = iter.next() {
                    s.push_str(&first.to_string());
                }
                for value in iter {
                    s.push_str(", ");
                    s.push_str(&value.to_string());
                }
                s.push(']');

                s
            }
            Value::MarkerPair(marker, value) => {
                let mut s = String::new();
                s.push(':');
                s.push_str(&marker.to_string());
                s.push(' ');
                s.push_str(&value.to_string());

                s
            }
            Value::Unit => "()".to_string(),
        }
    }
}

impl Value {
    pub fn new_int(i: i64) -> Self {
        Value::Int(i)
    }

    pub fn new_string(s: &str) -> Self {
        Value::String(Rc::new(s.to_string()))
    }

    pub fn new_function_defn(f: FunctionDefn) -> Self {
        Value::FunctionDefn(Rc::new(f))
    }

    pub fn new_bool(b: bool) -> Self {
        Value::Bool(b)
    }

    pub fn new_list(l: Vec<Value>) -> Self {
        Value::List(Rc::new(l))
    }

    pub fn new_marker_pair(marker: impl Into<String>, value: Value) -> Self {
        Value::MarkerPair(Rc::new(marker.into()), Box::new(value))
    }

    pub fn new_unit() -> Self {
        Value::Unit
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
                expected: "String".to_string(),
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

    pub fn as_marker_pair(self) -> Result<(Rc<String>, Box<Value>), EvalError> {
        match self {
            Value::MarkerPair(marker, value) => Ok((marker, value)),
            _ => Err(EvalError::TypeError {
                expected: "MarkerPair".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_list(self) -> Result<Rc<Vec<Value>>, EvalError> {
        match self {
            Value::List(l) => Ok(l),
            _ => Err(EvalError::TypeError {
                expected: "List".to_string(),
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
            Value::List(_) => "List".to_string(),
            Value::MarkerPair(_, _) => "MarkerPair".to_string(),
            Value::Unit => "Unit".to_string(),
        }
    }
}
