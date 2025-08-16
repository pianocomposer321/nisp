use std::rc::Rc;

use thiserror::Error;

use crate::{
    callable::FunctionDefn,
    scope::{Scope, ScopeError},
};

pub fn eval_block(scope: Scope, exprs: Vec<Expr>) -> Result<Value, EvalError> {
    let values = Expr::values(scope.clone(), exprs)?;
    Ok(values.into_iter().last().unwrap_or(Value::Unit))
}

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

    #[error("Pattern match does not match: left {left:?} right {right:?}")]
    PatternMatchDoesNotMatch { left: Expr, right: Value },

    #[error("No matching pattern: {right:?}")]
    NoMatchingPattern { right: Value },

    #[error("Undefined variable: {name}")]
    UndefinedVariable { name: String },

    #[error("Undefined field on list {list:?}: {field}")]
    UndefinedField { list: Value, field: String },

    #[error("Index out of bounds on list {list:?}: {index}")]
    IndexOutOfBounds { list: Value, index: i64 },

    #[error("Assertion failed")]
    AssertionFailed,

    #[error(transparent)]
    ParserError(#[from] crate::parser::ParsingError),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Call(Rc<String>, Vec<Expr>),
    // TODO: this should cache marked pairs in a HashMap
    List(Vec<Expr>),
    Block(Vec<Expr>),
    Int(i64),
    Bool(bool),
    String(Rc<String>),
    Symbol(Rc<String>),
    SpreadOp(Rc<String>),
    MarkerPair(Rc<String>, Box<Expr>),
    DotOp(Box<Expr>, Box<Expr>),
    Unit,
}

impl Expr {
    pub fn eval(self, scope: Scope) -> Result<Value, EvalError> {
        match self {
            Expr::Int(i) => Ok(Value::Int(i)),
            Expr::String(s) => Ok(Value::String(s)),
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
                child_scope.pattern_match_assign(
                    Expr::List(function.args()?),
                    Value::new_list(Expr::values(scope.clone(), args)?),
                )?;
                function.call(child_scope.clone())
            }
            Expr::Symbol(name) => {
                if let Ok(value) = scope.get_value(&name) {
                    return Ok(value);
                }
                Err(EvalError::UndefinedVariable {
                    name: name.to_string(),
                })
            }
            Expr::Bool(b) => Ok(Value::Bool(b)),
            Expr::Block(b) => eval_block(Scope::child(scope), b),
            Expr::List(l) => {
                let values = Expr::values(scope.clone(), l)?;
                Ok(Value::new_list(values))
            }
            Expr::MarkerPair(marker, expr) => Ok(Value::MarkerPair(
                marker,
                Box::new(expr.eval(scope.clone())?),
            )),
            Expr::DotOp(left, right) => {
                let left = left.eval(scope.clone())?.as_list()?;
                match *right {
                    Expr::Symbol(name) => {
                        // TODO: this should use a cache instead of searching the list every time
                        for value in left.iter() {
                            if let Ok((marker, value)) = value.clone().as_marker_pair() {
                                if marker == name {
                                    return Ok(*value);
                                }
                            }
                        }
                        Err(EvalError::UndefinedField {
                            list: Value::List(left),
                            field: name.to_string(),
                        })
                    }
                    Expr::Int(ind) => {
                        if ind >= left.len() as i64 {
                            return Err(EvalError::IndexOutOfBounds {
                                list: Value::List(left),
                                index: ind,
                            });
                        }

                        let ind = ind as usize;
                        let value = left[ind].clone();
                        return Ok(value);
                    }
                    _ => {
                        return Err(EvalError::TypeError {
                            expected: "Symbol or Int".to_string(),
                            got: right.type_name(),
                        });
                    }
                }
            }
            _ => todo!(),
        }
    }

    pub fn new_call(name: &str, args: Vec<Expr>) -> Self {
        Self::Call(Rc::new(name.to_string()), args)
    }

    pub fn new_list(args: Vec<Expr>) -> Self {
        Self::List(args)
    }

    pub fn new_block(args: Vec<Expr>) -> Self {
        Self::Block(args)
    }

    pub fn new_int(i: i64) -> Self {
        Self::Int(i)
    }

    pub fn new_bool(b: bool) -> Self {
        Self::Bool(b)
    }

    pub fn new_string(s: &str) -> Self {
        Self::String(Rc::new(s.to_string()))
    }

    pub fn new_symbol(s: &str) -> Self {
        Self::Symbol(Rc::new(s.to_string()))
    }

    pub fn new_spread_op(s: &str) -> Self {
        Self::SpreadOp(Rc::new(s.to_string()))
    }

    pub fn new_unit() -> Self {
        Self::Unit
    }

    pub fn new_marker_pair(marker: impl Into<String>, expr: Expr) -> Self {
        Self::MarkerPair(Rc::new(marker.into()), Box::new(expr))
    }

    pub fn new_dot_op(left: Expr, right: Expr) -> Self {
        Self::DotOp(Box::new(left), Box::new(right))
    }

    pub fn values(scope: Scope, exprs: Vec<Expr>) -> Result<Vec<Value>, EvalError> {
        exprs.into_iter().map(|e| e.eval(scope.clone())).collect()
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
            Expr::SpreadOp(_) => "RestOp".to_string(),
            Expr::MarkerPair(_, _) => "MarkerPair".to_string(),
            Expr::DotOp(_, _) => "DotOp".to_string(),
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
            Expr::String(s) => Ok(nisp::unwrap_rc(s, |rc| rc.to_string())),
            _ => Err(EvalError::TypeError {
                expected: "String".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_symbol(self) -> Result<String, EvalError> {
        match self {
            Expr::Symbol(s) => Ok(nisp::unwrap_rc(s, |rc| rc.to_string())),
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
            _ => Ok(vec![self]),
        }
    }

    pub fn as_rest_op(self) -> Result<String, EvalError> {
        match self {
            Expr::SpreadOp(s) => Ok(nisp::unwrap_rc(s, |rc| rc.to_string())),
            _ => Err(EvalError::TypeError {
                expected: "RestOp".to_string(),
                got: self.type_name(),
            }),
        }
    }

    pub fn as_marker_pair(self) -> Result<(Rc<String>, Box<Expr>), EvalError> {
        match self {
            Expr::MarkerPair(marker, expr) => Ok((marker, expr)),
            _ => Err(EvalError::TypeError {
                expected: "MarkerPair".to_string(),
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

#[cfg(test)]
mod test {
    use std::fs::{File, read_to_string};

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

    fn make_scope() -> Scope {
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
        let scope = make_scope();
        let mut values = eval(scope, "42")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(42));

        Ok(())
    }

    #[test]
    fn eval_string() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope, "\"hello, world\"")?.into_iter();
        assert_next_value_eq!(values, Value::new_string("hello, world"));

        Ok(())
    }

    #[test]
    fn eval_call() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope, "(+ 1 2)")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(3));

        Ok(())
    }

    #[test]
    fn eval_multiple_args() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope, "(+ 1 2 3)")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(6));

        Ok(())
    }

    #[test]
    fn eval_variable() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope, "(set x 10) x")?.into_iter();
        assert_next_value_eq!(values, Value::new_unit());
        assert_next_value_eq!(values, Value::new_int(10));

        Ok(())
    }

    #[test]
    fn eval_print() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(print \"hello\")")?.into_iter();
        assert_next_value_eq!(values, Value::new_unit());

        let mut values = eval(scope.clone(), "(print \"hello\" \"world\")")?.into_iter();
        assert_next_value_eq!(values, Value::new_unit());

        Ok(())
    }

    #[test]
    fn eval_defn() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(defn f [x] (+ x 1))")?.into_iter();
        assert_next_value_eq!(values, Value::new_unit());

        let mut values = eval(scope.clone(), "(f 1)")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(2));

        Ok(())
    }

    #[test]
    fn eval_defn_with_block() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(
            scope.clone(),
            "(defn f [x] { (set y (+ x 1)) (set z (+ x 2)) (+ y z) })",
        )?
        .into_iter();
        assert_next_value_eq!(values, Value::new_unit());

        let mut values = eval(scope.clone(), "(f 1)")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(5));

        Ok(())
    }

    #[test]
    fn eval_if() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(if true 1 2)")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(1));

        let mut values = eval(scope.clone(), "(if false 1 2)")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(2));

        Ok(())
    }

    #[test]
    fn eval_eq() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(= 1 1)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(true));

        let mut values = eval(scope.clone(), "(= 1 2)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(false));

        Ok(())
    }

    #[test]
    fn eval_recursive_function() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(
            scope.clone(),
            "(defn f [x] (if (= x 10) 10 (f (+ x 1)))) (f 1)",
        )?
        .into_iter();
        assert_next_value_eq!(values, Value::new_unit());
        assert_next_value_eq!(values, Value::new_int(10));

        Ok(())
    }

    #[test]
    fn eval_block() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(set x { 1 2 3 }) x")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);
        assert_next_value_eq!(values, Value::new_int(3));

        Ok(())
    }

    #[test]
    fn eval_assert() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(assert (= 1 1) (= 2 2))")?.into_iter();
        assert_next_value_eq!(values, Value::new_unit());

        let values = eval(scope.clone(), "(assert (= 1 2))");
        assert!(matches!(
            values.unwrap_err(),
            ExprTestError::EvalError(EvalError::AssertionFailed)
        ));
        Ok(())
    }

    #[test]
    fn eval_sub() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(- 2 1)")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(1));

        let mut values = eval(scope.clone(), "(- 1 2)")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(-1));

        let mut values = eval(scope.clone(), "(- 1 2 3)")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(-4));

        Ok(())
    }

    #[test]
    fn eval_geq() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(>= 1 1)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(true));

        let mut values = eval(scope.clone(), "(>= 1 2)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(false));

        let mut values = eval(scope.clone(), "(>= 2 1)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(true));

        Ok(())
    }

    #[test]
    fn eval_leq() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(<= 1 1)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(true));

        let mut values = eval(scope.clone(), "(<= 1 2)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(true));

        let mut values = eval(scope.clone(), "(<= 2 1)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(false));

        Ok(())
    }

    #[test]
    fn eval_gt() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(> 1 1)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(false));

        let mut values = eval(scope.clone(), "(> 1 2)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(false));

        let mut values = eval(scope.clone(), "(> 2 1)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(true));

        Ok(())
    }

    #[test]
    fn eval_lt() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(< 1 1)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(false));

        let mut values = eval(scope.clone(), "(< 1 2)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(true));

        let mut values = eval(scope.clone(), "(< 2 1)")?.into_iter();
        assert_next_value_eq!(values, Value::new_bool(false));

        Ok(())
    }

    #[test]
    fn eval_test_files() -> ExprTestResult<()> {
        use walkdir::WalkDir;

        for entry in WalkDir::new("tests")
            .into_iter()
            .filter(|e| !e.as_ref().unwrap().path().is_dir())
        {
            let contents = read_to_string(entry.unwrap().path()).unwrap();
            let scope = make_scope();
            eval(scope.clone(), contents.as_str())?;
            let main = scope.get_value("main")?.as_function_defn()?;
            main.call(scope.clone())?;
        }

        Ok(())
    }

    #[test]
    fn eval_string_fn() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(string \"hello\")")?.into_iter();
        assert_next_value_eq!(values, Value::new_string("hello"));

        let mut values = eval(scope.clone(), "(string 123)")?.into_iter();
        assert_next_value_eq!(values, Value::new_string("123"));

        Ok(())
    }

    #[test]
    fn eval_list() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "[1 2 3]")?.into_iter();
        assert_next_value_eq!(
            values,
            Value::new_list(vec![
                Value::new_int(1),
                Value::new_int(2),
                Value::new_int(3)
            ])
        );

        Ok(())
    }

    #[test]
    fn list_to_string() -> ExprTestResult<()> {
        let list = Value::new_list(vec![
            Value::new_int(1),
            Value::new_int(2),
            Value::new_int(3),
        ]);
        assert_eq!(list.to_string(), "[1, 2, 3]");

        Ok(())
    }

    #[test]
    fn eval_match() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(
            scope.clone(),
            "(match 1 [ 1 \"one\" 2 \"two\" 3 \"three\" ])",
        )?
        .into_iter();
        assert_next_value_eq!(values, Value::new_string("one"));

        let mut values = eval(
            scope.clone(),
            "(match \"hello\" [ \"world\" 1 \"hello\" 2 ])",
        )?
        .into_iter();
        assert_next_value_eq!(values, Value::new_int(2));

        Ok(())
    }

    #[test]
    fn eval_match_with_block() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(match 2 [ 1 { (set x \"one\") x } 2 { (set x \"two\") x } 3 { (set x \"three\") x } ])")?.into_iter();
        assert_next_value_eq!(values, Value::new_string("two"));

        Ok(())
    }

    #[test]
    fn eval_match_pattern() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(
            scope.clone(),
            "(match [1 2 3] [ [x y] (+ x y) [x y 1] (+ x y) [1 x y] (+ x y) ])",
        )?
        .into_iter();
        assert_next_value_eq!(values, Value::new_int(5));

        Ok(())
    }

    #[test]
    fn eval_match_with_list_spread() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(
            scope.clone(),
            "(match [1 2 3] [ [x] (+ x 1) [x &rest] [x rest]])",
        )?
        .into_iter();
        assert_next_value_eq!(
            values,
            Value::new_list(vec![
                Value::new_int(1),
                Value::new_list(vec![Value::new_int(2), Value::new_int(3)])
            ])
        );

        Ok(())
    }

    #[test]
    fn eval_and() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(and true true true)")?.into_iter();
        assert_next_value_eq!(values, Value::Bool(true));

        let mut values = eval(scope.clone(), "(and true false true)")?.into_iter();
        assert_next_value_eq!(values, Value::Bool(false));

        let mut values = eval(scope.clone(), "(and false true true)")?.into_iter();
        assert_next_value_eq!(values, Value::Bool(false));

        let mut values = eval(scope.clone(), "(and false false false)")?.into_iter();
        assert_next_value_eq!(values, Value::Bool(false));

        Ok(())
    }

    #[test]
    fn eval_or() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(or true true true)")?.into_iter();
        assert_next_value_eq!(values, Value::Bool(true));

        let mut values = eval(scope.clone(), "(or true false true)")?.into_iter();
        assert_next_value_eq!(values, Value::Bool(true));

        let mut values = eval(scope.clone(), "(or false true true)")?.into_iter();
        assert_next_value_eq!(values, Value::Bool(true));

        let mut values = eval(scope.clone(), "(or false false false)")?.into_iter();
        assert_next_value_eq!(values, Value::Bool(false));

        Ok(())
    }

    #[test]
    fn eval_cond() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(cond [(= 1 1) 1])")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(1));

        let mut values = eval(scope.clone(), "(cond [(= 1 2) 1])")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);

        let mut values = eval(scope.clone(), "(cond [(= 1 1) 1 (= 1 2) 2])")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(1));

        let mut values = eval(scope.clone(), "(cond [(= 1 2) 1 (= 2 2) 2])")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(2));

        Ok(())
    }

    #[test]
    fn eval_set_pattern_match() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(set [a b] [1 2]) a b")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);
        assert_next_value_eq!(values, Value::new_int(1));
        assert_next_value_eq!(values, Value::new_int(2));

        Ok(())
    }

    #[test]
    fn eval_set_match() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(set-match [a b] [1 2]) a b")?.into_iter();
        assert_next_value_eq!(values, Value::Bool(true));
        assert_next_value_eq!(values, Value::new_int(1));
        assert_next_value_eq!(values, Value::new_int(2));

        let mut values = eval(scope.clone(), "(set-match [a b c] [1 2])")?.into_iter();
        assert_next_value_eq!(values, Value::Bool(false));

        let mut values = eval(scope.clone(), "(set-match [a &rest] [1 2 3]) a rest")?.into_iter();
        assert_next_value_eq!(values, Value::Bool(true));
        assert_next_value_eq!(values, Value::new_int(1));
        assert_next_value_eq!(
            values,
            Value::new_list(vec![Value::new_int(2), Value::new_int(3)])
        );

        Ok(())
    }

    #[test]
    fn eval_list_with_marker() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "[:key 123]")?.into_iter();
        assert_next_value_eq!(
            values,
            Value::new_list(vec![Value::new_marker_pair("key", Value::new_int(123))])
        );

        Ok(())
    }

    #[test]
    fn eval_match_with_marker() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(match [:key 123] [ [:key x] x ])")?.into_iter();
        assert_next_value_eq!(values, Value::new_int(123));

        Ok(())
    }

    #[test]
    fn eval_set_with_marker() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(set [:key x] [:key 123]) x")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);
        assert_next_value_eq!(values, Value::new_int(123));

        let scope = make_scope();
        let values = eval(scope.clone(), "(set [:key x] [:other-key 123]) x");
        assert!(matches!(
            values.unwrap_err(),
            ExprTestError::EvalError(EvalError::PatternMatchDoesNotMatch { .. })
        ));

        let scope = make_scope();
        let mut values = eval(
            scope.clone(),
            "(set [last-name] [:last-name \"Doe\"]) last-name",
        )?
        .into_iter();
        assert_next_value_eq!(values, Value::Unit);
        assert_next_value_eq!(values, Value::new_string("Doe"));

        let scope = make_scope();
        let mut values = eval(scope.clone(), "(set [x] [1]) x")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);
        assert_next_value_eq!(values, Value::new_int(1));

        let scope = make_scope();
        let values = eval(scope.clone(), "(set [first-name] [:last-name \"Doe\"])");
        assert!(matches!(
            values.unwrap_err(),
            ExprTestError::EvalError(EvalError::PatternMatchDoesNotMatch { .. })
        ));

        Ok(())
    }

    #[test]
    fn eval_marker_out_of_order() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(
            scope.clone(),
            "(set [world hello] [:hello 1 :world 2]) world hello",
        )?
        .into_iter();
        assert_next_value_eq!(values, Value::Unit);
        assert_next_value_eq!(values, Value::new_int(2));
        assert_next_value_eq!(values, Value::new_int(1));

        Ok(())
    }

    #[test]
    fn eval_kwargs() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(
            scope.clone(),
            "(set [&args end] [1 2 3 :end \"hello\"]) args end",
        )?
        .into_iter();
        assert_next_value_eq!(values, Value::Unit);
        assert_next_value_eq!(
            values,
            Value::new_list(vec![
                Value::new_int(1),
                Value::new_int(2),
                Value::new_int(3)
            ])
        );
        assert_next_value_eq!(values, Value::new_string("hello"));

        Ok(())
    }

    #[test]
    fn eval_dot() -> ExprTestResult<()> {
        let scope = make_scope();
        let mut values = eval(scope.clone(), "(set john-doe [:first-name \"John\" :last-name \"Doe\"]) john-doe.first-name john-doe.last-name")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);
        assert_next_value_eq!(values, Value::new_string("John"));
        assert_next_value_eq!(values, Value::new_string("Doe"));

        let scope = make_scope();
        let values = eval(
            scope.clone(),
            "(set john-doe [:first-name \"John\" :last-name \"Doe\"]) john-doe.middle-name",
        );
        assert!(matches!(
            values.unwrap_err(),
            ExprTestError::EvalError(EvalError::UndefinedField {
                list: Value::List(_),
                field: _
            })
        ));

        let scope = make_scope();
        let mut values = eval(scope.clone(), "(set x [1 2 3]) x.0 x.1 x.2")?.into_iter();
        assert_next_value_eq!(values, Value::Unit);
        assert_next_value_eq!(values, Value::new_int(1));
        assert_next_value_eq!(values, Value::new_int(2));
        assert_next_value_eq!(values, Value::new_int(3));

        let scope = make_scope();
        let values = eval(scope.clone(), "(set x [1 2 3]) x.3");
        assert!(matches!(
            values.unwrap_err(),
            ExprTestError::EvalError(EvalError::IndexOutOfBounds {
                list: Value::List(_),
                index: 3
            })
        ));

        Ok(())
    }
}
