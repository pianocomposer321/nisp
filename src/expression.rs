use thiserror::Error;

#[derive(Error, Debug)]
pub enum EvalError {
    
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

#[derive(Debug, PartialEq, Eq)]
pub enum Value {
    Int(i64),
    String(String),
    Unit,
}

impl Expr {
    pub fn eval(self) -> Result<Value, EvalError> {
        match self {
            Expr::Int(i) => Ok(Value::Int(i)),
            Expr::String(s) => Ok(Value::String(s)),
            _ => todo!()
        }
    }
}

#[cfg(test)]
mod test {
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

    fn eval(input: &str) -> ExprTestResult<Vec<Value>> {
        let mut lexer = crate::lexer::Lexer::from_string(input);
        let mut parser = crate::parser::Parser::new(lexer.iter().collect());
        let exprs = parser.parse_all()?;
        exprs.into_iter().map(|e| e.eval().map_err(|e| e.into())).collect()
    }

    #[test]
    fn eval_int() -> ExprTestResult<()> {
        let mut values = eval("42")?.into_iter();
        assert_next_value_eq!(values, Value::Int(42));

        Ok(())
    }

    #[test]
    fn eval_string() -> ExprTestResult<()> {
        let mut values = eval("\"hello, world\"")?.into_iter();
        assert_next_value_eq!(values, Value::String("hello, world".to_string()));

        Ok(())
    }
}
