use crate::lexer::{self, Token};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParsingError {
    #[error("Unexpected End Of File while parsing")]
    UnexpectedEOF,
    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(Token)
}

type ParsingResult<T> = Result<T, ParsingError>;

#[derive(Debug, PartialEq, Eq)]
pub enum Expr {
    Call(String, Vec<Expr>),
    List(Vec<Expr>),
    Int(i64),
    String(String),
    Symbol(String),
    Unit,
}

pub struct Parser {
    tokens: Vec<lexer::Token>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<lexer::Token>) -> Self {
        Self { tokens, position: 0 }
    }

    pub fn parse_all(&mut self) -> ParsingResult<Vec<Expr>> {
        let mut exprs = vec![];
        while let Ok(expr) = self.parse_next_expr() {
            dbg!(&expr);
            exprs.push(expr);
        }
        Ok(exprs)
    }

    pub fn parse_next_expr(&mut self) -> ParsingResult<Expr> {
        dbg!(&self.tokens);

        let token = self.get_token();
        if !token.is_ok() {
            self.advance();
            return Ok(Expr::Unit);
        }
        let token = token?;
        match token {
            Token::IntLiteral(i) => {
                self.advance();
                Ok(Expr::Int(i))
            },
            Token::StringLiteral(s) => {
                self.advance();
                Ok(Expr::String(s))
            }
            Token::Symbol(s) => {
                self.advance();
                Ok(Expr::Symbol(s))
            },
            _ => {
                self.advance();
                Err(ParsingError::UnexpectedToken(token))
            },
        }
    }

    fn get_token(&self) -> Result<lexer::Token, ParsingError> {
        if self.position >= self.tokens.len() {
            return Err(ParsingError::UnexpectedEOF);
        }
        Ok(self.tokens[self.position].clone())
    }

    fn advance(&mut self) {
        self.position += 1;
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_next_expr_eq {
        ($parser:expr, $value:expr) => {
            let expr = $parser.parse_next_expr()?;
            assert_eq!(expr, $value);
        };
    }

    fn parse(input: &str) -> Parser {
        Parser::new(lexer::Lexer::from_string(input).iter().collect())
    }

    #[test]
    fn parse_number() -> ParsingResult<()> {
        let mut p = parse("123");
        assert_next_expr_eq!(p, Expr::Int(123));

        Ok(())
    }

    #[test]
    fn parse_string() -> ParsingResult<()> {
        let mut p = parse("\"Hello, world!\"");
        assert_next_expr_eq!(p, Expr::String("Hello, world!".to_string()));

        Ok(())
    }

    #[test]
    fn parse_symbol() -> ParsingResult<()> {
        let mut p = parse("foo");
        assert_next_expr_eq!(p, Expr::Symbol("foo".to_string()));

        Ok(())
    }
}
