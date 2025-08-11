use std::rc::Rc;

use crate::{expression::Expr, lexer::{self, Token}};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParsingError {
    #[error("Unexpected End Of File while parsing")]
    UnexpectedEOF,
    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(Token)
}

type ParsingResult<T> = Result<T, ParsingError>;

pub struct Parser {
    tokens: Rc<[lexer::Token]>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<lexer::Token>) -> Self {
        Self { tokens: tokens.into_boxed_slice().into(), position: 0 }
    }

    pub fn from(other: &Parser) -> Self {
        Self { tokens: other.tokens.clone(), position: other.position }
    }

    pub fn parse_all(&mut self) -> ParsingResult<Vec<Expr>> {
        let mut exprs = vec![];
        while let Ok(expr) = self.parse_next_expr() {
            exprs.push(expr);
        }
        Ok(exprs)
    }

    pub fn parse_next_expr(&mut self) -> ParsingResult<Expr> {
        let token = self.get_token()?;
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
            Token::OpenParen => {
                self.advance();
                let next_token = self.get_token()?;
                if next_token == Token::CloseParen {
                    self.advance();
                    Ok(Expr::Unit)
                } else if let Token::Symbol(s) = next_token {
                    self.advance();
                    let next_token = self.get_token()?;
                    if next_token == Token::CloseParen {
                        self.advance();
                        Ok(Expr::Call(s, vec![]))
                    } else {
                        let mut exprs = Vec::new();
                        let mut parser = Parser::from(self);

                        while let Ok(expr) = parser.parse_next_expr() {
                            exprs.push(expr);
                        }
                        self.position = parser.position;
                        Ok(Expr::Call(s, exprs))
                    }
                } else {
                    Err(ParsingError::UnexpectedToken(next_token))
                }
            },
            Token::OpenBracket => {
                self.advance();
                let next_token = self.get_token()?;
                if next_token == Token::CloseBracket {
                    self.advance();
                    Ok(Expr::List(vec![]))
                } else {
                    let mut exprs = Vec::new();
                    let mut parser = Parser::from(self);

                    while let Ok(expr) = parser.parse_next_expr() {
                        exprs.push(expr);
                    }
                    self.position = parser.position;
                    Ok(Expr::List(exprs))
                }
            }
            Token::OpenBrace => {
                self.advance();
                let next_token = self.get_token()?;
                if next_token == Token::CloseBrace {
                    self.advance();
                    Ok(Expr::Block(vec![]))
                } else {
                    let mut exprs = Vec::new();
                    let mut parser = Parser::from(self);

                    while let Ok(expr) = parser.parse_next_expr() {
                        exprs.push(expr);
                    }
                    self.position = parser.position;
                    Ok(Expr::Block(exprs))
                }
            }
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

    fn advance_by(&mut self, n: usize) {
        self.position += n;
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

    #[test]
    fn parse_empty() -> ParsingResult<()> {
        let mut p = parse("");
        assert!(matches!(p.parse_next_expr(), Err(ParsingError::UnexpectedEOF)));

        Ok(())
    }

    #[test]
    fn parse_unit() -> ParsingResult<()> {
        let mut p = parse("()");
        assert_next_expr_eq!(p, Expr::Unit);

        Ok(())
    }

    #[test]
    fn parse_empty_list() -> ParsingResult<()> {
        let mut p = parse("[]");
        assert_next_expr_eq!(p, Expr::List(vec![]));

        Ok(())
    }

    #[test]
    fn parse_list() -> ParsingResult<()> {
        let mut p = parse("[1 2 3]");
        assert_next_expr_eq!(p, Expr::List(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)]));

        Ok(())
    }

    #[test]
    fn parse_empty_call() -> ParsingResult<()> {
        let mut p = parse("(foo)");
        assert_next_expr_eq!(p, Expr::Call("foo".to_string(), vec![]));

        Ok(())
    }

    #[test]
    fn parse_call() -> ParsingResult<()> {
        let mut p = parse("(foo 123)");
        assert_next_expr_eq!(p, Expr::Call("foo".to_string(), vec![Expr::Int(123)]));

        Ok(())
    }

    #[test]
    fn parse_empty_block() -> ParsingResult<()> {
        let mut p = parse("{}");
        assert_next_expr_eq!(p, Expr::Block(vec![]));
        
        Ok(())
    }

    #[test]
    fn parse_block() -> ParsingResult<()> {
        let mut p = parse("{1 2 3}");
        assert_next_expr_eq!(p, Expr::Block(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3)]));

        Ok(())
    }

    #[test]
    fn parse_complex_expr() -> ParsingResult<()> {
        let mut p = parse("(foo [bar (baz 123 \"hello\") qux] quux)");
        assert_next_expr_eq!(p, Expr::Call("foo".to_string(), vec![
            Expr::List(vec![
                Expr::Symbol("bar".to_string()),
                Expr::Call("baz".to_string(), vec![Expr::Int(123), Expr::String("hello".to_string())]),
                Expr::Symbol("qux".to_string()),
            ]),
            Expr::Symbol("quux".to_string()),
        ]));
        Ok(())
    }

    #[test]
    fn parse_file() -> ParsingResult<()> {
        let mut p = Parser::new(super::lexer::Lexer::from_file("input.nisp").unwrap().iter().collect());
        assert_next_expr_eq!(p, Expr::Call("print".to_string(), vec![Expr::String("hello, world".to_string())]));
        assert_next_expr_eq!(p, Expr::Call("+".to_string(), vec![Expr::Int(123), Expr::Int(456)]));

        Ok(())
    }
}
