use std::path::Path;
use std::{fs, io};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexingError {
    #[error("Unexpected End Of File while lexing")]
    EOL,
    #[error("Unexpected End Of File while lexing")]
    EOF,
    #[error("Unexpected token: expected {expected:?} but got {got:?}")]
    UnexpectedToken {
        position: usize,
        expected: Token,
        got: Token,
    },
    #[error("Unable to parse integer")]
    ParseIntError(#[from] std::num::ParseIntError),
    #[error(transparent)]
    IOError(#[from] std::io::Error),
}

type LexingResult<T> = Result<T, LexingError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    /// (
    OpenParen,
    /// )
    CloseParen,
    /// [
    OpenBracket,
    /// ]
    CloseBracket,
    /// {
    OpenBrace,
    /// }
    CloseBrace,
    /// symbol
    Symbol(String),
    /// 123
    IntLiteral(i64),
    /// "string"
    StringLiteral(String),
    /// true, false
    BoolLiteral(bool),
}

pub struct TokenIter<'a>(&'a mut Lexer);

impl<'a> Iterator for TokenIter<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.get_next_token().ok()
    }
}

#[derive(Debug)]
pub struct Lexer {
    input: Vec<Vec<char>>,
    line: usize,
    col: usize,
}

#[allow(dead_code)]
impl Lexer {
    pub fn new(input: Vec<String>) -> Self {
        Self {
            input: input.iter().map(|s| s.chars().collect()).collect(),
            line: 0,
            col: 0,
        }
    }

    pub fn from_string(input: impl Into<String>) -> Self {
        let input: String = input.into();
        let input: Vec<Vec<char>> = input.lines().map(|s| s.chars().collect()).collect();
        Self {
            input,
            line: 0,
            col: 0,
        }
    }

    pub fn from_file(filename: impl AsRef<Path>) -> io::Result<Self> {
        let contents = fs::read_to_string(filename)?;
        let input: Vec<Vec<char>> = contents.lines().map(|s| s.chars().collect()).collect();

        Ok(Self {
            input,
            line: 0,
            col: 0,
        })
    }

    pub fn get_next_token(&mut self) -> LexingResult<Token> {
        while self.get_line()?.len() == 0 {
            self.advance_line();
        }

        while self.get_char()?.is_whitespace() {
            self.advance()?;
        }

        let ch = self.get_char()?;
        if ch == ';' {
            self.advance_line();
            return self.get_next_token();
        }

        if ch == '(' {
            self.advance()?;
            return Ok(Token::OpenParen);
        }
        if ch == ')' {
            self.advance()?;
            return Ok(Token::CloseParen);
        }
        if ch == '[' {
            self.advance()?;
            return Ok(Token::OpenBracket);
        }
        if ch == ']' {
            self.advance()?;
            return Ok(Token::CloseBracket);
        }
        if ch == '{' {
            self.advance()?;
            return Ok(Token::OpenBrace);
        }
        if ch == '}' {
            self.advance()?;
            return Ok(Token::CloseBrace);
        }
        if ch == '-' {
            self.advance()?;
            let next = self.get_slice_until_or_end(|c| !matches!(c, '0'..'9'));
            if matches!(next, Err(LexingError::EOF) | Err(LexingError::EOL)) {
                return Ok(Token::Symbol("-".to_string()));
            }
            let next = next?;

            if next.len() > 0 {
                let number: String = next.iter().collect();
                dbg!(&number);
                self.advance_by(number.len())?;
                let parsed = number.parse::<i64>()?;
                return Ok(Token::IntLiteral(parsed * -1));
            }
            return Ok(Token::Symbol("-".to_string()));
        }
        if matches!(ch, '0'..'9') {
            let number: String = self
                .get_slice_until_or_end(|c| !matches!(c, '0'..'9'))?
                .iter()
                .collect();
            self.advance_by(number.len())?;
            return Ok(Token::IntLiteral(number.parse()?));
        }
        if matches!(ch, '"') {
            self.advance()?;
            let string: String = self.get_slice_until(|c| c == '"')?.iter().collect();
            self.advance_by(string.len())?;
            self.advance()?;
            return Ok(Token::StringLiteral(string));
        }
        if self.get_slice_until_or_end(|c| c.is_whitespace() || matches!(c, ')' | ']' | '}'))? == ['t', 'r', 'u', 'e'] {
            self.advance_by(4)?;
            return Ok(Token::BoolLiteral(true));
        }
        if self.get_slice_until_or_end(|c| c.is_whitespace() || matches!(c, ')' | ']' | '}'))? == ['f', 'a', 'l', 's', 'e'] {
            self.advance_by(5)?;
            return Ok(Token::BoolLiteral(false));
        }

        // Assume anything else is a symbol
        let symbol: String = self
            .get_slice_until_or_end(|c| c.is_whitespace() || matches!(c, ')' | ']' | '}'))?
            .iter()
            .collect();
        self.advance_by(symbol.len())?;
        Ok(Token::Symbol(symbol))
    }

    pub fn iter(&mut self) -> TokenIter {
        TokenIter(self)
    }

    fn get_slice(&self, offset: usize) -> LexingResult<&[char]> {
        self.get_slice_at(0, offset)
    }

    fn get_slice_until(&self, pred: impl Fn(char) -> bool) -> LexingResult<&[char]> {
        self.get_slice(self.find(pred)? - 1)
    }

    fn get_slice_until_or_end(&self, pred: impl Fn(char) -> bool) -> LexingResult<&[char]> {
        // Safe to unwrap because find_or_end always returns an in-bounds index
        let offset = self.find_or_end(pred)?;
        if offset == 0 {
            return Ok(&[]);
        }
        self.get_slice(offset - 1)
    }

    /// Returns the offset of the first char for which pred(char) is true, starting at
    /// self.position + offset
    fn find_at(&self, mut offset: usize, pred: impl Fn(char) -> bool) -> Option<usize> {
        while !pred(self.get_char_at(offset).ok()?) {
            offset += 1;
        }
        Some(offset)
    }

    /// Returns the offset of the first char for which pred(char) is true
    fn find(&self, pred: impl Fn(char) -> bool) -> LexingResult<usize> {
        let mut offset = 0;
        while !pred(self.get_char_at(offset)?) {
            offset += 1;
        }
        Ok(offset)
    }

    /// Returns the offset of the first char for which pred(char) is true, or the offset that is
    /// one past the end of self.input.
    fn find_or_end(&self, pred: impl Fn(char) -> bool) -> LexingResult<usize> {
        Ok(self.find(pred).unwrap_or(self.offset_to_end()?))
    }

    fn find_or_end_at(&self, offset: usize, pred: impl Fn(char) -> bool) -> LexingResult<usize> {
        Ok(self.find_at(offset, pred).unwrap_or(self.offset_to_end()?))
    }

    fn get_slice_at(&self, smaller: usize, larger: usize) -> LexingResult<&[char]> {
        if self.col + larger >= self.get_line()?.len() {
            Err(LexingError::EOF)
        } else {
            Ok(&self.input[self.line][self.col + smaller..=self.col + larger])
        }
    }

    fn get_line(&self) -> LexingResult<&[char]> {
        self.input.get(self.line).map(|s| s.as_slice()).ok_or(LexingError::EOF)
    }

    fn get_char(&self) -> LexingResult<char> {
        self.get_char_at(0)
    }

    fn get_char_at(&self, offset: usize) -> LexingResult<char> {
        if self.line >= self.lines() {
            return Err(LexingError::EOF);
        }

        if self.col + offset >= self.get_line()?.len() {
            Err(LexingError::EOL)
        } else {
            Ok(self.input[self.line][self.col + offset])
        }
    }

    fn lines(&self) -> usize {
        self.input.len()
    }

    fn offset_to_end(&self) -> LexingResult<usize> {
        Ok(self.get_line()?.len() - self.col)
    }

    fn advance(&mut self) -> LexingResult<()> {
        self.col += 1;
        if self.col >= self.get_line()?.len() {
            self.col = 0;
            self.line += 1;
        }

        Ok(())
    }

    fn advance_line(&mut self) {
        self.line += 1;
        self.col = 0;
    }

    fn advance_by(&mut self, offset: usize) -> LexingResult<()> {
        self.col += offset;
        if self.col >= self.get_line()?.len() {
            let diff = self.col - self.get_line()?.len();
            self.line += 1;
            self.col = diff;
        }

        Ok(())
    }

    fn advance_until(&mut self, pred: impl Fn(char) -> bool) -> LexingResult<()> {
        let mut ch = self.get_char()?;
        while !pred(ch) {
            self.advance()?;
            ch = self.get_char()?;
        }

        Ok(())
    }

    fn backup(&mut self) -> LexingResult<()> {
        if self.col > 0 {
            self.col -= 1;
        } else if self.line > 0 {
            self.line -= 1;
            self.col = self.get_line()?.len() - 1;
        }
        Ok(())
    }

    fn backup_by(&mut self, n: usize) -> LexingResult<()> {
        if self.col > n {
            self.col -= n;
        } else if self.line > 0 {
            self.line -= 1;
            self.col = self.get_line()?.len() - n;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_next_token_eq {
        ($lexer:expr, $value:expr) => {
            let token = $lexer.get_next_token().unwrap();
            assert_eq!(token, $value);
            // assert_eq!($lexer.get_next_token(), Ok($value))
        };
    }

    macro_rules! assert_eof {
        ($lexer:expr) => {
            assert!(matches!($lexer.get_next_token(), Err(LexingError::EOF)));
        };
    }

    fn lex(s: &str) -> Lexer {
        Lexer::from_string(s)
    }

    #[test]
    fn lex_empty_parens() -> LexingResult<()> {
        let mut l = lex("()");
        assert_next_token_eq!(&mut l, Token::OpenParen);
        assert_next_token_eq!(&mut l, Token::CloseParen);
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_int() -> LexingResult<()> {
        let mut l = lex("123");
        assert_next_token_eq!(&mut l, Token::IntLiteral(123));
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_single_expression() -> LexingResult<()> {
        let mut l = lex("(expr)");
        assert_next_token_eq!(&mut l, Token::OpenParen);
        assert_next_token_eq!(&mut l, Token::Symbol("expr".to_string()));
        assert_next_token_eq!(&mut l, Token::CloseParen);
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_multiple_expression() -> LexingResult<()> {
        let mut l = lex("(+ 1 2)");
        assert_next_token_eq!(&mut l, Token::OpenParen);
        assert_next_token_eq!(&mut l, Token::Symbol("+".to_string()));
        assert_next_token_eq!(&mut l, Token::IntLiteral(1));
        assert_next_token_eq!(&mut l, Token::IntLiteral(2));
        assert_next_token_eq!(&mut l, Token::CloseParen);
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_multiple_expression_with_whitespace() -> LexingResult<()> {
        let mut l = lex("( + 1 2 )");
        assert_next_token_eq!(&mut l, Token::OpenParen);
        assert_next_token_eq!(&mut l, Token::Symbol("+".to_string()));
        assert_next_token_eq!(&mut l, Token::IntLiteral(1));
        assert_next_token_eq!(&mut l, Token::IntLiteral(2));
        assert_next_token_eq!(&mut l, Token::CloseParen);
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_empty_list() -> LexingResult<()> {
        let mut l = lex("[]");
        assert_next_token_eq!(&mut l, Token::OpenBracket);
        assert_next_token_eq!(&mut l, Token::CloseBracket);
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_single_list() -> LexingResult<()> {
        let mut l = lex("[1]");
        assert_next_token_eq!(&mut l, Token::OpenBracket);
        assert_next_token_eq!(&mut l, Token::IntLiteral(1));
        assert_next_token_eq!(&mut l, Token::CloseBracket);
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_multiple_list() -> LexingResult<()> {
        let mut l = lex("[1 2 3]");
        assert_next_token_eq!(&mut l, Token::OpenBracket);
        assert_next_token_eq!(&mut l, Token::IntLiteral(1));
        assert_next_token_eq!(&mut l, Token::IntLiteral(2));
        assert_next_token_eq!(&mut l, Token::IntLiteral(3));
        assert_next_token_eq!(&mut l, Token::CloseBracket);
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_multiple_list_with_whitespace() -> LexingResult<()> {
        let mut l = lex("[ 1 2 3 ]");
        assert_next_token_eq!(&mut l, Token::OpenBracket);
        assert_next_token_eq!(&mut l, Token::IntLiteral(1));
        assert_next_token_eq!(&mut l, Token::IntLiteral(2));
        assert_next_token_eq!(&mut l, Token::IntLiteral(3));
        assert_next_token_eq!(&mut l, Token::CloseBracket);
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_complex_expression() -> LexingResult<()> {
        let mut l = lex("(+ 1 (* 2 3))");
        assert_next_token_eq!(&mut l, Token::OpenParen);
        assert_next_token_eq!(&mut l, Token::Symbol("+".to_string()));
        assert_next_token_eq!(&mut l, Token::IntLiteral(1));
        assert_next_token_eq!(&mut l, Token::OpenParen);
        assert_next_token_eq!(&mut l, Token::Symbol("*".to_string()));
        assert_next_token_eq!(&mut l, Token::IntLiteral(2));
        assert_next_token_eq!(&mut l, Token::IntLiteral(3));
        assert_next_token_eq!(&mut l, Token::CloseParen);
        assert_next_token_eq!(&mut l, Token::CloseParen);
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_string() -> LexingResult<()> {
        let mut l = lex("\"hello, world\"");
        assert_next_token_eq!(&mut l, Token::StringLiteral("hello, world".to_string()));
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_file() -> LexingResult<()> {
        let mut l = Lexer::from_file("input.nisp")?;
        assert_next_token_eq!(&mut l, Token::OpenParen);
        assert_next_token_eq!(&mut l, Token::Symbol("print".to_string()));
        assert_next_token_eq!(&mut l, Token::StringLiteral("hello, world".to_string()));
        assert_next_token_eq!(&mut l, Token::CloseParen);
        assert_next_token_eq!(&mut l, Token::OpenParen);
        assert_next_token_eq!(&mut l, Token::Symbol("+".to_string()));
        assert_next_token_eq!(&mut l, Token::IntLiteral(123));
        assert_next_token_eq!(&mut l, Token::IntLiteral(456));
        assert_next_token_eq!(&mut l, Token::CloseParen);
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_comment() -> LexingResult<()> {
        let mut l = lex("(; comment\nprint \"hello, world\")");
        assert_next_token_eq!(&mut l, Token::OpenParen);
        assert_next_token_eq!(&mut l, Token::Symbol("print".to_string()));
        assert_next_token_eq!(&mut l, Token::StringLiteral("hello, world".to_string()));
        assert_next_token_eq!(&mut l, Token::CloseParen);
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_empty_block() -> LexingResult<()> {
        let mut l = lex("{}");
        assert_next_token_eq!(&mut l, Token::OpenBrace);
        assert_next_token_eq!(&mut l, Token::CloseBrace);
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_block() -> LexingResult<()> {
        let mut l = lex("{1 2 3}");
        assert_next_token_eq!(&mut l, Token::OpenBrace);
        assert_next_token_eq!(&mut l, Token::IntLiteral(1));
        assert_next_token_eq!(&mut l, Token::IntLiteral(2));
        assert_next_token_eq!(&mut l, Token::IntLiteral(3));
        assert_next_token_eq!(&mut l, Token::CloseBrace);
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_bool() -> LexingResult<()> {
        let mut l = lex("true");
        assert_next_token_eq!(&mut l, Token::BoolLiteral(true));
        assert_eof!(&mut l);

        let mut l = lex("false");
        assert_next_token_eq!(&mut l, Token::BoolLiteral(false));
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_negative_number() -> LexingResult<()> {
        let mut l = lex("-1");
        assert_next_token_eq!(&mut l, Token::IntLiteral(-1));
        assert_eof!(&mut l);

        Ok(())
    }

    #[test]
    fn lex_sub() -> LexingResult<()> {
        let mut l = lex("-");
        assert_next_token_eq!(&mut l, Token::Symbol("-".to_string()));
        assert_eof!(&mut l);

        Ok(())
    }
}
