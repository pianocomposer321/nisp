#![allow(unused)]

use crate::{expression, lexer, parser, vm};

use thiserror::Error;

#[derive(Debug, PartialEq)]
enum Op {
    Push(vm::Val),
    Add(usize),
    Eq,
}

#[derive(Error, Debug)]
enum CompileError {
    #[error("Unrecognized builtin: {0}")]
    UnrecognizedBuiltin(String),
    #[error("Unexpected EOF")]
    UnexpectedEOF,
}

type CompileResult<T> = Result<T, CompileError>;

struct Compiler {
    source: String,
    ast: Vec<expression::Expr>,
    cursor: usize,
}

impl Compiler {
    pub fn new(source: String, ast: Vec<expression::Expr>) -> Self {
        Self {
            source,
            ast,
            cursor: 0,
        }
    }

    fn try_compile_builtin(name: &str, args: &Vec<expression::Expr>) -> CompileResult<Vec<Op>> {
        match name {
            "+" => {
                let mut ops = Vec::new();
                for arg in args {
                    ops.extend(Self::compile_expr(arg)?)
                }
                ops.push(Op::Add(args.len()));
                Ok(ops)
            },
            "=" => {
                let mut ops = Self::compile_expr(&args[0])?;
                ops.extend(Self::compile_expr(&args[1])?);
                ops.push(Op::Eq);
                Ok(ops)
            },
            other => Err(CompileError::UnrecognizedBuiltin(other.to_string())),
        }
    }

    pub fn compile_expr(expr: &expression::Expr) -> CompileResult<Vec<Op>> {
        match expr {
            expression::Expr::Call(name, exprs) => Self::try_compile_builtin(name, exprs),
            expression::Expr::List(list) => todo!(),
            expression::Expr::Block(exprs) => todo!(),
            expression::Expr::Int(inner) => Ok(vec![Op::Push(vm::Val::Int(*inner))]),
            expression::Expr::Bool(inner) => Ok(vec![Op::Push(vm::Val::Bool(*inner))]),
            expression::Expr::String(_) => todo!(),
            expression::Expr::Symbol(_) => todo!(),
            expression::Expr::MarkerPair(_, expr) => todo!(),
            expression::Expr::DotOp(expr, expr1) => todo!(),
            expression::Expr::ListTail(_) => todo!(),
            expression::Expr::Unit => todo!(),
        }
    }

    pub fn compile_next_expr(&mut self) -> CompileResult<Vec<Op>> {
        let ops = Self::compile_expr(&self.ast[self.cursor].clone())?;
        self.cursor += 1;
        Ok(ops)
    }

    pub fn compile_all(mut self) -> CompileResult<Vec<Op>> {
        let mut ops = Vec::new();
        for expr in self.ast.iter().cloned() {
            ops.extend(Self::compile_expr(&expr)?);
        }
        Ok(ops)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! assert_next_op_eq {
        ($ops:expr, $value:expr) => {
            let expr = $ops.next().ok_or(CompileError::UnexpectedEOF)?;
            assert_eq!(expr, $value);
            // let expr = $parser.parse_next_expr()?;
            // assert_eq!(expr, $value);
        };
    }

    #[derive(Error, Debug)]
    enum CompileTestError {
        #[error(transparent)]
        ParsingError(#[from] parser::ParsingError),
        #[error(transparent)]
        CompileError(#[from] CompileError),
    }

    type CompileTestResult<T> = Result<T, CompileTestError>;

    fn make_compiler(input: &'static str) -> CompileTestResult<Compiler> {
        let mut lexer = lexer::Lexer::from_string(input);
        let mut parser = parser::Parser::new(lexer.iter().collect());
        let exprs = parser.parse_all()?;
        Ok(Compiler::new(input.to_string(), exprs))
    }

    #[test]
    fn int() -> CompileTestResult<()> {
        let mut compiler = make_compiler("123")?;
        let res = compiler.compile_next_expr()?;
        dbg!(&res);
        assert!(matches!(res[0], Op::Push(vm::Val::Int(123))));

        Ok(())
    }

    #[test]
    fn bool() -> CompileTestResult<()> {
        let mut compiler = make_compiler("true")?;
        let res = compiler.compile_next_expr()?;
        dbg!(&res);
        assert!(matches!(res[0], Op::Push(vm::Val::Bool(true))));

        Ok(())
    }

    #[test]
    fn add() -> CompileTestResult<()> {
        let mut compiler = make_compiler("(+ 1 2)")?;
        let mut res = compiler.compile_all()?.into_iter();
        assert_next_op_eq!(res, Op::Push(vm::Val::Int(1)));
        assert_next_op_eq!(res, Op::Push(vm::Val::Int(2)));
        assert_next_op_eq!(res, Op::Add(2));

        let mut compiler = make_compiler("(+ 1 2 3)")?;
        let mut res = compiler.compile_all()?.into_iter();
        assert_next_op_eq!(res, Op::Push(vm::Val::Int(1)));
        assert_next_op_eq!(res, Op::Push(vm::Val::Int(2)));
        assert_next_op_eq!(res, Op::Push(vm::Val::Int(3)));
        assert_next_op_eq!(res, Op::Add(3));

        Ok(())
    }

    #[test]
    fn eq() -> CompileTestResult<()> {
        let mut compiler = make_compiler("(= 1 2)")?;
        let mut res = compiler.compile_all()?.into_iter();
        dbg!(&res);
        assert_next_op_eq!(res, Op::Push(vm::Val::Int(1)));
        assert_next_op_eq!(res, Op::Push(vm::Val::Int(2)));
        assert_next_op_eq!(res, Op::Eq);

        Ok(())
    }
}
