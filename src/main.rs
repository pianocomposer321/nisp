pub mod callable;
pub mod expression;
pub mod lexer;
pub mod parser;
pub mod scope;

use std::io;

use thiserror::Error;

use lexer::LexingError;

#[derive(Error, Debug)]
enum MainError {
    #[error(transparent)]
    LexingError(#[from] LexingError),
    #[error(transparent)]
    IoError(#[from] io::Error),
}

fn main() -> Result<(), MainError> {
    let mut l = lexer::Lexer::from_file("input.nisp")?;
    println!("{:?}", l.get_next_token());

    Ok(())
}
