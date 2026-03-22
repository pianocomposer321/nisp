#![allow(unused)]

use crate::{parser, lexer, expression};

#[derive(Debug, PartialEq)]
pub enum Op {
    Push(Val),
    Add(usize),
    Eq,
}

#[derive(Debug, PartialEq)]
pub enum Val {
    Int(i64),
    Bool(bool),
}
