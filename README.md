# nisp

A lisp-like interpreted language written in Rust.

## Usage

```
$ cargo run -- input.nisp
```

## Example

```lisp
(let add (fn [a b] (+ a b)))
(let x (add 1 2))
(print x)
```
