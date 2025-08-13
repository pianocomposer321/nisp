# nisp

A lisp-like interpreted language written in Rust.

## Usage

```
$ cargo run -- input.nisp
```

## Example

```lisp
(defn fib [n] {
  (cond [
    (= n 0) 1
    (= n 1) 1
    true (+ (fib (- n 1)) (fib (- n 2)))
  ])
})

(fib 10) ; 45
(fib 20) ; 955
```

For more examples, see the [tests](https://github.com/composer/nisp/tree/master/tests).
