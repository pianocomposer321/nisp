# Spread operator (Implemented)

```lisp
(def print [&args] {
  ...
})

(print "Hello" "world")
```

# Markers

```lisp
(def print [*args end] {
  ...
})

(print "hi" :end "")

(set map [:first-name "John" :last-name "Doe"])
(print map.first-name) ; prints "John"
(print map.last-name) ; prints "Doe"
```

# Pattern matching (Partially implemented)

```lisp
(set [hello world] ["hello" "world"])
(print hello) ; prints "hello"
(print world) ; prints "world"

(set [hello world] [:world "world" :hello "hello"])
(print hello) ; prints "hello"
(print world) ; prints "world"

(set only-last [:last-name "Doe"])
(match only-last [
  [first-name last-name] {
    (print first-name)
    (print last-name)
  }
  [first-name] {
    (print first-name)
  }
  [last-name] { ; this one will execute, because only last-name was defined
    (print last-name)
  }
])
```

# Multiple function definitions

```lisp
(def print [*args] {
  ...
})

(def print [*args end] {
  ...
})

(print "hello") ; uses first definition
(print "hello" :end "") ; uses second definition
```

# Annotations

## Loose annotations

- Can be user-defined
- Useful for things like encoding success vs failure

```lisp
(def can-error [error] {
  (if error @err "An error" @ok "Not an error")
})

(match (can-error false) [
  @ok success-value {
    (print "Function did not error")
    (print success-value)
  }
  @err error-value {
    (print "Function did error")
    (print error-value)
  }
])
```

## Strong annotations

- Can only be defined by the interpreter/compiler
- Useful for things like type-definitions

```lisp
(def typesafe-function [#int value] {
  (print (+ 1 value))
})

(typesafe-function 1)       ; succeeds because 1 matches the strong annotation of int
(typesafe-function "Hello") ; fails because no definition of typesafe-function
                            ; with a matching signature was found - the pattern
                            ; matching failed

(set val 123)
(match val {
  #string val {
    (print "String: " val)
  }
  #int val {
    (print "Integer:" val) ; this branch will run
  }
})
```
