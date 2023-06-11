# MyScheme

A Scheme interpreter written in Rust, with the aim of becoming mostly compliant with [R5RS](https://www.schemers.org/Documents/Standards/R5RS/r5rs.pdf).

## Supported Features

- Complete lexing and parsing of S-expressions
- Evaluation of all expressions
  - Primitive forms: `define`, `lambda`, `if`, `quote`, `set!`
  - Derived forms: `cond`, `case`, `and`, `or`, `let`, `let*`, `letrec`, `begin`, `do`, named `let`, `delay`, and quasiquotation (`quasiquote`, `unquote`, `unquote-splicing`)
- Proper tail recursion, as required by R5RS
- First-class continuations
- [Implementation of almost all required primitive functions](src/primitives/)

### Not Yet Implemented

- Hygienic macros
- Complex numbers

## Examples

```scheme
> (define (fact n) (if (zero? n) 1 (* n (fact (- n 1)))))
> (fact 10)
3628800
> (define (sum lst)
|   (let loop ((lst lst) (acc 0))
|     (cond
|       ((null? lst) acc)
|       (else (loop (cdr lst) (+ acc (car lst)))))))
> (sum '(1 2 3 4 5))
15
> (define (make-partial f . args)
|   (lambda more-args (apply f (append args more-args))))
> (define add-3 (make-partial + 1 2))
> (add-3 10 20)
33
> (call-with-current-continuation (lambda (k) (list 1 2 (k 3) 4 5)))
3
> (let*
|     ((yin ((lambda (cc) (newline) cc)
|            (call-with-current-continuation (lambda (c) c))))
|      (yang ((lambda (cc) (display #\*) cc)
|             (call-with-current-continuation (lambda (c) c)))))
|   (yin yang))

*
**
***
****
*****
...
```

For more examples, see [the integration tests](tests/).

## Implementation Details

TODO
