# MyScheme

A Scheme interpreter written in Rust, with the aim of becoming mostly compliant with [R5RS](https://www.schemers.org/Documents/Standards/R5RS/r5rs.pdf).

## Running

```bash
git clone https://github.com/ianzhao05/myscheme.git && cd myscheme
cargo run # start a REPL
cargo run -- file.scm # run a file
cargo run -- -i file.scm # run a file followed by a REPL
```

A WebAssembly version of the interpreter is available at [ianzhao05.github.io/myscheme](https://ianzhao05.github.io/myscheme/).

## Supported Features

- Complete lexing and parsing of S-expressions
- Evaluation of all expressions
  - Primitive forms: `define`, `lambda`, `if`, `quote`, `set!`
  - Derived forms: `cond`, `case`, `and`, `or`, `let`, `let*`, `letrec`, `begin`, `do`, named `let`, `delay`, and quasiquotation (`quasiquote`, `unquote`, `unquote-splicing`)
- Proper tail recursion, as required by R5RS
- First-class continuations (with `call-with-current-continuation` and `dynamic-wind`)
- Implementation of all required primitive functions
- Hygienic, referentially transparent macros

### Not Yet Implemented

- `let-syntax`, `letrec-syntax`
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

There are four main phases of evaluation: lexing, reading, parsing, and evaluation.

### Lexing

First, the input string is lexed into a stream of tokens using a series of regular expressions that match the various token types. For instance, the string `(+ x 2)` would be lexed into `[LParen, Identifier(+), Identifier(x), Number(2), RParen]`.

Lexing logic is found in [lexer.rs](src/lexer.rs).

### Reading

In Scheme, like in most dialects of Lisp, the grammar of valid expressions is a subset of the grammar of S-expressions, or "data." After lexing, the stream of tokens is converted into data, defined in ([datum.rs](src/datum.rs)). Data consists of a tree-like structure of atoms (numbers, booleans, characters, strings, and symbols), proper and improper lists, and vectors. Reading logic is found in [reader.rs](src/reader.rs).

For instance, the stream of tokens above would be read as `List[Symbol(+), Symbol(x), Number(2)]`. Additionally, the various "quote" tokens are read as their equivalent lists, e.g., `'x` is read as `List[Symbol(quote), Symbol(x)]`.

### Parsing

The data is then parsed into an abstract syntax tree (AST) in [parser.rs](src/parser.rs). The AST, which is the internal representation of Scheme code, is defined in [expr.rs](src/expr.rs). Which the exception of a few optimizations, all valid Scheme expressions are transformed into their equivalent representation with only the primitive forms `define`, `lambda`, `if`, `quote`, and `set!`, which are mapped to the variants defined in [expr.rs](src/expr.rs). Note that the `quote` special form stops the parsing of its argument, instead preserving it as data.

For instance:

- The AST for the expression `(+ x 2)` would be `ProcCall { operator: Variable(+), operands: [Variable(x), Number(2)] }`.
- The `let` special form is parsed as an equivalent `lambda` expression, e.g., `(let ((x 1)) (+ x 2))` is equivalent to `((lambda (x) (+ x 2)) 1)` and will be transformed as such.
- The `cond` special form is parsed into an expression using only simple conditionals (`if`s), e.g., `(cond ((> x 0) 1) ((< x 0) -1) (else 0))` is equivalent to `(if (> x 0) 1 (if (< x 0) -1 0))` and will be transformed as such.
- Expressions involving `quasiquote` are transformed into `list`, `cons`, `append`, `vector` and `list->vector` calls, e.g., `` `(1 ,(+ 1 2) ,@(list 4 5))`` is parsed as `(append (list 1 (+ 1 2)) (list 4 5))`.
- Some forms, like `letrec` are more complex. `(letrec ((x (lambda () x))) (x))` is parsed similarly to

  ```scheme
  (let ((x <undefined>))
    (let ((<temp_name> (lambda () x)))
      (set! x <temp_name>))
    (x))
  ```

  The intermediate `let` is necessary to prevent the bindings from referencing each other before initialization.
- The other derived special forms are similarly transformed while they are parsed. The transformations were mostly taken from section 7.3 of the R5RS specification.

### Evaluation

Evaluation is mainly performed in [evaler.rs](src/evaler.rs). The entry point of evaluation is the `eval` function, which consumes an expression and an environment and returns the result of evaluating the expression in the given environment, which is either successful, yielding a value as defined in [object.rs](src/object.rs), or unsuccessful, yielding an error. Objects include primitive values (like numbers and symbols), pairs (lists are just nested pairs), vectors, procedures, ports, and more.

#### Environments and Closures

An environment represents the bindings available at any point in a program. An environment consists of a mapping from symbol to object reference and a reference to a parent environment (unless it is the top-level environment). Scheme's lexical scope rules lend themselves to a nested environment structure: when a symbol is looked up in an environment, the environment is searched first, then its parent, and so on until the symbol is found (or not, which is an error). Environments are implemented in [env.rs](src/env.rs).

When a `lambda` expression or procedure definition is encountered, the current environment is captured and stored in the procedure object. When the procedure is called, a new environment is created with the captured environment as its parent and initialized with the procedure's parameters bound to the arguments passed to the procedure call. Then, the procedure's body is evaluated in the new environment. Procedures are found in [proc.rs](src/proc.rs).

#### General Strategy

The evaluation strategy is inspired by R. Kent Dybvig's excellent dissertation [Three Implementation Models for Scheme](https://www.cs.indiana.edu/~dyb/pubs/3imp.pdf). Since Dybvig uses a compilation rather than an interpretation strategy, the implementation is different in a few respects, but the general idea is similar.

The naive approach to evaluation is a simple recursive procedure. For example, `(if test consequent alternate)` would be evaluated by evaluating `test`, then evaluating `consequent` if `test` evaluates to a true value, or `alternate` otherwise, with each evaluation performed by a recursive call. Procedure applications would be similar: simply evaluate the operator and each operand in sequence, then apply the operator to the operands. While simple, this approach has two major drawbacks: the stack is easily overflowed by recursive calls, and continuations are difficult if not impossible to implement.

The solution is to use a manual, heap-allocated ["spaghetti" stack](https://en.wikipedia.org/wiki/Parent_pointer_tree) and a continuation-based approach to evaluation that is tail-recursive. Continuations, not to be confused with the continuation objects produced by `call-with-current-continuation`, are structures that represent the rest of the computation to be performed. For instance, the continuation of `(if test consequent alternate)` stores `consequent` and `alternate`, and when the value of `test` is "applied" to this continuation, either `consequent` or `alternate` is set to be the next expression to be evaluated. Continuations store any such necessary data as well as a reference to the parent continuation (unless it is of the `Return` or `Apply` variant). Continuations are defined in [cont.rs](src/cont.rs).

Evaluation is modeled by a loop where each iteration represents one "step" of the evaluation (either evaluating a single expression or applying a value to continuation). Each iteration consumes and returns a `State` (also defined in [cont.rs](src/cont.rs)), comparable to the registers of Dybvig's virtual machine. The `State` contains an accumulator that is either an expression or a value to apply to a continuation, the current continuation, the current environment, the current "rib" (vector of values to be applied to procedures), a reference to the active stack frame, and a reference to the active winds (used by `dynamic-wind`). The "loop" is actually a tail-recursive function that is trampolined in [trampoline.rs](src/trampoline.rs) to emulate a loop. This also has the advantage of being able to split up the logic of evaluation, as any function that consumes and returns a `State` can be integrated into the loop. For example, `eval` defers the logic of procedure application to [proc.rs](src/proc.rs), where procedures implement the `Call` trait by defining a method that consumes and returns a `State`.

For example, `(if #t a 2)` would be evaluated as follows (say that `a` is bound to `1` in the environment):

1. The initial value of the accumulator is the expression `(if #t a 2)`, and the continuation is simply `Return` (assuming this is at the top-level).
2. The conditional is converted into the continuation `Conditional { consequent: a, alternate: 2, cont: Return }`, and the expression `#t` is left in the accumulator.
3. The expression `#t` is encountered, which is a simply an atom, so it is replaced by the value `#t` in the accumulator.
4. The value `#t` is applied to the continuation above, which sets the accumulator to the expression `a` and the continuation to `Return`.
5. The expression `a` is encountered, which leads to a variable lookup. The accumulator is set to `a`'s value, which is `1`.
6. The value `1` is applied to the continuation `Return`, which simply halts the loop with the value `1` since we are at the top-level. If we were not at the top-level, a stack frame would be popped, the previous state would be restored, and the value `1` would be left in the accumulator.

#### Procedure Application and Continuations

Procedure application involves the creation of a new stack frame, which saves the continuation, environment, and rib of the previous state (they are also defined in [cont.rs](src/cont.rs)). The state is then replaced with a continuation that, when invoked, will evaluate the operator and operands one-by-one. The last continuation in the chain is `Apply`, which extends the procedure's saved environment as previously described and replaces the current state with one that will evaluate the procedure's body in the new environment. The last continuation in this chain is `Return`, which will pop off the stack frame, restore the previous state, and leave the return value in the accumulator. For calls in a tail position, which occurs simply when the current continuation is `Return`, the current state does not need to be saved, so no stack frame is created. This is sufficient to support unbounded tail recursion.

Continuations (the results of `call-with-current-continuation`) are implemented by objects that simply capture the current stack and winds. When they are called, they build a continuation that will execute thunks registered through `dynamic-wind` according to the change in dynamic extent as a result of the call. Then, they restore the stored stack and arrange to return the given value, which jumps execution of the program back to where the continuation was saved. The stack needs to be stored as a tree rather than as a linked list: continuations are first-class objects that can be stored in environments, which necessitates keeping their particular branch of the stack alive for as long as they are. Winds are stored in a similar manner.

### Other Details

#### Primitive Functions

Primitive functions like arithmetic operations, `cons`, `eq?`, `read`, and many others are implemented in the [primitives](src/primitives/) module. They can be divided in three categories:

- Basic primitives: These make up the vast majority of primitives and are Rust functions that consume a slice of objects and return a value or an error.
- Control primitives: Some primitives, like `call-with-current-continuation` and `apply`, require full control over the current evaluation state. Thus, they are Rust functions that consume and return a `State`.
- Library primitives: These are primitives that are implemented in Scheme itself, like `list` and `map`. They are defined in string literals and are evaluated prior to the main program.

#### Symbol Interning

Symbols are interned through [interner.rs](src/interner.rs) through a simple hash-table implementation, meaning that every symbol is represented only by an integer index. This allows for fast equality checks and environment lookups, as well as fewer allocations and a smaller memory footprint. Symbols are interned beginning in the lexing phase.
