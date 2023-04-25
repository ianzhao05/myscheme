# MyScheme

A Scheme interpreter written in Rust, with the aim of becoming mostly compliant with [R5RS](https://www.schemers.org/Documents/Standards/R5RS/r5rs.pdf).

- [x] Near-complete expression parsing
- [x] Evaluation of fundamental expressions (`define`, `lambda`, `if`, `set!`, `quote`)
- [x] Evaluation of derived expressions (`let`, `cond`, `case`, `do`, etc.)
- [x] Proper tail recursion ([#3][pr3])
- [ ] First-class continuations ([#3][pr3])
- [ ] Macros

[pr3]: https://github.com/ianzhao05/myscheme/pull/3

## Examples

See [the integration tests](tests/integration.rs).
