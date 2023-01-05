# MyScheme

A Scheme interpreter written in Rust, with the aim of becoming mostly compliant with [R5RS](https://www.schemers.org/Documents/Standards/R5RS/r5rs.pdf).

- [x] Near-complete expression parsing
- [x] Evaluation of fundamental expressions (`define`, `lambda`, `if`, `set!`, `quote`)
- [ ] Evaluation of derived expressions
- [x] Proper tail recursion ([#2][pr2])
- [ ] First-class continuations ([#2][pr2])
- [ ] Macros

[pr2]: https://github.com/ianzhao05/myscheme/pull/2

## Examples

See [the integration tests](tests/integration.rs).
