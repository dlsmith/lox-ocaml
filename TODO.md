# TODO

- [x] Clean up tests by factoring out boilerplate
- [ ] Surface all scanner/parser errors rather than only the first
- [ ] Add line-level error reporting during parsing (we only now support it
  during evaluation).
- [ ] Be more consistent and useful with `Alcotest` assertion strings. Things
  like "same value" are not useful.
- [ ] `Env` without refs/mutability?
- [ ] We skipped the full resolver implementation in favor of fixing the
  closure issue by copying the environment at the time function declarations
  are evaluated. The one feature we lose (as of the beginning of the classes
  chapter) is prohibiting variable redefinition in local scopes.
- [ ] Classes?
