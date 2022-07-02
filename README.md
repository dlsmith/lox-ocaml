# lox-ocaml

An implementation of (something close to) the jlox tree-walk interpreter from
[Crafting Interpreters](https://www.craftinginterpreters.com/). I'm also using
this as an opportunity to learn more about statically-typed functional
programming patterns and the OCaml language. Thus, the implementation diverges
from the text in a number of ways (e.g., pattern-matching over visitors,
`Result`s over exceptions, etc.) and the use of OCaml is unlikely to have great
style or be highly idiomatic.

### TODO

- [ ] Surface all scanner/parser errors rather than only the first
- [ ] We skipped the full resolver implementation in favor of fixing the
  closure issue by copying the environment at the time function declarations
  are evaluated. The one feature we lose (as of the beginning of the classes
  chapter) is prohibiting variable redefinition in local scopes.
- [ ] Add line-level error reporting during parsing (we only now support it
  during evaluation).
- [ ] `Env` without refs/mutability?
