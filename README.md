# ppx_viewpattern

[![ci workflow status](https://github.com/sim642/ppx_viewpattern/actions/workflows/ci.yml/badge.svg)](https://github.com/sim642/ppx_viewpattern/actions/workflows/ci.yml)
[![GitHub release status](https://img.shields.io/github/v/release/sim642/ppx_viewpattern)](https://github.com/sim642/ppx_viewpattern/releases)
[![opam package status](https://badgen.net/opam/v/ppx_viewpattern)](https://opam.ocaml.org/packages/ppx_viewpattern)

Transformation for view patterns in OCaml.
_Attempts to_ imitate [Haskell view patterns](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/view_patterns.html).


## Installation
```console
opam install ppx_viewpattern
```

## Usage
In dune:
```
(preprocess (pps ppx_viewpattern))
```

## Syntax
Use `[%view? pat when exp]` as a pattern to apply `exp` to whatever the pattern is matching and match the result of the `exp` application against `pat`.
Analogous to the Haskell view pattern `exp -> pat`.

See [`example/`](example/) for example usage.

Supported in:
* `match ... with ...` cases,
* `function ...` cases,
* `fun ... -> ...` arguments,
* `let ... in ...` expression binding left-hand sides,
* `try ... with ...` cases,
* nested inside the `pat` and `exp` parts of a view pattern itself.

### Caveats
A view pattern disables `redundant-case` and `partial-match` warnings for the corresponding `match`/`function`.

Currently unsupported in:
- `let ...` definition left-hand sides (at top level or in modules).
