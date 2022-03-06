# ppx_viewpattern

Transformation for view patterns in OCaml.
_Attempts to_ imitate [Haskell view patterns](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/view_patterns.html).


## Usage
In dune:
```
(preprocess (pps ppx_viewpattern))
```

## Syntax
Use `[%view? pat when exp]` as a pattern to apply `exp` to whatever the pattern is matching and match the result of the `exp` application against `pat`.
Analogous to the Haskell view pattern `exp -> pat`.

Supported in:
* `match ... with ...` cases,
* `function ...` cases,
* `fun ... -> ...` arguments,
* `let ... in ...` expression binding left-hand sides,
* nested inside the `pat` part of a view pattern itself.

Currently unsupported in:
- [ ] `let ...` definition left-hand sides (at top level or in modules)
