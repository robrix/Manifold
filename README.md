# `Manifold`

An experiment in [quantitative type theory][] inspired by [@bentnib][] & [@pigworker][], using a [proof refinement][]–style approach due to [@beka_valentine][].

[@beka_valentine]: https://twitter.com/beka_valentine
[@bentnib]: https://twitter.com/bentnib
[@pigworker]: https://twitter.com/pigworker
[proof refinement]: http://languagengine.co/blog/bidirectional-proof-refinement/
[quantitative type theory]: https://bentnib.org/quantitative-type-theory.html


## Goals

- Understand QTT better than I do at time of writing
- Practice skills involved in working with languages: DSLs, effects, parsing, typechecking, evaluation, etc.


## Getting started

After cloning (with submodules), `cabal new-repl` in the project will launch `ghci`. Then:

- `import Manifold.REPL` to bring the REPL symbols into scope
- `runIO repl` to run the Manifold REPL inside `ghci`

There’s not a lot you can do with it just yet, but `:help` will tell you about the meta-commands you can run, and you can write little programs in the language’s (quite ad hoc) syntax:

```
λ: True
Value (Bool True)
λ: (\ (x : Bool) . x)
Value (Abs (N "x",Context {unContext = []}) (Term (Var (N "x"))))
λ: (\ (x : Bool) . x) True
Value (Bool True)
```

(As this project is purely experimental, that syntax may change without warning.)
