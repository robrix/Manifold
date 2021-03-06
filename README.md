# `Manifold`

An experiment in [quantitative type theory][] inspired by [@bentnib][] & [@pigworker][].

[@bentnib]: https://twitter.com/bentnib
[@pigworker]: https://twitter.com/pigworker
[quantitative type theory]: https://bentnib.org/quantitative-type-theory.html


## Goals

- Understand QTT better than I do at time of writing
- Practice skills involved in working with languages: DSLs, effects, parsing, typechecking, evaluation, etc.


## Non-goals

- A consistent logic (at least, not until I learn more about termination proofs)
- Reasonableness, stability, etc.; please don’t write software in this language (but if you must, feel free to file issues)


## Getting started

After cloning (with submodules), `cabal new-repl` in the project will launch `ghci`. Then:

- `import Manifold.REPL` to bring the REPL symbols into scope
- `runIO repl` to run the Manifold REPL inside `ghci`

There’s not a lot you can do with it just yet, but `:help` will tell you about the meta-commands you can run, and you can write little programs in the language’s (quite ad hoc) syntax:

```
λ: True
True
λ: (\ (x : Bool) . x)
\ x : ◊ . x
λ: :t (\ (x : Bool) . x)
(x @() : Bool) -> Bool
λ: (\ (x : Bool) . x) True
True
```

(As this project is purely experimental, that syntax may change without warning.)

Note that the syntax is currently extremely explicit; not only do you have to include all type abstractions and applications explicitly, you also have to annotate all variable introductions: `let`s, pi types, and lambdas all take type annotations:

```
λ: let (id : (x : Type) -> x -> x) = (\ (x : Type) (y : x) . y) in id Bool True
True
```
