OCaml-Z3
========

An overlay for the OCaml Z3 binding. Highly WIP.

[Z3](https://z3.codeplex.com/) is an SMT solver developed by microsoft. A new ocaml binding is available in the branch `ml-ng`.

This is an overlay to the new OCaml binding.

## Official bindings

The official bindings propose a target `make ocamlfind_install` but for various reason, it might be useful to proceed differently. It is possible to use this library to build the OCaml official binding by symlinking (or copying) `ocamlz3` to `src/api/ml` and using `configure --enable-binding`.

## Optiz3

A fork by Arie Gurfinkel ([here](https://z3.codeplex.com/SourceControl/network/forks/arie/optiz3?branch=optiz3) called optiz3 allows to do optimization with solving.

[This fork](https://z3.codeplex.com/SourceControl/network/forks/gradanne/mlopti?branch=optiz3-ml) allows to use the new OCaml binding and the optimization.

Some additional functions are provided if this fork is used with `./configure --enable-optiz3`


## Miscs

Tests are available under `test/` and built with `configure --enable-tests` and `make test`.
