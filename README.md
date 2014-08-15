OCaml-Z3
========

An overlay for the OCaml Z3 binding. Highly WIP.

[Z3](https://z3.codeplex.com/) is an SMT solver developed by microsoft. A new ocaml binding is available in the branch `ml-ng`.

This is an overlay to the new OCaml binding.

## Official bindings

To install the official bindings, execute
```
python2 scripts/mk_make.py --ml
cd build
make -j4
```
Go [take a coffee](https://xkcd.com/303/) and then:
```
make ocamlfind_install
```

## Optiz3

[This fork](https://z3.codeplex.com/SourceControl/network/forks/gradanne/mlopti?branch=optiz3-ml) allows to use the new OCaml binding and an optimizing solver (originally developed by Arie Gurfinkel [here](https://z3.codeplex.com/SourceControl/network/forks/arie/optiz3?branch=optiz3)).

Some additional functions are provided if this fork is used with `./configure --enable-optiz3`

The packages `z3` and `z3overlay` are provided in the [drupam](https://github.com/Drup/drupam) repository.
Beware that you still need to compile and install the fork of z3 on your system. A good way is to fetch the fork, compile and install it, and pin the repository in opam, then install the opam package.

## Miscs

Tests are available under `test/` and built with `configure --enable-tests` and `make test`.
