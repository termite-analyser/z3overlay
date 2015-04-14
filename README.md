OCaml-Z3
========

An overlay for the OCaml Z3 binding.

[Z3](https://github.com/Z3Prover/z3) is an SMT solver developed by microsoft. A new ocaml binding is available in the branch [`unstable`](https://github.com/Z3Prover/z3/tree/unstable).

This package contains an overlay to this new official binding.

This overlays allows to track the type of terms in the typesystem, allowing the OCaml typechecker to check the validity of Z3 formulas and making the model extraction easier.

## Howto

You can see an example of use in [`test/simple.ml`](test/simple.ml). Here is a taste:
```ocaml
(** We hide the context by instanciating a functor: *)
module Z = ZZ3.Make(struct let ctx = Z3.mk_context [] end)

(** The result of the functor is safe for opening (contains only types and modules. *)
open Z

(** We create a solver for future usage. *)
let solver = Solver.make ()

(** We create new SMT variables and specify their types. *)
let x = Symbol.declare Real "x" in
let y = Symbol.declare Real "y" in

(** We can define SMT formulas using an OCaml-like syntax.
    [!] transforms a symbol into a term.
*)
let t = T.( !x + !y >= int 3 )

(** We assert the formula in the SMT solver. *)
let () = Solver.add ~solver sum t

(** We can now solve it and extract the result: *)
let model = match Solver.check ~solver [] with
  | Unsat _ | Unkown _ -> failwith "Oh noees"
  | Sat (lazy model) -> model

(** Finally we easily get back the values in the model as inferred by Z3 without any casting! *)
let val_x = Model.get_value ~model x
let val_y = Model.get_value ~model y
```

## Install

The dependencies are:

- Z3
- zarith

### The official bindings
To install the official bindings, execute
```
python2 scripts/mk_make.py --ml
cd build
make -j4
```
Go [take a coffee](https://xkcd.com/303/) and then:
```
sudo make install
make ocamlfind_install
```

Alternatively, one can only do `make ocamlfind_install` and directs `LD_LIBRARY_PATH` to the directory containing `libz3.so` (probably `` `ocamlfind printconf destdir`/stublibs``).

### z3overlay
Once the official bindings are installed, install this package simply by `make && make install`.

### Tests

Tests are available under `test/` and built with `configure --enable-tests` and `make test`.


## Optimizing SMT solver.

A branch containing an optimizing solver for z3 is available in the [`opt`](https://github.com/Z3Prover/z3/tree/opt) branch. The [`opt`](https://github.com/termite-analyser/z3overlay/tree/opt) contains an adaptation for Z3overlay.
