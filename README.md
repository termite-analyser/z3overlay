z3overlay
========

An overlay for the OCaml Z3 binding.

[Z3](https://github.com/Z3Prover/z3) is an SMT solver developed by microsoft.

This package contains an overlay to official binding.

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

(** Finally we easily get back the values in the model as inferred by Z3
    without any casting! *)
let val_x = Model.get_value ~model x
let val_y = Model.get_value ~model y
```

## Install

```
opam pin add z3overlay https://github.com/termite-analyser/z3overlay.git
```
