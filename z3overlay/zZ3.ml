open ZZ3Utils
open Z3
open ZZ3_sigs

module Make (C : Context) = struct

  let ctx = C.ctx

  type zint  = [ `Int ]
  type zbool = [ `Bool ]
  type zreal = [ `Real ]

  type znum = [ zint | zreal ]
  type zany = [ zint | zbool | zreal ]

  type (_, _) typ =
    | Int : (Z.t, [> zint]) typ
    | Bool : (bool, [> zbool]) typ
    | Real : (Q.t, [> zreal]) typ
    | Num : (Q.t, [> znum] ) typ

  type +'a term = Z3.Expr.expr

  type ('a,'b) symbol = ('a,'b) typ * Expr.expr

  module Symbol = struct

    let get_typ = fst

    let declare (type a) (type b) (ty : (a,b) typ) s : (a,b) symbol =
      match ty with
        | Int -> Int, Arithmetic.Integer.mk_const_s ctx s
        | Bool -> Bool, Boolean.mk_const_s ctx s
        | Real -> Real, Arithmetic.Real.mk_const_s ctx s
        | Num -> Num, Arithmetic.Real.mk_const_s ctx s

    let term (type a) (type b) (ty : (a,b) typ) (e : b term) : (a,b) symbol =
      (ty, e)

    let trustme (type a) (type b) (ty : (a,b) typ) e : (a,b) symbol =
      (ty, e)

  end

  module T = struct
    let symbol s = snd s

    let eq x y = Boolean.mk_eq ctx x y
    let distinct x = Boolean.mk_distinct ctx x
    let ite b then_ else_ =
      Boolean.mk_ite ctx b then_ else_

    let true_       = Boolean.mk_true ctx
    let false_      = Boolean.mk_false ctx
    let and_ t      = Boolean.mk_and ctx t
    let or_ t       = Boolean.mk_or ctx t
    let not t       = Boolean.mk_not ctx t
    let imply t1 t2 = Boolean.mk_implies ctx t1 t2
    let iff t1 t2   = Boolean.mk_iff ctx t1 t2
    let xor t1 t2   = Boolean.mk_xor ctx t1 t2

    let ge t1 t2 = Arithmetic.mk_ge ctx t1 t2
    let le t1 t2 = Arithmetic.mk_le ctx t1 t2
    let gt t1 t2 = Arithmetic.mk_gt ctx t1 t2
    let lt t1 t2 = Arithmetic.mk_lt ctx t1 t2

    (* We go through strings since we would loss precision otherwise. *)
    let int i = Arithmetic.Integer.mk_numeral_s ctx @@ Z.to_string i
    let rat x = Arithmetic.Real.mk_numeral_s ctx @@ Q.to_string x
    let i2q t = Arithmetic.Integer.mk_int2real ctx t
    let q2i t = Arithmetic.Real.mk_real2int ctx t


    let neg t = Arithmetic.mk_unary_minus ctx t
    let add t = Arithmetic.mk_add ctx t
    let sub t = Arithmetic.mk_sub ctx t
    let mul t = Arithmetic.mk_mul ctx t
    let div t1 t2 = Arithmetic.mk_div ctx t1 t2

    let mod_ t1 t2 = Arithmetic.Integer.mk_mod ctx t1 t2
    let rem t1 t2  = Arithmetic.Integer.mk_rem ctx t1 t2

    let ( ! ) x = symbol x
    let ( = ) x y = eq x y
    let ( <> ) x y = distinct [x ; y]

    let ( && ) x y = and_ [ x ; y ]
    let ( || ) x y = or_ [ x ; y ]
    let ( <=> ) x y = iff x y
    let ( ==> ) x y = imply x y
    let ( lxor ) x y = xor x y

    let ( < )  x y = lt x y
    let ( <= ) x y = le x y
    let ( > )  x y = gt x y
    let ( >= ) x y = ge x y

    let ( + ) x y = add [ x ; y ]
    let ( - ) x y = sub [ x ; y ]
    let ( * ) x y = mul [ x ; y ]
    let ( / ) x y = div x y

    let ( mod ) x y = mod_ x y

    let simplify ?params t = Expr.simplify t params

    let to_string t = Expr.to_string t

    let raw t = t

  end


  type sat =
    | Unsat of Z3.Expr.expr Lazy.t (** Proof *)
    | Sat of Z3.Model.model Lazy.t (** Model *)
    | Unkown of string (** Reason *)

  (** {2 Solver calls} *)
  module Solver = struct

    let make () =
      Z3.Solver.mk_simple_solver ctx

    let add ~solver x =
      Z3.Solver.add solver [x]

    let check ~solver l =
      let open Z3.Solver in
      match check solver (l) with
        | UNSATISFIABLE -> Unsat (lazy (opt_get @@ get_proof solver))
        | UNKNOWN -> Unkown (get_reason_unknown solver)
        | SATISFIABLE -> Sat (lazy (opt_get @@ get_model solver))

  end

  (** {2 Model extraction} *)
  module Model = struct

    exception No_value of Expr.expr

    let bool_of_lbool = function
      | Z3enums.L_TRUE -> true
      | Z3enums.L_FALSE -> false
      | Z3enums.L_UNDEF -> raise (Z3.Error "lbool")

    let get_value (type a) (type b) ~model ((ty,t) : (a, b) symbol) : a =
      let x = match Model.eval model t true with
        | None -> raise (No_value t)
        | Some x -> x
      in
      match ty with
        | Int -> Z.of_string @@ Arithmetic.Integer.to_string x
        | Bool -> bool_of_lbool @@ Boolean.get_bool_value x
        | Real -> Q.of_string @@ Arithmetic.Real.to_string x
        | Num -> Q.of_string @@ Arithmetic.Real.to_string x
  end


end
