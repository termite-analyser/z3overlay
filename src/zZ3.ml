open ZZ3Utils
open Z3
open ZZ3_sigs

module Make (C : Context) = struct

  let ctx = C.ctx

  type _ typ =
    | Int : Z.t typ
    | Bool : bool typ
    | Real : Q.t typ
    | Array : 'a typ * 'b typ -> ('a -> 'b) typ

  type +'a term = Z3.Expr.expr

  type 'a symbol = 'a typ * Expr.expr

  module Symbol = struct

    let get_typ = fst

    let rec sort : type a . a typ -> Sort.sort =
      function
        | Int -> Arithmetic.Integer.mk_sort ctx
        | Bool -> Boolean.mk_sort ctx
        | Real -> Arithmetic.Real.mk_sort ctx
        | Array (src,dst) -> Z3Array.mk_sort ctx (sort src) (sort dst)

    let declare (type a) (ty : a typ) s : a symbol =
      match ty with
        | Int -> Int, Arithmetic.Integer.mk_const_s ctx s
        | Bool -> Bool, Boolean.mk_const_s ctx s
        | Real -> Real, Arithmetic.Real.mk_const_s ctx s
        | Array (src,dst) ->
            Array (src,dst),
            Z3Array.mk_const_s ctx s (sort src) (sort dst)

    let term (type a) (ty : a typ) (e : a term) : a symbol =
      (ty, e)

    let trustme (type a) (ty : a typ) e : a symbol =
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
    let bool b      = if b then true_ else false_
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
    let int i = Arithmetic.Integer.mk_numeral_i ctx i
    let bigint i = Arithmetic.Integer.mk_numeral_s ctx @@ Z.to_string i
    let rat x =
      let s = Q.to_string x in
      try Arithmetic.Real.mk_numeral_s ctx s
      with _ -> failwith (Printf.sprintf "Real.mk_numeral_s parse error on %s." s)
    let i2q t = Arithmetic.Integer.mk_int2real ctx t
    let q2i t = Arithmetic.Real.mk_real2int ctx t


    let neg t = Arithmetic.mk_unary_minus ctx t
    let add t = Arithmetic.mk_add ctx t
    let sub t = Arithmetic.mk_sub ctx t
    let mul t = Arithmetic.mk_mul ctx t
    let div t1 t2 = Arithmetic.mk_div ctx t1 t2

    let negf t = Arithmetic.mk_unary_minus ctx t
    let addf t = Arithmetic.mk_add ctx t
    let subf t = Arithmetic.mk_sub ctx t
    let mulf t = Arithmetic.mk_mul ctx t
    let divf t1 t2 = Arithmetic.mk_div ctx t1 t2

    let ixor t1 t2 = BitVector.mk_xor ctx t1 t2

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

    let (~-) x = neg x
    let ( + ) x y = add [ x ; y ]
    let ( - ) x y = sub [ x ; y ]
    let ( * ) x y = mul [ x ; y ]
    let ( / ) x y = div x y

    let (~-.) x = negf x
    let ( +. ) x y = addf [ x ; y ]
    let ( -. ) x y = subf [ x ; y ]
    let ( *. ) x y = mulf [ x ; y ]
    let ( /. ) x y = divf x y

    let ( mod ) x y = mod_ x y

    let simplify ?params t = Expr.simplify t params

    let to_string t = Expr.to_string t

    let raw t = t

    let with_typ : type a . a typ -> a -> a term =
      fun ty x -> match ty with
        | Int -> bigint x
        | Real -> rat x
        | Bool -> bool x
        | Array (_,_) -> invalid_arg "ZZ3.with_typ: Can't reify an array"

  end


  module Z3Array = struct
    open Z3Array

    let get a i = mk_select ctx a i
    let set a i v = mk_store ctx a i v

    let make :
      type d r .(d -> r) typ -> r term -> (d -> r) term
      = fun k v ->
        match k with
        | Array (src, _) -> mk_const_array ctx (Symbol.sort src) v
        | Int ->
          (* The typechecker doesn't know that Z.t is not a function. *)
          assert false

    let default a =
      mk_term_array ctx a

    let of_array ~typ ~default arr =
      let a0 = make typ default in
      Array.fold_left (fun a (k,v) -> set a k v) a0 arr

    let of_indexed ~typ ~default arr =
      let a0 = make (Array (Int, typ)) default in
      let n = Array.length arr in
      let rec aux i a =
        if i < n then a
        else aux (i+1) @@ set a (T.int i) arr.(i)
      in aux 0 a0

    let of_list ~typ ~default arr =
      let a0 = make typ default in
      List.fold_left (fun a (k,v) -> set a k v) a0 arr

  end



  type sat =
    | Unsat of Z3.Expr.expr Lazy.t (** Proof *)
    | Sat of Z3.Model.model Lazy.t (** Model *)
    | Unkown of string (** Reason *)

  (** {2 Solver calls} *)
  module Solver = struct
    include Z3.Solver

    type t = solver

    let make () = mk_simple_solver ctx
    let add ~solver x = add solver [x]
    let pop x = pop x 1

    let check ~solver l =
      match check solver l with
        | UNSATISFIABLE -> Unsat (lazy (opt_get @@ get_proof solver))
        | UNKNOWN -> Unkown (get_reason_unknown solver)
        | SATISFIABLE -> Sat (lazy (opt_get @@ get_model solver))

  end

  (** {2 Optimizing solver calls} *)
  module Optimize = struct
    include Z3.Optimize
    type t = optimize

    let make () = mk_opt ctx

    let add ~solver x = add solver [T.raw x]

    let add_soft ~id ~solver ~weight x =
      add_soft solver (T.raw x) weight id

    let minimize ~solver x =
      minimize solver x

    let maximize ~solver x =
      maximize solver x

    let get_upper ~objective x =
      Symbol.term Real (get_upper objective x)

    let get_lower ~objective x =
      Symbol.term Real (get_lower objective x)

    let check ~solver l =
      List.iter (add ~solver) l ;
      let open Z3.Optimize in
      let v = match check solver with
        | Z3.Solver.UNSATISFIABLE ->
          Unsat (lazy (failwith "get_proof is not implemented for optimizing solvers."))
        | Z3.Solver.UNKNOWN -> Unkown (get_reason_unknown solver)
        | Z3.Solver.SATISFIABLE -> Sat (lazy (opt_get @@ get_model solver))
      in v

  end

  (** {2 Model extraction} *)
  module Model = struct

    exception No_value of Expr.expr

    let bool_of_lbool = function
      | Z3enums.L_TRUE -> true
      | Z3enums.L_FALSE -> false
      | Z3enums.L_UNDEF -> raise (Z3.Error "lbool")

    let get_value (type a) ~model ((ty,t) : a symbol) : a =
      let get_val t = match Model.eval model t true with
        | None -> raise (No_value t)
        | Some x -> x
      in
      let rec aux : type a . a typ -> a term -> a = fun ty t ->
        match ty with
          | Int -> Z.of_string @@ Arithmetic.Integer.numeral_to_string @@ get_val t
          | Bool -> bool_of_lbool @@ Boolean.get_bool_value @@ get_val t
          | Real -> Q.of_string @@ Arithmetic.Real.numeral_to_string @@ get_val t
          | Array (src, dst) -> begin
              let f v = aux dst (Z3Array.get t (T.with_typ src v))
              in f
            end
      in aux ty t

  end


end
