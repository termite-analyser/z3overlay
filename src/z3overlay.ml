open Z3

let opt_get = function
  | None -> raise @@ Z3.Error "opt_get"
  | Some x -> x

module type Context = sig

  val ctx : Z3.context

end

type seq = unit
type regex = unit

module Make (C : Context) = struct

  let ctx = C.ctx

  type zint  = [ `Int ]
  type zbool = [ `Bool ]
  type zreal = [ `Real ]

  type znum = [ zint | zreal ]
  type zany = [ zint | zbool | zreal ]

  type ('domain, 'range) zarray = [ `Zarray of ('domain * 'range) ]

  type zchar = [ `Char ]
  type 'el zseq = [ `Seq of 'el ]
  type zstring = zchar zseq
  type 'el zregex = [ `Regex of 'el ]

  type (_, _) typ =
    | Int : (Z.t, [> zint]) typ
    | Bool : (bool, [> zbool]) typ
    | Real : (Q.t, [> zreal]) typ
    | Num : (Q.t, [> znum] ) typ
    | Array : ('a, 'x) typ * ('b, 'y) typ -> ('a -> 'b, ('x,'y) zarray ) typ
    | Seq : ('a,'x) typ -> (seq, 'x zseq ) typ
    | String : (string, zchar zseq ) typ
    | Regex : ('a,'x) typ -> (regex, 'x zregex ) typ

  type +'a term = Z3.Expr.expr

  type ('a,'b) symbol = ('a,'b) typ * Expr.expr

  module Symbol = struct

    let get_typ = fst

    let rec sort : type a b . (a,b) typ -> Sort.sort =
      function
        | Int -> Arithmetic.Integer.mk_sort ctx
        | Bool -> Boolean.mk_sort ctx
        | Real -> Arithmetic.Real.mk_sort ctx
        | Num -> Arithmetic.Real.mk_sort ctx
        | Array (src,dst) -> Z3Array.mk_sort ctx (sort src) (sort dst)
        | Seq el -> Seq.mk_seq_sort ctx @@ sort el
        | String -> Seq.mk_string_sort ctx
        | Regex el -> Seq.mk_re_sort ctx @@ sort el

    let declare (type a) (type b) (ty : (a,b) typ) s : (a,b) symbol =
      match ty with
        | Int -> Int, Arithmetic.Integer.mk_const_s ctx s
        | Bool -> Bool, Boolean.mk_const_s ctx s
        | Real -> Real, Arithmetic.Real.mk_const_s ctx s
        | Num -> Num, Arithmetic.Real.mk_const_s ctx s
        | Array (src,dst) ->
            Array (src,dst),
            Z3Array.mk_const_s ctx s (sort src) (sort dst)
        | String -> String, Expr.mk_const_s ctx s (sort String)
        | Seq el -> Seq el, Expr.mk_const_s ctx s (sort @@ Seq el)
        | Regex el -> Regex el, Expr.mk_const_s ctx s (sort @@ Regex el)

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

    let ( + ) x y = add [ x ; y ]
    let ( - ) x y = sub [ x ; y ]
    let ( * ) x y = mul [ x ; y ]
    let ( / ) x y = div x y

    let ( mod ) x y = mod_ x y

    let simplify ?params t = Expr.simplify t params

    let to_string t = Expr.to_string t

    let raw t = t

    let with_typ : type a b . (a, b) typ -> a -> b term =
      fun ty x -> match ty with
        | Int -> bigint x
        | Real -> rat x
        | Num -> rat x
        | Bool -> bool x
        | String | Seq _ | Regex _
        | Array (_,_) -> raise @@ Error "Can't reify an array"

  end


  module Z3Array = struct
    open Z3Array

    let get a i = mk_select ctx a i
    let set a i v = mk_store ctx a i v

    let make (Array (src,_)) v =
      mk_const_array ctx (Symbol.sort src) v

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

  module Z3Seq = struct
    type 'a t = 'a zseq term

    let empty ty = Seq.mk_seq_empty ctx (Symbol.sort ty)
    let singleton x = Seq.mk_seq_unit ctx x
    let concat l = Seq.mk_seq_concat ctx l
    let prefix ~prefix:l1 l2 = Seq.mk_seq_prefix ctx l1 l2
    let suffix ~suffix:l1 l2 = Seq.mk_seq_suffix ctx l1 l2
    let contains l1 l2 = Seq.mk_seq_contains ctx l1 l2

    let at a i = Seq.mk_seq_at ctx a i
    let length a = Seq.mk_seq_length ctx a

    let of_string s = Seq.mk_string ctx s

    let (@.) = at
  end

  module Z3Regex = struct
    type 'a t = 'a zregex term

    let from_seq s = Seq.mk_seq_to_re ctx s
    let in_re s re = Seq.mk_seq_in_re ctx s re
    let plus re = Seq.mk_re_plus ctx re
    let star re = Seq.mk_re_star ctx re
    let option re = Seq.mk_re_option ctx re
    let union reL = Seq.mk_re_union ctx reL
    let concat reL = Seq.mk_re_concat ctx reL
    let range i j = Seq.mk_re_range ctx (Z3Seq.singleton i) (Z3Seq.singleton j)
    let loop re i j = Seq.mk_re_loop ctx re i j
    let inter reL = Seq.mk_re_intersect ctx (List.length reL) reL
    let complement re = Seq.mk_re_complement ctx re
    let empty ty = Seq.mk_re_empty ctx (Symbol.sort ty)
    let any ty = Seq.mk_re_full ctx (Symbol.sort ty)

  end
  

  type sat =
    | Unsat of Z3.Expr.expr Lazy.t (** Proof *)
    | Sat of Z3.Model.model Lazy.t (** Model *)
    | Unkown of string (** Reason *)

  module type SOLVER = sig
    type t

    val make : unit -> t

    val push : t -> unit
    val pop : t -> unit

    val add : solver:t -> [> zbool] term -> unit
    val check : solver:t -> [> zbool] term list -> sat
  end
  
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

    let get_upper handle =
      Symbol.term Num (get_upper handle)

    let get_lower handle =
      Symbol.term Num (get_lower handle)

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

    let get_value (type a) (type b) ~model ((ty,t) : (a, b) symbol) : a =
      let get_val t = match Model.eval model t true with
        | None -> raise (No_value t)
        | Some x -> x
      in
      let rec aux : type a b . (a, b) typ -> b term -> a = fun ty t ->
        match ty with
          | Int -> Z.of_string @@ Arithmetic.Integer.numeral_to_string @@ get_val t
          | Bool -> bool_of_lbool @@ Boolean.get_bool_value @@ get_val t
          | Real -> Q.of_string @@ Arithmetic.Real.numeral_to_string @@ get_val t
          | Num -> Q.of_string @@ Arithmetic.Real.numeral_to_string @@ get_val t
          | Array (src, dst) -> begin
              let f v = aux dst (Z3Array.get t (T.with_typ src v))
              in f
            end
          | String -> Seq.get_string ctx @@ get_val t
          | Seq _ -> ()
          | Regex _ -> ()
      in aux ty t

  end


end
