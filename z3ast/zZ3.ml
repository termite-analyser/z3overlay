open Z3

type zint = [ `int ]
type zbool = [ `bool ]
type zreal = [ `real ]

type znum = [ zint | zreal ]
type zany = [ zint | zbool | zreal ]

type (_, _) typ =
  | Int : (int, [> zint]) typ
  | Bool : (bool, [> zbool]) typ
  | Real : (int * int, [> zreal]) typ
  | Num : (int * int, [> znum] ) typ

type ('a,'b) symbol = ('a,'b) typ * Expr.expr

and _ t =

  | S : (_,'a) symbol -> 'a t
  | Eq : ((_,'a) typ * 'a t * 'a t) -> [> zbool] t
  | Distinct : ((_,'a) typ * 'a t list) -> [> zbool] t
  | Ite : [< zbool ] t * ([< zany ] as 'a) t * 'a t -> 'a t

  | I : int -> [> zint ] t
  | Q : int * int -> [> zreal ] t
  | I2Q : [< zint ] t -> [> zreal ] t

  | True : [> zbool ] t
  | False : [> zbool ] t
  | And : [< zbool ] t list -> [> zbool ] t
  | Or : [< zbool ] t list -> [> zbool ] t
  | Not : [< zbool ] t -> [> zbool ] t
  | Imply : ([< zbool ] t * [< zbool ] t) -> [> zbool ] t
  | Iff : ([< zbool ] t * [< zbool ] t) -> [> zbool ] t
  | Xor : ([< zbool ] t * [< zbool ] t) -> [> zbool ] t

  | Ge : ([< znum ] t * [< znum ] t) -> [> zbool ] t
  | Le : ([< znum ] t * [< znum ] t) -> [> zbool ] t
  | Gt : ([< znum ] t * [< znum ] t) -> [> zbool ] t
  | Lt : ([< znum ] t * [< znum ] t) -> [> zbool ] t

  | Neg : ([< znum ] as 'a) t -> 'a t
  | Add : ([< znum ] as 'a) t list -> 'a t
  | Sub : ([< znum ] as 'a) t list -> 'a t
  | Mul : ([< znum ] as 'a) t list -> 'a t

  | Div : (([< znum ] as 'a) t * 'a t) -> 'a t

  | Mod : [< zint ] t * [< zint ] t -> [> zint ] t


module T = struct

  let (!) x = S x

  let ( = ) x y = Eq (Num,x,y)
  let ( <> ) x y = Distinct (Num,[x ; y])

  let not x = Not x
  let ( && ) x y = And [ x ; y ]
  let ( || ) x y = Or [ x ; y ]
  let ( <=> ) x y = Iff (x,y)
  let ( ==> ) x y = Imply (x,y)
  let ( lxor ) x y = Xor (x,y)

  let ( < ) x y = Lt (x,y)
  let ( <= ) x y = Le (x,y)
  let ( > ) x y = Gt (x,y)
  let ( >= ) x y = Ge (x,y)

  let ( + ) x y = Add [ x ; y ]
  let ( - ) x y = Sub [ x ; y ]
  let ( * ) x y = Mul [ x ; y ]
  let ( / ) x y = Div (x, y)

  let ( mod ) x y = Mod (x,y)

end


(** {2 Transformation to {! Z3.Expr.expr }} *)

let decl_const (type a) (type b) ~ctx (ty : (a,b) typ) s : (a,b) symbol =
  match ty with
  | Int -> Int, Arithmetic.Integer.mk_const_s ctx s
  | Bool -> Bool, Boolean.mk_const_s ctx s
  | Real -> Real, Arithmetic.Real.mk_const_s ctx s
  | Num -> Num, Arithmetic.Real.mk_const_s ctx s


let rec to_expr : type a . ctx:Z3.context -> a t -> Z3.Expr.expr = fun ~ctx t ->
  let open Expr in
  let open Boolean in
  let open Arithmetic in
  match t with
    | S s -> snd s
    | Eq (_, x,y) -> mk_eq ctx (to_expr ~ctx x) (to_expr ~ctx y)
    | Distinct (_,x) -> mk_distinct ctx (List.map (to_expr ~ctx) x)
    | Ite (b, then_, else_) ->
        mk_ite ctx (to_expr ~ctx b) (to_expr ~ctx then_) (to_expr ~ctx else_)

    | I i -> Integer.mk_numeral_i ctx i
    | Q (n,d) -> Real.mk_numeral_nd ctx n d
    | I2Q t -> to_expr ~ctx t

    | True -> mk_true ctx
    | False -> mk_false ctx
    | And t -> mk_and ctx (List.map (to_expr ~ctx) t)
    | Or t -> mk_or ctx (List.map (to_expr ~ctx) t)
    | Not t -> mk_not ctx (to_expr ~ctx t)
    | Imply (t1,t2) -> mk_implies ctx (to_expr ~ctx t1) (to_expr ~ctx t2)
    | Iff (t1,t2) -> mk_iff ctx (to_expr ~ctx t1) (to_expr ~ctx t2)
    | Xor (t1,t2) -> mk_xor ctx (to_expr ~ctx t1) (to_expr ~ctx t2)

    | Ge (t1,t2) -> mk_ge ctx (to_expr ~ctx t1) (to_expr ~ctx t2)
    | Le (t1,t2) -> mk_le ctx (to_expr ~ctx t1) (to_expr ~ctx t2)
    | Gt (t1,t2) -> mk_gt ctx (to_expr ~ctx t1) (to_expr ~ctx t2)
    | Lt (t1,t2) -> mk_lt ctx (to_expr ~ctx t1) (to_expr ~ctx t2)

    | Neg t -> mk_unary_minus ctx (to_expr ~ctx t)
    | Add t -> mk_add ctx (List.map (to_expr ~ctx) t)
    | Sub t -> mk_sub ctx (List.map (to_expr ~ctx) t)
    | Mul t -> mk_mul ctx (List.map (to_expr ~ctx) t)

    | Div (t1,t2) -> mk_div ctx (to_expr ~ctx t1) (to_expr ~ctx t2)

    | Mod (t1,t2) -> Integer.mk_mod ctx (to_expr ~ctx t1) (to_expr ~ctx t2)




(** {2 Extraction from {! Z3.Expr.expr }} *)

exception Not_handled of Expr.expr

let ex_bin f t = match Expr.get_args t with
  | [ t1 ; t2 ] -> (f t1,f t2)
  | _ -> raise (Not_handled t)

let ex_ter t =  match Expr.get_args t with
  | [ t1 ; t2 ; t3 ] -> (t1,t2,t3)
  | _ -> raise (Not_handled t)

let bool_of_lbool = function
  | Z3enums.L_TRUE -> true
  | Z3enums.L_FALSE -> false
  | Z3enums.L_UNDEF -> raise (Z3.Error "lbool")

let rec of_any_expr t =
  let open Expr in
  let open Boolean in
  let open Arithmetic in
  if is_bool t then `Bool (of_bool_expr t)
  else if is_real t then `Real (of_num_expr t)
  else if is_int t then `Int (of_num_expr t)

  else raise (Not_handled t)

and of_bool_expr t : zbool t =
  let open Expr in
  let open Boolean in
  let open Arithmetic in

  if is_true t then True
  else if is_false t then False
  else if is_and t then And (List.map of_bool_expr @@ get_args t)
  else if is_or t then Or (List.map of_bool_expr @@ get_args t)
  else if is_not t then Not (of_bool_expr t)
  else if is_implies t then Not (of_bool_expr t)
  else if is_iff t then Iff (ex_bin of_bool_expr t)
  else if is_xor t then Xor (ex_bin of_bool_expr t)

  else if is_ge t then Ge (ex_bin of_num_expr t)
  else if is_le t then Le (ex_bin of_num_expr t)
  else if is_gt t then Gt (ex_bin of_num_expr t)
  else if is_lt t then Lt (ex_bin of_num_expr t)

  else raise (Not_handled t)

and of_num_expr t : [> znum ] t =
  let open Expr in
  let open Arithmetic in
  if is_int_numeral t then I (Integer.get_int t)
  else if is_arithmetic_numeral t then
    let n = Integer.get_int @@ Real.get_numerator t in
    let d = Integer.get_int @@ Real.get_denominator t in
    Q (n,d)

  else if is_uminus t then Neg (of_num_expr t)
  else if is_add t then Add (List.map of_num_expr @@ get_args t)
  else if is_sub t then Sub (List.map of_num_expr @@ get_args t)
  else if is_mul t then Mul (List.map of_num_expr @@ get_args t)
  else if is_div t then Div (ex_bin of_num_expr t)

  else raise (Not_handled t)


(** {2 Handy overlay functions} *)

let add ~ctx ~solver (x : zbool t) =
  Z3.Solver.add solver [to_expr ~ctx x]

type sat =
  | Unsat of Z3.Expr.expr Lazy.t (** Proof *)
  | Sat of Z3.Model.model Lazy.t (** Model *)
  | Unkown of string (** Reason *)

let opt_get = function
  | None -> raise @@ Z3.Error "opt_get"
  | Some x -> x

let check ~ctx ~solver l =
  let open Z3.Solver in
  match check solver (List.map (to_expr ~ctx) l) with
    | UNSATISFIABLE -> Unsat (lazy (opt_get @@ get_proof solver))
    | UNKNOWN -> Unkown (get_reason_unknown solver)
    | SATISFIABLE -> Sat (lazy (opt_get @@ get_model solver))

(** {2 Model extraction} *)

exception No_value of Expr.expr

let get_real t =
  let open Arithmetic in
  let n = Integer.get_int @@ Real.get_numerator t in
  let d = Integer.get_int @@ Real.get_denominator t in
  (n,d)


let get_value (type a) (type b) ~model ((ty,t) : (a, b) symbol) : a =
  let x = match Model.eval model t true with
    | None -> raise (No_value t)
    | Some x -> x
  in
  match ty with
  | Int -> Arithmetic.Integer.get_int x
  | Bool -> bool_of_lbool @@ Boolean.get_bool_value x
  | Real -> get_real x
  | Num -> get_real x
