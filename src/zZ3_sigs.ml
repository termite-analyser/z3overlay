(** Signatures for z3overlay. *)

(** Input signature of the functor, only a context. *)
module type Context = sig

  val ctx : Z3.context

end

module type SOLVER = sig

  type t
  type sat
  type _ term

  val make : unit -> t

  val push : t -> unit
  val pop : t -> unit

  val add : solver:t -> bool term -> unit
  val check : solver:t -> bool term list -> sat

end

(** Output signature of the functor. *)
module type S = sig

  val ctx : Z3.context

  type _ typ =
    | Int : Z.t typ
    | Bool : bool typ
    | Real : Q.t typ
    | Array : 'a typ * 'b typ -> ('a -> 'b) typ

  type +'a term = private Z3.Expr.expr

  type 'a symbol

  module Symbol : sig

    val get_typ : 'a symbol -> 'a typ

    val declare : 'a typ -> string -> 'a symbol

    val term : 'a typ -> 'a term -> 'a symbol

    (** Unsafe cast. Use at your own risks. *)
    val trustme : 'a typ -> Z3.Expr.expr -> 'a symbol
  end

  (** Term constructors. Direct calls to the Z3 api. *)
  module T : sig

    val symbol : _ symbol -> 'a term
    val simplify : ?params:Z3.Params.params -> 'a term -> 'a term
    val eq : 'a term -> 'a term -> bool term
    val distinct : 'a term list -> bool term
    val ite : bool term -> 'a term -> 'a term -> 'a term

    val int : int -> Z.t term
    val bigint : Z.t -> Z.t term
    val rat : Q.t -> Q.t term
    val i2q : Z.t term -> Q.t term
    val q2i : Q.t term -> Z.t term

    val true_ : bool term
    val false_ : bool term
    val bool : bool -> bool term
    val and_ : bool term list -> bool term
    val or_ : bool term list -> bool term
    val not : bool term -> bool term
    val imply : bool term -> bool term -> bool term
    val iff : bool term -> bool term -> bool term
    val xor : bool term -> bool term -> bool term

    val ge : _ term -> _ term -> bool term
    val le : _ term -> _ term -> bool term
    val gt : _ term -> _ term -> bool term
    val lt : _ term -> _ term -> bool term

    val neg : Z.t term -> Z.t term
    val add : Z.t term list -> Z.t term
    val sub : Z.t term list -> Z.t term
    val mul : Z.t term list -> Z.t term
    val div : Z.t term -> Z.t term -> Z.t term

    val ixor : Z.t term -> Z.t term -> Z.t term
    val mod_ : Z.t term -> Z.t term -> Z.t term
    val rem : Z.t term -> Z.t term -> Q.t term

    val negf : Q.t term -> Q.t term
    val addf : Q.t term list -> Q.t term
    val subf : Q.t term list -> Q.t term
    val mulf : Q.t term list -> Q.t term
    val divf : Q.t term -> Q.t term -> Q.t term

    val ( ! ) : 'a symbol -> 'a term
    val ( = ) : 'a term -> 'a term -> bool term
    val ( <> ) : 'a term -> 'a term -> bool term

    val ( && )   : bool term -> bool term -> bool term
    val ( || )   : bool term -> bool term -> bool term
    val ( <=> )  : bool term -> bool term -> bool term
    val ( ==> )  : bool term -> bool term -> bool term
    val ( lxor ) : bool term -> bool term -> bool term

    val ( < )  : _ term -> _ term -> bool term
    val ( <= ) : _ term -> _ term -> bool term
    val ( > )  : _ term -> _ term -> bool term
    val ( >= ) : _ term -> _ term -> bool term

    val ( ~- ) : Z.t term -> Z.t term
    val ( + ) : Z.t term -> Z.t term -> Z.t term
    val ( - ) : Z.t term -> Z.t term -> Z.t term
    val ( * ) : Z.t term -> Z.t term -> Z.t term
    val ( / ) : Z.t term -> Z.t term -> Z.t term

    val ( mod ) : Z.t term -> Z.t term -> Z.t term

    val ( ~-. ) : Q.t term -> Q.t term
    val ( +. ) : Q.t term -> Q.t term -> Q.t term
    val ( -. ) : Q.t term -> Q.t term -> Q.t term
    val ( *. ) : Q.t term -> Q.t term -> Q.t term
    val ( /. ) : Q.t term -> Q.t term -> Q.t term

    val with_typ : 'a typ -> 'a -> 'b term

    val to_string : 'a term -> string
    val raw : 'a term -> Z3.Expr.expr

  end

  module Z3Array : sig
    val get : ('d -> 'r) term -> 'd term -> 'r term

    val set :
      ('d -> 'r) term -> 'd term -> 'r term -> ('d -> 'r) term
    val make :
      ('d -> 'r) typ -> 'r term -> ('d -> 'r) term

    val default : ('d -> 'r) term -> 'r term

    val of_indexed :
      typ:'a typ -> default:'r term ->
      'r term array -> (Z.t -> 'r) term

    val of_array :
      typ:('d -> 'r) typ -> default:'r term ->
      ('d term * 'r term) array -> ('d -> 'r) term

    val of_list :
      typ:('d -> 'r) typ -> default:'r term ->
      ('d term * 'r term) list -> ('d -> 'r) term

  end


  type sat =
    | Unsat of Z3.Expr.expr Lazy.t (** Proof *)
    | Sat of Z3.Model.model Lazy.t (** Model *)
    | Unkown of string (** Reason *)

  module Solver : SOLVER
    with type t = Z3.Solver.solver
     and type sat := sat
     and type 'a term := 'a term

  module Optimize : sig

    include SOLVER
      with type t = Z3.Optimize.optimize
       and type sat := sat
       and type 'a term := 'a term

    type handle

    val add_soft :
      id:Z3.Symbol.symbol ->
      solver:t ->
      weight:string -> bool term -> handle

    val maximize :
      solver:t ->
      Q.t term -> handle
    val minimize :
      solver:t ->
      Q.t term -> handle

    val get_upper :
      objective:handle -> int -> Q.t symbol

    val get_lower :
      objective:handle -> int -> Q.t symbol

  end

  module Model : sig

    val get_value : model:Z3.Model.model -> 'a symbol -> 'a

  end

end
