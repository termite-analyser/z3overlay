(** An overlay for the OCaml Z3 bindings. *)

module type Context = sig

  val ctx : Z3.context

end

type seq
type regex

module Make (C:Context) : sig

  val ctx : Z3.context

  type zint  = [ `Int ]
  type zbool = [ `Bool ]
  type zreal = [ `Real ]

  type znum = [ zint | zreal ]
  type zany = [ zint | zbool | zreal ]

  type zchar = [ `Char ]
  type 'a zseq = [ `Seq of 'a ]
  type zstring = zchar zseq
  type 'a zregex = [ `Regex of 'a ]

  type ('domain, 'range) zarray = [ `Zarray of ('domain * 'range) ]

  type (_,_) typ =
    | Int : (Z.t, [> zint]) typ
    | Bool : (bool, [> zbool]) typ
    | Real : (Q.t, [> zreal]) typ
    | Num : (Q.t, [> znum] ) typ
    | Array : ('a, 'x) typ * ('b, 'y) typ -> ('a -> 'b, ('x, 'y) zarray ) typ
    | Seq : ('a, 'x) typ -> (regex, 'x zseq) typ
    | String : (string, zchar zseq) typ
    | Regex : ('a, 'x) typ -> (regex, 'x zregex) typ

  type +'a term = private Z3.Expr.expr

  type ('a,'b) symbol

  module Symbol : sig

    val get_typ : ('a, 'b) symbol -> ('a, 'b) typ

    val declare : ('a, 'b) typ -> string -> ('a, 'b) symbol

    val term : ('a, 'b) typ -> 'b term -> ('a, 'b) symbol

    (** Unsafe cast. Use at your own risks. *)
    val trustme : ('a, 'b) typ -> Z3.Expr.expr -> ('a, 'b) symbol
  end

  (** Term constructors. Direct calls to the Z3 api. *)
  module T : sig

    val symbol : (_,'a) symbol -> 'a term
    val simplify : ?params:Z3.Params.params -> 'a term -> 'a term
    val eq : 'a term -> 'a term -> [> zbool] term
    val distinct : 'a term list -> [> zbool] term
    val ite : [< zbool ] term -> ([< zany ] as 'a) term -> 'a term -> 'a term

    val int : int -> [> zint ] term
    val bigint : Z.t -> [> zint ] term
    val rat : Q.t -> [> zreal ] term
    val i2q : [< zint ] term -> [> zreal ] term
    val q2i : [< zreal ] term -> [> zint ] term

    val true_ : [> zbool ] term
    val false_ : [> zbool ] term
    val bool : bool -> [> zbool ] term
    val and_ : [< zbool ] term list -> [> zbool ] term
    val or_ : [< zbool ] term list -> [> zbool ] term
    val not : [< zbool ] term -> [> zbool ] term
    val imply : [< zbool ] term -> [< zbool ] term -> [> zbool ] term
    val iff : [< zbool ] term -> [< zbool ] term -> [> zbool ] term
    val xor : [< zbool ] term -> [< zbool ] term -> [> zbool ] term

    val ge : [< znum ] term -> [< znum ] term -> [> zbool ] term
    val le : [< znum ] term -> [< znum ] term -> [> zbool ] term
    val gt : [< znum ] term -> [< znum ] term -> [> zbool ] term
    val lt : [< znum ] term -> [< znum ] term -> [> zbool ] term

    val neg : ([< znum ] as 'a) term -> 'a term
    val add : ([< znum ] as 'a) term list -> 'a term
    val sub : ([< znum ] as 'a) term list -> 'a term
    val mul : ([< znum ] as 'a) term list -> 'a term
    val ixor : [< zint ] term -> [< zint ] term -> [> zint ] term

    val div : ([< znum ] as 'a) term -> 'a term -> 'a term
    val mod_ : [< zint ] term -> [< zint ] term -> [> zint ] term
    val rem : [< znum ] term -> [< znum ] term -> [> znum ] term

    val ( ! ) : (_,'a) symbol -> 'a term
    val ( = ) : 'a term -> 'a term -> [> zbool] term
    val ( <> ) : 'a term -> 'a term -> [> zbool] term

    val ( && )   : [< zbool ] term -> [< zbool ] term -> [> zbool ] term
    val ( || )   : [< zbool ] term -> [< zbool ] term -> [> zbool ] term
    val ( <=> )  : [< zbool ] term -> [< zbool ] term -> [> zbool ] term
    val ( ==> )  : [< zbool ] term -> [< zbool ] term -> [> zbool ] term
    val ( lxor ) : [< zbool ] term -> [< zbool ] term -> [> zbool ] term

    val ( < )  : [< znum ] term -> [< znum ] term -> [> zbool ] term
    val ( <= ) : [< znum ] term -> [< znum ] term -> [> zbool ] term
    val ( > )  : [< znum ] term -> [< znum ] term -> [> zbool ] term
    val ( >= ) : [< znum ] term -> [< znum ] term -> [> zbool ] term

    val ( + ) : ([< znum ] as 'a) term -> 'a term -> 'a term
    val ( - ) : ([< znum ] as 'a) term -> 'a term -> 'a term
    val ( * ) : ([< znum ] as 'a) term -> 'a term -> 'a term
    val ( / ) : ([< znum ] as 'a) term -> 'a term -> 'a term

    val ( mod ) : [< zint ] term -> [< zint ] term -> [> zint ] term

    val with_typ : ('a, 'b) typ -> 'a -> 'b term

    val to_string : 'a term -> string
    val raw : 'a term -> Z3.Expr.expr

  end

  module Z3Array : sig
    val get : [< ('d, 'r) zarray] term -> 'd term -> 'r term

    val set :
      [< ('d, 'r) zarray] term -> 'd term -> 'r term -> [> ('d, 'r) zarray] term
    val make :
      ('a -> 'b, ('d, 'r) zarray) typ -> 'r term -> [> ('d, 'r) zarray] term

    val default : [< ('d, 'r) zarray] term -> 'r term

    val of_indexed :
      typ:('a, 'r) typ -> default:'r term ->
      'r term array -> ([> zint ], 'r) zarray term

    val of_array :
      typ:('a -> 'b, ('d, 'r) zarray) typ -> default:'r term ->
      ('d term * 'r term) array -> ('d, 'r) zarray term

    val of_list :
      typ:('a -> 'b, ('d, 'r) zarray) typ -> default:'r term ->
      ('d term * 'r term) list -> ('d, 'r) zarray term

  end

  module Z3Seq : sig
    type 'a t = 'a zseq term

    val empty : ('a, 'b) typ -> 'b t
    val singleton : 'a term -> 'a t
    val concat : 'a t list -> 'a t
    val prefix : prefix:'a t -> 'a t -> zbool term
    val suffix : suffix:'a t -> 'a t -> zbool term
    val contains : 'a t -> 'a t -> zbool term

    val at : 'a t -> int term -> 'a term
    val length : 'a t -> int term

    val of_string : string -> zstring term

    val (@.) : 'a t -> int term -> 'a term
  end


  module Z3Regex : sig
    type 'a t = 'a zregex term
    
    val from_seq : 'a Z3Seq.t -> 'a t
    val in_re : 'a Z3Seq.t -> 'a t -> zbool term
    val plus : 'a t -> 'a t
    val star : 'a t -> 'a t
    val option : 'a t -> 'a t
    val union : 'a t list -> 'a t
    val concat : 'a t list -> 'a t
    val range : 'a term -> 'a term -> 'a t
    val loop : 'a t -> int -> int -> 'a t
    val inter : 'a t list -> 'a t
    val complement : 'a t -> 'a t
    val empty : ('a, 'b) typ -> 'a t
    val any : ('a, 'b) typ -> 'a t

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
  
  module Solver : SOLVER
    with type t = Z3.Solver.solver

  module Optimize : sig

    include SOLVER
      with type t = Z3.Optimize.optimize

    type handle

    val add_soft :
      id:Z3.Symbol.symbol ->
      solver:t ->
      weight:string -> zbool term -> handle

    val maximize :
      solver:t ->
      [< znum] term -> handle
    val minimize :
      solver:t ->
      [< znum] term -> handle

    val get_upper :
      handle -> (Q.t, [> znum ]) symbol

    val get_lower :
      handle -> (Q.t, [> znum ]) symbol

  end

  module Model : sig

    val get_value : model:Z3.Model.model -> ('a, 'b) symbol -> 'a

  end

end

