open Z3

module type Context = sig

  val ctx : Z3.context

end


module type S = sig

  val ctx : Z3.context

  type zint  = [ `Int ]
  type zbool = [ `Bool ]
  type zreal = [ `Real ]

  type znum = [ zint | zreal ]
  type zany = [ zint | zbool | zreal ]

  type (_,_) typ =
    | Int : (Z.t, [> zint]) typ
    | Bool : (bool, [> zbool]) typ
    | Real : (Q.t, [> zreal]) typ
    | Num : (Q.t, [> znum] ) typ


  module Symbol : sig
    type ('a,'b) t

    val get_typ : ('a, 'b) t -> ('a, 'b) typ

    val declare : ('a, 'b) typ -> string -> ('a, 'b) t

    (** Unsafe cast. Use at your own risks. *)
    val trustme : ('a, 'b) typ -> Z3.Expr.expr -> ('a, 'b) t
  end

  type +'a t = private Z3.Expr.expr

  (** Term constructors. Direct calls to the Z3 api. *)
  module T : sig

    val symbol : (_,'a) Symbol.t -> 'a t
    val eq : 'a t -> 'a t -> [> zbool] t
    val distinct : 'a t list -> [> zbool] t
    val ite : [< zbool ] t -> ([< zany ] as 'a) t -> 'a t -> 'a t

    val int : Z.t -> [> zint ] t
    val rat : Q.t -> [> zreal ] t
    val i2q : [< zint ] t -> [> zreal ] t
    val q2i : [< zreal ] t -> [> zint ] t

    val true_ : [> zbool ] t
    val false_ : [> zbool ] t
    val and_ : [< zbool ] t list -> [> zbool ] t
    val or_ : [< zbool ] t list -> [> zbool ] t
    val not : [< zbool ] t -> [> zbool ] t
    val imply : [< zbool ] t -> [< zbool ] t -> [> zbool ] t
    val iff : [< zbool ] t -> [< zbool ] t -> [> zbool ] t
    val xor : [< zbool ] t -> [< zbool ] t -> [> zbool ] t

    val ge : [< znum ] t -> [< znum ] t -> [> zbool ] t
    val le : [< znum ] t -> [< znum ] t -> [> zbool ] t
    val gt : [< znum ] t -> [< znum ] t -> [> zbool ] t
    val lt : [< znum ] t -> [< znum ] t -> [> zbool ] t

    val neg : ([< znum ] as 'a) t -> 'a t
    val add : ([< znum ] as 'a) t list -> 'a t
    val sub : ([< znum ] as 'a) t list -> 'a t
    val mul : ([< znum ] as 'a) t list -> 'a t

    val div : ([< znum ] as 'a) t -> 'a t -> 'a t
    val mod_ : [< zint ] t -> [< zint ] t -> [> zint ] t
    val rem : [< znum ] t -> [< znum ] t -> [> znum ] t

    val ( ! ) : (_,'a) Symbol.t -> 'a t
    val ( = ) : 'a t -> 'a t -> [> zbool] t
    val ( <> ) : 'a t -> 'a t -> [> zbool] t

    val ( && )   : [< zbool ] t -> [< zbool ] t -> [> zbool ] t
    val ( || )   : [< zbool ] t -> [< zbool ] t -> [> zbool ] t
    val ( <=> )  : [< zbool ] t -> [< zbool ] t -> [> zbool ] t
    val ( ==> )  : [< zbool ] t -> [< zbool ] t -> [> zbool ] t
    val ( lxor ) : [< zbool ] t -> [< zbool ] t -> [> zbool ] t

    val ( < )  : [< znum ] t -> [< znum ] t -> [> zbool ] t
    val ( <= ) : [< znum ] t -> [< znum ] t -> [> zbool ] t
    val ( > )  : [< znum ] t -> [< znum ] t -> [> zbool ] t
    val ( >= ) : [< znum ] t -> [< znum ] t -> [> zbool ] t

    val ( + ) : ([< znum ] as 'a) t -> 'a t -> 'a t
    val ( - ) : ([< znum ] as 'a) t -> 'a t -> 'a t
    val ( * ) : ([< znum ] as 'a) t -> 'a t -> 'a t
    val ( / ) : ([< znum ] as 'a) t -> 'a t -> 'a t

    val ( mod ) : [< zint ] t -> [< zint ] t -> [> zint ] t


  end


  type sat =
    | Unsat of Z3.Expr.expr Lazy.t (** Proof *)
    | Sat of Z3.Model.model Lazy.t (** Model *)
    | Unkown of string (** Reason *)

  module Solver : sig

    val make : unit -> Solver.solver

    val add : solver:Solver.solver -> zbool t -> unit

    val check : solver:Z3.Solver.solver -> zbool t list -> sat

  end

  module Model : sig

    val get_value : model:Model.model -> ('a, 'b) Symbol.t -> 'a

  end

end
