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
    val simplify : ?params:Params.params -> 'a term -> 'a term
    val eq : 'a term -> 'a term -> [> zbool] term
    val distinct : 'a term list -> [> zbool] term
    val ite : [< zbool ] term -> ([< zany ] as 'a) term -> 'a term -> 'a term

    val int : Z.t -> [> zint ] term
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


    val to_string : 'a term -> string
    val raw : 'a term -> Z3.Expr.expr

  end


  type sat =
    | Unsat of Z3.Expr.expr Lazy.t (** Proof *)
    | Sat of Z3.Model.model Lazy.t (** Model *)
    | Unkown of string (** Reason *)

  module Solver : sig

    val make : unit -> Solver.solver

    val add : solver:Solver.solver -> zbool term -> unit

    val check : solver:Z3.Solver.solver -> zbool term list -> sat

  end

  module Model : sig

    val get_value : model:Model.model -> ('a, 'b) symbol -> 'a

  end

end
