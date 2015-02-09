open Z3


module Optimize = struct

  type optimize = z3_native_object

  let create ( ctx : context ) ( no : Z3native.ptr ) =
    let res : optimize = { m_ctx = ctx ;
                         m_n_obj = null ;
                         inc_ref = Z3native.optimize_inc_ref ;
                         dec_ref = Z3native.optimize_dec_ref } in
    (z3obj_sno res ctx (Z3native.mk_optimize (context_gno ctx))) ;
    (z3obj_create res) ;
    res

  let get_help ( x : optimize ) =
    Z3native.optimize__get_help (z3obj_gnc x) (z3obj_gno x)

  let set_params ( x : optimize ) ( p : Params.params )=
    Z3native.optimize_set_params (z3obj_gnc x) (z3obj_gno x) (z3obj_gno p)

  let get_param_descrs ( x : optimize ) =
    Params.ParamDescrs.param_descrs_of_ptr (z3obj_gc x) (Z3native.optimize_get_param_descrs (z3obj_gnc x) (z3obj_gno x))


  let add ( x : optimize ) ( constraints : expr list ) =
    let f e = (Z3native.optimize_assert (z3obj_gnc x) (z3obj_gno x) (Expr.gno e)) in
    ignore (List.map f constraints) ;
    ()

  (* weight is a string *)
  let add_soft
      ( x : optimize ) ( constraints : expr list )
      ( weight : string ) =
    let f e = (
      Z3native.optimize_assert_soft
        (z3obj_gnc x) (z3obj_gno x) (Expr.gno e) weight
    ) in
    ignore (List.map f constraints) ;
    ()

end
