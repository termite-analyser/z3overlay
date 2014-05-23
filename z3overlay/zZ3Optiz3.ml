open Utils
open Z3

module Make (M : ZZ3_sigs.S) = struct

  open M

  let bounded = function
    | Z3enums.L_TRUE -> true
    | _ -> false

  let check ~solver (criteria : M.znum M.t) b =
    let open Z3.Solver in
    let (v, b) = check_opti solver (criteria :> Expr.expr) b in
    let v = match v with
      | UNSATISFIABLE -> Unsat (lazy (opt_get @@ get_proof solver))
      | UNKNOWN -> Unkown (get_reason_unknown solver)
      | SATISFIABLE -> Sat (lazy (opt_get @@ get_model solver))
    in (v,bounded b)

end
