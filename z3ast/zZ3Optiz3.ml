open Z3
open ZZ3


let bounded = function
  | Z3enums.L_TRUE -> true
  | _ -> false

let check_opti ~ctx ~solver criteria b =
  let open Z3.Solver in
  let (v, b) = check_opti solver (to_expr ~ctx criteria) b in
  let v = match v with
    | UNSATISFIABLE -> Unsat (lazy (opt_get @@ get_proof solver))
    | UNKNOWN -> Unkown (get_reason_unknown solver)
    | SATISFIABLE -> Sat (lazy (opt_get @@ get_model solver))
  in (v,bounded b)
