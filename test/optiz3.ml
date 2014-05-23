open Z3


module ZZ3 = ZZ3.Make (struct let ctx = mk_context [] end)
module Opti = ZZ3Optiz3.Make (ZZ3)
open ZZ3

let _ =

  let solver = Solver.make () in

  let x = Symbol.declare Real "x" in
  let y = Symbol.declare Real "y" in
  let z = Symbol.declare Real "z" in

  let t = T.( !y <= int Z.(~$ 3) && !x + !y <= rat Q.(5 // 2)) in
  let t' = T.( !z = int Z.(~$ 2) * !y + !x ) in

  let optim = T.symbol z in

  Solver.add ~solver t ;
  Solver.add ~solver t' ;

  let (result, unbound) = Opti.check ~solver optim true in

  let model = match result with
      | Unsat _ | Unkown _ -> failwith "truc"
      | Sat (lazy model) -> model
  in
  let vy = Model.get_value ~model y in
  let vx = Model.get_value ~model x in

  Printf.printf "y = %s \nx = %s\n" (Q.to_string vy) (Q.to_string vx) ;
  if unbound then print_endline "Unbounded"
