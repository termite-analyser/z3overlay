open Z3

module ZZ3 = ZZ3.Make (struct let ctx = mk_context [] end)
open ZZ3


let _ =

  let solver = Solver.make () in

  let x = Symbol.declare Real "x" in
  let y = Symbol.declare Real "y" in

  let t = T.( !y <= int 3 && !x + !y <= rat Q.(5 // 2)) in

  Solver.add ~solver t ;

  let result = Solver.check ~solver [] in

  let model = match result with
      | Unsat _ | Unkown _ -> failwith "Oh noees"
      | Sat (lazy model) -> model
  in
  let vy = Model.get_value ~model y in
  let vx = Model.get_value ~model x in

  Printf.printf "y = %s \nx = %s\n" (Q.to_string vy) (Q.to_string vx)
