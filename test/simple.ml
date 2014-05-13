open Z3
open ZZ3

let _ =

  let ctx = mk_context [] in
  let solver = Solver.mk_simple_solver ctx in

  let x = decl_const ~ctx Real "x" in
  let y = decl_const ~ctx Real "y" in

  let t = T.( !y <= I Z.(~$ 3) && !x + !y <= Q Q.(5 // 2)) in

  add ~ctx ~solver t ;

  let result = check ~ctx ~solver [] in

  let model = match result with
      | Unsat _ | Unkown _ -> failwith "truc"
      | Sat (lazy model) -> model
  in
  let vy = get_value ~model y in
  let vx = get_value ~model x in

  Printf.printf "y = %s \nx = %s\n" (Q.to_string vy) (Q.to_string vx)
