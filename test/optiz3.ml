open Z3
open ZZ3
open ZZ3Optiz3

let foo (x,y) =
  float_of_int x /. float_of_int y

let _ =

  let ctx = mk_context [] in
  let solver = Solver.mk_simple_solver ctx in

  let x = decl_const ~ctx Real "x" in
  let y = decl_const ~ctx Real "y" in
  let z = decl_const ~ctx Real "z" in

  let t = T.( !y <= I 3 && !x + !y <= Q (5,2)) in
  let t' = T.( !z = I 2 * !y + !x ) in

  let optim = T.(!z) in

  add ~ctx ~solver t ;
  add ~ctx ~solver t' ;

  let (result, unbound) = check_opti ~ctx ~solver optim true in

  let model = match result with
      | Unsat _ | Unkown _ -> failwith "truc"
      | Sat (lazy model) -> model
  in
  let vy = get_value ~model y in
  let vx = get_value ~model x in

  Printf.printf "y = %f \nx = %f\n" (foo vy) (foo vx) ;
  if unbound then print_endline "Unbounded"
