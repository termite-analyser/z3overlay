
module ZZ3 = ZZ3.Make (struct let ctx = Z3.mk_context [] end)
open ZZ3

let _ =

  let solver = Optimize.make () in

  let x = Symbol.declare Real "x" in
  let y = Symbol.declare Real "y" in

  let t = T.( !y <= int 3 && !x + !y <= rat Q.(5 // 2)) in
  let t' = T.( !y >= int 4 && !y <= int 5 && !x<= int 2 ) in

  let optim = T.( int 2 * !y + !x ) in

  Optimize.add ~solver T.( t || t') ;

  let o = Z3.Optimize.maximize solver (T.raw optim) in

  let result = Optimize.check ~solver  in

  let model = match result with
      | None -> failwith "truc"
      | Some model -> model
  in
  let vy = Model.get_value ~model y in
  let vx = Model.get_value ~model x in

  let ox = Z3.Arithmetic.Real.to_decimal_string (Z3.Optimize.get_upper solver o) 4 in
  Printf.printf "y = %s \nx = %s\nopt = %s" (Q.to_string vy) (Q.to_string vx) ox ;
