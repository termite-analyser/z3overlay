
let opt_get = function
  | None -> raise @@ Z3.Error "opt_get"
  | Some x -> x
