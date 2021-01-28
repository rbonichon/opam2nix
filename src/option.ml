include Stdlib.Option  

let tap fn = function
  | None -> None
  | Some x ->
      fn x;
      Some x

let filter p = function
  | None -> None
  | Some x -> if p x then Some x else None

let may fn = function None -> () | Some x -> fn x

let default_fn d v = match v with Some v -> v | None -> d ()

let or_else alt v = match v with Some _ -> v | None -> alt

let or_else_fn alt v = match v with Some _ -> v | None -> alt ()

let or_failwith msg v = match v with Some v -> v | None -> failwith msg

let to_string fn = function None -> "None" | Some x -> "Some(" ^ fn x ^ ")"

let sequence_result = function
  | None -> Ok None
  | Some (Error e) -> Error e
  | Some (Ok x) -> Ok (Some x)
