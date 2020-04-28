(** Extension of the list module of the standard library *)

include Stdlib.List

let fold_left_same f l =
  match l with a :: l -> fold_left f a l | [] -> Raise.inv_arg "List.fold_left_same: empty list"
