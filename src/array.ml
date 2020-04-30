(** This module is for extending the [Array] module of the standard library *)

include Stdlib.Array

let map_mut f arr =
  let len = length arr in
  for i = 0 to len - 1 do
    unsafe_get arr i |> f |> unsafe_set arr i
  done

(** [of_list_map f l = of_list (List.map f l) = map f (of_list l)] *)
let of_list_map f = function
  | [] -> [||]
  | a :: t as l ->
      let first = f a in
      let res = create (Stdlib.List.length l) first in
      let rec fill_from i = function
        | [] -> ()
        | a :: l ->
            unsafe_set res i (f a);
            fill_from (i + 1) l
      in
      fill_from 1 t;
      res