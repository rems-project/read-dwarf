(** This module is for extending the
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html}[Array]}
    module of the standard library *)

include Stdlib.Array

(** Mutate the array by replacing each value [x] by [f x] *)
let map_mut f arr =
  let len = length arr in
  for i = 0 to len - 1 do
    unsafe_get arr i |> f |> unsafe_set arr i
  done

(** [of_list_mapi f l = of_list (List.mapi f i l) = mapi f (of_list l)] *)
let of_list_mapi f = function
  | [] -> [||]
  | a :: t as l ->
      let first = f 0 a in
      let res = make (Stdlib.List.length l) first in
      let rec fill_from i = function
        | [] -> ()
        | a :: l ->
            unsafe_set res i (f i a);
            fill_from (i + 1) l
      in
      fill_from 1 t;
      res

(** [of_list_map f l = of_list (List.map f l) = map f (of_list l)] *)
let of_list_map f = of_list_mapi (Fun.const f)
