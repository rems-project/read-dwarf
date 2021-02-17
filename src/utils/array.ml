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

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Array scanning } *)

(** Find the first value satisfying the predicate and return it with its index.
    Throw [Not_found] if no value satisfies the predicate *)
let find_pair (pred : 'a -> bool) (arr : 'a array) : int * 'a =
  let len = length arr in
  let rec from n =
    if n >= len then raise Not_found
    else
      let v = unsafe_get arr n in
      if pred v then (n, v) else from (n + 1)
  in
  from 0

(** Find the first value satisfying the predicate.
    Throw [Not_found] if no value satisfies the predicate *)
let find pred arr = find_pair pred arr |> snd

(** Find the first index whose value satisfies the predicate.
    Throw [Not_found] if no value satisfies the predicate *)
let find_index pred arr = find_pair pred arr |> fst

(** Find all the values satisfying the predicate and return them with their index.*)
let find_all_pairs (pred : 'a -> bool) (arr : 'a t) : (int * 'a) list =
  let len = length arr in
  let res = ref [] in
  for i = len - 1 downto 0 do
    let v = unsafe_get arr i in
    if pred v then res := (i, v) :: !res
  done;
  !res

(** Find all the values satisfying the predicate*)
let find_all pred (arr : 'a t) : 'a list = find_all_pairs pred arr |> List.map snd

(** Find all the indices whose value satisfies the predicate*)
let find_all_indices pred (arr : 'a t) : int list = find_all_pairs pred arr |> List.map fst
