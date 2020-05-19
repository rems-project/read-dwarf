(** Extension of the list module of the standard library *)

include Stdlib.List

let fold_left_same f l =
  match l with a :: l -> fold_left f a l | [] -> Raise.inv_arg "List.fold_left_same: empty list"

(** [of_list_map f l = of_array (Array.map f l) = map f (of_array l)] *)
let of_array_map f a = Stdlib.Array.fold_right (fun a l -> f a :: l) a []

let of_array_mapi f a =
  let l = Stdlib.Array.length a in
  let rec from acc i =
    if i = -1 then [] else from (f i (Stdlib.Array.unsafe_get a i) :: acc) (i - 1)
  in
  from [] (l - 1)

let concat_map_rev f l =
  let rec aux f acc = function
    | [] -> acc
    | x :: l ->
        let xs = f x in
        aux f (rev_append xs acc) l
  in
  aux f [] l

(* This is defined in OCaml 4.10, TODO find a clean way of doing conditional compilation,
   otherwise this will shadow the official concat_map in 4.10*)
let concat_map f l = rev @@ concat_map_rev f l

(** Drop the specified number of item from the list.
    If n is greater than the size of the list, then return the empty list *)
let rec drop n l =
  assert (n >= 0);
  match (n, l) with (0, _) -> l | (_, []) -> [] | (_, a :: t) -> drop (n - 1) t

(** Take the specified number of items from the list, but reverse
    If n is greater than the size of the list, then return the list

    Tail-recursive

*)
let rec take_rev n l =
  assert (n >= 0);
  let rec take_rev_acc acc n l =
    match (n, l) with
    | (0, _) -> acc
    | (_, []) -> acc
    | (_, a :: t) -> take_rev_acc (a :: acc) (n - 1) t
  in
  take_rev_acc [] n l

(** Take the specified number of items from the list.
    If n is greater than the size of the list, then return the list

    [l = take n l @ drop n l]
*)
let rec take n l = take_rev n l |> rev

(** [sub l pos len] return the sub-list of l starting at pos of length len *)
let rec sub ~pos ~len l = take len (drop pos l)
