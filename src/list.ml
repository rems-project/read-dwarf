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

(* This is defined in OCaml 4.10, TODO find a clean way of doing conditional compilation,
   otherwise this will shadow the official find_map in 4.10*)
let rec find_map f = function
  | [] -> None
  | x :: l -> (
      match f x with Some _ as result -> result | None -> find_map f l
    )

(** Takes a list of option and only keeps the [Some]. Equivalent to [filter_map Fun.id] *)
let rec filter_opt = function
  | [] -> []
  | Some a :: l -> a :: filter_opt l
  | None :: l -> filter_opt l

(** This function behaves as [partition] then a [map]. First we do a [partition]
    with the function assuming [Some] means [true], then for all the extracted
    elements we map [f] on them and return the results.

    Formally: [partition_map f l = (filter (fun a -> f a = None) l, filter_map f l)] *)
let rec partition_map (f : 'a -> 'b option) : 'a list -> 'a list * 'b list = function
  | [] -> ([], [])
  | a :: l -> (
      let (main, newl) = partition_map f l in
      match f a with Some b -> (main, b :: newl) | None -> (a :: main, newl)
    )

(** Return [None] if the list is empty, and [Some list] if the list is not empty *)
let none_if_empty = function [] -> None | l -> Some l

(** Monadic bind. It's just {!concat_map} *)
let bind l f = concat_map f l

(** Remove the first element matching the predicate.
    Returns [None] if no element matches the predicate *)
let rec remove f = function
  | [] -> None
  | a :: l when f a -> Some l
  | a :: l -> remove f l |> Stdlib.Option.map (List.cons a)

(** Drop the specified number of item from the list.
    If n is greater than the size of the list, then return the empty list *)
let rec drop n l =
  assert (n >= 0);
  match (n, l) with (0, _) -> l | (_, []) -> [] | (_, _ :: t) -> drop (n - 1) t

(** Take the specified number of items from the list, but reverse
    If n is greater than the size of the list, then return the list

    Tail-recursive

*)
let take_rev n l =
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
let take n l = take_rev n l |> rev

(** [sub l pos len] return the sub-list of l starting at pos of length len *)
let sub ~pos ~len l = take len (drop pos l)

(** Same as [combine] but if a list is shorter then the element of the longest
    list are discarded *)
let rec short_combine l1 l2 =
  match (l1, l2) with (a1 :: t1, a2 :: t2) -> (a1, a2) :: short_combine t1 t2 | _ -> []

(** Same as [merge] but duplicate elements are deleted. It is assumed that element
    are not duplicate in the argument (like if sorted with [sort_uniq] *)
let rec merge_uniq cmp l1 l2 =
  match (l1, l2) with
  | ([], _) -> l2
  | (_, []) -> l1
  | (a1 :: t1, a2 :: t2) -> (
      match cmp a1 a2 with
      | 0 -> a1 :: merge_uniq cmp t1 t2
      | x when x < 0 -> a1 :: merge_uniq cmp t1 l2
      | _ -> a2 :: merge_uniq cmp l1 t2
    )

let of_seq_rev s =
  let rec aux acc s = match s () with Seq.Nil -> acc | Seq.Cons (a, s) -> aux (a :: acc) s in
  aux [] s

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Equality and comparison } *)

let rec equal eq l1 l2 =
  match (l1, l2) with
  | ([], []) -> true
  | (a1 :: t1, a2 :: t2) when eq a1 a2 -> equal eq t1 t2
  | _ -> false

(** If the list have the same length this a lexicographic compare.
    If one list is shorter, then the missing value a considered smaller than any actual values.

    A mental model could to view list as infinite sequence of options that are [None] after the end of the list.
    Then it would be proper lexicographic ordering with [Option.compare]
*)
let rec compare cmp l1 l2 =
  match (l1, l2) with
  | ([], []) -> 0
  | (a1 :: t1, a2 :: t2) ->
      let c = cmp a1 a2 in
      if c <> 0 then c else compare cmp t1 t2
  | (_, []) -> 1
  | ([], _) -> -1

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 List monad } *)

(** Applicative let binding. [let+ x = xl in e = let* x = xl in e] *)
let ( let+ ) o f = map f o

(** Not strict applicative merge ({!short_combine}) *)
let ( and+ ) = short_combine

(** Iterative let binding (The expression in the in must be unit). This replace
    implicitly a unit member of the monad that is assumed to be uninteresting to a true unit.
    In other words, It's an [iter]
*)
let ( let+! ) o f = iter f o

(** Strict applicative merge ([combine]). Will throw if list have different length *)
let ( and+! ) = combine

(** Monadic let binding *)
let ( let* ) o b = bind o b

(** Do the Cartesian product of the two list *)
let prod l1 l2 =
  let* x1 = l1 in
  let+ x2 = l2 in
  (x1, x2)

(** Monadic merge. [let* x = xl and* y = yl in ... = let* x= xl in let* y = yl in ...] *)
let ( and* ) = prod
