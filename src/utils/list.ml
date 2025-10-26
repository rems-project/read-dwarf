(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(** Extension of the
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html}[List]}
    module of the standard library.

    It port forward function of ocaml 4.10 among others.


*)

include Stdlib.List

(** [repeat n a] return a list of [n] [a]s. *)
let rec repeat n a = if n <= 0 then [] else a :: repeat (n - 1) a

(** Set the nth value to the new value and return the modified list *)
let rec set_nth l n v =
  match l with
  | [] -> Raise.inv_arg "List.set_nth: invalid index"
  | _ :: l when n = 0 -> v :: l
  | a :: l -> a :: set_nth l (n - 1) v

(** Give the last element of the list. Raise [Invalid_argument] if the list is empty. *)
let rec last = function
  | [a] -> a
  | _ :: l -> last l
  | [] -> Raise.inv_arg "List.last on empty_list"

(** Give the last element of the list. Return [None] if the list is empty. *)
let rec last_opt = function [a] -> Some a | _ :: l -> last_opt l | [] -> None

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Iterators } *)

(** Same as fold_left, but do not require a start element, instead the function
    start with the first element of the list:
    [fold_left_same f [b1; ...; bn] = f (... (f (f b1 b2) ...) bn].

    It will fail with [Invalid_argument] if the list is empty.

    It can also be written as [fold_left_same f l = fold_left f (hd l) (tl l)] *)
let fold_left_same f l =
  match l with a :: l -> fold_left f a l | [] -> Raise.inv_arg "List.fold_left_same: empty list"

(** Map a function at the same time as we are creating a list from an array

    Warning: The function is mapped from the right to the left (in case it has side-effects).

   [of_array_map f l = of_array (Array.map f l) = map f (of_array l)] *)
let of_array_map f a = Stdlib.Array.fold_right (fun a l -> f a :: l) a []

(** Same as {!of_array_map} but with the index *)
let of_array_mapi f a =
  let l = Stdlib.Array.length a in
  let rec from acc i =
    if i = -1 then [] else from (f i (Stdlib.Array.unsafe_get a i) :: acc) (i - 1)
  in
  from [] (l - 1)

(** Same as {!concat_map} then {!rev} *)
let concat_map_rev f l =
  let rec aux f acc = function
    | [] -> acc
    | x :: l ->
        let xs = f x in
        aux f (rev_append xs acc) l
  in
  aux f [] l

(** Same as [map] then [concat].

    TODO: find a clean way of doing conditional compilation,
    otherwise this will shadow the official
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html#VALconcat_map}[concat_map]}
    in 4.10 *)
let concat_map f l : _ t = rev @@ concat_map_rev f l

(* This is defined in OCaml 4.10, TODO find a clean way of doing conditional compilation,
   otherwise this will shadow the official find_map in 4.10*)

(** [find_map f l] applies [f] to the elements of [l] in order,
    and returns the first result of the form [Some v], or [None] if [f] always return [None]

    TODO: find a clean way of doing conditional compilation,
    otherwise this will shadow the official
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html#VALfind_map}[find_map]}
    in 4.10 *)
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

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Removing elements } *)

(** Remove the first element matching the predicate.
    Returns [None] if no element matches the predicate *)
let rec remove f = function
  | [] -> None
  | a :: l when f a -> Some l
  | a :: l -> remove f l |> Stdlib.Option.map (cons a)

(** Drop the specified number of item from the list.
    If n is greater than the size of the list, then return the empty list *)
let rec drop n l =
  assert (n >= 0);
  match (n, l) with (0, _) -> l | (_, []) -> [] | (_, _ :: t) -> drop (n - 1) t

(** Take the specified number of items from the list, but reverse
    If n is greater than the size of the list, then return the list reversed

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

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Sorted list manipulation } *)

(** Same as
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html#VALmerge}[merge]}
    but duplicate elements are deleted. It is assumed that element
    are not duplicate in the argument (like if sorted with
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html#VALsort_uniq}[sort_uniq]}) *)
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

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Sequences } *)

(** Build a list from a sequence, but in reverse. Same as [of_seq] then [rev] *)
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

let rec mem eq x = function [] -> false | y :: ys -> eq x y || mem eq x ys

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

(** Monadic bind. It's just {!concat_map} *)
let bind l f = concat_map f l

(** Monadic return *)
let return a = [a]

(** Same as [combine] but if a list is shorter then the elements of the longest
    list are discarded *)
let rec short_combine l1 l2 =
  match (l1, l2) with (a1 :: t1, a2 :: t2) -> (a1, a2) :: short_combine t1 t2 | _ -> []

(** Applicative let binding. [let+ x = xl in e = let* x = xl in return e] *)
let ( let+ ) o f = map f o

(** Not strict applicative merge ({!short_combine}).
    If both list have different length, the longer one is cropped *)
let ( and+ ) = short_combine

(** Iterative let binding (The expression in the in must be unit). This replace
    implicitly a unit member of the monad (that is assumed to be uninteresting) to a true unit.
    In other words, it's an [iter]: [let+! x = l in e = List.iter (fun x -> e) l] *)
let ( let+! ) o f = iter f o

(** Strict applicative merge ([combine]). Will throw if lists have different length *)
let ( and+! ) = combine

(** Monadic let binding *)
let ( let* ) o b = bind o b

(** Do the Cartesian product of two lists *)
let prod l1 l2 =
  let* x1 = l1 in
  let+ x2 = l2 in
  (x1, x2)

(** Monadic merge. [let* x = xl and* y = yl in ... = let* x= xl in let* y = yl in ...] *)
let ( and* ) = prod

let hd_opt = function
| [] -> None
| h :: _ -> Some h

let rec transpose ~defaults l =
  let first = map hd_opt l in
  let rest = map (drop 1) l in
  if for_all Stdlib.Option.is_none first then
    []
  else
    let t = transpose ~defaults rest in
    let h = combine first defaults |> map (fun (value, default) -> Stdlib.Option.value ~default value) in
    h :: t
