(** This module is for extending the
    {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/Seq.html} Seq}
    module of the standard library *)

include Stdlib.Seq

(** Add a counter starting at [start] (default 0) in front of each element of the sequence *)
let add_count ?(start = 0) (s : 'a t) : (int * 'a) t =
  let rec aux i s () =
    match s () with Nil -> Nil | Cons (v, s') -> Cons ((i, v), aux (i + 1) s')
  in
  aux start s

(** Generate an integer sequence up to [len]. Optionally may start at [start] instead of 0 *)
let iota ?(start = 0) len : int t =
  let rec aux endi cur () = if cur < endi then Cons (cur, aux endi (cur + 1)) else Nil in
  aux (start + len) start

(** Generate an integer sequence up to [endi] by stepping [step].
    Optionally may start at [start] instead of 0 *)
let iota_step_up ?(start = 0) ~step ~endi : int t =
  let rec aux step endi cur () =
    if cur < endi then Cons (cur, aux step endi (cur + step)) else Nil
  in
  aux step endi start

(** Make the sequence stop when the condition is met *)
let rec stop_at f (s : 'a t) () =
  match s () with
  | Nil -> Nil
  | Cons (a, _) when f a -> Nil
  | Cons (a, s') -> Cons (a, stop_at f s')

(** Add a new element in front of the sequence. That element will appear first before
    the rest of the sequence.

    Added to [Stdlib] in Ocaml 4.11 *)
let cons (v : 'a) (s : 'a t) () = Seq.Cons (v, s)

(** Applies the specified function to the elements of the sequence in order,
    and returns the first result of the form [Some v], or [None] if no such result was returned.

    See {{:https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html#VALfind_map} List.find_map}
*)
let rec find_map f seq =
  match seq () with
  | Nil -> None
  | Cons (a, seq) ->
      let v = f a in
      if v = None then find_map f seq else v
