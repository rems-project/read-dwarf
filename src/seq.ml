(** This module is eor extending the [Seq] module of the standard library *)

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

let rec stop_at f (s : 'a Seq.t) () =
  match s () with
  | Nil -> Nil
  | Cons (a, s') when f a -> Nil
  | Cons (a, s') -> Cons (a, stop_at f s')
