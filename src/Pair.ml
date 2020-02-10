(** This module contain random utility dealing with pairs *)

(** Map each function on one side of the pair *)
let map f1 f2 (a, b) = (f1 a, f2 b)

(** Iter each function on one side of the pair *)
let iter f1 f2 (a, b) =
  f1 a;
  f2 b;
  ()

(** Swap the element of a pair *)
let swap (x, y) = (y, x)

(** Apply a two argument function on a pair *)
let apply f (x, y) = f x y
