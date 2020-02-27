(* The documentation is in the mli file *)

let map f1 f2 (a, b) = (f1 a, f2 b)

let iter f1 f2 (a, b) =
  f1 a;
  f2 b;
  ()

let swap (x, y) = (y, x)

let compare ?(fst = compare) ?(snd = compare) (a1, b1) (a2, b2) =
  match fst a1 a2 with 0 -> snd b1 b2 | x when x < 0 -> -1 | _ -> 1
