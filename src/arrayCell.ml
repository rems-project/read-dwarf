(* The documentation is in the mli file *)

type 'a t = { arr : 'a array; pos : int }

let make arr pos = { arr; pos }

let set cell v = cell.arr.(cell.pos) <- v

let get cell = cell.arr.(cell.pos)
