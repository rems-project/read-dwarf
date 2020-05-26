(* The documentation is in the mli file *)

type 'a t = { arr : 'a array; pos : int }

let make arr pos =
  let len = Array.length arr in
  if not (pos < len && pos >= 0) then
    Raise.inv_arg "Array cell at pos %d but length is %d" pos len;
  { arr; pos }

let set cell v = cell.arr.(cell.pos) <- v

let get cell = cell.arr.(cell.pos)
