(* The documentation is in the mli file *)

type t = int ref

let make start = ref (start - 1)

let get (ir : t) =
  incr ir;
  !ir

let read ir = !ir

let skip = incr
