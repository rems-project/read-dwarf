(* The documentation is in the mli file *)

type 'a t = 'a Weak.t

let empty () : 'a t = Weak.create 1

let seto ptr (opt : 'a option) = Weak.set ptr 0 opt

let set ptr a = seto ptr (Some a)

let reset ptr = seto ptr None

let make x =
  let ptr = empty () in
  set ptr x;
  ptr

let geto ptr = Weak.get ptr 0

exception Deleted

let get ptr = match geto ptr with None -> raise Deleted | Some a -> a

let check ptr = Weak.check ptr 0
