(* This module provide try-with-finally kind of exception handling *)

(** This exception it thrown when both function and protector have thrown *)
exception Protect_both of exn * exn

let _ =
  Printexc.(
    register_printer (function
      | Protect_both (e1, e2) -> Some ("Protect_both(" ^ to_string e1 ^ ", " ^ to_string e2 ^ ")")
      | _ -> None))

(** `protect f p` runs f then p even if f throws.

    If one of f or p throw, then that exception is transmitted as is
    If both throw, the pair of exceptions is encapsulated in Protect_both and thrown.
*)
let protect (f : unit -> 'a) (p : unit -> unit) : 'a =
  let resf = try Ok (f ()) with e -> Error e in
  let resp = try Ok (p ()) with e -> Error e in
  match (resf, resp) with
  | (Ok a, Ok ()) -> a
  | (Ok _, Error e) -> raise e
  | (Error e, Ok ()) -> raise e
  | (Error ef, Error ep) -> raise (Protect_both (ef, ep))
