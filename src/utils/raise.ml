(** This module provide convenience facilities to raise exception or other exception management *)

(** Printf like funtion that throws an [Invalid_Argument] with the formated string *)
let inv_arg fmt = Printf.ksprintf invalid_arg fmt

(** Printf like funtion that throws a [Failure] with the formated string *)
let fail fmt = Printf.ksprintf failwith fmt

(** Raise again an exception without losing the backtrace. *)
let again exn = Printexc.raise_with_backtrace exn (Printexc.get_raw_backtrace ())

exception Todo

(** Put that in unfinished places of the code that need to be completed *)
let todo () = raise Todo

exception Unreachable

(** Put that in-place of dead code that is required by the typer *)
let unreachable () = raise Unreachable
