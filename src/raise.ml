(** This module provide convenience facilities to raise exception or other exception management *)

(** Printf like funtion that throws an [Invalid_Argument] with the formated string *)
let inv_arg fmt = Printf.ksprintf invalid_arg fmt

(** Printf like funtion that throws a [Failure] with the formated string *)
let fail fmt = Printf.ksprintf failwith fmt
