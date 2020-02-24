(* Note : fatal0 and fatal2 are removed since now fatal accept any number of arguments.
   Idem for nonfatal
*)

(** Report a fatal error in the Printf style format *)
let fatal fmt =
  Printf.kfprintf
    (fun _ ->
      flush stderr;
      exit 1)
    stderr fmt

(** Report a non-fatal error in the Printf style format *)
let nonfatal fmt = Printf.kfprintf (fun _ -> flush stderr) stderr fmt
