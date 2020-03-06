(* Note : fatal0 and fatal2 are removed since now fatal accept any number of arguments.
   Idem for nonfatal
*)

module L = Logs.Logger (struct
  let str = "Warn"
end)

(** Report a fatal error in the Printf style format *)
let fatal fmt = L.fatal fmt

(** Report a non-fatal error in the Printf style format *)
let nonfatal fmt = L.err fmt
