(** Miscellaneous types and utility functions used throughout the analyse code *)

open Logs.Logger (struct
  let str = __MODULE__
end)

(** TODO: Maybe just use Z.t everywhere (it's shorter) *)
type natural = Nat_big_num.num

(** machine address *)
type addr = natural

let pp_addr (a : natural) = Ml_bindings.hex_string_of_big_int_pad8 a

(** index into instruction-indexed arrays *)
type index = int

(** TODO move that to {!Config}? *)
let measure_time = false

(** Print the time this function call took. The string is just for the printed message *)
let time s f x =
  if measure_time then (
    let t1 : Unix.process_times = Unix.times () in
    let y = f x in
    let t2 : Unix.process_times = Unix.times () in
    Printf.printf "time %s user %6.3f  system %6.3f\n" s (t2.tms_utime -. t1.tms_utime)
      (t2.tms_stime -. t1.tms_stime);
    y
  )
  else f x

(** Read all lines of a file *)
let read_file_lines (name : string) : (string array, string) result =
  match Files.(read (input_array input_line)) name with
  | lines -> Ok lines
  | exception Sys_error s -> Error (Printf.sprintf "read_file_lines Sys_error \"%s\"\n" s)

(** escape HTML *)
let html_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp"
      | '<' -> Buffer.add_string buf "&lt"
      | '>' -> Buffer.add_string buf "&gt"
      | '\"' -> Buffer.add_string buf "&quot"
      | '\'' -> Buffer.add_string buf "&apos"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let esc m s = match m with AnalyseTypes.Ascii -> s | AnalyseTypes.Html -> html_escape s

let sys_command s =
  if !AnalyseGlobals.copy_sources_dry_run then Printf.printf "dry run: %s\n" s
  else
    let exit_code = Sys.command s in
    if exit_code <> 0 then fatal "sys_command %s failed with exit code %d" s exit_code else ()
