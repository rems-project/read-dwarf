(* This file is about interacting with isla *)

open Protect
open Unix

(* TODO make that changable from CLI or environment *)
let isla = ref "isla-footprint"

(** Call the `which` shell command to get a executable position from its name *)
let which arg = input_line @@ open_process_in @@ "which " ^ arg

(** Parse a single Isla instruction output from a channel *)
let parse_one_instr (c : in_channel) =
  let t = Isla_lang.Parser.term_start Isla_lang.Lexer.token @@ Lexing.from_channel c in
  PP.(println @@ Isla_lang.PP.pp_term t)

(** Thrown when Isla program did not return with code 0 *)
exception Isla_Crashed

(* args.(0) must be empty *)
let isla_cmd args =
  (* let _ = PP.(println @@ array qstring args) in *)
  let isla = which !isla in
  args.(0) <- isla;
  let isla_output = open_process_args_in isla args in
  let closing () =
    match close_process_in isla_output with
    | WEXITED code when code = 0 -> ()
    | code ->
        PP.(println @@ statusi code);
        raise Isla_Crashed
  in
  protect (fun () -> parse_one_instr isla_output) closing
