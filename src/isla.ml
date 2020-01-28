(* This file is about interacting with isla *)

open Protect
open Unix

(* TODO make that changable from CLI or environment *)
let isla = ref "isla-footprint"

(** Call the `which` shell command to get a executable position from its name *)
let which arg = input_line @@ open_process_in @@ "which " ^ arg

(** Parse a single Isla instruction output from a Lexing.lexbuf *)
let parse_term (filename : string) (l : Lexing.lexbuf) : Isla_lang.term =
  l.lex_curr_p <- { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  try Isla_lang.Parser.term_start Isla_lang.Lexer.token @@ l with
  | Isla_lang.Parser.Error -> PP.(fatal @@ Isla_lang.pp_lpos l.lex_start_p ^^ !^": Syntax error")
  | Isla_lang.Lexer.Error _ ->
      PP.(fatal @@ Isla_lang.pp_lpos l.lex_start_p ^^ !^": Unexpected character")

(** Parse a single Isla instruction output from a string *)
let parse_term_string (filename : string) (s : string) : Isla_lang.term =
  parse_term filename @@ Lexing.from_string ~with_positions:true s

(** Parse a single Isla instruction output from a channel *)
let parse_term_channel (filename : string) (c : in_channel) : Isla_lang.term =
  parse_term filename @@ Lexing.from_channel ~with_positions:true c

(** Thrown when Isla program did not return with code 0 *)
exception Isla_Crashed

(* args.(0) must be empty *)
(* cont will be called on the channel. It may return something *)
let isla_cmd (cont : in_channel -> 'a) (args : string array) : 'a =
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
  protect (fun () -> cont isla_output) closing
