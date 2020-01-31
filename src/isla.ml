(* This file is about interacting with isla *)

open Protect
open Unix

(*****************************************************************************)
(*        Aliases                                                            *)
(*****************************************************************************)

(* direct aliases *)

include Isla_lang.AST

type loc = Lexing.position

(* location aliases *)

type 'v lterm = ('v, lrng) term

type 'v ltrc = ('v, lrng) trc

type 'v levent = ('v, lrng) event

type 'v lexp = ('v, lrng) exp

(* string aliases *)

type svar = string var

type 'a sterm = (string, 'a) term

type 'a strc = (string, 'a) trc

type 'a sevent = (string, 'a) event

type 'a sexp = (string, 'a) exp

(* raw aliases : parser output *)

type rvar = string var

type rterm = (string, lrng) term

type rtrc = (string, lrng) trc

type revent = (string, lrng) event

type rexp = (string, lrng) exp

(*****************************************************************************)
(*        Isla parsing                                                       *)
(*****************************************************************************)

(** Parse a single Isla instruction output from a Lexing.lexbuf *)
let parse_term (filename : string) (l : Lexing.lexbuf) : rterm =
  l.lex_curr_p <- { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  try Isla_lang.Parser.term_start Isla_lang.Lexer.token @@ l with
  | Isla_lang.Parser.Error -> PP.(fatal @@ loc l.lex_start_p ^^ !^": Syntax error")
  | Isla_lang.Lexer.Error _ -> PP.(fatal @@ loc l.lex_start_p ^^ !^": Unexpected character")

(** Parse a single Isla instruction output from a string *)
let parse_term_string (filename : string) (s : string) : rterm =
  parse_term filename @@ Lexing.from_string ~with_positions:true s

(** Parse a single Isla instruction output from a channel *)
let parse_term_channel (filename : string) (c : in_channel) : rterm =
  parse_term filename @@ Lexing.from_channel ~with_positions:true c

(*****************************************************************************)
(*        Isla Calling                                                       *)
(*****************************************************************************)

(* TODO maybe move that into another file *)

(* TODO make that changable from CLI or environment *)
let isla = ref "isla-footprint"

(** Call the `which` shell command to get a executable position from its name *)
let which arg = input_line @@ open_process_in @@ "which " ^ arg

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
