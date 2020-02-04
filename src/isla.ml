(* This file is about interacting with isla *)

(*****************************************************************************)
(*        Aliases                                                            *)
(*****************************************************************************)

(* direct aliases *)

include Isla_lang.AST
module Lexer = Isla_lang.Lexer
module Parser = Isla_lang.Parser

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

type 'a parser = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a

(** Parse a single Isla instruction output from a Lexing.lexbuf *)
let parse (parser : 'a parser) (filename : string) (l : Lexing.lexbuf) : 'a =
  l.lex_curr_p <- { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  try parser Lexer.token @@ l with
  | Parser.Error -> PP.(fatal @@ loc l.lex_start_p ^^ !^": Syntax error")
  | Lexer.Error s -> PP.(fatal @@ loc l.lex_curr_p ^^ !^": Unexpected character")

(** Parse a single Isla expression from a Lexing.lexbuf *)
let parse_exp : string -> Lexing.lexbuf -> rexp = parse Parser.exp_start

(** Parse a single Isla expression from a string *)
let parse_exp_string (filename : string) (s : string) : rexp =
  parse_exp filename @@ Lexing.from_string ~with_positions:true s

(** Parse a single Isla expression from a channel *)
let parse_exp_channel (filename : string) (c : in_channel) : rexp =
  parse_exp filename @@ Lexing.from_channel ~with_positions:true c

(** Parse a single Isla instruction output from a Lexing.lexbuf *)
let parse_term : string -> Lexing.lexbuf -> rterm = parse Parser.term_start

(** Parse a single Isla instruction output from a string *)
let parse_term_string (filename : string) (s : string) : rterm =
  parse_term filename @@ Lexing.from_string ~with_positions:true s

(** Parse a single Isla instruction output from a channel *)
let parse_term_channel (filename : string) (c : in_channel) : rterm =
  parse_term filename @@ Lexing.from_channel ~with_positions:true c

let _ =
  Tests.add_test "Isla.parse.exp.var.state" (fun () ->
      let s = "|test|" in
      let exp = parse_exp_string "test string in Isla.parse.exp.var.state" s in
      match exp with Var (State "test", _) -> true | _ -> false)

let _ =
  Tests.add_test "Isla.parse.exp.var.free" (fun () ->
      let s = "v42" in
      let exp = parse_exp_string "test string in Isla.parse.exp.var.free" s in
      match exp with Var (Free 42, _) -> true | _ -> false)

let _ =
  Tests.add_test "Isla.parse.exp.and" (fun () ->
      let s = "(and v1 v2)" in
      let exp = parse_exp_string "test string in Isla.parse.exp.and" s in
      match exp with Binop (And, Var (Free 1, _), Var (Free 2, _), _) -> true | _ -> false)

(*****************************************************************************)
(*        Isla Calling                                                       *)
(*****************************************************************************)

(* TODO maybe move that into another file *)

(* TODO make that changable from CLI or environment *)
let isla = ref "isla-footprint"

(* args.(0) must be empty *)
(* cont will be called on the channel. It may return something *)
let isla_cmd args cont = Cmd.input_exec !isla args cont
