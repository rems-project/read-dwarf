(* This file is about interacting with isla *)

(*****************************************************************************)
(*        Aliases                                                            *)
(*****************************************************************************)

(* direct aliases *)

include Isla_lang.AST
module Lexer = Isla_lang.Lexer
module Parser = Isla_lang.Parser

(** {!Bimap.t} test*)
type loc = Lexing.position

(* location aliases *)

type 'v ltrc = ('v, lrng) trc

type 'v levent = ('v, lrng) event

type 'v lexp = ('v, lrng) exp

(* string aliases *)

type svar = string var

type 'a strc = (string, 'a) trc

type 'a sevent = (string, 'a) event

type 'a sexp = (string, 'a) exp

(* raw aliases : parser output *)

type rvar = string var

type rtrc = (string, lrng) trc

type revent = (string, lrng) event

type rexp = (string, lrng) exp

(*****************************************************************************)
(*        Isla parsing                                                       *)
(*****************************************************************************)

(** Exception that represent an Isla parsing error *)
exception ParseError of loc * string

(* Registering and pretty printer for that exception *)
let _ =
  Printexc.register_printer (function
    | ParseError (l, s) ->
        Some PP.(sprint @@ prefix 2 1 (loc l ^^ !^": ") (!^"ParseError: " ^^ !^s))
    | _ -> None)

(** Exception that represent an Isla lexing error *)
exception LexError of loc * string

(* Registering and pretty printer for that exception *)
let _ =
  Printexc.register_printer (function
    | LexError (l, s) -> Some PP.(sprint @@ prefix 2 1 (loc l ^^ !^": ") (!^"LexError: " ^^ !^s))
    | _ -> None)

type lexer = Lexing.lexbuf -> Parser.token

type 'a parser = lexer -> Lexing.lexbuf -> 'a

(** Parse a single Isla instruction output from a Lexing.lexbuf *)
let parse (parser : 'a parser) ?(filename = "default") (l : Lexing.lexbuf) : 'a =
  l.lex_curr_p <- { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  try parser Lexer.token @@ l with
  | Parser.Error -> raise (ParseError (l.lex_start_p, "Syntax error"))
  | Lexer.Error s -> raise (LexError (l.lex_start_p, "Unexpected character"))

(** Parse a single Isla expression from a Lexing.lexbuf *)
let parse_exp : ?filename:string -> Lexing.lexbuf -> rexp = parse Parser.exp_start

(** Parse a single Isla expression from a string *)
let parse_exp_string ?(filename = "default") (s : string) : rexp =
  parse_exp ~filename @@ Lexing.from_string ~with_positions:true s

(** Parse a single Isla expression from a channel *)
let parse_exp_channel ?(filename = "default") (c : in_channel) : rexp =
  parse_exp ~filename @@ Lexing.from_channel ~with_positions:true c

(** Parse an Isla trace from a Lexing.lexbuf *)
let parse_trc : ?filename:string -> Lexing.lexbuf -> rtrc = parse Parser.trc_start

(** Parse an Isla trace from a string *)
let parse_trc_string ?(filename = "default") (s : string) : rtrc =
  parse_trc ~filename @@ Lexing.from_string ~with_positions:true s

(** Parse an Isla trace from a channel *)
let parse_trc_channel ?(filename = "default") (c : in_channel) : rtrc =
  parse_trc ~filename @@ Lexing.from_channel ~with_positions:true c

let _ =
  Tests.add_test "Isla.parse.exp.var.state" (fun () ->
      let s = "|test|" in
      let exp = parse_exp_string ~filename:"test string in Isla.parse.exp.var.state" s in
      match exp with Var (State "test", _) -> true | _ -> false)

let _ =
  Tests.add_test "Isla.parse.exp.var.free" (fun () ->
      let s = "v42" in
      let exp = parse_exp_string ~filename:"test string in Isla.parse.exp.var.free" s in
      match exp with Var (Free 42, _) -> true | _ -> false)

let _ =
  Tests.add_test "Isla.parse.exp.and" (fun () ->
      let s = "(and v1 v2)" in
      let exp = parse_exp_string ~filename:"test string in Isla.parse.exp.and" s in
      match exp with Manyop (And, [Var (Free 1, _); Var (Free 2, _)], _) -> true | _ -> false)
