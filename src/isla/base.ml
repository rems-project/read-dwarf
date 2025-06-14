(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Copyright (c) 2020-2021 Thibaut Pérami                                          *)
(*  Copyright (c) 2020-2021 Dhruv Makwana                                           *)
(*  Copyright (c) 2019-2021 Peter Sewell                                            *)
(*  All rights reserved.                                                            *)
(*                                                                                  *)
(*  This software was developed by the University of Cambridge Computer             *)
(*  Laboratory as part of the Rigorous Engineering of Mainstream Systems            *)
(*  (REMS) project.                                                                 *)
(*                                                                                  *)
(*  This project has been partly funded by EPSRC grant EP/K008528/1.                *)
(*  This project has received funding from the European Research Council            *)
(*  (ERC) under the European Union's Horizon 2020 research and innovation           *)
(*  programme (grant agreement No 789108, ERC Advanced Grant ELVER).                *)
(*  This project has been partly funded by an EPSRC Doctoral Training studentship.  *)
(*  This project has been partly funded by Google.                                  *)
(*                                                                                  *)
(*  Redistribution and use in source and binary forms, with or without              *)
(*  modification, are permitted provided that the following conditions              *)
(*  are met:                                                                        *)
(*  1. Redistributions of source code must retain the above copyright               *)
(*     notice, this list of conditions and the following disclaimer.                *)
(*  2. Redistributions in binary form must reproduce the above copyright            *)
(*     notice, this list of conditions and the following disclaimer in              *)
(*     the documentation and/or other materials provided with the                   *)
(*     distribution.                                                                *)
(*                                                                                  *)
(*  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''              *)
(*  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED               *)
(*  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A                 *)
(*  PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR             *)
(*  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,                    *)
(*  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT                *)
(*  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF                *)
(*  USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *)
(*  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,              *)
(*  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT              *)
(*  OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF              *)
(*  SUCH DAMAGE.                                                                    *)
(*                                                                                  *)
(*==================================================================================*)

(** This module wraps all [isla-lang] functionality. No other module should
    directly touch the [Isla_lang] module.

    The isla trace syntax is compose of {{!event}events} that a regrouped into
    {{!trc}traces}. There are a lot of possible events, and some of them are
    unsupported. in particular all the ones that deal with concurrency.

    All the variable in isla traces are numbered. They are named [v28] in the
    syntax and are represented as a plain [int] in Ocaml. The structure of the
    traces is a mix of SMT declaration about those variable and actual processor
    events. SMT declaration use SMT expressions of type {!exp} which entirely
    distinct from {!Ast.exp} (Use {!Isla.Conv} to convert). On the contrary
    processor event do not contain direct SMT expressions but Sail values of type
    {!valu}. A {!valu} can be either a single symbolic variable, a concrete
    bitvector/boolean/enumeration value or a more complex sail structure with
    fields that contain other {!valu}s and some other things.

    Event of reading and writing complex register like [PSTATE] in [AArch64]
    will provide the whole structure as a valu to be read or written. However,
    Those events may contain a accessor list that implies that only specific
    field of the struct are written or read. This useful, because of the flat
    representation of registers in {!State.Reg}: each field is considered to be a
    separate register.

    The isla types are polymorphic over the annotation because that comes from
    [isla-lang], but in practice the only annotation I use the source position of
    type {!lrng}, that's why there is a set of aliases starting with [r].*)

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Aliases } *)

include Isla_lang.AST
module Lexer = Isla_lang.Lexer
module Parser = Isla_lang.Parser

type loc = Lexing.position

(** The type of raw traces out of the parser *)
type rtrc = lrng trc

(** The type of raw events out of the parser *)
type revent = lrng event

(** The type of raw SMT declaration out of the parser *)
type rsmt = lrng smt

(** The type of raw expressions out of the parser *)
type rexp = lrng exp

type rtrcs = lrng trcs

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 IslaTrace parsing } *)

(** Exception that represent an Isla parsing error *)
exception ParseError of loc * string

(* Registering a pretty printer for that exception *)
let _ =
  Printexc.register_printer (function
    | ParseError (l, s) ->
        Some Pp.(sprint @@ prefix 2 1 (Ast.loc l ^^ !^": ") (!^"ParseError: " ^^ !^s))
    | _ -> None)

(** Exception that represent an Isla lexing error *)
exception LexError of loc * string

(* Registering a pretty printer for that exception *)
let _ =
  Printexc.register_printer (function
    | LexError (l, s) ->
        Some Pp.(sprint @@ prefix 2 1 (Ast.loc l ^^ !^": ") (!^"LexError: " ^^ !^s))
    | _ -> None)

type lexer = Lexing.lexbuf -> Parser.token

type 'a parser = lexer -> Lexing.lexbuf -> 'a

(** Parse a single Isla instruction output from a Lexing.lexbuf *)
let parse (parser : 'a parser) ?(filename = "default") (l : Lexing.lexbuf) : 'a =
  l.lex_curr_p <- { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  try parser Lexer.token @@ l with
  | Parser.Error -> raise (ParseError (l.lex_start_p, "Syntax error"))
  | Lexer.Error _ -> raise (LexError (l.lex_start_p, "Unexpected character"))

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
  let print_around n =
    if has_debug () then
      let lines =
        s |> String.split_on_char '\n'
        |> List.sub ~pos:(max 0 (n - 3)) ~len:5
        |> String.concat "\n  "
      in
      debug "Error at lines:\n  %s" lines
  in

  let lexbuf = Lexing.from_string ~with_positions:true s in
  lexbuf.lex_curr_p <- { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  try Parser.trc_start Lexer.token lexbuf with
  | Parser.Error ->
      print_around lexbuf.lex_start_p.pos_lnum;
      raise (ParseError (lexbuf.lex_start_p, "Syntax error"))
  | Lexer.Error _ ->
      print_around lexbuf.lex_start_p.pos_lnum;
      raise (LexError (lexbuf.lex_start_p, "Unexpected token"))

(** Parse an Isla trace from a channel *)
let parse_trc_channel ?(filename = "default") (c : in_channel) : rtrc =
  parse_trc ~filename @@ Lexing.from_channel ~with_positions:true c

let parse_trcs = parse Parser.trcs_start

let parse_trcs_string ?filename (s : string) : rtrcs =
  parse_trcs ?filename @@ Lexing.from_string ~with_positions:true s

let parse_trcs_channel ?filename (c : in_channel) : rtrcs =
  parse_trcs ?filename @@ Lexing.from_channel ~with_positions:true c

let parse_segments ?filename l = match parse_trcs ?filename l with
| TracesWithSegments (s, []) -> s
| _ -> raise (ParseError (l.lex_start_p, "Data is not SEGMENTS"))

let parse_segments_string ?filename (s : string) : instruction_segments =
  parse_segments ?filename @@ Lexing.from_string ~with_positions:true s

let parse_segments_channel ?filename (c : in_channel) : instruction_segments =
  parse_segments ?filename @@ Lexing.from_channel ~with_positions:true c

(*$R
    try
      let exp = parse_exp_string ~filename:"test" "v42" in
      match exp with Var (42, _) -> () | _ -> assert_failure "Wrong expression parsed"
    with
    | exn -> assert_failure (Printf.sprintf "Thrown: %s" (Printexc.to_string exn))
*)

(*$R
    try
      let exp = parse_exp_string ~filename:"test" "(and v1 v2)" in
      match exp with
        | Manyop (And, [Var (1, _); Var (2, _)], _)  -> ()
        | _ -> assert_failure "Wrong expression parsed"
    with
    | exn -> assert_failure (Printf.sprintf "Thrown: %s" (Printexc.to_string exn))
*)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 IslaTrace pretty printing } *)

include Isla_lang.PP
