(** The main module to use the AST of expression and SMT operation
    for a more generic overview of the AST, see {!SymbolicExpressions}.*)

include AstGen.Def

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Size } *)

module Size = struct
  include AstGen.Def.Size

  let to_bv : t -> 'a ty = function
    | B8 -> Ty_BitVec 8
    | B16 -> Ty_BitVec 16
    | B32 -> Ty_BitVec 32
    | B64 -> Ty_BitVec 64
end

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Parsing } *)

module Parser = AstGen.Parser
module Lexer = AstGen.Lexer

let unknown = Isla_lang.AST.UnknownRng

let loc = Isla_lang.AST.pp_lpos

(** Exception that represent an Isla parsing error *)
exception ParseError of loc * string

(* Registering a pretty printer for that exception *)
let _ =
  Printexc.register_printer (function
    | ParseError (l, s) ->
        Some Pp.(sprint @@ prefix 2 1 (loc l ^^ !^": ") (!^"ParseError: " ^^ !^s))
    | _ -> None)

(** Exception that represent an Isla lexing error *)
exception LexError of loc * string

(* Registering a pretty printer for that exception *)
let _ =
  Printexc.register_printer (function
    | LexError (l, s) -> Some Pp.(sprint @@ prefix 2 1 (loc l ^^ !^": ") (!^"LexError: " ^^ !^s))
    | _ -> None)

type lexbuf = Lexing.lexbuf

type lexer = lexbuf -> Parser.token

type 'a parser = lexer -> lexbuf -> 'a

(** Parse a single Isla instruction output from a Lexing.lexbuf *)
let parse (parser : 'a parser) (conv : 'b -> lexbuf) ?(filename = "default") (i : 'b) : 'a =
  let l = conv i in
  l.lex_curr_p <- { pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
  try parser Lexer.token @@ l with
  | Parser.Error -> raise (ParseError (l.lex_start_p, "Syntax error"))
  | Lexer.Error _ -> raise (LexError (l.lex_start_p, "Unexpected character"))

let from_string = Lexing.from_string ~with_positions:true

let from_channel = Lexing.from_channel ~with_positions:true

(** Parse a single Isla expression from a string *)
let parse_exp_string = parse Parser.exp_start from_string

(** Parse a single Isla expression from a channel *)
let parse_exp_channel = parse Parser.exp_start from_channel

(** Parse a single Isla expression from a string *)
let parse_smt_ans_string = parse Parser.smt_ans_start from_string

(** Parse a single Isla expression from a channel *)
let parse_smt_ans_channel = parse Parser.smt_ans_start from_channel

(* All the pretty printer have pp_name names, so it corresponds to the convention *)
include AstGen.Parser_pp

(** Prints a lexing location *)
let pp_loc = Isla_lang.AST.pp_lpos

(** Prints a lexing range *)
let pp_lrng = Isla_lang.AST.pp_lrng

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Analysers }

    All function that start with [is_]
*)

let is_atomic = function
  | Var _ -> true
  | Bool _ -> true
  | Bits _ -> true
  | Enum _ -> true
  | Bound _ -> true
  | _ -> false

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1:destr Destructors }

    Aparently I overestimated ocaml type-system in it's handling of empty types.

    Here are some function to destroy empty types.
*)

let destr_binmem : no binmem -> 'a = function Select _ -> . | Store _ -> .

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Expectors }
    Functions to assert that a specific constructor is used and get the value
*)

let expect_bits : _ exp -> BitVec.t = function
  | Bits (bv, _) -> bv
  | _ -> Raise.inv_arg "Expected a constant bitvector expression"

let ty_expect_bv : _ ty -> int = function
  | Ty_BitVec i -> i
  | _ -> Raise.inv_arg "Expected a bitvector type"

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Construtors } *)

let assert_smt e = Assert e

let simplify_smt ?(flags = []) e = Simplify (e, flags)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Comparisons } *)

(** Equality for expression. Polymorphic equality will fail.
    I love boilerplate code and love Ocaml! *)
let rec equal_exp ?(annot = fun _ _ -> true) ~var ?(bnd = fun _ _ -> true) e e' =
  let eqe = equal_exp ~annot ~var ~bnd in
  match (e, e') with
  | (Var (v, a), Var (v', a')) -> var v v' && annot a a'
  | (Bound (b, a), Bound (b', a')) -> bnd b b' && annot a a'
  | (Bits (bv, a), Bits (bv', a')) -> bv = bv' && annot a a'
  | (Bool (b, a), Bool (b', a')) -> b = b' && annot a a'
  | (Enum (e, a), Enum (e', a')) -> e = e' && annot a a'
  | (Unop (u, e, a), Unop (u', e', a')) -> u = u' && eqe e e' && annot a a'
  | (Binop (b, e, e2, a), Binop (b', e', e2', a')) ->
      b = b' && eqe e e' && eqe e2 e2' && annot a a'
  | (Manyop (m, el, a), Manyop (m', el', a')) -> m = m' && List.equal eqe el el' && annot a a'
  | (Ite (c, e, e2, a), Ite (c', e', e2', a')) -> eqe c c' && eqe e e' && eqe e2 e2' && annot a a'
  | (Let (b, bl, e, a), Let (b', bl', e', a')) ->
      let eq_bind = Pair.equal ~fst:bnd ~snd:eqe in
      eq_bind b b' && List.equal eq_bind bl bl' && eqe e e' && annot a a'
  | _ -> false
