include AstDef

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Size } *)

module Size = struct
  include AstDef.Size

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

module Parser = AstParser
module Lexer = AstLexer

let unknown = Isla.UnknownRng

(** Exception that represent an Isla parsing error *)
exception ParseError of loc * string

(* Registering a pretty printer for that exception *)
let _ =
  Printexc.register_printer (function
    | ParseError (l, s) ->
        Some PP.(sprint @@ prefix 2 1 (loc l ^^ !^": ") (!^"ParseError: " ^^ !^s))
    | _ -> None)

(** Exception that represent an Isla lexing error *)
exception LexError of loc * string

(* Registering a pretty printer for that exception *)
let _ =
  Printexc.register_printer (function
    | LexError (l, s) -> Some PP.(sprint @@ prefix 2 1 (loc l ^^ !^": ") (!^"LexError: " ^^ !^s))
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
  | Lexer.Error s -> raise (LexError (l.lex_start_p, "Unexpected character"))

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
include AstParser_pp

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Desctructor }

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
  | _ -> Raise.fail "expected a bitvector expression"

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Operators } *)

module Op = struct
  let add a b = Manyop (Bvmanyarith Bvadd, [a; b], unknown)

  let ( + ) a b = add a b

  let sub a b = Binop (Bvarith Bvsub, a, b, unknown)

  let ( - ) a b = sub a b

  let mul a b = Manyop (Bvmanyarith Bvmul, [a; b], unknown)

  let ( * ) a b = add a b

  let sdiv a b = Binop (Bvarith Bvsdiv, a, b, unknown)

  let not a = Unop (Not, a, unknown)

  let extract a b e = Unop (Extract (a, b), e, unknown)

  let var v = Var (v, unknown)

  let eq a b = Binop (Eq, a, b, unknown)

  let ( = ) = eq

  let bits bv = Bits (bv, unknown)

  let bits_int ~size i = bits (BitVec.of_int ~size i)

  let bits_smt s = bits (BitVec.of_smt s)

  let assert_op e = Assert e

  let simplify ?(flags = []) e = Simplify (e, flags)
end
