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
(*  The project has been partly funded by EPSRC grant EP/K008528/1.                 *)
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

(** This module provides a human readable pretty printing for {!Ast} expressions

    If you don't want to bother with details, use [pp_exp] and don't read the rest.

    The precedence are the ones from C/C++ and Ocaml with some tweaks.
    In particular the precedence between bitwise operation and arithmetic operation
    are separated, so parenthesis will always be required between them.

    The order is:
    - Extraction and extension
    - Concatenation
    - unary minus, unary bitwise negation, unary reduction
    - multiplications, divisions, separately shift
    - additions, substractions, separately bitwise operation
    - comparisons
    - equality
    - and
    - or
    - ifs

    Unary operator cannot linebreak (but their content can)

    Examples:

      - [-a\[1-3\].2a:6\[z+32\]] which is:
        [(bvneg (concat ((_ extract 3 1) a) ((_ zero_extend 32) #b101010)))]
*)

open Pp

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Precedence } *)

(** The operators possible precedence *)
type prec =
  | IF
  | OR
  | AND
  | EQ
  | COMP
  | ADD
  | MUL
  | BITS
  | SHIFTS
  | UNARY
  | CONCAT
  | EXTRACT
  | PARENS

(** Figure out if an expression of precedence [inner] in an
    expression of precedence [outer] needs parentheses *)
let compat ~outer ~inner =
  let arith o = o = ADD || o = MUL in
  let bits o = o = BITS || o = SHIFTS in
  if (arith outer && bits inner) || (bits outer && bits inner) then false else outer < inner

let prec_unop : Ast.unop -> prec = function
  | Extract _ | ZeroExtend _ | SignExtend _ -> EXTRACT
  | _ -> UNARY

let prec_bvarith : Ast.bvarith -> prec = function
  | Bvnand -> AND
  | Bvnor | Bvxnor -> OR
  | Bvsub -> ADD
  | Bvshl | Bvlshr | Bvashr -> SHIFTS
  | Bvudiv | Bvudivi | Bvsdiv | Bvsdivi | Bvurem | Bvuremi | Bvsrem | Bvsremi | Bvsmod | Bvsmodi
    ->
      MUL

let prec_binop : Ast.no Ast.binop -> prec = function
  | Eq -> EQ
  | Bvarith bva -> prec_bvarith bva
  | Bvcomp _ -> COMP
  | Binmem m -> Ast.destr_binmem m

let prec_bvmanyarith : Ast.bvmanyarith -> prec = function
  | Bvand | Bvor | Bvxor -> BITS
  | Bvadd -> ADD
  | Bvmul -> MUL

let prec_manyop : Ast.manyop -> prec = function
  | And -> AND
  | Or -> OR
  | Concat -> CONCAT
  | Bvmanyarith bvma -> prec_bvmanyarith bvma

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Actual Pretty printing } *)

let pp_bits bv =
  let size = BitVec.size bv in
  let main_part = bv |> BitVec.to_string ~base:16 ~force_width:false ~prefix:true |> string in
  match size with
  | 1 -> if BitVec.to_z bv = Z.zero then !^"0:1" else !^"1:1"
  | 64 -> main_part
  | _ -> main_part ^^ colon ^^ int size

let ppnot = Pp.char '!'

let pp_unop (u : Ast.unop) doc =
  match u with
  | Not -> ppnot ^^ doc
  | Bvnot -> tilde ^^ doc
  | Bvneg -> minus ^^ doc
  | Bvredor -> !^"|>" ^^ doc ^^ !^"<|"
  | Bvredand -> !^"&>" ^^ doc ^^ !^"<&"
  | Extract (b, a) -> doc ^^ dprintf "[%d:%d]" a b
  | ZeroExtend m -> doc ^^ dprintf "[z+%d]" m
  | SignExtend m -> doc ^^ dprintf "[s+%d]" m

let sym_bvarith : Ast.bvarith -> document = function
  | Bvnand -> !^"&!&"
  | Bvnor -> !^"|!|"
  | Bvxnor -> !^"^!^"
  | Bvsub -> minus
  | Bvudiv -> !^"u/"
  | Bvudivi -> !^"u/i"
  | Bvsdiv -> !^"s/"
  | Bvsdivi -> !^"s/i"
  | Bvurem -> !^"urem"
  | Bvuremi -> !^"uremi"
  | Bvsrem -> !^"srem"
  | Bvsremi -> !^"sremi"
  | Bvsmod -> !^"smod"
  | Bvsmodi -> !^"smodi"
  | Bvshl -> !^"<<"
  | Bvlshr -> !^"l>>"
  | Bvashr -> !^"a>>"

let sym_bvcomp : Ast.bvcomp -> document = function
  | Bvult -> !^"u<"
  | Bvslt -> !^"s<"
  | Bvule -> !^"u<="
  | Bvsle -> !^"s<="
  | Bvuge -> !^"u>="
  | Bvsge -> !^"s>="
  | Bvugt -> !^"u>"
  | Bvsgt -> !^"s>"

let pp_binop (b : Ast.no Ast.binop) doc doc' =
  let bin sym doc doc' = doc ^^ space ^^ sym ^^ nbspace ^^ doc' |> nest 4 |> group in
  match b with
  | Eq -> bin equals doc doc' |> group
  | Bvarith bva -> bin (sym_bvarith bva) doc doc' |> group
  | Bvcomp bvc -> bin (sym_bvcomp bvc) doc doc' |> group
  | Binmem m -> Ast.destr_binmem m

let sym_bvmanyarith : Ast.bvmanyarith -> document = function
  | Bvand -> !^"&"
  | Bvor -> bar
  | Bvxor -> !^"^"
  | Bvadd -> plus
  | Bvmul -> star

let pp_manyop (m : Ast.manyop) docs =
  let many sym docs = separate (space ^^ sym ^^ nbspace) docs |> nest 4 |> group in
  match m with
  | And -> many !^"and" docs
  | Or -> many !^"or" docs
  | Concat -> separate (break 0 ^^ dot) docs |> nest 4 |> group
  | Bvmanyarith bvma -> many (sym_bvmanyarith bvma) docs

let pp_if cond doc doc' =
  prefix 2 1 !^"if" cond ^/^ prefix 2 1 !^"then" doc ^/^ prefix 2 1 !^"else" doc' |> group

(** Pretty print an expression and return its precedence *)
let rec pp_exp_prec ppv : ('a, 'v, Ast.no, Ast.no) Ast.exp -> document * prec =
  let parens_if ~outer (doc, inner) = if compat ~outer ~inner then doc else parens doc in
  function
  | Var (v, _) -> (group (ppv v), PARENS)
  | Bound _ -> .
  | Bits (bv, _) -> (pp_bits bv, PARENS)
  | Bool (b, _) -> (bool b, PARENS)
  | Enum (e, _) -> (Ast.pp_enum e, PARENS)
  | Unop (u, e, _) ->
      let outer = prec_unop u in
      let doc = parens_if ~outer @@ pp_exp_prec ppv e in
      (pp_unop u doc, outer)
  | Binop (b, e, e', _) ->
      let outer = prec_binop b in
      let doc = parens_if ~outer @@ pp_exp_prec ppv e in
      let doc' = parens_if ~outer @@ pp_exp_prec ppv e' in
      (pp_binop b doc doc', outer)
  | Manyop (m, el, _) ->
      let outer = prec_manyop m in
      let docs = List.map (fun e -> parens_if ~outer @@ pp_exp_prec ppv e) el in
      (pp_manyop m docs, outer)
  | Vec (el, _) ->
      let docs = List.map (fun e -> fst @@ pp_exp_prec ppv e) el in
      (brackets @@ separate comma docs, PARENS)
  | Ite (c, e, e', _) ->
      let outer = IF in
      let docc = parens_if ~outer @@ pp_exp_prec ppv c in
      let doc = parens_if ~outer @@ pp_exp_prec ppv e in
      let doc' = parens_if ~outer @@ pp_exp_prec ppv e' in
      (pp_if docc doc doc', outer)
  | Let _ -> .

(** The main function for pretty printing an expression *)
let pp_exp ppv exp = pp_exp_prec ppv exp |> fst
