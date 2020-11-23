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

(* The documentation is in the mli file *)

exception Symbolic

type 'v context = 'v -> Value.t

let eval_unop (u : Ast.unop) v =
  match u with
  | Not -> v |> Value.expect_bool |> not |> Value.bool
  | Bvnot -> v |> Value.expect_bv |> BitVec.lognot |> Value.bv
  | Bvneg -> v |> Value.expect_bv |> BitVec.neg |> Value.bv
  | Bvredand -> v |> Value.expect_bv |> BitVec.redand |> BitVec.of_bool |> Value.bv
  | Bvredor -> v |> Value.expect_bv |> BitVec.redor |> BitVec.of_bool |> Value.bv
  | Extract (b, a) -> v |> Value.expect_bv |> BitVec.extract a b |> Value.bv
  | ZeroExtend m -> v |> Value.expect_bv |> BitVec.zero_extend m |> Value.bv
  | SignExtend m -> v |> Value.expect_bv |> BitVec.sign_extend m |> Value.bv

let eval_bvarith (b : Ast.bvarith) v v' =
  let open BitVec in
  match b with
  | Bvnand -> lnot (v land v')
  | Bvnor -> lnot (v lor v')
  | Bvxnor -> lnot (v lxor v')
  | Bvsub -> v - v'
  | Bvudiv -> udiv v v'
  | Bvudivi -> udiv v v'
  | Bvsdiv -> sdiv v v'
  | Bvsdivi -> sdiv v v'
  | Bvurem -> urem v v'
  | Bvuremi -> urem v v'
  | Bvsrem -> srem v v'
  | Bvsremi -> srem v v'
  | Bvsmod -> smod v v'
  | Bvsmodi -> smod v v'
  | Bvshl -> v lsl v'
  | Bvlshr -> v lsr v'
  | Bvashr -> v asr v'

let eval_bvcomp (b : Ast.bvcomp) v v' =
  let open BitVec in
  match b with
  | Bvult -> to_uz v < to_uz v'
  | Bvslt -> to_z v < to_z v'
  | Bvule -> to_uz v <= to_uz v'
  | Bvsle -> to_z v <= to_z v'
  | Bvuge -> to_uz v >= to_uz v'
  | Bvsge -> to_z v >= to_z v'
  | Bvugt -> to_uz v > to_uz v'
  | Bvsgt -> to_z v > to_z v'

let eval_binop (b : Ast.no Ast.binop) v v' =
  match b with
  | Eq -> v = v' |> Value.bool
  | Bvarith bva -> eval_bvarith bva (Value.expect_bv v) (Value.expect_bv v') |> Value.bv
  | Bvcomp bvc -> eval_bvcomp bvc (Value.expect_bv v) (Value.expect_bv v') |> Value.bool
  | Binmem m -> Ast.destr_binmem m

let eval_bvmanyarith (m : Ast.bvmanyarith) (bvs : BitVec.t list) =
  let open BitVec in
  match m with
  | Bvand -> List.fold_left_same ( land ) bvs
  | Bvor -> List.fold_left_same ( lor ) bvs
  | Bvxor -> List.fold_left_same ( lxor ) bvs
  | Bvadd -> List.fold_left_same ( + ) bvs
  | Bvmul -> List.fold_left_same ( * ) bvs

let eval_manyop (m : Ast.manyop) vs =
  match m with
  | And -> List.for_all Value.expect_bool vs |> Value.bool
  | Or -> List.exists Value.expect_bool vs |> Value.bool
  | Concat -> vs |> List.map Value.expect_bv |> List.fold_left_same BitVec.concat |> Value.bv
  | Bvmanyarith bvma -> eval_bvmanyarith bvma (List.map Value.expect_bv vs) |> Value.bv

let rec eval ?(ctxt = fun _ -> raise Symbolic) (e : ('a, 'v, Ast.no, Ast.no) Ast.exp) : Value.t =
  match e with
  | Var (v, _) -> ctxt v
  | Bound _ -> .
  | Bits (bv, _) -> bv |> Value.bv
  | Bool (b, _) -> b |> Value.bool
  | Enum (enum, _) -> enum |> Value.enum
  | Vec (es, _) -> es |> List.map (eval ~ctxt) |> Value.vec
  | Unop (u, v, _) -> v |> eval ~ctxt |> eval_unop u
  | Binop (b, v, v', _) -> eval_binop b (eval ~ctxt v) (eval ~ctxt v')
  | Manyop (m, vs, _) -> vs |> List.map (eval ~ctxt) |> eval_manyop m
  | Ite (c, e, e', _) ->
      let cv = eval ~ctxt c in
      if cv |> Value.expect_bool then eval ~ctxt e else eval ~ctxt e'
  (*| Call _ -> Raise.todo () *)
  (*| Exists _ *)
  | Let _ -> .

let rec is_concrete (exp : _ Ast.exp) : bool =
  match exp with
  | Bound _ -> false
  | Var _ -> false
  | exp -> Ast.Manip.direct_exp_for_all_exp is_concrete exp

let eval_if_concrete (exp : _ Ast.exp) : Value.t option =
  try eval exp |> Option.some with Symbolic -> None
