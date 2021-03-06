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

(** This module is for testing {!ConcreteEval} *)

open Common
module ConcreteEval = Exp.ConcreteEval
module Value = Exp.Value
module Typed = Exp.Typed

let const_exp_gen_ty size ty =
  ExpGen.Gen.(
    exp_from_params { typ = ty; size; bv_atom_gen = bv_consts; bool_atom_gen = bool_consts })

let const_exp_gen size = Gen.(ExpGen.Gen.typ >>= const_exp_gen_ty size)

(** Shrink by trying all sub expressions *)
let const_exp_shrinker_top exp yield = Ast.Manip.direct_exp_iter_exp yield exp

(** Shrink by replacing a non-atomic constant expression by it's constEval evaluation *)
let const_exp_shrinker_bot exp yield =
  if Ast.is_atomic exp then ()
  else
    let value = ConcreteEval.eval exp in
    let expv = Value.to_exp value in
    yield expv

(** Shrink by using both {!const_exp_shrinker_top} and {!const_exp_shrinker_bot} *)
let const_exp_shrinker exp yield =
  const_exp_shrinker_top exp yield;
  ExpGen.shrink_propagate const_exp_shrinker_bot exp yield

let const_exp = ExpGen.from_gen ~shrink:const_exp_shrinker Gen.(small_nat >>= const_exp_gen)

let concrete_eval =
  QCT.make ~count:1000 ~name:"ConcreteEval against Z3" const_exp (fun exp ->
      try
        let value = ConcreteEval.eval exp in
        (* PP.(eprintln @@ dprintf "exp:" ^^ ExpGen.ExpT.pp_smt exp); *)
        (* PP.eprintln (Value.pp value); *)
        let expv = Value.to_exp value in
        let asserteq = Typed.(exp = expv) in
        ExpGen.Z3.check_full asserteq = Some true
      with Division_by_zero -> Q.assume_fail ()
      (* We want to assume there is no division by 0 *))

let tests = [concrete_eval]
