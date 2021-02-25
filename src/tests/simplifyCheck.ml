(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Read-dwarf, located in the src/ and test_asm/ directories, is subject to this   *)
(*  BSD 2-Clause License. This license does not apply to files outside these        *)
(*  directories.                                                                    *)
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

(** This module is about testing Z3 simplify against Z3 check. It's not really
    about testing Z3 itself but about testing our parsing of Z3 expression output
    (in particular let bindings unfolding) *)

open Common

let var_exp_gen_ty size ty =
  ExpGen.Gen.(
    exp_from_params
      { typ = ty; size; bv_atom_gen = bv_atom_with_var; bool_atom_gen = bool_atom_with_var })

let var_exp_gen size = Gen.(ExpGen.Gen.typ >>= var_exp_gen_ty size)

(** Shrink by trying all sub expressions *)
let const_exp_shrinker_top exp yield = Ast.Manip.direct_exp_iter_exp yield exp

(** Shrink by replacing a non-atomic expression by it's simplified version *)
let const_exp_shrinker_bot exp yield =
  if Ast.is_atomic exp then ()
  else
    try
      let simplified = ExpGen.Z3.simplify_full exp in
      yield simplified
    with _ -> (* If any exception happens, we don't shrink *)
              ()

(** Shrink by using both {!const_exp_shrinker_top} and {!const_exp_shrinker_bot} *)
let const_exp_shrinker exp yield =
  const_exp_shrinker_top exp yield;
  ExpGen.shrink_propagate const_exp_shrinker_bot exp yield

let var_exp = ExpGen.from_gen ~shrink:const_exp_shrinker Gen.(small_nat >>= var_exp_gen)

let simplify_check =
  QCT.make ~count:100 ~name:"Z3 simplify against Z3 check" var_exp (fun exp ->
      try
        let simplified = ExpGen.Z3.simplify_full exp in
        let asserteq = Exp.Typed.(exp = simplified) in
        match ExpGen.Z3.check_full asserteq with Some b -> b | None -> Q.assume_fail ()
        (* Sometime Z3 can't check it's own simplifcation.
           This is both rare and weird. I just ask to restart in that case *)
      with exn ->
        Z3.reset ();
        Raise.again exn)

let tests = [simplify_check]
