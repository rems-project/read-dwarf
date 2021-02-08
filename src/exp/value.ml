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

(** This module provide a type to represent concrete values.

    There is not concrete value for memory yet, but maybe there should be at some point to
    be able to fully support concrete evaluation without external tool*)

(** The type of concrete values *)
type t = Bool of bool | Enum of Ast.enum | Bv of BitVec.t | Vec of t list

(** String representation of concrete values *)
let to_string = function
  | Bool true -> "true"
  | Bool false -> "false"
  | Enum (a, n) -> Printf.sprintf "enum%d/%d" a n
  | Bv bv -> BitVec.to_smt bv
  | Vec _ -> (* FIXME: figure out right rep of vectors (tuples?) *) Raise.todo ()

(** Pretty printer for concrete values *)
let pp r = r |> to_string |> Pp.string

(** {!Bool} constructor *)
let bool b = Bool b

(** {!Enum} constructor *)
let enum enum = Enum enum

(** {!Bv} constructor *)
let bv bv = Bv bv

(** {!Vec} constructor *)
let vec vs = Vec vs

(** Extract a boolean or fail *)
let expect_bool = function
  | Bool bool -> bool
  | r -> Raise.fail "Expected boolean but got %s" (to_string r)

(** Extract an enumeration or fail *)
let expect_enum = function
  | Enum enum -> enum
  | r -> Raise.fail "Expected enumeration but got %s" (to_string r)

(** Extract a bit vector or fail. If [size] is specified, then it fail if the size don't match *)
let expect_bv ?size r =
  match (size, r) with
  | (Some size, Bv bv) when BitVec.size bv = size -> bv
  | (None, Bv bv) -> bv
  | (Some size, _) -> Raise.fail "Expected bit vector of size %d but got %s" size (to_string r)
  | (None, _) -> Raise.fail "Expected bit vector but got %s" (to_string r)

(** Convert to a constant expression *)
let rec to_exp : t -> ('v, 'm) Typed.t = function
  | Bool b -> Typed.bool b
  | Enum enum -> Typed.enum enum
  | Bv bv -> Typed.bits bv
  | Vec vs -> Typed.vec @@ List.map to_exp vs
