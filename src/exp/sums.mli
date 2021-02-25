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

(** This module provide sum manipulation functionality on top of typed expression {!Typed.t}

    This provide a semantic view of sums as list of terms and conversion

*)

(** Split an expression as a list of terms. This function sees through +,- and extracts.

    Any expression [e] should have the same semantic meaning as [Typed.sum (split e)].

    TODO I need to sort according to an arbitrary order to be able to compare reliably.
    This will probably be part of a more general simplifier work.*)
val split : ('v, 'm) Typed.t -> ('v, 'm) Typed.t list

(** Merge a list of terms into a sum expression.
    This is an upgrade of {!Typed.sum} to allow empty lists.
    In the case of an empty list, a [0] of size [size] will be inserted instead. *)
val merge : size:int -> ('v, 'm) Typed.t list -> ('v, 'm) Typed.t

(** Add a [term] to a sum, This is the same, as using {!split}, then adding [term] to the list,
    then merging with {!Typed.sum}*)
val add_term : term:('v, 'm) Typed.t -> ('v, 'm) Typed.t -> ('v, 'm) Typed.t

(** Remove a [term] from a sum.
    Return [Some res] if successful and [None] otherwise. *)
val remove_term :
  equal:(('v, 'm) Typed.t -> ('v, 'm) Typed.t -> bool) ->
  term:('v, 'm) Typed.t ->
  ('v, 'm) Typed.t ->
  ('v, 'm) Typed.t option

(** Same as {!remove_term} but if the [term] is not found, add the opposite
    ({!Ast.Bvneg}) to the sum*)
val smart_substract :
  equal:(('v, 'm) Typed.t -> ('v, 'm) Typed.t -> bool) ->
  term:('v, 'm) Typed.t ->
  ('v, 'm) Typed.t ->
  ('v, 'm) Typed.t

(** Split away the concrete terms of the sum and the symbolic part.
    The symbolic part can be [None] if the expression was fully concrete.
    If the symbolic part is [Some e], then
    [not ]{!has_concrete_term}[ e] will hold.*)
val split_concrete : ('v, Ast.no) Typed.t -> ('v, Ast.no) Typed.t option * BitVec.t

(** Tells if an expression has a concrete term *)
val has_concrete_term : ('v, 'm) Typed.t -> bool
