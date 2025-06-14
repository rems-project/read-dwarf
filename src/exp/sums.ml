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

open Logs.Logger (struct
  let str = __MODULE__
end)

(* The documentation is in the mli file *)

let rec split =
  let open Ast in
  function
  | Manyop (Bvmanyarith Bvadd, l, _) -> List.concat_map split l
  | Unop (Extract (last, (0 as first)), e, _) ->
      let l = split e in
      List.map (Typed.extract ~first ~last) l
  | Unop (Bvneg, e, _) ->
      let l = split e in
      List.map Typed.neg l
  | Binop (Bvarith Bvsub, e, e', _) ->
      let l = split e in
      let l' = split e' in
      let rl' = List.rev_map Typed.neg l' in
      List.rev_append rl' l
  | e -> [e]

let merge ~size l = if l = [] then Typed.zero ~size else Typed.sum l

let add_term ~term exp = term :: split exp |> Typed.sum

let remove_term ~equal ~term exp =
  let terms = split exp in
  match List.remove (equal term) terms with
  | None -> None
  | Some [] ->
      let size = Typed.expect_bv (Typed.get_type term) in
      Some (Typed.zero ~size)
  | Some l -> Some (Typed.sum l)

(** Same as {!remove_term} but if the term is not found, add the opposite to the sum*)
let smart_substract ~equal ~term exp =
  let terms = split exp in
  match List.remove (equal term) terms with
  | None -> Typed.sum @@ (Typed.neg term :: terms)
  | Some [] ->
      let size = Typed.expect_bv (Typed.get_type term) in
      Typed.zero ~size
  | Some l -> Typed.sum l

(** Split away the concrete terms of the sum *)
let split_concrete exp =
  let size = Typed.expect_bv (Typed.get_type exp) in
  let terms = split exp in
  debug "Split:";
  List.iter (fun t -> debug "\t%t" Pp.(top (PpExp.pp_exp (fun _ -> !^"var")) t)) terms;
  let (symterms, concvals) = List.partition_map ConcreteEval.eval_if_concrete terms in
  let concbvs = List.map Value.expect_bv concvals in
  let concbv = List.fold_left BitVec.( + ) (BitVec.zero ~size) concbvs in
  let symterm = match symterms with [] -> None | _ -> Some (Typed.sum symterms) in
  (symterm, concbv)

(** Tells if an expression has a concrete term *)
let has_concrete_term exp = exp |> split |> List.exists ConcreteEval.is_concrete
