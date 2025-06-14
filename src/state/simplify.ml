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

(** This module provide utility to simplify states *)

open Logs.Logger (struct
  let str = __MODULE__
end)

(* This is an extension of the State module *)
open Base
module Z3St = Z3.Make (Var)

(** Context free simplify, just expression by expression. Do it mutably *)
let ctxfree state = map_mut_exp Z3St.simplify_full state

(** Do a context aware simplify, for now, it just does a context free simplify and then
    remove assertion that proven true or false by Z3
*)
module ContextFull = Z3.ContextCounter (struct
  let str = "State context-full simplification"
end)

(** Do a context aware simplify, for now, it just does a context free simplify and then
    remove assertion that proven true or false by Z3.

    If a state has a false assertion anywhere then all assertions will collapse
    in a single false.
*)
let ctxfull state =
  debug "Simplifying state %t" (Pp.top Id.pp state.id);
  let serv = Z3.ensure_started_get () in
  ContextFull.openc ();
  let declared = Z3St.Htbl.create 100 in
  let declare exp = Z3St.declare_vars ~declared serv exp in
  let rec load state =
    Option.iter load state.base_state;
    List.iter (Z3St.send_assert_decl ~declared serv) state.asserts
  in
  (* load previous assertion into Z3 *)
  Option.iter load state.base_state;
  (* Context-free simplification of expressions *)
  (* TODO make that also context_full *)
  map_mut_exp (fun e -> if Ast.is_atomic e then e else Z3St.simplify_decl ~declared serv e) state;
  (* Context-full simplification of assertion *)
  let found_false = ref false in
  let new_asserts =
    List.filter_map
      (fun e ->
        declare e;
        match Z3St.check_both serv e with
        | Some true ->
            debug "%t is redundant" (Pp.top Exp.pp e);
            None
        | Some false ->
            found_false := true;
            debug "%t is impossible" (Pp.top Exp.pp e);
            None
        | None -> 
            debug "%t is possible" (Pp.top Exp.pp e);
            Z3St.send_assert serv e;
            Some e
        )
      state.asserts
  in
  (* If state is impossible then it has a single assertion: false *)
  if !found_false then set_impossible state else set_asserts state new_asserts;
  debug "closing";
  ContextFull.closec ()
