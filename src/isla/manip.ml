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

(** This module provide generic manipulation function of isla ast *)

open Base

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Trace Properties} *)

(** Check if a trace is linear (has no branches) *)
let is_linear (Trace events : 'a trc) =
  let rec no_branch = function [] -> true | Branch _ :: _ -> false | _ :: l -> no_branch l in
  no_branch events

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Get annotations } *)

(** Get the annotation of an expression *)
let annot_exp : 'a exp -> 'a = function
  | Val (_, a) -> a
  | Unop (_, _, a) -> a
  | Binop (_, _, _, a) -> a
  | Manyop (_, _, a) -> a
  | Ite (_, _, _, a) -> a

(** Get the annotation of an event *)
let annot_event : 'a event -> 'a = function
  | Smt (_, a) -> a
  | Branch (_, _, a) -> a
  | BranchAddress (_, a) -> a
  | ReadReg (_, _, _, a) -> a
  | WriteReg (_, _, _, a) -> a
  | Cycle a -> a
  | WakeRequest a -> a
  | ReadMem (_, _, _, _, _, a) -> a
  | WriteMem (_, _, _, _, _, _, a) -> a
  | Barrier (_, a) -> a
  | CacheOp (_, _, a) -> a
  | Instr (_, a) -> a
  | Sleeping (_, a) -> a
  | SleepRequest a -> a
  | MarkReg (_, _, a) -> a
  | AssumeReg (_, _, _, a) -> a
  | Assume (_, a) -> a
  | FunAssume (_, _, _, a) -> a
  | UseFunAssume (_, _, _, a) -> a
  | AbstractCall (_, _, _, a) -> a
  | AbstractPrimop (_, _, _, a) -> a

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Non-recursive maps and iters }

   This section is filled on demand.

   [direct_a_map_b] take a function [b -> b] and applies it to all [b] in [a], non-recursively.
   Then a new a with the same structure is formed.

   [direct_a_iter_b] take a function [b -> unit] and applies it to all [b] in [a], non-recursively.
*)

let direct_exp_map_exp (f : 'a exp -> 'a exp) = function
  | Unop (u, e, l) -> Unop (u, f e, l)
  | Binop (b, e, e', l) -> Binop (b, f e, f e', l)
  | Manyop (m, el, l) -> Manyop (m, List.map f el, l)
  | Ite (c, e, e', l) -> Ite (f c, f e, f e', l)
  | Val (v, a) -> Val (v, a)

let direct_exp_iter_exp (i : 'a exp -> unit) = function
  | Unop (_, e, _) -> i e
  | Binop (_, e, e', _) ->
      i e;
      i e'
  | Manyop (_, el, _) -> List.iter i el
  | Ite (c, e, e', _) ->
      i c;
      i e;
      i e'
  | Val _ -> ()

let direct_smt_map_exp (m : 'a exp -> 'a exp) : 'a smt -> 'a smt = function
  | DefineConst (v, exp) -> DefineConst (v, m exp)
  | Assert exp -> Assert (m exp)
  | DeclareConst _ as d -> d
  | DefineEnum _ as d -> d

let direct_event_map_exp (m : 'a exp -> 'a exp) : 'a event -> 'a event = function
  | Smt (smt, l) -> Smt (direct_smt_map_exp m smt, l)
  | event -> event

let direct_event_iter_valu (i : valu -> unit) : 'a event -> unit = function
  | Smt _ -> ()
  | Branch _ -> ()
  | BranchAddress (v, _) -> i v
  | ReadReg (_, _, v, _) -> i v
  | WriteReg (_, _, v, _) -> i v
  | Cycle _ -> ()
  | WakeRequest _ -> ()
  | SleepRequest _ -> ()
  | ReadMem (v, v', v'', _, v_opt, _) ->
      i v;
      i v';
      i v'';
      Option.iter i v_opt
  | WriteMem (_, v, v', v'', _, v_opt, _) ->
      i v;
      i v';
      i v'';
      Option.iter i v_opt
  | Barrier (v, _) -> i v
  | CacheOp (v, v', _) ->
      i v;
      i v'
  | MarkReg _ -> ()
  | Instr (v, _) -> i v
  | Sleeping _ -> ()
  | AssumeReg (_, _, v, _) -> i v
  | Assume _ -> ()
  | FunAssume (_, v,_, _) -> i v
  | UseFunAssume (_, v,_, _) -> i v
  | AbstractCall (_, v,_, _) -> i v
  | AbstractPrimop (_, v,_, _) -> i v

let direct_event_map_valu (m : valu -> valu) : 'a event -> 'a event = function
  | BranchAddress (v, l) -> BranchAddress (m v, l)
  | ReadReg (a, b, v, c) -> ReadReg (a, b, m v, c)
  | WriteReg (a, b, v, c) -> WriteReg (a, b, m v, c)
  | ReadMem (v, v', v'', a, v_opt, b) -> ReadMem (m v, m v', m v'', a, Option.map m v_opt, b)
  | WriteMem (a, v, v', v'', b, v_opt, c) ->
      WriteMem (a, m v, m v', m v'', b, Option.map m v_opt, c)
  | Barrier (v, l) -> Barrier (m v, l)
  | CacheOp (v, v', l) -> CacheOp (m v, m v', l)
  | Instr (v, l) -> Instr (m v, l)
  | e -> e

let direct_valu_iter_valu (i : valu -> unit) : valu -> unit = function
  | RegVal_Base _ -> ()
  | RegVal_I _ -> ()
  | RegVal_String _ -> ()
  | RegVal_Unit -> ()
  | RegVal_Constructor _ -> ()
  | RegVal_Vector l -> List.iter i l
  | RegVal_List l -> List.iter i l
  | RegVal_Struct l -> List.iter (fun (_, v) -> i v) l
  | RegVal_Poison -> ()

let direct_valu_map_valu (m : valu -> valu) : valu -> valu = function
  | RegVal_Vector l -> RegVal_Vector (List.map m l)
  | RegVal_List l -> RegVal_List (List.map m l)
  | RegVal_Struct l -> RegVal_Struct (List.map (Pair.map Fun.id m) l)
  | valu -> valu

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Recursive maps and iters }

    This section is filled on demand.

    [a_map_b] take a function [b -> b] and applies it to all the [b] in [a], and do that
    recursively on all b that appear directly or indirectly in a

    [a_iter_b] take a function [b -> unit] and applies it to all the [b] in [a], and do that
    recursively

    Doing this when a = b is not well defined, and can be easily done using the direct
    version from previous section.

    In case where a type is not recusive like [event],
    both direct and recursive versions are the same.*)

(** iterate a function on all the variable of an expression *)
let rec exp_iter_var (f : int -> unit) : 'a exp -> unit = function
  | Val (Val_Symbolic v, _) -> f v
  | exp -> direct_exp_iter_exp (exp_iter_var f) exp

let event_iter_valu = direct_event_iter_valu

(** Iterate a function over a [valu] broken up into fields, extending the given path. *)
let rec iter_valu_path (f : string list -> valu -> unit) (path : string list) : valu -> unit = function
  | RegVal_Struct l -> List.iter (fun (name, v) -> iter_valu_path f (path @ [name]) v) l
  | v -> f path v

(** Map a function over a [valu] broken up into fields, extending the given path. *)
let rec map_valu_path (f : string list -> valu -> 'a) (path : string list) : valu -> 'a list = function
  | RegVal_Struct l -> List.concat_map (fun (name, v) -> map_valu_path f (path @ [name]) v) l
  | v -> [f path v]

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Variable substitution } *)

(** Substitute variable with expression according to subst function *)
let rec var_subst (subst : int -> 'a -> 'a exp) (exp : 'a exp) : 'a exp =
  let s = var_subst subst in
  match exp with Val (Val_Symbolic v, a) -> subst v a | _ -> direct_exp_map_exp s exp

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Accessor list conversion } *)

let accessor_of_string s = Field s

let string_of_accessor (Field s) = s

let accessor_of_string_list = function [] -> Nil | l -> Cons (List.map accessor_of_string l)

let string_of_accessor_list = function Nil -> [] | Cons l -> List.map string_of_accessor l

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Valu string path access } *)

(** Follow the path in a value like A.B.C in (struct (|B| (struct (|C| ...)))) *)
let rec valu_get valu path =
  match (valu, path) with
  | (_, []) -> valu
  | (RegVal_Struct s, a :: l) -> valu_get (List.assoc a s) l
  | _ -> failwith "islaManip.valu_get: Invalid path in IslaManip.valu_get"

(*****************************************************************************)
(*****************************************************************************)
(*****************************************************************************)
(** {1 Trace Filtering }

    This is some basic trace filtering that remove unwanted item from the trace in
    various situations
*)

(** Split the trace between before and after the "cycle" event *)
let split_cycle : 'a trc -> 'a trc * 'a trc = function
  | Trace l ->
      let rec split_cycle_list aux = function
        | [] -> (List.rev aux, [])
        | Cycle _ :: l -> (List.rev aux, l)
        | a :: l -> split_cycle_list (a :: aux) l
      in
      let (l1, l2) = split_cycle_list [] l in
      (Trace l1, Trace l2)

(** Remove all events before the "cycle" event, keep the SMT statements *)
let remove_init : 'a trc -> 'a trc = function
  | Trace l ->
      let rec pop_until_cycle = function
        | [] -> []
        | Cycle _ :: l -> l
        | Smt (v, a) :: l -> Smt (v, a) :: pop_until_cycle l
        | _ :: l -> pop_until_cycle l
      in
      Trace (pop_until_cycle l)

(** Remove all the events related to ignored registers *)
let remove_ignored ignored_regs : 'a trc -> 'a trc = function
  | Trace l ->
      Trace
        (List.filter
           (function
             | ReadReg (name, _, _, _) | WriteReg (name, _, _, _) ->
                 not @@ List.mem String.equal name ignored_regs
             | _ -> true)
           l)
