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

open Base

open Logs.Logger (struct
  let str = __MODULE__
end)

(** Explain what a variable means in the current context,
    Each of the variable of the old trace is associated to one of these *)
type simplify_context_element =
  | Declared of { ty : ty; loc : lrng }
      (** This mean that the variable has been declared in the old trace with this
          trace, but not yet in the new trace *)
  | Defined of { exp : rexp; loc : lrng }
      (** This mean that the variable has been defined in the old trace with this
          expression, but not yet in the new trace *)
  | Processed of int
      (** This means that the variable has been be processed and it gives
          the equivalent variable number in the new trace *)

let expect_processed = function
  | Processed i -> i
  | _ -> Raise.fail "Variables should be processed at this point"

(** Preprocess a single trace *)
let simplify_trc ?(num_segments = 0) (Trace events : rtrc) : rtrc =
  (* Phase 1: Discover which variable are actually used *)
  let used = HashVector.empty () in
  let process_used event =
    let rec process_used_valu = function
      | RegVal_Base (Val_Symbolic i) ->
          debug "Marking v%d as used" i;
          HashVector.set used i ()
      | v -> Manip.direct_valu_iter_valu process_used_valu v
    in
    Manip.event_iter_valu process_used_valu event
  in
  List.iter process_used events;
  debug "The list of used vars is:\n%t" (Pp.top (HashVector.pp Pp.erase) used);
  (* Phase 2: Only keep variables that are actually used
     This is done lazily. On declaration or definition, variables are
     Stored in a lazy fashion in the context.
     If they are used as determined by the previous phase, they are immediately declared.
     When an expression is required, all defined variable in the expression
     not yet commited are inlined.
     Variables are also renumbered at the same time. *)
  let simplify_context = HashVector.empty () in
  (* Segment variables should not be renamed (we assume that those are v0-v(num_segments-1)) *)
  let new_variables = Counter.make num_segments in
  let res = ref [] in
  let push_event (d : revent) = res := d :: !res in
  let push_smt loc (d : rsmt) = push_event (Smt (d, loc)) in
  (* Commits the variable i.e output its declaration/definition in the trace,
     and return the new value *)
  let rec commit i =
    match HashVector.get simplify_context i with
    | Declared { ty; loc } ->
        debug "Commiting declared variable %d" i;
        let new_val = if i < num_segments then i else Counter.get new_variables in
        HashVector.set simplify_context i (Processed new_val);
        push_smt loc (DeclareConst (new_val, ty));
        new_val
    | Defined { exp; loc } ->
        debug "Commiting defined variable %d" i;
        let new_val = if i < num_segments then i else Counter.get new_variables in
        HashVector.set simplify_context i (Processed new_val);
        debug "New id is %d" new_val;
        let new_exp = simplify_exp exp in
        debug "New exp is %t" Pp.(top pp_exp new_exp);
        push_smt loc (DefineConst (new_val, new_exp));
        new_val
    | Processed v -> v
  (* Simplify and expression, by inlining all not committed defined variable and
     committing all required declared variables
  *)
  and simplify_exp (e : rexp) : rexp =
    let simplify_var v loc : rexp =
      match HashVector.get simplify_context v with
      | Declared _ ->
          debug "Simplifying declared variable %d" v;
          let new_v = commit v in
          Val (Val_Symbolic new_v, loc)
      | Processed new_v -> Val (Val_Symbolic new_v, loc)
      | Defined { exp; _ } ->
          debug "Simplifying defined variable %d with exp %t" v (Pp.top pp_exp exp);
          simplify_exp exp
    in
    Manip.var_subst simplify_var e
  in
  let rec simplify_valu = function
    | RegVal_Base (Val_Symbolic i) -> RegVal_Base (Val_Symbolic (i |> HashVector.get simplify_context |> expect_processed))
    | valu -> Manip.direct_valu_map_valu simplify_valu valu
  in
  let simplify_event = function
    | Smt (DeclareConst (v, ty), loc) ->
        debug "Declaring v%d with type %t" v (Pp.top pp_ty ty);
        HashVector.set simplify_context v (Declared { ty; loc });
        if HashVector.mem used v then commit v |> ignore
    | Smt (DefineConst (v, exp), loc) ->
        debug "Defining v%d with value %t" v (Pp.top pp_exp exp);
        HashVector.set simplify_context v (Defined { exp; loc });
        if HashVector.mem used v then commit v |> ignore
    | Smt (Assert exp, loc) ->
        let nexp = simplify_exp exp in
        debug "Asserting old %t new %t" (Pp.top pp_exp exp) (Pp.top pp_exp nexp);
        push_event @@ Smt (Assert nexp, loc)
    | event ->
        debug "Handling event %t" (Pp.top pp_event event);
        push_event @@ Manip.direct_event_map_valu simplify_valu event
  in
  List.iter simplify_event events;
  debug "Nearly finished simplify_trc with %d variables, now reversing"
    (1 + Counter.read new_variables);
  Trace (List.rev !res)

let preprocess (config : Server.config) ((segs, trcs) : Server.trcs) : rtrcs =
  let num_segments =
    segs
    |> Option.map (fun (Segments s) -> 
      let x = List.length s in
      assert (List.for_all (fun (Segment (_,_,v)) -> v < x) s);
      x
    )
    |> Option.value ~default:0
  in
  let preprocess_one (b, trc) =
    if not b then None
    else
      let trc = trc |> Manip.remove_init |> Manip.remove_ignored config.ignored_regs in
      let trc = simplify_trc ~num_segments trc in
      Some trc
  in
  let trcs = List.filter_map preprocess_one trcs in
  match segs with
  | None -> Traces (trcs)
  | Some segs -> TracesWithSegments (segs, trcs)
