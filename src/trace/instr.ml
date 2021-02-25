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

(** This module provide the representation of an instruction.
    It only contain generic information about the opcode and not specific
    information about a place in the code.*)

module Reg = State.Reg

(** A simple trace with it's metadata *)
type trace_meta = { trace : Base.t; jump : bool; footprint : Reg.t list }

(** A full instruction representation *)
type t = { traces : trace_meta list; length : int;  (** Bytes length *) footprint : Reg.t list }

(** Helper to access the [footprint] field *)
let footprint t = t.footprint

(** Compute the metadata of trace *)
let trace_meta_of_trace trace =
  let pc = Arch.pc () in
  let footprint = ref [] in
  let jump = ref false in
  let process_reg reg = footprint := reg :: !footprint in
  let process_var = function Base.Var.Register reg -> process_reg reg | _ -> () in
  let process_exp : Base.exp -> unit = Ast.Manip.exp_iter_var process_var in
  let process_event : Base.event -> unit = function
    | WriteReg { reg; value } ->
        process_reg reg;
        process_exp value;
        if reg = pc then jump := true
    | ReadMem { addr; _ } -> process_exp addr
    | WriteMem { addr; value; _ } ->
        process_exp addr;
        process_exp value
    | Assert exp -> process_exp exp
  in
  List.iter process_event trace;
  { trace; jump = !jump; footprint = List.sort_uniq compare !footprint }

(** Generate an instruction data from a list of traces *)
let of_traces traces =
  let traces = List.map trace_meta_of_trace traces in
  let length = 4 (* TODO *) in
  let footprint =
    List.fold_left (fun l (tr : trace_meta) -> List.merge_uniq compare l tr.footprint) [] traces
  in
  { traces; length; footprint }

(** Pretty print the representation of an instruction *)
let pp instr =
  let open Pp in
  separate_mapi hardline
    (fun i trc -> prefix 4 1 (dprintf "Trace %d:" i) (Base.pp trc.trace))
    instr.traces
