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
    It only contains generic information about the opcode and not specific
    information about a place in the code.*)

module Reg = State.Reg

(** A simple trace with its metadata

    If there is a [WriteReg {reg;value}] event in [trace] where [reg = Arch.pc ()],
    [jump = Some value], otherwise it is [None]. If there are multiple such events,
    it will store the value of the last one. *)
type trace_meta = {
  trace : Base.t;
  jump_target : Base.exp option;
  read : Reg.t list;
  written : Reg.t list;
}

module SMap = Map.Make (String)

(** A full instruction representation *)
type t = {
  traces : trace_meta list;
  length : int;  (** Bytes length *)
  read : Reg.t list;
  written : Reg.t list;
  opcode : BytesSeq.t;
  relocation : Elf.Relocations.rel option;
}

let dedup_regs = List.sort_uniq State.Reg.compare

let footprint x = dedup_regs @@ x.read @ x.written

(** Compute the metadata of trace *)
let trace_meta_of_trace trace =
  let pc = Arch.pc () in
  let read = ref [] in
  let written = ref [] in
  let jump = ref None in
  let process_var = function
    | Base.Var.Register reg -> read := reg :: !read
    | Base.Var.(Read _ | NonDet _ | Segment _) -> ()
  in
  let process_exp : Base.exp -> unit = Ast.Manip.exp_iter_var process_var in
  let process_event : Base.event -> unit = function
    | WriteReg { reg; value } ->
        written := reg :: !written;
        process_exp value;
        if reg = pc then jump := Some value
    | ReadMem { addr; value = _; size = _ } -> process_exp addr
    | WriteMem { addr; value; size = _ } ->
        process_exp addr;
        process_exp value
    | Assert exp -> process_exp exp
  in
  List.iter process_event trace;
  { trace; jump_target = !jump; read = dedup_regs !read; written = dedup_regs !written }

(** Generate full instruction data from a list of traces *)
let of_traces ((opcode: BytesSeq.t), (relocation: Elf.Relocations.rel option)) traces =
  let traces = List.map trace_meta_of_trace traces in
  let length = BytesSeq.length opcode in
  let read = dedup_regs @@ List.concat_map (fun (tr : trace_meta) -> tr.read) traces in
  let written = dedup_regs @@ List.concat_map (fun (tr : trace_meta) -> tr.written) traces in

  { traces; length; read; written; opcode; relocation }

(** Pretty print the representation of an instruction *)
let pp instr =
  let open Pp in
  !^"Relocation" ^^ optional Elf.Relocations.pp_rel instr.relocation ^^ hardline ^^
  separate_mapi hardline
    (fun i trc -> prefix 4 1 (dprintf "Trace %d:" i) (Base.pp trc.trace))
    instr.traces
