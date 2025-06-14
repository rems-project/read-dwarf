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

(** This module provides a caching system for fully processed traces

    The top level function to get traces from an opcode is {!get_traces}.
    This is the function called by the {!Run.Runner}.*)

open Logs.Logger (struct
  let str = __MODULE__
end)

open Fun
module Opcode = Isla.Cache.Opcode
module Epoch = Isla.Cache.Epoch

(** Store element of type {!Base.t}[list] on disk.

    First a hashmap from register numbers to path and type is stored.
    Then the actual tracelist is marshaled.

    When loading the register numbering may be different, and some
    may not exist. So we create all missing register, then
    we replace the old number with the new ones.
*)
module Traces = struct
  open Base

  type t = Base.t list

  module Reg = State.Reg

  type regs = (Reg.t, Reg.Path.t * Reg.ty) Hashtbl.t

  let to_file file (traces : t) =
    let writer channel traces =
      let regs : regs = Hashtbl.create 5 in
      let add_reg reg = Hashtbl.add regs reg (Reg.to_path reg, Reg.reg_type reg) in
      let add_regs_exp =
        Ast.Manip.exp_iter_var (function Var.Register reg -> add_reg reg | _ -> ())
      in
      let add_regs = function
        | WriteReg { reg; value } ->
            add_reg reg;
            add_regs_exp value
        | ReadMem { addr; _ } -> add_regs_exp addr
        | WriteMem { addr; value; _ } ->
            add_regs_exp addr;
            add_regs_exp value
        | Assert exp -> add_regs_exp exp
      in
      List.iter (List.iter add_regs) traces;
      Marshal.to_channel channel regs [Marshal.No_sharing];
      Marshal.to_channel channel traces [Marshal.No_sharing]
    in
    Files.write_bin writer file traces

  let of_file file : t =
    let reader channel =
      let new_regs = Hashtbl.create 10 in
      let regs : regs = Marshal.from_channel channel in
      Hashtbl.iter
        (fun old_reg (path, ty) ->
          debug "Adding %s with type %t" (Reg.Path.to_string path) Pp.(top Reg.pp_ty ty);
          Hashtbl.add new_regs old_reg (Reg.ensure_add path ty))
        regs;
      (* HACK: Here we are breaking the invariant of Reg.t.
         The values of type Reg.t generated by Marshal are invalid values.
         However doing it in a type safe would require have a separate
         type of traces with plain integer instead of Reg.t which would
         be useless complicated *)
      let old_traces : t = Marshal.from_channel channel in
      let update_reg = Hashtbl.find new_regs in
      let update_exp =
        Ast.Manip.exp_map_var (function
          | Var.Register reg -> Var.Register (update_reg reg)
          | v -> v)
      in
      let update_event = function
        | WriteReg { reg; value } -> WriteReg { reg = update_reg reg; value = update_exp value }
        | ReadMem rm -> ReadMem { rm with addr = update_exp rm.addr }
        | WriteMem wm ->
            WriteMem { wm with addr = update_exp wm.addr; value = update_exp wm.value }
        | Assert exp -> Assert (update_exp exp)
      in
      let traces = List.map (List.map update_event) old_traces in
      traces
    in
    Files.read_bin reader file
end

module TC = Utils.Cache.Make (Opcode) (Traces) (Epoch)

let cache : TC.t option ref = ref None

type config = Isla.Cache.config

(** Start the caching system. Start {!Isla.Cache} too *)
let start (config : Isla.Cache.config) =
  Isla.Cache.start config;
  cache := Some (TC.make "traces" (Epoch.of_config config))

(** Stop the caching system *)
let stop () =
  begin
    match !cache with
    | Some _ -> Isla.Cache.stop ()
    | None -> ()
  end;
  cache := None

(** Get the cache and fails if the cache wasn't started *)
let get_cache () =
  match !cache with Some cache -> cache | None -> failwith "Trace cache was not started"

(** Get the traces of the opcode given. Use {!Isla.Server} if the value is not in the cache *)
let get_traces (opcode : Isla.Server.opcode) : Base.t list =
  let cache = get_cache () in
  match TC.get_opt cache (Some opcode) with
  | Some trcs -> trcs
  | None ->
      let segments, isla_traces = match Isla.Cache.get_traces opcode with
      | Traces t -> [], t
      | TracesWithSegments (Segments s, t) -> s, t
      in 
      let traces = List.map (tee (Isla.Type.type_trc %> ignore) %> Base.of_isla segments) isla_traces in
      let straces = List.map Base.simplify traces in
      TC.add cache (Some opcode) straces;
      straces

(** Get a full blown {!Instr} from the opcode, going through the whole Isla pipeline
    if necessary.*)
let get_instr (opcode : BytesSeq.t * Elf.Relocations.rel option) : Instr.t =
  let raw_opcode, reloc = opcode in
  let reloc_target = Option.map (fun (x : Elf.Relocations.rel) -> x.target) reloc in
  Instr.of_traces opcode @@ get_traces (raw_opcode, reloc_target)
