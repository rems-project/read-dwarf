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

(** This module provide the program runner that caches all the information
    required to make a state transition

    Later this module will handle inlining in a transparent way, and
    may also compress basic block traces (We'll need to check this is okay with type inference)

    For now this module load instructions on a per-symbol basis, and do not
    try to load instructions outside of function symbol. TODO: remove this restriction.

    The final goal of this module is to encode all necessary information to perform a
    state transition. In particular, the {!run} function should never require more
    argument: If extra information is required, it should be part of the runner if
    it is dependent on the whole program or in the state if it is specific to the state.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

module Reg = State.Reg

(** Give the instruction descriptor at a given address *)
type slot =
  | Normal of Trace.Instr.t  (** The traces and the size of the instruction *)
  | Special  (** Special instructions. Will be used to represent external events *)
  | Nocode
      (** The is no code at this address. Running it is UB.
          Also used if an address is in between instructions *)
  | IslaFail  (** This means Isla pipeline failed on that instruction *)

type t = {
  elf : Elf.File.t;
  dwarf : Dw.t option;
  instrs : (int, slot) Hashtbl.t;  (** Instruction cache *)
  pc : Reg.t;
  funcs : int Vec.t;  (** Loaded functions by loading order *)
}

let of_elf ?dwarf elf =
  let instrs = Hashtbl.create 100 in
  let funcs = Vec.empty () in
  let pc = Arch.pc () in
  { elf; dwarf; instrs; pc; funcs }

let of_dwarf dwarf = of_elf ~dwarf dwarf.elf

(** Load a symbol into the runner. All instruction traces are fetched and cached.

    TODO support variable length instructions.*)
let load_sym runner (sym : Elf.Symbol.t) =
  info "Loading symbol %s in %s" sym.name runner.elf.filename;
  Vec.add_one runner.funcs sym.addr;
  let opcode_list = BytesSeq.to_listbs ~len:4 sym.data in
  List.iteri
    (fun index code ->
      let addr = sym.addr + (4 * index) in
      try
        let instr = Trace.Cache.get_instr code in
        if instr.traces = [] then begin
          debug "Instruction at 0x%x in %s is loaded as special" addr sym.name;
          Hashtbl.add runner.instrs addr Special
        end
        else begin
          debug "Instruction at 0x%x in %s is loaded as normal. Traces are:\n%t" addr sym.name
            Pp.(topi Trace.Instr.pp instr);
          Hashtbl.add runner.instrs addr (Normal instr)
        end
      with exn ->
        warn "Could not convert isla trace of instruction at 0x%x in %s to Trace.t: %s" addr
          runner.elf.filename (Printexc.to_string exn);
        Hashtbl.add runner.instrs addr IslaFail)
    opcode_list

(** Fetch an instruction, and return corresponding slot. *)
let fetch (runner : t) (pc : int) : slot =
  debug "Fetching PC 0x%x" pc;
  match Hashtbl.find_opt runner.instrs pc with
  | Some v -> v
  | None -> (
      match Elf.SymTable.of_addr_opt runner.elf.symbols pc with
      | Some sym when sym.typ = Elf.Symbol.FUNC ->
          if Hashtbl.mem runner.instrs sym.addr then begin
            warn "Tried to fetch in middle of instructions in %s at 0x%x" runner.elf.filename pc;
            Hashtbl.add runner.instrs pc Nocode;
            Nocode
          end
          else begin
            load_sym runner sym;
            match Hashtbl.find_opt runner.instrs pc with
            | Some v -> v
            | None ->
                warn "Tried to fetch in middle of instructions in %s at 0x%x" runner.elf.filename
                  pc;
                Hashtbl.add runner.instrs pc Nocode;
                Nocode
          end
      | _ ->
          warn "Tried to fetch outside of normal code in %s at 0x%x" runner.elf.filename pc;
          Hashtbl.add runner.instrs pc Nocode;
          Nocode
    )

(** Run the traces on the state.

    If the state is unlocked and the instruction is single trace, then
    the function mutates the state and returns the same state.

    Otherwise the state is locked (if not) and new states are
    returned

    In any case the returned states are unlocked.
*)
let execute_normal ?(prelock = ignore) ~pc runner (instr : Trace.Instr.t) state =
  let dwarf = runner.dwarf in
  let next = instr.length in
  let run_pure () =
    List.map
      (fun (trc : Trace.Instr.trace_meta) ->
        let nstate = State.copy state in
        State.set_last_pc nstate pc;
        Trace.Run.trace_pc_mut ?dwarf ~next nstate trc.trace;
        nstate)
      instr.traces
  in
  if not (State.is_locked state) then begin
    match instr.traces with
    | [trc] ->
        State.set_last_pc state pc;
        Trace.Run.trace_pc_mut ?dwarf ~next state trc.trace;
        [state]
    | _ ->
        prelock state;
        State.lock state;
        run_pure ()
  end
  else run_pure ()

(** Do the whole fetch and execute cycle.
    Take the PC from the state, and fetch it's {{!Instr}instruction} and then run it.
    It return the list of possible behavior of that instruction.
    Normally the union of the set of concrete states represented by this
    list of symbolic state cover all the defined behaviors of the fetched instruction
    from the initial state.

    If the state is unlocked and the instruction is single trace, then
    the function mutates the state and returns the same state.

    Otherwise the state is locked (if not) and new unlocked states are returned.

    In any case the returned states are unlocked.
*)
let run ?prelock runner state : State.t list =
  let pc_exp = State.get_reg_exp state runner.pc in
  try
    let pc = pc_exp |> Ast.expect_bits |> BitVec.to_int in
    match fetch runner pc with
    | Normal instr -> execute_normal ?prelock ~pc runner instr state
    | Special ->
        Raise.fail "Special instruction at 0x%x in %s. unsupported for now" pc runner.elf.filename
    | Nocode -> Raise.fail "Trying to run 0x%x in %s: no code there" pc runner.elf.filename
    | IslaFail ->
        Raise.fail "Trying to run 0x%x in %s: Isla pipeline failed on that instruction" pc
          runner.elf.filename
  with exn ->
    err "Trying to run instruction at %t in %s: Unexpected error"
      Pp.(top State.Exp.pp pc_exp)
      runner.elf.filename;
    Raise.again exn

(** Return the {!Instr.t} data of the instruction at address,
    and throw [Not_found] if the instruction was invalid*)
let expect_normal runner addr =
  match fetch runner addr with Normal instr -> instr | _ -> raise Not_found

(** Return the {!Instr.t} data of the instruction at address,
    and [None] if the instruction was invalid*)
let get_normal_opt runner addr : Trace.Instr.t option =
  match fetch runner addr with Normal instr -> Some instr | _ -> None

(** Pretty prints a instruction slot *)
let pp_slot =
  let open Pp in
  function
  | Normal instr ->
      prefix 4 1 (dprintf "Normal instruction of size %d:" instr.length) (Trace.Instr.pp instr)
  | Special -> !^"Special instruction"
  | Nocode -> !^"Not an instruction"
  | IslaFail -> !^"Isla failed at that PC: investigate"

(** Dump instruction table *)
let pp_instr (runner : t) =
  let open Pp in
  hashtbl_sorted ~name:"Instructions" ~compare ptr pp_slot runner.instrs
