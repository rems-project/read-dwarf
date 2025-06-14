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

(** This module define all architecture-dependent configuration

    It should be used instead of {!Arch} inside the architecture dependent modules.

    Everything inside this module is copied into {!Arch}, so module that can depend on
    {!Arch} may do so.
*)

(** Describe the C API of a function *)
type func_api = { args : Ctype.t list; ret : Ctype.t option }

(** Describe the ABI of a function

    This is a record because I expect to add many other fields later.
*)
type func_abi = {
  init : State.t -> State.t;
      (** Gives the initial state for verifying the function, from a given global
          register state. Only global registers are kept. *)
}

(** The map of dwarf register: Which register number map to which ISA register *)
type dwarf_reg_map = State.Reg.t array

(** Tells if this Arch module supports this architecture *)
val supports : Config.Arch.t -> bool

(** If this arch module {!supports} the architecture, then initialize read-dwarf
  state using this architecture *)
val init : Config.Arch.t -> unit

(** Return [Some(arch)] is the loaded arch is [arch] and [None] if nothing is loaded yet. *)
val initialized : unit -> Config.Arch.t option

(** The name of the arch module. Must be the name of the module i.e. [Config.arch_module] *)
val module_name : string

(** For dynamic arch module, the name of the dynamically loaded module.
  Otherwise {!module_name} *)
val loaded_name : string

(** The true size of addresses for memory operation *)
val address_size : int

(** Get the register map of the architecture *)
val dwarf_reg_map : unit -> dwarf_reg_map

(** Tell if a register is local for the ABI *)
val is_local : State.Reg.t -> bool

(** Give the opcode of the nop instruction (For Sail/Isla initialisation *)
val nop : unit -> BytesSeq.t

(** Give the ABI of a function from it's C API *)
val get_abi : func_api -> func_abi

(** Give the register index for the program counter *)
val pc : unit -> State.Reg.t

(** Give the register index for the stack pointer *)
val sp : unit -> State.Reg.t

(** Take an instruction string and give the name of an temporary ELF file created
  that contains the instruction at symbol instr.*)
val assemble_to_elf : string -> string

(**  Split a byte-sequence into a list of instructions. *)
val split_into_instrs : Elf.Symbol.data -> Elf.Symbol.data list

(** Tell if an instruction is a return instruction. *)
val is_ret : BytesSeq.t -> bool

(** Tell if an instruction is a compare instruction.
    Returns [Some (reg,bv)] where the contents of [reg] are compared against the
    value [bv] if it is and [None] if not. *)
val is_cmp : BytesSeq.t -> (State.Reg.t * BitVec.t) option

(** Tell if an instruction is an (unconditional) branch on immediate with
    link instructions.  Returns [Some bv] where [bv] is the offset (from the
    address of this instruction, in the range +/-128MB) that is branched to
    if it is and [None] if not. *)
val is_bl : BytesSeq.t -> BitVec.t option
