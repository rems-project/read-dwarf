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

(*****************************************************************************)

(*****************************************************************************)

(** types of control-flow abstraction of instructions, from objdump and branch table data *)
open Utils

type control_flow_insn =
  | C_no_instruction
  | C_plain
  | C_ret
  | C_eret
  | C_branch of addr (*numeric addr*) * string (*symbolic addr*)
  | C_branch_and_link of addr (*numeric addr*) * string (*symbolic addr*)
  | C_branch_cond of string (*mnemonic*) * addr (*numeric addr*) * string (*symbolic addr*)
  | C_branch_register of string (*argument*)
  | C_smc_hvc of string

(*mnemonic*)

type target_kind =
  | T_plain_successor
  | T_branch
  | T_branch_and_link_call
  | T_branch_and_link_call_noreturn
  | T_branch_and_link_successor
  | T_branch_cond_branch
  | T_branch_cond_successor
  | T_branch_register
  | T_smc_hvc_successor
  | T_out_of_range of addr

type target = target_kind * addr * index * string

type instruction = {
  i_addr : addr;
  i_opcode : int list;
  i_mnemonic : string;
  i_operands : string;
  i_control_flow : control_flow_insn;
  i_targets : target list;
  i_relocation : (string * string) option;
}

type come_from = {
  cf_target_kind : target_kind;
  cf_addr : addr;
  cf_index : index;
  cf_control_flow : control_flow_insn;
  cf_desc : string;
}

type weight = L | B

type glyph =
  | Glr of weight
  | Gud of weight
  | Gru of weight
  | Grd of weight
  | Grud of weight
  | Glrud of weight * weight
  | Ggt
  | Glt
  | GX
  | Gnone
  | Gquery
