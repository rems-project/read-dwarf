(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Read-dwarf, located in the src/ and test_asm/ directories, is subject to this   *)
(*  BSD 2-Clause License. This license does not apply to files outside these        *)
(*  directories.                                                                    *)
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

(*****************************************************************************)
(**       collect the various test analysis data                             *)

(*****************************************************************************)
(*open Printf*)
(*open Types*)

open Utils

(*open SdtUtils*)
open ElfTypes

(*open ControlFlowTypes*)
open CollectedType
open DwarfLineInfo
open ElfSymbols
open DwarfFrameInfo
open ControlFlow
open ComeFrom

(*open RangedVarType*)

(*open QemuLog*)

(*open ControlFlowPpDot*)
open ControlFlowPpText

(*open CallGraph*)

open DwarfVarInfo
open DwarfInliningInfo

let mk_analysis test filename_objdump_d filename_branch_table_option =
  (* compute the basic control-flow data *)
  let (instructions, index_of_address, index_option_of_address, address_of_index) =
    time "mk_instructions" (mk_instructions test filename_objdump_d) filename_branch_table_option
  in

  let line_info =
    time "mk_line_info" (mk_line_info test.dwarf_static.ds_evaluated_line_info) instructions
  in

  let indirect_branches = time "mk_indirect_branches" mk_indirect_branches instructions in

  let come_froms = time "mk_come_froms" mk_come_froms instructions in

  let sdt =
    time "mk_sdt_dwarf"
      (Dwarf.mk_sdt_dwarf test.dwarf_static.ds_dwarf)
      test.dwarf_static.ds_subprogram_line_extents
  in

  let ranged_vars_at_instructions =
    time "mk_ranged_vars_at_instructions" (mk_ranged_vars_at_instructions sdt) instructions
  in

  let elf_symbols = time "mk_elf_symbols" (mk_elf_symbols test) instructions in

  let frame_info = time "mk_frame_info" (mk_frame_info test) instructions in

  (*Printf.printf  "%s" (pp_indirect_branches indirect_branches); flush stdout;*)
  let (inlining, pp_inlining_label_prefix) =
    time "mk_inlining" (mk_inlining test sdt) instructions
  in

  let acf_width = 60 in
  let max_branch_distance = None (* Some instruction_count, or None for unlimited *) in
  let (rendered_control_flow, rendered_control_flow_inbetweens, rendered_control_flow_width) =
    time "render_ascii_control_flow"
      (render_ascii_control_flow max_branch_distance acf_width)
      instructions
  in

  let an =
    {
      index_of_address;
      index_option_of_address;
      address_of_index;
      instructions;
      line_info;
      elf_symbols;
      (*      objdump_lines;*)
      frame_info;
      indirect_branches;
      come_froms;
      sdt;
      ranged_vars_at_instructions;
      inlining;
      pp_inlining_label_prefix;
      rendered_control_flow;
      rendered_control_flow_inbetweens;
      rendered_control_flow_width;
    }
  in

  an
