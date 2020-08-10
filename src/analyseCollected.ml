(*****************************************************************************)
(**       collect the various test analysis data                             *)

(*****************************************************************************)
(*open Printf*)
(*open Types*)

open AnalyseUtils

(*open AnalyseSdtUtils*)
open AnalyseElfTypes

(*open AnalyseControlFlowTypes*)
open AnalyseCollectedType
open AnalyseDwarfLineInfo
open AnalyseElfSymbols
open AnalyseDwarfFrameInfo
open AnalyseControlFlow
open AnalyseComeFrom

(*open AnalyseRangedVarType*)

(*open AnalyseQemuLog*)

(*open AnalyseControlFlowPpDot*)
open AnalyseControlFlowPpText

(*open AnalyseCallGraph*)

open AnalyseDwarfVarInfo
open AnalyseDwarfInliningInfo

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
