(*****************************************************************************)
(** type of collected test analysis                                          *)

(*****************************************************************************)

open Utils
open DwarfLineInfo

(*open DwarfFrameInfo*)
open ControlFlowTypes
open RangedVarType

type analysis = {
  index_of_address : addr -> int;
  index_option_of_address : addr -> int option;
  address_of_index : int -> addr;
  instructions : instruction array;
  line_info : evaluated_line_info_for_instruction list array;
  elf_symbols : string list array;
  (*  objdump_lines : (addr (*address*) * natural (*insn/data*) * string) option array;*)
  frame_info :
    (addr (*addr*) * string (*cfa*) * (string (*rname*) * string) (*rinfo*) list) option array;
  indirect_branches : instruction list;
  come_froms : come_from list array;
  sdt : Dwarf.sdt_dwarf;
  ranged_vars_at_instructions : ranged_vars_at_instructions;
  inlining :
    ( string (*ppd_labels*)
    * string (*new inlining*)
    * (int (*label*) * Dwarf.inlined_subroutine_data_by_range_entry) list
    )
    array;
  pp_inlining_label_prefix : string -> string;
  rendered_control_flow : string array;
  rendered_control_flow_inbetweens : string array;
  rendered_control_flow_width : int;
}
