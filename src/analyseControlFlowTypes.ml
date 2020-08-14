(*****************************************************************************)

(*****************************************************************************)

(** types of control-flow abstraction of instructions, from objdump and branch table data *)
open AnalyseUtils

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
}

type come_from = {
  cf_target_kind : target_kind;
  cf_addr : addr;
  cf_index : index;
  cf_control_flow : control_flow_insn;
  cf_desc : string;
}
