(*****************************************************************************)
(**    type of DWARF variable info per-instruction (and globals)             *)

(*****************************************************************************)

open AnalyseUtils

type ranged_var =
  (natural * natural * Dwarf.operation list)
  * (Dwarf.sdt_variable_or_formal_parameter * string list)

type ranged_vars_at_instructions = {
  rvai_globals : (Dwarf.sdt_variable_or_formal_parameter * string list) list;
  rvai_params :
    (addr * (string (* function name *) * Dwarf.sdt_variable_or_formal_parameter list)) list;
  rvai_current : ranged_var list array;
  rvai_new : ranged_var list array;
  rvai_old : ranged_var list array;
  rvai_remaining : ranged_var list array;
}
