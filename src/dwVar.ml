(** This module contain all the definition to handle local and global variables
    as defined in the DWARF information of the target file *)

(** Type of a DWARF variable *)
type t = { name : string; param : bool }

(** Type of a DWARF variable in linksem *)
type linksem_t = Dwarf.sdt_variable_or_formal_parameter

(** Create a DWARF variable from it's linksem counterpart *)
let of_linksem (lvar : linksem_t) =
  let name = lvar.svfp_name in
  let param = match lvar.svfp_kind with SVPK_var -> false | SVPK_param -> true in
  { name; param }

let pp_raw v =
  let kind = if v.param then "param" else "var" in
  PP.(record kind [("name", string v.name)])
