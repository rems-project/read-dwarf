(** This module contain all the definition to handle functions
    as defined in the DWARF information of the target file *)

module Var = DwVar

(** Type of a dwarf function that may or may not be inlined

    If this type stand on it's own, then it is inlined. If it is inside a {!t}
    then it's a top level function. There is no separate type for inline functions
    because they do not have any special data that a top level function may not have *)
type func = { name : string; scope : scope }

(** Type of a dwarf scope that may contain recursively other data.
    The lists here have the semantic meaning of sets: the order is irrelevant. *)
and scope = { vars : Var.t list; funcs : func list; scopes : scope list }

(** This is the type a dwarf function in linksem *)
type linksem_func = Dwarf.sdt_subroutine

(** This is the type a dwarf scope in linksem *)
type linksem_scope = Dwarf.sdt_lexical_block

(** Create and Dwarf function from it linksem counterpart *)
let rec func_of_linksem (elf : Elf.File.t) (lfun : linksem_func) =
  let name = Option.value ~default:"" lfun.ss_name in
  let scope_of_linksem_func (lfun : linksem_func) =
    let vars = List.rev_map (Var.of_linksem elf) lfun.ss_vars in
    let funcs = List.rev_map (func_of_linksem elf) lfun.ss_subroutines in
    let scopes = List.rev_map (scope_of_linksem elf) lfun.ss_lexical_blocks in
    { vars; funcs; scopes }
  in
  let scope = scope_of_linksem_func lfun in
  { name; scope }

(** Create and Dwarf scope from it linksem counterpart *)
and scope_of_linksem (elf : Elf.File.t) (lsc : linksem_scope) =
  let vars = List.rev_map (Var.of_linksem elf) lsc.slb_vars in
  let funcs = List.rev_map (func_of_linksem elf) lsc.slb_subroutines in
  let scopes = List.rev_map (scope_of_linksem elf) lsc.slb_lexical_blocks in
  { vars; funcs; scopes }

let rec pp_raw_func f : PP.document =
  PP.(record "func" [("name", string f.name); ("scope", pp_raw_scope f.scope)])

and pp_raw_scope s =
  PP.(
    record "scope"
      [
        ("vars", list Var.pp_raw s.vars);
        ("funcs", list pp_raw_func s.funcs);
        ("scopes", list pp_raw_scope s.scopes);
      ])

(** This the type of a top-level function. It may have an associated elf symbol

    This type will contain all necessary indexes for function-wide fast access
    to relevant dwarf information.
*)
type t = { sym : Elf.Sym.t option; func : func }

(** This is the type a dwarf top_level function in linksem *)
type linksem_t = Dwarf.sdt_subroutine

(** Create a dwarf top level function from its Linksem counterpart. The ELF file is to get a
    potential matching symbol. For now the matching is made with name only, but in the future
    code addresses may also be used to be more resilient *)
let of_linksem (elf : Elf.File.t) (lfun : linksem_t) =
  let func = func_of_linksem elf lfun in
  let sym = Elf.SymTbl.of_name_opt elf.symbols func.name in
  { sym; func }

(** Pretty print a raw top level function *)
and pp_raw f =
  PP.(record "tfunc" [("elf", opt Elf.Sym.pp_raw f.sym); ("func", pp_raw_func f.func)])
