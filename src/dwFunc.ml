(** This module contain all the definition to handle functions
    as defined in the DWARF information of the target file *)

module Var = DwVar

open Logs.Logger (struct
  let str = __MODULE__
end)

(** Type of a dwarf function that may or may not be inlined

    If this type stand on it's own, then it is inlined. If it is inside a {!t}
    then it's a top level function. There is no separate type for inline functions
    because they do not have any special data that a top level function may not have *)
type func = { name : string; scope : scope; ret : Ctype.t option }

(** Type of a dwarf scope that may contain recursively other data.
    The lists here have the semantic meaning of sets: the order is irrelevant. *)
and scope = { vars : Var.t list; funcs : func list; scopes : scope list }

(** This is the type a dwarf function in linksem *)
type linksem_func = Dwarf.sdt_subroutine

(*PS: why introduce these aliases? *)

(** This is the type a dwarf scope in linksem *)
type linksem_scope = Dwarf.sdt_lexical_block

(** Create a Dwarf function from its linksem counterpart *)
let rec func_of_linksem (elf : Elf.File.t) (env : Ctype.env) (lfun : linksem_func) =
  let name = Option.value ~default:"" lfun.ss_name in
  let scope_of_linksem_func (lfun : linksem_func) =
    let vars = List.rev_map (Var.of_linksem elf env) lfun.ss_vars in
    let funcs = List.rev_map (func_of_linksem elf env) lfun.ss_subroutines in
    let scopes = List.rev_map (scope_of_linksem elf env) lfun.ss_lexical_blocks in
    { vars; funcs; scopes }
  in
  let scope = scope_of_linksem_func lfun in
  let ret = Option.map (Ctype.of_linksem ~env) lfun.ss_type in
  { name; scope; ret }

(** Create and Dwarf scope from its linksem counterpart *)
and scope_of_linksem (elf : Elf.File.t) (tenv : Ctype.env) (lsc : linksem_scope) =
  let vars = List.rev_map (Var.of_linksem elf tenv) lsc.slb_vars in
  let funcs = List.rev_map (func_of_linksem elf tenv) lsc.slb_subroutines in
  let scopes = List.rev_map (scope_of_linksem elf tenv) lsc.slb_lexical_blocks in
  { vars; funcs; scopes }

(** Get the API of the function *)
let func_get_api func : Arch.func_api =
  let args =
    List.filter_map (fun (v : Var.t) -> if v.param then Some v.ctype else None) func.scope.vars
  in
  let ret = func.ret in
  { args; ret }

let rec pp_raw_func f : PP.document =
  let open PP in
  record "func"
    [
      ("name", string f.name);
      ("ret", Option.fold ~none:!^"none" ~some:Ctype.pp f.ret);
      ("scope", pp_raw_scope f.scope);
    ]

and pp_raw_scope s =
  let open PP in
  record "scope"
    [
      ("vars", list Var.pp_raw s.vars);
      ("funcs", list pp_raw_func s.funcs);
      ("scopes", list pp_raw_scope s.scopes);
    ]

(** This the type of a top-level function. It may have an associated elf symbol

    This type will contain all necessary indexes for function-wide fast access
    to relevant dwarf information.
*)
type t = { sym : Elf.Sym.t option; func : func }

(** This is the type a dwarf top_level function in linksem *)
type linksem_t = Dwarf.sdt_subroutine

(*PS another gratuitous alias? *)

(** Create a dwarf top level function from its Linksem counterpart. The ELF file is to get a
    potential matching symbol. For now the matching is made with name only, but in the future
    code addresses may also be used to be more resilient *)
let of_linksem (elf : Elf.File.t) (tenv : Ctype.env) (lfun : linksem_t) =
  let func = func_of_linksem elf tenv lfun in
  let sym =
    (* try finding an identically named ELF symbol; if that fails, try finding an ELF symbol with the same address*)
    match Elf.SymTbl.of_name_opt elf.symbols func.name with
    | Some sym -> Some sym
    | None -> (
        match lfun.ss_entry_address with
        | Some a -> (
            match Elf.SymTbl.of_addr_opt elf.symbols (Nat_big_num.to_int a) with
            | Some sym -> Some sym
            | None -> None
          )
        | None -> None
      )
  in
  { sym; func }

(** Get the API of a top level function *)
let get_api tf = func_get_api tf.func

(** Pretty print a raw top level function *)
and pp_raw f =
  PP.(record "tfunc" [("elf", opt Elf.Sym.pp_raw f.sym); ("func", pp_raw_func f.func)])
