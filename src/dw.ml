(** This module  provides the specifically interpreted DWARF information
    needed for read-dwarf operations

    I would have called this module dwarf but linksem decided that is was a good idea
    to dump all its modules in the global namespace.
*)

module Var = DwVar
module Func = DwFunc

(** The type that represent a elf-dwarf binary whose information has been fully interpreted *)
type t = {
  elf : Elf.File.t;
  ldwarf : Dwarf.dwarf;
  ldwarf_sdt : Dwarf.sdt_dwarf;
  funcs : Func.t list;
  vars : Var.t list;
}

(** Error on Dwarf parsing *)
exception DwarfError of string

let _ =
  Printexc.register_printer (function
    | DwarfError s -> Some (Printf.sprintf "DwarfError: %s" s)
    | _ -> None)

(** Throw an {!DwarfError} *)
let dwarferror fmt = Printf.ksprintf (fun s -> raise (DwarfError s)) fmt

(** Get Dwarf information from an Elf file.

    May raise an {!DwarfError} if a problem occurs.
*)
let of_elf (elf : Elf.File.t) =
  let ldwarf =
    match Dwarf.extract_dwarf elf.linksem with
    | Some d -> d
    | None -> dwarferror "Linksem extract_dwarf failed"
  in
  let ldwarf_sdt = Dwarf.mk_sdt_dwarf ldwarf in
  let process_cu (funcs, vars) (cu : Dwarf.sdt_compilation_unit) =
    let nfuncs = List.rev_map (Func.of_linksem elf) cu.scu_subroutines in
    let nvars = List.rev_map (Var.of_linksem elf) cu.scu_vars in
    (List.rev_append nfuncs funcs, List.rev_append nvars vars)
  in
  let (funcs, vars) = List.fold_left process_cu ([], []) ldwarf_sdt.sd_compilation_units in
  { elf; ldwarf; ldwarf_sdt; funcs; vars }

(** Get Dwarf information from an Elf file by name. Use {!ElfFile.of_file} *)
let of_file (filename : string) = filename |> Elf.File.of_file |> of_elf

(** Pretty print dwarf data as an ocaml structure *)
let pp_raw d =
  PP.(record "dw" [("funcs", list Func.pp_raw d.funcs); ("vars", list Var.pp_raw d.vars)])
