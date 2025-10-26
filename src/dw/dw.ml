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

(** This module  provides the specifically interpreted DWARF information
    needed for read-dwarf operations

    I would have called this module dwarf but linksem decided that is was a good idea
    to dump all its modules in the global namespace.*)

module Var = Var
module Func = Func
module Loc = Loc

open Logs.Logger (struct
  let str = __MODULE__
end)

(** The type that represent a elf-dwarf binary whose information has been fully interpreted *)
type t = {
  elf : Elf.File.t;
  ldwarf : Dwarf.dwarf;
  ldwarf_sdt : Dwarf.sdt_dwarf;
  funcs : (string, Func.t) Hashtbl.t;
  vars : (string, Var.t) Hashtbl.t;
  tenv : Ctype.env;
}

(** Error on Dwarf parsing *)
exception DwarfError of string

let _ =
  Printexc.register_printer (function
    | DwarfError s -> Some (Printf.sprintf "DwarfError: %s" s)
    | _ -> None)

(** Throw a {!DwarfError} *)
let dwarferror fmt = Printf.ksprintf (fun s -> raise (DwarfError s)) fmt

(** Get Dwarf information from an Elf file.

    May raise a {!DwarfError} if a problem occurs.*)
let of_elf (elf : Elf.File.t) =
  info "Loading architecture %s for %s" (Elf.File.machine_to_string elf.machine) elf.filename;
  Arch.load_elf_arch elf;
  info "Extracting dwarf of %s" elf.filename;
  let ldwarf =
    match Dwarf.extract_dwarf elf.linksem Abi_aarch64_symbolic_relocation.aarch64_data_relocation_interpreter with
    | Some d -> d
    | None -> dwarferror "Linksem extract_dwarf failed"
  in
  info "Processed base DWARF";
  let ldwarf_sdt : Dwarf.sdt_dwarf =
    Dwarf.mk_sdt_dwarf ldwarf (Dwarf.subprogram_line_extents ldwarf)
  in
  info "Processed sdt DWARF";
  debug "Linksem DWARF type environement %t" (fun o ->
      output_string o @@ Dwarf.pp_all_struct_union_enum_types' ldwarf);
  let ltenv = Dwarf.struct_union_enum_types ldwarf in
  let tenv = Ctype.env_of_linksem ltenv in
  info "Processed type environement";
  let process_cu (funcs, vars) (cu : Dwarf.sdt_compilation_unit) =
    let nfuncs = List.rev_map (Func.of_linksem elf tenv) cu.scu_subroutines in
    let nvars = List.rev_map (Var.of_linksem elf tenv) cu.scu_vars in
    (List.rev_append nfuncs funcs, List.rev_append nvars vars)
  in
  let (funcs, vars) = List.fold_left process_cu ([], []) ldwarf_sdt.sd_compilation_units in
  let funcs =
    funcs |> List.to_seq
    |> Seq.map (fun (func : Func.t) -> (func.func.name, func))
    |> Hashtbl.of_seq
  in
  let vars =
    vars |> List.to_seq |> Seq.map (fun (var : Var.t) -> (var.name, var)) |> Hashtbl.of_seq
  in
  { elf; ldwarf; ldwarf_sdt; funcs; vars; tenv }

(** Get Dwarf information from an Elf file by name. Use {!Elf.File.of_file} *)
let of_file (filename : string) = filename |> Elf.File.of_file |> of_elf

(** Get a function by [name] *)
let get_func_opt ~name dw = Hashtbl.find_opt dw.funcs name

(** Pretty print dwarf data as an ocaml structure *)
let pp_raw d =
  Pp.(
    record "dw"
      [
        ("funcs", hashtbl string Func.pp_raw d.funcs);
        ("vars", hashtbl string Var.pp_raw d.vars);
        ("types", Ctype.pp_env d.tenv);
      ])
