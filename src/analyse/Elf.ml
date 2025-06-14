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

open Utils
open ElfTypes

open Logs.Logger (struct
  let str = __MODULE__
end)

(*****************************************************************************)
(**       pp symbol map                                                      *)

(*****************************************************************************)

let pp_symbol_map (symbol_map : Elf_file.global_symbol_init_info) =
  String.concat ""
    (List.map
       (fun (name, (typ, _size, address, _mb, _binding)) ->
         Printf.sprintf "**** name = %s  address = %s  typ = %d\n" name (pp_addr (Sym_ocaml.Num.Absolute address))
           (Sym.to_int (Sym_ocaml.Num.Absolute typ)))
       symbol_map)

(*****************************************************************************)
(**       use linksem to parse ELF file and extract DWARF info               *)

(*****************************************************************************)

let parse_elf_file (filename : string) : test =
  (* call ELF analyser on file *)
  let bs = match Byte_sequence.acquire filename with
    | Error.Fail s -> fatal "Linksem: Byte_sequence.acquire: %s" s
    | Error.Success x -> x
  in
  let f64 = match Elf_file.read_elf64_file bs with
    | Error.Fail s -> fatal "Linksem: read_elf64_file: %s" s
    | Error.Success x -> x
  in
  let symbol_map = match Symbols.get_elf64_file_global_symbol_init f64 with
    | Error.Fail s -> fatal "LinksemRelocatable: get_elf64_file_global_symbol_init: %s" s
    | Error.Success x -> x
  in

  let elf_file = Elf_file.ELF_File_64 f64 in

  let entry = f64.elf64_file_header.elf64_entry in
  let machine = f64.elf64_file_header.elf64_machine in
  (* linksem main_elf --symbols looks ok for gcc and clang

     That uses                 Elf_file.read_elf64_file bs0 >>= fun f1 ->
                return (Harness_interface.harness_string_of_elf64_syms
  *)

  (*
  let pp_string_table strtab =
    match strtab with String_table.Strings(c,s) ->
      String.map (function c' -> if c'=c then ' ' else c') s
  in

  (*
  (* check the underlying string table - looks right for clang and gcc*)
  let string_table :String_table.string_table =
    match Elf_file.get_elf64_file_symbol_string_table f64 with
    | Error.Success x -> x
    | Error.Fail s -> raise (Failure ("foo "^s))
  in
  Printf.printf "%s\n" (pp_string_table string_table);
  exit 0;
*)

  (* check the symbol table - plausible looking "Name" offsets for both gcc and clang *)

  (match Elf_file.get_elf64_file_symbol_table f64 with
  | Error.Success (symtab,strtab) ->
       Printf.printf "%s\n%s" (pp_string_table strtab)
         (Elf_symbol_table.string_of_elf64_symbol_table symtab)
  | Error.Fail s -> raise (Failure "foo"));




  (* check the symbol_map - right number of entries, and strings for gcc,
     but no strings for clang... *)
  Printf.printf "symbol_map=\n%s"  (pp_symbol_map symbol_map);
  (* Printf.printf "%s\n" (Sail_interface.string_of_executable_process_image elf_epi);*)
(*  exit 0;*)
 *)

  (*  Debug.print_string "elf segments etc\n";*)
    (* architectures from linksem elf_header.lem *)
    let arch =
      if f64.elf64_file_header.elf64_machine = Elf_header.elf_ma_aarch64 then AArch64
      else if f64.elf64_file_header.elf64_machine = Elf_header.elf_ma_x86_64 then X86
      else fatal "unrecognised ELF file architecture"
    in

    let ds =
      match Dwarf.extract_dwarf_static (Elf_file.ELF_File_64 f64) Abi_aarch64_symbolic_relocation.aarch64_data_relocation_interpreter with
      | None -> fatal "%s" "extract_dwarf_static failed"
      | Some ds ->
          (* Debug.print_string2 (Dwarf.pp_analysed_location_data ds.Dwarf.ds_dwarf
                                    ds.Dwarf.ds_analysed_location_data);
              Debug.print_string2 (Dwarf.pp_evaluated_frame_info
                                    ds.Dwarf.ds_evaluated_frame_info);*)
          ds
    in
    let dwarf_semi_pp_frame_info =
      Dwarf.semi_pp_evaluated_frame_info ds.ds_evaluated_frame_info
    in
    let test =
      {
        elf_file;
        arch;
        symbol_map (*@ (symbols_for_stacks !Globals.elf_threads)*);
        e_entry = Sym_ocaml.Num.Absolute (entry);
        e_machine = Sym_ocaml.Num.Absolute (machine);
        dwarf_static = ds;
        dwarf_semi_pp_frame_info;
      }
    in
    test

(*****************************************************************************)
(**       marshal and unmarshal test                                         *)

(*****************************************************************************)

let marshal_to_file filename test =
  let c = open_out filename in
  Marshal.to_channel c test [];
  close_out c

let marshal_from_file filename : test option =
  try
    let c = open_in filename in
    let test = Marshal.from_channel c in
    close_in c;
    Some test
  with
  | Sys_error _ -> None
  | e -> raise e
