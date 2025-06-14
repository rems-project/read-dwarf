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

(** This module represent an ELF 64 file. We do not deal with 32 bit ELF files for now
    The main interesting information is the symbol table.
*)

open Logs.Logger (struct
  let str = __MODULE__
end)

module SymTbl = SymTable

(** The machine type of the ELF file. It can be a known architecture, or one
    that is not in {!Arch} *)
type machine = Supp of Config.Arch.t | Other of int

(** Convert the linksem machine number to {!machine} *)
let machine_of_linksem lmachine =
  if lmachine = Elf_header.elf_ma_386 then Supp X86
  else if lmachine = Elf_header.elf_ma_x86_64 then Supp X86_64
  else if lmachine = Elf_header.elf_ma_arm then Supp ARM
  else if lmachine = Elf_header.elf_ma_aarch64 then Supp AARCH64
  else if lmachine = Elf_header.elf_ma_ppc then Supp PpC
  else if lmachine = Elf_header.elf_ma_ppc64 then Supp PpC64
  else if lmachine = Elf_header.elf_ma_riscv then Supp RISCV64
  else Other (Z.to_int lmachine)

(** Convert a {!machine} to a human readable string *)
let machine_to_string = function
  | Supp a -> Config.Arch.to_string a
  | Other i -> Printf.sprintf "Other(%d)" i

(** Pretty prints a {!machine} *)
let pp_machine mach = mach |> machine_to_string |> Pp.string

module SMap = Map.Make(String)

type section = {
  name : string;
  size : int;
  align : int;
}

(** The type containing all the information about an ELF file *)
type t = {
  filename : string;  (** The name on the file system. Useful for error messages *)
  symbols : SymTbl.t;  (** The symbol table *)
  entry : int;  (** The address of the entry point; only used in [dumpSym.ml] *)
  machine : machine;
      (** The target architecture of the file; only used in [arch.ml, dumpSym.ml, dw.ml] *)
  linksem : Elf_file.elf_file;
      (** The original linksem structure for the file; only used in  [dw.ml] *)
  rodata : Segment.t SMap.t;  (** The read-only data sections *)
  sections : section list;
}

(** Error on Elf parsing *)
exception ElfError of string

let _ =
  Printexc.register_printer (function
    | ElfError s -> Some (Printf.sprintf "ElfError: %s" s)
    | _ -> None)

(** Throw an {!ElfError} *)
let elferror fmt = Printf.ksprintf (fun s -> raise (ElfError s)) fmt

(** Parse an ELF file to create an {!Elf.File.t} using Linksem.

    May raise an {!ElfError}
*)
let of_file (filename : string) =
  info "Loading ELF file %s" filename;
  (* parse the ELF file using linksem *)
  let bs = match Byte_sequence.acquire filename with
    | Error.Fail s -> elferror "Linksem: Byte_sequence.acquire: %s" s
    | Error.Success x -> x
  in
  let elf64_file = match Elf_file.read_elf64_file bs with
    | Error.Fail s -> elferror "Linksem: read_elf64_file: %s" s
    | Error.Success x -> x
  in
  let symbol_map = match LinksemRelocatable.get_elf64_file_global_symbol_init elf64_file with
    | Error.Fail s -> elferror "LinksemRelocatable: get_elf64_file_global_symbol_init: %s" s
    | Error.Success x -> x
  in
  (* let ( (elf_file : Elf_file.elf_file),
        (elf_epi : Sail_interface.executable_process_image),
        (symbol_map : Elf_file.global_symbol_init_info) ) =
    match Sail_interface.populate_and_obtain_global_symbol_init_info filename with
    | Error.Fail s -> elferror "Linksem: populate_and_obtain_global_symbol_init_info: %s" s
    | Error.Success x -> x
  in *)
  (* Check this is a 64 bits ELF file *)
  let entry = Z.to_int elf64_file.elf64_file_header.elf64_entry in
  let machine = machine_of_linksem elf64_file.elf64_file_header.elf64_machine in
  debug "Loading ELF symbols of %s" filename;
  let symbols = SymTbl.of_linksem symbol_map in
  debug "Adding .rodata section of %s" filename;
  (* We add the .rodata section seperately from the symbols because
     - it can contain non-symbol information such as string literals and
       constants used in branch-register target calculations
     - the range of the section is guaranteed to overlap with any symbols
       within it, and so not suitable to be stored in the [RngMap] *)
  let elf_file = Elf_file.ELF_File_64 elf64_file in
  let sections = List.filter_map (fun (s:Elf_interpreted_section.elf64_interpreted_section) ->
    if Z.equal Z.zero (Z.logand s.elf64_section_flags Elf_section_header_table.shf_alloc) then
      None
    else
      Some {
        name=s.elf64_section_name_as_string;
        size=Z.to_int s.elf64_section_size;
        align=Z.to_int s.elf64_section_align;
      }
  ) elf64_file.elf64_file_interpreted_sections
  in
  let rodata =
    SMap.of_list @@ List.filter_map Option.(fun (section:Elf_interpreted_section.elf64_interpreted_section) ->
      let+ sname = if String.starts_with ~prefix:".rodata" section.elf64_section_name_as_string then
        Some section.elf64_section_name_as_string
      else
        None
      in
      let data = section.elf64_section_body in
      let relocations = match LinksemRelocatable.get_relocations_for_section elf64_file sname with
      | Error.Fail s -> elferror "LinksemRelocatable: get_relocations_for_section: %s" s
      | Error.Success x -> Relocations.of_linksem x
      in 
      (* let (_, addr, data) =
        Dwarf.extract_section_body elf_file Abi_aarch64_symbolic_relocation.aarch64_data_relocation_interpreter sname false
        (* `false' argument is for returning an empty byte-sequence if
          section is not found, instead of throwing an exception *)
      in *)
      (
        sname,
        Segment.
        {
          data = (data, relocations);
          addr = 0; (* Meaningless for relocatable files *)
          size = BytesSeq.length data;
          read = true;
          write = false;
          execute = false;
        }
      )
    ) elf64_file.elf64_file_interpreted_sections
  in
  info "ELF file %s has been loaded" filename;
  { filename; symbols; entry; machine; linksem = elf_file; rodata; sections }
