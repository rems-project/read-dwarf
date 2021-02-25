(*==================================================================================*)
(*  BSD 2-Clause License                                                            *)
(*                                                                                  *)
(*  Read-dwarf, located in the src/ and test_asm/ directories, is subject to this   *)
(*  BSD 2-Clause License. This license does not apply to files outside these        *)
(*  directories.                                                                    *)
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
(*  The project has been partly funded by EPSRC grant EP/K008528/1.                 *)
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
  else Other (Z.to_int lmachine)

(** Convert a {!machine} to a human readable string *)
let machine_to_string = function
  | Supp a -> Config.Arch.to_string a
  | Other i -> Printf.sprintf "Other(%d)" i

(** Pretty prints a {!machine} *)
let pp_machine mach = mach |> machine_to_string |> Pp.string

(** The type containing all the information about an ELF file *)
type t = {
  filename : string;  (** The name on the file system. Useful for error messages *)
  symbols : SymTbl.t;  (** The symbol table *)
  segments : Segment.t list;  (** The list of Segment loaded in RAM *)
  entry : int;  (** The address of the entry point *)
  machine : machine;  (** The target architecture of the file *)
  linksem : Elf_file.elf_file;  (** The original linksem structure for the file *)
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
  let ( (elf_file : Elf_file.elf_file),
        (elf_epi : Sail_interface.executable_process_image),
        (symbol_map : Elf_file.global_symbol_init_info) ) =
    match Sail_interface.populate_and_obtain_global_symbol_init_info filename with
    | Error.Fail s -> elferror "Linksem: populate_and_obtain_global_symbol_init_info: %s" s
    | Error.Success x -> x
  in
  (* Check this is a 64 bits ELF file *)
  begin
    match elf_file with
    | Elf_file.ELF_File_32 _ -> elferror "32 bits elf files unsupported"
    | _ -> ()
  end;
  let (segments, entry, machine) =
    match elf_epi with
    | ELF_Class_32 _ -> elferror "32 bits elf file class unsupported"
    | ELF_Class_64 (s, e, m) -> (s, e, m)
  in

  (* Extract all the segments *)
  let segments =
    List.filter_map
      (fun (seg, prov) -> if prov = Elf_file.FromELF then Some seg else None)
      segments
  in
  let entry = Z.to_int entry in
  let machine = machine_of_linksem machine in
  debug "Loading ELF segments of %s" filename;
  let segments = List.map Segment.of_linksem segments in
  debug "Loading ELF symbols of %s" filename;
  let symbols = SymTbl.of_linksem segments symbol_map in
  info "ELF file %s has been loaded" filename;
  { filename; symbols; segments; entry; machine; linksem = elf_file }
