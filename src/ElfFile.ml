(** This module represent an ELF 64 file we do not deal with 32 bit for now *)

open Logs.Logger (struct
  let str = __MODULE__
end)

module SymTbl = ElfSymTable

type machine = Supp of ArchType.t | Other of int

let machine_of_linksem lmachine =
  if lmachine = Elf_header.elf_ma_386 then Supp X86
  else if lmachine = Elf_header.elf_ma_x86_64 then Supp X86_64
  else if lmachine = Elf_header.elf_ma_arm then Supp ARM
  else if lmachine = Elf_header.elf_ma_aarch64 then Supp AARCH64
  else if lmachine = Elf_header.elf_ma_ppc then Supp PPC
  else if lmachine = Elf_header.elf_ma_ppc64 then Supp PPC64
  else Other (Z.to_int lmachine)

let machine_to_string = function
  | Supp a -> ArchType.to_string a
  | Other i -> Printf.sprintf "Other(%d)" i

let pp_machine mach = mach |> machine_to_string |> PP.string

(** The type containing all the information about an ELF file *)
type t = {
  filename : string;
  symbols : SymTbl.t;
  segments : Segment.t list;
  entry : int;
  machine : machine;
  linksem : Elf_file.elf_file;
}

(** Error on Elf parsing *)
exception ElfError of string

let _ =
  Printexc.register_printer (function
    | ElfError s -> Some (Printf.sprintf "ElfError: %s" s)
    | _ -> None)

(** Throw an {!ElfError} *)
let elferror fmt = Printf.ksprintf (fun s -> raise (ElfError s)) fmt

(** Parse an ELF file to create an {!ElfFile.t} using Linksem.

    May raise an {!ElfError}
*)
let of_file (filename : string) =
  info "Loading ELF file %s" filename;
  let ( (elf_file : Elf_file.elf_file),
        (elf_epi : Sail_interface.executable_process_image),
        (symbol_map : Elf_file.global_symbol_init_info) ) =
    match Sail_interface.populate_and_obtain_global_symbol_init_info filename with
    | Error.Fail s -> elferror "Linksem: populate_and_obtain_global_symbol_init_info: %s" s
    | Error.Success x -> x
  in
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
