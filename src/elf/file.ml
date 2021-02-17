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

(** Parse an ELF file to create an {!ElfFile.t} using Linksem.

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
