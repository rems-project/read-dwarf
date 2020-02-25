(** This module represent an ELF 64 file we do not deal with 32 bit for now *)

module SymTbl = ElfSymTable

type machine = X86 | X86_64 | PPC | PPC64 | ARM | AARCH64 | RISCV | Other of int

let machine_of_linksem lmachine =
  if lmachine = Elf_header.elf_ma_386 then X86
  else if lmachine = Elf_header.elf_ma_x86_64 then X86_64
  else if lmachine = Elf_header.elf_ma_arm then ARM
  else if lmachine = Elf_header.elf_ma_aarch64 then AARCH64
  else if lmachine = Elf_header.elf_ma_ppc then PPC
  else if lmachine = Elf_header.elf_ma_ppc64 then PPC64
  else if lmachine = Elf_header.elf_ma_riscv then RISCV
  else Other (Z.to_int lmachine)

let machine_to_string = function
  | X86 -> "X86"
  | X86_64 -> "X86_64"
  | PPC -> "PPC"
  | PPC64 -> "PPC64"
  | ARM -> "ARM"
  | AARCH64 -> "AARCH64"
  | RISCV -> "RISCV"
  | Other i -> Printf.sprintf "Other(%d)" i

let pp_machine mach = mach |> machine_to_string |> PP.string

(** The type containing all the information about an ELF file *)
type t = {
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
    Lem_list.mapMaybe
      (fun (seg, prov) -> if prov = Elf_file.FromELF then Some seg else None)
      segments
  in
  let entry = Z.to_int entry in
  let machine = machine_of_linksem machine in
  let segments = List.map Segment.of_linksem segments in
  let symbols = SymTbl.of_linksem segments symbol_map in
  { symbols; segments; entry; machine; linksem = elf_file }
