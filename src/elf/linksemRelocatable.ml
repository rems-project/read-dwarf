(* TODO header *)

module SMap = Map.Make (String)

type sym_addr = string * Z.t

type rels =
  | AArch64 of (Z.t, Abi_aarch64_symbolic_relocation.aarch64_relocation_target Elf_symbolic.universal_relocation) Pmap.map

type sym_data =
Byte_sequence_wrapper.byte_sequence * rels


(* Like in linksem, but address is section+offset, data has relocations and with a writable flag *)
type symbol = string * (Z.t * Z.t * sym_addr * sym_data * Z.t) * bool

type global_symbol_init_info = symbol list

open Elf_symbol_table
open Elf_interpreted_section

let get_relocations_for_section (f:Elf_file.elf64_file) section =
  let machine = f.elf64_file_header.elf64_machine in
  if machine = Elf_header.elf_ma_aarch64 then
    Error.bind
      (Elf_symbolic.extract_elf64_relocations_for_section f Abi_aarch64_symbolic_relocation.aarch64_relocation_interpreter section)
      @@ fun relocs -> Error.return (AArch64 relocs)
  else
    Error.fail @@ "machine not supported " ^ (Elf_header.string_of_elf_machine_architecture machine)

let get_elf64_file_global_symbol_init (f: Elf_file.elf64_file) : global_symbol_init_info Error.error =
  let secs = f.elf64_file_interpreted_sections in
  Error.bind (Elf_file.get_elf64_file_symbol_table f) @@ fun (symtab, strtab) ->
    let rel_cache = ref SMap.empty in
    let get_relocs section =
      match SMap.find_opt section !rel_cache with
      | Some rels -> rels
      | None -> get_relocations_for_section f section
    in
    List.filter_map (
      fun entry ->
        let name = Uint32_wrapper.to_bigint entry.elf64_st_name in
        let addr_offset = Uint64_wrapper.to_bigint entry.elf64_st_value in
        let size = Uint64_wrapper.to_bigint entry.elf64_st_size in
        let shndx = Uint32_wrapper.to_int entry.elf64_st_shndx in
        let typ  = Elf_symbol_table.extract_symbol_type entry.elf64_st_info in
        let bnd  = Elf_symbol_table.extract_symbol_binding entry.elf64_st_info in
        Option.map (
          fun section ->
            let addr = (section.elf64_section_name_as_string, addr_offset) in
            let data = if Byte_sequence.length0 section.elf64_section_body = Z.zero then
              Error.return (Byte_sequence.zeros size)
            else
              Byte_sequence.offset_and_cut addr_offset size section.elf64_section_body
            in
            Error.bind (get_relocs section.elf64_section_name_as_string) @@ fun (AArch64 relocs) ->
              let relocs = relocs
              |> Pmap.bindings_list
              |> List.fold_left (fun m (pos, r) ->
                  let sz = size in
                  let open Z in
                  let open Compare in
                  if pos >= addr_offset && pos < addr_offset + sz then
                    Pmap.add (pos - addr_offset) r m
                  else
                    m
                ) (Pmap.empty Z.compare)
              |> fun x -> AArch64 x
              in
            Error.bind data @@ fun data ->
              Error.bind (String_table.get_string_at name strtab) @@ fun str ->
                let write = Elf_file.flag_is_set Elf_section_header_table.shf_write section.elf64_section_flags in
              Error.return (str, (typ, size, addr, (data, relocs), bnd), write)
        ) (List.nth_opt secs shndx)
    ) symtab |> Error.mapM Fun.id