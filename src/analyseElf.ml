open AnalyseUtils
open AnalyseElfTypes

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
         Printf.sprintf "**** name = %s  address = %s  typ = %d\n" name (pp_addr address)
           (Nat_big_num.to_int typ))
       symbol_map)

(*****************************************************************************)
(**       use linksem to parse ELF file and extract DWARF info               *)

(*****************************************************************************)

let parse_elf_file (filename : string) : test =
  (* call ELF analyser on file *)
  let info = Sail_interface.populate_and_obtain_global_symbol_init_info filename in

  let ( (elf_file : Elf_file.elf_file),
        (elf_epi : Sail_interface.executable_process_image),
        (symbol_map : Elf_file.global_symbol_init_info) ) =
    match info with
    | Error.Fail s -> fatal "populate_and_obtain_global_symbol_init_info: %s" s
    | Error.Success x -> x
  in

  let f64 =
    match elf_file with Elf_file.ELF_File_64 f -> f | _ -> raise (Failure "not Elf64")
  in

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
  match (elf_epi, elf_file) with
  | (Sail_interface.ELF_Class_32 _, _) -> fatal "%s" "cannot handle ELF_Class_32"
  | (_, Elf_file.ELF_File_32 _) -> fatal "%s" "cannot handle ELF_File_32"
  | (Sail_interface.ELF_Class_64 (segments, e_entry, e_machine), Elf_file.ELF_File_64 f1) ->
      (* architectures from linksem elf_header.lem *)
      let arch =
        if f64.elf64_file_header.elf64_machine = Elf_header.elf_ma_aarch64 then AArch64
        else if f64.elf64_file_header.elf64_machine = Elf_header.elf_ma_x86_64 then X86
        else fatal "unrecognised ELF file architecture"
      in

      (* remove all the auto generated segments (they contain only 0s) *)
      let segments =
        Lem_list.mapMaybe
          (fun (seg, prov) -> if prov = Elf_file.FromELF then Some seg else None)
          segments
      in
      let ds =
        match Dwarf.extract_dwarf_static (Elf_file.ELF_File_64 f1) with
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
          segments;
          e_entry;
          e_machine;
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
