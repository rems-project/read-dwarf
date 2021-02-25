(** This module is the body implementation for the [copy-sources] subcommand. *)

open Logs.Logger (struct
  let str = __MODULE__
end)

open Analyse.Utils

let process_file () : unit =
  (* TODO: make idiomatic Cmdliner :-(  *)
  let filename_elf =
    match !Analyse.Globals.elf with Some s -> s | None -> fatal "no --elf option\n"
  in

  let _ =
    match !Analyse.Globals.src_target_dir with
    | Some _ -> ()
    | None -> fatal "no --src-target-dir option\n"
  in

  let test = Analyse.Utils.time "parse_elf_file" Analyse.Elf.parse_elf_file filename_elf in

  let ds : Dwarf.dwarf_static = test.Analyse.ElfTypes.dwarf_static in

  let evaluated_line_info : Dwarf.evaluated_line_info = ds.Dwarf.ds_evaluated_line_info in

  let open Dwarf in
  let lnhs : line_number_header list = List.map fst evaluated_line_info in

  let files_of_lnh lnh :
      (string option (*comp_dir*) * string option (*lnh_include_directory*) * string)
      (*lnfe_path*)
      list =
    List.map
      (function
        | lnfe ->
            ( lnh.lnh_comp_dir,
              (let dir = Nat_big_num.to_int lnfe.lnfe_directory_index in
               if dir = 0 then None
               else
                 Some
                   (Byte_sequence.string_of_byte_sequence
                      (List.nth lnh.lnh_include_directories (dir - 1)))),
              Byte_sequence.string_of_byte_sequence lnfe.lnfe_path ))
      lnh.lnh_file_entries
  in

  let files = List.concat_map files_of_lnh lnhs in

  (*List.iter (function (dir,path) -> Printf.printf "%s  %s\n" dir path) files ; *)
  let copies =
    List.map
      (function
        | (comp_diro, dir, file) ->
            let (directory_original, directory_replacement) =
              Analyse.DwarfLineInfo.actual_directories !Analyse.Globals.src_target_dir
                (comp_diro, dir, file)
            in

            let source = Filename.concat directory_original file in
            let target = Filename.concat directory_replacement file in
            (source, target, directory_replacement))
      files
  in

  (*Printf.printf "%s" (Dwarf.pad_rows (  ["source=";  "target=";  "targetdir="]::copies));*)
  let copy (source, target, target_dir) =
    sys_command ("mkdir -p " ^ Filename.quote target_dir);
    sys_command ("cp -a " ^ Filename.quote source ^ " " ^ Filename.quote target)
  in

  List.iter copy copies;

  ()