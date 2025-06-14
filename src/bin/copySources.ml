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
              (let dir = Sym.to_int lnfe.lnfe_directory_index in
               if dir = 0 then None
               else
                 Some
                   (Byte_sequence.string_of_byte_sequence
                      (Dwarf_byte_sequence.sym_bs_expect_const (List.nth lnh.lnh_include_directories (dir - 1))))),
              Byte_sequence.string_of_byte_sequence (Dwarf_byte_sequence.sym_bs_expect_const lnfe.lnfe_path) ))
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
