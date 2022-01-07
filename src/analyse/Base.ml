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

open Logs.Logger (struct
  let str = __MODULE__
end)

(*open Printf*)
open Types
open Utils
open SdtUtils

(*open ElfTypes *)
open Elf

(*open ControlFlowTypes*)
open CollectedType

(*open DwarfLineInfo*)
(*open ElfSymbols*)

(*open DwarfFrameInfo*)
(*open ControlFlowTypes*)
(*open ControlFlow*)
(*open ComeFrom*)
(*open RangedVarType*)
open QemuLog
open ControlFlowPpDot

(*open ControlFlowPpText *)
(*open CallGraph*)
(*open DwarfVarInfo*)
(*open DwarfInliningInfo*)
open Collected
open Pp

(*****************************************************************************)
(*       analysis top-level                                                  *)
(*****************************************************************************)

let process_file () : unit =

  (*filename_objdump_d filename_branch_tables (filename_elf : string) : unit =*)
  let m = !Globals.ppmode in

  (* TODO: make idiomatic Cmdliner :-(  *)
  let filename_elf = match !Globals.elf with Some s -> s | None -> fatal "no --elf option\n" in

  let filename_objdump_d =
    match !Globals.objdump_d with Some s -> s | None -> fatal "no --objdump-d option\n"
  in

  let filename_branch_tables_option =
    !Globals.branch_table_data_file
    (*
    match !Globals.branch_table_data_file with
    | Some s -> s
    | None -> fatal "no --branch-tables option\n"*)
  in

  let filename_out_file_option =
    !Globals.out_file
    (* match m with
       | Ascii -> !Globals.out_file
       | Html -> Option.map (function s -> s ^ ".html") !Globals.out_file*)
  in

  (* try caching linksem output - though linksem only takes 5s, so scarcely worth the possible confusion. It's recomputing the variable info that takes the time *)
  (*
  let filename_marshalled = filename ^ ".linksem-marshalled" in
  let test =
    match marshal_from_file filename_marshalled with
    | None ->
       let test = parse_elf_file filename in
       marshal_to_file filename_marshalled test;
       test
    | Some test ->
       test
  in
   *)
  let test = time "parse_elf_file" parse_elf_file filename_elf in

  let an =
    time "mk_analysis" (mk_analysis test filename_objdump_d) filename_branch_tables_option
  in

  match (!Globals.elf2, !Globals.objdump_d2, !Globals.branch_table_data_file2) with
  | (None, _, _) -> (
      (* read qemu log if present *)
      let visitedo : bool array option =
        match !Globals.qemu_log with
        | None -> None
        | Some qemu_filename -> Some (read_qemu_log an qemu_filename)
      in

      (* output CFG dot file *)
      ( match !Globals.cfg_dot_file with
      | Some cfg_dot_file ->
          let start_indices =
            List.map
              (function k -> (k, nesting_init None))
              (Array.find_all_indices (function ss -> ss <> []) an.elf_symbols)
          in
          let graph = mk_cfg test an visitedo "" false false start_indices in
          Printf.printf "cfg branch nodes: %d\n" (count_branch_nodes graph);
          (*            let graph' = reachable_subgraph graph ["mpool_fini"] in*)
          pp_cfg graph cfg_dot_file true
      | None -> ()
      );

      (* output annotated objdump *)
      let c = match filename_out_file_option with Some f -> open_out f | None -> stdout in

      (* copy emacs syntax highlighting blob to output. todo: sometime de-hard-code the filename*)
      begin
        match m with
        | Ascii -> (
            match read_file_lines (output_preamble_file "emacs-highlighting") with
            | Error _ -> ()
            | Ok lines -> Array.iter (function s -> Printf.fprintf c "%s\n" s) lines
          )
        | Html -> ()
      end;

      (* copy html preamble blob to output. todo: sometime de-hard-code the filename*)
      begin
        match m with
        | Ascii -> ()
        | Html -> (
            match read_file_lines (output_preamble_file "html-preamble.html") with
            | Error _ -> ()
            | Ok lines -> Array.iter (function s -> Printf.fprintf c "%s\n" s) lines
          )
      end;

      Printf.fprintf c "%s" (time "pp_test_analysis" pp_test_analysis m test an);

      (* copy html postamble blob to output. todo: sometime de-hard-code the filename*)
      begin
        match m with
        | Ascii -> ()
        | Html -> (
            match read_file_lines (output_preamble_file "html-postamble.html") with
            | Error _ -> ()
            | Ok lines -> Array.iter (function s -> Printf.fprintf c "%s\n" s) lines
          )
      end;

      match filename_out_file_option with Some _ -> close_out c | None -> ()
    )
  | (Some filename_elf2, Some filename_objdump_d2, filename_branch_tables_option_2) -> (
      match !Globals.cfg_dot_file with
      | Some cfg_dot_file ->
          let test2 = parse_elf_file filename_elf2 in

          let an2 = mk_analysis test2 filename_objdump_d2 filename_branch_tables_option_2 in

          let parse_source_node_list _test an (ano2 : analysis option) (so : string option) :
              (index * nesting) list =
            match so with
            | None -> []
            | Some s ->
                List.map
                  (function
                    | s' -> (
                        match
                          address_of_subroutine_name an.sdt s' (*address_of_elf_symbol test s'*)
                        with
                        | Some addr -> (
                            let k = an.index_of_address addr in
                            match ano2 with
                            | None -> (k, nesting_init None)
                            | Some an2 -> (
                                match find_sdt_subroutine_by_name true an2.sdt s' with
                                | Some ss -> (k, nesting_init (Some ss))
                                | None ->
                                    fatal "no corresponding ano2 sdt subroutine found for %s\n" s'
                              )
                          )
                        | None -> fatal "address_of_elf_symbol failed on \"%s\"\n" s'
                      ))
                  (String.split_on_char ' ' s)
          in

          let all_elf_symbol_indices an =
            List.map
              (function k -> (k, nesting_init None))
              (Array.find_all_indices (function ss -> ss <> []) an.elf_symbols)
          in

          (* if/when we want to run comparison without specifying the start symbols, we'll need to add the sdt_subroutine data to the above for the to-be-inlined O0 case *)
          let start_indices =
            match parse_source_node_list test an (Some an2) !Globals.cfg_source_nodes with
            | [] -> all_elf_symbol_indices an
            | cfg_source_node_list0 -> cfg_source_node_list0
          in

          let start_indices2 =
            match parse_source_node_list test2 an2 None !Globals.cfg_source_nodes2 with
            | [] -> all_elf_symbol_indices an2
            | cfg_source_node_list2 -> cfg_source_node_list2
          in

          let cfg_dot_file_root = String.sub cfg_dot_file 0 (String.length cfg_dot_file - 4) in

          let graph0 = mk_cfg test an None "O0_" false true start_indices in

          let graph2 = mk_cfg test2 an2 None "O2_" false false start_indices2 in

          let graph = graph_cfg_union graph0 graph2 in

          let graph' = correlate_source_line graph0 graph2 in

          (*
          let graph'' = graph_cfg_union graph graph' in
          let cfg_dot_file_union = cfg_dot_file_root ^ "_union.dot" in
          pp_cfg graph cfg_dot_file_union false;
 *)

          (* it'd be nice to layout nodes and base edges without the correlate edges and then to layout correlate edges only wrt node positions and glom them together.  But that seems impossible, as dot seems not to respect position attributes (only neato/fdp do). There also seems to be no dot edge attribute that would make the edge ignored for layout, or for that edge's layout to ignore the other edges.  So instead we do the following hackery *)
          let cfg_dot_file_base = cfg_dot_file_root ^ "_base.dot" in
          let cfg_dot_file_layout = cfg_dot_file_root ^ "_layout.dot" in
          pp_cfg graph cfg_dot_file_base false;
          ignore @@ Unix.system ("dot -Txdot " ^ cfg_dot_file_base ^ " > " ^ cfg_dot_file_layout);
          let layout_lines =
            match read_file_lines cfg_dot_file_layout with
            | Ok lines -> lines
            | Error s -> fatal "couldn't read cfg_dot_file_layout %s" s
          in
          let ppd_correlate_edges = List.map (pp_edge "black") graph'.gc_edges in
          let c = open_out cfg_dot_file in
          Array.iteri
            (function
              | j -> (
                  function
                  | _line ->
                      if j + 1 = Array.length layout_lines then ()
                      else Printf.fprintf c "%s\n" layout_lines.(j)
                ))
            layout_lines;
          List.iter (function line -> Printf.fprintf c "%s\n" line) ppd_correlate_edges;
          Printf.fprintf c "}\n";
          close_out c;
          ignore @@ Unix.system ("dot -Tpdf " ^ cfg_dot_file ^ " > " ^ cfg_dot_file_root ^ ".pdf");
          ignore @@ Unix.system ("dot -Tsvg " ^ cfg_dot_file ^ " > " ^ cfg_dot_file_root ^ ".svg")
      | None -> fatal "no dot file\n"
    )
  | _ -> fatal "missing files for elf2\n"

(* dot emacs regexp that appears to match edges: [a-zA-Z0-9_]+ *->[^;]*;    *)
