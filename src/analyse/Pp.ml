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

open Types
open Utils
open ElfTypes
open ControlFlowTypes
open CollectedType
open DwarfLineInfo
open DwarfFrameInfo
open ControlFlow
open ComeFrom
open CallGraph
open DwarfVarInfo

(*****************************************************************************)
(**     render collected analysis data to text or css                        *)

(*****************************************************************************)

type render_kind =
  | Render_symbol_star
  | Render_symbol_nostar
  | Render_source
  | Render_frame
  | Render_instruction
  | Render_vars
  | Render_vars_new
  | Render_vars_old
  | Render_inlining
  | Render_ctrlflow
  | Render_relocation

let render_colour = function
  | Render_symbol_star -> "gold"
  | Render_symbol_nostar -> "moccasin"
  | Render_source -> "darkorange"
  | Render_frame -> "cyan"
  | Render_instruction -> "white"
  | Render_vars -> "mediumaquamarine"
  | Render_vars_new -> "mediumaquamarine"
  | Render_vars_old -> "grey"
  | Render_inlining -> "red"
  | Render_ctrlflow -> "white"
  | Render_relocation -> "purple"

let render_class_name = function
  | Render_symbol_star -> "symbol-star"
  | Render_symbol_nostar -> "symbol-nostar"
  | Render_source -> "source"
  | Render_frame -> "frame"
  | Render_instruction -> "instruction"
  | Render_vars -> "vars"
  | Render_vars_new -> "vars-new"
  | Render_vars_old -> "vars-old"
  | Render_inlining -> "inlining"
  | Render_ctrlflow -> "ctrlflow"
  | Render_relocation -> "relocation"

type html_idiom = HI_span | HI_pre | HI_classless_span | HI_font

let html_idiom = HI_span

let css m (rk : render_kind) s =
  match m with
  | Ascii -> s
  | Html ->
      (* seeing if putting the newlines outside the spans avoids the browser rendering performance problem.  Not so far... *)
      if s = "" then ""
      else
        let lines = String.split_on_char '\n' s in
        String.concat "\n"
          (List.map
             (function
               | line -> (
                   match html_idiom with
                   (* the browser HTML rendering of large files can be very slow, hence this experimentation *)
                   | HI_span ->
                       (* try with a span for each unit *)
                       "<span class=\"" ^ render_class_name rk ^ "\">" ^ html_escape line
                       ^ "</span>"
                       (*"</" ^ render_class_name rk ^ ">"*)
                   | HI_pre ->
                       (* try with a pre for each unit *)
                       "<pre color=\"" ^ render_colour rk ^ "\">" ^ html_escape line ^ "</pre>"
                   | HI_classless_span ->
                       (* try with a classless span for each unit *)
                       "<span color=\"" ^ render_colour rk ^ "\">" ^ html_escape line ^ "</span>"
                   | HI_font ->
                       (* try with an html font for each unit (NOT HTML5) - best on large files so far - ok on firefox; too slow on chromium*)
                       "<font color=\"" ^ render_colour rk ^ "\">" ^ html_escape line ^ "</font>"
                 ))
             lines)

(*****************************************************************************)
(**       pretty-print one instruction                                       *)

(*****************************************************************************)

(* plumbing to print diffs from one instruction to the next *)
let last_frame_info = ref ""

let last_var_info = ref []

let last_source_info = ref ""

let pp_instruction_init () =
  last_frame_info := "";
  last_var_info := ([] : string list);
  last_source_info := ""

let pp_instruction m test an rendered_control_flow_common_prefix_end k i =
  (* the come_froms for this instruction, calculated first to determine whether this is the start of a basic block *)
  let addr = i.i_addr in
  let come_froms' =
    List.filter (function cf -> cf.cf_target_kind <> T_plain_successor) an.come_froms.(k)
  in

  (* the inlining for this instruction *)
  let (ppd_labels, ppd_new_inlining, _) = an.inlining.(k) in

  (* the elf symbols at this address, if any (and reset the last_var_info if any) *)
  let elf_symbols = an.elf_symbols.(k) in
  (match elf_symbols with [] -> () | _ -> last_var_info := []);

  (* is this the start of a basic block? *)
  ( if come_froms' <> [] || elf_symbols <> [] then
    an.pp_inlining_label_prefix ""
    ^ css m Render_ctrlflow
        (ControlFlowPpText.pp_glyphs rendered_control_flow_common_prefix_end
           an.rendered_control_flow_inbetweens.(k))
    ^ "\n"
  else ""
  )
  (* link target *)
  ^ ( match m with
    | Ascii -> ""
    | Html -> "<span id=\"" ^ pp_addr addr ^ " display=\"none\"></span>"
    )
  (* symbols *)
  ^ String.concat ""
      (let pp_symb (rk : render_kind) (addstar : bool) (s : string) =
         let sym = (if addstar then "**" else "  ") ^ pp_addr addr ^ " <" ^ s ^ ">:" in
         let ctrl =
           an.pp_inlining_label_prefix ""
           ^ ControlFlowPpText.pp_glyphs rendered_control_flow_common_prefix_end
               an.rendered_control_flow_inbetweens.(k)
         in
         let (_, non_overlapped_ctrl) =
           (* ctrl here can contain interesting UTF8 unicode, while sym is a plain string; hence the following UTF8-aware code *)
           let utf8_drop (n : int) (s : string) =
             let folder
                 (*: (int*string) Uutf.String.folder*)
                   ( (n : int (*number of prefix chars still to drop*)),
                     (s : string (*accumulated string*)) ) (_pos : int) um =
               if n > 0 then (n - 1, "")
               else
                 match um with
                 | `Uchar u ->
                     (* yuck *)
                     let b = Buffer.create 4 in
                     Uutf.Buffer.add_utf_8 b u;
                     let s' = Bytes.to_string (Buffer.to_bytes b) in
                     (0, s ^ s')
                 | `Malformed s' -> (0, s ^ s')
             in
             Uutf.String.fold_utf_8 folder (n, "") s
           in
           utf8_drop (String.length sym) ctrl
         in
         css m rk sym ^ css m Render_ctrlflow non_overlapped_ctrl ^ "\n"
       in
       let (syms_nodollar, syms_dollar) =
         List.partition (fun s -> not (String.contains s '$')) elf_symbols
       in
       List.map (pp_symb Render_symbol_star true) syms_nodollar
       @ List.map (pp_symb Render_symbol_nostar false) syms_dollar)
  (* function parameters at this address *)
  ^
  let pp_params addr params =
    match List.assoc_opt addr params with
    | None -> ""
    | Some (name, vars) -> (
        "+ " ^ name ^ " params:"
        ^
        match vars with
        | [] -> " none\n"
        | _ ->
            "\n"
            ^ String.concat ""
                (List.map (pp_sdt_concise_variable_or_formal_parameter 0 true) vars)
      )
  in
  css m Render_vars (pp_params addr an.ranged_vars_at_instructions.rvai_params)
  (* the new inlining info for this address *)
  ^ css m Render_inlining ppd_new_inlining
  (* the source file lines (if any) associated to this address *)
  (* OLD VERSION *)
  (* ^ begin
       if !Globals.show_source then
         let source_info =
           match pp_dwarf_source_file_lines () test.dwarf_static true addr with
           | Some s ->
               (* the inlining label prefix *)
               an.pp_inlining_label_prefix ppd_labels
               ^ an.rendered_control_flow_inbetweens.(k)
               ^ s ^ "\n"
           | None -> ""
         in
         if source_info = !last_source_info then "" (*"unchanged\n"*)
         else (
           last_source_info := source_info;
           source_info
         )
       else ""
     end
  *)
  (* NEW VERSION *)
  ^ begin
      let pp_line multiple elifi =
        css m Render_inlining (an.pp_inlining_label_prefix ppd_labels)
        ^ css m Render_ctrlflow
            (ControlFlowPpText.pp_glyphs rendered_control_flow_common_prefix_end
               an.rendered_control_flow_inbetweens.(k))
        ^ ""
        ^ css m Render_source
            (pp_dwarf_source_file_lines' m test.dwarf_static !Globals.show_source multiple elifi)
        ^ "\n"
      in
      (* if there's just a single entry, suppress iff it's a non-start entry, but if there are (confusingly) multiple, show all *)
      let lines =
        match an.line_info.(k) with
        | [] -> ""
        | [elifi] -> if elifi.elifi_start then pp_line false elifi else ""
        | elifis -> String.concat "" (List.map (pp_line true) elifis)
      in
      lines
    end
  (* the frame info for this address *)
  (*TODO: precompute the diffs to make this pure *)
  ^ begin
      if !Globals.show_cfa then
        let frame_info = pp_frame_info m an.frame_info k in
        if frame_info = !last_frame_info then "" (*"CFA: unchanged\n"*)
        else (
          last_frame_info := frame_info;
          (* the inlining label prefix *)
          css m Render_inlining (an.pp_inlining_label_prefix ppd_labels)
          ^ css m Render_ctrlflow
              (ControlFlowPpText.pp_glyphs rendered_control_flow_common_prefix_end
                 an.rendered_control_flow_inbetweens.(k))
          ^ css m Render_frame frame_info
        )
      else ""
    end
  (* the variables whose location ranges include this address - old version*)
  (* ^ begin
         if (*true*) !Globals.show_vars then (
           let als_old = !last_var_info in
           let als_new (*fald*) = Dwarf.filtered_analysed_location_data test.dwarf_static addr in
           last_var_info := als_new;
           Dwarf.pp_analysed_location_data_diff test.dwarf_static.ds_dwarf als_old als_new
         )
         else ""
       end
     ^ "\n"
  *)
  (* the variables whose location ranges include this address - new version*)
  ^ begin
      if !Globals.show_vars then
        css m Render_vars_new (pp_ranged_vars "+" an.ranged_vars_at_instructions.rvai_new.(k))
        (*        ^ pp_ranged_vars "C" an.ranged_vars_at_instructions.rvai_current.(k)*)
        (*        ^ pp_ranged_vars "R" an.ranged_vars_at_instructions.rvai_remaining.(k)*)
      else ""
    end
  (* the inlining label prefix *)
  ^ css m Render_inlining
      ("~"
      ^
      let s = an.pp_inlining_label_prefix ppd_labels in
      String.sub s 1 (String.length s - 1)
      )
  (* the rendered control flow *)
  ^ css m Render_ctrlflow
      (ControlFlowPpText.pp_glyphs rendered_control_flow_common_prefix_end
         an.rendered_control_flow.(k))
  (* the address and (hex) instruction *)
  ^ css m Render_instruction (
      pp_addr addr ^ ":  "
      ^ pp_opcode_bytes test.arch i.i_opcode
      (* the dissassembly from objdump *)
      ^ "  "
      ^ i.i_mnemonic ^ "\t" ^ i.i_operands
      )
  ^ css m Render_relocation
      (match i.i_relocation with
       | None -> ""
       | Some (typ, targ) ->
           "\t" ^ typ ^ " " ^ targ
      )
  (* the instruction's control flow *)
  (* any indirect-branch control flow from this instruction *)
  ^ css m Render_ctrlflow
      (begin
         match i.i_control_flow with
         | C_branch_register _ ->
             " -> "
             ^ String.concat ","
                 (List.map
                    (function
                      | (_, a', _, s) -> pp_target_addr_wrt addr i.i_control_flow a' ^ "" ^ s ^ "")
                    i.i_targets)
             ^ " "
         | _ -> ""
       end
      (* any control flow to this instruction *)
      ^ pp_come_froms addr come_froms'
      ^ "\n"
      )
  ^
  if (*true*) !Globals.show_vars then
    if k < Array.length an.instructions - 1 then
      css m Render_vars_old (pp_ranged_vars "-" an.ranged_vars_at_instructions.rvai_old.(k + 1))
    else ""
  else ""

(*****************************************************************************)
(**       pretty-print test analysis                                         *)

(*****************************************************************************)

(* 


html or ascii
file-per-compilation-unit or [single file for linksem and single file for read-dwarf]
files flattened to one dir or files in tree

global
  list of compilation units
  
  .debug_loc section
  .debug_ranges secion
  .debug_frame section
  evaluation of frame data
  analysis of location data
  inlined subroutine info (all)
  inlined subroutine info (by range) (all)
  simple die tree globals (all)
  simple die tree locals (all)

  read-dwarf instruction count
  read-dwarf globals
  read-dwarf struct/union type definitions
  read-dwarf call graph
  read-dwarf transitive call graph

  subprogram line-number extent info

per compilation unit:

  read-dwarf instructions
  abbreviation table
  die tree
  .debug_line section: line number info
  evaluated line number info
  simple die tree
  simple die tree globals
  simple die tree locals
  inlined subroutine info
  inlined subroutine info (by range)

links:

  branch target: link to that line in the read-dwarf instructions. If a function start address, also link to the simple die tree and to the die tree
  struct/union/enum type definition: link to that entry in the read-dwarf type defns and to the source file decl and to the die tree
  source line: link somehow to that line in the sources
  function 


   *)

let skylight () =
  match !Globals.comp_dir with
  | None -> (warn "skylight: no --comp_dir"; "skylight: no --comp_dir")
  | Some comp_dir -> (
      match !Globals.out_dir with
      | None -> fatal "skylight: no --out_dir"
      | Some out_dir -> (
          let filename = out_dir ^ ".files" in
          sys_command
            ("find " ^ comp_dir ^ " -type f | while read f; do echo ${f} ; done > " ^ filename);
          match read_file_lines filename with
          | Error s -> fatal "skylight: %s" s
          | Ok files ->
              let links =
                List.map
                  (function
                    | file ->
                        let target = Filename.basename file ^ ".html" in
                        if !Globals.skylight then
                          sys_command
                            ("skylighting -n " ^ file ^ " > " ^ Filename.concat out_dir target)
                        else ();
                        let link = "@<a href=\"" ^ target ^ "\">" ^ file ^ "</a>@\n" in
                        link)
                  (Array.to_list files)
              in
              let chunk_body = String.concat "" links in
              chunk_body
        )
    )

let chunk_filename_whole m _filename_stem chunk_name : string (*path*) * string (*file*) =
  ( "",
    (* filename_stem
       ^*)
    chunk_name ^ match m with Ascii -> "" | Html -> ".html" )

let chunk_filename_per_cu m _filename_stem chunk_name cu : string (*path*) * string (*file*) =
  let _dirname = Filename.dirname cu.Dwarf.scu_name in
  let basename = Filename.basename cu.Dwarf.scu_name in
  let extension = Filename.extension basename in
  let basename_chopped = Filename.chop_extension basename in
  let extension_mangled = String.map (function c -> if c = '.' then '_' else c) extension in
  let cu_name = basename_chopped ^ extension_mangled in
  ( "",
    (*filename_stem
      ^ "_"
      ^ *)
    cu_name ^ "_" ^ chunk_name ^ match m with Ascii -> "" | Html -> ".html" )

let wrap_chunks m chunks =
  List.map
    (function
      | (chunk_name, chunk_title, chunk_body) -> (chunk_name, chunk_title, esc m chunk_body))
    chunks

let whole_file_chunks m test an filename_stem cu_files =
  (*  let pr s = prerr_endline s in*)
  (*  let ps s = ((prerr_endline s); s)  in *)
  let pr _ = () in
  let ps s = s in
  let open Dwarf in
  let ds = test.dwarf_static in
  let d = ds.ds_dwarf in
  let c = p_context_of_d d in
  let (cuh_default : compilation_unit_header) =
    let cu = myhead d.d_compilation_units in
    cu.cu_header
  in
  pr "1";
  let iss = analyse_inlined_subroutines_sdt_dwarf an.sdt in
  pr "2";
  let sources_chunk_body = skylight () in
  pr "3";
  let cu_index_body =
    String.concat ""
      (List.map
         (function[@ocaml.warning "-8"] (* inexhaustive pattern match *)
           | (_path, filename, cu_title, _chunk_title) :: _ as _per_cu_files -> (
               match m with
               | Ascii -> cu_title ^ " " ^ filename ^ "\n"
               | Html ->
                   "<a href=\"" ^ filename ^ "\">" ^ cu_title ^ "</a> "
                   (*^ "<a href=\""
                     ^ filename ^ "\">" ^ chunk_title ^ "</a> "*)
                   (*    ^ "<a href=\"" ^ filename' ^ "\">" ^ chunk_title' ^ "</a>"*)
                   ^ "\n"
             ))
         cu_files)
  in
  pr "4";
  let chunks =
    (* ("compilation_units"             , "compilation units",         cu_index_body)
       ::*)
    wrap_chunks m
      (
        [ (ps "_loc", ".debug_loc location lists", pp_loc c cuh_default d.d_loc)]
        @ (if not(!Globals.suppress_stuff) then 
             [  (ps "_loc_eval", "evaluated location info",
                 pp_analysed_location_data ds.ds_dwarf ds.ds_analysed_location_data ) ] else [] )
        @ [
            (ps "_ranges", ".debug_ranges range lists", pp_ranges c cuh_default d.d_ranges);
            (ps "_frame", ".debug_frame frame info", pp_frame_info c cuh_default d.d_frame_info);
            (ps "_frame_eval", "evaluated frame info", pp_evaluated_frame_info ds.ds_evaluated_frame_info);
            (ps "_inlined", "inlined subroutine info", pp_inlined_subroutines ds iss);
            (ps "_inlined_by_range",
             "inlined subroutine info by range",
             pp_inlined_subroutines_by_range ds (analyse_inlined_subroutines_by_range iss) );
            (ps "_sdt_globals", "simple die tree globals", pp_sdt_globals_dwarf an.sdt);
            (ps "_sdt_locals", "simple die tree locals", pp_sdt_locals_dwarf an.sdt);
            (*    ("subprogram_line_extents" , "subprogram line-number extent info", Dwarf.pp_subprograms ds.ds_subprogram_line_extents);*)
            (ps "_types",
             "struct/union/enum type definitions",
             let ctyps : Dwarf.c_type list = Dwarf.struct_union_enum_types d in
             String.concat "" (List.map Dwarf.pp_struct_union_type_defn' ctyps) )
          ]
        @
          (
            let (call_graph, transitive_call_graph) = pp_call_graph test an in
            [ (ps "_call_graph", "call graph", call_graph);
              (ps "_call_graph_trans", "transitive call graph", transitive_call_graph) ]
          )
        @  [(ps "_count", "instruction count", string_of_int (Array.length an.instructions)) ]
      )
  in
  let index_chunk =
    (ps "index",
      "index",
      "<a href=\"sources.html\">source files</a>\n\n" ^ cu_index_body ^ "\n"
      ^ String.concat ""
          (List.map
             (function
               | (chunk_name, chunk_title, _chunk_body) -> (
                   let (_path, filename) = chunk_filename_whole m filename_stem chunk_name in
                   match m with
                   | Ascii -> chunk_title ^ " " ^ filename
                   | Html -> "<a href=\"" ^ filename ^ "\">" ^ chunk_title ^ "</a>\n"
                 ))
             chunks) )
  in
  let sources_chunk = ("sources", "source files", sources_chunk_body) in
  (index_chunk :: wrap_chunks m [sources_chunk]) @ chunks

(*      
      ^ "\n************** .debug_line section: line number info   ****************\n"
  ^ pp_line_info d.d_line_info
       ^ "************** simple die tree *************************\n"
       ^        pp_sdt_dwarf sdt_d
     ^ "************** line info *************************\n"
     ^ pp_evaluated_line_info ds.ds_evaluated_line_info
 *)

(* strangely, the pc range data for some CUs seems not to cover the objdump-printed disassembly, e.g. in the last CU of noub.elf *)
let pp_instructions_ranged m test an (low, high) =
  (* [ (f k a.(k)) ; (f (k+1) a.(k+1)) ; ... ; (f k' a.(k'-1)) ] *)
(*  Printf.printf "pp_instructions_ranged size=%i low=%s  high=%s \n" (Array.length an.instructions) (pp_addr low)  (pp_addr high) ;
  Printf.printf "pp_instructions_ranged indices: low=%i  high=%i \n"   (an.index_of_address low)  (an.index_of_address high);
 *)
  let index_low = an.index_of_address low in
  let index_high = (an.index_of_address (Sym.sub high (Sym.of_int 4)))+1 in
  let rec subarray_map_to_list f a k k' =
    if k >= k' then [] else f k a.(k) :: subarray_map_to_list f a (k + 1) k'
  in
  let rendered_control_flow_common_prefix_end =
    ControlFlowPpText.common_prefix_end
      (subarray_map_to_list
         (function
           | k -> (
               function _i -> an.rendered_control_flow.(k)
             ))
         an.instructions index_low index_high
      @ subarray_map_to_list
          (function
            | k -> (
                function _i -> an.rendered_control_flow_inbetweens.(k)
              ))
         an.instructions index_low index_high
      )
  in

  pp_instruction_init ();
  String.concat ""
    (subarray_map_to_list
       (pp_instruction m test an rendered_control_flow_common_prefix_end)
       an.instructions index_low index_high)

let chunks_of_ranged_cu m test an filename_stem ((low, high), cu) =
  let open Dwarf in
  let ds = test.dwarf_static in
  let d = ds.ds_dwarf in
  let c = p_context_of_d d in
  let (cu', _, _) = cu.scu_cupdie in
  let iss = analyse_inlined_subroutines_sdt_compilation_unit cu in
  let title = "Compilation unit " ^ pp_addr low ^ " " ^ pp_addr high ^ " " ^ cu.Dwarf.scu_name in
  let chunks0 =
    wrap_chunks m
      [
        (* chunk name, title, body *)
        ("header", "header", pp_compilation_unit_header cu'.cu_header);
        ( "die_abbrev",
          ".debug_abbrev die abbreviation table",
          pp_abbreviations_table cu'.cu_abbreviations_table );
        ( "die",
          ".debug_info die tree",
          pp_die c cu'.cu_header d.d_str true (*indent*) (Nat_big_num.of_int 0) true cu'.cu_die );
        ( "line",
          ".debug_line line number info",
          let lnp = line_number_program_of_compilation_unit d cu' in
          pp_line_number_program lnp );
        ( "line_eval",
          ".debug_line evaluated line info",
          let lnrs = evaluated_line_info_of_compilation_unit d cu' ds.ds_evaluated_line_info in
          pp_line_number_registerss lnrs );
        ("sdt", "simple die tree", pp_sdt_compilation_unit (Nat_big_num.of_int 0) cu);
        ( "sdt_globals",
          "simple die tree globals",
          pp_sdt_globals_compilation_unit (Nat_big_num.of_int 0) cu );
        ( "sdt_locals",
          "simple die tree locals",
          pp_sdt_locals_compilation_unit (Nat_big_num.of_int 0) cu );
        ("inlined", "inlined subroutine info", pp_inlined_subroutines ds iss);
        ( "inlined_by_range",
          "inlined subroutine info by range",
          pp_inlined_subroutines_by_range ds (analyse_inlined_subroutines_by_range iss) );
      ]
  in
  let index_body =
    String.concat ""
      (List.map
         (function
           | (chunk_name, chunk_title, _chunk_body) -> (
               let (_path, filename) = chunk_filename_per_cu m filename_stem chunk_name cu in
               match m with
               | Ascii -> chunk_title ^ " " ^ filename
               | Html -> "<a href=\"" ^ filename ^ "\">" ^ chunk_title ^ "</a>\n"
             ))
         chunks0)
  in
  (* let index_chunk =
     ( "index",
       "index",
       index_body) *)
  let instructions_chunk =
    ( "instructions",
      "instructions",
      index_body ^ "\n" ^ pp_instructions_ranged m test an (low, high) )
  in

  (title, instructions_chunk :: chunks0)

let wrap_body m (chunk_name, chunk_title, chunk_body) =
  let read_html name =
    let rec inter_p = function
      | [] -> Error "not found"
      | dir::dirs ->
         let filename = Filename.concat dir name  in
         if Sys.file_exists filename
         then read_file_lines filename
         else inter_p dirs
    in inter_p (Htmlpaths.Sites.html)
  in
  match m with
  | Ascii ->
      ( if chunk_name = "instructions" then
        match read_html "emacs-highlighting" with
        | Error _ -> "Error: src/analyse/no emacs-highlighting file\n"
        | Ok lines -> String.concat "\n" (Array.to_list lines)
      else ""
      )
      ^ "* ************* " ^ chunk_title ^ " **********\n" ^ chunk_body
  | Html -> (
      ( if chunk_name = "instructions" then
        match read_html "html-preamble-insts.html" with
        | Error _ -> "Error: src/analyse/no html-preamble-insts.html file\n"
        | Ok lines -> String.concat "\n" (Array.to_list lines)
      else
        match read_html "html-preamble.html" with
        | Error _ -> "Error: no src/analyse/html-preamble.html file\n"
        | Ok lines -> String.concat "\n" (Array.to_list lines)
      )
      ^ "<h1>" ^ chunk_title ^ "</h1>\n" ^ chunk_body
      ^
      match read_html "html-postamble.html" with
      | Error _ -> "Error: no src/analyse/html-postamble.html file\n"
      | Ok lines -> String.concat "\n" (Array.to_list lines)
    )

let output_file (path, filename, body) =
  let _out_dir = match !Globals.out_dir with Some s -> s | None -> "" in
  (*sys_command ("mkdir -p " ^ Filename.quote out_dir);*)
  let c =
    match !Globals.out_dir with
    | Some out_dir -> open_out (Filename.concat out_dir (Filename.concat path filename))
    | None -> stdout
  in
  Printf.fprintf c "%s" body;
  match !Globals.out_dir with Some _out_dir -> close_out c | None -> ()

let output_whole_file_files m test an filename_stem cu_files =
  let chunks = whole_file_chunks m test an filename_stem cu_files in
  List.iter
    (function
      | (chunk_name, chunk_title, chunk_body) ->
          let (path, filename) = chunk_filename_whole m filename_stem chunk_name in
          let body = wrap_body m (chunk_name, chunk_title, chunk_body) in
          output_file (path, filename, body))
    chunks

let output_per_cu_files m test an filename_stem re_ranged_compilation_units =
  List.map
    (function
      | ((low, high), cu) ->
         prerr_endline ("output_per_cu_files cu " ^ cu.Dwarf.scu_name);
         let (cu_title, chunks) =
            chunks_of_ranged_cu m test an filename_stem ((low, high), cu)
          in
          List.map
            (function
             | (chunk_name, chunk_title, chunk_body) ->
                (*                prerr_endline ("output_per_cu_files chunk_name " ^ chunk_name);*)
                  let (path, filename) = chunk_filename_per_cu m filename_stem chunk_name cu in
                  let body =
                    wrap_body m (chunk_name, cu_title ^ "\n" ^ chunk_title, chunk_body)
                  in
                  output_file (path, filename, body);
                  (path, filename, cu_title, chunk_title))
            chunks)
    re_ranged_compilation_units

let pp_test_analysis m test an =
  (* pick address ranges for each compilation unit.  In pkvm all compilation units currently have exactly one range, the lowest-address range starts at the start of the code, and they happen to be in address order (though I don't want to depend on that). But these ranges are not contiguous, so instead we'll use the range from the start of one to the start of the next, except for the last *)
  ( match !Globals.out_dir with
  | None -> ()
  | Some _ ->
      let _rangeless_compilation_units : Dwarf.sdt_compilation_unit list =
        List.concat_map
          (function
            | (cu : Dwarf.sdt_compilation_unit) -> (
                match cu.scu_pc_ranges with None -> [cu] | Some _ranges -> []
              ))
          an.sdt.Dwarf.sd_compilation_units
      in
      let ranges_of_compilation_units : ((addr * addr) * Dwarf.sdt_compilation_unit) list =
        List.concat_map
          (function
            | (cu : Dwarf.sdt_compilation_unit) -> (
                match cu.scu_pc_ranges with
                | None -> []
                | Some ranges -> List.map (function (low, high) ->
                                             (*                                             Printf.printf "CU %s range %s : %s %s\n" cu.scu_name (Dwarf.pp_cupdie cu.scu_cupdie) (pp_addr low) (pp_addr high) ; flush stdout; *)
                                             ((low, high), cu)) ranges
              ))
          an.sdt.Dwarf.sd_compilation_units
      in
      let ranges_of_compilation_units' : ((addr * addr) * Dwarf.sdt_compilation_unit) list =
        List.sort
          (function
            | ((low, _high), _cu) -> (
                function ((low', _high'), _cu') -> compare low low'
              ))
          ranges_of_compilation_units
      in
(*
      let re_ranged_compilation_units : ((addr * addr) * Dwarf.sdt_compilation_unit) list =
        let rec f (rcus : ((addr * addr) * Dwarf.sdt_compilation_unit) list) :
            ((addr * addr) * Dwarf.sdt_compilation_unit) list =
          match rcus with
          | ((low, _high), cu) :: (((low', _high'), _cu') :: _ as rcus') ->
              ((low, low'), cu) :: f rcus'
          | [((low, _high), cu)] ->
              [((low, an.instructions.(Array.length an.instructions - 1).i_addr), cu)]
          | [] -> []
        in
        f ranges_of_compilation_units'
      in
 *)
      (* HACK: for pKVM the above re-ranging was useful for some reason, but in general not *)
      let re_ranged_compilation_units = ranges_of_compilation_units' in

      let filename_stem = "" in

      let cu_files = output_per_cu_files m test an filename_stem re_ranged_compilation_units in

      (*      if not(!Globals.suppress_whole_file_files) then*)
      output_whole_file_files m test an filename_stem cu_files
(*      else
        ()*)
      (*

  String.concat ""
    (List.map
       (function cu -> "no range " ^ cu.Dwarf.scu_name ^ "\n")
       rangeless_compilation_units)
  ^ String.concat ""
      (List.map
         (function
           | ((low, high), cu) ->
               pp_addr low ^ " " ^ pp_addr high ^ " " ^ cu.Dwarf.scu_name ^ "\n")
         ranges_of_compilation_units')
     *)
      (*
  ^
    let chunks = chunks_of_ranged_compilation_units m test an 

    String.concat ""
      (List.map

'''''

         re_ranged_compilation_units)
 *)
  );
  match m with
  | Ascii ->
      "* ************* instruction count *****************\n"
      ^ string_of_int (Array.length an.instructions)
      ^ " instructions\n" ^ "* ************* globals *****************\n"
      ^ pp_vars an.ranged_vars_at_instructions.rvai_globals
      (* ^ "************** locals *****************\n"
         ^ pp_ranged_vars
      *)
      ^ "\n* ************* instructions *****************\n"
      ^ ( pp_instruction_init ();
          String.concat ""
            (Array.to_list (Array.mapi (pp_instruction m test an 0) an.instructions))
        )
      ^ "* ************* struct/union/enum type definitions *****************\n"
      ^ (let d = test.dwarf_static.ds_dwarf in

         (* let c = Dwarf.p_context_of_d d in
            Dwarf.pp_all_aggregate_types c d*)
         (*     Dwarf.pp_all_struct_union_enum_types' d)*)
         let ctyps : Dwarf.c_type list = Dwarf.struct_union_enum_types d in
         String.concat "" (List.map Dwarf.pp_struct_union_type_defn' ctyps))
      (*  ^ "\n* ************* branch targets *****************\n"*)
      (*  ^ pp_branch_targets instructions*)
      ^ "\n* ************* call graph *****************\n"
      ^
      let (call_graph, transitive_call_graph) = pp_call_graph test an in
      call_graph ^ "* ************* transitive call graph **************\n"
      ^ transitive_call_graph
  | Html ->
     "\n* ************* instructions *****************\n"
     ^ (pp_instruction_init ();
      String.concat "" (Array.to_list (Array.mapi (pp_instruction m test an 0) an.instructions)))
