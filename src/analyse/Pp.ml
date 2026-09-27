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
  | Render_alternative
  | Render_jump_label

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
  | Render_alternative -> "yellowgreen"
  | Render_jump_label -> "hotpink"

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
  | Render_alternative -> "alternative"
  | Render_jump_label -> "jump-label"

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

(* the come_froms for an instruction, other than plain fallthrough, which determine whether it is the start of a basic block *)
let come_froms_of an k = List.filter (function cf -> cf.cf_target_kind <> T_plain_successor) an.come_froms.(k)

(* Claude: pp_instruction_plain is now split into a prefix (symbols, source,
   frame and variable information before the instruction), the instruction
   line itself, and a suffix (variables going out of scope), so that the
   alternatives rendering can put the footprint of an entry together *)
(* the parameters of a function starting at this instruction, as "+ f params:" lines *)
let pp_instruction_params m an i =
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
  css m Render_vars (pp_params i.i_addr an.ranged_vars_at_instructions.rvai_params)

(* Claude: the variables whose location ranges start at instruction k, as
   "+" lines; the alternatives rendering puts these (and the params lines)
   before a block as a whole rather than inside it *)
let pp_instruction_vars_new m an k =
  if !Globals.show_vars then
    css m Render_vars_new (pp_ranged_vars "+" an.ranged_vars_at_instructions.rvai_new.(k))
    (*        ^ pp_ranged_vars "C" an.ranged_vars_at_instructions.rvai_current.(k)*)
    (*        ^ pp_ranged_vars "R" an.ranged_vars_at_instructions.rvai_remaining.(k)*)
  else ""

(* Claude: the inlining header lines of instruction k (the inlined
   subroutines whose ranges start here, labelled), if any *)
let pp_instruction_inlining_header m an k =
  let (_, ppd_new_inlining, _) = an.inlining.(k) in
  css m Render_inlining ppd_new_inlining

(* Claude: ~in_block:true leaves out the params, inlining header and variable
   lines, which the alternatives rendering prints before the block instead *)
let pp_instruction_prefix ?(in_block = false) m test an rendered_control_flow_common_prefix_end k i =
  let addr = i.i_addr in
  let come_froms' = come_froms_of an k in

  (* the inlining for this instruction *)
  let (ppd_labels, _, _) = an.inlining.(k) in

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
  ^ (if in_block then "" else pp_instruction_params m an i)
  (* the new inlining info for this address *)
  ^ (if in_block then "" else pp_instruction_inlining_header m an k)
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
  ^ (if in_block then "" else pp_instruction_vars_new m an k)


(* Claude: the value of the relocation at instruction address [a], as linksem's
   symbolic expression over section bases (the ABI's formula for the
   relocation type, e.g. S + A - P, or Page(S + A) - Page(P) for adrp), for
   printing after the relocation objdump shows; "" if there is none in the
   ELF or the file is not relocatable *)
let pp_relocation_value test (a : addr) : string =
  match (test.elf_file, a) with
  | (Elf_file.ELF_File_64 f64, Sym_ocaml.Num.Offset (section, off)) -> (
      match SymbolicReloc.section_relocations f64 section with
      | None -> ""
      | Some tbl -> (
          match Hashtbl.find_opt tbl (Z.to_int off) with
          | None -> ""
          | Some (rel : SymbolicReloc.relocation) ->
              (* printed as the "words written" expressions are, via Symbolic_resolution's printer,
                 and evaluated under the recorded environment if there is one *)
              let e = Symbolic_resolution.sym_expr_of_symbolic_expression rel.rel_desc_value in
              "  = "
              ^ Symbolic_resolution.string_of_sym_expr (Symbolic_resolution.simplify_sym_expr e)
              ^ ResolutionEnv.pp_resolved ~width:8 e ) )
  | _ -> ""

(* Claude: the relocation objdump shows on an instruction, followed by its
   value as linksem's symbolic expression *)
let pp_relocation test (a : addr) (r : (string * string) option) : string =
  match r with None -> "" | Some (typ, targ) -> "\t" ^ typ ^ " " ^ targ ^ pp_relocation_value test a

let pp_instruction_line m test an rendered_control_flow_common_prefix_end k i =
  let addr = i.i_addr in
  let come_froms' = come_froms_of an k in
  let (ppd_labels, _, _) = an.inlining.(k) in
  (* the inlining label prefix *)
  css m Render_inlining
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
  ^ css m Render_relocation (pp_relocation test addr i.i_relocation)
  (* Claude: a static-key test site, or a jump-label branch target, from the jump table *)
  ^ css m Render_jump_label
      (match an.jump_table with
      | None -> ""
      | Some t ->
          String.concat ""
            (List.map (fun e -> "\t" ^ JumpTablePp.pp_site_with_word t e) (JumpTable.at_code t addr)
            @ List.map (fun e -> "\t" ^ JumpTablePp.pp_target e) (JumpTable.at_target t addr)))
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
         | _ -> (
             (* Claude: targets with no instruction here to carry a come-from: branches to
                symbols undefined in this object, and to addresses outside the objdump *)
             match
               List.filter_map
                 (function
                   | (T_external _, _, _, s) -> Some (s ^ "(external)")
                   | (T_out_of_range a', _, _, s) -> Some (pp_addr a' ^ s ^ "(out-of-range)")
                   | _ -> None)
                 i.i_targets
             with
             | [] -> ""
             | ts -> " -> " ^ String.concat "," ts ^ " " )
       end
      (* any control flow to this instruction *)
      ^ pp_come_froms addr come_froms'
      ^ "\n"
      )

(* the variables whose location ranges end after instruction k, as "-" lines *)
let pp_instruction_suffix m an k =
  if (*true*) !Globals.show_vars then
    if k < Array.length an.instructions - 1 then
      css m Render_vars_old (pp_ranged_vars "-" an.ranged_vars_at_instructions.rvai_old.(k + 1))
    else ""
  else ""

(* Claude: ~in_block:true leaves out the params, inlining header and "+"/"-"
   variable lines, for the alternatives rendering, which prints them around
   the block instead *)
let pp_instruction_plain ?(in_block = false) m test an rendered_control_flow_common_prefix_end k i =
  pp_instruction_prefix ~in_block m test an rendered_control_flow_common_prefix_end k i
  ^ pp_instruction_line m test an rendered_control_flow_common_prefix_end k i
  ^ if in_block then "" else pp_instruction_suffix m an k

(* Claude: other commands (run-func-rd, rel-prog) render single instructions
   by index; they get the plain rendering, without alternatives *)
let pp_instruction = pp_instruction_plain

(*****************************************************************************)
(**       pretty-print alternatives groups                                   *)

(*****************************************************************************)

(* Claude: alternatives entries based at this instruction that are not
   rendered as a block (empty footprint, footprint not contiguous in the
   objdump, or base inside the footprint of the block being rendered), as
   !! lines before the instruction *)
let pp_ungrouped_alternatives m an ~(in_group : AlternativesType.entry list) (i : instruction) =
  match an.alternatives with
  | None -> ""
  | Some t ->
      let here = try AlternativesType.SymMap.find i.i_addr t.by_orig with Not_found -> [] in
      let others = List.filter (fun e -> not (List.memq e in_group)) here in
      String.concat ""
        (List.map
           (fun (e : AlternativesType.entry) ->
             let reason =
               if AlternativesType.nr_inst e = 0 then "empty footprint"
               else if in_group <> [] then "its base is inside the enclosing alternative"
               else "footprint not contiguous in the objdump"
             in
             css m Render_alternative
               ("!! alternative not rendered as a block (" ^ reason ^ "): " ^ AlternativesPp.pp_condition e ^ "\n"))
           others)

(* Claude: a replacement instruction shown at the original site it is
   copied to: the objdump's disassembly of it at the replacement address,
   re-addressed.  What patch_alternative() in arch/arm64/kernel/alternative.c
   actually writes there (an immediate branch or adrp re-targeted when its
   target lies outside the block) is linksem's model's business, rendered
   from its words below the instructions *)
let rebase_replacement_instruction ~(orig : addr) ~(alt : addr) (r : instruction) : instruction =
  { r with i_addr = Sym.add orig (Sym.sub r.i_addr alt); i_targets = [] }

(* Claude: the word of an objdump instruction, as objdump prints it *)
let word_of_opcode (opcode : int list) : Z.t =
  List.fold_left (fun acc b -> Z.add (Z.shift_left acc 8) (Z.of_int b)) Z.zero opcode

(* Claude: the note on a replacement line from linksem's word for it: nothing
   if the word is the instruction as assembled, else what changes *)
let replacement_note (r : instruction option) (w : Symbolic_resolution.sym_expr option) : string =
  match (r, w) with
  | (Some r, Some w) -> (
      match AlternativesPp.word_constant w with
      | Some x when Z.equal x (word_of_opcode r.i_opcode) -> ""
      | Some x -> "re-encoded by patch_alternative(): 0x" ^ Z.format "%08x" x
      | None -> "re-targeted by patch_alternative(), see the words below" )
  | _ -> ""

(* Claude: the blank inlining and control-flow columns of a line inside a
   block's "---" part, of the same width as those of the default instruction
   at index k_ref, so that its address starts in the address column *)
let pp_blank_prefix m an rendered_control_flow_common_prefix_end k_ref =
  let blank_glyphs = Array.make (Array.length an.rendered_control_flow.(k_ref)) Gnone in
  css m Render_inlining
    ("~"
    ^
    let s = an.pp_inlining_label_prefix "" in
    String.sub s 1 (String.length s - 1)
    )
  ^ css m Render_ctrlflow (ControlFlowPpText.pp_glyphs rendered_control_flow_common_prefix_end blank_glyphs)

(* Claude: the words linksem's model says the entry writes, one line per
   instruction of the footprint, in the address column: a constant as a hex
   word, otherwise a summary of what it depends on, with the whole
   expression on a second line; or why linksem cannot say *)
let pp_linksem_words m an rendered_control_flow_common_prefix_end k_ref (t : AlternativesType.table)
    (e : AlternativesType.entry) =
  let base = AlternativesType.orig_addr e in
  let section = match base with Sym_ocaml.Num.Offset (s, _) -> s | Sym_ocaml.Num.Absolute _ -> "" in
  let prefix = pp_blank_prefix m an rendered_control_flow_common_prefix_end k_ref in
  match AlternativesType.words t e with
  | Error why -> css m Render_alternative ("!! linksem cannot give the words written: " ^ why ^ "\n")
  | Ok (words, _checks) ->
      css m Render_alternative "words written (linksem's model):\n"
      ^ String.concat ""
          (List.map
             (fun (off, w) ->
               prefix
               ^ css m Render_alternative
                   (Printf.sprintf "%s:  %s%s\n"
                      (AlternativesPp.pp_addr (Sym_ocaml.Num.Offset (section, off)))
                      (AlternativesPp.pp_word_summary w)
                      (match AlternativesPp.word_constant w with Some _ -> "" | None -> ResolutionEnv.pp_resolved w))
               ^
               match AlternativesPp.word_constant w with
               | Some _ -> ""
               | None ->
                   prefix
                   ^ css m Render_alternative
                       ("    " ^ Symbolic_resolution.string_of_sym_expr (Symbolic_resolution.simplify_sym_expr w) ^ "\n"))
             words)

(* Claude: one replacement instruction, in the layout of pp_instruction_line
   but with blank inlining and control-flow columns (of the same width as
   those of the default instruction at index k_ref) *)
let pp_replacement_line m test an rendered_control_flow_common_prefix_end k_ref ~(assembled_at : addr) (r : instruction)
    (note : string) =
  pp_blank_prefix m an rendered_control_flow_common_prefix_end k_ref
  ^ css m Render_alternative
      (pp_addr r.i_addr ^ ":  " ^ pp_opcode_bytes test.arch r.i_opcode ^ "  " ^ r.i_mnemonic ^ "\t" ^ r.i_operands)
  ^ css m Render_relocation (pp_relocation test assembled_at r.i_relocation)
  ^ css m Render_alternative ((if note = "" then "" else "  [" ^ note ^ "]") ^ "\n")

(* Claude: the "---" part of a block: what one entry does when applied: the
   callback or the replacement instructions (from the objdump), then the
   words linksem's model says are written *)
let pp_alternative_action m test an rendered_control_flow_common_prefix_end (ks : int list) (e : AlternativesType.entry) =
  let fetch a = Option.map (fun k -> an.instructions.(k)) (an.index_option_of_address a) in
  let k_ref = List.hd ks in
  let t = Option.get an.alternatives in
  css m Render_alternative "---\n"
  ^
  match AlternativesAction.action_of_entry ~fetch e with
  | AlternativesAction.Act_callback { callback; kind; _ } ->
      css m Render_alternative
        ("callback " ^ callback
        ^ (match AlternativesPp.describe_callback kind with Some d -> ": " ^ d | None -> "")
        ^ "\n")
      ^ pp_linksem_words m an rendered_control_flow_common_prefix_end k_ref t e
  | AlternativesAction.Act_unresolved { why; _ } -> css m Render_alternative ("unresolved action: " ^ why ^ "\n")
  | AlternativesAction.Act_replace { replacement_addr; replacement; _ } ->
      let orig = AlternativesType.orig_addr e in
      let words = match AlternativesType.words t e with Ok (ws, _) -> List.map snd ws | Error _ -> [] in
      let word_at j = List.nth_opt words j in
      css m Render_alternative
        (Printf.sprintf "replacement (assembled at %s, shown as patched in at %s):\n" (pp_addr replacement_addr)
           (pp_addr orig))
      ^ String.concat ""
          (List.mapi
             (fun j (a, ro) ->
               match ro with
               | None -> css m Render_alternative (pp_addr a ^ ":  <no instruction at this address in the objdump>\n")
               | Some r ->
                   let r' = rebase_replacement_instruction ~orig ~alt:replacement_addr r in
                   pp_replacement_line m test an rendered_control_flow_common_prefix_end k_ref ~assembled_at:a r'
                     (replacement_note ro (word_at j)))
             replacement)
      ^ pp_linksem_words m an rendered_control_flow_common_prefix_end k_ref t e

(* Claude: render one instruction group: a single instruction as before, or
   an alternatives footprint as a block: the params and "+" variable lines
   of all its instructions, then their inlining header lines, the header
   with the conditions, the default instructions rendered as usual but
   without those lines, then for each entry a "---" part with its
   replacement or callback, the footer, and the "-" variable lines of all
   its instructions *)
let pp_group m test an rendered_control_flow_common_prefix_end (g : instruction_group) =
  match g with
  | G_single k ->
      let i = an.instructions.(k) in
      pp_ungrouped_alternatives m an ~in_group:[] i ^ pp_instruction_plain m test an rendered_control_flow_common_prefix_end k i
  | G_alternative (es, ks) ->
      (* the location-info lines of the footprint's instructions, then their inlining headers *)
      String.concat ""
        (List.map (fun k -> pp_instruction_params m an an.instructions.(k) ^ pp_instruction_vars_new m an k) ks)
      ^ String.concat "" (List.map (pp_instruction_inlining_header m an) ks)
      ^ css m Render_alternative
          ("---alternative---\n" ^ String.concat "" (List.map (fun e -> AlternativesPp.pp_condition e ^ "\n") es))
      ^ String.concat ""
          (List.map
             (fun k ->
               let i = an.instructions.(k) in
               pp_ungrouped_alternatives m an ~in_group:es i
               ^ pp_instruction_plain ~in_block:true m test an rendered_control_flow_common_prefix_end k i)
             ks)
      ^ String.concat "" (List.map (pp_alternative_action m test an rendered_control_flow_common_prefix_end ks) es)
      ^ css m Render_alternative "---end---\n"
      ^ String.concat "" (List.map (pp_instruction_suffix m an) ks)

(* Claude: render the groups covering instruction indices [index_low, index_high).
   A footprint that starts before index_low was rendered as a block with the
   range containing its base; here its instructions in range are rendered
   plainly, after a note.  A footprint that starts in range but extends past
   index_high is rendered whole *)
let pp_groups_ranged m test an rendered_control_flow_common_prefix_end index_low index_high =
  if index_high <= index_low then ""
  else
    let g_lo = an.group_of_index.(index_low) and g_hi = an.group_of_index.(index_high - 1) in
    let rec loop g =
      if g > g_hi then []
      else
        ( match an.instruction_groups.(g) with
        | G_single _ as gr -> pp_group m test an rendered_control_flow_common_prefix_end gr
        | G_alternative (_, ks) as gr ->
            if List.hd ks >= index_low then pp_group m test an rendered_control_flow_common_prefix_end gr
            else
              css m Render_alternative
                ("(continuation of the alternative footprint starting at "
                ^ pp_addr an.instructions.(List.hd ks).i_addr
                ^ ", rendered there)\n")
              ^ String.concat ""
                  (List.filter_map
                     (fun k ->
                       if k >= index_low && k < index_high then
                         Some (pp_instruction_plain m test an rendered_control_flow_common_prefix_end k an.instructions.(k))
                       else None)
                     ks) )
        :: loop (g + 1)
    in
    String.concat "" (loop g_lo)

let pp_groups_all m test an =
  String.concat "" (Array.to_list (Array.map (pp_group m test an 0) an.instruction_groups))

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

(* Claude: the source files referenced by the line-number information of
   the instructions, as the paths the source lines are read from (the DWARF
   directory with --comp-dir substituted), deduplicated and sorted by path.
   Formerly the sources page listed every file under --comp-dir, found with
   `find`: 96k files and a 14 MB page for a kernel tree *)
let referenced_source_files an : ((string option * string option * string) * string) list =
  let tbl = Hashtbl.create 256 in
  Array.iter
    (fun elifis ->
      List.iter
        (fun elifi ->
          let lnh = elifi.elifi_entry.elie_lnh in
          let lnr = elifi.elifi_entry.elie_lnr in
          let ufe = Dwarf.unpack_file_entry lnh lnr.lnr_file in
          if not (Hashtbl.mem tbl ufe) then begin
            let (_directory_original, directory_replacement) = actual_directories !Globals.comp_dir ufe in
            let (_, _, file) = ufe in
            (* Claude: tidy the "/./" that DWARF directory names such as "./arch/..." leave *)
            let path = Filename.concat directory_replacement file in
            let path = Str.global_replace (Str.regexp_string "/./") "/" path in
            Hashtbl.replace tbl ufe path
          end)
        elifis)
    an.line_info;
  List.sort (fun (_, p1) (_, p2) -> compare p1 p2) (Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl [])

(* Claude: the sources page: a link per referenced source file, to the file
   itself (relative to --out-dir, as for the source-line links) or, with
   --skylight, to a highlighted copy made here; also writes the list of
   paths to <out-dir>.files *)
let skylight an =
  match !Globals.out_dir with
  | None -> fatal "sources page: no --out-dir"
  | Some out_dir ->
      let files = referenced_source_files an in
      let skylight = !Globals.skylight in
      let c = open_out (out_dir ^ ".files") in
      List.iter (fun (_, path) -> Printf.fprintf c "%s\n" path) files;
      close_out c;
      String.concat ""
        (List.map
           (fun (ufe, path) ->
             let target =
               if skylight then begin
                 let target = Filename.basename path ^ ".html" in
                 (* Claude: -f html: skylighting's default output format is ANSI terminal colouring;
                    its html line numbers carry id="N" anchors, matching the #N fragments of the
                    source-line links *)
                 sys_command
                   ("skylighting -f html -n " ^ Filename.quote path ^ " > " ^ Filename.quote (Filename.concat out_dir target));
                 target
               end
               else source_href ufe
             in
             html_escape_toggle ^ "<a href=\"" ^ target ^ "\">" ^ path ^ "</a>" ^ html_escape_toggle ^ "\n")
           files)

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
  let sources_chunk_body = skylight an in
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
        (* Claude: the kinds of alternatives in the whole object, if it has alternatives data *)
        @ ( match an.alternatives with
          | None -> []
          | Some t ->
              [ (ps "_alternative_kinds", "alternative kinds",
                 AlternativesPp.pp_alternative_kinds (Array.to_list t.AlternativesType.entries)) ] )
        (* Claude: the static keys tested in the whole object, if it has a jump table *)
        @ ( match an.jump_table with
          | None -> []
          | Some t ->
              [ (ps "_static_keys", "static keys", JumpTablePp.pp_static_keys (Array.to_list t.JumpTable.entries)) ] )
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
  (* Claude: a CU whose code was discarded (e.g. by ld -r with a linker
     script) has an absolute pc range that matches no instruction; say so
     rather than failing *)
  match (an.index_option_of_address low, an.index_option_of_address (Sym.sub high (Sym.of_int 4))) with
  | (None, _) | (_, None) ->
      "(no instructions in range " ^ pp_addr low ^ " " ^ pp_addr high ^ ")\n"
  | (Some index_low, Some index_high') ->
  let index_high = index_high' + 1 in
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
  pp_groups_ranged m test an rendered_control_flow_common_prefix_end index_low index_high

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
      ( [
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
      (* Claude: the kinds of alternatives whose original code is in this compilation unit's
         range, if the object has alternatives data *)
      @ ( match an.alternatives with
        | None -> []
        | Some t ->
            let in_range (e : AlternativesType.entry) =
              match an.index_option_of_address (AlternativesType.orig_addr e) with
              | None -> false
              | Some k -> (
                  match
                    (an.index_option_of_address low, an.index_option_of_address (Sym.sub high (Sym.of_int 4)))
                  with
                  | (Some index_low, Some index_high') -> k >= index_low && k <= index_high'
                  | _ -> false )
            in
            [ ( "alternative_kinds",
                "alternative kinds",
                AlternativesPp.pp_alternative_kinds
                  (List.filter in_range (Array.to_list t.AlternativesType.entries)) ) ] )
      (* Claude: the static keys whose test sites are in this compilation unit's range *)
      @ ( match an.jump_table with
        | None -> []
        | Some t ->
            let in_range (e : JumpTable.entry) =
              match an.index_option_of_address (JumpTable.code_addr e) with
              | None -> false
              | Some k -> (
                  match
                    (an.index_option_of_address low, an.index_option_of_address (Sym.sub high (Sym.of_int 4)))
                  with
                  | (Some index_low, Some index_high') -> k >= index_low && k <= index_high'
                  | _ -> false )
            in
            [ ( "static_keys",
                "static keys",
                JumpTablePp.pp_static_keys (List.filter in_range (Array.to_list t.JumpTable.entries)) ) ] )
    )
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

(* Claude: the multi-file output (per-compilation-unit pages and the
   whole-file chunk pages), written to --out-dir if given; separate from the
   single whole-file rendering of pp_test_analysis so that the latter can be
   skipped when nobody wants it *)
(* Claude: --skylight needs the skylighting program; if it is not installed,
   say so once and turn the option off, so that both the source-line links
   and the sources page fall back to plain links to the source files *)
let check_skylight_available () =
  if !Globals.skylight && Sys.command "command -v skylighting > /dev/null 2>&1" <> 0 then begin
    warn "--skylight given but the skylighting program is not installed; linking to the source files instead";
    Globals.skylight := false
  end

let output_multi_file_analysis m test an =
  check_skylight_available ();
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
  )

(* Claude: the single whole-file rendering, as a string *)
let pp_test_analysis m test an =
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
          pp_groups_all m test an
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
      pp_groups_all m test an)
