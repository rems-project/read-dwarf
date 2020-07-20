open Types
open AnalyseUtils
open AnalyseElfTypes
open AnalyseControlFlowTypes
open AnalyseCollectedType
open AnalyseDwarfLineInfo
open AnalyseDwarfFrameInfo
open AnalyseControlFlow
open AnalyseComeFrom
open AnalyseCallGraph
open AnalyseDwarfVarInfo

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

let render_class_name = function
  | Render_symbol_star -> "symbol_star"
  | Render_symbol_nostar -> "symbol_nostar"
  | Render_source -> "source"
  | Render_frame -> "frame"
  | Render_instruction -> "instruction"
  | Render_vars -> "vars"
  | Render_vars_new -> "vars_new"
  | Render_vars_old -> "vars_old"
  | Render_inlining -> "inlining"
  | Render_ctrlflow -> "ctrlflow"

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
               | line ->
                   (* try with a span for each unit *)
                   (*                "<span class=\"" ^ render_class_name rk ^ "\">" ^ html_escape line ^ "</" ^ render_class_name rk ^ ">"
)*)
                   (* try with a pre for each unit *)
                   (*               "<pre color=\"" ^ render_colour rk ^ "\">" ^ html_escape line ^ "</pre>"*)
                   (* try with a classless span for each unit *)
                   (*               "<span color=\"" ^ render_colour rk ^ "\">" ^ html_escape line ^ "</span>"*)
                   (* try with an html font for each unit (NOT HTML5) - best so far - ok on firefox; too slow on chromium*)
                   "<font color=\"" ^ render_colour rk ^ "\">" ^ html_escape line ^ "</font>")
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

let pp_instruction m test an k i =
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
    ^ css m Render_ctrlflow an.rendered_control_flow_inbetweens.(k)
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
         let ctrl = an.pp_inlining_label_prefix "" ^ an.rendered_control_flow_inbetweens.(k) in
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
  (*  ^ begin
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
        ^ css m Render_ctrlflow an.rendered_control_flow_inbetweens.(k)
        ^ ""
        ^ css m Render_source
            (pp_dwarf_source_file_lines' test.dwarf_static !Globals.show_source multiple elifi)
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
          ^ css m Render_ctrlflow an.rendered_control_flow_inbetweens.(k)
          ^ css m Render_frame frame_info
        )
      else ""
    end
  (* the variables whose location ranges include this address - old version*)
  (*  ^ begin
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
  ^ css m Render_ctrlflow an.rendered_control_flow.(k)
  (* the address and (hex) instruction *)
  ^ css m Render_instruction
      (pp_addr addr ^ ":  "
      ^ pp_opcode_bytes test.arch i.i_opcode
      (* the dissassembly from objdump *)
      ^ "  "
      ^ i.i_mnemonic ^ "\t" ^ i.i_operands
      )
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

let pp_test_analysis m test an =
  match m with
  | Ascii ->
      "* ************* instruction count *****************\n"
      ^ string_of_int (Array.length an.instructions)
      ^ " instructions\n" ^ "* ************* globals *****************\n"
      ^ pp_vars an.ranged_vars_at_instructions.rvai_globals
      (*  ^ "************** locals *****************\n"
  ^ pp_ranged_vars
 *)
      ^ "\n* ************* instructions *****************\n"
      ^ ( pp_instruction_init ();
          String.concat "" (Array.to_list (Array.mapi (pp_instruction m test an) an.instructions))
        )
      ^ "* ************* struct/union/enum type definitions *****************\n"
      ^ (let d = test.dwarf_static.ds_dwarf in

         (*     let c = Dwarf.p_context_of_d d in
     Dwarf.pp_all_aggregate_types c d*)
         (*     Dwarf.pp_all_struct_union_enum_types' d)*)
         let ctyps : Dwarf.c_type list = Dwarf.struct_union_enum_types d in
         String.concat "" (List.map Dwarf.pp_struct_union_type_defn' ctyps))
      (*  ^ "\n* ************* branch targets *****************\n"*)
      (*  ^ pp_branch_targets instructions*)
      ^ "\n* ************* call graph *****************\n"
      ^ pp_call_graph test
          ( (*instructions,*)
            an.instructions,
            an.index_of_address,
            an.address_of_index,
            an.indirect_branches )
  | Html ->
      (* "\n* ************* instructions *****************\n" *)
      pp_instruction_init ();
      String.concat "" (Array.to_list (Array.mapi (pp_instruction m test an) an.instructions))
