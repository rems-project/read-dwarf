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

(*****************************************************************************)
(** extract and pretty-print DWARF variable info from linksem simple die tree view, adapted from dwarf.lem  *)

(*****************************************************************************)

open Logs.Logger (struct
  let str = __MODULE__
end)

open Utils
open ControlFlowTypes
open RangedVarType

(* pp *)

let indent_level (indent : bool) (level : int) : string =
  if indent then String.make (level * 3) ' ' else " "

let indent_level_plus_one indent level : string =
  if indent then indent_level indent (level + 1) else " " ^ "   "

let pp_sdt_concise_variable_or_formal_parameter_main (level : int)
    (svfp : Dwarf.sdt_variable_or_formal_parameter) : string =
  let indent = indent_level true level in
  "" ^ indent
  (*  ^ indent ^ "cupdie:" ^  pp_cupdie3 svfp.svfp_cupdie ^ "\n"*)
  (*^ indent ^ "name:" ^*) ^ svfp.svfp_name
  ^ "  "
  (*^ indent ^ "kind:" *) ^ (match svfp.svfp_kind with SVPK_var -> "var" | SVPK_param -> "param")
  ^ "  "
  (*^ indent ^ "type:" *)
  ^ (match svfp.svfp_type with None -> "none" | Some t -> Dwarf.pp_type_info_deep t)
  ^ "  "
  (*^ indent ^ "const_value:"*)
  ^ (match svfp.svfp_const_value with None -> "" | Some v -> "const:" ^ Sym.to_string v)
  ^ "  "

(*^ indent ^ "external:" ^  show svfp.svfp_external ^ "\n"*)
(*^ indent ^ "declaration:" ^  show svfp.svfp_declaration ^ "\n"*)
(*^ indent ^ "locations:" *)

let pp_sdt_concise_variable_or_formal_parameter (level : int) (is_params : bool)
    (svfp : Dwarf.sdt_variable_or_formal_parameter) : string =
  (if is_params then "+" else "")
  ^ pp_sdt_concise_variable_or_formal_parameter_main level svfp
  ^
  match svfp.svfp_locations with
  | None -> "no locations\n"
  | Some [loc] -> " " ^ Dwarf.pp_parsed_single_location_description (Z.of_int 0) loc
  | Some locs ->
      "\n"
      ^ String.concat ""
          (Lem_list.map
             (function
               | loc ->
                   "+"
                   ^ Dwarf.pp_parsed_single_location_description
                       (Z.of_int (level + 1))
                       loc)
             locs)

(*  ^ indent ^ "decl:" ^ (match svfp.svfp_decl with Nothing -> "none\n" | Just ((ufe,line) as ud) -> "\n" ^ indent_level true (level+1) ^ pp_ufe ufe ^ " " ^ show line ^ "\n" end)*)

let pp_sdt_globals_compilation_unit (level : int) (cu : Dwarf.sdt_compilation_unit) : string =
  let indent = indent_level true level in
  ""
  (*  ^ indent ^ "cupdie:" ^  pp_cupdie3 cu.scu_cupdie ^ "\n"*)
  ^ indent
  (*"name:" ^*) ^ cu.scu_name
  ^ "\n"
  (*  ^ indent ^ "vars:" ^  "\n"*)
  ^ String.concat ""
      (Lem_list.map (pp_sdt_concise_variable_or_formal_parameter (level + 1) false) cu.scu_vars)

(*  ^ indent ^ "subroutines :" ^  (match cu.scu_subroutines with | [] -> "none\n" | sus -> "\n" ^ String.concat "\n" (List.map  (pp_sdt_subroutine (level+1)) sus) end) *)

let pp_sdt_globals_dwarf (sdt_d : Dwarf.sdt_dwarf) : string =
  let indent_level = 0 in
  String.concat ""
    (List.map (pp_sdt_globals_compilation_unit indent_level) sdt_d.sd_compilation_units)

(* ******************  local vars *************** *)

let maybe_name x : string = match x with None -> "no name" | Some y -> y

let rec locals_subroutine context (ss : Dwarf.sdt_subroutine) =
  let name = maybe_name ss.ss_name in
  let kind1 =
    match ss.ss_kind with SSK_subprogram -> "" | SSK_inlined_subroutine -> "(inlined)"
  in
  let context1 = (name ^ kind1) :: context in
  List.map (function var -> (var, context1)) ss.ss_vars
  @ begin
      match ss.ss_abstract_origin with
      | None -> []
      | Some ss' ->
          let kind2 = "(abstract origin)" in
          let context2 = (name ^ kind2) :: context in
          List.map (function var -> (var, context2)) ss'.ss_vars
          (* TODO: what about the unspecified parameters? *)
    end
  @ List.flatten (List.map (locals_subroutine context1) ss.ss_subroutines)
  @ List.flatten (List.map (locals_lexical_block context1) ss.ss_lexical_blocks)

(*
    ^ (indent (*^ "name:"                   ^*) ^ (pp_sdt_maybe ss.ss_name (fun name1 -> name1 ^ "\n")
  (*  ^ indent ^ "cupdie:"                 ^ pp_cupdie3 ss.ss_cupdie ^ "\n"*)
  ^ (indent ^ ("kind:"                   ^ (((match ss.ss_kind with SSK_subprogram -> "subprogram" | SSK_inlined_subroutine -> "inlined subroutine" )) ^ ("\n"
  ^ (indent ^ ("call site:"              ^ (pp_sdt_maybe ss.ss_call_site (fun ud -> "\n" ^ (indent_level true (Sym.add level(Sym.of_int 1)) ^ (pp_ud ud ^ "\n")))
  ^ (indent ^ ("abstract origin:"        ^ (pp_sdt_maybe ss.ss_abstract_origin (fun s -> "\n" ^ locals__subroutine (Sym.add level(Sym.of_int 1)) s)
  (*  ^ indent ^ "type:"                   ^ pp_sdt_maybe ss.ss_type (fun typ -> pp_type_info_deep typ ^"\n" end)*)
  ^ (indent ^ ("vars:"                   ^ (pp_sdt_list ss.ss_vars (pp_sdt_concise_variable_or_formal_parameter (Sym.add level(Sym.of_int 1)))
  ^ (indent ^ ("unspecified_parameters:" ^ (pp_sdt_list ss.ss_unspecified_parameters (pp_sdt_unspecified_parameter (Sym.add level(Sym.of_int 1)))
  (*  ^ indent ^ "pc ranges:"              ^ pp_pc_ranges (level+1) ss.ss_pc_ranges*)
  ^ (indent ^ ("subroutines:"            ^ (pp_sdt_list ss.ss_subroutines (locals__subroutine (Sym.add level(Sym.of_int 1)))
  ^ (indent ^ ("lexical_blocks:"         ^ (pp_sdt_list ss.ss_lexical_blocks (locals__lexical_block (Sym.add level(Sym.of_int 1)))
  (*  ^ indent ^ "decl:"                   ^ pp_sdt_maybe ss.ss_decl (fun ((ufe,line) as ud) -> "\n" ^ indent_level true (level+1) ^ pp_ufe ufe ^ " " ^ show line ^ "\n" end)*)
  (*  ^ indent ^ "noreturn:"               ^ show ss.ss_noreturn ^ "\n"*)
  (*  ^ indent ^ "external:"               ^ show ss.ss_external ^"\n"*)
  ^ "\n")))))))))))))))))))))))))
 *)
and locals_lexical_block context (lb : Dwarf.sdt_lexical_block) =
  let context1 = "lexblock" :: context in
  List.map (function var -> (var, context1)) lb.slb_vars
  @ List.flatten (List.map (locals_subroutine context1) lb.slb_subroutines)
  @ List.flatten (List.map (locals_lexical_block context1) lb.slb_lexical_blocks)

(*
  ""
  (*  ^ indent ^ "cupdie:"         ^ pp_cupdie3 lb.slb_cupdie ^ "\n"*)
  ^ (indent ^ ("vars:"           ^ (pp_sdt_list lb.slb_vars (pp_sdt_concise_variable_or_formal_parameter (Sym.add level(Sym.of_int 1)))
  (*  ^ indent ^ "pc ranges:"      ^ pp_pc_ranges (level+1) lb.slb_pc_ranges*)
  ^ (indent ^ ("subroutines :"   ^ (pp_sdt_list lb.slb_subroutines (locals__subroutine (Sym.add level(Sym.of_int 1)))
  ^ (indent ^ ("lexical_blocks:" ^ (pp_sdt_list lb.slb_lexical_blocks (locals__lexical_block (Sym.add level(Sym.of_int 1)))
  ^ "\n"))))))))))
 *)

let locals_compilation_unit context (cu : Dwarf.sdt_compilation_unit) =
  let name = cu.scu_name in
  let context1 = name :: context in
  (*List.map (function var -> (var, context1)) cu.scu_vars
    @*)
  List.flatten (List.map (locals_subroutine context1) cu.scu_subroutines)

(*
  ""
  ^ (indent (*^ "name:"         *) ^ (cu.scu_name ^ ("\n"
  (*  ^ indent ^ "cupdie:"       ^ pp_cupdie3 cu.scu_cupdie ^ "\n"*)
  ^ (indent ^ ("vars:"         ^ (pp_sdt_list cu.scu_vars (pp_sdt_concise_variable_or_formal_parameter (Sym.add level(Sym.of_int 1)))
  ^ (indent ^ ("subroutines :" ^ pp_sdt_list cu.scu_subroutines (locals__subroutine (Sym.add level(Sym.of_int 1))))))))))))
 *)
let locals_dwarf (sdt_d : Dwarf.sdt_dwarf) :
    (Dwarf.sdt_variable_or_formal_parameter * string list) (*context*) list =
  let context = [] in
  (*List.map (function var -> (var, context1)) cu.scu_vars
    @*)
  List.flatten (List.map (locals_compilation_unit context) sdt_d.sd_compilation_units)

let globals_compilation_unit context (cu : Dwarf.sdt_compilation_unit) =
  let name = cu.scu_name in
  let context1 = name :: context in
  List.map (function var -> (var, context1)) cu.scu_vars

(*@
  List.flatten (List.map (locals_subroutine context1) cu.scu_subroutines)*)

let globals_dwarf (sdt_d : Dwarf.sdt_dwarf) :
    (Dwarf.sdt_variable_or_formal_parameter * string list) (*context*) list =
  let context = [] in
  List.flatten (List.map (globals_compilation_unit context) sdt_d.sd_compilation_units)

let params_subroutine (ss : Dwarf.sdt_subroutine) =
  let name = maybe_name ss.ss_name in
  match ss.ss_entry_address with
  | None -> None
  | Some addr ->
      let vars =
        List.filter
          (function
            | (svfp : Dwarf.sdt_variable_or_formal_parameter) -> svfp.svfp_kind = SVPK_param)
          ss.ss_vars
      in
      Some (addr, (name, vars))

let params_compilation_unit (cu : Dwarf.sdt_compilation_unit) =
  List.filter_map params_subroutine cu.scu_subroutines

let params_dwarf (sdt_d : Dwarf.sdt_dwarf) =
  List.flatten (List.map params_compilation_unit sdt_d.sd_compilation_units)

let pp_context context = String.concat ":" context

let pp_vars (vars : (Dwarf.sdt_variable_or_formal_parameter * string list) list) : string =
  String.concat ""
    (List.map
       (function
         | (var, context) ->
             pp_context context ^ "\n" ^ pp_sdt_concise_variable_or_formal_parameter 1 false var)
       vars)

let pp_ranged_var (prefix : string) (var : ranged_var) : string =
  let ((n1, n2, ops), (svfp, context)) = var in
  prefix
  ^ pp_sdt_concise_variable_or_formal_parameter_main 0 svfp
  ^ (let s = Dwarf.pp_parsed_single_location_description (Nat_big_num.of_int 0) (n1, n2, ops) in
     String.sub s 0 (String.length s - 1))
  (*hackish stripping of trailing \n from linksem - TODO: fix linksem interface*)
  ^ " "
  ^ pp_context context
  ^ ( match svfp.svfp_decl with
    | None -> ""
    | Some (_ufe, line, _subprogram_name) -> ":" ^ string_of_int line
    )
  ^ "\n"

let pp_ranged_vars (prefix : string) (vars : ranged_var list) : string =
  String.concat "" (List.map (pp_ranged_var prefix) vars)

let compare_pc_ranges ((n1, _, _), _) ((n1', _, _), _) = Sym.Ordered.compare n1 n1'

let local_by_pc_ranges (((svfp : Dwarf.sdt_variable_or_formal_parameter), _context) as var) :
    ranged_var list =
  List.map
    (function (_n1, _n2, _ops) as pc_range -> (pc_range, var))
    (match svfp.svfp_locations with Some locs -> locs | None -> [])

let locals_by_pc_ranges
    (vars : (Dwarf.sdt_variable_or_formal_parameter * string list) (*context*) list) :
    ranged_var list =
  List.stable_sort compare_pc_ranges (List.flatten (List.map local_by_pc_ranges vars))

(* TODO: sometimes an absence of location list means it doesn't exist at runtime, and sometimes it uses the enclosing PC range in some way? *)

let partition_first g xs =
  let rec partition_first' g xs acc =
    match xs with
    | [] -> (List.rev acc, [])
    | x :: xs' -> if g x then partition_first' g xs' (x :: acc) else (List.rev acc, x :: xs')
  in
  partition_first' g xs []

let mk_ranged_vars_at_instructions (sdt_d : Dwarf.sdt_dwarf) instructions :
    ranged_vars_at_instructions =
  let locals = locals_dwarf sdt_d in
  let locals_by_pc_ranges : ranged_var list = locals_by_pc_ranges locals in

  let size = Array.length instructions in
  let rvai_current = Array.make size [] in
  let rvai_new = Array.make size [] in
  let rvai_old = Array.make size [] in
  let rvai_remaining = Array.make size [] in

  let rec f (addr_prev : addr) (prev : ranged_var list) (remaining : ranged_var list) (k : index)
      =
    if k >= size then ()
    else
      let addr = instructions.(k).i_addr in
      if not (Sym.Ordered.less addr_prev addr) then
        fatal "mk_ranged_vars_at_instructions found non-increasing address %s" (pp_addr addr);
      let (still_current, old) =
        List.partition (function ((_, n2, _), _) -> Sym.Ordered.less addr n2) prev
      in
      let (new', remaining') =
        partition_first
          (function ((n1, _n2, _ops), _var) as _rv -> Sym.Ordered.greater_equal addr n1)
          remaining
      in
      (* TODO: do we need to drop any that have been totally skipped over? *)
      let current = still_current @ new' in
      rvai_current.(k) <- current;
      rvai_new.(k) <- new';
      rvai_old.(k) <- old;
      rvai_remaining.(k) <- remaining';
      f addr current remaining' (k + 1)
  in
  f (Sym.of_int (0 - 1)) [] locals_by_pc_ranges 0;

  {
    rvai_globals = globals_dwarf sdt_d;
    rvai_params = params_dwarf sdt_d;
    rvai_current;
    rvai_new;
    rvai_old;
    rvai_remaining;
  }

(*
let local_locals (vars: ranged_var list) instructions  : ranged_vars_at_locations
   *)
