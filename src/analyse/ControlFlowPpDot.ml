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
(**       pp control-flow graph to dot                                       *)

(*****************************************************************************)

open Logs.Logger (struct
  let str = __MODULE__
end)

open Utils
open SdtUtils
open ElfTypes
open ControlFlowTypes
open ControlFlow
open CollectedType
open DwarfLineInfo
open ComeFrom

(* pp to dot a CFG.  Make a node for each non-{C_plain, C_branch}
   instruction, and an extra "start" node for each ELF symbol or other
   bl target.

   In choosing to only make nodes for conditional/indirect branches
   and calls/returns, we are not making nodes for every control-flow
   target, so this is a quotient of the underlying basic-block
   control-flow graph.  For example, it won't visualise multiple
   branches into distinct points of a straight-line control-flow
   sequence.  However, it will have most of the essential choices, and
   be simpler to look at.

*)

(* nesting info *)
type nesting = {
  n_indices : index list;
  (* the list of O0 call sites we are in, used to make node names, when recursing over O0; empty when generating an O2 graph *)
  n_current : Dwarf.sdt_subroutine option;
  (* the O2 subroutine that we are morally "within", when recursing over O0; None otherwise, eg when generating an O2 graph *)
  n_stack :
    (string (*sdt_subroutine_name*) * int) (*call-site line*)
                                           (* * int (*call-site column*)*) list;
      (* the data which should be comparable between inlined-O0 and O2 *)
}

type node_kind_cfg =
  | CFG_node_start (* elf symbol or other bl target *)
  | CFG_node_branch_cond
  | CFG_node_branch_register
  | CFG_node_branch_and_link
  (*  | CFG_node_bl_noreturn*)
  | CFG_node_smc_hvc
  | CFG_node_ret
  | CFG_node_eret

type edge_kind_cfg =
  | CFG_edge_flow
  | CFG_edge_branch_and_link_inline
  | CFG_edge_ret_inline
  | CFG_edge_correlate

type node_cfg = {
  nc_name : string;
  nc_kind : node_kind_cfg;
  nc_label : string;
  nc_addr : addr;
  nc_index : index;
  nc_source_lines : (string (*subprogram name*) * int) (*line number*) list;
  nc_nesting : nesting;
  nc_stack :
    (string (*sdt_subroutine_name*) * int) (*call-site line*)
                                           (* * int (*call-site column*) *)
    list;
  nc_colour : string;
  nc_ppd_instruction : string list;
  nc_visited : bool;
}

type edge_cfg = string * string * edge_kind_cfg

(* graphs and graph fragments (in which the edges may mention other nodes) *)
type graph_cfg = {
  gc_start_nodes : node_cfg list;
  (* "start" nodes - synthetic nodes, for elf symbols or other bl targets*)
  gc_nodes : node_cfg list;
  (* non-start interior nodes  - corresponding to particular interesting control-flow instructions *)
  gc_edges : edge_cfg list;
  (* edges *)
  gc_edges_exiting : edge_cfg list;
  (* edges leaving a subgraph*)
  gc_subgraphs : (string (*subgraph name*) * string (*subgraph colour*) * graph_cfg) list;
}

(* the gc_edges_exiting have to be kept separate because if they are within the subgraph in the generated .dot, graphviz pulls the target node *within* the subgraph, even if it isn't *)

let nesting_init (sso : Dwarf.sdt_subroutine option) =
  { n_indices = []; n_current = sso; n_stack = [] }

let cupdie_id
    (((cu : Dwarf.compilation_unit), (parents : Dwarf.die list), (die : Dwarf.die)) :
      Dwarf.cupdie) =
  ( cu.cu_header.cuh_offset,
    List.map (function (die : Dwarf.die) -> die.die_offset) parents,
    die.die_offset )

let inlining_stack_at_index (an : analysis) k =
  let (_, _, xs) = an.inlining.(k) in
  let ys : Dwarf.inlined_subroutine_data_by_range_entry list = List.map snd xs in
  let iss : Dwarf.inlined_subroutine list =
    List.map (function ((_n1, _n2), (_m, _n), is) -> is) ys
  in
  (* the set of current inlining stacks, parent-first in each *)
  let ssss : Dwarf.sdt_subroutine list list =
    List.map
      (function
        | (is : Dwarf.inlined_subroutine) ->
            List.rev (is.is_inlined_subroutine_sdt :: is.is_inlined_subroutine_sdt_parents))
      iss
  in
  (* is sss2 an additional inlining from sss1? *)
  let rec prefix (sss1 : Dwarf.sdt_subroutine list) (sss2 : Dwarf.sdt_subroutine list) =
    match (sss1, sss2) with
    | ([], _) -> true
    | (ss1 :: sss1', ss2 :: sss2') ->
        if cupdie_id ss1.ss_cupdie = cupdie_id ss2.ss_cupdie then prefix sss1' sss2' else false
    | _ -> false
  in
  (* find all the maximal inlinings - those that are not prefixes of any others *)
  let rec maximal' acc ssss =
    match ssss with
    | [] -> acc
    | sss :: ssss' ->
        if List.exists (function sss' -> prefix sss sss') (acc @ ssss') then maximal' acc ssss'
        else maximal' (sss :: acc) ssss'
  in
  let maximal = maximal' [] ssss in
  match maximal with
  | [] -> []
  | [sss] -> (
      let nls : (string * int) list =
        List.map
          (function
            | (ss : Dwarf.sdt_subroutine) -> (
                ( (match ss.ss_name with None -> "no-name" | Some name -> name),
                  match ss.ss_call_site with
                  | Some (_ufe, line, _subprogram_name) -> line
                  | None -> -3
                  (*fatal "no call site"*) )
              ))
          (List.rev sss)
      in
      let rec shift line nls =
        match nls with [] -> [] | (name', line') :: nls' -> (name', line) :: shift line' nls'
      in
      match nls with [] -> [] | (_name, line) :: nls' -> shift line nls'
    )
  | _ ->
      err "inlining_stack_at_index found non-prefix-related inlinings:\n%s"
        (String.concat "\n===---\n"
           (List.map
              (function
                | sss' ->
                    String.concat "\n---\n"
                      (List.map (Dwarf.pp_sdt_subroutine (Nat_big_num.of_int 0)) sss'))
              maximal));
      []

(* the above is not working right for the case of handler.c:667
struct vcpu *fiq_lower(void)
{
	return irq_lower();
}
which ends up with maximal containing:

           irq_lower (subprogram)         api_preempt  vcpu_index
fiq_lower  irq_lower (inlined subroutine) api_preempt  vcpu_index

where the first irq_lower's has no call site
 *)

let graph_cfg_empty () =
  { gc_start_nodes = []; gc_nodes = []; gc_edges = []; gc_edges_exiting = []; gc_subgraphs = [] }

let graph_cfg_union g1 g2 =
  {
    gc_start_nodes = g1.gc_start_nodes @ g2.gc_start_nodes;
    gc_nodes = g1.gc_nodes @ g2.gc_nodes;
    gc_edges = g1.gc_edges @ g2.gc_edges;
    gc_edges_exiting = g1.gc_edges_exiting @ g2.gc_edges_exiting;
    gc_subgraphs = g1.gc_subgraphs @ g2.gc_subgraphs;
  }

let count_branch_nodes g =
  List.length
    (List.filter
       (function
         | n -> (
             match n.nc_kind with
             | CFG_node_branch_cond | CFG_node_branch_register -> true
             | _ -> false
           ))
       g.gc_nodes)

(* the graphviz svg colours from https://www.graphviz.org/doc/info/colors.html without those too close to white or those that dot complains about*)
let colour_pairs_svg =
  [
    (*"aliceblue","black";*)
    (*"antiquewhite","black";*)
    ("aqua", "black");
    ("aquamarine", "black");
    (*"azure","black";*)
    (*"beige","black";*)
    (*"bisque","black";*)
    ("black", "white");
    (*"blanchedalmond","black";*)
    ("blue", "white");
    ("blueviolet", "black");
    ("brown", "white");
    ("burlywood", "black");
    ("cadetblue", "black");
    ("chartreuse", "black");
    ("chocolate", "white");
    ("coral", "black");
    ("cornflowerblue", "black");
    (*"cornsilk","black";*)
    ("crimson", "black");
    ("cyan", "black");
    ("darkblue", "white");
    ("darkcyan", "white");
    ("darkgoldenrod", "black");
    (*"darkgray","black";*)
    ("darkgreen", "white");
    ("darkgrey", "white");
    ("darkkhaki", "white");
    ("darkmagenta", "white");
    ("darkolivegreen", "white");
    ("darkorange", "black");
    ("darkorchid", "black");
    ("darkred", "white");
    ("darksalmon", "black");
    ("darkseagreen", "white");
    ("darkslateblue", "white");
    ("darkslategray", "white");
    ("darkslategrey", "white");
    ("darkturquoise", "white");
    ("darkviolet", "black");
    ("deeppink", "black");
    ("deepskyblue", "black");
    ("dimgray", "white");
    (*    "dimgrey","black";*)
    ("dodgerblue", "black");
    ("firebrick", "black");
    (*"floralwhite","black";*)
    ("forestgreen", "white");
    ("fuchsia", "black");
    (*"gainsboro","black";*)
    (*"ghostwhite","black";*)
    ("gold", "black");
    ("goldenrod", "black");
    ("gray", "black");
    (*  "grey","black";*)
    ("green", "black");
    ("greenyellow", "black");
    (*"honeydew","black";*)
    ("hotpink", "black");
    ("indianred", "black");
    ("indigo", "white");
    (*"ivory","black";*)
    ("khaki", "black");
    (*"lavender","black";*)
    (*"lavenderblush","black";*)
    ("lawngreen", "black");
    (*"lemonchiffon","black";*)
    ("lightblue", "black");
    ("lightcoral", "black");
    (*"lightcyan","black";*)
    (*"lightgoldenrodyellow","black";*)
    (*"lightgray","black";*)
    ("lightgreen", "black");
    (*    "lightgrey","black";*)
    ("lightpink", "black");
    ("lightsalmon", "black");
    ("lightseagreen", "black");
    ("lightskyblue", "black");
    ("lightslategray", "black");
    ("lightslategrey", "black");
    ("lightsteelblue", "black");
    (*"lightyellow","black";*)
    ("lime", "black");
    ("limegreen", "black");
    (*"linen","black";*)
    ("magenta", "black");
    ("maroon", "black");
    ("mediumaquamarine", "black");
    ("mediumblue", "white");
    ("mediumorchid", "black");
    ("mediumpurple", "black");
    ("mediumseagreen", "black");
    ("mediumslateblue", "black");
    ("mediumspringgreen", "black");
    ("mediumturquoise", "black");
    ("mediumvioletred", "black");
    ("midnightblue", "white");
    (*"mintcream","black";*)
    (*"mistyrose","black";*)
    ("moccasin", "black");
    ("navajowhite", "black");
    ("navy", "white");
    (*"oldlace","black";*)
    ("olive", "black");
    ("olivedrab", "black");
    ("orange", "black");
    ("orangered", "black");
    ("orchid", "black");
    ("palegoldenrod", "black");
    ("palegreen", "black");
    (*"paleturquoise","black";*)
    ("palevioletred", "black");
    (*"papayawhip","black";*)
    (*"peachpuff","black";*)
    ("peru", "white");
    ("pink", "black");
    ("plum", "black");
    ("powderblue", "black");
    ("purple", "black");
    ("red", "black");
    ("rosybrown", "black");
    ("royalblue", "black");
    ("saddlebrown", "white");
    ("salmon", "black");
    ("sandybrown", "black");
    ("seagreen", "black");
    (*"seashell","black";*)
    ("sienna", "black");
    ("silver", "black");
    ("skyblue", "black");
    ("slateblue", "black");
    ("slategray", "black");
    ("slategrey", "black");
    (*"snow","black";*)
    ("springgreen", "black");
    ("steelblue", "black");
    ("tan", "black");
    ("teal", "black");
    ("thistle", "black");
    ("tomato", "black");
    ("turquoise", "black");
    ("violet", "black");
    ("wheat", "black");
    (*"white","black";*)
    (*"whitesmoke","black";*)
    ("yellow", "black");
    ("yellowgreen", "black");
  ]

let colours_svg = List.map fst colour_pairs_svg

let colours_dot_complains =
  [
    "teal";
    "darkgrey";
    "silver";
    "darkcyan";
    "olive";
    "darkmagenta";
    "aqua";
    "darkred";
    "lime";
    "lightgreen";
    "darkblue";
    "fuchsia";
  ]

let colours =
  List.filter (function c -> not (List.mem String.equal c colours_dot_complains)) colours_svg

let include_tooltips = true

let mk_ppd_instruction test an label k _nesting =
  let m = Types.Html in
  if include_tooltips then
    (* TODO: reduce the nasty code duplication between this and pp_instruction *)
    let i = an.instructions.(k) in
    let addr = i.i_addr in
    let come_froms' =
      List.filter (function cf -> cf.cf_target_kind <> T_plain_successor) an.come_froms.(k)
    in
    let lines =
      [label]
      @ List.map
          (pp_dwarf_source_file_lines' m test.dwarf_static !Globals.show_source false)
          an.line_info.(k)
      (* the address and (hex) instruction *)
      @ [
          pp_addr addr ^ ":  "
          ^ pp_opcode_bytes test.arch i.i_opcode
          (* the dissassembly from objdump *)
          ^ "  "
          ^ i.i_mnemonic ^ "\t" ^ i.i_operands
          (* any indirect-branch control flow from this instruction *)
          ^ begin
              match i.i_control_flow with
              | C_branch_register _ ->
                  " -> "
                  ^ String.concat ","
                      (List.map
                         (function
                           | (_, a', _, s) ->
                               pp_target_addr_wrt addr i.i_control_flow a' ^ "" ^ s ^ "")
                         i.i_targets)
                  ^ " "
              | _ -> ""
            end
          (* any control flow to this instruction *)
          ^ pp_come_froms addr come_froms';
        ]
    in
    lines
  else []

let pp_node_inlining n =
  let pp_source_line (subprogram_name, line) = subprogram_name ^ ":" ^ string_of_int line in
  let pp_source_lines sls =
    match sls with
    | [(subprogram_name, line)] -> pp_source_line (subprogram_name, line)
    | [] -> "no-source-lines"
    | _ -> "[" ^ String.concat ", " (List.map pp_source_line sls) ^ "]"
  in
  let pp_stack stack =
    String.concat " "
      (List.map (function (name, line (*,col*)) -> pp_source_line (name, line)) stack)
  in
  (*        [(match nesting.n_current with None -> "no-current" | Some ss -> match ss.ss_name with None ->"no-name" | Some name -> name ) ^ " " ^ pp_stack nesting.n_stack]*)
  ["nesting: " ^ pp_source_lines n.nc_source_lines ^ " " ^ pp_stack n.nc_nesting.n_stack]
  @ ["nc_stack: " ^ pp_source_lines n.nc_source_lines ^ " " ^ pp_stack n.nc_stack]

(* make a control-flow graph, starting from start_indices (which should be ELF symbol indices).  If recurse_flat, then recurse (not inlining) on all bl targets  (this option is no longer used).  If inline_all, then recurse (inlining) through all bl targets. Add node_name_prefix to all node names, so that graphs can be unioned. *)

let mk_cfg test an visitedo node_name_prefix (recurse_flat : bool) (_inline_all : bool)
    (start_indices : (index * nesting) list) : graph_cfg =
  let colour k =
    let subprogram_names =
      List.sort_uniq compare
        (List.map
           (function elifi -> mk_subprogram_name test.dwarf_static elifi)
           an.line_info.(k))
    in
    match subprogram_names with
    | [subprogram_name] ->
        let colour =
          List.nth colours (Hashtbl.hash subprogram_name land 65535 * List.length colours / 65536)
        in
        colour
    | _ -> "black"
  in

  let is_visited k = match visitedo with None -> false | Some visited -> visited.(k) in

  (* the graphette start nodes are the instructions which are either
       - elf symbols, or
       - the branch target (but not the successor) of a C_branch_and_link
          (probably these will all also have elf symbols) *)
  let is_graphette_start_target (cf : come_from) =
    match cf.cf_target_kind with
    | T_plain_successor -> false
    | T_branch -> false
    | T_branch_and_link_call -> true
    | T_branch_and_link_call_noreturn -> true
    | T_branch_and_link_successor -> false
    | T_branch_cond_branch -> false
    | T_branch_cond_successor -> false
    | T_branch_register -> false
    | T_smc_hvc_successor -> false
    | T_out_of_range _ -> false
  in

  let is_graphette_start k =
    an.elf_symbols.(k) <> [] || List.exists is_graphette_start_target an.come_froms.(k)
  in

  (* the (non-start) nodes are all the intereresting (non-unconditional-branch) control-flow instructions *)
  let is_graph_non_start_node k =
    let i = an.instructions.(k) in
    match i.i_control_flow with
    | C_no_instruction -> false
    | C_plain -> false
    | C_ret -> true
    | C_eret -> true
    | C_branch _ -> false
    | C_branch_and_link _ -> true
    | C_smc_hvc _ -> true
    | C_branch_cond _ -> true
    | C_branch_register _ -> true
  in

  let pp_node_name_nesting nesting =
    String.concat "_"
      (List.map (function k -> pp_addr an.instructions.(k).i_addr) nesting.n_indices)
  in

  (* we make up an additional node for all ELF symbols and bl targets; all others are just the address *)
  let node_name_start nesting addr =
    node_name_prefix ^ "start_" ^ pp_addr addr ^ "_" ^ pp_node_name_nesting nesting
  in
  let node_name nesting addr =
    node_name_prefix ^ pp_addr addr ^ "_" ^ pp_node_name_nesting nesting
  in

  (* need to track branch-visited edges because Hf loops back to a nop (abort.c) and a wfi (wait for interrupt)*)
  let rec next_non_start_node_name nesting visited k =
    let i = an.instructions.(k) in
    match i.i_control_flow with
    | C_plain -> (
        match i.i_targets with
        | [(_, _, k', _)] -> next_non_start_node_name nesting visited k'
        | _ -> fatal "non-unique plain targets at %s" (pp_addr i.i_addr)
      )
    | C_branch _ -> (
        match i.i_targets with
        | [(_, addr', k', _)] ->
            if List.mem Int.equal k' visited then (node_name nesting addr', k')
              (* TODO: something more useful *)
            else next_non_start_node_name nesting (k' :: visited) k'
        | _ -> fatal "non-unique branch targets at %s" (pp_addr i.i_addr)
      )
    | _ -> (node_name nesting i.i_addr, k)
  in

  let mk_node nesting k kind label =
    let i = an.instructions.(k) in
    {
      nc_name = node_name nesting i.i_addr;
      nc_kind = kind;
      nc_label = label;
      nc_addr = i.i_addr;
      nc_index = k;
      nc_source_lines = dwarf_source_file_line_numbers_by_index test an.line_info k;
      nc_nesting = nesting;
      nc_stack = inlining_stack_at_index an k;
      nc_colour = colour k;
      nc_ppd_instruction = mk_ppd_instruction test an label k nesting;
      nc_visited = is_visited k;
    }
  in

  let mk_node_simple nesting k kind label_prefix =
    let i = an.instructions.(k) in
    let label = label_prefix ^ pp_addr i.i_addr in
    mk_node nesting k kind label
  in

  (* make the little piece of graph from a start node at k to its first non-start node *)
  let graphette_start _return_target (k, nesting) : graph_cfg * (index * nesting) list
      (* work_list_new *) =
    let i = an.instructions.(k) in
    let ss = an.elf_symbols.(k) in
    let (s, nn) =
      match ss with
      | [] -> (pp_addr i.i_addr, node_name nesting i.i_addr)
      | _ -> (List.hd (List.rev ss), node_name_start nesting i.i_addr)
    in
    let node =
      {
        nc_name = nn;
        nc_kind = CFG_node_start;
        nc_label = node_name_prefix ^ s;
        nc_addr = i.i_addr;
        nc_index = k;
        nc_source_lines = dwarf_source_file_line_numbers_by_index test an.line_info k;
        nc_nesting = nesting;
        nc_stack = inlining_stack_at_index an k;
        nc_colour = colour k;
        nc_ppd_instruction = mk_ppd_instruction test an s k nesting;
        nc_visited = is_visited k;
      }
    in
    let (nn', k') = next_non_start_node_name nesting [] k in
    let edge = (node.nc_name, nn', CFG_edge_flow) in
    ( {
        gc_start_nodes = [node];
        gc_nodes = [];
        gc_edges = [edge];
        gc_edges_exiting = [];
        gc_subgraphs = [];
      },
      [(k', nesting)] )
  in

  let discard_out_of_range_targets targets =
    List.filter (function (T_out_of_range _, _, _, _) -> false | _ -> true) targets
  in

  (* make the piece of graph from a non-start node onwards, following fall-through control flow and branch-and-link successors, up to the first interesting control flow - including the outgoing edges, but not their target nodes. Recurse (via mk_graph) at bl instructions that are inlined  *)
  let rec graphette_normal return_target (k, nesting) :
      graph_cfg * (index * nesting) list * (* work_list_new *)
      (index * nesting) list (* bl targets *) =
    (*Printf.printf "gb k=%d\n a=%s" k (pp_addr (address_of_index k));flush stdout;*)
    let i = an.instructions.(k) in
    match i.i_control_flow with
    | C_no_instruction ->
        fatal "graphette_normal on C_no_instruction"
        (*graphette_body acc_nodes acc_edges visited nn_last (k + 1)*)
    | C_plain ->
        fatal "graphette_normal on C_plain"
        (*graphette_body acc_nodes acc_edges visited nn_last (k + 1)*)
    | C_branch _ ->
        fatal "graphette_normal on C_branch"
        (*graphette_body acc_nodes acc_edges visited nn_last (k + 1)*)
    | C_ret ->
        let node = mk_node nesting k CFG_node_ret "ret" in
        let edges_exiting =
          match return_target with
          | Some nn' -> [(node.nc_name, nn', CFG_edge_ret_inline)]
          | None -> []
        in
        ( {
            gc_start_nodes = [];
            gc_nodes = [node];
            gc_edges = [];
            gc_edges_exiting = edges_exiting;
            gc_subgraphs = [];
          },
          [],
          [] )
    | C_eret ->
        let node = mk_node nesting k CFG_node_eret "eret" in
        ( {
            gc_start_nodes = [];
            gc_nodes = [node];
            gc_edges = [];
            gc_edges_exiting = [];
            gc_subgraphs = [];
          },
          [],
          [] )
    | C_smc_hvc _ ->
        let node = mk_node_simple nesting k CFG_node_smc_hvc "smc/hvc " in
        let (edges, work_list_new) =
          List.split
            (List.filter_map
               (function
                 | (T_smc_hvc_successor, _, k', _) ->
                     let (nn', k'') = next_non_start_node_name nesting [] k' in
                     Some ((node.nc_name, nn', CFG_edge_flow), (k'', nesting))
                 | _ -> None)
               i.i_targets)
        in
        ( {
            gc_start_nodes = [];
            gc_nodes = [node];
            gc_edges = edges;
            gc_edges_exiting = [];
            gc_subgraphs = [];
          },
          work_list_new,
          [] )
    | C_branch_cond (_mnemonic, _, _) ->
        let node = mk_node_simple nesting k CFG_node_branch_cond "" in
        let (edges, work_list_new) =
          List.split
            (List.map
               (function
                 | (_, _, k', _) ->
                     let (nn', k'') = next_non_start_node_name nesting [] k' in
                     ((node.nc_name, nn', CFG_edge_flow), (k'', nesting)))
               (discard_out_of_range_targets i.i_targets))
        in
        ( {
            gc_start_nodes = [];
            gc_nodes = [node];
            gc_edges = edges;
            gc_edges_exiting = [];
            gc_subgraphs = [];
          },
          work_list_new,
          [] )
    | C_branch_register _ ->
        let node = mk_node_simple nesting k CFG_node_branch_register "" in
        let (edges, work_list_new) =
          List.split
            (List.sort_uniq compare
               (List.map
                  (function
                    | (_, _, k', _) ->
                        let (nn', k'') = next_non_start_node_name nesting [] k' in
                        ((node.nc_name, nn', CFG_edge_flow), (k'', nesting)))
                  (discard_out_of_range_targets i.i_targets)))
        in
        ( {
            gc_start_nodes = [];
            gc_nodes = [node];
            gc_edges = edges;
            gc_edges_exiting = [];
            gc_subgraphs = [];
          },
          work_list_new,
          [] )
    | C_branch_and_link (_, s) ->
        let k_call =
          match
            List.filter_map
              (function
                | (T_branch_and_link_call, _, k', _) -> Some k'
                | (T_branch_and_link_call_noreturn, _, k', _) -> Some k'
                | _ -> None)
              (discard_out_of_range_targets i.i_targets)
          with
          | [k_call] -> k_call
          | _ -> fatal "non-unique k_call"
        in
        let nn_k_successor =
          match
            List.filter_map
              (function (T_branch_and_link_successor, _, k', _) -> Some k' | _ -> None)
              (discard_out_of_range_targets i.i_targets)
          with
          | [k'] ->
              let (nn', k'') = next_non_start_node_name nesting [] k' in
              Some (nn', k'')
          | _ ->
              (* noreturn *)
              None
        in

        let new_ss_O2_ambient_option =
          match nesting.n_current with
          | None -> None
          | Some ss_O2_ambient -> (
              (* find the bl target subprogram name in the O0 *)
              let ss_target_name =
                let names =
                  List.sort_uniq compare
                    (List.filter_map
                       (function (ss : Dwarf.sdt_subroutine) -> ss.ss_name)
                       (find_sdt_subroutine_by_entry_address an.sdt (an.address_of_index k_call)))
                in
                match names with
                | [name] -> name
                | _ -> fatal "multiple ss_target names: %s\n" (String.concat ", " names)
              in

              (* find all the source lines associated with this instruction in the O0 *)
              let source_lines =
                List.sort_uniq compare
                  (List.map
                     (function
                       | elifi ->
                           let lnh = elifi.elifi_entry.elie_lnh in
                           let lnr = elifi.elifi_entry.elie_lnr in
                           let ((_comp_dir, _dir, _file) as ufe) =
                             Dwarf.unpack_file_entry lnh lnr.lnr_file
                           in
                           (ufe, Sym.to_int lnr.lnr_line))
                     an.line_info.(k))
              in

              (* find all the O2 inlined subroutines (possibly inside lexical blocks) of ss_O2_ambient that have the same name as the O0 target, and the same call site (unpacked_file_entry * line) as one of the O0 source lines *)
              let is_corresponding_O2_subroutine (ss' : Dwarf.sdt_subroutine) =
                ss'.ss_name = Some ss_target_name
                &&
                match ss'.ss_call_site with
                | None -> false
                | Some (ufe, line, _subprogram_name) ->
                    List.mem Stdlib.( = ) (ufe, line) source_lines
              in

              match
                find_sdt_subroutine_subroutine true is_corresponding_O2_subroutine ss_O2_ambient
              with
              | [ss] -> Some ss
              | [] -> None
              | _ -> fatal "multiple ss_O2_ambient'"
            )
        in

        let inline = (*inline_all*) new_ss_O2_ambient_option <> None in

        let node = mk_node nesting k CFG_node_branch_and_link s in
        if (not inline) || List.mem Int.equal k nesting.n_indices then
          (* not inline: construct a new node for the bl. If not noreturn, add an edge to its successor and return that in the new worklist, otherwise stop at this node.  In either case, if recurse_flat, add the bl target to the bl_target_indices *)
          let nesting_call = nesting in

          match nn_k_successor with
          | Some (nn', k'') ->
              let edges = [(node.nc_name, nn', CFG_edge_flow)] in
              ( {
                  gc_start_nodes = [];
                  gc_nodes = [node];
                  gc_edges = edges;
                  gc_edges_exiting = [];
                  gc_subgraphs = [];
                },
                [(k'', nesting)],
                if recurse_flat then [(k_call, nesting_call)] else [] )
          | None ->
              ( {
                  gc_start_nodes = [];
                  gc_nodes = [node];
                  gc_edges = [];
                  gc_edges_exiting = [];
                  gc_subgraphs = [];
                },
                [],
                if recurse_flat then [(k_call, nesting_call)] else [] )
        else
          (* inline: construct a new node for the bl and a new subgraph for the inlined subroutine, and an edge between them (faking up the node name that mk_graph will use for its start node).  Pass the return_target' in to the subgraph construction, for it to add edges from the ret to the successor (if any) *)
          let nesting_call =
            {
              n_indices = k :: nesting.n_indices;
              n_current = new_ss_O2_ambient_option;
              n_stack =
                ( match nesting.n_current with
                | None -> fatal "no ss_call"
                | Some ss_current ->
                    ( (match ss_current.ss_name with None -> fatal "no name" | Some name -> name),
                      match new_ss_O2_ambient_option with
                      | None ->
                          fatal "no call site for\n%s"
                            (Dwarf.pp_sdt_subroutine (Nat_big_num.of_int 0) ss_current)
                      | Some new_ss_O2_ambient -> (
                          match new_ss_O2_ambient.ss_call_site with
                          | None ->
                              fatal "no call site2 for\n%s"
                                (Dwarf.pp_sdt_subroutine (Nat_big_num.of_int 0) ss_current)
                          | Some (_ufe, line, _subprogram_name) -> line
                        )
                      (*,
                        0*) )
                    :: nesting.n_stack
                );
            }
          in
          let (return_target', work_list_new) =
            match nn_k_successor with
            | Some (nn', k'') -> (Some nn', [(k'', nesting)])
            | None -> (None, [])
          in
          let subgraph_name = "cluster_" ^ pp_node_name_nesting nesting_call in
          let subgraph_colour = colour k_call in
          let subgraph = mk_graph return_target' [(k_call, nesting_call)] in
          let nn'' = node_name_start nesting_call an.instructions.(k_call).i_addr in
          let edges = [(node.nc_name, nn'', CFG_edge_branch_and_link_inline)] in
          ( {
              gc_start_nodes = [];
              gc_nodes = [node];
              gc_edges = edges;
              gc_edges_exiting = [];
              gc_subgraphs = [(subgraph_name, subgraph_colour, subgraph)];
            },
            work_list_new,
            [] )
  (* make pieces of graph, using graphette_start and/or graphette normal as appropriate, for each index in the work_list *)
  and mk_graph' return_target g_acc (visited : index list) (work_list : (index * nesting) list) =
    match work_list with
    | [] -> g_acc
    | (k, nesting) :: work_list' -> (
        if List.mem Int.equal k visited then mk_graph' return_target g_acc visited work_list'
        else
          (* Printf.printf "mk_graph' working on %d %s %s\n" k
               (pp_addr an.instructions.(k).i_addr)
               (String.concat "," an.elf_symbols.(k));
             flush stdout;
          *)
          match (is_graphette_start k, is_graph_non_start_node k) with
          | (true, true) ->
              (* graphette start, where the initial instruction is also a non-start node *)
              let (g1, _) = graphette_start return_target (k, nesting) in
              let (g2, work_list_new, bl_target_indices) =
                graphette_normal return_target (k, nesting)
              in
              let g_acc' = graph_cfg_union g1 (graph_cfg_union g2 g_acc) in
              let visited' = k :: visited in
              let work_list' = work_list_new @ bl_target_indices @ work_list' in
              mk_graph' return_target g_acc' visited' work_list'
          | (true, false) ->
              (* graphette start, where the initial instruction is not also a non-start node *)
              let (g1, work_list_new) = graphette_start return_target (k, nesting) in
              let g_acc' = graph_cfg_union g1 g_acc in
              let visited' = k :: visited in
              let work_list' = work_list_new @ work_list' in
              mk_graph' return_target g_acc' visited' work_list'
          | (false, true) ->
              (* non-graphette-start, non-start node*)
              let (g2, work_list_new, bl_target_indices) =
                graphette_normal return_target (k, nesting)
              in
              let g_acc' = graph_cfg_union g2 g_acc in
              let visited' = k :: visited in
              let work_list' = work_list_new @ bl_target_indices @ work_list' in
              mk_graph' return_target g_acc' visited' work_list'
          | (false, false) ->
              (* non-graphette-start, and not a non-start node*)
              warn
                "mk_graph' called on index %d at %s which is neither is_graphette_start nor \
                 is_graph_non_start_node - could be a self-loop"
                k
                (pp_addr an.instructions.(k).i_addr);
              mk_graph' return_target g_acc visited work_list'
      )
  and mk_graph return_target (work_list : (index * nesting) list) =
    mk_graph' return_target (graph_cfg_empty ()) [] work_list
  in

  mk_graph None start_indices

(* render graph to graphviz dot file *)

let pp_colour colour visited =
  if not visited then
    "[color=\"" ^ colour ^ "\"]" (*^ "[fillcolor=\"" ^ colour ^ "\"]"*) ^ "[fontcolor=\""
    ^ colour ^ "\"]"
  else
    "[color=\"" ^ colour ^ "\"]" ^ "[style=\"filled\"][fillcolor=\"" ^ colour ^ "\"]"
    ^ "[fontcolor=\""
    ^ List.assoc colour colour_pairs_svg
    ^ "\"]"

let margin = "[margin=\"0.03,0.02\"]"

(* let nodesep = "[nodesep=\"0.25\"]" in (*graphviz default *) *)
let nodesep = "[nodesep=\"0.1\"]"

let pp_node_name nn = "\"" ^ nn ^ "\""

let pp_edge graph_colour (nn, nn', cek) =
  match cek with
  | CFG_edge_flow | CFG_edge_branch_and_link_inline | CFG_edge_ret_inline ->
      pp_node_name nn ^ " -> " ^ pp_node_name nn' ^ nodesep ^ "[color=\"" ^ graph_colour ^ "\"]"
      ^ ";\n"
  | CFG_edge_correlate ->
      pp_node_name nn ^ " -> " ^ pp_node_name nn' ^ nodesep
      ^ "[constraint=\"false\";style=\"dashed\";color=\"lightgrey\"];\n"

let pp_cfg (g : graph_cfg) cfg_dot_file rankmin : unit =
  (*    let margin = "[margin=\"0.11,0.055\"]" in  (*graphviz default*) *)
  let pp_node node =
    let shape =
      match node.nc_kind with
      | CFG_node_branch_and_link | CFG_node_smc_hvc -> "[shape=\"box\"]"
      | _ -> ""
    in
    Printf.sprintf "%s [label=\"%s\"][tooltip=\"%s\"]%s%s%s;\n" (pp_node_name node.nc_name)
      node.nc_label
      (html_escape (String.concat "\n" (node.nc_ppd_instruction @ pp_node_inlining node)))
      margin shape
      (pp_colour node.nc_colour node.nc_visited)
  in

  let c = open_out cfg_dot_file in
  Printf.fprintf c "digraph g {\n";
  Printf.fprintf c "rankdir=\"LR\";\n";

  let rec pp_cfg' graph_colour indent (g : graph_cfg) : unit =
    (* edges should really carry their colour, as nodes do, without this hackish passing of graph_colour *)
    List.iter
      (function node -> Printf.fprintf c "%s%s\n" indent (pp_node node))
      g.gc_start_nodes;
    (* fixing rank=min for the start nodes is useful for whole-Hafnium plots, but not for compare *)
    if rankmin then
      Printf.fprintf c "%s{ rank=min; %s }\n" indent
        (String.concat ""
           (List.map (function node -> pp_node_name node.nc_name ^ ";") g.gc_start_nodes))
    else ();
    List.iter (function node -> Printf.fprintf c "%s%s\n" indent (pp_node node)) g.gc_nodes;
    List.iter
      (function e -> Printf.fprintf c "%s%s\n" indent (pp_edge graph_colour e))
      g.gc_edges;
    List.iter
      (function
        | (subgraph_name, subgraph_colour, g') ->
            Printf.fprintf c "%ssubgraph %s {\n%scolor=%s\n" indent subgraph_name indent
              subgraph_colour;
            pp_cfg' subgraph_colour ("  " ^ indent) g';
            Printf.fprintf c "}\n";
            List.iter
              (function e -> Printf.fprintf c "%s%s\n" indent (pp_edge subgraph_colour e))
              g'.gc_edges_exiting)
      g.gc_subgraphs
  in

  pp_cfg' "black" "" g;
  Printf.fprintf c "}\n";

  let _ = close_out c in
  ()

(* carve out reachable subgraph from some starting node labels (using that so that the user can just paste in from the rendered graph if need be) *)

let reachable_subgraph (g : graph_cfg) (labels_start : string list) : graph_cfg =
  let nodes_all : node_cfg list = g.gc_start_nodes @ g.gc_nodes in
  let edges_all : (string * string list) list =
    List.map
      (function
        | node ->
            ( node.nc_name,
              List.filter_map
                (function (nn1, nn2, _cek) -> if nn1 = node.nc_name then Some nn2 else None)
                g.gc_edges ))
      nodes_all
  in

  let rec stupid_reachability (through_bl : bool) (acc_reachable : string list)
      (todo : string list) : string list =
    match todo with
    | [] -> acc_reachable
    | nn :: todo' ->
        if List.mem String.equal nn acc_reachable then
          stupid_reachability through_bl acc_reachable todo'
        else
          let new_nodes = List.assoc nn edges_all in
          (*          let new_nodes_bl = if through_bl && *)
          stupid_reachability through_bl (nn :: acc_reachable)
            ((*new_nodes_bl @ *) new_nodes @ todo')
  in
  let start_node_names =
    List.filter_map
      (function
        | node ->
            if List.mem String.equal node.nc_label labels_start then Some node.nc_name else None)
      nodes_all
  in
  let node_names_reachable = stupid_reachability false [] start_node_names in
  let edges_reachable =
    List.filter
      (function
        | (nn, nn', _cek) ->
            List.mem String.equal nn node_names_reachable
            && List.mem String.equal nn' node_names_reachable)
      g.gc_edges
  in
  let nodes_reachable_start =
    List.filter
      (function node -> List.mem String.equal node.nc_name node_names_reachable)
      g.gc_start_nodes
  in
  let nodes_reachable_rest =
    List.filter
      (function node -> List.mem String.equal node.nc_name node_names_reachable)
      g.gc_nodes
  in
  {
    gc_start_nodes = nodes_reachable_start;
    gc_nodes = nodes_reachable_rest;
    gc_edges = edges_reachable;
    gc_edges_exiting = [];
    (*HACK*)
    gc_subgraphs = [] (* HACK*);
  }

(*
module P = Graph.Pack
http://ocamlgraph.lri.fr/doc/Fixpoint.html
 *)

(* same-source-line edges *)

let rec graph_nodes g =
  g.gc_nodes @ List.concat_map graph_nodes (List.map (function (_, _, g') -> g') g.gc_subgraphs)

let correlate_source_line g1 g2 : graph_cfg =
  let is_branch_cond = function
    | node -> (
        match node.nc_kind with
        | CFG_node_branch_cond | CFG_node_branch_register | CFG_node_ret
         |CFG_node_branch_and_link ->
            true
        | _ -> false
      )
  in
  let nodes_branch_cond1 = List.filter is_branch_cond (graph_nodes g1) in
  let nodes_branch_cond2 = List.filter is_branch_cond (graph_nodes g2) in

  let intersects xs ys = List.exists (function x -> List.mem Stdlib.( = ) x ys) xs in
  let edges =
    List.concat
      (List.map
         (function
           | n1 ->
               List.filter_map
                 (function
                   | n2 ->
                       if
                         intersects n1.nc_source_lines n2.nc_source_lines
                         && n1.nc_nesting.n_stack = n2.nc_stack
                       then Some (n1.nc_name, n2.nc_name, CFG_edge_correlate)
                       else None)
                 nodes_branch_cond2)
         nodes_branch_cond1)
  in
  {
    gc_start_nodes = [];
    gc_nodes = [];
    gc_edges = edges;
    gc_edges_exiting = [];
    gc_subgraphs = [];
  }
