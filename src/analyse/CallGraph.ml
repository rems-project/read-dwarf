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
(**      compute call-graph                                                  *)

(*****************************************************************************)

open Logs.Logger (struct
  let str = __MODULE__
end)

open Utils
open ElfTypes
open ControlFlowTypes

type call_graph_node = addr * index * string list

let mk_call_graph test (an : CollectedType.analysis) =
  let mask_addr x:natural =
    if !Globals.morello
    then Sym.shift_left (Sym.shift_right x 1) 1
    else x in
  (* take the nodes to be all the elf symbol addresses of stt_func
     symbol type (each with their list of elf symbol names) together
     with all the other-address bl-targets (of which in Hf there are just
     three, the same in O0 and O2, presumably explicit in assembly) *)
  let elf_symbols : (natural * string list) list =
    let elf_symbol_addresses =
      List.sort_uniq compare
        (List.filter_map
           (fun (_name, (typ, _size, address, _mb, _binding)) ->
             if typ = Elf_symbol_table.stt_func then Some (mask_addr address) else None)
           test.symbol_map)
    in
    List.map
      (fun address ->
        let names =
          List.sort_uniq compare
            (List.filter_map
               (fun (name, (_typ, _size, address', _mb, _binding)) ->
                 if address' = address && String.length name >= 1 && name.[0] <> '$' then
                   Some name
                 else None)
               test.symbol_map)
        in
        (address, names))
      elf_symbol_addresses
  in

  let extra_bl_targets' =
    List.concat
      (List.map
         (function
           | i ->
               let bl_targets =
                 List.filter
                   (function
                     | (tk', _, _, _) -> (
                         match tk' with
                         | T_branch_and_link_call | T_branch_and_link_call_noreturn -> true
                         | _ -> false
                       ))
                   i.i_targets
               in
               List.filter_map
                 (function
                   | (_, a', _, s') ->
                       if
                         not
                           (List.exists
                              (function (a'', _) -> Sym.Ordered.equal a' a'')
                              elf_symbols)
                       then Some (a', ["FROM BL:" ^ s'])
                       else None)
                 bl_targets)
         (Array.to_list an.instructions))
  in

  let rec dedup axs acc =
    match axs with
    | [] -> acc
    | (a, x) :: axs' ->
        if not (List.exists (function (a', _) -> Sym.Ordered.equal a a') acc) then
          dedup axs' ((a, x) :: acc)
        else dedup axs' acc
  in

  let extra_bl_targets = dedup extra_bl_targets' [] in

  let nodes0 =
    List.sort
      (function
        | (a, _) -> (
            function (a', _) -> Sym.Ordered.compare a a'
          ))
      (elf_symbols @ extra_bl_targets)
  in

  let nodes : call_graph_node list =
    List.map (function (a, ss) -> (a, an.index_of_address a, ss)) nodes0
  in

  let node_of_index k =
    match List.find_opt (function (_, k', _) -> k' = k) nodes with
    | Some n -> n
    | None ->
        err "node_of_index %d\n" k;
        List.hd nodes
  in

  let rec stupid_reachability (acc_reachable : int list) (acc_bl_targets : int list)
      (todo : int list) : int list * int list =
    match todo with
    | [] -> (acc_reachable, acc_bl_targets)
    | k :: todo' ->
        if List.mem Int.equal k acc_reachable then
          stupid_reachability acc_reachable acc_bl_targets todo'
        else if not (k < Array.length an.instructions) then
          stupid_reachability acc_reachable acc_bl_targets todo'
        else
          let i = an.instructions.(k) in
          let (bl_targets, non_bl_targets) =
            List.partition
              (function
                | (tk'', _, _, _) -> (
                    match tk'' with
                    | T_branch_and_link_call | T_branch_and_link_call_noreturn -> true
                    | _ -> false
                  ))
              i.i_targets
          in
          let bl_target_indices = List.map (function (_, _, k'', _) -> k'') bl_targets in
          let non_bl_target_indices =
            List.map (function (_, _, k'', _) -> k'') non_bl_targets
          in
          stupid_reachability (k :: acc_reachable)
            (List.sort_uniq compare (bl_target_indices @ acc_bl_targets))
            (non_bl_target_indices @ todo')
  in

  let bl_target_indices k =
    let (_reachable, bl_target_indices) = stupid_reachability [] [] [k] in
    bl_target_indices
  in

  let call_graph =
    List.map
      (function (_, k, _) as node -> (node, List.map node_of_index (bl_target_indices k)))
      nodes
  in

  let rec stupid_reachability' (acc_reachable : call_graph_node list)
      (todo : call_graph_node list) : call_graph_node list =
    match todo with
    | [] -> acc_reachable
    | ((_, k, _) as n) :: todo' ->
        if List.exists (function (_, k', _) -> k' = k) acc_reachable then
          stupid_reachability' acc_reachable todo'
        else
          let (_, targets) = List.find (function ((_, k', _), _) -> k' = k) call_graph in
          stupid_reachability' (n :: acc_reachable) (targets @ todo')
  in

  let transitive_call_graph =
    List.map
      (function
        | (_, k, _) as n ->
            let (_, targets) = List.find (function ((_, k', _), _) -> k' = k) call_graph in
            (n, stupid_reachability' [] targets))
      nodes
  in

  (call_graph, transitive_call_graph)


let pp_call_graph test an =
  let pp_node (a, _, ss) =
    pp_addr a (*" " ^ string_of_int k ^*) ^ " <" ^ String.concat ", " ss ^ ">"
  in

  let pp_call_graph_entry (n, ns) =
    pp_node n ^ ":\n" ^ String.concat "" (List.map (function n' -> "  " ^ pp_node n' ^ "\n") ns)
  in

  let pp_call_graph call_graph = String.concat "" (List.map pp_call_graph_entry call_graph) in

  let pp_transitive_call_graph transitive_call_graph =
    String.concat ""
      (List.map
         (function
           | (((_, k, _) as n), ns) ->
               (if List.exists (function (_, k', _) -> k' = k) ns then "RECURSIVE " else "")
               ^ "\n"
               ^ pp_call_graph_entry (n, ns))
         transitive_call_graph)
  in

  let call_graph, transitive_call_graph = mk_call_graph test an in

  (pp_call_graph call_graph, pp_transitive_call_graph transitive_call_graph)
